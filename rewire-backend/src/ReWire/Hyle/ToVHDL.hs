{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Safe #-}
-- | The VHDL backend on Hyle, mirroring ReWire.Hyle.ToVerilog (the
--   cosimulation check keeps the two aligned). With widths explicit and
--   exact in the IR, the width-reconstruction machinery of the old backend
--   (@wcast@/@expWidth@) disappears: every emitted expression already has
--   exactly the width its context requires, and the rw_helpers package is
--   needed only for the operations themselves (VHDL std_logic_vector has no
--   arithmetic), not for resizing discipline.
--
--   Defn modules never take clock or reset ports (sequential externs are
--   device-level instances). Extern ports connect by name; ports left
--   anonymous in the source-level extern descriptor reach this backend with
--   the @p<i>@ names synthesized by the producer, which the hand-written
--   VHDL implementations in the test suite already use.
module ReWire.Hyle.ToVHDL (compileProgram, testbench) where

import ReWire.Annotation (Annote, Annotated (ann))
import ReWire.BitVector (BV, width, bitVec, zeros, ones, lsb1)
import ReWire.Config (Config, ResetFlag (..), vhdlPackages)
import ReWire.Hyle.Interp (Ins, subRange, inputValue, yamlPrefixes)
import ReWire.Hyle.Mangle (mangleFresh, mangleMod, pickFresh, seedNames, stripFreshTag)
import ReWire.Error (failAt, failInternal, AstError, MonadError)
import ReWire.Hyle.Syntax as M
import ReWire.Hyle.ToVerilog (clockReset, defnPortNames, lastComponent, tagReg)
import ReWire.Pretty (showt)

import qualified ReWire.BitVector  as BV
import qualified ReWire.Config     as C
import qualified ReWire.VHDL.Syntax as H

import Control.Arrow ((&&&), first)
import Control.Lens ((^.))
import Control.Monad.State.Strict (MonadState, runStateT, modify', gets)
import Data.HashMap.Strict (HashMap)
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict     as OMap
import qualified Data.Text           as T

-- | Local value environment.
type LEnv = HashMap M.Name H.Exp

-- | Extern declarations, by name.
type XEnv = HashMap M.Name Extern

-- | Entity port names per defn ('defnPortNames'): call-site component
--   declarations must use the entity's own port names, since VHDL default
--   binding matches component formals to entity ports by name.
type PEnv = HashMap M.GId [Text]

data TS = TS
      { tsFresh    :: !(HashMap Text Int)
      , tsSigs     :: ![H.Signal]
      , tsConsts   :: ![H.Signal]                    -- ^ Emitted constant declarations (st_* state names).
      , tsComps    :: !(HashMap Text H.Component)
      , tsExps     :: !(Map Text H.Name)             -- ^ Emitted let RHS ('expKey') -> its wire (backend CSE).
      , tsCalls    :: !(Map (M.GId, [Text]) H.Name)  -- ^ Emitted defn call ('expKey' args; never an extern XCall) -> its result wire.
      , tsTags     :: !(Maybe (Text, M.Size, [(Text, Integer)])) -- ^ The tagged state register (device only): name, width, value display names.
      , tsTagsDone :: !Bool                          -- ^ The st_* constants have been declared.
      }

-- | Initial state, with the ambient names (ports, registers -- emitted
--   verbatim, never passing through 'fresh') seeded into the used map so
--   generated names can't collide with them. Seeds are case-folded to match
--   the case-folding in 'fresh' (VHDL basic identifiers are
--   case-insensitive).
ts0 :: Maybe (Text, M.Size, [(Text, Integer)]) -> [Text] -> TS
ts0 tags ambient = TS (seedNames $ map T.toLower ambient) [] [] mempty mempty mempty tags False

-- | A fresh signal/instance name: freshening tags stripped, mangled,
--   case-folded (the collision defense for VHDL's case-insensitive
--   namespace), and suffixed out of the way of every name already issued or
--   seeded (reserved words need no dodge here -- the pretty-printer escapes
--   them as extended identifiers). The suffix separator keeps suffixed
--   names basic identifiers (no capitals, which would force extended-
--   identifier escaping).
fresh :: MonadState TS m => Text -> m H.Name
fresh s = do
      used <- gets tsFresh
      let (n, used') = pickFresh "_r" used $ T.toLower $ mangleFresh $ stripFreshTag s
      modify' $ \ ts -> ts { tsFresh = used' }
      pure n

newWire :: MonadState TS m => M.Size -> Text -> m H.Name
newWire sz n = do
      n' <- fresh n
      modify' $ \ ts -> ts { tsSigs = tsSigs ts <> [H.Signal n' sz Nothing] }
      pure n'

-- | Bind a let-bound value: copy-propagate bare names and literals (no
--   wire, no assign), reuse the wire of an already-emitted identical
--   right-hand side (backend CSE), and otherwise emit one assign to a
--   fresh wire (seeded with the binder's name), recording it for reuse.
bindLet :: MonadState TS m => M.Size -> Text -> H.Exp -> m (H.Exp, [H.Stmt])
bindLet sz x = \ case
      e@H.Var {} -> pure (e, [])
      e@H.Lit {} -> pure (e, [])
      e          -> gets (OMap.lookup (expKey e) . tsExps) >>= \ case
            Just w  -> pure (H.Var w, [])
            Nothing -> do
                  w <- newWire sz x
                  modify' $ \ ts -> ts { tsExps = OMap.insert (expKey e) w $ tsExps ts }
                  (H.Var w, ) <$> assignStmts (H.LVName w) e

-- | Identity key for compiled expressions (the CSE memos): the full Show
--   rendering. The derived Eq/Ord on 'H.Exp' inherit the bv package's
--   width-blind BV instances (bitVec 4 0 == bitVec 8 0), which would
--   unify expressions differing only in a literal's width -- a
--   miscompilation, since widths are load-bearing in RTL. BV's Show
--   (@[4]0@) carries the width, so the rendering is width-exact. (The
--   selected-assignment matcher needs no key: its scrutinees are
--   restricted to literal-free shapes.)
expKey :: H.Exp -> Text
expKey = T.pack . show

-- | The statement driving a target: a selected signal assignment when the
--   right-hand side is a same-scrutinee comparison chain (2-state
--   equivalent to the rw_cond chain it replaces; doc/hyle.md, section 8.3),
--   otherwise a plain concurrent assignment. A chain over the tagged state
--   register also declares the st_* constants (the choices themselves stay
--   literal bit-strings).
assignStmts :: MonadState TS m => H.LVal -> H.Exp -> m [H.Stmt]
assignStmts lv e = case matchSelChain e of
      Just (scrut, arms, dflt) -> do
            declareTagConsts scrut
            pure [H.SelAssign scrut lv arms dflt]
      Nothing -> pure [H.Assign lv e]

-- | Recognize a same-scrutinee comparison chain (see
--   ToVerilog.matchCaseChain): an rw_cond tree testing one scrutinee for
--   rw_eq against literals (either operand order), distinct, same-width,
--   and at least two comparisons deep, ending in an unconditioned else. The
--   scrutinee is restricted to names and static slices, whose subtypes the
--   selected signal assignment can determine.
matchSelChain :: H.Exp -> Maybe (H.Exp, [(BV, H.Exp)], H.Exp)
matchSelChain = \ case
      H.FunCall "rw_cond" [c, t, f]
            | Just (s, v) <- eqLit c
            , selectable s
            , (arms, dflt) <- chain s f
            , not $ null arms -- at least two comparisons
            , all ((== width v) . width . fst) arms
            , distinct $ v : map fst arms
            -> Just (s, (v, t) : arms, dflt)
      _     -> Nothing
      where eqLit :: H.Exp -> Maybe (H.Exp, BV)
            eqLit = \ case
                  H.FunCall "rw_eq" [s, H.Lit v] | width v > 0 -> Just (s, v)
                  H.FunCall "rw_eq" [H.Lit v, s] | width v > 0 -> Just (s, v)
                  _                                            -> Nothing

            selectable :: H.Exp -> Bool
            selectable = \ case
                  H.Var {}   -> True
                  H.Slice {} -> True
                  H.Elem {}  -> True
                  _          -> False

            chain :: H.Exp -> H.Exp -> ([(BV, H.Exp)], H.Exp)
            chain s = \ case
                  H.FunCall "rw_cond" [c, t, f] | Just (s', v) <- eqLit c, s' == s -> first ((v, t) :) $ chain s f
                  e                                                                -> ([], e)

            distinct :: [BV] -> Bool
            distinct vs = length (nub vs) == length vs

-- | Declare the st_* state-name constants (once per unit) when the
--   scrutinee is the tagged state register.
declareTagConsts :: MonadState TS m => H.Exp -> m ()
declareTagConsts scrut = gets tsTags >>= \ case
      Just (reg, sz, tags) | scrut == H.Var reg -> gets tsTagsDone >>= \ case
            True  -> pure ()
            False -> do
                  modify' $ \ ts -> ts { tsTagsDone = True }
                  cs <- mapM (\ (nm, v) -> (\ n -> H.Constant n sz $ bitVec (fromIntegral sz) v) <$> fresh ("st_" <> nm)) tags
                  modify' $ \ ts -> ts { tsConsts = tsConsts ts <> cs }
      _ -> pure ()

addComponent :: MonadState TS m => H.Component -> m ()
addComponent c@(H.Component n _ _) = modify' $ \ ts -> ts { tsComps = Map.insert n c $ tsComps ts }

components :: TS -> [H.Component]
components = sortOn (\ (H.Component n _ _) -> n) . Map.elems . tsComps

compileProgram :: forall m. MonadError AstError m => Config -> M.Program -> m H.Device
compileProgram conf (M.Program exts ds dev) = do
      top <- compileDevice conf xenv penv dev
      ds' <- mapM (compileDefn conf xenv penv) ds
      pure $ H.Device $ top : ds'
      where xenv :: XEnv
            xenv = Map.fromList $ map (\ e -> (extName e, e)) exts

            penv :: PEnv
            penv = Map.fromList $ map (defnName &&& defnPortNames) ds

compileDefn :: forall m. MonadError AstError m => Config -> XEnv -> PEnv -> M.Defn -> m H.Unit
compileDefn conf xenv penv d@(M.Defn _ g (M.Sig _ argSzs _) ps body _ (Blind docs)) = do
      (stmts, ts) <- flip runStateT (ts0 Nothing $ portNames <> [ "res" | sizeOf body > 0 ]) $ do
            (e, estmts) <- compileExp xenv penv lenv body
            resStmts    <- if sizeOf body > 0 then assignStmts (H.LVName "res") e else pure []
            pure $ estmts <> resStmts
      -- Header comments: the unmangled Hyle name, then the defn's doc lines.
      pure $ H.Unit (mangleMod g) (g : docs) (unitPackages conf)
                    (zipWith (\ pn (_, sz) -> H.Port pn H.In sz) portNames live <> [ H.Port "res" H.Out (sizeOf body) | sizeOf body > 0 ])
                    (components ts) (tsSigs ts)
                    stmts
      where -- Zero-width parameters and results are erased; references
            -- compile to the empty literal.
            live :: [(M.Name, M.Size)]
            live = [ (x, sz) | (x, sz) <- zip ps argSzs, sz > 0 ]

            portNames :: [Text]
            portNames = defnPortNames d

            lenv :: LEnv
            lenv = Map.fromList $ [ (x, H.Lit BV.nil) | (x, 0) <- zip ps argSzs ]
                              <> zip (map fst live) (map H.Var portNames)

compileDevice :: forall m. MonadError AstError m => Config -> XEnv -> PEnv -> M.Device -> m H.Unit
compileDevice conf xenv penv (M.Device an top ins outs regs insts body (Blind tags)) = do
      (stmts, ts) <- flip runStateT (ts0 tagInfo ambient) $ do
            instWires <- Map.fromList . concat <$> mapM instOutWires insts
            (letStmts, lenv) <- foldStmts (ambientEnv instWires) body
            instStmts <- concat <$> mapM (compileInst instWires lenv) insts
            outStmts  <- concat <$> mapM (driveOut lenv) outs
            pure $ section "combinational logic" letStmts
                <> section "instances" instStmts
                <> section "outputs" outStmts
      pure $ H.Unit top [] (unitPackages conf)
                   (map (portIn . (, 1)) (catMaybes [mclk, mrst]) <> map portIn (live ins) <> map portOut (live outs))
                   (components ts)
                   (regLegend <> tsConsts ts <> regSigs <> tsSigs ts)
                   (stmts <> section "state register update" stateProcess)
      where (mclk, mrst) = clockReset conf regs insts

            -- | A section banner, only above a nonempty statement group.
            section :: Text -> [H.Stmt] -> [H.Stmt]
            section _ []      = []
            section banner ss = H.Comment banner : ss

            -- | The tagged state register, when live and named by devTags.
            tagInfo :: Maybe (Text, M.Size, [(Text, Integer)])
            tagInfo = case [ (x, sz) | M.Register _ x sz _ <- liveRegs, x == tagReg ] of
                  [(x, sz)] | not $ null tags -> Just (x, sz, tags)
                  _                           -> Nothing

            -- | A banner and a legend comment (width, initial value, and --
            --   for the tagged state register -- the state display names)
            --   above the register signal declarations.
            regLegend :: [H.Signal]
            regLegend | null liveRegs = []
                      | otherwise     = H.SigComment "state registers" : concatMap legend liveRegs

            legend :: M.Register -> [H.Signal]
            legend (M.Register _ x sz bv) = H.SigComment (x <> ": " <> showt sz <> " bits, init " <> BV.showHex bv)
                  : [ H.SigComment ("  states: " <> T.unwords [ showt v <> "=" <> nm | (nm, v) <- tags ]) | x == tagReg, not $ null tags ]

            portIn, portOut :: (M.Name, M.Size) -> H.Port
            portIn  (n, sz) = H.Port n H.In sz
            portOut (n, sz) = H.Port n H.Out sz

            live :: [(M.Name, M.Size)] -> [(M.Name, M.Size)]
            live = filter ((> 0) . snd)

            liveRegs :: [M.Register]
            liveRegs = [ r | r@(M.Register _ _ sz _) <- regs, sz > 0 ]

            -- | Names emitted verbatim (ports, registers): the fresh-name
            --   supply must avoid them.
            ambient :: [Text]
            ambient = catMaybes [mclk, mrst]
                   <> map fst (live ins) <> map fst (live outs)
                   <> concat [ [x, x <> "_next"] | M.Register _ x _ _ <- liveRegs ]

            -- | Register signals carry their initial values; the @_next@
            --   wires carry the per-cycle updates.
            regSigs :: [H.Signal]
            regSigs = concat [ [H.Signal x sz (Just bv), H.Signal (x <> "_next") sz Nothing] | M.Register _ x sz bv <- liveRegs ]

            ambientEnv :: HashMap (M.Name, M.Name) H.Name -> LEnv
            ambientEnv instWires = Map.fromList $
                     [ (x, if sz > 0 then H.Var x else H.Lit BV.nil) | (x, sz) <- ins ]
                  <> [ (x, if sz > 0 then H.Var x else H.Lit BV.nil) | M.Register _ x sz _ <- regs ]
                  <> [ (x <> "." <> q, H.Var w) | ((x, q), w) <- Map.toList instWires ]

            instOutWires :: MonadState TS m' => M.Instance -> m' [((M.Name, M.Name), H.Name)]
            instOutWires (M.Instance _ x ex _) = case Map.lookup ex xenv of
                  Nothing -> pure []
                  Just e  -> mapM (\ (q, sz) -> ((x, q), ) <$> newWire sz (x <> "_" <> q)) $ filter ((> 0) . snd) $ extOutputs e

            instWire :: HashMap (M.Name, M.Name) H.Name -> M.Name -> M.Name -> H.Name
            instWire m x q = fromMaybe (x <> "_" <> q) $ Map.lookup (x, q) m

            foldStmts :: (MonadState TS m', MonadError AstError m') => LEnv -> [M.Stmt] -> m' ([H.Stmt], LEnv)
            foldStmts lenv = \ case
                  [] -> pure ([], lenv)
                  M.SLet _ x e : rest | M.isNil e -> foldStmts (Map.insert x (H.Lit BV.nil) lenv) rest
                  M.SLet _ x e : rest -> do
                        (e', stmts)    <- compileExp xenv penv lenv e
                        (xv, bstmts)   <- bindLet (sizeOf e) x e'
                        (rest', lenv') <- foldStmts (Map.insert x xv lenv) rest
                        pure (stmts <> bstmts <> rest', lenv')
                  M.SNext _ _ e : rest | M.isNil e -> foldStmts lenv rest
                  M.SNext _ x e : rest -> do
                        (e', stmts) <- compileExp xenv penv lenv e
                        (rest', lenv') <- foldStmts lenv rest
                        pure (stmts <> [H.Assign (H.LVName $ x <> "_next") e'] <> rest', lenv')
                  M.SOutput {} : rest -> foldStmts lenv rest -- emitted after the lets, in port order
                  M.SInstIn {} : rest -> foldStmts lenv rest -- emitted with the instantiations

            driveOut :: (MonadState TS m', MonadError AstError m') => LEnv -> (M.Name, M.Size) -> m' [H.Stmt]
            driveOut _ (_, 0) = pure []
            driveOut lenv (x, _) = case [ e | M.SOutput _ x' e <- body, x' == x ] of
                  [e] -> do
                        (e', stmts) <- compileExp xenv penv lenv e
                        pure $ stmts <> [H.Assign (H.LVName x) e']
                  _   -> failInternal an $ "internal error: output " <> x <> " is not driven exactly once"

            compileInst :: (MonadState TS m', MonadError AstError m') => HashMap (M.Name, M.Name) H.Name -> LEnv -> M.Instance -> m' [H.Stmt]
            compileInst instWires lenv (M.Instance an' x ex cs) = case Map.lookup ex xenv of
                  Nothing -> failAt an' $ "instance " <> x <> ": unknown extern: " <> ex
                  Just e  -> do
                        addComponent $ extComponent e
                        (args, stmts) <- (map fst &&& concatMap snd) <$> mapM (driveIn lenv x) (filter ((> 0) . snd) $ extInputs e)
                        clkrst <- clockRstPorts e
                        inst   <- fresh x
                        let outws = [ (q, H.Var $ instWire instWires x q) | (q, sz) <- extOutputs e, sz > 0 ]
                        pure $ stmts <> [ H.Instantiate ex inst (zip (extGenerics e) $ map toInteger cs)
                                        $ clkrst <> args <> outws ]

            clockRstPorts :: MonadError AstError m' => Extern -> m' [(H.Name, H.Exp)]
            clockRstPorts e = case extKind e of
                  Comb      -> pure []
                  Seq mc mr -> do
                        c <- port' "clock" mc mclk
                        r <- port' "reset" mr mrst
                        pure $ catMaybes [c, r]
                  where port' :: MonadError AstError m' => Text -> Maybe M.Name -> Maybe Text -> m' (Maybe (H.Name, H.Exp))
                        port' what p sig = case (p, sig) of
                              (Nothing, _)      -> pure Nothing
                              (Just p', Just s) -> pure $ Just (p', H.Var s)
                              (Just _, Nothing) -> failAt (ann e) $ "external module requires a " <> what <> " signal, but we have no " <> what <> " to give it."

            driveIn :: (MonadState TS m', MonadError AstError m') => LEnv -> M.Name -> (M.Name, M.Size) -> m' ((H.Name, H.Exp), [H.Stmt])
            driveIn lenv x (p, sz) = case [ e | M.SInstIn _ x' p' e <- body, x' == x, p' == p ] of
                  [e] -> do
                        (e', stmts)  <- compileExp xenv penv lenv e
                        (e'', stmts') <- connect sz e'
                        pure ((p, e''), stmts <> stmts')
                  _   -> failInternal an $ "internal error: instance input " <> x <> "." <> p <> " is not driven exactly once"

            -- | The state-update process, with an (a)synchronous reset per
            --   the configured reset flags. No configured clock (--no-clock)
            --   means no process: registers hold their initial values.
            stateProcess :: [H.Stmt]
            stateProcess = case (liveRegs, mclk) of
                  ([], _)       -> []
                  (_, Nothing)  -> []
                  (_, Just clk) -> pure $ case (mrst, syncRst) of
                        (Just rstn, False) -> H.Process [clk, rstn] []
                              [H.SIf [(rstCond rstn, rstAssigns), (H.CondRising clk, nextAssigns)] []]
                        (Just rstn, True)  -> H.Process [clk] []
                              [H.SIf [(H.CondRising clk, [H.SIf [(rstCond rstn, rstAssigns)] nextAssigns])] []]
                        (Nothing, _)       -> H.Process [clk] []
                              [H.SIf [(H.CondRising clk, nextAssigns)] []]

            rstCond :: Text -> H.Cond
            rstCond rstn = H.CondEq rstn $ bitVec 1 $ fromEnum $ not invertRst

            rstAssigns :: [H.SeqStmt]
            rstAssigns = [ H.SAssign (H.LVName x) $ H.Lit bv | M.Register _ x _ bv <- liveRegs ]

            nextAssigns :: [H.SeqStmt]
            nextAssigns = [ H.SAssign (H.LVName x) $ H.Var $ x <> "_next" | M.Register _ x _ _ <- liveRegs ]

            invertRst, syncRst :: Bool
            invertRst = Inverted `elem` (conf^.C.resetFlags)
            syncRst   = Synchronous `elem` (conf^.C.resetFlags)

-- | The component declaration for an extern: generics, then clock/reset and
--   inputs, then outputs, with the (possibly synthesized) port names.
extComponent :: Extern -> H.Component
extComponent e = H.Component (extName e) (extGenerics e)
      $  map (\ n -> H.Port n H.In 1) (clkRstNames e)
      <> map (\ (p, sz) -> H.Port p H.In sz) (filter ((> 0) . snd) $ extInputs e)
      <> map (\ (q, sz) -> H.Port q H.Out sz) (filter ((> 0) . snd) $ extOutputs e)

clkRstNames :: Extern -> [M.Name]
clkRstNames e = case extKind e of
      Comb      -> []
      Seq mc mr -> catMaybes [mc, mr]

-- | A port-map actual: signal references connect directly; anything else is
--   hoisted into a temporary of exactly the port's width (VHDL port
--   associations are strict about form). Widths always agree by Hyle
--   invariant, so no resizing is involved. Clock signals connect directly by
--   construction (they are plain signal references).
connect :: (MonadState TS m, MonadError AstError m) => M.Size -> H.Exp -> m (H.Exp, [H.Stmt])
connect psz e
      | trivial e = pure (e, [])
      | otherwise = do
            tmp <- newWire psz "conn"
            pure (H.Var tmp, [H.Assign (H.LVName tmp) e])
      where trivial :: H.Exp -> Bool
            trivial = \ case
                  H.Var {}   -> True
                  H.Slice {} -> True
                  H.Elem {}  -> True
                  H.Lit {}   -> True
                  _          -> False

---

compileExps :: (MonadState TS m, MonadError AstError m) => XEnv -> PEnv -> LEnv -> [M.Exp] -> m ([H.Exp], [H.Stmt])
compileExps xenv penv lenv es = (map fst &&& concatMap snd) <$> mapM (compileExp xenv penv lenv) es

compileExp :: forall m. (MonadState TS m, MonadError AstError m) => XEnv -> PEnv -> LEnv -> M.Exp -> m (H.Exp, [H.Stmt])
compileExp xenv penv = go
      where go :: LEnv -> M.Exp -> m (H.Exp, [H.Stmt])
            go lenv = \ case
                  M.Lit _ bv          -> pure (litExp bv, [])
                  M.Undef _ sz        -> pure (litExp $ zeros $ fromIntegral sz, [])
                  M.Var an _ x        -> case Map.lookup x lenv of
                        Just e  -> pure (e, [])
                        Nothing -> failInternal an $ "unbound variable: " <> x
                  e@M.Cat {}          -> first hCat <$> compileExps xenv penv lenv (gather e)
                  M.Slice _ i k e     -> do
                        (e', stmts) <- go lenv e
                        (e'', stmts') <- sliceExp (sizeOf e) i k e'
                        pure (e'', stmts <> stmts')
                  M.Prim an sz op es  -> compilePrim lenv an sz op es
                  M.Call _ 0 _ _      -> pure (H.Lit BV.nil, []) -- zero-width call: dead logic, erased
                  M.Call _ sz g es    -> do
                        let esLive = filter (not . M.isNil) es
                        (es', stmts) <- compileExps xenv penv lenv esLive
                        -- The instantiate memo: an identical call already
                        -- has an instance in this unit; reuse its result
                        -- wire (defn entities are pure, so identical
                        -- arguments mean identical outputs).
                        gets (OMap.lookup (g, map expKey es') . tsCalls) >>= \ case
                              Just mr -> pure (H.Var mr, stmts)
                              Nothing -> do
                                    (conns, hoists) <- (map fst &&& concatMap snd) <$> mapM (uncurry connect) (zip (map sizeOf esLive) es')
                                    -- The component's port names must match
                                    -- the entity's: default binding
                                    -- associates them by name.
                                    let pns = fromMaybe (zipWith (\ i _ -> "arg" <> showt (i :: Int)) [0 ..] esLive) $ Map.lookup g penv
                                    addComponent $ H.Component (mangleMod g) []
                                          $  zipWith (\ pn sz' -> H.Port pn H.In sz') pns (map sizeOf esLive)
                                          <> [H.Port "res" H.Out sz]
                                    mr   <- newWire sz $ g <> "_out"
                                    inst <- fresh $ lastComponent g <> "_i"
                                    modify' $ \ ts -> ts { tsCalls = OMap.insert (g, map expKey es') mr $ tsCalls ts }
                                    pure (H.Var mr, stmts <> hoists <> [H.Instantiate (mangleMod g) inst [] $ map (mempty, ) $ conns <> [H.Var mr]])
                  M.XCall an sz x cs es -> case Map.lookup x xenv of
                        Nothing -> failAt an $ "unknown extern: " <> x
                        Just ex -> do
                              addComponent $ extComponent ex
                              let esLive = filter (not . M.isNil) es
                                  inPorts = filter ((> 0) . snd) $ extInputs ex
                              (es', stmts)    <- compileExps xenv penv lenv esLive
                              (conns, hoists) <- (map fst &&& concatMap snd) <$> mapM (uncurry connect) (zip (map sizeOf esLive) es')
                              mr   <- newWire sz "extres"
                              inst <- fresh $ x <> "_i"
                              pure ( H.Var mr
                                   , stmts <> hoists <> [H.Instantiate x inst (zip (extGenerics ex) $ map toInteger cs)
                                          $ zip (map fst inPorts) conns <> outSlices mr (extOutputs ex)])
                  M.If _ _ c t e      -> do
                        (c', stmts)   <- go lenv c
                        (t', stmts')  <- go lenv t
                        (e', stmts'') <- go lenv e
                        pure (H.FunCall "rw_cond" [c', t', e'], stmts <> stmts' <> stmts'')
                  M.Let _ _ x e1 e2
                        | M.isNil e1 -> go (Map.insert x (H.Lit BV.nil) lenv) e2
                        | otherwise  -> do
                              (e1', stmts)  <- go lenv e1
                              (xv, bstmts)  <- bindLet (sizeOf e1) x e1'
                              (e2', stmts') <- go (Map.insert x xv lenv) e2
                              pure (e2', stmts <> bstmts <> stmts')

            -- | The output ports of an extern call, associated with slices
            --   of the result wire (MSB-first in declaration order).
            outSlices :: H.Name -> [(M.Name, M.Size)] -> [(H.Name, H.Exp)]
            outSlices mr qs = [ (q, H.Slice mr off (off + fromIntegral sz - 1)) | (q, sz, off) <- offsets, sz > 0 ]
                  where offsets :: [(M.Name, M.Size, H.Index)]
                        offsets = snd $ foldr (\ (q, sz) (o, acc) -> (o + fromIntegral sz, (q, sz, o) : acc)) (0 :: H.Index, []) qs

            compilePrim :: LEnv -> Annote -> M.Size -> Op -> [M.Exp] -> m (H.Exp, [H.Stmt])
            compilePrim lenv an _sz op es = do
                  (es', stmts) <- compileExps xenv penv lenv es
                  let pure' e = pure (e, stmts)
                      bin f a b = pure' $ H.FunCall f [a, b]
                  case (op, es', es) of
                        (M.Add   , [a, b], _) -> bin "rw_add" a b
                        (M.Sub   , [a, b], _) -> bin "rw_sub" a b
                        (M.Mul   , [a, b], _) -> bin "rw_mul" a b
                        (M.Pow   , [a, b], _) -> bin "rw_pow" a b
                        (M.UDiv  , [a, b], _) -> bin "rw_div" a b
                        (M.UMod  , [a, b], _) -> bin "rw_mod" a b
                        (M.And   , [a, b], _) -> bin "rw_and" a b
                        (M.Or    , [a, b], _) -> bin "rw_or" a b
                        (M.XOr   , [a, b], _) -> bin "rw_xor" a b
                        (M.Not   , [a], _)    -> pure' $ H.FunCall "rw_not" [a]
                        (M.Shl   , [a, b], _) -> bin "rw_shiftl" a b
                        (M.LShr  , [a, b], _) -> bin "rw_shiftr" a b
                        (M.AShr  , [a, b], _) -> bin "rw_ashiftr" a b
                        (M.Eq    , [a, b], _) -> bin "rw_eq" a b
                        (M.Ne    , [a, b], _) -> bin "rw_neq" a b
                        (M.ULt   , [a, b], _) -> bin "rw_lt" a b
                        (M.ULe   , [a, b], _) -> bin "rw_lteq" a b
                        (M.UGt   , [a, b], _) -> bin "rw_gt" a b
                        (M.UGe   , [a, b], _) -> bin "rw_gteq" a b
                        (M.SLt   , [a, b], _) -> bin "rw_lts" a b
                        (M.SLe   , [a, b], _) -> bin "rw_lteqs" a b
                        (M.SGt   , [a, b], _) -> bin "rw_gts" a b
                        (M.SGe   , [a, b], _) -> bin "rw_gteqs" a b
                        (M.RedAnd, [a], _)    -> pure' $ H.FunCall "rw_rand" [a]
                        (M.RedOr , [a], _)    -> pure' $ H.FunCall "rw_ror" [a]
                        (M.RedXOr, [a], _)    -> pure' $ H.FunCall "rw_rxor" [a]
                        (M.ZExt m, [a], [ma])
                              | m == sizeOf ma -> pure' a
                              | otherwise      -> pure' $ H.FunCall "rw_resize" [a, H.Num $ fromIntegral m]
                        (M.Trunc m, [a], [ma])
                              | m == sizeOf ma -> pure' a
                              | otherwise      -> pure' $ H.FunCall "rw_resize" [a, H.Num $ fromIntegral m]
                        (M.SExt m, [a], [ma])
                              | m == sizeOf ma -> pure' a
                              | otherwise      -> pure' $ H.FunCall "rw_sext" [a, H.Num $ fromIntegral m]
                        (M.Rep k, [a], _)
                              | k == 0    -> pure' $ H.Lit BV.nil
                              | otherwise -> pure' $ H.FunCall "rw_repl" [H.Num k, a]
                        _ -> failInternal an $ "ill-formed primitive application: " <> opName op <> " with " <> showt (length es) <> " arguments"

            -- | Slice a compiled expression: slices on names and literals
            --   directly; anything else through a fresh wire.
            sliceExp :: M.Size -> M.Index -> M.Size -> H.Exp -> m (H.Exp, [H.Stmt])
            sliceExp w i k e
                  | k == 0          = pure (H.Lit BV.nil, [])
                  | i == 0, k == w  = pure (e, [])
                  | otherwise = case e of
                        H.Var n       -> pure (H.Slice n i' (i' + k' - 1), [])
                        H.Slice n a _ -> pure (H.Slice n (a + i') (a + i' + k' - 1), [])
                        H.Lit bv      -> pure (H.Lit $ subRange (fromIntegral i, fromIntegral (i + fromIntegral k) - 1) bv, [])
                        _             -> do
                              n <- newWire w "slice_in"
                              pure (H.Slice n i' (i' + k' - 1), [H.Assign (H.LVName n) e])
                  where i', k' :: H.Index
                        i' = fromIntegral i
                        k' = fromIntegral k

-- | Concatenation, filtering zero-width parts.
hCat :: [H.Exp] -> H.Exp
hCat es = case filter (not . hIsNil) es of
      [e] -> e
      es' -> H.Cat es'

hIsNil :: H.Exp -> Bool
hIsNil = \ case
      H.Lit bv -> width bv <= 0
      _        -> False

-- | Break up giant literals (as the Verilog backend does).
litExp :: BV -> H.Exp
litExp bv | width bv == 0         = H.Lit BV.nil
          | width bv < maxLit     = H.Lit bv
          | bv == zeros 1         = H.FunCall "rw_repl" [H.Num $ fromIntegral $ width bv, H.Lit $ zeros 1]
          | bv == ones (width bv) = H.FunCall "rw_repl" [H.Num $ fromIntegral $ width bv, H.Lit $ ones 1]
          | zs > 8                = hCat [H.Lit $ subRange (fromIntegral zs, width bv - 1) bv, H.FunCall "rw_repl" [H.Num zs, H.Lit $ zeros 1]]
          | otherwise             = H.Lit bv
      where zs :: Natural
            zs | bv == zeros 1 = fromIntegral $ width bv
               | otherwise     = fromIntegral $ lsb1 bv

            maxLit :: Int
            maxLit = 32

-- | The context clause for generated units: the configured VHDL packages
--   (--vhdl-packages) plus the defaults and the emitted rw_helpers package.
unitPackages :: Config -> [Text]
unitPackages conf = foldr (\ pk pks -> if pk `elem` pks then pks else pk : pks)
                          ["ieee.std_logic_1164.all", "ieee.numeric_std.all"]
                          (conf^.vhdlPackages)
                  <> ["work.rw_helpers.all"]

-- | A testbench driving the device with interp-style inputs and printing
--   outputs each cycle in the interpreter's YAML format (same protocol and
--   timing as the Verilog testbench).
testbench :: Config -> M.Device -> [Ins] -> H.Unit
testbench conf dev inps = H.Unit "tb" []
      ["ieee.std_logic_1164.all", "ieee.numeric_std.all", "std.textio.all"]
      []
      [H.Component (devName dev) [] $ map (\ (n, sz) -> H.Port n H.In sz) (clkRst <> ins) <> map (\ (n, sz) -> H.Port n H.Out sz) outs]
      (map (\ (n, sz) -> H.Signal n sz Nothing) $ clkRst <> ins <> outs)
      [ H.Instantiate (devName dev) "dut" [] $ map ((mempty, ) . H.Var . fst) $ clkRst <> ins <> outs
      , H.Process [] [H.LineVar "l" | not (null outs)] $ resetSeq <> concatMap cyc inps <> [H.SFinish]
      ]
      where (mclk, mrst) = clockReset conf (devRegisters dev) (devInstances dev)

            clkRst, ins, outs :: [(Text, M.Size)]
            clkRst = map (, 1) $ catMaybes [mclk, mrst]
            ins    = filter ((> 0) . snd) $ devInputs dev
            outs   = filter ((> 0) . snd) $ devOutputs dev

            drive :: Ins -> [H.SeqStmt]
            drive i = map (\ (n, sz) -> H.SAssign (H.LVName n) $ H.Lit $ bitVec (fromIntegral sz) $ inputValue sz i n) ins

            bit :: Bool -> H.Exp
            bit = H.Lit . bitVec 1 . fromEnum

            rstActive :: Bool
            rstActive = Inverted `notElem` (conf^.C.resetFlags)

            tick :: Text -> [H.SeqStmt]
            tick clk = [H.SWait 1, H.SAssign (H.LVName clk) $ bit True, H.SWait 5, H.SAssign (H.LVName clk) $ bit False]

            resetSeq :: [H.SeqStmt]
            resetSeq = case (mclk, mrst) of
                  (Just clk, Just rst) ->
                        [ H.SAssign (H.LVName clk) $ bit False, H.SAssign (H.LVName rst) $ bit rstActive ]
                        <> drive (headIns inps)
                        <> concat (replicate 2 [H.SWait 5, H.SAssign (H.LVName clk) $ bit True, H.SWait 5, H.SAssign (H.LVName clk) $ bit False])
                        <> [ H.SAssign (H.LVName rst) $ bit $ not rstActive ]
                  (Just clk, Nothing)  -> [ H.SAssign (H.LVName clk) $ bit False ]
                  _                    -> []

            cyc :: Ins -> [H.SeqStmt]
            cyc i = drive i <> case mclk of
                  Just clk -> [H.SWait 4] <> writes <> tick clk
                  Nothing  -> [H.SWait 5] <> writes <> [H.SWait 5]

            writes :: [H.SeqStmt]
            writes = zipWith (\ pre (n, _) -> H.SWriteLn [H.ChunkLit (pre <> n <> ": '0x"), H.ChunkHex (H.Var n), H.ChunkLit "'"]) yamlPrefixes outs

            headIns :: [Ins] -> Ins
            headIns = \ case
                  i : _ -> i
                  _     -> mempty

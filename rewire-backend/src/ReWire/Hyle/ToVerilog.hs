{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Safe #-}
-- | The Verilog backend on Hyle. With widths explicit and exact in the IR
--   (doc/hyle.md, G1), expression emission is a per-construct template: no
--   context-width reconstruction (the old @wcast@/@expWidth@), no pattern
--   compilation, no clock plumbing through the module tree (Hyle defns are
--   always pure -- sequential externs are device-level instances), and the
--   state machine is built from the device's explicit registers rather than
--   reconstructed from the resumption layout.
--
--   Defns are emitted one module each (inlining decisions are made upstream
--   by ReWire.Hyle.Transform.inline); the device becomes the top module:
--   per-register current/next signals updated in a single clocked process,
--   wire assignments for the device lets, and one instantiation per
--   sequential-extern instance.
module ReWire.Hyle.ToVerilog (compileProgram, testbench, clockReset, defnPortNames, lastComponent, tagReg) where

import ReWire.Annotation (Annote, Annotated (ann))
import ReWire.BitVector (BV, width, bitVec, nat, showHex, zeros, ones, lsb1, szBitRep)
import ReWire.Config (Config, ResetFlag (..))
import ReWire.Hyle.Interp (Ins, subRange, inputValue, yamlPrefixes)
import ReWire.Hyle.Mangle (mangleFresh, mangleMod, pickFresh, seedNames, stripFreshTag, svReserved)
import ReWire.Error (failAt, failInternal, AstError, MonadError)
import ReWire.Hyle.Syntax as M
import ReWire.Pretty (showt)
import ReWire.Verilog.Syntax as V

import qualified ReWire.Config as C

import Control.Arrow ((&&&), first)
import Control.Lens ((^.))

import Control.Monad.State.Strict (MonadState, runStateT, modify', gets)
import Data.Char (isDigit)
import Data.HashMap.Strict (HashMap)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict     as OMap
import qualified Data.Text           as T

-- | Per-module emission state: the fresh-name supply, the accumulated
--   signal declarations, the two backend-CSE memos (value numbering for
--   let-bound right-hand sides, and the instantiate memo for defn calls),
--   and the tagged state register (device only): its name, width, and the
--   display names for its values, plus the ST_* localparams once emitted.
data SigInfo = SigInfo
      { siFresh     :: !(HashMap Text Int)
      , siSigs      :: ![Signal]
      , siExps      :: !(Map Text V.Name)             -- ^ Emitted let RHS ('expKey') -> its wire.
      , siCalls     :: !(Map (M.GId, [Text]) V.Name)  -- ^ Emitted defn call ('expKey' args; never an extern XCall) -> its result wire.
      , siTags      :: !(Maybe (Text, M.Size, [(Text, Integer)]))
      , siTagConsts :: !(Maybe [(Integer, V.Name)])
      }

sigInfo0 :: Maybe (Text, M.Size, [(Text, Integer)]) -> [Text] -> SigInfo
sigInfo0 tags ambient = SigInfo (seedNames ambient) [] mempty mempty tags Nothing

-- | Identity key for compiled expressions (the CSE memos, the case-chain
--   same-scrutinee test): the full Show rendering. The derived Eq/Ord on
--   'V.Exp' inherit the bv package's width-blind BV instances (bitVec 4 0
--   == bitVec 8 0), which would unify expressions differing only in a
--   literal's width -- a miscompilation, since widths are load-bearing in
--   RTL. BV's Show (@[4]0@) carries the width, so the rendering is
--   width-exact.
expKey :: V.Exp -> Text
expKey = T.pack . show

-- | Local value environment.
type LEnv = HashMap M.Name V.Exp

-- | Extern declarations, by name.
type XEnv = HashMap M.Name Extern

-- | A fresh signal/instance name: freshening tags stripped (so an inlined
--   @r0$i3@ renders as wire @r0@), mangled (case preserved -- Verilog is
--   case-sensitive), dodging Verilog keywords (all-lowercase and matched
--   exactly, since keywords are case-sensitive too), and suffixed out of
--   the way of every name already issued or seeded as ambient (ports and
--   registers are emitted verbatim and never pass through here -- see the
--   'sigInfo0' calls).
fresh :: MonadState SigInfo m => Text -> m V.Name
fresh s = do
      used <- gets siFresh
      let (n, used') = pickFresh "R" used $ dodge $ mangleFresh $ stripFreshTag s
      modify' $ \ si -> si { siFresh = used' }
      pure n
      where dodge :: Text -> Text
            dodge n | svReserved n = n <> "_"
                    | otherwise    = n

newWire :: MonadState SigInfo m => M.Size -> Text -> m V.Name
newWire sz n = do
      n' <- fresh n
      modify' $ \ si -> si { siSigs = siSigs si <> [mkSignal (n', sz)] }
      pure n'

-- | Bind a let-bound value: copy-propagate bare names and literals (no
--   wire, no assign), reuse the wire of an already-emitted identical
--   right-hand side (backend CSE), and otherwise emit one binding of a
--   fresh wire (seeded with the binder's name), recording it for reuse.
bindLet :: MonadState SigInfo m => M.Size -> Text -> V.Exp -> m (V.Exp, [V.Stmt])
bindLet sz x = \ case
      e@(LVal (V.Name _)) -> pure (e, [])
      e@LitBits {}        -> pure (e, [])
      e                   -> gets (OMap.lookup (expKey e) . siExps) >>= \ case
            Just w  -> pure (LVal $ V.Name w, [])
            Nothing -> do
                  (w, stmts) <- bindStmts sz x e
                  modify' $ \ si -> si { siExps = OMap.insert (expKey e) w $ siExps si }
                  pure (LVal $ V.Name w, stmts)

-- | The statements binding a fresh name (seeded with the binder's name) to
--   a compiled right-hand side: a same-scrutinee comparison chain becomes a
--   case statement targeting a head-declared logic (2-state equivalent to
--   the ternary chain it replaces; doc/hyle.md, section 8.3), anything else
--   a net declaration assignment at the statement position (declare-at-use).
bindStmts :: MonadState SigInfo m => M.Size -> Text -> V.Exp -> m (V.Name, [V.Stmt])
bindStmts sz x e = case matchCaseChain e of
      Just chain -> do
            w <- newWire sz x
            (w, ) <$> caseAssign (V.Name w) chain
      Nothing    -> do
            w <- fresh x
            pure (w, [WireAssign (fromIntegral sz) w e])

-- | Recognize a same-scrutinee comparison chain: a conditional testing one
--   scrutinee for equality against literals (in either operand order),
--   distinct and at least two comparisons deep, ending in an unconditioned
--   else. Yields the scrutinee, the arms, and the default. Recognition is
--   on the compiled expression, so it applies wherever the shape occurs
--   (the state dispatch, constructor-tag decodes); chains that don't match
--   keep the ternary form.
matchCaseChain :: V.Exp -> Maybe (V.Exp, [(BV, V.Exp)], V.Exp)
matchCaseChain = \ case
      Cond c t f | Just (s, v) <- eqLit c
                 , (arms, dflt) <- chain s f
                 , not $ null arms -- at least two comparisons
                 , all ((== width v) . width . fst) arms
                 , distinct $ v : map fst arms
                 -> Just (s, (v, t) : arms, dflt)
      _          -> Nothing
      where eqLit :: V.Exp -> Maybe (V.Exp, BV)
            eqLit = \ case
                  V.Eq s (LitBits v) | not $ isLit s -> Just (s, v)
                  V.Eq (LitBits v) s | not $ isLit s -> Just (s, v)
                  _                                  -> Nothing

            isLit :: V.Exp -> Bool
            isLit = \ case
                  LitBits {} -> True
                  _          -> False

            -- Scrutinees compare by 'expKey': the derived V.Exp Eq is
            -- width-blind on embedded literals.
            chain :: V.Exp -> V.Exp -> ([(BV, V.Exp)], V.Exp)
            chain s = \ case
                  Cond c t f | Just (s', v) <- eqLit c, expKey s' == expKey s -> first ((v, t) :) $ chain s f
                  e                                                           -> ([], e)

            -- Width-exact distinctness (BV's Eq compares values only).
            distinct :: [BV] -> Bool
            distinct vs = length (nub $ map (\ v -> (width v, nat v)) vs) == length vs

-- | An always_comb case statement assigning the target (which must be
--   declared as a logic, not a net): one item per arm, the unconditioned
--   else as the default item (chains are total by construction, keeping
--   lint clean). The case is over a named wire (iverilog's always_comb
--   doesn't fully support constant selects in the implicit sensitivity
--   list); the scrutinee binds through 'bindLet', so names pass through
--   and repeated scrutinees share the wire.
caseAssign :: MonadState SigInfo m => V.LVal -> (V.Exp, [(BV, V.Exp)], V.Exp) -> m [V.Stmt]
caseAssign lv (scrut, arms, dflt) = do
      (scrut', sstmts) <- bindLet scrutSz "scrut" scrut
      (lps, label)     <- tagLabels scrut'
      pure $ sstmts <> lps <> [ AlwaysComb $ Case scrut' [ (label v, SeqAssign lv e) | (v, e) <- arms ]
                                           $ SeqAssign lv dflt ]
      where scrutSz :: M.Size
            scrutSz = case arms of
                  (v, _) : _ -> fromIntegral $ width v
                  _          -> 0

-- | Labels for case items over the given scrutinee: when it is the tagged
--   state register, emit one ST_* localparam per state (once per module;
--   localparams share the module namespace, so the names register with the
--   fresh-name supply) and label matching values with them; otherwise plain
--   literals.
tagLabels :: MonadState SigInfo m => V.Exp -> m ([V.Stmt], BV -> V.Exp)
tagLabels scrut = gets siTags >>= \ case
      Just (reg, sz, tags) | scrut == LVal (V.Name reg) -> gets siTagConsts >>= \ case
            Just consts -> pure ([], label consts)
            Nothing     -> do
                  consts <- mapM (\ (nm, v) -> (v, ) <$> fresh ("ST_" <> T.toUpper (mangleFresh nm))) tags
                  modify' $ \ si -> si { siTagConsts = Just consts }
                  pure ([ LocalParam (fromIntegral sz) c $ bitVec (fromIntegral sz) v | (v, c) <- consts ], label consts)
      _ -> pure ([], LitBits)
      where label :: [(Integer, V.Name)] -> BV -> V.Exp
            label consts v = maybe (LitBits v) (LVal . V.Name) $ lookup (nat v) consts

compileProgram :: forall m. MonadError AstError m => Config -> M.Program -> m V.Device
compileProgram conf (M.Program exts ds dev) = do
      top <- compileDevice conf xenv dev
      ds' <- mapM (compileDefn xenv) ds
      pure $ V.Device $ top : ds'
      where xenv :: XEnv
            xenv = Map.fromList $ map (\ e -> (extName e, e)) exts

compileDefn :: forall m. MonadError AstError m => XEnv -> M.Defn -> m V.Module
compileDefn xenv d@(M.Defn _ g (M.Sig _ argSzs _) ps body _ (Blind docs)) = do
      (stmts, si) <- flip runStateT (sigInfo0 Nothing ambient) $ do
            (e, estmts) <- compileExp xenv lenv body
            resStmts    <- if sizeOf body > 0 then resultStmts e else pure []
            pure $ estmts <> resStmts
      -- Header comments: the unmangled Hyle name, then the defn's doc lines.
      pure $ V.Module (mangleMod g) (g : docs) (inputs <> outputs) (siSigs si) stmts
      where -- | The result port drive: a case statement for a
            --   same-scrutinee chain ("res" is an output logic), otherwise
            --   a continuous assign.
            resultStmts :: MonadState SigInfo m' => V.Exp -> m' [V.Stmt]
            resultStmts e = case matchCaseChain e of
                  Just chain -> caseAssign (V.Name "res") chain
                  Nothing    -> pure [Assign (V.Name "res") e]

            -- Zero-width parameters and results are erased (doc/hyle.md,
            -- section 8.6): no ports, and references compile to nil.
            live :: [(M.Name, M.Size)]
            live = [ (x, sz) | (x, sz) <- zip ps argSzs, sz > 0 ]

            portNames :: [Text]
            portNames = defnPortNames d

            ambient :: [Text]
            ambient = portNames <> [ "res" | sizeOf body > 0 ]

            lenv :: LEnv
            lenv = Map.fromList $ [ (x, V.nil) | (x, 0) <- zip ps argSzs ]
                              <> zip (map fst live) (map (LVal . V.Name) portNames)

            inputs :: [Port]
            inputs = zipWith (curry $ Input . mkSignal) portNames $ map snd live

            outputs :: [Port]
            outputs = [ Output $ mkSignal ("res", sizeOf body) | sizeOf body > 0 ]

-- | Display names for a defn module's live (nonzero-width) parameters,
--   shared by the Verilog and VHDL backends: VHDL component declarations
--   must agree with the entity's port names (default binding matches
--   formals to entity ports by name), so both the entity and every call
--   site derive the same list from the defn alone. Each parameter name is
--   stripped of freshening tags and mangled; @arg<i>@ is the fallback for
--   names that are SystemVerilog keywords, don't start with a letter or
--   underscore, or duplicate an earlier port (positionally, compared
--   case-folded: VHDL basic identifiers are case-insensitive). The result
--   port is always @res@, seeded into the dedupe so no parameter takes it.
defnPortNames :: M.Defn -> [Text]
defnPortNames (M.Defn _ _ (M.Sig _ argSzs _) ps _ _ _) = go ["res"] $ zip [0 ..] [ p | (p, sz) <- zip ps argSzs, sz > 0 ]
      where go :: [Text] -> [(Int, Text)] -> [Text]
            go _     []              = []
            go taken ((i, p) : rest) = n : go (T.toLower n : taken) rest
                  where n :: Text
                        n | usable cand = cand
                          | otherwise   = fallback 0

                        cand :: Text
                        cand = mangleFresh $ stripFreshTag p

                        usable :: Text -> Bool
                        usable c = not (T.null c)
                                && not (isDigit $ T.head c)
                                && not (svReserved c)
                                && T.toLower c `notElem` taken

                        -- | @arg<i>@, itself suffixed out of the way in the
                        --   (unlikely) case a parameter is literally named
                        --   that.
                        fallback :: Int -> Text
                        fallback k | usable f  = f
                                   | otherwise = fallback $ k + 1
                              where f | k <= 0    = "arg" <> showt i
                                      | otherwise = "arg" <> showt i <> "_" <> showt k

-- | The text after the last '.' of a qualified name (e.g. @main.getIns@ ->
--   @getIns@): the seed for instance labels.
lastComponent :: Text -> Text
lastComponent = T.takeWhileEnd (/= '.')

-- | The register the device's tag table (devTags) names the values of.
tagReg :: Text
tagReg = "__resumption_tag"

compileDevice :: forall m. MonadError AstError m => Config -> XEnv -> M.Device -> m V.Module
compileDevice conf xenv (M.Device an top ins outs regs insts body (Blind tags)) = do
      -- N.B.: a device with registers but no configured clock (--no-clock)
      -- gets no register process, so the registers hold their initial
      -- values (the flags smoke tests exercise this combination; the user
      -- owns the consequences).
      (stmts, si) <- flip runStateT (sigInfo0 tagInfo ambient) $ do
            instWires <- Map.fromList . concat <$> mapM instOutWires insts
            (letStmts, lenv) <- foldStmts (ambientEnv instWires) body
            instStmts <- concat <$> mapM (compileInst instWires lenv) insts
            outStmts  <- concat <$> mapM (driveOut lenv) outs
            pure $ section "combinational logic" letStmts
                <> section "instances" instStmts
                <> section "outputs" outStmts
      pure $ V.Module top [] ports (siSigs si)
           $ regDecls <> stmts <> section "state register update" registerProcess
      where (mclk, mrst) = clockReset conf regs insts

            -- | A section banner, only above a nonempty statement group.
            section :: Text -> [V.Stmt] -> [V.Stmt]
            section _ []      = []
            section banner ss = Comment banner : ss

            -- | The tagged state register, when live and named by devTags.
            tagInfo :: Maybe (Text, M.Size, [(Text, Integer)])
            tagInfo = case [ (x, sz) | M.Register _ x sz _ <- liveRegs, x == tagReg ] of
                  [(x, sz)] | not $ null tags -> Just (x, sz, tags)
                  _                           -> Nothing

            -- | Register declarations in statement position, under a banner
            --   and a legend comment (width, initial value, and -- for the
            --   tagged state register -- the state display names).
            regDecls :: [V.Stmt]
            regDecls | null liveRegs = []
                     | otherwise     = Comment "state registers" : concatMap legend liveRegs <> map Decl regSigs

            legend :: M.Register -> [V.Stmt]
            legend (M.Register _ x sz bv) = Comment (x <> ": " <> showt sz <> " bits, init " <> showHex bv)
                  : [ Comment ("  states: " <> T.unwords [ showt v <> "=" <> nm | (nm, v) <- tags ]) | x == tagReg, not $ null tags ]

            ports :: [Port]
            ports = map (Input . mkSignal . (, 1)) (catMaybes [mclk, mrst])
                 <> map (Input . mkSignal) (live ins)
                 <> map (Output . mkSignal) (live outs)

            -- Zero-width wires, ports, and registers are erased
            -- (doc/hyle.md, section 8.6).
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

            regSigs :: [Signal]
            regSigs = concat [ [mkSignal (x, sz), mkSignal (x <> "_next", sz)] | M.Register _ x sz _ <- liveRegs ]

            -- | Ambient names: inputs and registers read by their own
            --   (port/signal) names, instance outputs by their wires.
            ambientEnv :: HashMap (M.Name, M.Name) V.Name -> LEnv
            ambientEnv instWires = Map.fromList $
                     [ (x, if sz > 0 then LVal $ V.Name x else V.nil) | (x, sz) <- ins ]
                  <> [ (x, if sz > 0 then LVal $ V.Name x else V.nil) | M.Register _ x sz _ <- regs ]
                  <> [ (x <> "." <> q, LVal $ V.Name w) | ((x, q), w) <- Map.toList instWires ]

            -- | One wire per output port of each instance, keyed by
            --   (instance, port).
            instOutWires :: MonadState SigInfo m' => M.Instance -> m' [((M.Name, M.Name), V.Name)]
            instOutWires (M.Instance _ x ex _) = case Map.lookup ex xenv of
                  Nothing -> pure []
                  Just e  -> mapM (\ (q, sz) -> ((x, q), ) <$> newWire sz (x <> "_" <> q)) $ filter ((> 0) . snd) $ extOutputs e

            instWire :: HashMap (M.Name, M.Name) V.Name -> M.Name -> M.Name -> V.Name
            instWire m x q = fromMaybe (x <> "_" <> q) $ Map.lookup (x, q) m

            foldStmts :: (MonadState SigInfo m', MonadError AstError m') => LEnv -> [M.Stmt] -> m' ([V.Stmt], LEnv)
            foldStmts lenv = \ case
                  [] -> pure ([], lenv)
                  M.SLet _ x e : rest | M.isNil e -> foldStmts (Map.insert x V.nil lenv) rest
                  M.SLet _ x e : rest -> do
                        (e', stmts)    <- compileExp xenv lenv e
                        (xv, bstmts)   <- bindLet (sizeOf e) x e'
                        (rest', lenv') <- foldStmts (Map.insert x xv lenv) rest
                        pure (stmts <> bstmts <> rest', lenv')
                  M.SNext _ _ e : rest | M.isNil e -> foldStmts lenv rest
                  M.SNext _ x e : rest -> do
                        (e', stmts) <- compileExp xenv lenv e
                        (rest', lenv') <- foldStmts lenv rest
                        pure (stmts <> [Assign (V.Name $ x <> "_next") e'] <> rest', lenv')
                  M.SOutput {} : rest -> foldStmts lenv rest -- emitted after the lets, in port order
                  M.SInstIn {} : rest -> foldStmts lenv rest -- emitted with the instantiations

            driveOut :: (MonadState SigInfo m', MonadError AstError m') => LEnv -> (M.Name, M.Size) -> m' [V.Stmt]
            driveOut _ (_, 0) = pure []
            driveOut lenv (x, _) = case [ e | M.SOutput _ x' e <- body, x' == x ] of
                  [e] -> do
                        (e', stmts) <- compileExp xenv lenv e
                        pure $ stmts <> [Assign (V.Name x) e']
                  _   -> failInternal an $ "internal error: output " <> x <> " is not driven exactly once"

            compileInst :: (MonadState SigInfo m', MonadError AstError m') => HashMap (M.Name, M.Name) V.Name -> LEnv -> M.Instance -> m' [V.Stmt]
            compileInst instWires lenv (M.Instance an' x ex cs) = case Map.lookup ex xenv of
                  Nothing -> failAt an' $ "instance " <> x <> ": unknown extern: " <> ex
                  Just e  -> do
                        (args, stmts) <- (map fst &&& concatMap snd) <$> mapM (driveIn lenv x) (filter ((> 0) . snd) $ extInputs e)
                        clkrst <- clockRstPorts e
                        inst   <- fresh x
                        -- Positional connections (clock, reset, inputs,
                        -- outputs, in declaration order): the hand-written
                        -- implementations don't know synthesized port names.
                        let outws = [ LVal $ V.Name $ instWire instWires x q | (q, sz) <- extOutputs e, sz > 0 ]
                        pure $ stmts <> [ Instantiate ex inst (zip (extGenerics e) $ map (LitBits . bitVec 32 . toInteger) cs)
                                        $ map (mempty, ) $ map snd clkrst <> map snd args <> outws ]

            clockRstPorts :: MonadError AstError m' => Extern -> m' [(V.Name, V.Exp)]
            clockRstPorts e = case extKind e of
                  Comb      -> pure []
                  Seq mc mr -> do
                        c <- wire "clock" mc mclk
                        r <- wire "reset" mr mrst
                        pure $ catMaybes [c, r]
                  where wire :: MonadError AstError m' => Text -> Maybe M.Name -> Maybe Text -> m' (Maybe (V.Name, V.Exp))
                        wire what p sig = case (p, sig) of
                              (Nothing, _)      -> pure Nothing
                              (Just p', Just s) -> pure $ Just (p', LVal $ V.Name s)
                              (Just _, Nothing) -> failAt (ann e) $ "external module requires a " <> what <> " signal, but we have no " <> what <> " to give it."

            driveIn :: (MonadState SigInfo m', MonadError AstError m') => LEnv -> M.Name -> (M.Name, M.Size) -> m' ((V.Name, V.Exp), [V.Stmt])
            driveIn lenv x (p, _) = case [ e | M.SInstIn _ x' p' e <- body, x' == x, p' == p ] of
                  [e] -> do
                        (e', stmts) <- compileExp xenv lenv e
                        pure ((p, e'), stmts)
                  _   -> failInternal an $ "internal error: instance input " <> x <> "." <> p <> " is not driven exactly once"

            -- | The single clocked process updating the registers (one
            --   nonblocking assign per register; the reset arm reloads the
            --   per-register initials), plus per-register power-on initials.
            registerProcess :: [V.Stmt]
            registerProcess = case (liveRegs, mclk) of
                  ([], _)        -> []
                  (_, Nothing)   -> [] -- rejected above
                  (_, Just clk)  ->
                        [ Initial $ SeqAssign (V.Name x) $ bvToExp bv | M.Register _ x _ bv <- liveRegs ]
                        <> [ Always (Pos clk : rstEdge) $ Block $ case mrst of
                                    Just rst -> [ IfElse (V.Eq (LVal $ V.Name rst) $ LitBits $ bitVec 1 $ fromEnum $ not invertRst)
                                                      (Block rstAssigns)
                                                      (Block nextAssigns) ]
                                    Nothing  -> nextAssigns ]

            rstAssigns, nextAssigns :: [V.Stmt]
            rstAssigns  = [ ParAssign (V.Name x) $ bvToExp bv | M.Register _ x _ bv <- liveRegs ]
            nextAssigns = [ ParAssign (V.Name x) $ LVal $ V.Name $ x <> "_next" | M.Register _ x _ _ <- liveRegs ]

            rstEdge :: [Sensitivity]
            rstEdge = case mrst of
                  Nothing              -> []
                  Just _ | syncRst     -> []
                  Just rst | invertRst -> [Neg rst]
                  Just rst             -> [Pos rst]

            invertRst, syncRst :: Bool
            invertRst = Inverted `elem` (conf^.C.resetFlags)
            syncRst   = Synchronous `elem` (conf^.C.resetFlags)

-- | Clock and reset port names, when the device has them: no clock when
--   there are no registers and no instances; no reset without a clock.
clockReset :: Config -> [M.Register] -> [M.Instance] -> (Maybe Text, Maybe Text)
clockReset conf regs insts
      | null regs && null insts = (Nothing, Nothing)
      | T.null (conf^.C.clock)  = (Nothing, Nothing)
      | T.null (conf^.C.reset)  = (Just $ conf^.C.clock, Nothing)
      | otherwise               = (Just $ conf^.C.clock, Just $ conf^.C.reset)

---

compileExps :: (MonadState SigInfo m, MonadError AstError m) => XEnv -> LEnv -> [M.Exp] -> m ([V.Exp], [V.Stmt])
compileExps xenv lenv es = (map fst &&& concatMap snd) <$> mapM (compileExp xenv lenv) es

compileExp :: forall m. (MonadState SigInfo m, MonadError AstError m) => XEnv -> LEnv -> M.Exp -> m (V.Exp, [V.Stmt])
compileExp xenv = go
      where go :: LEnv -> M.Exp -> m (V.Exp, [V.Stmt])
            go lenv = \ case
                  M.Lit _ bv          -> pure (bvToExp bv, [])
                  M.Undef _ sz        -> pure (bvToExp $ zeros $ fromIntegral sz, [])
                  M.Var an _ x        -> case Map.lookup x lenv of
                        Just e  -> pure (e, [])
                        Nothing -> failInternal an $ "unbound variable: " <> x
                  e@M.Cat {}          -> first V.cat <$> compileExps xenv lenv (gather e)
                  M.Slice _ i k e     -> do
                        (e', stmts) <- go lenv e
                        (e'', stmts') <- sliceExp (sizeOf e) i k e'
                        pure (e'', stmts <> stmts')
                  M.Prim an sz op es  -> compilePrim lenv an sz op es
                  M.Call _ 0 _ _      -> pure (V.nil, []) -- zero-width call: dead logic, erased
                  M.Call _ sz g es    -> do
                        (es', stmts) <- compileExps xenv lenv $ filter (not . M.isNil) es
                        -- The instantiate memo: an identical call already
                        -- has an instance in this module; reuse its result
                        -- wire (defn modules are pure, so identical
                        -- arguments mean identical outputs).
                        gets (OMap.lookup (g, map expKey es') . siCalls) >>= \ case
                              Just mr -> pure (LVal $ V.Name mr, stmts)
                              Nothing -> do
                                    mr   <- newWire sz $ g <> "_out"
                                    inst <- fresh $ lastComponent g <> "_i"
                                    modify' $ \ si -> si { siCalls = OMap.insert (g, map expKey es') mr $ siCalls si }
                                    pure (LVal $ V.Name mr, stmts <> [Instantiate (mangleMod g) inst [] $ map (mempty, ) $ es' <> [LVal $ V.Name mr]])
                  -- Extern ports connect positionally (declaration order):
                  -- the hand-written Verilog implementations don't know the
                  -- synthesized names of anonymous ports.
                  M.XCall an sz x cs es -> case Map.lookup x xenv of
                        Nothing -> failAt an $ "unknown extern: " <> x
                        Just ex -> do
                              (es', stmts) <- compileExps xenv lenv $ filter (not . M.isNil) es
                              mr           <- newWire sz "extres"
                              inst         <- fresh $ x <> "_i"
                              pure ( LVal $ V.Name mr
                                   , stmts <> [Instantiate x inst (zip (extGenerics ex) $ map (LitBits . bitVec 32 . toInteger) cs)
                                          $ map (mempty, ) $ es' <> map snd (outRanges mr (extOutputs ex))])
                  M.If _ _ c t e      -> do
                        (c', stmts)   <- go lenv c
                        (t', stmts')  <- go lenv t
                        (e', stmts'') <- go lenv e
                        pure (Cond c' t' e', stmts <> stmts' <> stmts'')
                  M.Let _ _ x e1 e2
                        | M.isNil e1  -> go (Map.insert x V.nil lenv) e2
                        | otherwise -> do
                              (e1', stmts)  <- go lenv e1
                              (xv, bstmts)  <- bindLet (sizeOf e1) x e1'
                              (e2', stmts') <- go (Map.insert x xv lenv) e2
                              pure (e2', stmts <> bstmts <> stmts')

            compilePrim :: LEnv -> Annote -> M.Size -> Op -> [M.Exp] -> m (V.Exp, [V.Stmt])
            compilePrim lenv an _sz op es = do
                  (es', stmts) <- compileExps xenv lenv es
                  let pure' e = pure (e, stmts)
                  case (op, es', es) of
                        (M.Add   , [a, b], _) -> pure' $ V.Add a b
                        (M.Sub   , [a, b], _) -> pure' $ V.Sub a b
                        (M.Mul   , [a, b], _) -> pure' $ V.Mul a b
                        (M.Pow   , [a, b], _) -> pure' $ V.Pow a b
                        (M.UDiv  , [a, b], [_, mb]) -> pure' $ Cond (V.Eq b $ bvToExp $ zeros $ fromIntegral $ sizeOf mb) (bvToExp $ ones $ fromIntegral $ sizeOf mb) (V.Div a b)
                        (M.UMod  , [a, b], [_, mb]) -> pure' $ Cond (V.Eq b $ bvToExp $ zeros $ fromIntegral $ sizeOf mb) a (V.Mod a b)
                        (M.And   , [a, b], _) -> pure' $ V.And a b
                        (M.Or    , [a, b], _) -> pure' $ V.Or a b
                        (M.XOr   , [a, b], _) -> pure' $ V.XOr a b
                        (M.Not   , [a], _)    -> pure' $ V.Not a
                        (M.Shl   , [a, b], _) -> pure' $ V.LShift a b
                        (M.LShr  , [a, b], _) -> pure' $ V.RShift a b
                        -- $unsigned(.) isolates the signed shift from the
                        -- parent expression's signedness context (function
                        -- arguments are self-determined); without it, an
                        -- enclosing unsigned operation turns >>> logical.
                        (M.AShr  , [a, b], _) -> pure' $ V.Unsigned $ V.RShiftArith a b
                        (M.Eq    , [a, b], _) -> pure' $ V.Eq a b
                        (M.Ne    , [a, b], _) -> pure' $ V.NEq a b
                        (M.ULt   , [a, b], _) -> pure' $ V.Lt a b
                        (M.ULe   , [a, b], _) -> pure' $ V.LtEq a b
                        (M.UGt   , [a, b], _) -> pure' $ V.Gt a b
                        (M.UGe   , [a, b], _) -> pure' $ V.GtEq a b
                        (M.SLt   , [a, b], _) -> pure' $ V.Lt (Signed a) (Signed b)
                        (M.SLe   , [a, b], _) -> pure' $ V.LtEq (Signed a) (Signed b)
                        (M.SGt   , [a, b], _) -> pure' $ V.Gt (Signed a) (Signed b)
                        (M.SGe   , [a, b], _) -> pure' $ V.GtEq (Signed a) (Signed b)
                        (M.RedAnd, [a], _)    -> pure' $ V.RAnd a
                        (M.RedOr , [a], _)    -> pure' $ V.ROr a
                        (M.RedXOr, [a], _)    -> pure' $ V.RXOr a
                        (M.ZExt m, [a], [ma])
                              | m == sizeOf ma -> pure' a
                              | otherwise      -> pure' $ V.cat [bvToExp $ zeros $ fromIntegral (m - sizeOf ma), a]
                        (M.Trunc m, [a], [ma]) -> do
                              (a', stmts') <- sliceExp (sizeOf ma) 0 m a
                              pure (a', stmts <> stmts')
                        (M.SExt m, [a], [ma])
                              | m == sizeOf ma -> pure' a
                              | otherwise      -> do
                                    (msb, stmts') <- sliceExp (sizeOf ma) (fromIntegral (sizeOf ma) - 1) 1 a
                                    pure (V.cat [Repl (toLit $ fromIntegral $ m - sizeOf ma) msb, a], stmts <> stmts')
                        (M.Rep k, [a], _)
                              | k == 0    -> pure' V.nil
                              | otherwise -> pure' $ Repl (toLit k) a
                        _ -> failInternal an $ "ill-formed primitive application: " <> opName op <> " with " <> showt (length es) <> " arguments"

            -- | Slice a compiled expression: ranges on names and literals
            --   directly; anything else through a fresh wire (declared at
            --   use).
            sliceExp :: M.Size -> M.Index -> M.Size -> V.Exp -> m (V.Exp, [V.Stmt])
            sliceExp w i k e
                  | k == 0          = pure (V.nil, [])
                  | i == 0, k == w  = pure (e, [])
                  | otherwise = case e of
                        LVal (V.Name n)      -> pure (LVal $ mkRange n i' (i' + k' - 1), [])
                        LVal (V.Range n a _) -> pure (LVal $ mkRange n (a + i') (a + i' + k' - 1), [])
                        LitBits bv           -> pure (LitBits $ subRange (fromIntegral i, fromIntegral (i + fromIntegral k) - 1) bv, [])
                        _                    -> do
                              n <- fresh "slice_in"
                              pure (LVal $ mkRange n i' (i' + k' - 1), [WireAssign (fromIntegral w) n e])
                  where i', k' :: V.Index
                        i' = fromIntegral i
                        k' = fromIntegral k

-- | The output ports of an extern call, as ranges of the result wire
--   (MSB-first in declaration order).
outRanges :: V.Name -> [(M.Name, M.Size)] -> [(V.Name, V.Exp)]
outRanges mr qs = [ (q, LVal $ mkRange mr off (off + fromIntegral sz - 1)) | (q, sz, off) <- offsets, sz > 0 ]
      where offsets :: [(M.Name, M.Size, V.Index)]
            offsets = snd $ foldr (\ (q, sz) (o, acc) -> (o + fromIntegral sz, (q, sz, o) : acc)) (0 :: V.Index, []) qs

mkRange :: V.Name -> V.Index -> V.Index -> LVal
mkRange n i j | i == j    = Element n i
              | otherwise = Range n i j

mkSignal :: (Text, M.Size) -> Signal
mkSignal (n, sz) = Logic [fromIntegral sz] n []

toLit :: Natural -> V.Exp
toLit v = LitBits $ bitVec (fromIntegral $ szBitRep v) v

-- | Break up giant literals (as the Core backend does).
bvToExp :: BV -> V.Exp
bvToExp bv | width bv == 0         = V.nil
           | width bv < maxLit     = LitBits bv
           | bv == zeros 1         = Repl (toLit $ fromIntegral $ width bv) $ LitBits $ zeros 1
           | bv == ones (width bv) = Repl (toLit $ fromIntegral $ width bv) $ LitBits $ ones 1
           | zs > 8                = V.cat [LitBits $ subRange (fromIntegral zs, width bv - 1) bv, Repl (toLit zs) $ LitBits $ zeros 1]
           | otherwise             = LitBits bv
      where zs :: Natural
            zs | bv == zeros 1 = fromIntegral $ width bv
               | otherwise     = fromIntegral $ lsb1 bv

            maxLit :: Int
            maxLit = 32

---

-- | A testbench driving the device with interp-style inputs and printing
--   outputs each cycle in the interpreter's YAML format (same protocol and
--   timing as the Core backend's testbench).
testbench :: Config -> M.Device -> [Ins] -> V.Module
testbench conf dev inps = V.Module "tb" [] []
      (  map mkSignal (clkRst <> ins)
      <> map (\ (n, sz) -> Wire [fromIntegral sz] n []) outs )
      (  Instantiate (devName dev) "dut" [] (map ((mempty, ) . LVal . V.Name . fst) $ clkRst <> ins <> outs)
      :  [ Initial $ Block $ resetStmts <> concatMap cyc inps <> [Finish] ] )
      where (mclk, mrst) = clockReset conf (devRegisters dev) (devInstances dev)

            clkRst, ins, outs :: [(Text, M.Size)]
            clkRst = map (, 1) $ catMaybes [mclk, mrst]
            ins    = filter ((> 0) . snd) $ devInputs dev
            outs   = filter ((> 0) . snd) $ devOutputs dev

            drive :: Ins -> [V.Stmt]
            drive i = map (\ (n, sz) -> SeqAssign (V.Name n) $ LitBits $ bitVec (fromIntegral sz) $ inputValue (fromIntegral sz) i n) ins

            bit :: Bool -> V.Exp
            bit = LitBits . bitVec 1 . fromEnum

            rstActive :: Bool
            rstActive = Inverted `notElem` (conf^.C.resetFlags)

            tick :: Text -> [V.Stmt]
            tick clk = [Delay 1, SeqAssign (V.Name clk) $ bit True, Delay 5, SeqAssign (V.Name clk) $ bit False]

            resetStmts :: [V.Stmt]
            resetStmts = case (mclk, mrst) of
                  (Just clk, Just rst) ->
                        [ SeqAssign (V.Name clk) $ bit False, SeqAssign (V.Name rst) $ bit rstActive ]
                        <> drive (headIns inps)
                        <> concat (replicate 2 [Delay 5, SeqAssign (V.Name clk) $ bit True, Delay 5, SeqAssign (V.Name clk) $ bit False])
                        <> [ SeqAssign (V.Name rst) $ bit $ not rstActive ]
                  (Just clk, Nothing)  -> [ SeqAssign (V.Name clk) $ bit False ]
                  _                    -> []

            cyc :: Ins -> [V.Stmt]
            cyc i = drive i <> case mclk of
                  Just clk -> [Delay 4] <> disps <> tick clk
                  Nothing  -> [Delay 5] <> disps <> [Delay 5]

            disps :: [V.Stmt]
            disps = zipWith (\ pre (n, _) -> Display (pre <> n <> ": '0x%0h'") [LVal $ V.Name n]) yamlPrefixes outs

            headIns :: [Ins] -> Ins
            headIns = \ case
                  i : _ -> i
                  _     -> mempty

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
import ReWire.Hyle.Mangle (mangleFresh, mangleMod, pickFresh, seedNames)
import ReWire.Error (failAt, failInternal, AstError, MonadError)
import ReWire.Hyle.Syntax as M
import ReWire.Hyle.ToVerilog (clockReset)
import ReWire.Pretty (showt)

import qualified ReWire.BitVector  as BV
import qualified ReWire.Config     as C
import qualified ReWire.VHDL.Syntax as H

import Control.Arrow ((&&&), first)
import Control.Lens ((^.))
import Control.Monad.State.Strict (MonadState, runStateT, modify', gets)
import Data.HashMap.Strict (HashMap)
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T

-- | Local value environment.
type LEnv = HashMap M.Name H.Exp

-- | Extern declarations, by name.
type XEnv = HashMap M.Name Extern

data TS = TS
      { tsFresh :: !(HashMap Text Int)
      , tsSigs  :: ![H.Signal]
      , tsComps :: !(HashMap Text H.Component)
      }

-- | Initial state, with the ambient names (ports, registers -- emitted
--   verbatim, never passing through 'fresh') seeded into the used map so
--   generated names can't collide with them. Seeds are case-folded to match
--   the case-folding in 'fresh' (VHDL basic identifiers are
--   case-insensitive).
ts0 :: [Text] -> TS
ts0 ambient = TS (seedNames $ map T.toLower ambient) [] mempty

-- | A fresh signal/instance name: mangled, case-folded (the collision
--   defense for VHDL's case-insensitive namespace), and suffixed out of the
--   way of every name already issued or seeded (reserved words need no
--   dodge here -- the pretty-printer escapes them as extended identifiers).
fresh :: MonadState TS m => Text -> m H.Name
fresh s = do
      used <- gets tsFresh
      let (n, used') = pickFresh "R" used $ T.toLower $ mangleFresh s
      modify' $ \ ts -> ts { tsFresh = used' }
      pure n

newWire :: MonadState TS m => M.Size -> Text -> m H.Name
newWire sz n = do
      n' <- fresh n
      modify' $ \ ts -> ts { tsSigs = tsSigs ts <> [H.Signal n' sz Nothing] }
      pure n'

addComponent :: MonadState TS m => H.Component -> m ()
addComponent c@(H.Component n _ _) = modify' $ \ ts -> ts { tsComps = Map.insert n c $ tsComps ts }

components :: TS -> [H.Component]
components = sortOn (\ (H.Component n _ _) -> n) . Map.elems . tsComps

compileProgram :: forall m. MonadError AstError m => Config -> M.Program -> m H.Device
compileProgram conf (M.Program exts ds dev) = do
      top <- compileDevice conf xenv dev
      ds' <- mapM (compileDefn conf xenv) ds
      pure $ H.Device $ top : ds'
      where xenv :: XEnv
            xenv = Map.fromList $ map (\ e -> (extName e, e)) exts

compileDefn :: forall m. MonadError AstError m => Config -> XEnv -> M.Defn -> m H.Unit
compileDefn conf xenv (M.Defn _ g (M.Sig _ argSzs _) ps body _ _) = do
      ((e, stmts), ts) <- flip runStateT (ts0 $ argNames <> [ "res" | sizeOf body > 0 ]) $ compileExp xenv lenv body
      pure $ H.Unit (mangleMod g) (unitPackages conf)
                    (map (\ (pn, sz) -> H.Port pn H.In sz) (zip argNames $ map snd live) <> [ H.Port "res" H.Out (sizeOf body) | sizeOf body > 0 ])
                    (components ts) (tsSigs ts)
                    (stmts <> [ H.Assign (H.LVName "res") e | sizeOf body > 0 ])
      where -- Zero-width parameters and results are erased; references
            -- compile to the empty literal.
            live :: [(M.Name, M.Size)]
            live = [ (x, sz) | (x, sz) <- zip ps argSzs, sz > 0 ]

            argNames :: [Text]
            argNames = zipWith (\ _ i -> "arg" <> showt (i :: Int)) live [0 ..]

            lenv :: LEnv
            lenv = Map.fromList $ [ (x, H.Lit BV.nil) | (x, 0) <- zip ps argSzs ]
                              <> zip (map fst live) (map H.Var argNames)

compileDevice :: forall m. MonadError AstError m => Config -> XEnv -> M.Device -> m H.Unit
compileDevice conf xenv (M.Device an top ins outs regs insts body _) = do
      (stmts, ts) <- flip runStateT (ts0 ambient) $ do
            instWires <- Map.fromList . concat <$> mapM instOutWires insts
            (letStmts, lenv) <- foldStmts (ambientEnv instWires) body
            instStmts <- concat <$> mapM (compileInst instWires lenv) insts
            outStmts  <- concat <$> mapM (driveOut lenv) outs
            pure $ letStmts <> instStmts <> outStmts
      pure $ H.Unit top (unitPackages conf)
                   (map (portIn . (, 1)) (catMaybes [mclk, mrst]) <> map portIn (live ins) <> map portOut (live outs))
                   (components ts)
                   (regSigs <> tsSigs ts)
                   (stmts <> stateProcess)
      where (mclk, mrst) = clockReset conf regs insts

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
                        (e', stmts) <- compileExp xenv lenv e
                        n           <- newWire (sizeOf e) x
                        (rest', lenv') <- foldStmts (Map.insert x (H.Var n) lenv) rest
                        pure (stmts <> [H.Assign (H.LVName n) e'] <> rest', lenv')
                  M.SNext _ _ e : rest | M.isNil e -> foldStmts lenv rest
                  M.SNext _ x e : rest -> do
                        (e', stmts) <- compileExp xenv lenv e
                        (rest', lenv') <- foldStmts lenv rest
                        pure (stmts <> [H.Assign (H.LVName $ x <> "_next") e'] <> rest', lenv')
                  M.SOutput {} : rest -> foldStmts lenv rest -- emitted after the lets, in port order
                  M.SInstIn {} : rest -> foldStmts lenv rest -- emitted with the instantiations

            driveOut :: (MonadState TS m', MonadError AstError m') => LEnv -> (M.Name, M.Size) -> m' [H.Stmt]
            driveOut _ (_, 0) = pure []
            driveOut lenv (x, _) = case [ e | M.SOutput _ x' e <- body, x' == x ] of
                  [e] -> do
                        (e', stmts) <- compileExp xenv lenv e
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
                        (e', stmts)  <- compileExp xenv lenv e
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

compileExps :: (MonadState TS m, MonadError AstError m) => XEnv -> LEnv -> [M.Exp] -> m ([H.Exp], [H.Stmt])
compileExps xenv lenv es = (map fst &&& concatMap snd) <$> mapM (compileExp xenv lenv) es

compileExp :: forall m. (MonadState TS m, MonadError AstError m) => XEnv -> LEnv -> M.Exp -> m (H.Exp, [H.Stmt])
compileExp xenv = go
      where go :: LEnv -> M.Exp -> m (H.Exp, [H.Stmt])
            go lenv = \ case
                  M.Lit _ bv          -> pure (litExp bv, [])
                  M.Undef _ sz        -> pure (litExp $ zeros $ fromIntegral sz, [])
                  M.Var an _ x        -> case Map.lookup x lenv of
                        Just e  -> pure (e, [])
                        Nothing -> failInternal an $ "unbound variable: " <> x
                  e@M.Cat {}          -> first hCat <$> compileExps xenv lenv (gather e)
                  M.Slice _ i k e     -> do
                        (e', stmts) <- go lenv e
                        (e'', stmts') <- sliceExp (sizeOf e) i k e'
                        pure (e'', stmts <> stmts')
                  M.Prim an sz op es  -> compilePrim lenv an sz op es
                  M.Call _ 0 _ _      -> pure (H.Lit BV.nil, []) -- zero-width call: dead logic, erased
                  M.Call _ sz g es    -> do
                        let esLive = filter (not . M.isNil) es
                        (es', stmts)    <- compileExps xenv lenv esLive
                        (conns, hoists) <- (map fst &&& concatMap snd) <$> mapM (uncurry connect) (zip (map sizeOf esLive) es')
                        addComponent $ H.Component (mangleMod g) []
                              $  zipWith (\ i sz' -> H.Port ("arg" <> showt (i :: Int)) H.In sz') [0 ..] (map sizeOf esLive)
                              <> [H.Port "res" H.Out sz]
                        mr   <- newWire sz $ g <> "_out"
                        inst <- fresh "inst"
                        pure (H.Var mr, stmts <> hoists <> [H.Instantiate (mangleMod g) inst [] $ map (mempty, ) $ conns <> [H.Var mr]])
                  M.XCall an sz x cs es -> case Map.lookup x xenv of
                        Nothing -> failAt an $ "unknown extern: " <> x
                        Just ex -> do
                              addComponent $ extComponent ex
                              let esLive = filter (not . M.isNil) es
                                  inPorts = filter ((> 0) . snd) $ extInputs ex
                              (es', stmts)    <- compileExps xenv lenv esLive
                              (conns, hoists) <- (map fst &&& concatMap snd) <$> mapM (uncurry connect) (zip (map sizeOf esLive) es')
                              mr   <- newWire sz "extres"
                              inst <- fresh "inst"
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
                              n             <- newWire (sizeOf e1) x
                              (e2', stmts') <- go (Map.insert x (H.Var n) lenv) e2
                              pure (e2', stmts <> [H.Assign (H.LVName n) e1'] <> stmts')

            -- | The output ports of an extern call, associated with slices
            --   of the result wire (MSB-first in declaration order).
            outSlices :: H.Name -> [(M.Name, M.Size)] -> [(H.Name, H.Exp)]
            outSlices mr qs = [ (q, H.Slice mr off (off + fromIntegral sz - 1)) | (q, sz, off) <- offsets, sz > 0 ]
                  where offsets :: [(M.Name, M.Size, H.Index)]
                        offsets = snd $ foldr (\ (q, sz) (o, acc) -> (o + fromIntegral sz, (q, sz, o) : acc)) (0 :: H.Index, []) qs

            compilePrim :: LEnv -> Annote -> M.Size -> Op -> [M.Exp] -> m (H.Exp, [H.Stmt])
            compilePrim lenv an _sz op es = do
                  (es', stmts) <- compileExps xenv lenv es
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
testbench conf dev inps = H.Unit "tb"
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

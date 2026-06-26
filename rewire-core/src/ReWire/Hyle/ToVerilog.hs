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
module ReWire.Hyle.ToVerilog (compileProgram, testbench, clockReset) where

import ReWire.Annotation (Annote)
import ReWire.BitVector (BV, width, bitVec, zeros, ones, lsb1, szBitRep)
import ReWire.Config (Config, ResetFlag (..))
import ReWire.Hyle.Interp (Ins, subRange, inputValue, yamlPrefixes)
import ReWire.Hyle.Mangle (mangleFresh, mangleMod)
import ReWire.Error (failAt, failInternal, AstError, MonadError)
import ReWire.Hyle.Syntax as M
import ReWire.Pretty (showt)
import ReWire.Verilog.Syntax as V

import qualified ReWire.Config as C

import Control.Arrow ((&&&), first, second)
import Control.Lens ((^.))

import Control.Monad.State.Strict (MonadState, runStateT, modify', gets)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T

type SigInfo = (HashMap Text Int, [Signal])

-- | Local value environment.
type LEnv = HashMap M.Name V.Exp

-- | Extern declarations, by name.
type XEnv = HashMap M.Name Extern

fresh :: MonadState SigInfo m => Text -> m V.Name
fresh s = do
      let s' = T.toLower $ mangleFresh s
      ctr <- gets $ fromMaybe 0 . Map.lookup s' . fst
      modify' $ first $ Map.insert s' $ ctr + 1
      pure $ s' <> (if ctr > 0 then "R" <> showt ctr else mempty)

newWire :: MonadState SigInfo m => M.Size -> Text -> m V.Name
newWire sz n = do
      n' <- fresh n
      modify' $ second (<> [mkSignal (n', sz)])
      pure n'

compileProgram :: forall m. MonadError AstError m => Config -> M.Program -> m V.Device
compileProgram conf (M.Program exts ds dev) = do
      top <- compileDevice conf xenv dev
      ds' <- mapM (compileDefn xenv) ds
      pure $ V.Device $ top : ds'
      where xenv :: XEnv
            xenv = Map.fromList $ map (\ e -> (extName e, e)) exts

compileDefn :: forall m. MonadError AstError m => XEnv -> M.Defn -> m V.Module
compileDefn xenv (M.Defn _ g (M.Sig _ argSzs _) ps body) = do
      ((e, stmts), (_, sigs)) <- flip runStateT (mempty, []) $ compileExp xenv lenv body
      pure $ V.Module (mangleMod g) (inputs <> outputs) sigs
           $ stmts <> [ Assign (V.Name "res") e | sizeOf body > 0 ]
      where -- Zero-width parameters and results are erased (doc/hyle.md,
            -- section 8.6): no ports, and references compile to nil.
            live :: [(M.Name, M.Size)]
            live = [ (x, sz) | (x, sz) <- zip ps argSzs, sz > 0 ]

            argNames :: [Text]
            argNames = zipWith (\ _ i -> "arg" <> showt (i :: Int)) live [0 ..]

            lenv :: LEnv
            lenv = Map.fromList $ [ (x, V.nil) | (x, 0) <- zip ps argSzs ]
                              <> zip (map fst live) (map (LVal . V.Name) argNames)

            inputs :: [Port]
            inputs = zipWith (curry $ Input . mkSignal) argNames $ map snd live

            outputs :: [Port]
            outputs = [ Output $ mkSignal ("res", sizeOf body) | sizeOf body > 0 ]

compileDevice :: forall m. MonadError AstError m => Config -> XEnv -> M.Device -> m V.Module
compileDevice conf xenv (M.Device an top ins outs regs insts body) = do
      -- N.B.: a device with registers but no configured clock (--no-clock)
      -- gets no register process, so the registers hold their initial
      -- values (the flags smoke tests exercise this combination; the user
      -- owns the consequences).
      (stmts, (_, sigs)) <- flip runStateT (mempty, []) $ do
            instWires <- Map.fromList . concat <$> mapM instOutWires insts
            (letStmts, lenv) <- foldStmts (ambientEnv instWires) body
            instStmts <- concat <$> mapM (compileInst instWires lenv) insts
            outStmts  <- concat <$> mapM (driveOut lenv) outs
            pure $ letStmts <> instStmts <> outStmts
      pure $ V.Module top ports (regSigs <> sigs) $ stmts <> registerProcess
      where (mclk, mrst) = clockReset conf regs insts

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
                        (e', stmts) <- compileExp xenv lenv e
                        lv          <- newWire (sizeOf e) x
                        (rest', lenv') <- foldStmts (Map.insert x (LVal $ V.Name lv) lenv) rest
                        pure (stmts <> [Assign (V.Name lv) e'] <> rest', lenv')
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
                              (Just _, Nothing) -> failAt an $ "external module requires a " <> what <> " signal, but we have no " <> what <> " to give it."

            driveIn :: (MonadState SigInfo m', MonadError AstError m') => LEnv -> M.Name -> (M.Name, M.Size) -> m' ((V.Name, V.Exp), [V.Stmt])
            driveIn lenv x (p, _) = case [ e | M.SInstIn _ x' p' e <- body, x' == x, p' == p ] of
                  [e] -> do
                        (e', stmts) <- compileExp xenv lenv e
                        pure ((p, e'), stmts)
                  _   -> failInternal an $ "internal error: instance input " <> x <> "." <> p <> " is not driven exactly once"

            -- | The single clocked process updating all registers, plus
            --   power-on initials.
            registerProcess :: [V.Stmt]
            registerProcess = case (liveRegs, mclk) of
                  ([], _)        -> []
                  (_, Nothing)   -> [] -- rejected above
                  (_, Just clk)  ->
                        [ Initial $ SeqAssign lvCurr initExp
                        , Always (Pos clk : rstEdge) $ Block [ ifRst ]
                        ]

            initExp :: V.Exp
            initExp = bvToExp $ mconcat [ bv | M.Register _ _ _ bv <- liveRegs ]

            lvCurr, lvNext :: LVal
            lvCurr = mkLVals [ V.Name x | M.Register _ x _ _ <- liveRegs ]
            lvNext = mkLVals [ V.Name $ x <> "_next" | M.Register _ x _ _ <- liveRegs ]

            ifRst :: V.Stmt
            ifRst = case mrst of
                  Just rst -> IfElse (V.Eq (LVal $ V.Name rst) $ LitBits $ bitVec 1 $ fromEnum $ not invertRst)
                        (Block [ ParAssign lvCurr initExp ])
                        (Block [ ParAssign lvCurr $ LVal lvNext ])
                  Nothing  -> ParAssign lvCurr $ LVal lvNext

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
                        mr           <- newWire sz $ g <> "_out"
                        inst         <- fresh "inst"
                        pure (LVal $ V.Name mr, stmts <> [Instantiate (mangleMod g) inst [] $ map (mempty, ) $ es' <> [LVal $ V.Name mr]])
                  -- Extern ports connect positionally (declaration order):
                  -- the hand-written Verilog implementations don't know the
                  -- synthesized names of anonymous ports.
                  M.XCall an sz x cs es -> case Map.lookup x xenv of
                        Nothing -> failAt an $ "unknown extern: " <> x
                        Just ex -> do
                              (es', stmts) <- compileExps xenv lenv $ filter (not . M.isNil) es
                              mr           <- newWire sz "extres"
                              inst         <- fresh "inst"
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
                              lv            <- newWire (sizeOf e1) x
                              (e2', stmts') <- go (Map.insert x (LVal $ V.Name lv) lenv) e2
                              pure (e2', stmts <> [Assign (V.Name lv) e1'] <> stmts')

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
            --   directly; anything else through a fresh wire.
            sliceExp :: M.Size -> M.Index -> M.Size -> V.Exp -> m (V.Exp, [V.Stmt])
            sliceExp w i k e
                  | k == 0          = pure (V.nil, [])
                  | i == 0, k == w  = pure (e, [])
                  | otherwise = case e of
                        LVal (V.Name n)      -> pure (LVal $ mkRange n i' (i' + k' - 1), [])
                        LVal (V.Range n a _) -> pure (LVal $ mkRange n (a + i') (a + i' + k' - 1), [])
                        LitBits bv           -> pure (LitBits $ subRange (fromIntegral i, fromIntegral (i + fromIntegral k) - 1) bv, [])
                        _                    -> do
                              n <- newWire w "slice_in"
                              pure (LVal $ mkRange n i' (i' + k' - 1), [Assign (V.Name n) e])
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

mkLVals :: [LVal] -> LVal
mkLVals = \ case
      [lv] -> lv
      lvs  -> LVals lvs

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
testbench conf dev inps = V.Module "tb" []
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

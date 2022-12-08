{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.ToVerilog (compileProgram) where

import ReWire.Annotation (noAnn)
import ReWire.Core.Mangle (mangle)
import ReWire.Core.Syntax as C hiding (Name, Size, Index)
import ReWire.Error
import ReWire.Flags (Flag (..))
import ReWire.Verilog.Syntax as V
import ReWire.Core.Interp
      ( patApply', patMatches', subRange
      , dispatchWires, pausePrefix, extraWires
      , resumptionSize
      , clock, reset
      )
import ReWire.Pretty (prettyPrint)

import Control.Arrow ((&&&), first, second)
import Control.Monad (liftM2)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.State (MonadState, runStateT, modify, gets)
import Data.BitVector (width, bitVec, BV, zeros, ones, lsb1, (==.), (@.))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, listToMaybe)
import TextShow (showt)
import qualified Data.BitVector as BV
import qualified Data.HashMap.Strict as Map

data FreshMode = FreshInit | FreshRun
type Fresh = (FreshMode, HashMap Text Int)
type DefnMap = HashMap GId C.Exp
type SigInfo = (Fresh, [Signal])

freshInit0 :: Fresh
freshInit0 = (FreshInit, mempty)

freshRun0 :: Fresh
freshRun0 = (FreshRun, mempty)

fresh' :: MonadState SigInfo m => Text -> m Name
fresh' s = do
      (mode, ctrs) <- gets fst
      let ctr = fromMaybe 0 $ Map.lookup s ctrs
      modify $ first $ second $ Map.insert s (ctr + 1)
      pure $ s <> modeTag mode <> ctrTag ctr
      where modeTag :: FreshMode -> Text
            modeTag = \ case
                  FreshInit -> "S0"
                  _         -> mempty

            ctrTag :: Int -> Text
            ctrTag = \ case
                  n | n > 0 -> "R" <> showt n
                  _         -> mempty

fresh :: MonadState SigInfo m => Text -> m Name
fresh s = mangle <$> fresh' s

newWire :: MonadState SigInfo m => V.Size -> Name -> m LVal
newWire sz n = do
      n' <- fresh n
      modify $ second (++ [mkSignal (n', sz)])
      pure $ Name n'

newWire' :: MonadState SigInfo m => V.Size -> Name -> m LVal
newWire' sz n = do
      n' <- fresh' n
      modify $ second (++ [mkSignal (n', sz)])
      pure $ Name n'

lookupWidth :: MonadState SigInfo m => Name -> m (Maybe Size)
lookupWidth n = gets (lookup n . map proj <$> snd)
      where proj :: Signal -> (Name, Size)
            proj = \ case
                  Wire  []   n _ -> (n, 0)
                  Wire  [sz] n _ -> (n, sz)
                  Logic []   n _ -> (n, 0)
                  Logic [sz] n _ -> (n, sz)
                  Reg   []   n _ -> (n, 0)
                  Reg   [sz] n _ -> (n, sz)

getClock :: MonadState SigInfo m => m LVal
getClock = gets snd >>= pure . \ case
     s : _ -> Name $ sigName s
     _     -> Name "clk"

compileProgram :: (MonadFail m, MonadError AstError m) => [Flag] -> C.Program -> m V.Program
compileProgram flags (C.Program st ds)
      | FlagNoFlatten `elem` flags = do
            st' <- runReaderT (compileStartDefn flags st) defnMap
            ds' <- mapM (compileDefn flags) $ filter ((/= getState0 st) . defnName) ds
            pure $ V.Program $ st' : ds'
      | otherwise                  = V.Program . pure <$> runReaderT (compileStartDefn flags st) defnMap
      where defnMap :: DefnMap
            defnMap = Map.fromList $ map ((mangle . defnName) &&& defnBody) ds

            -- | Initial state should be inlined, so we can filter out its defn.
            getState0 :: StartDefn -> Name
            getState0 (C.StartDefn _ _ _ state0) = state0

compileStartDefn :: (MonadError AstError m, MonadFail m, MonadReader DefnMap m)
                 => [Flag] -> C.StartDefn -> m Module
compileStartDefn flags (C.StartDefn _ w loop state0) = do

      ((rStart, ssStart), (_, _ : _ : startSigs)) <- flip runStateT (freshInit0, sigs0) (compileCall (filter (/= FlagNoFlatten) flags) (mangle state0) (resumptionSize w) [])

      let (stateInit, ssStart', startSigs')
            | unclocked = (Nothing,          mempty,  mempty)
            | otherwise = (initState rStart, ssStart, startSigs)

      ((rLoop, ssLoop),   (_, _ : _ : loopSigs))  <- flip runStateT (freshRun0, sigs0) $ compileCall flags (mangle loop) (resumptionSize w)
            $  [ V.cat $ map (LVal . Name . fst) $ dispatchWires w | not (null $ dispatchWires w) ]
            <> [ V.cat $ map (LVal . Name . fst) $ inputWires w    | not (null $ inputWires w) ]

      pure $ Module "top_level" (inputs <> outputs) (loopSigs <> startSigs' <> sigs)
            $  ssStart'
            <> ssLoop
            <> [ Assign lvPause rLoop ]
            <> case stateInit of
                  Just initExp -> [ Initial $ ParAssign lvCurrState initExp ]
                               <> [ Always (map (Pos . fst) (clock flags) <> rstEdge) $ Block [ ifRst initExp ] ]
                  _            -> [ ]
      where sigs0 :: [Signal]
            sigs0 = map mkSignal [clk0, rst0]
                  where clk0 :: (Name, Size)
                        clk0 = fromMaybe ("clk", 0) $ listToMaybe $ clock flags
                        rst0 :: (Name, Size)
                        rst0 = fromMaybe ("rst", 0) $ listToMaybe $ reset flags

            inputs :: [Port]
            inputs = map (Input . mkSignal)  $ clock flags <> reset flags <> inputWires w

            outputs :: [Port]
            outputs = map (Output . mkSignal) $ outputWires w

            sigs :: [Signal]
            sigs = map mkSignal $ allWires w

            ifRst :: V.Exp -> Stmt
            ifRst init = case reset flags of
                  [(sRst, sz)] -> IfElse (V.Eq (LVal $ Name sRst) $ LitBits $ bitVec (fromIntegral sz) $ fromEnum $ not invertRst)
                        (Block [ ParAssign lvCurrState init ] )
                        (Block [ ParAssign lvCurrState $ LVal lvNxtState ])
                  _            -> ParAssign lvCurrState $ LVal lvNxtState

            unclocked :: Bool
            unclocked = FlagNoClock `elem` flags || null (dispatchWires w)

            -- | Initial/reset state.
            initState :: V.Exp -> Maybe V.Exp
            initState e = case (expToBV e, fromIntegral (sum $ snd <$> dispatchWires w)) of
                  (Just bv, n) | n > 0 -> pure $ bvToExp $ subRange (0, n - 1) bv
                  (_, n)       | n > 0 -> pure $ bvToExp $ zeros n -- TODO(chathhorn): make configurable?
                  _                    -> Nothing

            lvPause :: LVal
            lvPause = mkLVals $ map (Name . fst) $ pauseWires w

            lvCurrState :: LVal
            lvCurrState = mkLVals $ map (Name . fst) $ dispatchWires w

            lvNxtState :: LVal
            lvNxtState = mkLVals $ map (Name . fst) $ nextDispatchWires w

            rstEdge :: [Sensitivity]
            rstEdge = case reset flags of
                  [(sRst, _)] | syncRst   -> []
                              | invertRst -> [Neg sRst]
                              | otherwise -> [Pos sRst]
                  _                       -> []

            invertRst :: Bool
            invertRst = FlagInvertReset `elem` flags

            syncRst :: Bool
            syncRst = FlagSyncReset `elem` flags

            pauseWires :: Wiring -> [(Name, Size)]
            pauseWires w = pausePrefix w <> nextDispatchWires w

            nextDispatchWires :: Wiring -> [(Name, Size)]
            nextDispatchWires w = map (first (<> "_next")) $ dispatchWires w

            allWires :: Wiring -> [(Name, Size)]
            allWires w = extraWires w <> dispatchWires w <> nextDispatchWires w

compileDefn :: (MonadFail m, MonadError AstError m) => [Flag] -> C.Defn -> m V.Module
compileDefn flags (C.Defn _ n (Sig _ inps outp) body) = do
      ((e, stmts), (_, sigs)) <- flip runStateT (freshRun0, []) (runReaderT (compileExp flags (map (LVal . Name) argNames) body) Map.empty)
      pure $ V.Module (mangle n) (inputs <> outputs) sigs $ stmts <> [Assign (Name "res") e]
      where argNames :: [Name]
            argNames = zipWith (\ _ x -> "arg" <> showt x) inps [0::Int ..]

            inputs :: [Port]
            inputs = zipWith (curry $ Input . mkSignal) argNames inps

            outputs :: [Port]
            outputs = map (Output . mkSignal) [("res", outp)]

-- | Inlines a defn or instantiates an already-compiled defn.
compileCall :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
             => [Flag] -> GId -> V.Size -> [V.Exp] -> m (V.Exp, [Stmt])
compileCall flags g sz lvars
      | FlagNoFlatten `elem` flags = do
            mr         <- newWire sz "callRes"
            inst'      <- fresh' g
            let stmt   =  Instantiate g inst' [] $ zip (repeat mempty) $ lvars <> [LVal mr]
            pure (LVal mr, [stmt])
      | otherwise = asks (Map.lookup g) >>= \ case
            Just body -> do
                  (e, stmts) <- compileExp flags lvars body
                  e'         <- wcast sz e
                  pure (e', stmts)
            _ -> failAt noAnn $ "ToVerilog: compileCall: failed to find definition for " <> g <> " while flattening."

instantiate :: (MonadFail m, MonadState SigInfo m, MonadError AstError m) => ExternSig -> GId -> Text -> V.Size -> [V.Exp] -> m (V.Exp, [Stmt])
instantiate (ExternSig _ ps clk args res) g inst sz lvars = do
      Name mr     <- newWire sz "extRes"
      stmt        <- do
            inst' <- fresh' inst
            (args', lvars') <- do
                  clk' <- getClock
                  pure $ if T.null clk then (args, lvars) else ((clk, 1) : args, LVal clk' : lvars)
            pure $ Instantiate g inst' (map (second $ LitBits . bitVec 32) ps)
                 $ zip (fst <$> args') lvars' <> zip (fst <$> res) (toSubRanges mr (snd <$> res))
      pure (LVal (Name mr), [stmt])

compileExps :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
            => [Flag] -> [V.Exp] -> [C.Exp] -> m ([V.Exp], [Stmt])
compileExps flags lvars es = (map fst &&& concatMap snd) <$> mapM (compileExp flags lvars) es

compileExp :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
            => [Flag] -> [V.Exp] -> C.Exp -> m (V.Exp, [Stmt])
compileExp flags lvars = \ case
      LVar _  _ (lkupLVal -> Just x)                -> pure (x, [])
      LVar an _ _                                   -> failAt an "ToVerilog: compileExp: encountered unknown LVar."
      Lit _ bv                                      -> pure (bvToExp bv, [])
      C.Concat _ e1 e2                              -> first V.cat <$> compileExps flags lvars (gather e1 <> gather e2)
      Call _ sz (Global g) e ps els                 -> mkCall ("g" <> g) e ps els $ compileCall flags (mangle g) sz
      Call _ sz (SetRef r) e ps els                 -> mkCall "setRef" e ps els $ \ [a, b] -> case a of
            a@(LVal (Element _ _)) -> do -- TODO(chathhorn) TODO TODO
                  let wa  = 1
                  r' <- newWire' wa r
                  b' <- wcast sz b
                  pure (b', [Assign r' a])
            a@(LVal (Range _ i j)) -> do -- TODO(chathhorn) TODO TODO
                  let wa  = fromIntegral $ j - i + 1
                  r' <- newWire' wa r
                  b' <- wcast sz b
                  pure (b', [Assign r' a])
            LVal (Name a) -> do
                  wa  <- fromMaybe 0 <$> lookupWidth a
                  r' <- newWire' wa r
                  b' <- wcast sz b
                  pure (b', [Assign r' $ LVal $ Name a])
            a -> error $ T.unpack $ "Got: " <> prettyPrint a
      Call _ sz (GetRef r) _ _ _                         -> (,[]) <$> wcast sz (LVal $ Name r)
      Call _ sz (Prim (binOp -> Just op)) e ps els -> mkCall "binOp"  e ps els $ \ [x, y] -> (,[]) <$> wcast sz (op x y)
      Call _ sz (Prim (unOp -> Just op)) e ps els  -> mkCall "unOp"   e ps els $ \ [x]    -> (,[]) <$> wcast sz (op x)
      Call _ sz (Prim MSBit) e ps els              -> mkCall "msbit"  e ps els $ \ [x]    -> (,[]) <$> wcast sz (projBit (fromIntegral (argsSize ps) - 1) x)
      Call _ sz (Prim Resize) e ps els             -> mkCall "resize" e ps els $ \ [x]    -> (,[]) <$> wcast sz x
      Call _ sz (Prim Id) e ps els                 -> mkCall "id"     e ps els $ \ xs     -> (,[]) <$> wcast sz (V.cat xs)
      Call a _  (Prim p) _ _ _                     -> failAt a $ "ToVerilog: compileExp: encountered unknown primitive: " <> showt p
      Call _ sz (Extern sig ex inst) e ps els      -> mkCall ex       e ps els $ instantiate sig ex inst sz
      Call _ sz (Const bv) e ps els                -> mkCall "lit"    e ps els $ \ _      -> (,[]) <$> wcast sz (bvToExp bv)
      where mkCall :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
                    => Name -> C.Exp -> [Pat] -> C.Exp -> ([V.Exp] -> m (V.Exp, [Stmt])) -> m (V.Exp, [Stmt])
            mkCall s e ps els f = do
                  (e', stmts)    <- compileExp flags lvars e
                  (els', stmts') <- compileExp flags lvars els
                  case litVal e' of
                        Just bv -> do
                              (fes, fstmts) <- f $ patApplyLit bv ps
                              pure (if patMatchesLit bv ps then fes else els', stmts <> stmts' <> fstmts)
                        _      -> do
                              Name n <- newWire (sizeOf e) s
                              (fes, fstmts) <- f $ map LVal $ patApply n ps
                              pure  ( let c = patMatches n ps in
                                      if | litTrue c || C.isNil els -> fes
                                         | litFalse c               -> els'
                                         | otherwise                -> Cond c fes els'
                                    , stmts <> stmts' <> [ Assign (Name n) e' ] <> fstmts
                                    )

            litTrue :: V.Exp -> Bool
            litTrue e = case litVal e of
                  Just v  -> v /= zeros 1
                  Nothing -> False

            litFalse :: V.Exp -> Bool
            litFalse e = case litVal e of
                  Just v  -> v == zeros 1
                  Nothing -> False

            litVal :: V.Exp -> Maybe BV
            litVal = \ case
                  LitBits b -> Just b
                  _         -> Nothing

            lkupLVal :: LId -> Maybe V.Exp
            lkupLVal = flip lookup (zip [0::LId ..] lvars)

            projBit :: Index -> V.Exp -> V.Exp
            projBit ix = \ case
                  LVal (Range n i _)     -> LVal $ Element n $ ix + i
                  LVal (Name n)          -> LVal $ Element n ix
                  LitBits bv | bv @. ix  -> LitBits $ ones 1
                             | otherwise -> LitBits $ zeros 1
                  e                      -> e -- TODO(chathhorn): kludgy.

-- | Attempt to break up giant literals.
bvToExp :: BV -> V.Exp
bvToExp bv | width bv < maxLit      = LitBits bv
           | bv == zeros 1          = Repl (toLit $ width bv) $ LitBits (zeros 1)
           | bv == ones (width bv)  = Repl (toLit $ width bv) $ LitBits (ones 1)
           | zs > 8                 = V.cat [LitBits $ subRange (fromIntegral zs, width bv - 1) bv, Repl (toLit zs) $ LitBits $ zeros 1]
           | otherwise              = LitBits bv -- TODO(chathhorn)
      where zs :: Size -- trailing zeros
            zs | bv == zeros 1 = fromIntegral $ width bv
               | otherwise     = fromIntegral $ lsb1 bv

            maxLit :: Int
            maxLit = 32

toLit :: Integral a => a -> V.Exp
toLit v = LitBits $ bitVec (fromIntegral $ ceilLog2 v) v

expToBV :: V.Exp -> Maybe BV
expToBV = \ case
      LitBits bv                 -> Just bv
      Repl (expToBV -> Just n) e -> BV.replicate (BV.nat n) <$> expToBV e
      V.Concat es                -> BV.concat <$> mapM expToBV es
      _                          -> Nothing

-- | Size of the inclusive range [i, j].
nbits :: Index -> Index -> Size
nbits i j = fromIntegral (j - i + 1)

mkLVals :: [V.LVal] -> V.LVal
mkLVals = \ case
      [e] -> e
      es  -> LVals es

mkRange :: Name -> Index -> Index -> V.LVal
mkRange n i j | i == j    = Element n i
              | otherwise = Range n i j

mkWCast :: Size -> V.Exp -> V.Exp
mkWCast sz = \ case
      LVal (Range n i j) | sz < nbits i j -> LVal $ mkRange n i (j - fromIntegral (nbits i j - sz))
      WCast _ e                           -> WCast sz e
      e                                   -> WCast sz e

-- | Recreates the effect of assignment by adding bitwidth casts to an expression:
-- > wcast s e
-- should return an e' that behaves the same as though e appeared as
-- > x = e
-- where x has a width of s. I.e., "wcast s e" fixes the "bitwidth context" for
-- the expression e to s.
wcast :: MonadState SigInfo m => Size -> V.Exp -> m V.Exp
wcast sz e = expWidth e >>= \ case
      Just sz' | sz < sz' -> trunc e
               | sz > sz' -> case e of
                  V.Add  e1 e2      -> V.Add         <$> pad e1 <*> pad e2
                  V.Sub  e1 e2      -> V.Sub         <$> pad e1 <*> pad e2
                  V.Mul  e1 e2      -> V.Mul         <$> pad e1 <*> pad e2
                  V.Div  e1 e2      -> V.Div         <$> pad e1 <*> pad e2
                  V.Mod  e1 e2      -> V.Mod         <$> pad e1 <*> pad e2
                  V.And  e1 e2      -> V.And         <$> pad e1 <*> pad e2
                  V.Or   e1 e2      -> V.Or          <$> pad e1 <*> pad e2
                  V.XOr  e1 e2      -> V.XOr         <$> pad e1 <*> pad e2
                  V.XNor e1 e2      -> V.XNor        <$> pad e1 <*> pad e2
                  V.Cond c e1 e2    -> V.Cond c      <$> pad e1 <*> pad e2
                  V.Pow         e n -> V.Pow         <$> pad e  <*> pure n
                  V.LShift      e n -> V.LShift      <$> pad e  <*> pure n
                  V.RShift      e n -> V.RShift      <$> pad e  <*> pure n
                  V.LShiftArith e n -> V.LShiftArith <$> pad e  <*> pure n
                  V.RShiftArith e n -> V.RShiftArith <$> pad e  <*> pure n
                  V.Not e           -> V.Not         <$> pad e
                  e                 -> pad e
      _                   -> pure e
      where pad :: MonadState SigInfo m => V.Exp -> m V.Exp
            pad e = expWidth e >>= \ case
                        Just sz' | sz > sz' -> pure $ mkWCast sz e
                        _                   -> pure e
            trunc :: MonadState SigInfo m => V.Exp -> m V.Exp
            trunc e = expWidth e >>= \ case
                        Just sz' | sz < sz' -> pure $ mkWCast sz e
                        _                   -> pure e

-- Expression                     Bit Length         Notes
-- -----------------------------------------------------------------------------
-- Unsized constants              "Same as integer"  (see ** below)
-- Sized constants                As given
-- i [+ - * / % & | ^ ^~ ~^] j    max{L(i),L(j)}
-- [+ - ~] i                      L(i)
-- i [=== !== == != > >= < <=] j  1 bit              i,j sized to max(L(i),L(j))
-- i [&& ||] j                    1 bit              i,j self-determined
-- [& ~& | ~| ^ ~^ ^~ !] i        1 bit              i self-determined
-- i [>> << ** >>> <<<] j         L(i)               j self-determined
-- i ? j : k                      max(L(j),L(k))     i self-determined
-- {i, ..., j}                    L(i)+...+L(j)      all self-determined
-- {i {j, ..., k}}                i*(L(j)+...+L(k))  all self-determined
-- -----------------------------------------------------------------------------
expWidth :: MonadState SigInfo m => V.Exp -> m (Maybe Size)
expWidth = \ case
      V.Add    e1 e2                -> largest e1 e2
      V.Sub    e1 e2                -> largest e1 e2
      V.Mul    e1 e2                -> largest e1 e2
      V.Div    e1 e2                -> largest e1 e2
      V.Mod    e1 e2                -> largest e1 e2
      V.And    e1 e2                -> largest e1 e2
      V.Or     e1 e2                -> largest e1 e2
      V.XOr    e1 e2                -> largest e1 e2
      V.XNor   e1 e2                -> largest e1 e2
      V.Cond _ e1 e2                -> largest e1 e2
      V.Pow         e _             -> expWidth e
      V.LShift      e _             -> expWidth e
      V.RShift      e _             -> expWidth e
      V.LShiftArith e _             -> expWidth e
      V.RShiftArith e _             -> expWidth e
      V.Not         e               -> expWidth e
      V.LAnd _ _                    -> pure $ Just 1
      V.LOr _ _                     -> pure $ Just 1
      V.LNot _                      -> pure $ Just 1
      V.RAnd _                      -> pure $ Just 1
      V.RNAnd _                     -> pure $ Just 1
      V.ROr _                       -> pure $ Just 1
      V.RNor _                      -> pure $ Just 1
      V.RXOr _                      -> pure $ Just 1
      V.RXNor _                     -> pure $ Just 1
      V.Eq  _ _                     -> pure $ Just 1
      V.NEq _ _                     -> pure $ Just 1
      V.CEq _ _                     -> pure $ Just 1
      V.CNEq _ _                    -> pure $ Just 1
      V.Lt _ _                      -> pure $ Just 1
      V.Gt _ _                      -> pure $ Just 1
      V.LtEq _ _                    -> pure $ Just 1
      V.GtEq _ _                    -> pure $ Just 1
      V.Concat es                   -> sumMaybes <$> mapM expWidth es
      V.Repl (expToBV -> Just sz) e -> fmap (* fromIntegral (BV.nat sz)) <$> expWidth e
      V.Repl (expToBV -> Nothing) _ -> pure Nothing
      V.WCast sz _                  -> pure $ Just sz
      V.LitBits bv                  -> pure $ Just $ fromIntegral $ width bv
      V.LVal lv                     -> lvalWidth lv
      where largest :: MonadState SigInfo m => V.Exp -> V.Exp -> m (Maybe Size)
            largest e1 e2 = liftM2 max <$> expWidth e1 <*> expWidth e2

            lvalWidth :: MonadState SigInfo m => LVal -> m (Maybe Size)
            lvalWidth = \ case
                  Element _ _ -> pure $ Just 1
                  Range _ i j -> pure $ Just $ fromIntegral $ nbits i j
                  Name n      -> lookupWidth n
                  LVals lvs   -> sumMaybes <$> mapM lvalWidth lvs

sumMaybes :: Num a => [Maybe a] -> Maybe a
sumMaybes = foldMaybes (+)

foldMaybes :: (a -> a -> a) -> [Maybe a] -> Maybe a
foldMaybes f = \ case
      a : b : ms -> foldMaybes f $ (f <$> a <*> b) : ms
      [a]        -> a
      _          -> Nothing

binOp :: Prim -> Maybe (V.Exp -> V.Exp -> V.Exp)
binOp = flip lookup primBinOps

unOp :: Prim -> Maybe (V.Exp -> V.Exp)
unOp = flip lookup primUnOps

primBinOps :: [(Prim, V.Exp -> V.Exp -> V.Exp)]
primBinOps =
      [ ( C.Add         , V.Add)
      , ( C.Sub         , V.Sub)
      , ( C.Mul         , V.Mul)
      , ( C.Div         , V.Div)
      , ( C.Mod         , V.Mod)
      , ( C.Pow         , V.Pow)
      , ( C.LAnd        , V.LAnd)
      , ( C.LOr         , V.LOr)
      , ( C.And         , V.And)
      , ( C.Or          , V.Or)
      , ( C.XOr         , V.XOr)
      , ( C.XNor        , V.XNor)
      , ( C.LShift      , V.LShift)
      , ( C.RShift      , V.RShift)
      , ( C.RShiftArith , V.RShiftArith)
      , ( C.Eq          , V.Eq)
      , ( C.Gt          , V.Gt)
      , ( C.GtEq        , V.GtEq)
      , ( C.Lt          , V.Lt)
      , ( C.LtEq        , V.LtEq)
      , ( C.Replicate   , V.Repl)
      ]

primUnOps :: [(Prim, V.Exp -> V.Exp)]
primUnOps =
      [ ( C.LNot  , V.LNot)
      , ( C.Not   , V.Not)
      , ( C.RAnd  , V.RAnd)
      , ( C.RNAnd , V.RNAnd)
      , ( C.ROr   , V.ROr)
      , ( C.RNor  , V.RNor)
      , ( C.RXOr  , V.RXOr)
      , ( C.RXNor , V.RXNor)
      ]

-- | Returns a boolean expression that is true when the pattern matches.
patMatches :: Name -> [Pat] -> V.Exp
patMatches x ps =  case patMatches' (\ i j bv -> [V.Eq (LVal $ mkRange x i j) $ LitBits bv]) ps of
      ps'@(_ : _) -> foldr1 V.LAnd ps'
      []          -> bTrue

patMatchesLit :: BV -> [Pat] -> Bool
patMatchesLit bv = and . patMatches' (\ i j bv' -> [subRange (i, j) bv ==. bv'])

-- | Returns a list of ranges bound by pattern variables.
patApply :: Name -> [Pat] -> [LVal]
patApply x = patApply' (\ i j -> [mkRange x i j])

patApplyLit :: BV -> [Pat] -> [V.Exp]
patApplyLit bv = patApply' (\ i j -> [LitBits $ subRange (i, j) bv])

toSubRanges :: Name -> [Size] -> [V.Exp]
toSubRanges n = patApply' (\ i j -> [LVal $ mkRange n i j]) . map (PatVar noAnn)

argsSize :: [Pat] -> Size
argsSize = sum . map patToSize
      where patToSize :: Pat -> Size
            patToSize = \ case
                  PatVar _ sz -> sz
                  _           -> 0

mkSignal :: (Name, Size) -> Signal
mkSignal (n, sz) = Logic [sz] n []

-- TODO(chathhorn): duplicated from Crust/ToCore.hs
ceilLog2 :: Integral a => a -> a
ceilLog2 n | toInteger n < 1 = 0
ceilLog2 n                   = ceiling $ logBase 2 (fromIntegral n :: Double)

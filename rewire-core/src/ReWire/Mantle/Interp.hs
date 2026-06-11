{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
-- | The Mantle interpreter: a direct transcription of the denotational
--   semantics in doc/core.md (sections 5 and 6). The device denotes a Mealy
--   stream function: registers start at their initial values, outputs and
--   next-register values are computed from the body's equations each cycle.
--
--   Programs containing instances (sequential externs) or calls to
--   model-less combinational externs cannot be evaluated, as with the Core
--   interpreter today.
module ReWire.Mantle.Interp (interp, evalExp, evalOp, IEnv (..), Ins, Outs, run, subRange, inputValue, yamlPrefixes) where

import ReWire.Annotation (Annote, Annotated (ann))
import ReWire.BitVector (BV, bitVec, nat, width, ones, zeros, ashr)
import ReWire.Config (Config, verbose)
import ReWire.Error (failAt, MonadError, AstError)
import ReWire.Mantle.Syntax
import ReWire.Pretty (showt)

import qualified ReWire.BitVector as BV

import Control.Lens ((^.))
import Control.Monad (foldM, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits (Bits (..))
import Data.HashMap.Strict (HashMap)
import Data.Machine.MealyT (MealyT (..), runMealyT)
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map
import qualified Data.Text.IO        as T

-- | Per-cycle input values and output bitvectors, by wire name.
type Ins  = HashMap Name Value
type Outs = HashMap Name BV
type Sts  = HashMap Name BV

-- | Runs non-interactively: given a stream of inputs, produces a stream of
--   outputs.
run :: MonadIO m => Config -> MealyT m Ins Outs -> [Ins] -> m [Outs]
run conf m = \ case
      []         -> pure []
      (ip : ips) -> do
            (b, m') <- runMealyT m ip
            when (conf^.verbose) $ liftIO $ do
                  T.putStrLn "Debug: Interpreting mantle: completed cycle."
                  T.putStr $ mconcat $ (\ (k, v) -> "\t" <> k <> ": " <> BV.showHex v <> "\n") <$> Map.toList b
            (b :) <$> run conf m' ips

-- | The value driven onto an input wire for a cycle (interp-style inputs:
--   missing wires are driven to zero; values are truncated to the wire
--   width).
inputValue :: Size -> Ins -> Name -> Value
inputValue w ins n = Map.findWithDefault 0 n ins `mod` (2 ^ toInteger w)

-- | Per-cycle output records in the interpreter's YAML format: the first
--   output prefixed with "- ", the rest indented, in port order.
yamlPrefixes :: [Text]
yamlPrefixes = "- " : repeat "  "

-- | The inclusive bit range [i, j] of a bitvector.
subRange :: (Int, Int) -> BV -> BV
subRange (i, j) b | j - i >= 0 = bitVec (j - i + 1) $ nat b `div` (2 ^ toInteger i)
                  | otherwise  = bitVec 0 (0 :: Integer)

data IEnv = IEnv
      { envDefns   :: HashMap GId Defn
      , envExterns :: HashMap Name Extern
      }

interp :: MonadError AstError m => Config -> Program -> MealyT m Ins Outs
interp _conf (Program exts ds dev) = unfoldMealyT step sts0
      where env :: IEnv
            env = IEnv (Map.fromList $ map (\ d -> (defnName d, d)) ds)
                       (Map.fromList $ map (\ e -> (extName e, e)) exts)

            sts0 :: Sts
            sts0 = Map.fromList [ (x, bv) | Register _ x _ bv <- devRegisters dev ]

            step :: MonadError AstError m => Sts -> Ins -> m (Outs, Sts)
            step sts ins = do
                  unless (null $ devInstances dev) $
                        failAt (ann dev) "Mantle/Interp: cannot evaluate a device with extern instances"
                  let rho0 = sts <> Map.fromList [ (x, bitVec (fromIntegral sz) $ Map.findWithDefault 0 x ins)
                                                 | (x, sz) <- devInputs dev ]
                  (_, outs, sts') <- foldM stmt (rho0, mempty, sts) $ devBody dev
                  pure (outs, sts')

            stmt :: MonadError AstError m => (HashMap Name BV, Outs, Sts) -> Stmt -> m (HashMap Name BV, Outs, Sts)
            stmt (rho, outs, sts) = \ case
                  SLet _ x e      -> (\ v -> (Map.insert x v rho, outs, sts)) <$> evalExp env rho e
                  SOutput _ x e   -> (\ v -> (rho, Map.insert x v outs, sts)) <$> evalExp env rho e
                  SNext _ x e     -> (\ v -> (rho, outs, Map.insert x v sts)) <$> evalExp env rho e
                  SInstIn an _ _ _ -> failAt an "Mantle/Interp: cannot evaluate a device with extern instances"

evalExp :: forall m. MonadError AstError m => IEnv -> HashMap Name BV -> Exp -> m BV
evalExp env = go
      where go :: HashMap Name BV -> Exp -> m BV
            go rho = \ case
                  Lit _ bv      -> pure bv
                  Undef _ sz    -> pure $ zeros $ fromIntegral sz
                  Var an _ x    -> maybe (failAt an $ "Mantle/Interp: unbound variable: " <> x) pure $ Map.lookup x rho
                  Cat _ e1 e2   -> (<>) <$> go rho e1 <*> go rho e2
                  Slice _ i k e -> slice i k <$> go rho e
                  Prim an _ op es -> mapM (go rho) es >>= evalOp an op
                  Call an _ g es  -> case Map.lookup g $ envDefns env of
                        Nothing -> failAt an $ "Mantle/Interp: call to unknown definition: " <> g
                        Just d  -> mapM (go rho) es >>= apply d
                  XCall an _ x _ es -> case Map.lookup x (envExterns env) >>= extModel of
                        Just g | Just d <- Map.lookup g $ envDefns env -> mapM (go rho) es >>= apply d
                        _ -> failAt an $ "Mantle/Interp: cannot evaluate extern " <> x
                                      <> " (no usable Haskell model: see the rwPrimExtern documentation)."
                  If _ _ c t e  -> do
                        c' <- go rho c
                        if nat c' /= 0 then go rho t else go rho e
                  Let _ _ x e1 e2 -> do
                        v <- go rho e1
                        go (Map.insert x v rho) e2

            apply :: Defn -> [BV] -> m BV
            apply (Defn _ _ _ ps body) vs = go (Map.fromList $ zip ps vs) body

slice :: Index -> Size -> BV -> BV
slice i k x | k == 0    = BV.nil
            | otherwise = BV.bitVec (fromIntegral k) $ nat x `shiftR` fromIntegral i

-- | Primitive denotations (doc/core.md, section 5.2).
evalOp :: MonadError AstError m => Annote -> Op -> [BV] -> m BV
evalOp an op vs = case (op, vs) of
      (Add   , [a, b]) -> arith a b (+)
      (Sub   , [a, b]) -> arith a b (-)
      (Mul   , [a, b]) -> arith a b (*)
      (UDiv  , [a, b]) -> pure $ if nat b == 0 then ones (width a) else mkBV (width a) $ nat a `div` nat b
      (UMod  , [a, b]) -> pure $ if nat b == 0 then a else mkBV (width a) $ nat a `mod` nat b
      (Pow   , [a, b]) -> arith a b (^)
      (And   , [a, b]) -> arith a b (.&.)
      (Or    , [a, b]) -> arith a b (.|.)
      (XOr   , [a, b]) -> arith a b xor
      (Not   , [a])    -> pure $ mkBV (width a) $ complement $ nat a
      (Shl   , [a, b]) -> pure $ shiftOp a b $ \ x s -> x `shiftL` s
      (LShr  , [a, b]) -> pure $ shiftOp a b $ \ x s -> x `shiftR` s
      (AShr  , [a, b]) -> pure $ if nat b >= toInteger (width a)
                                  then (if sint a < 0 then ones (width a) else zeros (width a))
                                  else ashr a b
      (Eq    , [a, b]) -> cmp $ nat a == nat b
      (Ne    , [a, b]) -> cmp $ nat a /= nat b
      (ULt   , [a, b]) -> cmp $ nat a < nat b
      (ULe   , [a, b]) -> cmp $ nat a <= nat b
      (UGt   , [a, b]) -> cmp $ nat a > nat b
      (UGe   , [a, b]) -> cmp $ nat a >= nat b
      (SLt   , [a, b]) -> cmp $ sint a < sint b
      (SLe   , [a, b]) -> cmp $ sint a <= sint b
      (SGt   , [a, b]) -> cmp $ sint a > sint b
      (SGe   , [a, b]) -> cmp $ sint a >= sint b
      (RedAnd, [a])    -> cmp $ nat a == 2 ^ width a - 1
      (RedOr , [a])    -> cmp $ nat a /= 0
      (RedXOr, [a])    -> cmp $ odd $ popCount $ nat a
      (ZExt m, [a])    -> pure $ mkBV (fromIntegral m) $ nat a
      (SExt m, [a])    -> pure $ mkBV (fromIntegral m) $ sint a
      (Trunc m, [a])   -> pure $ slice 0 m a
      (Rep k , [a])    -> pure $ mconcat $ replicate (fromIntegral k) a
      _                -> failAt an $ "Mantle/Interp: ill-formed primitive application: " <> opName op <> " with " <> showt (length vs) <> " arguments"
      where arith :: Monad m' => BV -> BV -> (Integer -> Integer -> Integer) -> m' BV
            arith a b f = pure $ mkBV (width a) $ nat a `f` nat b

            cmp :: Monad m' => Bool -> m' BV
            cmp c = pure $ if c then ones 1 else zeros 1

            shiftOp :: BV -> BV -> (Integer -> Int -> Integer) -> BV
            shiftOp a b f | nat b >= toInteger (width a) = zeros $ width a
                          | otherwise                    = mkBV (width a) $ nat a `f` fromInteger (nat b)

            mkBV :: Int -> Integer -> BV
            mkBV = bitVec

-- | Signed (two's-complement) reading.
sint :: BV -> Integer
sint x | width x == 0                       = 0
       | testBit (nat x) (width x - 1)      = nat x - 2 ^ width x
       | otherwise                          = nat x

unfoldMealyT :: Applicative m => (s -> a -> m (b, s)) -> s -> MealyT m a b
unfoldMealyT f = mealy
      where mealy s = MealyT $ \ a -> (\ (b, s') -> (b, mealy s')) <$> f s a

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Interp
      ( interp, interpDefn
      , Ins, Outs, Out, run
      , patMatches, patMatches'
      , patApply'
      , interpExp, DefnMap
      , subRange
      , dispatchWires, pausePrefix, extraWires
      , resumptionSize
      , clock, reset
      ) where

import ReWire.Flags (Flag (..))
import ReWire.Core.Syntax
      ( Program (..)
      , Name, Value, Index, Size
      , Wiring (..), Prim (..)
      , GId, LId
      , Pat (..), Exp (..)
      , Target (..)
      , Defn (..)
      , Sig (..)
      , StartDefn (..)
      , bvFalse, sizeOf, gather, cat, nil
      )
import ReWire.Annotation (ann, noAnn, Annote)
import ReWire.Error (failAt', MonadError, AstError)

import Control.Arrow ((&&&), second)
import Control.Monad (msum)
import Control.Monad.Except (throwError)
import Data.BitVector (BV, bitVec, (@@), nat, width, showHex, (>>.), (<<.), (==.), ashr)
import Data.Bits (Bits (..))
import Data.HashMap.Strict (HashMap)
import Data.List (foldl')
import Data.Machine ((<~), source)
import Data.Machine.MealyT (MealyT (..))
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import TextShow (showt)
import qualified Data.BitVector as BV
import qualified Data.HashMap.Strict as Map
import qualified Data.Machine as M
import qualified Data.Yaml as YAML

mkBV :: Integral v => Size -> v -> BV
mkBV sz = bitVec (fromIntegral sz)

subRange :: (Index, Index) -> BV -> BV
subRange (i, j) b = b @@ (j, i)

newtype Out = Out BV
instance Show Out where
      show (Out bv) = showHex bv
instance YAML.ToJSON Out where
      toJSON = YAML.String . pack . show

type Ins = HashMap Name Value
type Outs = HashMap Name Out
type Sts = HashMap Name Out

type DefnMap = HashMap GId Defn

-- | Runs non-interactively -- given a stream of inputs, produces a stream of outputs.
run :: Monad m => MealyT m a b -> [a] -> m [b]
run m ip = M.runT (M.autoT m <~ source ip)

interp :: MonadError AstError m => [Flag] -> Program -> MealyT m Ins Outs
interp _flags (Program st ds) = interpStartDefn defnMap st
      where defnMap :: DefnMap
            defnMap = Map.fromList $ map (defnName &&& id) ds

interpStartDefn :: MonadError AstError m => DefnMap -> StartDefn -> MealyT m Ins Outs
interpStartDefn defns (StartDefn _ w loop' state0') = MealyT $ \ _ -> do
            so <- splitOutputs <$> interpDefn defns state0 mempty
            pure (filterOutput so, unfoldMealyT f $ filterDispatch so)
      -- So:        loop   :: ((r, s), i) -> R (o, s)
      --            state0 :: R (o, s)
      --            where R = Done (a, s) | Pause (o, r, s)
      -- Assuming neither should ever be Done.
      where f :: MonadError AstError m => Sts -> Ins -> m (Outs, Sts)
            f s i = (filterOutput &&& filterDispatch) . splitOutputs <$> interpDefn defns loop (joinInputs $ Map.map outValue s <> i)

            splitOutputs :: BV -> Outs
            splitOutputs b = Map.fromList $ zip (map fst $ pauseWires w) $ map Out $ toSubRanges b $ map snd $ pauseWires w

            joinInputs :: Ins -> BV
            joinInputs vs = mconcat $ zipWith mkBV (map snd st_inps) $ map (fromMaybe 0 . flip Map.lookup vs . fst) st_inps

            st_inps :: [(Name, Size)]
            st_inps = dispatchWires w <> inputWires w

            filterOutput :: Outs -> Outs
            filterOutput = Map.filterWithKey (\ k _ -> k `elem` map fst (outputWires w))

            filterDispatch :: Outs -> Sts
            filterDispatch = Map.filterWithKey (\ k _ -> k `elem` map fst (dispatchWires w))

            outValue :: Out -> Value
            outValue (Out bv) = nat bv

            pauseWires :: Wiring -> [(Name, Size)]
            pauseWires w = pausePrefix w <> dispatchWires w

            loop   = fromMaybe (error "Core: interpStartDefn: no loop.")   $ Map.lookup loop' defns
            state0 = fromMaybe (error "Core: interpStartDefn: no state0.") $ Map.lookup state0' defns

interpDefn :: MonadError AstError m => DefnMap -> Defn -> BV -> m BV
interpDefn defns (Defn _ _ (Sig _ inSizes outSize) body) v = trunc <$> reThrow (interpExp defns (split inSizes v) body)
      where split :: [Size] -> BV -> [BV]
            split = flip toSubRanges

            trunc :: BV -> BV
            trunc = subRange (0, fromIntegral outSize - 1)

            reThrow :: MonadError AstError m => Either (Exp, AstError) BV -> m BV
            reThrow = \ case
                  Left (_, err) -> throwError err
                  Right bv      -> return bv

-- | Attempts to evalute a Core expression to a value. On error, returns a
--  potentially-partially-evaluated expression.
interpExp :: DefnMap -> [BV] -> Exp -> Either (Exp, AstError) BV
interpExp defns lvars exp = case exp of
      LVar _ _ (lkupVal -> Just v) -> pure v
      lv@LVar {}                   -> failAt' lv (ann lv) "Core/Interp: interpExp: encountered unknown LVar."
      Lit  _ bv                    -> pure bv
      Concat an e1 e2              -> do
            (v1, v2, _) <- evaluate e1 e2 (Concat an)
            pure $ v1 <> v2
      Call _ _ (Global (lkupDefn -> Just g)) e ps els -> do
            (e', els', call')  <- evaluate e els reCall
            if patMatches e' ps then
                  case interpDefn defns g (patApply e' ps) of
                        Left err -> throwError (call', err)
                        Right bv -> pure bv
            else pure els'
      Call an sz (Prim nm@(binOp -> Just op)) e ps els -> do
            (e', els', call')  <- evaluate e els reCall
            if patMatches e' ps then case patApplyR e' ps of
                  [x, y] -> pure $ op sz x y
                  _      -> failAt' call' an $ "Core/Interp: interpExp: arity mismatch (" <> showt nm <> ")."
            else pure els'
      Call an sz (Prim nm@(unOp -> Just op)) e ps els -> do
            (e', els', call')  <- evaluate e els reCall
            if patMatches e' ps then case patApplyR e' ps of
                  [x]    -> pure $ op sz x
                  _      -> failAt' call' an $ "Core/Interp: interpExp: arity mismatch (" <> showt nm <> ")."
            else pure els'
      Call _ sz (Prim Resize) e ps els -> do
            (e', els', _)  <- evaluate e els reCall
            if patMatches e' ps then pure $ mkBV sz $ nat $ patApply e' ps else pure els'
      Call _ _ (Prim Id) e ps els      -> if patMatchesE e ps then interpExp defns lvars $ patApplyE e ps else do
            (e', els', _)  <- evaluate e els reCall
            pure $ if patMatches e' ps then patApply e' ps else els'
      Call _  sz (Const v) e ps els    -> if patMatchesE e ps then pure $ mkBV sz v else do
            (e', els', _)  <- evaluate e els reCall
            pure $ if patMatches e' ps then mkBV sz v else els'
      Call an _ (Extern _ s _) _ _ _   -> failAt' exp an $ "Core/Interp: interpExp: unknown extern: " <> s
      e                                -> failAt' e (ann e) "Core/Interp: interpExp: encountered unsupported expression."
      where lkupVal :: LId -> Maybe BV
            lkupVal = flip lookup (zip [0::LId ..] lvars)

            lkupDefn :: Name -> Maybe Defn
            lkupDefn = flip Map.lookup defns

            evaluate :: MonadError (Exp, AstError) m => Exp -> Exp -> (Exp -> Exp -> Exp) -> m (BV, BV, Exp)
            evaluate e els inst = case (,) <$> me <*> mels of
                  Left (_, err)    -> throwError (call', err)
                  Right (e', els') -> pure (e', els', call')
                  where me    = interpExp defns lvars e
                        mels  = interpExp defns lvars els
                        call' = inst (toExp (ann e) me) (toExp (ann els) mels)

            reCall :: Exp -> Exp -> Exp
            reCall e' els' = case exp of
                  Call an sz t _ ps _ -> Call an sz t e' ps els'
                  c                   -> c

            toExp :: Annote -> Either (Exp, a) BV -> Exp
            toExp an = \ case
                  Left (e', _) -> e'
                  Right bv     -> Lit an bv

data PatMatch = MatchVar Index Index
              | MatchLit Index Index BV

patInterp :: [Pat] -> [PatMatch]
patInterp ps = snd $ foldl' patInterp' (fromIntegral $ sum $ map sizeOf ps, []) ps
      where patInterp' :: (Index, [PatMatch]) -> Pat -> (Index, [PatMatch])
            patInterp' (off, e) = \ case
                  PatVar _ (fromIntegral -> sz)      | sz > 0 -> (off - sz, e <> [MatchVar (off - sz) (off - 1)])
                  PatWildCard _ (fromIntegral -> sz) | sz > 0 -> (off - sz, e)
                  PatLit _ bv@(width -> sz)          | sz > 0 -> (off - sz, e <> [MatchLit (off - sz) (off - 1) bv])
                  _                                           -> (off, e)

patMatches' :: Monoid m => (Index -> Index -> BV -> m) -> [Pat] -> m
patMatches' inj = mconcat . map (\ case
      MatchLit i j bv -> inj i j bv
      _               -> mempty) . patInterp

patMatches :: BV -> [Pat] -> Bool
patMatches x = and . patMatches' (\ i j bv -> [subRange (i, j) x ==. bv])

patMatchesE :: Exp -> [Pat] -> Bool
patMatchesE es ps = and $ (length (gather es) == length ps) : zipWith match' (gather es) ps
      where match' :: Exp -> Pat -> Bool
            match' (Lit _ bv) (PatLit _ bv')     = bv ==. bv'
            match' e          (PatVar _ sz)      = sizeOf e == sz
            match' e          (PatWildCard _ sz) = sizeOf e == sz
            match' _          _                  = False

patApply' :: Monoid m => (Index -> Index -> m) -> [Pat] -> m
patApply' inj = mconcat . map (\ case
      MatchVar i j -> inj i j
      _            -> mempty) . patInterp

patApply :: BV -> [Pat] -> BV
patApply x = patApply' $ \ i j -> subRange (i, j) x

patApplyR :: BV -> [Pat] -> [BV]
patApplyR x = patApply' $ \ i j -> [subRange (i, j) x]

patApplyE :: Exp -> [Pat] -> Exp
patApplyE e = cat . zipWith apply' (gather e)
      where apply' :: Exp -> Pat -> Exp
            apply' e = \ case
                  PatVar _ _ -> e
                  _          -> nil

toSubRanges :: BV -> [Size] -> [BV]
toSubRanges x = patApplyR x . map (PatVar noAnn)

binOp :: Prim -> Maybe (Size -> BV -> BV -> BV)
binOp = flip lookup primBinOps

unOp :: Prim -> Maybe (Size -> BV -> BV)
unOp = flip lookup primUnOps

type ZB = Integer -> Integer -> Bool

primBinOps :: [(Prim, Size -> BV -> BV -> BV)]
primBinOps = map (second zToBV)
      [ (Add         , (+))
      , (Sub         , (-))
      , (Mul         , (*))
      , (Div         , div)
      , (Mod         , mod)
      , (Pow         , (^))
      , (LAnd        , coerceZ (&&))
      , (LOr         , coerceZ (||))
      , (Gt          , coerceZ' (>))
      , (GtEq        , coerceZ' (>=))
      , (Lt          , coerceZ' (<))
      , (LtEq        , coerceZ' (<=))
      , (Eq          , coerceZ' (==))
      ] <> map (second $ \ op sz a b -> mkBV sz $ nat $ op a b)
      [ (And         , (.&.))
      , (Or          , (.|.))
      , (XOr         , xor)
      , (LShift      , (<<.))
      , (RShift      , (>>.))
      , (RShiftArith , ashr)
      ] <>
      [ (Replicate, \ sz a b -> mkBV sz $ nat $ BV.replicate (nat a) b)
      ]

primUnOps :: [(Prim, Size -> BV -> BV)]
primUnOps = map (second $ \ op sz -> mkBV sz . op) unops
      where unops :: [(Prim, BV -> Integer)]
            unops = [ (LNot  , fromIntegral . fromEnum . (== bvFalse))
                    , (Not   , nat . complement) -- TODO(chathhorn): semantics?
                    , (MSBit , \ x -> toInteger $ fromEnum $ testBit x (fromEnum $ width x - 1))
                    ]

coerceZ :: (Bounded a, Bounded b, Enum a, Enum b, Enum c) => (a -> b -> c) -> Integer -> Integer -> Integer
coerceZ f a b = toZ $ f (fromZ a) (fromZ b)
      where fromZ :: forall a. (Bounded a, Enum a) => Integer -> a
            fromZ n | fromEnum n >= fromEnum (minBound :: a)
                    , fromEnum n <= fromEnum (maxBound :: a) = toEnum $ fromEnum n
                    | otherwise                              = maxBound

coerceZ' :: Enum a => (Integer -> Integer -> a) -> Integer -> Integer -> Integer
coerceZ' f a b = toZ $ f a b

toZ :: Enum a => a -> Integer
toZ = toInteger . fromEnum

zToBV :: (Integer -> Integer -> Integer) -> Size -> BV -> BV -> BV
zToBV op sz a b = mkBV sz (nat a `op` nat b)

binIntify :: (Bool -> Bool -> Bool) -> Integer -> Integer -> Integer
binIntify op a b = if (a /= 0) `op` (b /= 0) then 1 else 0

pausePadding :: Wiring -> [(Name, Size)]
pausePadding w | paddingSize > 0 = [("__padding", fromIntegral paddingSize)]
               | otherwise       = []
      where paddingSize :: Int
            paddingSize = fromIntegral (resumptionSize w)              -- sizeof PuRe
                        - 1                                              -- count (Done | Pause)
                        - fromIntegral (sum $ snd <$> resumptionTag w) -- count R_ ctors
                        - fromIntegral (sum $ snd <$> outputWires w)   --  
                        - fromIntegral (sum $ snd <$> stateWires w)    -- 

resumptionTag :: Wiring -> [(Name, Size)]
resumptionTag w | tagSize > 0 = [("__resumption_tag", fromIntegral tagSize)]
                | otherwise   = []
      where tagSize :: Int
            tagSize = case sigLoop w of
                  Sig _ (a : _) _ -> fromIntegral a - fromIntegral (sum $ snd <$> stateWires w)
                  _               -> 0

resumptionSize :: Wiring -> Size
resumptionSize (sigState0 -> Sig _ _ s) = s

continue :: (Name, Size)
continue = ("__continue", 1)

pausePrefix :: Wiring -> [(Name, Size)]
pausePrefix w = continue : pausePadding w <> outputWires w

dispatchWires :: Wiring -> [(Name, Size)]
dispatchWires w = resumptionTag w <> stateWires w

extraWires :: Wiring -> [(Name, Size)]
extraWires w = continue : pausePadding w

clock :: [Flag] -> [(Name, Size)]
clock flags | FlagNoClock `elem` flags = []
            | otherwise                = [(sClk, 1)]
      where sClk :: Name
            sClk = fromMaybe "clk" $ msum $ map (\ case
                  FlagClockName s -> Just $ pack s
                  _               -> Nothing) flags

reset :: [Flag] -> [(Name, Size)]
reset flags | FlagNoReset `elem` flags || FlagNoClock `elem` flags = []
            | otherwise                                            = [(sRst, 1)]
      where sRst :: Name
            sRst = fromMaybe (if FlagInvertReset `elem` flags then "rst_n" else "rst") $ msum $ map (\ case
                  FlagResetName s -> Just $ pack s
                  _               -> Nothing) flags

unfoldMealyT :: Applicative m => (s -> a -> m (b, s)) -> s -> MealyT m a b
unfoldMealyT f = go
      where go s = MealyT $ \ a -> (,) <$> (fst <$> f s a) <*> (go . snd <$> f s a)

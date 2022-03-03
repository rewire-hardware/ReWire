{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Interp
      ( interp, interpDefn
      , Ins, Outs, Out, run
      , patMatches, patMatches'
      , patApply, patApply'
      , interpExps, DefnMap
      , subRange
      , dispatchWires, pausePrefix, extraWires
      , resumptionSize
      , clk, rst
      ) where

import ReWire.Flags (Flag (..))
import ReWire.Core.Syntax
      ( Program (..)
      , Name, Value, Index, Size
      , Wiring (..)
      , GId, LId
      , Pat (..), Exp (..)
      , Target (..)
      , Defn (..)
      , Sig (..)
      , StartDefn (..)
      , bvFalse
      , sizeOf
      )
import ReWire.Annotation (ann, noAnn, Annote)
import ReWire.Error (failAt', MonadError, AstError)

import Control.Arrow ((&&&), second)
import Control.Monad (msum)
import Control.Monad.Except (throwError)
import Data.BitVector (BV, bitVec, (@@), nat, width, showHex, (>>.), (<<.), ashr)
import Data.Bits (Bits (..))
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.List (foldl')
import Data.Machine ((<~), source)
import Data.Machine.MealyT (MealyT (..))
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import TextShow (showt)
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
            pure (filterOutput so, unfoldMealyT f $ filterState so)
      -- So:        loop   :: ((r, s), i) -> R (o, s)
      --            state0 :: R (o, s)
      --            where R = Done (a, s) | Pause (o, r, s)
      -- Assuming neither should ever be Done.
      where f :: MonadError AstError m => Sts -> Ins -> m (Outs, Sts)
            f s i = (filterOutput &&& filterState) . splitOutputs <$> interpDefn defns loop (joinInputs $ Map.map outValue s <> i)

            splitOutputs :: BV -> Outs
            splitOutputs b = Map.fromList $ zip (map fst $ pauseWires w) $ map Out $ toSubRanges b $ map snd $ pauseWires w

            joinInputs :: Ins -> BV
            joinInputs vs = mconcat $ zipWith mkBV (map snd st_inps) $ map (fromMaybe 0 . flip Map.lookup vs . fst) st_inps

            st_inps :: [(Name, Size)]
            st_inps = dispatchWires w <> inputWires w

            filterOutput :: Outs -> Outs
            filterOutput = Map.filterWithKey (\ k _ -> k `elem` map fst (outputWires w))

            filterState :: Outs -> Sts
            filterState = Map.filterWithKey (\ k _ -> k `elem` map fst (stateWires w))

            outValue :: Out -> Value
            outValue (Out bv) = nat bv

            pauseWires :: Wiring -> [(Name, Size)]
            pauseWires w = pausePrefix w <> stateWires w

            Just loop   = Map.lookup loop' defns
            Just state0 = Map.lookup state0' defns

interpDefn :: MonadError AstError m => DefnMap -> Defn -> BV -> m BV
interpDefn defns (Defn _ _ (Sig _ inSizes outSize) body) v = trunc <$> reThrow (interpExps defns (split inSizes v) body)
      where split :: [Size] -> BV -> [BV]
            split = flip toSubRanges

            trunc :: BV -> BV
            trunc = subRange (0, fromIntegral outSize - 1)

            reThrow :: MonadError AstError m => Either ([Exp], AstError) BV -> m BV
            reThrow = \ case
                  Left (_, err) -> throwError err
                  Right bv      -> return bv

interpExps :: DefnMap -> [BV] -> [Exp] -> Either ([Exp], AstError) BV
interpExps defns lvars es = case foldMapM (interpExp defns lvars) es of
      Left (_, err) -> Left (map part es, err)
      Right bv      -> pure bv
      where part :: Exp -> Exp
            part e = case interpExp defns lvars e of
                  Left (e', _) -> e'
                  Right bv     -> Lit (ann e) bv

-- | Attempts to evalute a Core expression to a value. On error, returns a
--  potentially-partially-evaluated expression.
interpExp :: DefnMap -> [BV] -> Exp -> Either (Exp, AstError) BV
interpExp defns lvars e = case e of
      LVar _ _ (lkupVal -> Just v) -> pure v
      LVar {}                      -> failAt' e (ann e) "Core/Interp: interpExp: encountered unknown LVar."
      Lit  _ bv                    -> pure bv
      Call _ _ (Global (lkupDefn -> Just g)) es ps els -> do
            (es', els', call')  <- callExps es els
            if patMatches es' ps then
                  case interpDefn defns g (patApply es' id ps) of
                        Left err -> throwError (call', err)
                        Right bv -> pure bv
            else pure els'
      Call an sz (Extern (Sig _ argSizes _) nm@(binOp -> Just op)) es ps els -> do
            (es', els', call')  <- callExps es els
            if patMatches es' ps then case toSubRanges (patApply es' id ps) argSizes of
                  [x, y] -> pure $ op sz x y
                  _      -> failAt' call' an $ "Core/Interp: interpExp: arity mismatch (" <> showt nm <> ")."
            else pure els'
      Call an sz (Extern (Sig _ argSizes _) nm@(unOp -> Just op)) es ps els -> do
            (es', els', call')  <- callExps es els
            if patMatches es' ps then case toSubRanges (patApply es' id ps) argSizes of
                  [x]    -> pure $ op sz x
                  _      -> failAt' call' an $ "Core/Interp: interpExp: arity mismatch (" <> showt nm <> ")."
            else pure els'
      Call an sz (Extern (Sig _ argSizes _) "msbit") es ps els -> do
            (es', els', call')  <- callExps es els
            if patMatches es' ps then case toSubRanges (patApply es' id ps) argSizes of
                  [x]    -> pure $ mkBV sz $ fromEnum $ testBit x (fromEnum $ width x - 1)
                  _      -> failAt' call' an "Core/Interp: interpExp: arity mismatch (msbit)."
            else pure els'
      Call _ _ Id es ps els -> do
            (es', els', _)  <- callExps es els
            if patMatches es' ps then pure $ patApply es' id ps
            else pure els'
      Call _  sz (Const v) es ps els -> do
            (es', els', _)  <- callExps es els
            if patMatches es' ps then pure $ mkBV sz v
            else pure els'
      Call an _ (Extern _ ex) _ _ _  -> failAt' e an $ "Core/Interp: interpExp: unknown extern: " <> ex
      _                              -> failAt' e (ann e) "Core/Interp: interpExp: encountered unsupported expression."
      where lkupVal :: LId -> Maybe BV
            lkupVal = flip lookup (zip [0::LId ..] lvars)

            lkupDefn :: Name -> Maybe Defn
            lkupDefn = flip Map.lookup defns

            callExps :: MonadError (Exp, AstError) m => [Exp] -> [Exp] -> m (BV, BV, Exp)
            callExps es els = case (,) <$> mes <*> mels of
                  Left (_, err)     -> throwError (call', err)
                  Right (es', els') -> pure (es', els', call')
                  where mes   = interpExps defns lvars es
                        mels  = interpExps defns lvars els
                        call' = reCall mes mels e

            reCall :: Either ([Exp], AstError) BV -> Either ([Exp], AstError) BV -> Exp -> Exp
            reCall mes mels = \ case
                  Call an sz t _ ps _ -> Call an sz t (toExps an mes) ps (toExps an mels)
                  e                   -> e

            toExps :: Annote -> Either ([Exp], a) BV -> [Exp]
            toExps an = \ case
                  Left (es, _) -> es
                  Right bv     -> [Lit an bv]

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

patApply' :: Monoid m => (Index -> Index -> m) -> [Pat] -> m
patApply' inj = mconcat . map (\ case
      MatchVar i j -> inj i j
      _            -> mempty) . patInterp

patMatches :: BV -> [Pat] -> Bool
patMatches x = and . patMatches' (\ i j bv -> [subRange (i, j) x == bv])

patApply :: Monoid m => BV -> (BV -> m) -> [Pat] -> m
patApply x inj = patApply' (\ i j -> inj (subRange (i, j) x))

toSubRanges :: BV -> [Size] -> [BV]
toSubRanges bv szs = patApply bv pure (map (PatVar noAnn) szs)

binOp :: Name -> Maybe (Size -> BV -> BV -> BV)
binOp = flip lookup primBinOps

unOp :: Name -> Maybe (Size -> BV -> BV)
unOp = flip lookup primUnOps

primBinOps :: [(Name, Size -> BV -> BV -> BV)]
primBinOps = map (second binBitify)
      [ ( "+"   , (+))
      , ( "-"   , (-))
      , ( "*"   , (*))
      , ( "/"   , div)
      , ( "%"   , mod)
      , ( "**"  , (^))
      , ( "&&"  , binIntify (&&))
      , ( "||"  , binIntify (||))
      ] <> map (second $ \ op sz a b -> mkBV sz $ nat $ op a b)
      [ ( "&"   , (.&.))
      , ( "|"   , (.|.))
      , ( "^"   , xor)
--       , ( "~^"  , XNor)
      , ( "<<"  , (<<.))
      , ( ">>"  , (>>.))
      , ( "<<<" , (<<.))
      , ( ">>>" , ashr)
      , ( "concat" , (<>))
      ]

primUnOps :: [(Name, Size -> BV -> BV)]
primUnOps = map (second $ \ op sz -> mkBV sz . op) unops
      where unops :: [(Name, BV -> Integer)]
            unops = [ ( "!"      , fromIntegral . fromEnum . (== bvFalse))
                    , ( "~"      , nat . complement) -- TODO(chathhorn): semantics?
              --       , ( "&"      , RAnd)
              --       , ( "~&"     , RNAnd)
              --       , ( "|"      , ROr)
              --       , ( "~|"     , RNor)
              --       , ( "^"      , RXor)
              --       , ( "~^"     , RXNor)
                    , ( "resize" , nat)
                    ]

binBitify :: (Integer -> Integer -> Integer) -> Size -> BV -> BV -> BV
binBitify op sz a b = mkBV sz (nat a `op` nat b)

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
pausePrefix w = continue : pausePadding w <> outputWires w <> resumptionTag w

dispatchWires :: Wiring -> [(Name, Size)]
dispatchWires w = resumptionTag w <> stateWires w

extraWires :: Wiring -> [(Name, Size)]
extraWires w = continue : pausePadding w <> resumptionTag w

clk :: [Flag] -> [(Name, Size)]
clk flags | FlagNoClock `elem` flags = []
          | otherwise                = [(sClk, 1)]
      where sClk :: Name
            sClk = fromMaybe "clk" $ msum $ map (\ case
                  FlagClockName s -> Just $ pack s
                  _               -> Nothing) flags

rst :: [Flag] -> [(Name, Size)]
rst flags | FlagNoReset `elem` flags || FlagNoClock `elem` flags = []
          | otherwise                                            = [(sRst, 1)]
      where sRst :: Name
            sRst = fromMaybe (if FlagInvertReset `elem` flags then "rst_n" else "rst") $ msum $ map (\ case
                  FlagResetName s -> Just $ pack s
                  _               -> Nothing) flags

foldMapM :: (Monad m, Monoid w, Foldable t) => (a -> m w) -> t a -> m w
foldMapM f = foldlM (\ acc a -> f a <&> mappend acc) mempty

unfoldMealyT :: Applicative m => (s -> a -> m (b, s)) -> s -> MealyT m a b
unfoldMealyT f = go
      where go s = MealyT $ \ a -> (,) <$> (fst <$> f s a) <*> (go . snd <$> f s a)

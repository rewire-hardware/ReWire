{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Interp (interp, stateVars, interpDefn, Ins, Outs, Out, run, patMatches, patMatches', patApply, patApply', interpExps, DefnMap, subRange) where

import ReWire.Flags (Flag (..))
import ReWire.Core.Syntax
      ( Program (..)
      , Name, Value, Index, Size
      , GId, LId
      , Pat (..), Exp (..)
      , Target (..)
      , Defn (..)
      , Sig (..)
      , StartDefn (..)
      , bvFalse
      , sizeOf
      )
import ReWire.Annotation (noAnn)

import Data.List (foldl')
import Data.Machine (Mealy (..), auto, (<~), source)
import qualified Data.Machine as M
import Control.Arrow ((&&&), second)
import Data.BitVector (BV, bitVec, (@@), nat, width, showHex, (>>.), (<<.), ashr)
import Data.Bits (Bits (..))
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import Data.List.Split (splitOn)
import Data.Text (pack)
import qualified Data.HashMap.Strict as Map
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

type Outs = HashMap Name Out
type Ins = HashMap Name Value

type DefnMap = HashMap GId Defn

-- | Runs non-interactively -- given a stream of inputs, produces a stream of outputs.
run :: Mealy a b -> [a] -> [b]
run m ip = M.run (auto m <~ source ip)

interp :: [Flag] -> Program -> Mealy Ins Outs
interp flags (Program st ds) = interpStartDefn flags defnMap (stateVars flags stateSize) st
      where defnMap :: DefnMap
            defnMap = Map.fromList $ map (defnName &&& id) ds

            stateSize :: Size
            stateSize = case st of
                  StartDefn _ _ _ (_, Sig _ (arg0Size : _) _) _ -> arg0Size
                  _                                               -> 0

stateVars :: [Flag] -> Size -> [(Name, Size)]
stateVars flags totalSize = stateVars' <> if remainder > 0 then [("__state", remainder)] else []
      where -- | Take state names from the flags only as long as the sum of their
            --   sizes is less than the total bits of state we have to divvy up.
            stateVars' :: [(Name, Size)]
            stateVars' = snd $ foldl' (\ (tot, st) (n, sz) -> if tot + sz <= totalSize then (tot + sz, st <> [(n, sz)]) else (tot, st)) (0, [])
                             $ concatMap getState flags

            getState :: Flag -> [(Name, Size)]
            getState = \ case
                  FlagStateNames sts -> map toStatePair $ splitOn "," sts
                  _                  -> []

            toStatePair :: String -> (Name, Size)
            toStatePair s = case splitOn ":" s of
                  [n, sz] -> (pack n, read sz)
                  n       -> (pack $ mconcat n, 1)

            remainder :: Size
            remainder = totalSize - sum (map snd stateVars')

-- TODO(chathhorn): make state explicit?
interpStartDefn :: [Flag] -> DefnMap -> [(Name, Size)] -> StartDefn -> Mealy Ins Outs
interpStartDefn flags defns state (StartDefn _ inps outps (loop', _) (state0', Sig _ _ initSize)) =
      r $ filterOutput $ splitOutputs $ interpDefn defns state0 mempty
      -- So:        loop   :: ((r, s), i) -> R (o, s)
      --            state0 :: R (o, s)
      --            where R = Done (a, s) | Pause (o, r, s)
      -- Assuming neither should ever be Done.
      where r :: Outs -> Mealy Ins Outs
            r so = Mealy $ \ i -> (so, r $ f $ transferState so i)

            f :: Ins -> Outs
            f i' = filterOutput $ splitInputs (joinInputs i') <> splitOutputs (interpDefn defns loop $ joinInputs i')

            splitOutputs :: BV -> Outs
            splitOutputs b = Map.fromList $ zip (map fst p_outps_st) $ map Out $ toSubRanges b $ map snd p_outps_st

            joinInputs :: Ins -> BV
            joinInputs vs = mconcat $ zipWith mkBV (map snd st_inps) $ map (fromMaybe 0 . flip Map.lookup vs . fst) st_inps

            splitInputs :: BV -> Outs
            splitInputs b = Map.fromList $ zip (map (("__input_" <>) . fst) inps) $ map Out $ drop (length state) $ toSubRanges b $ map snd st_inps

            st_inps :: [(Name, Size)]
            st_inps = state <> inps

            outpSize :: Size
            outpSize = sum $ snd <$> outps

            stateSize :: Size
            stateSize = sum $ snd <$> state

            p_outps_st :: [(Name, Size)]
            p_outps_st = ("__continue", 1) : padding <> outps <> state

            filterOutput :: Outs -> Outs
            filterOutput | FlagV `elem` flags = id
                         | otherwise          = Map.filterWithKey (\ k _ -> k `elem` map fst (outps <> state))

            padding :: [(Name, Size)]
            padding | paddingSize > 0 = [("__padding", fromIntegral paddingSize)]
                    | otherwise       = []
                  where paddingSize :: Int
                        paddingSize = fromIntegral initSize - 1
                                    - fromIntegral outpSize
                                    - fromIntegral stateSize

            transferState :: Outs -> Ins -> Ins
            transferState ops' inp = foldr ((\ sn -> Map.insert sn $ maybe 0 outValue $ Map.lookup sn ops') . fst) inp state

            outValue :: Out -> Value
            outValue (Out bv) = nat bv

            Just loop   = Map.lookup loop' defns
            Just state0 = Map.lookup state0' defns

interpDefn :: DefnMap -> Defn -> BV -> BV
interpDefn defns (Defn _ _ (Sig _ inSizes outSize) body) = trunc . interpExps defns body . split inSizes
      where split :: [Size] -> BV -> [BV]
            split = flip toSubRanges

            trunc :: BV -> BV
            trunc = subRange (0, fromIntegral outSize - 1)

interpExps :: DefnMap -> [Exp] -> [BV] -> BV
interpExps defns es lvars = foldMap (interpExp defns lvars) es

interpExp :: DefnMap -> [BV] -> Exp -> BV
interpExp defns lvars = \ case
      LVar _ _ (lkupVal -> Just v) -> v
      -- LVar an _  _                    -> failAt an $ "ToVerilog: compileExp: encountered unknown LVar."
      Lit  _ bv                    -> bv
      Call _ _ (Global (lkupDefn -> Just g)) es ps els -> if patMatches (interpExps' es) ps
            then interpDefn defns g (patApply (interpExps' es) id ps)
            else interpExps defns els lvars
      Call _ sz (Extern (Sig _ argSizes _) nm@(binOp -> Just op)) es ps els -> if patMatches (interpExps' es) ps
            then case toSubRanges (patApply (interpExps' es) id ps) argSizes of
                  [x, y] -> op sz x y
                  _      -> error $ "Core/Interp: interpExp: arity mismatch (" <> show nm <> ")."
            else interpExps defns els lvars
      Call _ sz (Extern (Sig _ argSizes _) nm@(unOp -> Just op)) es ps els -> if patMatches (interpExps' es) ps
            then case toSubRanges (patApply (interpExps' es) id ps) argSizes of
                  [x]    -> op sz x
                  _      -> error $ "Core/Interp: interpExp: arity mismatch (" <> show nm <> ")."
            else interpExps defns els lvars
      Call _ sz (Extern (Sig _ argSizes _) "msbit") es ps els -> if patMatches (interpExps' es) ps
            then case toSubRanges (patApply (interpExps' es) id ps) argSizes of
                  [x]    -> mkBV sz $ fromEnum $ testBit x (fromEnum $ width x - 1)
                  _      -> error "Core/Interp: interpExp: arity mismatch (msbit)."
            else interpExps defns els lvars
      Call _ _ Id es ps els -> if patMatches (interpExps' es) ps
            then patApply (interpExps' es) id ps
            else interpExps defns els lvars
      Call _  sz (Const v) es ps els -> if patMatches (interpExps' es) ps
            then mkBV sz v
            else interpExps defns els lvars
      -- Call an _ (Extern ex) _ _ _ -> failAt an $ "ToVerilog: compileExp: unknown extern: " <> ex
      _ -> error "Core.Interp: encountered unsupported expression."
      where lkupVal :: LId -> Maybe BV
            lkupVal = flip lookup (zip [0::LId ..] lvars)

            lkupDefn :: Name -> Maybe Defn
            lkupDefn = flip Map.lookup defns

            interpExps' :: [Exp] -> BV
            interpExps' = flip (interpExps defns) lvars

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

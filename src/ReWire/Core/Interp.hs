{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Interp (interp, SV, run) where

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
      )

import Data.List (foldl')
import Data.Machine (Mealy (..), auto, (<~), source)
import qualified Data.Machine as M
import Control.Arrow ((&&&), (***))
-- import Data.BitVector.LittleEndian (BitVector, fromNumber, subRange, toUnsignedNumber, toSignedNumber, isZeroVector, dimension)
import Data.BitVector (BV, bitVec, (@@), uint, int, zeros, width)
import Data.Bits (Bits (..))
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

-- type BV = BitVector
fromNumber sz = bitVec (fromIntegral sz)
subRange (i, j) b = b @@ (j, i)
toUnsignedNumber = fromIntegral . uint

toSignedNumber :: Num a => BV -> a
toSignedNumber = fromIntegral . int

isZeroVector = (== zeros 1)
dimension = fromIntegral . width

type SV = HashMap Name Value
type M = Mealy SV SV
type DefnMap = HashMap GId Defn

-- | Runs non-interactively -- given a stream of inputs, produces a stream of outputs.
run :: M -> [SV] -> [SV]
run m ip = M.run (auto m <~ source ip)

interp :: [Flag] -> Program -> M
interp flags (Program st ds) = interpStartDefn defnMap st
      where defnMap :: DefnMap
            defnMap = Map.fromList $ map (defnName &&& id) ds

-- TODO(chathhorn): make state explicit?
interpStartDefn :: DefnMap -> StartDefn -> M
interpStartDefn defns (StartDefn _ inps outps (loop, (Sig _ (arg0Size:_) _)) (state0, Sig _ _ initSize)) = do
      let Just loop'   = Map.lookup loop defns
          Just state0' = Map.lookup state0 defns
          f            = splitOutputs . interpDefn defns loop' . joinInputs
          r so         = Mealy $ \ i -> (so <> i, r $ f $ transferState so i)
      r $ splitOutputs $ interpDefn defns state0' mempty
      -- So:        loop   :: (r, s, i) -> R (o, s)
      --            state0 :: R (o, s)
      --            where R = Done (a, s) | Pause (o, r, s)
      -- Assuming neither should ever be Done.
      -- TODO(chathhorn): handle Done/Pause: need to look at initial bit?
      where splitOutputs :: BV -> SV
            splitOutputs b = Map.fromList $ zip (map fst p_outps_st) $ map (toUnsignedNumber :: BV -> Value) $ toSubRanges b $ map snd p_outps_st

            joinInputs :: SV -> BV
            joinInputs vs = mconcat $ zipWith fromNumber (map snd st_inps) $ map snd $ insMap vs $ map (id *** const 0) st_inps

            st_inps :: [(Name, Size)]
            st_inps = state : inps

            p_outps_st :: [(Name, Size)]
            p_outps_st = ("__is_pause", 1) : outps <> [state]

            state :: (Name, Size)
            state = (stateName, initSize - sum (map snd outps) - 1) -- 1 for __is_pause

            stateName :: Name
            stateName = "__state"

            insMap :: SV -> [(Name, Value)] -> [(Name, Value)]
            insMap m = \ case
                  []                              -> []
                  ((k@(lkup -> Just v'), v) : vs) -> (k, v') : insMap m vs
                  (v : vs)                        -> v : insMap m vs
                  where lkup :: Name -> Maybe Value
                        lkup = flip Map.lookup m

            transferState :: SV -> SV -> SV
            transferState ops' = Map.insert stateName $ fromMaybe 0 $ Map.lookup stateName ops'

interpDefn :: DefnMap -> Defn -> BV -> BV
interpDefn defns (Defn _ n (Sig _ inSizes outSize) body) = trunc . interpExps defns body . split inSizes
      where split :: [Size] -> BV -> [BV]
            split = flip toSubRanges

            trunc :: BV -> BV
            trunc = subRange (0, outSize - 1)

interpExps :: DefnMap -> [Exp] -> [BV] -> BV
interpExps defns es lvars = foldMap (interpExp defns lvars) es

interpExp :: DefnMap -> [BV] -> Exp -> BV
interpExp defns lvars = \ case
      LVar _ sz (lkupVal -> Just v) -> v
      -- LVar an _  _                    -> failAt an $ "ToVerilog: compileExp: encountered unknown LVar."
      Lit  _ sz v                    -> fromNumber sz v
      Call _ sz (Global (lkupDefn -> Just g')) es ps els -> if patMatches (interpExps' es) ps
            then interpDefn defns g' (patApply (interpExps' es) ps)
            else interpExps defns els lvars
      Call _ sz (Extern (Sig _ argSizes _) (binOp -> Just op)) es ps els -> if patMatches (interpExps' es) ps
            then let [x, y] = toSubRanges (patApply (interpExps' es) ps) argSizes
                 in op sz x y
            else interpExps defns els lvars
      Call _ sz (Extern (Sig _ argSizes _) (unOp -> Just op)) es ps els -> if patMatches (interpExps' es) ps
            then let [x] = toSubRanges (patApply (interpExps' es) ps) argSizes
                 in op sz x
            else interpExps defns els lvars
      Call _ sz (Extern (Sig _ argSizes _) "msbit") es ps els -> if patMatches (interpExps' es) ps
            then let [x] = toSubRanges (patApply (interpExps' es) ps) argSizes
                 in fromNumber sz $ fromEnum $ testBit x (fromEnum $ dimension x - 1)
            else interpExps defns els lvars
      Call _  sz Id es ps els -> if patMatches (interpExps' es) ps
            then patApply (interpExps' es) ps
            else interpExps defns els lvars
      Call _  sz (Const v) es ps els -> if patMatches (interpExps' es) ps
            then fromNumber sz v
            else interpExps defns els lvars
      -- Call an _ (Extern ex) _ _ _ -> failAt an $ "ToVerilog: compileExp: unknown extern: " <> ex
      where lkupVal :: LId -> Maybe BV
            lkupVal = flip lookup (zip [0::LId ..] lvars)

            lkupDefn :: Name -> Maybe Defn
            lkupDefn = flip Map.lookup defns

            interpExps' :: [Exp] -> BV
            interpExps' = flip (interpExps defns) lvars

patMatches :: BV -> [Pat] -> Bool
patMatches x = snd . foldl' patMatch (0, True)
      where patMatch :: (Index, Bool) -> Pat -> (Index, Bool)
            patMatch (off, e) = \ case
                  PatVar _ sz      | sz > 0 -> (off + fromIntegral(sz), e)
                  PatWildCard _ sz | sz > 0 -> (off + fromIntegral(sz), e)
                  PatLit _ sz v    | sz > 0 ->
                        ( off + fromIntegral(sz)
                        , (&&) e $ subRange (fromIntegral(off), fromIntegral(off) + sz - 1) x == fromNumber sz v
                        )
                  _                         -> (off, e)

patApply :: BV -> [Pat] -> BV
patApply x = snd . foldl' patArg (0, mempty)
      where patArg :: (Index, BV) -> Pat -> (Index, BV)
            patArg (off, lvs) = \ case
                  PatVar _ sz      | sz > 0 ->
                        ( off + fromIntegral(sz)
                        , lvs <> subRange (fromIntegral(off), fromIntegral(off) + sz - 1) x
                        )
                  PatWildCard _ sz | sz > 0 -> (off + fromIntegral(sz), lvs)
                  PatLit _ sz _    | sz > 0 -> (off + fromIntegral(sz), lvs)
                  _                         -> (off, lvs)

-- | Converts a list of sizes to ranges. E.g.,
-- > toRanges [2, 4, 5] == [(0, 1), (2, 5), (6, 10)]
toRanges :: [Size] -> [(Word, Word)] -- Word to match subRange.
toRanges s = zipWith (\ a b -> (a, b - 1)) (scanl (+) 0 s) $ tail $ scanl (+) 0 s

toSubRanges :: BV -> [Size] -> [BV]
toSubRanges bv = map (flip subRange bv) . toRanges . fillSizeVec
      where fillSizeVec :: [Size] -> [Size]
            fillSizeVec szs | dimension bv > sum szs = szs <> [dimension bv - sum szs]
                            | otherwise              = szs

binOp :: Name -> Maybe (Size -> BV -> BV -> BV)
binOp = flip lookup primBinOps

unOp :: Name -> Maybe (Size -> BV -> BV)
unOp = flip lookup primUnOps

primBinOps :: [(Name, Size -> BV -> BV -> BV)]
primBinOps = map (id *** binBitify)
      [ ( "+"   , (+))
      , ( "-"   , (-))
      , ( "*"   , (*))
      , ( "/"   , div)
      , ( "%"   , mod)
      , ( "**"  , (^))
      , ( "&&"  , binIntify (&&))
      , ( "||"  , binIntify (||))
      ] <> map (id *** \ op sz a b -> fromNumber sz $ toSignedNumber $ op a b)
      [ ( "&"   , (.&.))
      , ( "|"   , (.|.))
      , ( "^"   , xor)
--       , ( "~^"  , XNor)
      , ( "<<"  , lshift)
      , ( ">>"  , rshift)
--       , ( "<<<" , LShiftArith)
--       , ( ">>>" , RShiftArith)
      ]

primUnOps :: [(Name, Size -> BV -> BV)]
primUnOps = map (id *** \ op sz -> fromNumber sz . op)
      [ ( "!"      , fromEnum . isZeroVector)
      , ( "~"      , toSignedNumber . complement) -- TODO(chathhorn): semantics?
--       , ( "&"      , RAnd)
--       , ( "~&"     , RNAnd)
--       , ( "|"      , ROr)
--       , ( "~|"     , RNor)
--       , ( "^"      , RXor)
--       , ( "~^"     , RXNor)
      , ( "resize" , toSignedNumber)
      ]

binBitify :: (Integer -> Integer -> Integer) -> Size -> BV -> BV -> BV
binBitify op sz a b = fromNumber sz (toSignedNumber a `op` toSignedNumber b)

binIntify :: (Bool -> Bool -> Bool) -> Integer -> Integer -> Integer
binIntify op a b = if (a /= 0) `op` (b /= 0) then 1 else 0

lshift :: BV -> BV -> BV
lshift a b = a `shiftL` toSignedNumber b

rshift :: BV -> BV -> BV
rshift a b = a `shiftR` toSignedNumber b

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Interp (interp, interpDefn, Ins, Outs, Out, run, patMatches, patApply, interpExps, DefnMap) where

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
import ReWire.Annotation (noAnn)

import Data.List (foldl')
import Data.Machine (Mealy (..), auto, (<~), source)
import qualified Data.Machine as M
import Control.Arrow ((&&&), (***))
import Data.BitVector (BV, bitVec, (@@), uint, int, zeros, width, showHex)
import Data.Bits (Bits (..))
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import Data.Text (pack)
import qualified Data.HashMap.Strict as Map
import qualified Data.Yaml as YAML

-- type BV = BitVector
fromNumber sz = bitVec (fromIntegral sz)
subRange (i, j) b = b @@ (j, i)
toUnsignedNumber = fromIntegral . uint
toSignedNumber :: Num a => BV -> a
toSignedNumber = fromIntegral . int
isZeroVector = (== zeros 1)

newtype Out = Out BV
instance Show Out where
      show (Out bv) = "[" <> show (width bv) <> "] " <> showHex bv
instance YAML.ToJSON Out where
      toJSON = YAML.String . pack . show

type Outs = HashMap Name Out
type Ins = HashMap Name Value

type DefnMap = HashMap GId Defn

-- | Runs non-interactively -- given a stream of inputs, produces a stream of outputs.
run :: Mealy a b -> [a] -> [b]
run m ip = M.run (auto m <~ source ip)

interp :: [Flag] -> Program -> Mealy Ins Outs
interp flags (Program st ds) = interpStartDefn defnMap st
      where defnMap :: DefnMap
            defnMap = Map.fromList $ map (defnName &&& id) ds

-- TODO(chathhorn): make state explicit?
interpStartDefn :: DefnMap -> StartDefn -> Mealy Ins Outs
interpStartDefn defns (StartDefn _ inps outps (loop, (Sig _ (arg0Size:_) _)) (state0, Sig _ _ initSize)) = do
      let Just loop'   = Map.lookup loop defns
          Just state0' = Map.lookup state0 defns
          -- f            = splitOutputs . interpDefn defns loop' . joinInputs
          f i'         = Map.insert "__input" (Out $ joinInputs i') $ splitOutputs $ interpDefn defns loop' $ joinInputs i'
          r so         = Mealy $ \ i -> (so, r $ f $ transferState so i)
      r $ splitOutputs $ interpDefn defns state0' mempty
      -- So:        loop   :: ((r, s), i) -> R (o, s)
      --            state0 :: R (o, s)
      --            where R = Done (a, s) | Pause (o, r, s)
      -- Assuming neither should ever be Done.
      -- TODO(chathhorn): handle Done/Pause: need to look at initial bit?
      where splitOutputs :: BV -> Outs
            splitOutputs b = Map.fromList $ (("__everything", Out b):) $ zip (map fst p_outps_st) $ map Out $ toSubRanges b $ map snd p_outps_st

            joinInputs :: Ins -> BV
            joinInputs vs = mconcat $ zipWith fromNumber (map snd st_inps) $ map (fromMaybe 0 . flip Map.lookup vs . fst) st_inps

            st_inps :: [(Name, Size)]
            st_inps = state : inps

            outpSize :: Size
            outpSize = sum $ snd <$> outps

            p_outps_st :: [(Name, Size)]
            p_outps_st = ("__continue", 1) : padding <> outps <> [state]

            state :: (Name, Size)
            state = (stateName, arg0Size)

            padding :: [(Name, Size)]
            padding | paddingSize > 0 = [("__padding", fromIntegral(paddingSize))]
                    | otherwise       = []
                  where paddingSize :: Int
                        paddingSize = fromIntegral(initSize) - 1
                                    - fromIntegral(outpSize)
                                    - fromIntegral(arg0Size)

            stateName :: Name
            stateName = "__state"

            transferState :: Outs -> Ins -> Ins
            transferState ops' = Map.insert stateName $ maybe 0 outValue $ Map.lookup stateName ops'

            outValue :: Out -> Value
            outValue (Out bv) = toUnsignedNumber bv

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
      Lit  _ bv                   -> bv
      Call _ sz (Global (lkupDefn -> Just g)) es ps els -> if patMatches (interpExps' es) ps
            then interpDefn defns g (patApply (interpExps' es) id ps)
            else interpExps defns els lvars
      Call _ sz (Extern (Sig _ argSizes _) (binOp -> Just op)) es ps els -> if patMatches (interpExps' es) ps
            then let [x, y] = toSubRanges (patApply (interpExps' es) id ps) argSizes
                 in op sz x y
            else interpExps defns els lvars
      Call _ sz (Extern (Sig _ argSizes _) (unOp -> Just op)) es ps els -> if patMatches (interpExps' es) ps
            then let [x] = toSubRanges (patApply (interpExps' es) id ps) argSizes
                 in op sz x
            else interpExps defns els lvars
      Call _ sz (Extern (Sig _ argSizes _) "msbit") es ps els -> if patMatches (interpExps' es) ps
            then let [x] = toSubRanges (patApply (interpExps' es) id ps) argSizes
                 in fromNumber sz $ fromEnum $ testBit x (fromEnum $ width x - 1)
            else interpExps defns els lvars
      Call _  sz Id es ps els -> if patMatches (interpExps' es) ps
            then patApply (interpExps' es) id ps
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
patMatches x = snd . foldl' patMatch (width x, True)
      where patMatch :: (Index, Bool) -> Pat -> (Index, Bool)
            patMatch (off, e) = \ case
                  PatVar _ (fromIntegral -> sz)      | sz > 0       -> (off - sz, e)
                  PatWildCard _ (fromIntegral -> sz) | sz > 0       -> (off - sz, e)
                  PatLit _ bv                        | width bv > 0 -> (off - width bv , (&&) e $ subRange (off - width bv, off - 1) x == bv)
                  _                                                 -> (off, e)

patApply :: Monoid m => BV -> (BV -> m) -> [Pat] -> m
patApply x inj = snd . foldl' patArg (width x, mempty)
      where -- patArg :: Monoid m => (Index, m) -> Pat -> (Index, m)
            patArg (off, lvs) = \ case
                  PatVar _ (fromIntegral -> sz)      | sz > 0 -> (off - sz, lvs <> inj (subRange (off - sz, off - 1) x))
                  PatWildCard _ (fromIntegral -> sz) | sz > 0 -> (off - sz, lvs)
                  PatLit _ (width -> sz)             | sz > 0 -> (off - sz, lvs)
                  _                                           -> (off, lvs)

toSubRanges :: BV -> [Size] -> [BV]
toSubRanges bv szs = patApply bv pure (map (PatVar noAnn) szs)

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

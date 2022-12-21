{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module PreWire.Primitives
      ( Identity, ReacT, StateT, Vec, R_, A_, PuRe (..), Ref (..), Proxy (..)
      , rwPrimError
      , rwPrimExtern
      , rwPrimSetRef
      , rwPrimGetRef
      , rwPrimBind
      , rwPrimReturn
      , rwPrimPut
      , rwPrimGet
      , rwPrimSignal
      , rwPrimLift
      , rwPrimExtrude
      , rwPrimUnfold
      , rwPrimVecFromList
      , rwPrimVecReplicate
      , rwPrimVecReverse
      , rwPrimVecSlice
      , rwPrimVecRSlice
      , rwPrimVecIndex
      , rwPrimVecUpdate
      , rwPrimVecConcat
      , rwPrimVecLength
      , rwPrimBits
      , rwPrimResize
      , rwPrimAdd
      , rwPrimSub
      , rwPrimMul
      , rwPrimDiv
      , rwPrimMod
      , rwPrimPow
      , rwPrimLAnd
      , rwPrimLOr
      , rwPrimAnd
      , rwPrimOr
      , rwPrimXOr
      , rwPrimXNor
      , rwPrimLShift
      , rwPrimRShift
      , rwPrimRShiftArith
      , rwPrimEq
      , rwPrimGt
      , rwPrimGtEq
      , rwPrimLt
      , rwPrimLtEq
      , rwPrimConcat
      , rwPrimLNot
      , rwPrimNot
      , rwPrimRAnd
      , rwPrimRNAnd
      , rwPrimROr
      , rwPrimRNor
      , rwPrimRXOr
      , rwPrimRXNor
      , rwPrimMSBit
      , type (+), type Monad, type MonadTrans
      ) where

-- Imports in this file are ignored by rwc.
import Prelude hiding (reverse, foldr, foldl, foldl1, zipWith, 
          take, drop, length, map, (++), or, and, head, tail, replicate)
import qualified Prelude as GHC
import Control.Monad.Identity
import Control.Monad.Resumption.Reactive
import Control.Monad.State
import GHC.TypeLits (type (+), KnownNat, natVal, natVal')
import Data.Vector.Sized
import Data.Bits
import Data.Bool()
import Data.Proxy ( Proxy(..) )
import Data.Bifunctor (first)
import PreWire.BitWord


-- ReWire primitives.

-- Primitive types:
-- data (->) a b
-- data ReacT i o m a
-- data StateT s m a
-- data Identity a
-- data Integer
-- data Bit

-- Also tuples:
-- data () = ()
-- data (a, b) = (a, b)
-- ...

data R_ -- Ctors generated during program build.
data A_ -- Ctors generated during program build.
type Vec n a = Vector n a -- Ctors built in.
newtype Ref a = Ref String
data PuRe s o = Done (A_, s) | Pause (o, (R_, s))

-- Definitions in this file are for ghc compat and ignored by rwc.

rwPrimError :: String -> a
rwPrimError = error

-- | The String and list arguments must be literals (after inlining).
rwPrimExtern :: [(String, Integer)] -- ^ Module parameters (name and integer literal value).
             -> String              -- ^ Clock signal name or empty for no clock.
             -> [(String, Integer)] -- ^ Module inputs (name and integer literal bitwidth).
             -> [(String, Integer)] -- ^ Module outputs (name and integer literal bitwidth).
             -> String              -- ^ Module name.
             -> a                   -- ^ Haskell definition to use when interpreting.
             -> String              -- ^ Instance name to use in generated Verilog.
             -> a
rwPrimExtern _ _ _ _ _ f _ = f

rwPrimSetRef :: Ref a -> a -> b -> b
rwPrimSetRef _ _ b = b

rwPrimGetRef :: Ref a -> a
rwPrimGetRef = error "Prim: get reference"

rwPrimBind :: Monad m => m a -> (a -> m b) -> m b
rwPrimBind = (>>=)

rwPrimReturn :: Monad m => a -> m a
rwPrimReturn = return

rwPrimPut :: Monad m => s -> StateT s m ()
rwPrimPut = put

rwPrimGet :: Monad m => StateT s m s
rwPrimGet = get

rwPrimSignal :: Monad m => o -> ReacT i o m i
rwPrimSignal = signal

rwPrimLift :: (MonadTrans t, Monad m) => m a -> t m a
rwPrimLift = lift

rwPrimExtrude :: Monad m => ReacT i o (StateT s m) a -> s -> ReacT i o m a
rwPrimExtrude (ReacT (StateT m)) s = 
   ReacT $ do (res,s') <- m s
              case res of
                Left y -> return (Left y)
                Right (o,k) -> return (Right (o, \ i -> rwPrimExtrude (k i) s'))

{- PuRe s o = Done (A_, s) | Pause (o, (R_, s)) -}
rwPrimUnfold :: ((R_, s) -> i -> PuRe s o) -> PuRe s o -> ReacT i o Identity A_
rwPrimUnfold _ (Done (a,_)) = return a
rwPrimUnfold f (Pause (o,b)) = do i <- signal o
                                  rwPrimUnfold f (f b i)

-- *** Built-in Vec functions. ***

-- | Turns a List literal into a Vec with fixed length. I.e.,
-- > [x, y, z] :: Vec 3 a
rwPrimVecFromList :: KnownNat n => [a] -> Vec n a
rwPrimVecFromList v = case fromList v of
   Just v' -> v'
   Nothing -> error "failed fromList: list is a different length than expected"


rwPrimVecReplicate :: KnownNat n => a -> Vec n a
rwPrimVecReplicate = replicate

rwPrimVecReverse :: Vec n a -> Vec n a
rwPrimVecReverse = reverse

rwPrimVecSlice :: (KnownNat i, KnownNat n) => Proxy i -> Vec ((i + n) + m) a -> Vec n a
rwPrimVecSlice = slice

-- | Slice indexed from the end of the Vec. Could be defined as:
-- > slice' i = reverse . slice i . reverse
rwPrimVecRSlice :: (KnownNat i, KnownNat n) => Proxy i -> Vec ((i + n) + m) a -> Vec n a
rwPrimVecRSlice i = reverse . slice i . reverse

rwPrimVecIndex :: KnownNat n => Vec ((n + m) + 1) a -> Proxy n -> a
rwPrimVecIndex = index'

rwPrimVecUpdate :: KnownNat n => Vec ((n + m) + 1) a -> Proxy n -> a -> Vec ((n + m) + 1) a
rwPrimVecUpdate v i a = update v (singleton (fromEnum $ natVal i,a))

-- | Concatenate vectors.
rwPrimVecConcat :: Vec n a -> Vec m a -> Vec (n + m) a
rwPrimVecConcat = (++)

rwPrimVecLength :: KnownNat n => Vec n a -> Int
rwPrimVecLength = length

-- w is bigendian
padTrunc' :: Int -> [Bool] -> [Bool]
padTrunc' d w 
      | l == d    = w
      | l < d     = pad' (d - l) w
      | otherwise = GHC.reverse . GHC.take d . GHC.reverse $ w
         where
           l = GHC.length w

-- | Interpret (NOT anything) an Integer as a bit vector.
rwPrimBits :: forall n. KnownNat n => Integer -> Vec n Bool
rwPrimBits = 
      rwPrimVecFromList . padTrunc' i 
                        . GHC.reverse . int2bits'
        where
          i = fromIntegral (natVal (Proxy :: Proxy n))

fromBool :: Bool -> Integer
fromBool = toInteger . fromEnum

-- should check that this works on big-endian
toInteger' :: Vec n Bool -> Integer
toInteger' = foldr (\ x s -> fromBool x + 2 * s) 0

{-
-- w is bigendian
resize' :: Int -> [Bit] -> [Bit]
resize' d w | l Prelude.== d = w
            | l Prelude.< d  = pad' (d Prelude.- l) w
            | l Prelude.> d  = reverse . take d . reverse $ w
       where
         l = length w
-}

-- | Truncates or zero-pads most significant bits.
rwPrimResize :: forall m n . KnownNat m => Vec n Bool -> Vec m Bool
rwPrimResize v = rwPrimVecFromList vs'
    where
      vs = toList v
      vs' = resize' (fromEnum (natVal (Proxy :: Proxy m))) vs


-- *** Primitive bitwise operations based on Verilog operators. ***

-- | Add.
rwPrimAdd :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimAdd v w = rwPrimVecFromList $ plus' (toList v) (toList w)

-- | Subtract.
rwPrimSub :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimSub v w = rwPrimVecFromList $ minus' (toList v) (toList w)

-- | Multiply.
rwPrimMul :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimMul v w = rwPrimVecFromList $ times' (toList v) (toList w)

-- | Divide.
rwPrimDiv :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimDiv v w = rwPrimVecFromList $ divide' (toList v) (toList w)

-- | Modulus.
rwPrimMod :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimMod v w = rwPrimVecFromList $ mod' (toList v) (toList w)

-- | Exponentiation.
rwPrimPow :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimPow v w = rwPrimVecFromList $ power' (toList v) (toList w)

-- | Logical and.
rwPrimLAnd :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLAnd v w = or v && or w

-- | Logical or.
rwPrimLOr :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLOr v w = or v || or w

-- | Bitwise and.
rwPrimAnd :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimAnd = zipWith (&&)

-- TODO(chathhorn): removing the dot causes the parser to choke.
-- | Bitwise or.
rwPrimOr :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimOr = zipWith (||)

-- | Bitwise exclusive or.
rwPrimXOr :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimXOr = zipWith xor

-- | Bitwise exclusive nor.
rwPrimXNor :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimXNor = zipWith (\ x y -> not (xor x y))

-- | Shift left.
rwPrimLShift :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimLShift v i = rwPrimVecFromList $ shiftL' (toList v) (toList i)

-- | Shift right.
rwPrimRShift :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimRShift v i = rwPrimVecFromList $ shiftR' (toList v) (toList i)

-- | Shift right, sign-extend.
rwPrimRShiftArith :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimRShiftArith v i = rwPrimVecFromList $ arithShiftR' (toList v) (toList i)

-- | Equal.
rwPrimEq :: Vec n Bool -> Vec m Bool -> Bool
rwPrimEq v w = toInteger' v == toInteger' w

-- Reminder: we assume all bit vectors are unsigned
-- | Greater-than.
rwPrimGt :: Vec n Bool -> Vec m Bool -> Bool
rwPrimGt v w = toInteger' v > toInteger' w

-- | Greater-than or equal.
rwPrimGtEq :: Vec n Bool -> Vec m Bool -> Bool
rwPrimGtEq v w = toInteger' v <= toInteger' w

-- | Less-than.
rwPrimLt :: Vec n Bool -> Vec m Bool -> Bool
rwPrimLt v w = toInteger' v < toInteger' w

-- | Less-than or equal.
rwPrimLtEq :: Vec n Bool -> Vec m Bool -> Bool
rwPrimLtEq v w = toInteger' v <= toInteger' w

-- | Concatenate.
rwPrimConcat :: Vec n Bool -> Vec m Bool -> Vec (n + m) Bool
rwPrimConcat = (++)

-- | Logical not.
rwPrimLNot :: Vec n Bool -> Bool
rwPrimLNot v = not (or v)  -- note that 'or' acts as 'toBool'

-- | Bitwise not.
rwPrimNot :: Vec n Bool -> Vec n Bool
rwPrimNot = map not

-- | Reduction and.
rwPrimRAnd :: Vec n Bool -> Bool
rwPrimRAnd = and

-- | Reduction nand.
rwPrimRNAnd :: Vec (1 + n) Bool -> Bool
rwPrimRNAnd = foldl1 (\ x y -> not (x && y)) 

-- | Reduction or.
rwPrimROr :: Vec n Bool -> Bool
rwPrimROr = or

-- | Reduction nor.
rwPrimRNor :: Vec (1 + n) Bool -> Bool
rwPrimRNor = foldl1 (\ x y -> not (x || y))

-- | Reduction xor.
rwPrimRXOr :: Vec (1 + n) Bool -> Bool
rwPrimRXOr = foldl1 xor

-- | Reduction xnor.
rwPrimRXNor :: Vec (1 + n) Bool -> Bool
rwPrimRXNor = foldl1 (\ x y -> not (xor x y))

-- | Most significant bit.
rwPrimMSBit :: Vec (1 + n) Bool -> Bool
rwPrimMSBit = head

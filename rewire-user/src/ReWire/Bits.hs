{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module ReWire.Bits where

import ReWire
import ReWire.Finite
import Prelude hiding (head, (<>), (==), (-), (^), (&&), (||))

type Lit = W 128

zero :: Bit
zero = False

one :: Bit
one = True

{-# INLINE bit #-}
bit :: W 1 -> Bit
bit = msbit

{-# INLINE toInteger #-}
toInteger :: W n -> Integer
toInteger = rwPrimToInteger

-- | Project range of bits.
--   a @@ (j, i) returns bits j (most significant) to i (least significant) from a (j >= i).
--   The Integer arguments must be non-negative integer literals (after inlining).
{-# INLINE (@@) #-}
(@@) :: (KnownNat n,KnownNat m) => W n -> (Integer, Integer) -> W m
a @@ (j, i) = bitSlice a j i

-- | Project single bit.
{-# INLINE (@.) #-}
(@.) :: KnownNat n => W n -> Integer -> Bit
a @. i = bitIndex a i

-- infixr 9 @., @@

-- *** Primitive bitwise operations based on Verilog operators. ***

infixr 9 **
infixl 8  *, /, %
infixl 7  +, -
infixl 6  <<., >>., >>>
infixl 6  >, >=, <, <=
infixr 6  <>
infixl 5  .&.
infixl 4  ^, ~^, `xor`
infixl 3  .|.
infixr 2  &&., &&&
infixr 1  ||., |||

-- | Interpret an Integer literal into a bit vector. Truncates most significant
--   bits or zero-pads to make it fit.
{-# INLINE lit #-}
lit :: KnownNat n => Integer -> W n
lit i = rwPrimResize (rwPrimBits i :: Lit)

-- | Resize bitvector, truncating or zero padding most significant bits.
{-# INLINE resize #-}
resize :: KnownNat m => W n -> W m
resize = rwPrimResize

-- | Sign extend bitvector
{-# INLINE sext #-}
sext :: KnownNat m => W n -> W m
sext = rwPrimSignextend

{-# INLINE bitSlice #-}
bitSlice :: (KnownNat n, KnownNat m) => W n -> Integer -> Integer -> W m
bitSlice v j i = finBitSlice v (finite j) (finite i)

{-# INLINE bitIndex #-}
bitIndex :: KnownNat n => W n -> Integer -> Bit
bitIndex v i = finBitIndex v (finite i)

{-# INLINE finBitSlice #-}
finBitSlice :: KnownNat m => W n -> Finite n -> Finite n -> W m
finBitSlice = rwPrimBitSlice

{-# INLINE finBitIndex #-}
finBitIndex :: W n -> Finite n -> Bit
finBitIndex = rwPrimBitIndex

-- | Add.
{-# INLINE (+) #-}
(+) :: KnownNat n => W n -> W n -> W n
(+) = rwPrimAdd

-- | Subtract.
{-# INLINE (-) #-}
(-) :: KnownNat n => W n -> W n -> W n
(-) = rwPrimSub

-- | Multiply.
{-# INLINE (*) #-}
(*) :: KnownNat n => W n -> W n -> W n
(*) = rwPrimMul

-- | Divide.
{-# INLINE (/) #-}
(/) :: KnownNat n => W n -> W n -> W n
(/) = rwPrimDiv

-- | Modulus.
{-# INLINE (%) #-}
(%) :: KnownNat n => W n -> W n -> W n
(%) = rwPrimMod

-- | Exponentiation.
{-# INLINE (**) #-}
(**) :: KnownNat n => W n -> W n -> W n
(**) = rwPrimPow

-- | Prelude (&&), but using built-ins.
{-# INLINE (&&&) #-}
(&&&) :: Bool -> Bool -> Bool
(&&&) a b = bit $ (fromList [a] :: W 1) .&. (fromList [b] :: W 1)

-- | Prelude (||), but using built-ins.
{-# INLINE (|||) #-}
(|||) :: Bool -> Bool -> Bool
(|||) a b = bit $ (fromList [a] :: W 1) .|. (fromList [b] :: W 1)

-- | Logical and.
{-# INLINE (&&.) #-}
(&&.) :: W n -> W n -> Bool
(&&.) = rwPrimLAnd

-- | Logical or.
{-# INLINE (||.) #-}
(||.) :: W n -> W n -> Bool
(||.) = rwPrimLOr

-- | Logical not.
{-# INLINE lnot #-}
lnot :: W n -> Bit
lnot = rwPrimLNot

-- | Bitwise and.
{-# INLINE (.&.) #-}
(.&.) :: W n -> W n -> W n
(.&.) = rwPrimAnd

-- | Bitwise or.
{-# INLINE (.|.) #-}
(.|.) :: W n -> W n -> W n
(.|.) = rwPrimOr

-- | Bitwise not.
{-# INLINE bnot #-}
bnot :: W n -> W n
bnot = rwPrimNot

-- | Bitwise exclusive or.
{-# INLINE (^) #-}
(^) :: W n -> W n -> W n
(^) = rwPrimXOr

-- | Logical xor, for Bit/Bool (using the built-in operator).
{-# INLINE xor #-}
xor :: Bool -> Bool -> Bool
xor a b = bit $ fromList [a] ^ fromList [b]

-- | Bitwise exclusive nor.
{-# INLINE (~^) #-}
(~^) :: W n -> W n -> W n
(~^) = rwPrimXNor

-- | Shift left.
{-# INLINE (<<.) #-}
(<<.) :: KnownNat n => W n -> W n -> W n
(<<.) = rwPrimLShift

-- | Shift right.
{-# INLINE (>>.) #-}
(>>.) :: KnownNat n => W n -> W n -> W n
(>>.) = rwPrimRShift

-- | Shift right, sign-extend.
{-# INLINE (>>>) #-}
(>>>) :: KnownNat n => W n -> W n -> W n
(>>>) = rwPrimRShiftArith

-- Could implement on Vec (m + n)? Could also reimplement Lshift/Rshift to use Proxy?
-- | Rotate right
{-# INLINE rotR #-}
rotR :: KnownNat m => W m -> W m -> W m
rotR n w = (w >>. n) .|. (w <<. (lit (len w) - n))

-- | Rotate left
{-# INLINE rotL #-}
rotL :: KnownNat m => W m -> W m -> W m
rotL n w = (w <<. n) .|. (w >>. (lit (len w) - n))

-- | Equal.
{-# INLINE (==) #-}
(==) :: W n -> W n -> Bool
(==) = rwPrimEq

-- | Not equal.
{-# INLINE (/=) #-}
(/=) :: W n -> W n -> Bool
(/=) a b = not (a == b)

-- | Greater-than.
{-# INLINE (>) #-}
(>) :: W n -> W n -> Bool
(>) = rwPrimGt

-- | Greater-than or equal.
{-# INLINE (>=) #-}
(>=) :: W n -> W n -> Bool
(>=) = rwPrimGtEq

-- | Less-than.
{-# INLINE (<) #-}
(<) :: W n -> W n -> Bool
(<) = rwPrimLt

-- | Less-than or equal.
{-# INLINE (<=) #-}
(<=) :: W n -> W n -> Bool
(<=) = rwPrimLtEq

-- | Concatenate.
{-# INLINE (<>) #-}
(<>) :: W n -> W m -> W (n + m)
(<>) = rwPrimVecConcat

-- | Reduction and.
{-# INLINE rAnd #-}
rAnd :: W n -> Bit
rAnd = rwPrimRAnd

-- | Reduction nand.
{-# INLINE rNAnd #-}
rNAnd :: W (1 + n) -> Bit
rNAnd = rwPrimRNAnd

-- | Reduction or.
{-# INLINE rOr #-}
rOr :: W n -> Bit
rOr = rwPrimROr

-- | Reduction nor.
{-# INLINE rNor #-}
rNor :: W (1 + n) -> Bit
rNor = rwPrimRNor

-- | Reduction xor.
{-# INLINE rXOr #-}
rXOr :: W (1 + n) -> Bit
rXOr = rwPrimRXOr

-- | Reduction xnor.
{-# INLINE rXNor #-}
rXNor :: W (1 + n) -> Bit
rXNor = rwPrimRXNor

-- | Most significant bit.
{-# INLINE msbit #-}
msbit :: W (1 + n) -> Bit
msbit = rwPrimMSBit

-- | Least significant bit.
{-# INLINE odd #-}
odd :: W (1 + n) -> Bool
odd b = bit (resize b :: W 1)

-- | Least significant bit.
{-# INLINE even #-}
even :: W (1 + n) -> Bool
even b = not (bit (resize b :: W 1))


{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module PreWire.Word where

import PreWire
import PreWire.Bit
import Prelude hiding (head, (<>), (==), (-))
import GHC.TypeLits ( KnownNat )

type W n = Vec n Bit


{-# INLINE bit #-}
bit :: W 1 -> Bit
bit = msbit

{-
-- | Project range of bits.
--   a @@ (j, i) returns bits j (most significant) to i (least significant) from a (j >= i).
--   The Integer arguments must be non-negative integer literals (after inlining).
{-# INLINE (@@) #-}
(@@) :: W n -> (Integer, Integer) -> W m
a @@ (j, i) = bitSlice a j i

-- | Project single bit.
{-# INLINE (@.) #-}
(@.) :: W n -> Integer -> Bit
a @. i = head (bitSlice a i i)

-- infixr 9 @., @@
-}
-- *** Primitive bitwise operations based on Verilog operators. ***

infixr 9 **
infixl 9 !
infixl 8  *, /, %
infixl 7  +, -
infixl 6  <<., >>., >>>
infixl 6  >, >=, <, <=
infixl 5  .&.
infixl 4  ^, ~^
infixl 3  .|.
infixr 2  &&.
infixr 1  ||.
infixl 0  <>

-- | Interpret an Integer literal into a bit vector. Truncates most significant
--   bits or zero-pads to make it fit.
{-# INLINE lit #-}
lit :: KnownNat n => Integer -> W n
lit = bits

-- | Bitvector representation.
{-# INLINE bits #-}
bits :: KnownNat n => Integer -> W n
bits = rwPrimBits

-- | Resize bitvector, truncating or zero padding most significant bits.
{-# INLINE resize #-}
resize :: KnownNat m => W n -> W m
resize = rwPrimResize

-- | lookup value at index n in vector
{-# INLINE (!) #-}
(!) :: KnownNat n => Vec ((n + m) + 1) a -> Proxy n -> a
(!) = index

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

-- | Logical and.
{-# INLINE (&&.) #-}
(&&.) :: W n -> W n -> Bool
(&&.) = rwPrimLAnd

-- | Logical or.
{-# INLINE (||.) #-}
(||.) :: W n -> W n -> Bool
(||.) = rwPrimLOr

-- | Bitwise and.
{-# INLINE (.&.) #-}
(.&.) :: W n -> W n -> W n
(.&.) = rwPrimAnd

-- TODO(chathhorn): removing the dot causes the parser to choke.
-- | Bitwise or.
{-# INLINE (.|.) #-}
(.|.) :: W n -> W n -> W n
(.|.) = rwPrimOr

-- | Bitwise exclusive or.
{-# INLINE (^) #-}
(^) :: W n -> W n -> W n
(^) = rwPrimXOr

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

-- | Rotate right
{-# INLINE rotR #-}
rotR :: KnownNat m => Vec m Bool -> Vec m Bool -> Vec m Bool
rotR n w = (w >>. n) .|. (w <<. (lit (toInteger $ PreWire.len w) - n))

-- | Rotate left
{-# INLINE rotL #-}
rotL :: KnownNat m => Vec m Bool -> Vec m Bool -> Vec m Bool
rotL n w = (w <<. n) .|. (w >>. (lit (toInteger $ PreWire.len w) - n))

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
(>) :: W n -> W m -> Bool
(>) = rwPrimGt

-- | Greater-than or equal.
{-# INLINE (>=) #-}
(>=) :: W n -> W m -> Bool
(>=) = rwPrimGtEq

-- | Less-than.
{-# INLINE (<) #-}
(<) :: W n -> W m -> Bit
(<) = rwPrimLt

-- | Less-than or equal.
{-# INLINE (<=) #-}
(<=) :: W n -> W m -> Bit
(<=) = rwPrimLtEq

-- | Concatenate.
{-# INLINE (<>) #-}
(<>) :: W n -> W m -> W (n + m)
(<>) = rwPrimConcat

-- | Logical not.
{-# INLINE lnot #-}
lnot :: W n -> Bit
lnot = rwPrimLNot

-- | Bitwise not.
{-# INLINE bnot #-}
bnot :: W n -> W n
bnot = rwPrimNot

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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module ReWire.Bits where

import ReWire
import Prelude hiding (head, (<>))

type Bit = Bool
type W n = Vec n Bit

zero :: Bit
zero = False

one :: Bit
one = True

{-# INLINE bit #-}
bit :: W 1 -> Bit
bit = msbit

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

-- *** Primitive bitwise operations based on Verilog operators. ***

infixr 9 **
infixl 8  *, /, %
infixl 7  +, -
infixl 6  <<., >>., >>>
infixl 6  >, >=, <, <=
infixl 5  &.
infixl 4  ^, ~^
infixl 3  |.
infixr 2  &&.
infixr 1  ||.
infixl 0  <>

-- | Interpret an Integer literal into a bit vector. Truncates most significant
--   bits or zero-pads to make it fit.
{-# INLINE lit #-}
lit :: Integer -> W n
lit = bits

-- | Bitvector representation.
{-# INLINE bits #-}
bits :: a -> W n
bits = rwPrimBits

-- | Resize bitvector, truncating or zero padding most significant bits.
{-# INLINE resize #-}
resize :: W n -> W m
resize = rwPrimResize

{-# INLINE bitSlice #-}
bitSlice :: W n -> Integer -> Integer -> W m
bitSlice = rwPrimBitSlice

{-# INLINE bitIndex #-}
bitIndex :: W n -> Integer -> Bit
bitIndex = rwPrimBitIndex

-- | Add.
{-# INLINE (+) #-}
(+) :: W n -> W n -> W n
(+) = rwPrimAdd

-- | Subtract.
{-# INLINE (-) #-}
(-) :: W n -> W n -> W n
(-) = rwPrimSub

-- | Multiply.
{-# INLINE (*) #-}
(*) :: W n -> W n -> W n
(*) = rwPrimMul

-- | Divide.
{-# INLINE (/) #-}
(/) :: W n -> W n -> W n
(/) = rwPrimDiv

-- | Modulus.
{-# INLINE (%) #-}
(%) :: W n -> W n -> W n
(%) = rwPrimMod

-- | Exponentiation.
{-# INLINE (**) #-}
(**) :: W n -> W n -> W n
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
{-# INLINE (&.) #-}
(&.) :: W n -> W n -> W n
(&.) = rwPrimAnd

-- TODO(chathhorn): removing the dot causes the parser to choke.
-- | Bitwise or.
{-# INLINE (|.) #-}
(|.) :: W n -> W n -> W n
(|.) = rwPrimOr

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
(<<.) :: W n -> W n -> W n
(<<.) = rwPrimLShift

-- | Shift right.
{-# INLINE (>>.) #-}
(>>.) :: W n -> W n -> W n
(>>.) = rwPrimRShift

-- | Shift right, sign-extend.
{-# INLINE (>>>) #-}
(>>>) :: W n -> W n -> W n
(>>>) = rwPrimRShiftArith

-- | Equal.
{-# INLINE (==) #-}
(==) :: W n -> W m -> Bool
(==) = rwPrimEq

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
rNAnd :: W n -> Bit
rNAnd = rwPrimRNAnd

-- | Reduction or.
{-# INLINE rOr #-}
rOr :: W n -> Bit
rOr = rwPrimROr

-- | Reduction nor.
{-# INLINE rNor #-}
rNor :: W n -> Bit
rNor = rwPrimRNor

-- | Reduction xor.
{-# INLINE rXOr #-}
rXOr :: W n -> Bit
rXOr = rwPrimRXOr

-- | Reduction xnor.
{-# INLINE rXNor #-}
rXNor :: W n -> Bit
rXNor = rwPrimRXNor

-- | Most significant bit.
{-# INLINE msbit #-}
msbit :: W n -> Bit
msbit = rwPrimMSBit

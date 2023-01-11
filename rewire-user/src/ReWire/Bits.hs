{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module ReWire.Bits where

import ReWire
import Prelude hiding (head, (<>), (==), (-), (^), (&&), (||))

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
a @. i = bitIndex a i

-- infixr 9 @., @@

-- *** Primitive bitwise operations based on Verilog operators. ***

infixr 9 **
infixl 9 !
infixl 8  *, /, %
infixl 7  +, -
infixl 6  <<., >>., >>>
infixl 6  >, >=, <, <=
infixr 6  <>
infixl 5  .&.
infixl 4  ^, ~^, `xor`
infixl 3  .|.
infixr 2  &&., &&, !=
infixr 1  ||., ||

-- | Interpret an Integer literal into a bit vector. Truncates most significant
--   bits or zero-pads to make it fit.
{-# INLINE lit #-}
lit :: KnownNat n => Integer -> W n
lit i = rwPrimResize (rwPrimBits i :: W 128)

-- | Resize bitvector, truncating or zero padding most significant bits.
{-# INLINE resize #-}
resize :: KnownNat m => W n -> W m
resize = rwPrimResize

{-# INLINE bitSlice #-}
bitSlice :: W n -> Integer -> Integer -> W m
bitSlice = rwPrimBitSlice

{-# INLINE bitIndex #-}
bitIndex :: W n -> Integer -> Bit
bitIndex = rwPrimBitIndex

-- | lookup value at index n in vector
{-# INLINE (!) #-}
(!) :: KnownNat n => Vec ((n + m) + 1) a -> Proxy n -> a
(!) = index

-- | assign new value a to index i
{-# INLINE (!=) #-}
(!=) :: KnownNat n => Vec ((n + m) + 1) a -> Proxy n -> a -> Vec ((n + m) + 1) a
v != i = update v i

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

-- | Prelude (&&), but using built-ins.
{-# INLINE (&&) #-}
(&&) :: Bool -> Bool -> Bool
(&&) a b = bit $ fromList [a] .&. fromList [b]

-- | Logical or.
{-# INLINE (||.) #-}
(||.) :: W n -> W n -> Bool
(||.) = rwPrimLOr

-- | Logical not.
{-# INLINE lnot #-}
lnot :: W n -> Bit
lnot = rwPrimLNot

-- | Prelude (||), but using built-ins.
{-# INLINE (||) #-}
(||) :: Bool -> Bool -> Bool
(||) a b = bit $ fromList [a] .|. fromList [b]

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

-- Note: rotates feel like they won't compile
-- Could implement on Vec (m + n)? Could also reimplement Lshift/Rshift to use Proxy?
-- | Rotate right
{-# INLINE rotR #-}
rotR :: KnownNat m => Vec m Bool -> Vec m Bool -> Vec m Bool
rotR n w = (w >>. n) .|. (w <<. (lit (ReWire.len w) - n))

-- | Rotate left
{-# INLINE rotL #-}
rotL :: KnownNat m => Vec m Bool -> Vec m Bool -> Vec m Bool
rotL n w = (w <<. n) .|. (w >>. (lit (ReWire.len w) - n))

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
(<) :: W n -> W n -> Bit
(<) = rwPrimLt

-- | Less-than or equal.
{-# INLINE (<=) #-}
(<=) :: W n -> W n -> Bit
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

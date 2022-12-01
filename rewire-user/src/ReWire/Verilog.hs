{-# LANGUAGE TypeOperators #-}
module ReWire.Verilog
      ( (+), (-), (*), (/)
      , (%), (**), (^), (~^)
      , (.|.), (.&.)
      , (||), (&&)
      , (<<), (>>), (<<<), (>>>)
      , (>), (>=), (<), (<=)
      , (<>)
      , lnot, bnot, rAnd, rNAnd
      , rOr, rNor, rXor, rXNor
      , msbit
      ) where

import ReWire
import ReWire.Bits
import Prelude (undefined)

infixr 9 **
infixl 8  *, /, %
infixl 7  +, -
infixl 6  <<, >>, <<<, >>>
infixl 6  >, >=, <, <=
infixl 5  .&.
infixl 4  ^, ~^
infixl 3  .|.
infixr 2  &&
infixr 1  ||
infixl 0  <>

-- | Add.
(+) :: W n -> W n -> W n
(+) = extern "+" undefined

-- | Subtract.
(-) :: W n -> W n -> W n
(-) = extern "-" undefined

-- | Multiply.
(*) :: W n -> W n -> W n
(*) = extern "*" undefined

-- | Divide.
(/) :: W n -> W n -> W n
(/) = extern "/" undefined

-- | Modulus.
(%) :: W n -> W n -> W n
(%) = extern "%" undefined

-- | Exponentiation.
(**) :: W n -> W n -> W n
(**) = extern "**" undefined

-- | Logical and.
(&&) :: W n -> W n -> Bit
(&&) = extern "&&" undefined

-- | Logical or.
(||) :: W n -> W n -> Bit
(||) = extern "||" undefined

-- | Bitwise and.
(.&.) :: W n -> W n -> W n
(.&.) = extern "&" undefined

-- TODO(chathhorn): removing the dot causes the parser to choke.
-- | Bitwise or.
(.|.) :: W n -> W n -> W n
(.|.) = extern "|" undefined

-- | Bitwise exclusive or.
(^) :: W n -> W n -> W n
(^) = extern "^" undefined

-- | Bitwise exclusive nor.
(~^) :: W n -> W n -> W n
(~^) = extern "~^" undefined

-- | Shift left.
(<<) :: W n -> W n -> W n
(<<) = extern "<<" undefined

-- | Shift right.
(>>) :: W n -> W n -> W n
(>>) = extern ">>" undefined

-- | Shift left.
(<<<) :: W n -> W n -> W n
(<<<) = extern "<<<" undefined

-- | Shift right, sign-extend.
(>>>) :: W n -> W n -> W n
(>>>) = extern ">>>" undefined

-- | Greater-than.
(>) :: W n -> W m -> Bit
(>) = extern ">" undefined

-- | Greater-than or equal.
(>=) :: W n -> W m -> Bit
(>=) = extern ">=" undefined

-- | Less-than.
(<) :: W n -> W m -> Bit
(<) = extern "<" undefined

-- | Less-than or equal.
(<=) :: W n -> W m -> Bit
(<=) = extern "<=" undefined

-- | Concatenate.
(<>) :: W n -> W m -> W (n + m)
(<>) = extern "concat" undefined

-- | Logical not.
lnot :: W n -> Bit
lnot = extern "!" undefined

-- | Bitwise not.
bnot :: W n -> W n
bnot = extern "~" undefined

-- | Reduction and.
rAnd :: W n -> Bit
rAnd = extern "&" undefined

-- | Reduction nand.
rNAnd :: W n -> Bit
rNAnd = extern "~&" undefined

-- | Reduction or.
rOr :: W n -> Bit
rOr = extern "|" undefined

-- | Reduction nor.
rNor :: W n -> Bit
rNor = extern "~|" undefined

-- | Reduction xor.
rXor :: W n -> Bit
rXor = extern "^" undefined

-- | Reduction xnor.
rXNor :: W n -> Bit
rXNor = extern "~^" undefined

-- | Most significant bit.
msbit :: W n -> Bit
msbit = extern "msbit" undefined

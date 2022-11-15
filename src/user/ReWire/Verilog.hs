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
      , msbit, lit, resize, replicate
      ) where

import ReWire
import Prelude (undefined, Integer)

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
(+) :: a -> a -> a
(+) = extern "+" undefined

-- | Subtract.
(-) :: a -> a -> a
(-) = extern "-" undefined

-- | Multiply.
(*) :: a -> a -> a
(*) = extern "*" undefined

-- | Divide.
(/) :: a -> a -> a
(/) = extern "/" undefined

-- | Modulus.
(%) :: a -> a -> a
(%) = extern "%" undefined

-- | Exponentiation.
(**) :: a -> a -> a
(**) = extern "**" undefined

-- | Logical and.
(&&) :: a -> a -> Bit
(&&) = extern "&&" undefined

-- | Logical or.
(||) :: a -> a -> Bit
(||) = extern "||" undefined

-- | Bitwise and.
(.&.) :: a -> a -> a
(.&.) = extern "&" undefined

-- TODO(chathhorn): removing the dot causes the parser to choke.
-- | Bitwise or.
(.|.) :: a -> a -> a
(.|.) = extern "|" undefined

-- | Bitwise exclusive or.
(^) :: a -> a -> a
(^) = extern "^" undefined

-- | Bitwise exclusive nor.
(~^) :: a -> a -> a
(~^) = extern "~^" undefined

-- | Shift left.
(<<) :: a -> a -> a
(<<) = extern "<<" undefined

-- | Shift right.
(>>) :: a -> a -> a
(>>) = extern ">>" undefined

-- | Shift left.
(<<<) :: a -> a -> a
(<<<) = extern "<<<" undefined

-- | Shift right, sign-extend.
(>>>) :: a -> a -> a
(>>>) = extern ">>>" undefined

-- | Greater-than.
(>) :: a -> b -> Bit
(>) = extern ">" undefined

-- | Greater-than or equal.
(>=) :: a -> b -> Bit
(>=) = extern ">=" undefined

-- | Less-than.
(<) :: a -> b -> Bit
(<) = extern "<" undefined

-- | Less-than or equal.
(<=) :: a -> b -> Bit
(<=) = extern "<=" undefined

-- | Concatenate.
(<>) :: a -> b -> c
(<>) = extern "concat" undefined

-- | Logical not.
lnot :: a -> Bit
lnot = extern "!" undefined

-- | Bitwise not.
bnot :: a -> a
bnot = extern "~" undefined

-- | Reduction and.
rAnd :: a -> Bit
rAnd = extern "&" undefined

-- | Reduction nand.
rNAnd :: a -> Bit
rNAnd = extern "~&" undefined

-- | Reduction or.
rOr :: a -> Bit
rOr = extern "|" undefined

-- | Reduction nor.
rNor :: a -> Bit
rNor = extern "~|" undefined

-- | Reduction xor.
rXor :: a -> Bit
rXor = extern "^" undefined

-- | Reduction xnor.
rXNor :: a -> Bit
rXNor = extern "~^" undefined

-- | Most significant bit.
msbit :: a -> Bit
msbit = extern "msbit" undefined

-- | Repeat.
replicate :: Integer -> a -> b
replicate = extern "replicate" undefined

-- | Turn an Integer literal into any type! Truncates most significant bits or
--   zero-pads to make it fit.
lit :: Integer -> a
lit = extern "resize" undefined

-- | Turns any type into any other type! Truncates most significant bits or
--   zero-pads to make it fit.
resize :: a -> b
resize = extern "resize" undefined

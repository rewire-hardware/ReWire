module ReWire.Verilog
      ( (+), (-), (*), (/)
      , (%), (**), (^), (~^)
      , (.|.), (.&.)
      , (||), (&&)
      , (<<), (>>), (<<<), (>>>)
      , (<>)
      , lnot, bnot, rAnd, rNAnd
      , rOr, rNor, rXor, rXNor
      , msbit, lit, resize, replicate
      ) where

import ReWire
import Prelude (undefined, Integer)

-- | Add.
{-# INLINE (+) #-}
(+) :: a -> a -> a
(+) = extern "+" add'

add' :: a -> a -> a
add' = undefined

-- | Subtract.
{-# INLINE (-) #-}
(-) :: a -> a -> a
(-) = extern "-" sub'

sub' :: a -> a -> a
sub' = undefined

-- | Multiply.
{-# INLINE (*) #-}
(*) :: a -> a -> a
(*) = extern "*" mul'

mul' :: a -> a -> a
mul' = undefined

-- | Divide.
{-# INLINE (/) #-}
(/) :: a -> a -> a
(/) = extern "/" div'

div' :: a -> a -> a
div' = undefined

-- | Modulus.
{-# INLINE (%) #-}
(%) :: a -> a -> a
(%) = extern "%" mod'

mod' :: a -> a -> a
mod' = undefined

-- | Power.
{-# INLINE (**) #-}
(**) :: a -> a -> a
(**) = extern "**" pow'

pow' :: a -> a -> a
pow' = undefined

-- | Logical and.
{-# INLINE (&&) #-}
(&&) :: a -> a -> Bit
(&&) = extern "&&" land'

land' :: a -> a -> Bit
land' = undefined

-- | Logical or.
{-# INLINE (||) #-}
(||) :: a -> a -> Bit
(||) = extern "||" lor'

lor' :: a -> a -> Bit
lor' = undefined

-- | Bitwise and.
{-# INLINE (.&.) #-}
(.&.) :: a -> a -> a
(.&.) = extern "&" and'

and' :: a -> a -> a
and' = undefined

-- TODO(chathhorn): removing the dot causes the parser to choke.
-- | Bitwise or.
{-# INLINE (.|.) #-}
(.|.) :: a -> a -> a
(.|.) = extern "|" or'

or' :: a -> a -> a
or' = undefined

-- | Bitwise exclusive or.
{-# INLINE (^) #-}
(^) :: a -> a -> a
(^) = extern "^" xor'

xor' :: a -> a -> a
xor' = undefined

-- | Bitwise exclusive nor.
{-# INLINE (~^) #-}
(~^) :: a -> a -> a
(~^) = extern "~^" xnor'

xnor' :: a -> a -> a
xnor' = undefined

-- | Shift left.
{-# INLINE (<<) #-}
(<<) :: a -> a -> a
(<<) = extern "<<" shiftl'

shiftl' :: a -> a -> a
shiftl' = undefined

-- | Shift right.
{-# INLINE (>>) #-}
(>>) :: a -> a -> a
(>>) = extern ">>" shiftr'

shiftr' :: a -> a -> a
shiftr' = undefined

-- | Shift left.
{-# INLINE (<<<) #-}
(<<<) :: a -> a -> a
(<<<) = extern "<<<" ashiftl'

ashiftl' :: a -> a -> a
ashiftl' = undefined

-- | Shift right, sign-extend.
{-# INLINE (>>>) #-}
(>>>) :: a -> a -> a
(>>>) = extern ">>>" ashiftr'

ashiftr' :: a -> a -> a
ashiftr' = undefined

-- | Concatenate.
{-# INLINE (<>) #-}
(<>) :: a -> b -> c
(<>) = extern "concat" concat'

concat' :: a -> b -> c
concat' = undefined

infixr 9 **
infixl 8 *, /, %
infixl 7 +, -
infixl 6 <<, >>, <<<, >>>
infixl 5 .&.
infixl 4 ^, ~^
infixl 3 .|.
infixr 2 &&
infixr 1 ||
infixl 0 <>

-- | Logical not.
{-# INLINE lnot #-}
lnot :: a -> Bit
lnot = extern "!" lnot'

lnot' :: a -> Bit
lnot' = undefined

-- | Bitwise not.
{-# INLINE bnot #-}
bnot :: a -> a
bnot = extern "~" bnot'

bnot' :: a -> a
bnot' = undefined

-- | Reduction and.
{-# INLINE rAnd #-}
rAnd :: a -> Bit
rAnd = extern "&" rAnd'

rAnd' :: a -> Bit
rAnd' = undefined

-- | Reduction nand.
{-# INLINE rNAnd #-}
rNAnd :: a -> Bit
rNAnd = extern "~&" rNAnd'

rNAnd' :: a -> Bit
rNAnd' = undefined

-- | Reduction or.
{-# INLINE rOr #-}
rOr :: a -> Bit
rOr = extern "|" rOr'

rOr' :: a -> Bit
rOr' = undefined

-- | Reduction nor.
{-# INLINE rNor #-}
rNor :: a -> Bit
rNor = extern "~|" rNor'

rNor' :: a -> Bit
rNor' = undefined

-- | Reduction xor.
{-# INLINE rXor #-}
rXor :: a -> Bit
rXor = extern "^" rXor'

rXor' :: a -> Bit
rXor' = undefined

-- | Reduction xnor.
{-# INLINE rXNor #-}
rXNor :: a -> Bit
rXNor = extern "~^" rXNor'

rXNor' :: a -> Bit
rXNor' = undefined

-- | Most significant bit.
{-# INLINE msbit #-}
msbit :: a -> Bit
msbit = extern "msbit" msbit'

msbit' :: a -> Bit
msbit' = undefined

-- | Repeat.
{-# INLINE replicate #-}
replicate :: Integer -> a -> b
replicate = extern "replicate" replicate'

replicate' :: Integer -> a -> b
replicate' = undefined

-- | Turn an Integer literal into any type! Truncates most significant bits or
--   zero-pads to make it fit.
{-# INLINE lit #-}
lit :: Integer -> a
lit = extern "resize" lit'

lit' :: Integer -> a
lit' = undefined

-- | Turns any type into any other type! Truncates most significant bits or
--   zero-pads to make it fit.
{-# INLINE resize #-}
resize :: a -> b
resize = extern "resize" resize'

resize' :: a -> b
resize' = undefined

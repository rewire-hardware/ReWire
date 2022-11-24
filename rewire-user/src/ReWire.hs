module ReWire
      ( module RWC.Primitives
      , extern, (@@), (@.), modify
      ) where

import RWC.Primitives

-- | The String argument must be a string literal (after inlining).
{-# INLINE extern #-}
extern :: String -> a -> a
extern n a = externWithSig [] "" [] [] n a ""

-- | Project range of bits.
{-# INLINE (@@) #-}
(@@) :: a -> (Integer, Integer) -> b
a @@ (j, i) = bits a j i

-- | Project single bit.
{-# INLINE (@.) #-}
(@.) :: a -> Integer -> Bit
a @. i = bit a i

infixr 9 @., @@

{-# INLINE modify #-}
modify :: Monad m => (s -> s) -> StT s m ()
modify f = get `rwBind` (\ x -> put (f x))

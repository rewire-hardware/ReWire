{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module ReWire
      ( module RWC.Primitives
      , extern
      , (@@), (@.)
      , modify
      , empty, singleton, cons, snoc, head, last, length
      , take, init, drop, tail
      ) where

import RWC.Primitives

import Prelude hiding (head, length, init, tail, last, (++), take, drop)

-- | The String argument must be a string literal (after inlining).
{-# INLINE extern #-}
extern :: String -> a -> a
extern n a = externWithSig [] "" [] [] n a ""

-- | Project range of bits.
--   a @@ (j, i) returns bits j (most significant) to i (least significant) from a (j >= i).
--   The Integer arguments must be non-negative integer literals (after inlining).
{-# INLINE (@@) #-}
(@@) :: Vec n Bit -> (Integer, Integer) -> Vec m Bit
a @@ (j, i) = bitSlice a j i

-- | Project single bit.
{-# INLINE (@.) #-}
(@.) :: Vec n Bit -> Integer -> Bit
a @. i = head (bitSlice a i i)

-- infixr 9 @., @@

{-# INLINE empty #-}
empty :: Vec 0 a
empty = fromList []

{-# INLINE singleton #-}
singleton :: a -> Vec 1 a
singleton a = fromList [a]

{-# INLINE cons #-}
cons :: a -> Vec n a -> Vec (1 + n) a
cons a v = fromList [a] ++ v

{-# INLINE snoc #-}
snoc :: Vec n a -> a -> Vec (n + 1) a
snoc v a = v ++ fromList [a]

head :: Vec (1 + n) a -> a
head v = index v (Proxy :: Proxy 0)

last :: Vec (n + 1) a -> a
last v = index v (lastIndex v)

lastIndex :: Vec (n + 1) a -> Proxy n
lastIndex _ = Proxy

length :: Vec n a -> Proxy n
length _ = Proxy

take :: Vec (n + m) a -> Vec n a
take v = slice (Proxy :: Proxy 0) v

init :: Vec (n + 1) a -> Vec n a
init v = take v

drop :: Vec (n + m) a -> Vec m a
drop v = slice' (Proxy :: Proxy 0) v

tail :: Vec (1 + n) a -> Vec n a
tail v = drop v

{-# INLINE modify #-}
modify :: Monad m => (s -> s) -> StT s m ()
modify f = get `rwBind` (\ x -> put (f x))

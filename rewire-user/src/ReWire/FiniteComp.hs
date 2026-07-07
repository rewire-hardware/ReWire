{-# LANGUAGE DataKinds #-}
module ReWire.FiniteComp where

import ReWire
import ReWire.Finite
import qualified ReWire.Bits as B


-- |
-- | Operations on Finites are actually operations on words
-- |

{-# INLINE (+) #-}
(+) :: KnownNat n => Finite n -> Finite n -> Finite n
a + b = toFinite' $ (fromFinite a :: B.Lit) B.+ fromFinite b

{-# INLINE (-) #-}
(-) :: KnownNat n => Finite n -> Finite n -> Finite n
a - b = toFinite' $ (fromFinite a :: B.Lit) B.- fromFinite b

{-# INLINE (*) #-}
(*) :: KnownNat n => Finite n -> Finite n -> Finite n
a * b = toFinite' $ (fromFinite a :: B.Lit) B.* fromFinite b

{-# INLINE div #-}
div :: KnownNat n => Finite n -> Finite n -> Finite n
div a b = toFinite' $ (fromFinite a :: B.Lit) B./ fromFinite b

{-# INLINE mod #-}
mod :: KnownNat n => Finite n -> Finite n -> Finite n
mod a b = toFinite' $ (fromFinite a :: B.Lit) B.% fromFinite b

{-# INLINE (==) #-}
(==) :: Finite n -> Finite n -> Bool
a == b = (fromFinite a :: B.Lit) B.== (fromFinite b :: B.Lit)

{-# INLINE (<) #-}
(<) :: Finite n -> Finite n -> Bool
a < b = (fromFinite a :: B.Lit) B.< (fromFinite b :: B.Lit)

{-# INLINE (<=) #-}
(<=) :: Finite n -> Finite n -> Bool
a <= b = (fromFinite a :: B.Lit) B.<= (fromFinite b :: B.Lit)

{-# INLINE (>) #-}
(>) :: Finite n -> Finite n -> Bool
a > b = (fromFinite a :: B.Lit) B.> (fromFinite b :: B.Lit)

{-# INLINE (>=) #-}
(>=) :: Finite n -> Finite n -> Bool
a >= b = (fromFinite a :: B.Lit) B.>= (fromFinite b :: B.Lit)

{-# INLINE even #-}
even :: Finite n -> Bool
even a = B.even (fromFinite a :: B.Lit)

{-# INLINE odd #-}
odd :: Finite n -> Bool
odd a = B.odd (fromFinite a :: B.Lit)

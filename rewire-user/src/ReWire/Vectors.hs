{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module ReWire.Vectors where

import ReWire
import qualified ReWire.Finite as F
import qualified ReWire.FiniteComp as FC
import Prelude hiding ((++), zip, take, drop, map, zipWith)

{-# INLINE replicate #-}
replicate :: KnownNat n => a -> Vec n a
replicate = rwPrimVecReplicate

{-# INLINE reverse #-}
reverse :: Vec n a -> Vec n a
reverse = rwPrimVecReverse

{-# INLINE slice #-}
slice :: (KnownNat i, KnownNat n) => Proxy i -> Vec ((i + n) + m) a -> Vec n a
slice = rwPrimVecSlice

{-# INLINE rslice #-}
rslice :: (KnownNat i, KnownNat n) => Proxy i -> Vec ((i + n) + m) a -> Vec n a
rslice = rwPrimVecRSlice

{-# INLINE index #-}
index :: Vec n a -> Finite n -> a
index = rwPrimVecIndex

{-# INLINE index' #-}
index' :: KnownNat n => Vec ((n + m) + 1) a -> Proxy n -> a
index' = rwPrimVecIndexProxy

{-# INLINE (++) #-}
(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(++) = rwPrimVecConcat

{-# INLINE empty #-}
empty :: Vec 0 a
empty = fromList []

{-# INLINE singleton #-}
singleton :: a -> Vec 1 a
singleton a = fromList [a]

{-# INLINE cons #-}
cons :: a -> Vec n a -> Vec (1 + n) a
cons x v = fromList [x] ++ v

{-# INLINE snoc #-}
snoc :: Vec n a -> a -> Vec (n + 1) a
snoc v x = v ++ fromList [x]

{-# INLINE head #-}
head :: Vec (1 + n) a -> a
head v = index' v (Proxy :: Proxy 0)

{-# INLINE last #-}
last :: KnownNat n => Vec n a -> a
last v = index v (lastIndex v)

{-# INLINE lastIndex #-}
lastIndex :: KnownNat n => Vec n a -> Finite n
lastIndex _ = F.maxBound

-- {-# INLINE lastIndex' #-}
-- lastIndex' :: KnownNat n => Vec (1 + n) a -> Proxy n
-- lastIndex' _ = Proxy

{-# INLINE take #-}
take :: KnownNat n => Vec (n + m) a -> Vec n a
take = slice (Proxy :: Proxy 0)

{-# INLINE init #-}
init :: KnownNat n => Vec (n + 1) a -> Vec n a
init = take

{-# INLINE drop #-}
drop :: KnownNat m => Vec (n + m) a -> Vec m a
drop = rslice (Proxy :: Proxy 0)

{-# INLINE tail #-}
tail :: KnownNat n => Vec (1 + n) a -> Vec n a
tail = drop

{-# INLINE update #-}
update :: KnownNat n => Vec n a -> Finite n -> a -> Vec n a
update v i x = generate (\ j -> if j FC.== i then x else index v j)

-- {-# INLINE bulkUpdate #-}
-- bulkUpdate :: KnownNat n => Vec n a -> Vec m (Finite n,a) -> Vec n a
-- bulkUpdate = rwPrimVecBulkUpdate

{-# INLINE map #-}
map :: (a -> b) -> Vec n a -> Vec n b
map = rwPrimVecMap

{-# INLINE generate #-}
generate :: KnownNat n => (Finite n -> a) -> Vec n a
generate = rwPrimVecGenerate

-- {-# INLINE iterate #-}
-- iterate :: KnownNat n => Proxy n -> (a -> a) -> a -> Vec n a
-- iterate = rwPrimVecIterate

-- {-# INLINE zip #-}
-- zip :: Vec n a -> Vec n b -> Vec n (a , b)
-- zip = rwPrimVecZip

-- {-# INLINE zipWith #-}
-- zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
-- zipWith f vs ws = map (uncurry f) (zip vs ws)

-- {-# INLINE zipWith3 #-}
-- zipWith3 :: (a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
-- zipWith3 f vs ws = zipWith (uncurry f) (zip vs ws)

-- | Returns evens from v concatenated with evens from w
{-# INLINE packlo #-}
packlo :: KnownNat n => Vec n a -> Vec n a -> Vec n a
packlo v w = generate (\ fi ->
      if fi FC.< n then index v (fi FC.* two)
                 else index w ((fi FC.- n) FC.* two))
  where
      one = F.finite 1
      two = F.finite 2
      n' = lastIndex v `FC.div` two
      n = if FC.even (lastIndex v) then n' else n' FC.+ one

-- | Returns odds from v concatenated with odds from w
{-# INLINE packhi #-}
packhi :: KnownNat n => Vec n a -> Vec n a -> Vec n a
packhi v w = generate (\ fi ->
      if fi FC.< n then index v ((fi FC.* two) FC.+ one)
                 else index w (((fi FC.- n) FC.* two) FC.+ one))
  where
      one = F.finite 1
      two = F.finite 2
      n' = lastIndex v `FC.div` two
      n = if FC.even (lastIndex v) then n' else n' FC.+ one

-- | Returns the first half of v interleaved with w
{-# INLINE unpacklo #-}
unpacklo :: KnownNat n => Vec n a -> Vec n a -> Vec n a
unpacklo v w = generate (\ fi ->
      if FC.even fi then index v (fi `FC.div` two)
                   else index w ((fi FC.- one) `FC.div` two))
  where
      one = F.finite 1
      two = F.finite 2

-- | Returns the second half of v interleaved with w
{-# INLINE unpackhi #-}
unpackhi :: KnownNat n => Vec n a -> Vec n a -> Vec n a
unpackhi v w = generate (\ fi ->
      if FC.even fi then index v (n FC.+ (fi `FC.div` two))
                   else index w (n FC.+ ((fi FC.- one) `FC.div` two)))
  where
      one = F.finite 1
      two = F.finite 2
      n' = lastIndex v `FC.div` two
      n = if FC.even (lastIndex v) then n' else n' FC.+ one

-- | lookup value at index n in vector
{-# INLINE (!) #-}
(!) :: KnownNat n => Vec ((n + m) + 1) a -> Proxy n -> a
(!) = index'

infixl 9 !

-- | assign new value a to index i
{-# INLINE (!=) #-}
(!=) :: KnownNat n => Vec n a -> Finite n -> a -> Vec n a
v != i = update v i

infixr 2  !=

{-# LANGUAGE DataKinds #-}
-- Instances with contexts: dfuns take dictionary arguments, eliminated
-- statically like every other dictionary. Covers a pair instance with two
-- class contexts, a polymorphic-width instance (KnownNat context), an
-- element-context Vec instance, and a contexted instance of a
-- single-method (newtype-dictionary) class.
import ReWire
import ReWire.Bits (lit, (^), (@.))
import ReWire.Vectors (map, (!))
import ReWire.Monad (iter, Dev)
import Prelude hiding ((^), map)

class Frob a where
      frob   :: a -> a
      unfrob :: a -> a

instance Frob Bool where
      frob     = Prelude.not
      unfrob b = b

instance KnownNat n => Frob (W n) where
      frob w   = w ^ lit 0x55
      unfrob w = w

instance (Frob a, Frob b) => Frob (a, b) where
      frob (a, b)   = (frob a, frob b)
      unfrob (a, b) = (unfrob a, unfrob b)

instance Frob a => Frob (Vec 4 a) where
      frob   = map frob
      unfrob = map unfrob

class Sole a where
      sole :: a -> a

instance Sole Bool where
      sole = Prelude.not

instance (Sole a, Sole b) => Sole (a, b) where
      sole (a, b) = (sole a, sole b)

twice :: Frob a => a -> a
twice x = frob (unfrob x)

-- Vec-of-pairs: a two-deep context chain (Vec 4 (a, b), then (Bool, Bool),
-- then Bool). N.B. the element cannot itself be Bool: W n is a synonym
-- for Vec n Bool, so an element-polymorphic Vec instance would overlap
-- the width-polymorphic W instance at Vec 4 Bool.
vbit :: W 8 -> Bool
vbit w = fst (twice (fromList [ (w @. 0, w @. 1), (w @. 2, w @. 3)
                              , (w @. 4, w @. 5), (w @. 6, w @. 7) ] :: Vec 4 (Bool, Bool)) ! (Proxy :: Proxy 0))

step :: W 8 -> W 8
step w = if fst (twice (w @. 0, vbit w)) || snd (sole (w @. 1, w @. 7)) then frob w else unfrob w

start :: Dev (W 8) (W 8)
start = iter step (lit 0)

main :: IO ()
main = undefined

{-# LANGUAGE DataKinds #-}
-- Cryptol FFI, binding forms: cry/cryho.cry exercises local function
-- bindings (with captures), top-level higher-order helpers applied to
-- named functions/lambdas/primitives, curry/uncurry, a function-valued
-- local chosen by a runtime condition (Bit at the boundary), and
-- type-indexed recursion (pow, unrolled through specializer clones).
import Prelude hiding ((==), (+))
import ReWire
import ReWire.Bits (lit, (==), (+))
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

locfn :: W 8 -> W 8 -> W 8
locfn = cryptol "cry/cryho.cry" "locfn" locfn

hof :: W 8 -> W 8
hof = cryptol "cry/cryho.cry" "hof" hof

cu :: W 8 -> W 8 -> W 8
cu = cryptol "cry/cryho.cry" "cu" cu

pick :: Bool -> W 8 -> W 8
pick = cryptol "cry/cryho.cry" "pick" pick

pow5 :: W 8 -> W 8
pow5 = cryptol "cry/cryho.cry" "pow5" pow5

go :: (W 8, W 8) -> W 8
go (a, b) = locfn a b + hof b + cu a b + pick (a == b) (pow5 a)

start :: Dev (W 8, W 8) (W 8)
start = iter go (lit 0, lit 0)

main :: IO ()
main = undefined

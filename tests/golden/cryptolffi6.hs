{-# LANGUAGE DataKinds #-}
-- Cryptol FFI, Z n (modular arithmetic): cry/cryzw.cry works in Z 251
-- and Z 257 internally (Ring operations reducing modulo n, fromInteger
-- reductions, fromZ/toInteger conversions) and returns words at the
-- boundary. zpoly is Horner evaluation of a fixed polynomial mod 257.
import Prelude hiding ((<>), (+))
import ReWire
import ReWire.Bits (lit, resize, (<>), (+))
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

zmix :: W 8 -> W 8 -> W 8
zmix = cryptol "cry/cryzw.cry" "zmix" zmix

zpoly :: W 16 -> W 16
zpoly = cryptol "cry/cryzw.cry" "zpoly" zpoly

go :: (W 8, W 8) -> W 8
go (a, b) = zmix a b + resize (zpoly (a <> b))

start :: Dev (W 8, W 8) (W 8)
start = iter go (lit 0, lit 0)

main :: IO ()
main = undefined

-- EXPECT-ERROR: is not representable in bitvector of size
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Finite (finite, fromFinite)
import ReWire.Monad (iter, Dev)
f :: W 8 -> W 2
f w = fromFinite (finite 3 :: Finite 100)
start :: Dev (W 8) (W 2)
start = iter f (lit 0)
main = undefined

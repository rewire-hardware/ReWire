-- EXPECT-ERROR: is not representable in Finite
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Finite (finite, fromFinite)
import ReWire.Monad (iter, Dev)
f :: W 8 -> W 8
f w = fromFinite (finite 5 :: Finite 4)
start :: Dev (W 8) (W 8)
start = iter f (lit 0)
main = undefined

-- EXPECT-ERROR: rwPrimFiniteMinBound: Finite 0 is uninhabited
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Finite (minBound, fromFinite)
import ReWire.Monad (iter, Dev)
z :: Finite 0
z = minBound
f :: W 8 -> W 8
f w = fromFinite z
start :: Dev (W 8) (W 8)
start = iter f (lit 0)
main = undefined

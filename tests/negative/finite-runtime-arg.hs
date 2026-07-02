-- EXPECT-ERROR: rwPrimFinite: can't determine argument value at compile-time
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit, msbit)
import ReWire.Finite (finite, fromFinite)
import ReWire.Monad (iter, Dev)
f :: W 8 -> W 8
f w = fromFinite (finite (if msbit w then 1 else 0) :: Finite 100)
start :: Dev (W 8) (W 8)
start = iter f (lit 0)
main = undefined

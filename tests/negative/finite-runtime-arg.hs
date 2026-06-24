-- EXPECT-ERROR: rwPrimFinite: can't determine argument value at compile-time
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Bits as B
import ReWire.Finite (finite, fromFinite)
import ReWire.Monad (iter, Dev)
f :: W 8 -> W 8
f w = fromFinite (finite (B.toInteger w) :: Finite 100)
start :: Dev (W 8) (W 8)
start = iter f (lit 0)
main = undefined

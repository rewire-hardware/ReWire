-- EXPECT-ERROR: Invalid bit slice
-- Slice bounds are (msb, lsb): here j < i, which is rejected.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit, (@@))
import ReWire.Monad (iter, Dev)

f :: W 8 -> W 4
f w = w @@ (0, 5)

start :: Dev (W 8) (W 4)
start = iter f (lit 0)

main = undefined

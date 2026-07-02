-- EXPECT-ERROR: rwPrimBitSlice must have arguments
-- Bit-slice indices must be literals after inlining; the index j here is a
-- runtime value rwc cannot reduce.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit, finBitSlice)
import ReWire.Finite (finite, toFinite')
import ReWire.Monad (iter, Dev)

f :: Finite 8 -> W 8 -> W 4
f j w = finBitSlice w j (finite 0)

g :: W 8 -> W 4
g w = f (toFinite' w) w

start :: Dev (W 8) (W 4)
start = iter g (lit 0)

main = undefined

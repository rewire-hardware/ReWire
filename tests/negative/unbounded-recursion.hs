-- EXPECT-ERROR: unsupported use of recursion
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^))
import ReWire
import ReWire.Bits
import ReWire.Monad (iter, Dev)

loop :: W 8 -> W 8
loop x = loop (x ^ lit 1)

start :: Dev (W 8) (W 8)
start = iter loop (lit 0)

main = undefined

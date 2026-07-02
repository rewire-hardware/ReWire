-- EXPECT-ERROR: is used with inconsistent signatures
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits (lit, (+))
import ReWire.Monad (iter, Dev)
extA :: W 8 -> W 8
extA = extern "myext" (\ x -> x)
extB :: W 8 -> W 8 -> W 8
extB = extern "myext" (\ x y -> x + y)
f :: W 8 -> W 8
f w = extA w + extB w w
start :: Dev (W 8) (W 8)
start = iter f (lit 0)
main = undefined

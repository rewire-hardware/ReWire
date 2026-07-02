-- EXPECT-ERROR: has conflicting models
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits (lit, (+))
import ReWire.Monad (iter, Dev)
modelA :: W 8 -> W 8
modelA x = x + lit 1
modelB :: W 8 -> W 8
modelB x = x + lit 2
extA :: W 8 -> W 8
extA = extern "myext" modelA
extB :: W 8 -> W 8
extB = extern "myext" modelB
f :: W 8 -> W 8
f w = extA w + extB w
start :: Dev (W 8) (W 8)
start = iter f (lit 0)
main = undefined

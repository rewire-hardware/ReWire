-- EXPECT-ERROR: Partial evaluation not terminating
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit, (+))
import ReWire.Monad (iter, Dev)
import Prelude hiding ((+))
apply :: (W 8 -> W 8) -> W 8 -> W 8
apply f x = f x
go :: (W 8 -> W 8) -> W 8 -> W 8
go f x = apply (go (apply f)) (f x)
start :: Dev (W 8) (W 8)
start = iter (go (\ y -> y + lit 1)) (lit 0)
main = undefined

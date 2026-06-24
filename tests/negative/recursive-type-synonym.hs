-- EXPECT-ERROR: Type synonym expansion not terminating
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)
type A = B
type B = A
start :: Dev (W 8) (W 8)
start = iter (\ x -> x) (lit 0)
main = undefined

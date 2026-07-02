-- EXPECT-ERROR: Cycle in type synonym declarations
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits
import ReWire.Monad (iter, Dev)
type T a = S a
type S a = T a
f :: T (W 8) -> W 8
f x = x
start :: Dev (W 8) (W 8)
start = iter f (lit 0)
main :: IO ()
main = undefined

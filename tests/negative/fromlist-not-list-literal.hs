-- EXPECT-ERROR: fromList: argument not a list literal
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits
import ReWire.Monad (iter, Dev)
f :: [Bool] -> W 2
f xs = fromList xs
g :: Bool -> W 2
g b = f [b, b]
start :: Dev Bool Bool
start = iter (\ x -> x) False
main = undefined

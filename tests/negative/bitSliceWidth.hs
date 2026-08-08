-- EXPECT-ERROR: does not match the declared result width
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits
import ReWire.Monad (Dev, iter)

-- Bits 6..2 make a 5-bit slice, but the declared result width is 3.
f :: W 8 -> W 3
f a = a @@ (6, 2)

start :: Dev (W 8) (W 3)
start = iter f (lit 0)

main :: IO ()
main = undefined

-- EXPECT-ERROR: Encountered call to built-in "error" function that was not eliminated
-- A call to ReWire.error on a live path cannot be compiled to hardware.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)

f :: W 8 -> W 8
f w = ReWire.error "boom"

start :: Dev (W 8) (W 8)
start = iter f (lit 0)

main = undefined

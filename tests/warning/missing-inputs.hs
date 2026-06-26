-- FLAGS: --interpret=missing-inputs.yaml --cycles 2
-- EXPECT-WARNING: Could not read inputs from missing-inputs.yaml
-- An explicitly named inputs file that can't be read warns (the inputs are
-- then all driven to zero); the default inputs.yaml is allowed to not exist.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit, (+))
import ReWire.Monad (iter, Dev)
import Prelude hiding ((+))

f :: W 8 -> W 8
f w = w + lit 1

start :: Dev (W 8) (W 8)
start = iter f (lit 0)

main = undefined

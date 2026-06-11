-- FLAGS: --core --testbench
-- EXPECT-WARNING: --testbench: no testbench generated
-- The --testbench flag only has an effect for the Verilog and VHDL targets.
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

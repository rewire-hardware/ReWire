-- EXPECT-WARNING: Ignoring the Haskell model for clocked extern 'mymod'
-- A clocked extern is stateful, so a pure per-cycle model can't be
-- cycle-accurate; the model is dropped with a warning.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)

compute :: W 16 -> W 8
compute = externWithSig
      []
      "clk"
      "rst"
      [("x", 16)]
      [("out", 8)]
      "mymod"
      computeModel
      ""

computeModel :: W 16 -> W 8
computeModel _ = lit 42

start :: Dev (W 16) (W 8)
start = iter compute (lit 0)

main = undefined

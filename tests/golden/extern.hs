{-# LANGUAGE DataKinds #-}
import ReWire ( signal, Identity, ReacT, W, externWithSig )
import ReWire.Monad (Dev)
import ReWire.Bits (lit, (@@), (@.), msbit)

start :: Dev (W 16) (W 8)
start = loop (lit 0)

loop :: W 16 -> ReacT (W 16) (W 8) Identity ()
loop i = return (compute i) >>= signal >>= loop

compute :: W 16 -> W 8
compute = externWithSig
      []           -- params
      "clk"
      "rst"
      [("x", 16)]  -- inputs
      [("out", 8)] -- outputs
      "mymod"
      compute
      ""

main = undefined

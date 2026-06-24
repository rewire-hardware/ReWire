-- EXPECT-ERROR: cannot be translated to a pure Cryptol function
-- FLAGS: --cryptol
{-# LANGUAGE DataKinds #-}
import ReWire ( signal, Identity, ReacT, W, externWithSig )
import ReWire.Monad (Dev)
import ReWire.Bits (lit)
start :: Dev (W 16) (W 8)
start = loop (lit 0)
loop :: W 16 -> ReacT (W 16) (W 8) Identity ()
loop i = return (compute i) >>= signal >>= loop
compute :: W 16 -> W 8
compute = externWithSig [] "clk" "rst" [("x", 16)] [("out", 8)] "mymod" compute ""
main = undefined

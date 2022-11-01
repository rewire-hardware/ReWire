import ReWire
import ReWire.Bits
import ReWire.Verilog

-- plus :: W8 -> W8 -> (W4, W4)
-- plus = externWithSig "plus" (["a", "b"], [("c1", 4), ("c2", 4)]) plus
plus :: W8 -> W8 -> (W4, W4)
plus = extern "plus" plus

detup :: (W4, W4) -> W8
detup = resize

begin :: ReT Bit W8 (StT W8 (StT W8 I)) ()
begin = lift (put zeroW8) >>= \zz ->
        lift (lift (put oneW8)) >>= \zz ->
        sig

sig :: ReT Bit W8 (StT W8 (StT W8 I)) ()
sig = do
      r0 <- lift get
      i <- signal r0
      case i of
            C -> sig
            S  -> incr

incr :: ReT Bit W8 (StT W8 (StT W8 I)) ()
incr = do
      r0 <- lift get
      r1 <- lift (lift get)
      lift (put r1)
      lift (lift (put (detup $ plus r0 r1)))
      sig

start :: ReT Bit W8 I ()
start = extrude (extrude begin zeroW8) oneW8

main = undefined

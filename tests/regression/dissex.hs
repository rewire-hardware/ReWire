import ReWire
import ReWire.Bits

sig :: ReT Bit W8 (StT W8 (StT W8 I)) ()
sig = do
      r0 <- lift get
      i <- signal r0
      case i of
            C -> sig
            S -> incr

incr :: ReT Bit W8 (StT W8 (StT W8 I)) ()
incr = do
      r0 <- lift get
      r1 <- lift (lift get)
      lift (put r1)
      lift (lift (put (plusW8 r0 r1)))
      sig

start :: ReT Bit W8 I ()
start = extrude (extrude sig zeroW8) oneW8

main = undefined

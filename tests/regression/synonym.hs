import ReWire
import ReWire.Bits

type W = W8

type SW = StT W

type S = SW I

type RT s = ReT Bit W s ()

begin :: RT (StT W8 (StT W8 I))
begin = lift (put zeroW8) >>= \zz ->
        lift (lift (put oneW8)) >>= \zz ->
        sig

sig :: RT (SW S)
sig = do
      r0 <- lift get
      i <- signal r0
      case i of
            C -> sig
            S  -> incr

incr :: RT (StT W8 S)
incr = do
      r0 <- lift get
      r1 <- lift (lift get)
      lift (put r1)
      lift (lift (put (plusW8 r0 r1)))
      sig

start :: RT I
start = extrude (extrude begin zeroW8) oneW8

main = undefined

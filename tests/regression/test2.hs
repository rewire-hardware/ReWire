import ReWire
import ReWire.Bits

incr :: ReT Bit W8 (StT W8 I) ()
incr = do
      r0 <- lift get
      lift (put r0)
      signal r0
      incr

start :: ReT Bit W8 I ()
start = extrude incr zeroW8

main = undefined

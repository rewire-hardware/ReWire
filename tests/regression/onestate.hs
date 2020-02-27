import ReWire
import ReWire.Bits

grunt :: StT W8 I ()
grunt = do
  x <- get
  put x

incr :: ReT Bit W8 (StT W8 I) ()
incr = do
  lift grunt
  x <- lift get
  signal x
  incr

start :: ReT Bit W8 I ()
start = extrude incr zeroW8

main = undefined

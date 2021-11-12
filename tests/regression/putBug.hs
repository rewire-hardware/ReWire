import ReWire
import ReWire.Bits

convtest :: Bit -> ReT Bit Bit (StT Bit I) ()
convtest b = do
      signal b
      lift (put b)
      convtest b

start :: ReT Bit Bit I ()
start = extrude (convtest C) C

main = undefined

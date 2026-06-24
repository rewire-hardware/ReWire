import ReWire
import ReWire.Bits

convtest :: Bit -> ReacT Bit Bit (StateT Bit Identity) ()
convtest b = do
      signal b
      lift (put b)
      convtest b

start :: ReacT Bit Bit Identity ()
start = extrude (convtest zero) zero

main = undefined

{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits

incr :: ReacT Bit (W 8) (StateT (W 8) Identity) ()
incr = do
      r0 <- lift get
      lift (put r0)
      signal r0
      incr

start :: ReacT Bit (W 8) Identity ()
start = extrude incr $ lit 0

main = undefined

{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits

{-# ANN grunt "this is ignored" #-}
grunt :: StateT (W 8) Identity ()
grunt = do
  x <- get
  put x

incr :: ReacT Bit (W 8) (StateT (W 8) Identity) ()
incr = do
  lift grunt
  x <- lift get
  signal x
  incr

start :: ReacT Bit (W 8) Identity ()
start = extrude incr $ lit 0

main = undefined

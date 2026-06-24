{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

begin :: ReacT Bit (W 8) (StateT (W 8) (StateT (W 8) Identity)) ()
begin = lift (put $ lit 0) >>= \zz ->
        lift (lift (put $ lit 1)) >>= \zz ->
        sig

sig :: ReacT Bit (W 8) (StateT (W 8) (StateT (W 8) Identity)) ()
sig = do
      r0 <- lift get
      i <- signal r0
      if i then sig else incr

incr :: ReacT Bit (W 8) (StateT (W 8) (StateT (W 8) Identity)) ()
incr = do
      r0 <- lift get
      r1 <- lift (lift get)
      lift (put r1)
      lift (lift (put (r0 + r1)))
      sig

fib :: ReacT Bit (W 8) Identity ()
fib = extrude (extrude begin $ lit 0) $ lit 1

fib' :: ReacT Bit (W 8) Identity ()
fib' = fib

start :: ReacT Bit (W 8) Identity ()
start = fib'

main = undefined

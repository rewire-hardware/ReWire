{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits (lit, (+))

zeroW8 :: W 8
zeroW8 = lit 0

oneW8 :: W 8
oneW8 = lit 1

plusW8 :: W 8 -> W 8 -> W 8
plusW8 = (+)

sig :: ReacT Bit (W 8) (StateT (W 8) (StateT (W 8) Identity)) ()
sig = do
      r0 <- lift get
      i <- signal r0
      if i then incr else sig

incr :: ReacT Bit (W 8) (StateT (W 8) (StateT (W 8) Identity)) ()
incr = do
      r0 <- lift get
      r1 <- lift (lift get)
      lift (put r1)
      lift (lift (put (plusW8 r0 r1)))
      sig

start :: ReacT Bit (W 8) Identity ()
start = extrude (extrude sig zeroW8) oneW8

main = undefined

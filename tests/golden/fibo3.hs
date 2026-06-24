{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+), head, last)
import ReWire
import ReWire.Bits
import ReWire.Vectors

type St = StateT (Vec 2 (W 8)) Identity

first :: St (W 8)
first = do
      v <- get
      pure $ head v

second :: St (W 8)
second = do
      v <- get
      pure $ last v

begin :: ReacT Bit (W 8) St ()
begin = do
      lift $ put $ fromList [lit 0, lit 1]
      sig

sig :: ReacT Bit (W 8) St ()
sig = do
      r0 <- lift first
      i <- signal r0
      if i then sig else incr

incr :: ReacT Bit (W 8) St ()
incr = do
      r0 <- lift first
      r1 <- lift second
      lift $ put $ fromList [r1, r0 + r1]
      sig

start :: ReacT Bit (W 8) Identity ()
start = extrude begin $ fromList [lit 0, lit 1]

main = undefined

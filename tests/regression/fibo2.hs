{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

start :: ReacT Bit (W 8) Identity ()
start = go

go :: ReacT Bit (W 8) Identity ()
go = loop (lit 0) $ lit 1

loop :: W 8 -> W 8 -> ReacT Bit (W 8) Identity ()
loop n m = do b <- signal n
              if b then loop n m else loop m (n + m)

main = undefined

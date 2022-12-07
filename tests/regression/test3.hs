{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits

tick :: ReacT Bit (W 8) (StateT (W 8) Identity) Bit
tick = lift get >>= \ x -> signal x

go :: ReacT Bit (W 8) (StateT (W 8) Identity) ()
go = do
      b <- tick
      if b then go else go

start :: ReacT Bit (W 8) Identity ()
start = extrude go $ lit 0

main = undefined

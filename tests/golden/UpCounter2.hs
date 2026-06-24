{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits (lit, resize, msbit, (+), (.|.), (<<.))
import ReWire.Vectors

{-# INLINE tick #-}
tick :: ReacT Bit (W 8) (StateT (W 8) Identity) Bit
tick = lift get >>= \ x -> signal x

msbitW8 :: W 8 -> W 8
msbitW8 n = resize $ singleton $ msbit n

incW8 :: W 8 -> W 8
incW8 n = n + lit 1

rolW8 :: W 8 -> W 8
rolW8 n = (n <<. (lit 1 :: W 8)) .|. msbitW8 n

zeroW8 :: W 8
zeroW8 = lit 0

go :: ReacT Bit (W 8) (StateT (W 8) Identity) ()
go = do
      b <- tick
      if b then lift get >>= \n -> lift (put (rolW8 n))
           else lift get >>= \n -> lift (put (incW8 n))
      go

start :: ReacT Bit (W 8) Identity ()
start = extrude go zeroW8

main = undefined

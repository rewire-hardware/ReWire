{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (W, lit)
import ReWire.Verilog ((+), (<<), (.|.), msbit)

tick :: ReT Bit (W 8) (StT (W 8) I) Bit
{-# INLINE tick #-}
tick = lift get >>= \ x -> signal x

msbitW8 :: W 8 -> W 8
msbitW8 n = resize $ singleton $ msbit n

incW8 :: W 8 -> W 8
incW8 n = n + lit 1

rolW8 :: W 8 -> W 8
rolW8 n = (n << lit 1) .|. msbitW8 n

zeroW8 :: W 8
zeroW8 = lit 0

go :: ReT Bit (W 8) (StT (W 8) I) ()
go = do
      b <- tick
      case b of
            S -> lift get >>= \n -> lift (put (incW8 n))
            C -> lift get >>= \n -> lift (put (rolW8 n))
      go

start :: ReT Bit (W 8) I ()
start = extrude go zeroW8

main = undefined

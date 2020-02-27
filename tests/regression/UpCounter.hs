import ReWire
import ReWire.Bits

tick :: ReT Bit W8 (StT W8 I) Bit
{-# INLINE tick #-}
tick = lift get >>= \ x -> signal x

go :: ReT Bit W8 (StT W8 I) ()
go = do
      b <- tick
      case b of
            S -> lift get >>= \n -> lift (put (incW8 n))
            C -> lift get >>= \n -> lift (put (rolW8 n))
      go

start :: ReT Bit W8 I ()
start = extrude go zeroW8

main = undefined

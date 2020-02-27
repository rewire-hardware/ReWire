import ReWire
import ReWire.Bits

tick :: ReT Bit W8 (StT W8 I) Bit
tick = lift get >>= \ x -> signal x

go :: ReT Bit W8 (StT W8 I) ()
go = do
      b <- tick
      case b of
            S -> go
            C -> go


start :: ReT Bit W8 I ()
start = extrude go zeroW8

main = undefined

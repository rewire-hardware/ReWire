import ReWire
import ReWire.Bits

start :: ReT Bit W8 I ()
start = go

go :: ReT Bit W8 I ()
go = loop zeroW8 oneW8

loop :: W8 -> W8 -> ReT Bit W8 I ()
loop n m = do b <- signal n
              case b of
                  S -> loop n m
                  C -> loop m (plusW8 n m)

main = undefined

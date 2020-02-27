import ReWire
import ReWire.Bits

go :: ReT Bit Bit (StT Bit (StT Bit I)) Bit
go = do
      x <- signal C
      y <- case x of { S  -> signal C ; C -> lift get }
      z <- signal (andb (case y of { S -> x ; C -> andb x x }) y)
      lift (lift (put y))
      go

start :: ReT Bit Bit I Bit
start = extrude (extrude go C) S

main = undefined

import ReWire
import ReWire.Bits

go :: ReT Bit Bit I Bit
go = do
      x <- signal C
      y <- case x of { S  -> signal C ; C -> signal x }
      z <- signal (andb (case y of { S -> x ; C -> andb x x }) y)
      go

start :: ReT Bit Bit I Bit
start = go

main = undefined

import ReWire.Lang.Bits

go :: ReT Bit Bit I Bit
go = do
      x <- signal Zero
      y <- case x of { One  -> signal Zero ; Zero -> signal x }
      z <- signal (bitAnd (case y of { One -> x ; Zero -> bitAnd x x }) y)
      go

start :: ReT Bit Bit I Bit
start = go

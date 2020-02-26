import ReWire.Lang.Bits

go :: ReT Bit Bit (StT Bit (StT Bit I)) Bit
go = do
      x <- signal Zero
      y <- case x of { One  -> signal Zero ; Zero -> lift get }
      z <- signal (bitAnd (case y of { One -> x ; Zero -> bitAnd x x }) y)
      lift (lift (put y))
      go

start :: ReT Bit Bit I Bit
start = extrude (extrude go Zero) One

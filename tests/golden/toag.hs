import ReWire

go :: ReacT Bit Bit Identity Bit
go = do
      x <- signal False
      y <- case x of { True  -> signal False ; False -> signal x }
      z <- signal ((case y of { True -> x ; False -> x && x }) && y)
      go

start :: ReacT Bit Bit Identity Bit
start = go

main = undefined

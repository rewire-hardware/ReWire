import ReWire

go :: ReacT Bit Bit (StateT Bit (StateT Bit Identity)) Bit
go = do
      x <- signal False
      y <- case x of { True  -> signal False ; False -> lift get }
      z <- signal ((case y of { True -> x ; False -> x && x }) && y)
      lift (lift (put y))
      go

start :: ReacT Bit Bit Identity Bit
start = extrude (extrude go False) True

main = undefined

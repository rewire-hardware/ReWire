{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits (lit, (+))

go :: ReacT () (W 8) (StateT (W 8) Identity) ()
go = do
      n <- lift get
      _ <- signal n
      lift (put (n + (lit 1 :: W 8)))
      go

start :: ReacT () (W 8) Identity ()
start = extrude go (lit 0)

main = undefined

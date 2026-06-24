-- Extrude applied to a parameterized device: the device expression reaching
-- purification is an application of a lifted definition, exercising the App
-- branch of purify's RExtrude case.
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

dev :: W 8 -> ReacT (W 8) (W 8) (StateT (W 8) Identity) ()
dev k = do
      s <- lift get
      i <- signal (s + k)
      lift (put i)
      dev k

top :: W 8 -> ReacT (W 8) (W 8) Identity ()
top n = extrude (dev n) (lit 0)

start :: ReacT (W 8) (W 8) Identity ()
start = top (lit 3)

main :: IO ()
main = undefined

{-# LANGUAGE DataKinds #-}
-- Three registers of distinct widths through a three-deep StateT tower:
-- exercises register naming and state layout in the device construct and
-- every back end's register handling.
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

loop :: W 4 -> ReacT (W 4) (W 16) (StateT (W 4) (StateT (W 8) (StateT (W 16) Identity))) ()
loop i = do
      a <- lift get
      b <- lift (lift get)
      c <- lift (lift (lift get))
      lift (put i)
      lift (lift (put (resize a + b)))
      lift (lift (lift (put (resize b + c))))
      i' <- signal (resize a + c)
      loop i'

start :: ReacT (W 4) (W 16) Identity ()
start = extrude (extrude (extrude (loop (lit 0)) (lit 1)) (lit 2)) (lit 3)

main :: IO ()
main = undefined

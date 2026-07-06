-- EXPECT-ERROR: never pauses (no machine to generate)
{-# LANGUAGE DataKinds #-}
import Prelude hiding (error)
import ReWire
import ReWire.Bits
grunt :: StateT (W 8) Identity ()
grunt = error "boom"
incr :: ReacT Bit (W 8) (StateT (W 8) Identity) ()
incr = do
      lift grunt
      x <- lift get
      signal x
      incr
start :: ReacT Bit (W 8) Identity ()
start = extrude incr (lit 0)
main = undefined

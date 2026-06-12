{-# LANGUAGE DataKinds #-}
-- Zero-width values end-to-end: a unit input (a width-0 input port), a
-- width-0 state register, and a W 0 operand in concatenation. Back ends
-- must erase all of these from interfaces and wires while preserving the
-- 8-bit counter behavior.
import Prelude hiding ((+), (++))
import ReWire
import ReWire.Bits
import ReWire.Monad (Dev)
import ReWire.Vectors (empty, (++))

loop :: ReacT () (W 8) (StateT (W 8) (StateT (W 0) Identity)) ()
loop = do
      z <- lift (lift get)
      x <- lift get
      lift (put (x + (z ++ lit 1)))
      lift (lift (put z))
      signal x
      loop

start :: Dev () (W 8)
start = extrude (extrude loop (lit 0)) empty

main :: IO ()
main = undefined

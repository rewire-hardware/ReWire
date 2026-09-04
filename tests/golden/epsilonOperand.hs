{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+), (==), map)
import ReWire
import ReWire.Bits
import ReWire.Vectors (map)

-- A forwarding reactive helper whose parameter is used under a
-- higher-order builtin's lambda (as a case scrutinee), called with a
-- computed argument from a block that ends in a goto: the block-graph
-- cleanup must not inline the forwarding block's goto through a
-- primitive expression into the atom-only slot.
mask :: Bool -> W 8 -> ReacT (W 8) (W 8) Identity ()
mask p v = out (map (\ x -> if p then zero else x) v)

out :: W 8 -> ReacT (W 8) (W 8) Identity ()
out v = do
      i <- signal v
      mask (i == lit 3) i

start :: ReacT (W 8) (W 8) Identity ()
start = out (lit 0)

main = undefined

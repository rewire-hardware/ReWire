import ReWire
import ReWire.Bits

baz :: Bit -> Bit -> Bit
baz b = if b then id else f
      where f = not
            c = one

start :: ReacT Bit Bit Identity ()
start = do
  signal $ not $ baz zero one
  start

main = undefined

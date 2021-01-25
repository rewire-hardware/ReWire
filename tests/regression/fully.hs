import ReWire
import ReWire.Bits

baz :: Bit -> Bit -> Bit
baz b = case b of
      C -> f
      S -> id
      where f S = C
            f C = S
            c   = S

start :: ReT Bit Bit I ()
start = do
  signal $ notb $ baz C S
  start

main = undefined

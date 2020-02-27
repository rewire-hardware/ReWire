import ReWire
import ReWire.Bits

foo :: (Bit, Bit) -> (Bit, Bit) -> (Bit, Bit)
foo a@(C, C)   b@(C, x)   = (\(C, C) c@(C, y@_) -> (x, y)) a b
foo a@(S, b@S) _          = (\a@(S, S) _ -> a) a a
foo _          a@(_, b@_) = (b, b)

baz :: Bit -> Bit
baz b = f c
      where f S = C
            f C = S
            c   = S

start :: ReT Bit Bit I ()
start = do
  signal $ notb $ baz C
  start

main = undefined

import ReWire
import ReWire.Bits

foo :: (Bit, Bit) -> (Bit, Bit) -> (Bit, Bit)
foo a@(False, False)  b@(False, x) = (\(False, False) c@(False, y@_) -> (x, y)) a b
foo a@(True, b@True) _             = (\a@(True, True) _ -> a) a a
foo _                a@(_, b@_)    = (b, b)

baz :: Bit -> Bit
baz b = f c
      where f True  = False
            f False = True
            c       = True

start :: ReacT Bit Bit Identity ()
start = do
  signal $ not $ baz False
  start

main = undefined

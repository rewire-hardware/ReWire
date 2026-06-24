import ReWire
import ReWire.Bits

type BitX = Bit

-- type BitY = BitX

foo :: (BitX, BitX) -> (BitX, BitX) -> (BitX, BitX)
foo a@(False, False)   b@(False, x)   = (\(False, False) c@(False, y@_) -> (x, y)) a b
foo a@(True, b@True) _          = (\a@(True, True) _ -> a) a a
foo _          a@(_, b@_) = (b, b)

baz :: BitX -> Bit
baz b = f c
      where f True = False
            f False = True
            c   = True

toBitX :: Bit -> BitX
toBitX = id

start :: ReacT BitX BitX Identity ()
start = do
  signal $ not $ baz $ toBitX False
  start

main = undefined


-- type T a b = Tx a b Int -> a -> b

-- COERCE:
--
-- forall a b. (T a b)                -> (Tx a b Int -> a -> b)
-- forall a b. (Tx a b Int -> a -> b) -> (T a b)
--

-- type A x = B x
--
-- unify t1 t2 = if   t1 unifies with t2 great.
--               else if (coerce t1) 


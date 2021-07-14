import ReWire
import ReWire.Bits

type BitX = Bit

-- type BitY = BitX

foo :: (BitX, BitX) -> (BitX, BitX) -> (BitX, BitX)
foo a@(C, C)   b@(C, x)   = (\(C, C) c@(C, y@_) -> (x, y)) a b
foo a@(S, b@S) _          = (\a@(S, S) _ -> a) a a
foo _          a@(_, b@_) = (b, b)

baz :: BitX -> Bit
baz b = f c
      where f S = C
            f C = S
            c   = S

toBitX :: Bit -> BitX
toBitX = id

start :: ReT BitX BitX I ()
start = do
  signal $ notb $ baz $ toBitX C
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


data Tuple2 a b = Tuple2 a b
data Bit = Zero | One

foo :: (Bit, Bit) -> (Bit, Bit) -> (Bit, Bit)
foo a@(Zero, Zero) b@(Zero, x) = (\(Zero, Zero) c@(Zero, y@_) -> (x, y)) a b
foo a@(One, b@One) _           = (\a@(One, One) _ -> a) a a
foo _ a@(_, b@_)               = (b, b)


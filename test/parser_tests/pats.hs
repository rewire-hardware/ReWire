data Bit = Zero | One

isEq :: Bit -> Bit -> Bool
isEq One  One  = True
isEq Zero Zero = True
isEq _    _    = False

foo :: (Bit, Bit) -> (Bit, Bit) -> (Bit, Bit)
foo a@(Zero, Zero) b@(Zero, x) = (\(Zero, Zero) c@(Zero, y@_) -> (x, y)) a b
foo a@(One, b@One) _           = (\a@(One, One) _ -> a) a a
foo _ a@(_, b@_)               = (b, b)

neg :: Bit -> Bit
neg b = if isEq b One then Zero else One

baz :: Bit -> Bit
baz b = f c
      where f One  = Zero
            f Zero = One
            c      = One



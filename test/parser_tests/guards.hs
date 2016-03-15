data Bit = Zero | One

eqb :: Bit -> Bit -> Bool
eqb One  One  = True
eqb Zero Zero = True
eqb _    _    = False

eqBool :: Bool -> Bool -> Bool
eqBool True  True  = True
eqBool False False = True
eqBool _     _     = False

eq :: (Bit, Bit, Bit) -> (Bit, Bit, Bit) -> Bool
(x, y, z) `eq` (x', y', z')
      | x `eqb` x' = case () of
            () | y `eqb` y' -> case () of
                  () | z `eqb` z' -> True
                     | True       -> False
               | True       -> False
      | True       = False

-- FunBind guard.
odd :: (Bit, Bit, Bit) -> Bool
odd bits
      | bits `eq` (Zero, Zero, One)  = True
      | bits `eq` (Zero, One, One)   = True
      | bits `eq` (One, Zero, One)   = True
      | bits `eq` (One, One, One)    = True
      | True                         = False

-- Alt guard.
odd' :: (Bit, Bit, Bit) -> Bool
odd' x = case x of
      (_, _, z)
            | z `eqb` One  -> True
            | z `eqb` Zero -> False
            | True      -> False
      _ -> False

-- PatBind guard (??).
nonsense :: Bool
nonsense
      | odd a `eqBool` odd' b = False
      | odd b `eqBool` odd' b = True
      | True                  = False
      where a :: (Bit, Bit, Bit)
            a = (One, One, Zero)

            b :: (Bit, Bit, Bit)
            b = (One, Zero, One)


import ReWire
import ReWire.Bits

eqBool :: Bool -> Bool -> Bool
eqBool True  True  = True
eqBool False False = True
eqBool _     _     = False

eq' :: (Bit, Bit, Bit) -> (Bit, Bit, Bit) -> Bool
(x, y, z) `eq'` (x', y', z')
      | x `eqb` x' = case () of
            () | y `eqb` y' -> case () of
                  () | z `eqb` z' -> True
                     | True         -> False
               | True               -> False
      | True                         = False

-- FunBind guard.
odd' :: (Bit, Bit, Bit) -> Bool
odd' bits
      | bits `eq'` (C, C, S) = True
      | bits `eq'` (C, S, S) = True
      | bits `eq'` (S, C, S) = True
      | bits `eq'` (S, S, S) = True
      | True                 = False

-- Alt guard.
odd'' :: (Bit, Bit, Bit) -> Bool
odd'' x = case x of
      (_, _, z)
            | z `eqb` S -> True
            | z `eqb` C -> False
            | True      -> False
      _ -> False

-- PatBind guard (??).
nonsense :: Bool
nonsense
      | odd' a `eqBool` odd'' b = False
      | odd' b `eqBool` odd'' b = True
      | True                    = False
      where a :: (Bit, Bit, Bit)
            a = (S, S, C)

            b :: (Bit, Bit, Bit)
            b = (S, C, S)

start :: ReT Bool Bool I ()
start = do
  signal nonsense
  start

main = undefined

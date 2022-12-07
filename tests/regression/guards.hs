import ReWire
import ReWire.Bits

eqBool :: Bool -> Bool -> Bool
eqBool True  True  = True
eqBool False False = True
eqBool _     _     = False

eq' :: (Bit, Bit, Bit) -> (Bit, Bit, Bit) -> Bool
(x, y, z) `eq'` (x', y', z')
      | x `eqBool` x' = case () of
            () | y `eqBool` y' -> case () of
                  () | z `eqBool` z' -> True
                     | True    -> False
               | True          -> False
      | True                    = False

-- FunBind guard.
odd' :: (Bit, Bit, Bit) -> Bool
odd' bs
      | bs `eq'` (zero, zero, one) = True
      | bs `eq'` (zero, one , one) = True
      | bs `eq'` (one , zero, one) = True
      | bs `eq'` (one , one , one) = True
      | True               = False

-- Alt guard.
odd'' :: (Bit, Bit, Bit) -> Bool
odd'' x = case x of
      (_, _, z)
            | z     -> True
            | not z -> False
            | True  -> False
      _ -> False

-- PatBind guard (??).
nonsense :: Bool
nonsense
      | odd' a `eqBool` odd'' b = False
      | odd' b `eqBool` odd'' b = True
      | True                    = False
      where a :: (Bit, Bit, Bit)
            a = (one, one, zero)

            b :: (Bit, Bit, Bit)
            b = (one, zero, one)

start :: ReacT Bool Bool Identity ()
start = do
  signal nonsense
  start

main = undefined

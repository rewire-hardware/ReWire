-- EXPECT-ERROR: unsupported use of a type class method: Semigroup.<>
-- EXPECT-ERROR: external classes cannot be compiled
-- A hand-written local instance of an external class does not help: the
-- class itself is external, so its evidence is erased and the method has
-- no bridge (symmetric with the derived-instance case).
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)

data T = A | B

instance Semigroup T where
      A <> x = x
      B <> _ = B

step :: Bool -> Bool
step b = case (if b then A else B) <> A of
      A -> True
      B -> False

start :: Dev Bool Bool
start = iter step False

main :: IO ()
main = undefined

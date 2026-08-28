-- EXPECT-ERROR: unsupported use of a type class method: Eq.==
-- EXPECT-ERROR: external classes cannot be compiled
-- Methods of external (non-home) classes have no bridge: their evidence
-- is erased. Deriving the instance does not help.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)

data T = A | B deriving Eq

step :: Bool -> Bool
step b = (if b then A else B) == A

start :: Dev Bool Bool
start = iter step False

main :: IO ()
main = undefined

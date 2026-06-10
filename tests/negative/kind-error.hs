-- EXPECT-ERROR: Kinds do not unify
-- Vec's first argument is a Nat, not a type.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)

f :: Vec Bool Bool -> Bool
f x = True

start :: Dev Bool Bool
start = iter f False

main = undefined

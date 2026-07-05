-- EXPECT-ERROR: Main.Frob Bool is expected
-- EXPECT-ERROR: Main.Frob Bool
-- Multi-method (data-dictionary) class instances compile, but a
-- single-method class has a newtype dictionary whose class type survives
-- to the Crust typechecker, so this instance is still rejected.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)

class Frob a where
      frob :: a -> a

instance Frob Bool where
      frob = Prelude.not

start :: Dev Bool Bool
start = iter frob False

main = undefined

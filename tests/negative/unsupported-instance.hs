-- EXPECT-ERROR: unsupported definition syntax
-- Type class instances are not part of the supported Haskell subset.
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

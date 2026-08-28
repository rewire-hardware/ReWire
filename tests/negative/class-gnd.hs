-- EXPECT-ERROR: has type Bool -> Bool but Main.N -> Main.N is expected
-- GeneralizedNewtypeDeriving coerces each method of the underlying
-- instance across the newtype ($cfrob = frob $fFrobBool |> co); the
-- coercion is not erasable (the two types differ representationally for
-- the bridge), so the derived method fails the post-bridge lint with a
-- type mismatch located at the deriving clause. This test pins that
-- failure shape; GND is documented as unsupported (doc/classes.md).
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import ReWire
import ReWire.Monad (iter, Dev)

class Frob a where
      frob :: a -> a
      unfrob :: a -> a

instance Frob Bool where
      frob = Prelude.not
      unfrob b = b

newtype N = N Bool deriving Frob

step :: Bool -> Bool
step b = case frob (N b) of
      N x -> x

start :: Dev Bool Bool
start = iter step False

main :: IO ()
main = undefined

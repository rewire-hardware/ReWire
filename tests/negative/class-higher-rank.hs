-- EXPECT-ERROR: unsupported higher-rank type
-- A method with its own type variable makes the dictionary field a forall
-- type; for a single-method class the newtype dictionary unwraps to that
-- polytype, and higher-rank types are out of the ReWire fragment. (A
-- multi-method class with such a method is rejected identically.)
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)

class Pad a where
      pad :: a -> b -> a

instance Pad Bool where
      pad x _ = x

step :: Bool -> Bool
step x = pad x ()

start :: Dev Bool Bool
start = iter step False

main :: IO ()
main = undefined

-- EXPECT-ERROR: unsupported recursive class dictionary: Main.C
-- A Constraint-kinded type parameter lets a single-method class mention
-- itself in its own method type; unwrapping its newtype dictionary would
-- regress forever, so the bridge's expansion bound rejects it.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)
import Data.Kind (Constraint)

data T (c :: Constraint) = MkT

class C a where
      m :: T (C a) -> a -> a

instance C Bool where
      m MkT = Prelude.not

start :: Dev Bool Bool
start = iter (m MkT) False

main :: IO ()
main = undefined

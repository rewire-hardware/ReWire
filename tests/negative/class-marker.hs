-- EXPECT-ERROR: not in the ReWire vocabulary: GHC.Prim.TYPE
-- EXPECT-ERROR: explicit kind annotation
-- A zero-method class with no superclass and no kind annotation gets a
-- kind-generalized dictionary constructor (C:Marked @(*) @Bool), and the
-- kind argument is out of the ReWire vocabulary. The kind-annotated form
-- (class Marked (a :: Type)) compiles.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)

class Marked a

instance Marked Bool

f :: Marked a => a -> a
f x = x

start :: Dev Bool Bool
start = iter f False

main :: IO ()
main = undefined

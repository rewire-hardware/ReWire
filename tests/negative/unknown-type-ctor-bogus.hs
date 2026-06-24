-- EXPECT-ERROR: Unknown type constructor or not fully-applied type synonym
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)
f :: Bogus -> Bool
f x = True
start :: Dev Bool Bool
start = iter f undefined
main = undefined

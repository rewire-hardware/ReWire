-- EXPECT-ERROR: Not in scope: type constructor or class ‘Bogus’
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)
f :: Bogus -> Bool
f x = True
start :: Dev Bool Bool
start = iter f undefined
main = undefined

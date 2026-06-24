-- EXPECT-ERROR: Unsupported Ctor syntax
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
import ReWire
import ReWire.Monad (iter, Dev)
data E = forall a. MkE a
start :: Dev Bool Bool
start = iter (\ x -> x) False
main = undefined

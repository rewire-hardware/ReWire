-- EXPECT-ERROR: unsupported type syntax
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
import ReWire
import ReWire.Monad (iter, Dev)
f :: _ -> Bool
f x = x
start :: Dev Bool Bool
start = iter f False
main = undefined

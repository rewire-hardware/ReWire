{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (Dev, iter)

start :: Dev Bool Bool
start = iter f False

f :: Bool -> Bool
f = not

main = undefined
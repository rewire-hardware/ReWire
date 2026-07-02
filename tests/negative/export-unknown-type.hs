-- EXPECT-ERROR: Not in scope: type constructor or class ‘NoSuchType’
module Main (NoSuchType, start, main) where
import ReWire
import ReWire.Monad (iter, Dev)
start :: Dev Bool Bool
start = iter Prelude.not False
main = undefined

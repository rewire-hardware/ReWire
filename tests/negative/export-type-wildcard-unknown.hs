-- EXPECT-ERROR: Unknown class or type name in export list
module Main (NoSuchType(..), start, main) where
import ReWire
import ReWire.Monad (iter, Dev)
start :: Dev Bool Bool
start = iter Prelude.not False
main = undefined

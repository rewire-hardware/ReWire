-- EXPECT-ERROR: Unknown variable name in export list
module Main (start, doesNotExist, main) where
import ReWire
import ReWire.Monad (iter, Dev)
start :: Dev Bool Bool
start = iter Prelude.not False
main = undefined

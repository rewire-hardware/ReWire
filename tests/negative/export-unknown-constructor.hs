-- EXPECT-ERROR: Unknown class or type name in export list
module Main (Foo(Bar, Nonexistent), start, main) where
import ReWire
import ReWire.Monad (iter, Dev)
data Foo = Bar | Baz
start :: Dev Bool Bool
start = iter Prelude.not False
main = undefined

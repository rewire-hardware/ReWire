-- EXPECT-ERROR: Variable not in scope: mystery
import ReWire
import ReWire.Monad (iter, Dev)

start :: Dev Bool Bool
start = iter mystery False

main = undefined

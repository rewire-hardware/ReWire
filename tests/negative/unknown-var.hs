-- EXPECT-ERROR: Unknown variable: mystery
import ReWire
import ReWire.Monad (iter, Dev)

start :: Dev Bool Bool
start = iter mystery False

main = undefined

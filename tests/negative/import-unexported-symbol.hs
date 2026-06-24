-- EXPECT-ERROR: Attempting to import an unexported symbol
import ReWire
import ReWire.Monad (iter, Dev, signal)
start :: Dev Bool Bool
start = iter id False
main = undefined

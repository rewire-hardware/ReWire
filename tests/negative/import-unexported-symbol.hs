-- EXPECT-ERROR: Module ‘ReWire.Monad’ does not export ‘signal’
import ReWire
import ReWire.Monad (iter, Dev, signal)
start :: Dev Bool Bool
start = iter id False
main = undefined

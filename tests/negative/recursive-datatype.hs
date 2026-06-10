-- EXPECT-ERROR: recursive datatype
-- A recursive datatype has no fixed bit width.
import ReWire
import ReWire.Monad (iter, Dev)

data L = N | C Bool L

grow :: L -> L
grow l = C True l

start :: Dev L L
start = iter grow N

main = undefined

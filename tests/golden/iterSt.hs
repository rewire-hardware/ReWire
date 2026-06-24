import ReWire
import ReWire.Monad (Dev, StateDev, iterSt)
import ReWire.Bits (xor)

start :: Dev Bool Bool
start = extrude loop False

loop :: StateDev Bool Bool (StateT Bool Identity)
loop = iterSt f False

f :: Bool -> Bool -> (Bool,Bool)
f i s = (s `xor` i,s)

main = undefined
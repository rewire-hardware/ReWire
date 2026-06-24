import ReWire ( signal, Identity, ReacT, StateT, lift, get, put)
import ReWire.Monad (Dev, extrudeDev)
import ReWire.Bits (xor)

start :: Dev Bool Bool
start = extrudeDev (loop True) True

loop :: Bool -> ReacT Bool Bool (StateT Bool Identity) ()
loop i = lift (stateAction i) >>= signal >>= loop

stateAction :: Bool -> StateT Bool Identity Bool
stateAction i = get >>= \ s -> put (i `xor` s) >> return s

main = undefined
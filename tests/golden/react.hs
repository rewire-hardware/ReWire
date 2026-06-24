import ReWire ( signal, Identity, ReacT )
import ReWire.Monad (Dev)

start :: Dev Bool Bool
start = device True

device :: Bool -> Dev Bool Bool
device = loop

loop :: Bool -> ReacT Bool Bool Identity ()
loop i = return (not i) >>= signal >>= loop

main = undefined
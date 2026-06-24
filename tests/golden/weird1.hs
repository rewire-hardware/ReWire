import ReWire
import ReWire.Bits

filt :: ReacT Bit Bit (StateT (Bit, Bit) Identity) Bit
filt = repl zero

repl :: Bit -> ReacT Bit Bit (StateT (Bit, Bit) Identity) Bit
repl _ = signal zero >>= repl

start :: ReacT Bit Bit Identity Bit
start = extrude filt (zero, zero)

main = undefined

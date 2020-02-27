import ReWire
import ReWire.Bits

filt :: ReT Bit Bit (StT (Bit, Bit) I) Bit
filt = repl C

repl :: Bit -> ReT Bit Bit (StT (Bit, Bit) I) Bit
repl _ = signal C >>= repl

start :: ReT Bit Bit I Bit
start = extrude filt (C, C)

main = undefined

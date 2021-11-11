import ReWire
import ReWire.Bits

type B = Bit

filt :: ReT B B (StT (B, B) I) B
filt = repl C

repl :: B -> ReT B B (StT (B, B) I) B
repl _ = signal C >>= repl

start :: ReT B B I B
start = extrude filt (C, C)

main = undefined

data Bit = Zero | One

filt :: ReT Bit Bit (StT (Bit, Bit) I) Bit
filt = repl Zero

repl :: Bit -> ReT Bit Bit (StT (Bit, Bit) I) Bit
repl _ = signal Zero >>= repl

start :: ReT Bit Bit I Bit
start = extrude filt (Zero, Zero)

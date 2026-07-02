-- EXPECT-ERROR: Not in scope: ‘f1’
import ReWire
import ReWire.Bits
data S = S Bit
mk :: S
mk = S {f1 = zero}
foo :: S -> Bit
foo (S a) = a
start :: ReacT Bit Bit Identity ()
start = do { signal (foo mk); start }
main = undefined

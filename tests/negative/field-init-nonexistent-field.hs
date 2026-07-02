-- EXPECT-ERROR: Not in scope: ‘nope’
import ReWire
import ReWire.Bits
data S = S { f1 :: Bit }
mk :: S
mk = S {nope = zero}
foo :: S -> Bit
foo (S a) = a
start :: ReacT Bit Bit Identity ()
start = do { signal (foo mk); start }
main = undefined

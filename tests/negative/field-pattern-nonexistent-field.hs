-- EXPECT-ERROR: Not in scope: ‘nope’
import ReWire
import ReWire.Bits
data S = S { f1 :: Bit }
foo :: S -> Bit
foo (S {nope = a}) = a
start :: ReacT Bit Bit Identity ()
start = do { signal (foo (S zero)); start }
main = undefined

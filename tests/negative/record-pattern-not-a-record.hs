-- EXPECT-ERROR: Record pattern found, but not a record
import ReWire
import ReWire.Bits
data S = S Bit Bit
foo :: S -> Bit
foo (S {f1 = a}) = a
start :: ReacT Bit Bit Identity ()
start = do { signal (foo (S zero one)); start }
main = undefined

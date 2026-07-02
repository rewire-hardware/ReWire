-- EXPECT-ERROR: Not in scope: record field ‘nope’
import ReWire
import ReWire.Bits
data S = S { f1 :: Bit }
upd :: S -> S
upd r = r {nope = zero}
foo :: S -> Bit
foo (S a) = a
start :: ReacT Bit Bit Identity ()
start = do { signal (foo (upd (S zero))); start }
main = undefined

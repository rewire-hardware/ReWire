-- EXPECT-ERROR: in a record update
{-# LANGUAGE RecordWildCards #-}
import ReWire
import ReWire.Bits
data S = S { f1 :: Bit }
upd :: S -> Bit -> S
upd r f1 = r {..}
foo :: S -> Bit
foo (S a) = a
start :: ReacT Bit Bit Identity ()
start = do { signal (foo (upd (S zero) one)); start }
main = undefined

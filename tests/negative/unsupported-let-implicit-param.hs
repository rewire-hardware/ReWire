-- EXPECT-ERROR: unsupported let syntax
{-# LANGUAGE ImplicitParams #-}
import ReWire
import ReWire.Bits
foo :: Bit
foo = let ?x = zero in bar
bar :: (?x :: Bit) => Bit
bar = ?x
start :: ReacT Bit Bit Identity ()
start = do { signal foo; start }
main = undefined

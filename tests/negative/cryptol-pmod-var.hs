-- Carry-less polynomial division/remainder is realized only for a
-- constant divisor polynomial (the quotient/remainder bits become fixed
-- XOR networks); a runtime divisor is rejected with a located error.
-- EXPECT-ERROR: polynomial division by a non-constant divisor
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

pmodvar :: W 8 -> W 9 -> W 8
pmodvar = cryptol "cry/cryneg.cry" "pmodvar" pmodvar

go :: (W 8, W 9) -> W 8
go (a, b) = pmodvar a b

start :: Dev (W 8, W 9) (W 8)
start = iter go (lit 0, lit 0)

main :: IO ()
main = undefined

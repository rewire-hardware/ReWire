-- Enums are interior-only: the entry point's type must be words,
-- vectors, and tuples (the ReWire side renders W 2 as [2], which cannot
-- unify with the Cryptol-side enum Dir), so the Cryptol typechecker
-- rejects the instantiation.
-- EXPECT-ERROR: Type mismatch
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

turn :: W 2 -> W 2
turn = cryptol "cry/cryneg.cry" "turn" turn

start :: Dev (W 2) (W 2)
start = iter turn (lit 0)

main :: IO ()
main = undefined

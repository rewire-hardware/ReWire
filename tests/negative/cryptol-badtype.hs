-- The Cryptol function is [8] -> [8]; using it at W 4 -> W 4 must be
-- rejected by the Cryptol typechecker.
-- EXPECT-ERROR: Type mismatch
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

f :: W 4 -> W 4
f = cryptol "cry/cryneg.cry" "f" f

start :: Dev (W 4) (W 4)
start = iter f (lit 0)

main :: IO ()
main = undefined

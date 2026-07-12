-- An infinite stream is realizable only when demand is statically
-- bounded (a finite take or a constant index). A variable index has
-- unbounded demand and is rejected with a located error.
-- EXPECT-ERROR: unbounded demand
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

varidx :: W 8 -> W 8
varidx = cryptol "cry/cryneg.cry" "varidx" varidx

start :: Dev (W 8) (W 8)
start = iter varidx (lit 0)

main :: IO ()
main = undefined

-- EXPECT-ERROR: Value not in scope: nosuch
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

f :: W 8 -> W 8
f = cryptol "cry/cryneg.cry" "nosuch" f

start :: Dev (W 8) (W 8)
start = iter f (lit 0)

main :: IO ()
main = undefined

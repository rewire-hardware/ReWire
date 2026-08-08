{-# LANGUAGE DataKinds #-}
-- Reductions and logical operations over zero-width operands: rAnd, rOr,
-- lnot, and (&&.) at W 0 must fold to the doc/hyle.md section 5.2 n = 0
-- identities (rAnd = True, the rest reduce through rOr = False) on every
-- backend -- including the Verilog printer, which cannot print a
-- zero-width operand.
import Prelude hiding ((+))
import ReWire
import ReWire.Bits
import ReWire.Monad (Dev, iter)

f :: W 8 -> W 8
f x = let z = resize x :: W 0 in
      if rAnd z && not (rOr z) && lnot z && not (z &&. z)
            then x + lit 1
            else lit 0

start :: Dev (W 8) (W 8)
start = iter f (lit 0)

main :: IO ()
main = undefined

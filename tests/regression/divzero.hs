{-# LANGUAGE DataKinds #-}
-- Division and modulus with a frequently-zero divisor: the pseudorandom
-- cosimulation stimulus must exercise the SMT-LIB division-by-zero
-- conventions (x / 0 = all-ones, x % 0 = x) at runtime on every target.
-- The divisions by the literal zero exercise the same conventions in the
-- partial evaluator at compile time.
import Prelude hiding ((/), (^), (++))
import ReWire
import ReWire.Bits
import ReWire.Monad (Dev, iter)
import ReWire.Vectors ((++))

f :: W 8 -> W 16
f a = (a / b) ++ ((a % b) ^ (lit 7 / lit 0))
      where b :: W 8
            b = (a .&. lit 3) % lit 0

start :: Dev (W 8) (W 16)
start = iter f (lit 0)

main :: IO ()
main = undefined

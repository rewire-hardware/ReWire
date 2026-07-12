{-# LANGUAGE DataKinds #-}
-- Cryptol FFI, bounded-demand infinite streams: cry/crystream.cry uses
-- infFrom/infFromThen, iterate (which desugars to scanl over an infinite
-- unit stream), repeat, a recursive infinite stream (an LFSR), and a
-- finite zip against an infinite stream -- all consumed by a finite
-- take or a constant index, so rwcry realizes only the demanded prefix.
import Prelude hiding ((+))
import ReWire
import ReWire.Bits (lit, resize, (+))
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

-- join (take{5} (infFrom x))
tk :: W 8 -> W 40
tk = cryptol "cry/crystream.cry" "tkw" tk

-- (iterate (\y -> y*3+1) x) @ 4
idx :: W 8 -> W 8
idx = cryptol "cry/crystream.cry" "idx" idx

-- join (take{3} (drop{2} (infFromThen x (x+2))))
dt :: W 8 -> W 24
dt = cryptol "cry/crystream.cry" "dtw" dt

-- join (take{6} of a recursive LFSR stream)
lfsr :: W 8 -> W 48
lfsr = cryptol "cry/crystream.cry" "lfsrw" lfsr

-- join [ a + b | a <- xs | b <- infFrom 0 ] (finite zip against a stream)
fz :: W 32 -> W 32
fz = cryptol "cry/crystream.cry" "fzw" fz

-- join (take{4} (repeat x))
rep :: W 8 -> W 32
rep = cryptol "cry/crystream.cry" "repw" rep

go :: (W 8, W 8) -> W 8
go (a, b) = resize (tk a) + idx a + resize (dt b) + resize (lfsr b)
      + resize (fz (resize a)) + resize (rep b)



start :: Dev (W 8, W 8) (W 8)
start = iter go (lit 0, lit 0)

main :: IO ()
main = undefined

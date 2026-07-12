{-# LANGUAGE DataKinds #-}
-- Cryptol FFI, sequences and streams: cry/cryseq.cry exercises
-- recursive comprehensions (a SHA-style message schedule, an AES-style
-- key expansion indexing the sequence under construction with Integer
-- arithmetic, CBC chaining), rotates, element shifts, update/updateEnd,
-- scanl, signed division, lg2, carry-less polynomial arithmetic
-- (pmult/pdiv/pmod), transpose, enumeration variants, and a 0-ary
-- entry point.
import Prelude hiding ((+), (^))
import ReWire
import ReWire.Bits (lit, (+), (^))
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

schedw :: W 8 -> W 8 -> W 8
schedw = cryptol "cry/cryseq.cry" "schedw" schedw

keyw :: W 8 -> W 8 -> W 8
keyw = cryptol "cry/cryseq.cry" "keyw" keyw

cbcw :: W 8 -> W 8 -> W 8
cbcw = cryptol "cry/cryseq.cry" "cbcw" cbcw

updw :: W 8 -> W 8 -> W 8
updw = cryptol "cry/cryseq.cry" "updw" updw

scansw :: W 8 -> W 8 -> W 8
scansw = cryptol "cry/cryseq.cry" "scansw" scansw

sdivs :: W 8 -> W 8 -> W 8
sdivs = cryptol "cry/cryseq.cry" "sdivs" sdivs

crc :: W 8 -> W 8
crc = cryptol "cry/cryseq.cry" "crc" crc

tspw :: W 8 -> W 8
tspw = cryptol "cry/cryseq.cry" "tspw" tspw

ranges :: W 8
ranges = cryptol "cry/cryseq.cry" "ranges" ranges

go :: (W 8, W 8) -> W 8
go (a, b) = schedw a b + keyw a b + cbcw a b + updw a b + scansw a b
      + sdivs a b + crc a ^ tspw b + ranges

start :: Dev (W 8, W 8) (W 8)
start = iter go (lit 0, lit 0)

main :: IO ()
main = undefined

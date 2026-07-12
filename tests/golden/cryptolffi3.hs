{-# LANGUAGE DataKinds #-}
-- Cryptol FFI, data shapes: cry/cryrec.cry exercises records
-- (construction, selection, update), a newtype, an enum with case
-- expressions (nested, with a default arm), tuple patterns, and
-- numeric constraint guards.
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

alu :: W 2 -> W 8 -> W 8 -> W 8
alu = cryptol "cry/cryrec.cry" "alu" alu

recmix :: W 8 -> W 8 -> W 8
recmix = cryptol "cry/cryrec.cry" "recmix" recmix

both :: W 8 -> W 8 -> W 8
both = cryptol "cry/cryrec.cry" "both" both

cguard :: W 8 -> W 8
cguard = cryptol "cry/cryrec.cry" "cguard" cguard

go :: (W 2, W 8, W 8) -> W 8
go (s, a, b) = alu s (recmix a b) (both (cguard a) b)

start :: Dev (W 2, W 8, W 8) (W 8)
start = iter go (lit 0, lit 0, lit 0)

main :: IO ()
main = undefined

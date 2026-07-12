{-# LANGUAGE DataKinds #-}
-- Cryptol FFI milestone: AES-128 block encryption (FIPS-197) from
-- cry/aes128.cry, realized entirely in hardware. The key schedule and
-- the round sequence are recursive comprehensions; SubBytes is an S-box
-- table lookup (a variable index compiled to shift-and-slice); MixColumns
-- multiplies in GF(2^8) via carry-less pmult/pmod. The device encrypts
-- one (key, plaintext) block per cycle; the stimulus drives known
-- FIPS-197 vectors and all backends agree with the reference ciphertext.
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

encrypt :: W 128 -> W 128 -> W 128
encrypt = cryptol "cry/aes128.cry" "encrypt" encrypt

go :: (W 128, W 128) -> W 128
go (key, pt) = encrypt key pt

start :: Dev (W 128, W 128) (W 128)
start = iter go (lit 0, lit 0)

main :: IO ()
main = undefined

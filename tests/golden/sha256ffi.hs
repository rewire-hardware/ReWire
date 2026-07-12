{-# LANGUAGE DataKinds #-}
-- Cryptol FFI milestone: a faithful SHA-256 (FIPS 180-4) compression of
-- a single padded 512-bit block, from cry/sha256.cry, realized entirely
-- in hardware. The message schedule and the 64-round state sequence are
-- recursive comprehensions; the round functions use rotates and shifts.
-- The device hashes one block per cycle; the stimulus drives known test
-- vectors (see sha256ffi.input.yaml) and the interpreter/HDL/Cryptol
-- legs all agree with the digests a reference implementation computes.
-- (tests/golden/sha256.hs is the same algorithm written natively in
-- ReWire; this is the Cryptol-sourced counterpart.)
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

sha256Block :: W 512 -> W 256
sha256Block = cryptol "cry/sha256.cry" "sha256Block" sha256Block

start :: Dev (W 512) (W 256)
start = iter sha256Block (lit 0)

main :: IO ()
main = undefined

{-# LANGUAGE DataKinds #-}
-- Cryptol FFI, Field at Z p: cry/cryfield.cry inverts and divides in
-- Z 251 (a prime field) using recip and (/.), which rwcry realizes as a
-- Fermat inverse (a^(p-2) mod p, square-and-multiply unrolled). Both are
-- interior-only and return words at the boundary.
import Prelude hiding ((+))
import ReWire
import ReWire.Bits (lit, (+))
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

inv :: W 8 -> W 8
inv = cryptol "cry/cryfield.cry" "inv" inv

divz :: W 8 -> W 8 -> W 8
divz = cryptol "cry/cryfield.cry" "divz" divz

go :: (W 8, W 8) -> W 8
go (a, b) = inv a + divz a b

start :: Dev (W 8, W 8) (W 8)
start = iter go (lit 0, lit 0)

main :: IO ()
main = undefined

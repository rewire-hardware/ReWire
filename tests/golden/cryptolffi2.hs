{-# LANGUAGE DataKinds #-}
-- Cryptol FFI, the wider fragment: comprehensions (unrolled), variable
-- indexing (shift-and-slice), sequence regrouping (split/join), folds
-- with a function argument, and one polymorphic Cryptol function
-- (flipHalves) instantiated at two different widths.
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

crysub :: W 16 -> W 16
crysub = cryptol "cry/crysbox.cry" "bytemap" crysub

cryflip16 :: W 16 -> W 16
cryflip16 = cryptol "cry/crysbox.cry" "flipHalves" cryflip16

cryflip4 :: W 4 -> W 4
cryflip4 = cryptol "cry/crysbox.cry" "flipHalves" cryflip4

cryxor :: W 16 -> W 4
cryxor = cryptol "cry/crysbox.cry" "xorWord" cryxor

go :: W 16 -> (W 16, W 4)
go x = (cryflip16 (crysub x), cryflip4 (cryxor x))

start :: Dev (W 16) (W 16, W 4)
start = iter go (lit 0)

main :: IO ()
main = undefined

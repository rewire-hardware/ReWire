{-# LANGUAGE DataKinds #-}
-- Cryptol FFI, module system: cry/crymods.cry defines a submodule and a
-- parameterized (functor) submodule with an instantiation, referenced
-- through a qualified name. The Cryptol module system resolves all of
-- this before translation and the specializer flattens it, so no extra
-- machinery is needed on the rwcry side.
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

mix :: W 8 -> W 8
mix = cryptol "cry/crymods.cry" "mix" mix

start :: Dev (W 8) (W 8)
start = iter mix (lit 0)

main :: IO ()
main = undefined

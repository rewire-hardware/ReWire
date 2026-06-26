-- EXPECT-WARNING: Encountered a live call to the built-in "error" function
-- A call to ReWire.error on a live path compiles to a zero (don't-care)
-- value, with a warning (fatal under -Werror).
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)

f :: W 8 -> W 8
f w = ReWire.error "boom"

start :: Dev (W 8) (W 8)
start = iter f (lit 0)

main = undefined

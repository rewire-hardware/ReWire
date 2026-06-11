-- EXPECT-WARNING: ignoring the Haskell model for extern someext
-- A non-self-referential extern implementation that can't be compiled as an
-- interpreter model (here: recursive) is neutered with a warning.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)

badModel :: W 8 -> W 8
badModel x = badModel x

myext :: W 8 -> W 8
myext = extern "someext" badModel

start :: Dev (W 8) (W 8)
start = iter myext (lit 0)

main = undefined

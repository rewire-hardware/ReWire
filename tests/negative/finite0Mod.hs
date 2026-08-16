{-# LANGUAGE DataKinds #-}
-- EXPECT-ERROR: rwPrimToFiniteMod: Finite 0 is uninhabited
-- Finite 0 has no values (matching Data.Finite), so the modular
-- conversion into it cannot be compiled: the GHC model errors (mod 0)
-- and no total hardware realization can agree with it.
import Prelude hiding (Word)
import ReWire
import ReWire.Bits
import ReWire.Finite (toFinite', fromFinite)

degen :: W 8 -> W 8
degen x = fromFinite (toFinite' x :: Finite 0)

dev :: W 8 -> ReacT (W 8) (W 8) Identity ()
dev a = do
      a' <- signal (degen a)
      dev a'

start :: ReacT (W 8) (W 8) Identity ()
start = dev (lit 0)

main :: IO ()
main = undefined

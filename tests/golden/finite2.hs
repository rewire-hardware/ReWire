{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire ( signal, Identity, ReacT, W, Finite)
import ReWire.Monad (Dev)
import ReWire.Bits (lit)
import ReWire.Finite (finite,toFinite,toFinite',fromFinite)
import ReWire.FiniteComp ((+))

start :: Dev (Finite 128) (Finite 20)
start = loop (finite 77)

loop :: Finite 128 -> ReacT (Finite 128) (Finite 20) Identity ()
loop i = return (compute i) >>= signal >>= loop

-- | Finite
-- finite : Integer -> Finite n
-- toFinite : W m -> Finite n [errors if n < 2 ^ m]
-- toFinite' : W m -> Finite n [uses mod instead of error]
-- fromFinite : Finite n -> W m
compute :: Finite 128 -> Finite 20
compute n = toFinite' (fromFinite n :: W 7) + toFinite (lit 6 :: W 3)

main = undefined
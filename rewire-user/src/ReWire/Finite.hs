{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver -fplugin GHC.TypeLits.Extra.Solver #-}
module ReWire.Finite where

import ReWire

-- | Convert an Integer into a @'Finite' n@, throws an error if >= @n@.
{-# INLINE finite #-}
finite :: KnownNat n => Integer -> Finite n
finite = rwPrimFinite

-- | Converts argument bitvector to Finite, raising error if unrepresentable.
{-# INLINE toFinite #-}
toFinite :: KnownNat n => W m -> Finite n
toFinite = rwPrimToFinite

{-# INLINE minBound #-}
minBound :: KnownNat n => Finite n
minBound = rwPrimFiniteMinBound

{-# INLINE maxBound #-}
maxBound :: KnownNat n => Finite n
maxBound = rwPrimFiniteMaxBound

-- | Converts argument bitvector to Finite n, reducing modulo n if necessary
--   (without raising error).
{-# INLINE toFinite' #-}
toFinite' :: KnownNat n => W m -> Finite n
toFinite' = rwPrimToFiniteMod

{-# INLINE fromFinite #-}
fromFinite :: KnownNat m => Finite n -> W m
fromFinite = rwPrimFromFinite

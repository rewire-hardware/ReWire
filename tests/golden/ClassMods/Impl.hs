{-# LANGUAGE DataKinds #-}
-- Orphan instances: the class lives in ClassMods.Gate, the instance type
-- (W 8) in rewire-user.
module ClassMods.Impl where

import ReWire
import ReWire.Bits (lit, (^), (.|.))
import Prelude hiding ((^))

import ClassMods.Gate

instance Gate (W 8) where
      inv w = w ^ lit 0xff
      par = (.|.)

instance Flip (W 8) where
      flick w = w ^ lit 0x0f

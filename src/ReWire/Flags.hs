{-# LANGUAGE Safe #-}
module ReWire.Flags where

data Flag = FlagO String
          | FlagV
          | FlagDHask1
          | FlagDHask2
          | FlagDCrust1
          | FlagDCrust2
          | FlagDCrust3
          | FlagDCrust4
          | FlagDCore
          | FlagDTypes
          | FlagLoadPath String
      deriving (Eq, Show)

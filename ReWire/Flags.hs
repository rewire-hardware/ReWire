{-# LANGUAGE Safe #-}
module ReWire.Flags where

data Flag = FlagO String
          | FlagD
          | FlagV
          | FlagDCrust1
          | FlagDCrust2
          | FlagDCrust3
          | FlagDCore
          | FlagDTypes
          | FlagLoadPath String
      deriving (Eq, Show)

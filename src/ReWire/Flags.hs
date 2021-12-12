{-# LANGUAGE Safe #-}
module ReWire.Flags where

data Flag = FlagO !String
          | FlagV
          | FlagFirrtl
          | FlagDHask1  | FlagDHask2
          | FlagDCrust1 | FlagDCrust2 | FlagDCrust3 | FlagDCrust4 | FlagDCrust5
          | FlagDCore1  | FlagDCore2
          | FlagDTypes
          | FlagInvertReset
          | FlagPkgs !String
          | FlagInputNames !String | FlagOutputNames !String
          | FlagLoadPath !String
      deriving (Eq, Show)

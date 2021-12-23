{-# LANGUAGE Safe #-}
module ReWire.Flags where

data Flag = FlagO !String
          | FlagV
          | FlagFirrtl  | FlagVerilog
          | FlagDHask1  | FlagDHask2
          | FlagDCrust1 | FlagDCrust2 | FlagDCrust3 | FlagDCrust4 | FlagDCrust5
          | FlagDCore1  | FlagDCore2
          | FlagDTypes
          | FlagFlatten
          | FlagInvertReset
          | FlagPkgs !String
          | FlagInputNames !String | FlagOutputNames !String | FlagStateNames !String
          | FlagLoadPath !String
          | FlagInterpret !(Maybe String) | FlagCycles !String
      deriving (Eq, Show)

{-# LANGUAGE Safe #-}
module ReWire.Flags where

data Flag = FlagO !String
          | FlagV       | FlagH
          | FlagFirrtl  | FlagVerilog | FlagVhdl
          | FlagDPass1  | FlagDPass2 | FlagDPass3
          | FlagDPass4 | FlagDPass5 | FlagDPass6 | FlagDPass7 | FlagDPass8 | FlagDPass9
          | FlagDPass10  | FlagDPass11
          | FlagFlatten
          | FlagInvertReset
          | FlagNoReset | FlagNoClock
          | FlagSyncReset
          | FlagPkgs !String
          | FlagClockName !String
          | FlagResetName !String
          | FlagInputNames !String | FlagOutputNames !String | FlagStateNames !String
          | FlagLoadPath !String
          | FlagTop !String
          | FlagInterpret !(Maybe String) | FlagCycles !String
      deriving (Eq, Show)

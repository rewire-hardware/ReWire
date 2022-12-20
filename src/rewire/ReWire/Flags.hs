{-# LANGUAGE Safe #-}
module ReWire.Flags where

data Flag = FlagO !String
          | FlagVerbose | FlagHelp
          | FlagFirrtl  | FlagVerilog | FlagVhdl
          | FlagDump !String
          | FlagFlatten
          | FlagInvertReset
          | FlagNoReset | FlagNoClock
          | FlagSyncReset
          | FlagVhdlPkgs !String
          | FlagClockName !String
          | FlagResetName !String
          | FlagInputNames !String | FlagOutputNames !String | FlagStateNames !String
          | FlagLoadPath !String
          | FlagTop !String
          | FlagInterpret !(Maybe String) | FlagCycles !String
          | FlagEvalDepth !String
          | FlagPretty
      deriving (Eq, Show)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
module ReWire.Flags where

import Data.Hashable (Hashable)
import GHC.Generics (Generic (..))

data Flag = FlagO !String
          | FlagVerbose | FlagHelp
          | FlagNoWarn | FlagW !String
          | FlagVerilog | FlagVhdl | FlagCryptol | FlagCore | FlagFromCore
          | FlagDump !String | FlagDumpAll
          | FlagFlatten
          | FlagInvertReset
          | FlagNoReset | FlagNoClock
          | FlagSyncReset
          | FlagVhdlPkgs !String
          | FlagClockName !String
          | FlagResetName !String
          | FlagInputNames !String | FlagOutputNames !String | FlagStateNames !String
          | FlagLoadPath !String
          | FlagStart !String
          | FlagTop !String
          | FlagInterpret !(Maybe String) | FlagCycles !String
          | FlagTestbench !(Maybe String)
          | FlagEvalDepth !String
          | FlagPretty
          | FlagDebugLint
          | FlagEidos
          | FlagNoHalt
          | FlagRtlOpt !String
          | FlagStableNames
          | FlagLocators
          | FlagNoLocators
      deriving (Eq, Show, Generic)

instance Hashable Flag

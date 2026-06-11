{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
module ReWire.Flags where

import Data.Hashable (Hashable)
import GHC.Generics (Generic (..))

data Flag = FlagO !String
          | FlagVerbose | FlagHelp
          | FlagNoWarn | FlagW !String
          | FlagVerilog | FlagVhdl | FlagCryptol | FlagCore | FlagFromCore
          | FlagMantle
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
          | FlagStart !String
          | FlagTop !String
          | FlagInterpret !(Maybe String) | FlagCycles !String
          | FlagTestbench !(Maybe String)
          | FlagEvalDepth !String
          | FlagPretty
          | FlagDebugTypeCheck
          | FlagRtlOpt !String
      deriving (Eq, Show, Generic)

instance Hashable Flag

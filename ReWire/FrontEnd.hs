{-# LANGUAGE Safe #-}
module ReWire.FrontEnd
      ( loadProgram
      , LoadPath
      , getSystemLoadPath
      ) where

import ReWire.Core.Syntax
import ReWire.Error
import ReWire.Crust.Cache
import ReWire.Flags (Flag (..))
import Paths_ReWire

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: [Flag] -> LoadPath -> FilePath -> SyntaxErrorT IO Program
loadProgram flags lp fp = runCache (getProgram flags fp) lp

getSystemLoadPath :: IO [FilePath]
getSystemLoadPath = ("." :) . pure <$> getDataFileName "lib"

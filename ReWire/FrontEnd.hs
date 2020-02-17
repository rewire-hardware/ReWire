{-# LANGUAGE Safe #-}
module ReWire.FrontEnd
      ( loadProgram
      , LoadPath
      , getSystemLoadPath
      ) where

import ReWire.Core.Syntax
import ReWire.Error
import ReWire.Crust.Cache
import Paths_ReWire

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: LoadPath -> FilePath -> IO (Either AstError Program)
loadProgram lp fp = runCache (getProgram fp) lp

getSystemLoadPath :: IO [FilePath]
getSystemLoadPath = ("." :) . pure <$> getDataFileName "lib"

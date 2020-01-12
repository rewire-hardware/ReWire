{-# LANGUAGE Safe #-}
module ReWire.FrontEnd
      ( loadProgram
      , LoadPath
      ) where

import ReWire.Core.Syntax
import ReWire.Error
import ReWire.FrontEnd.Cache

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: LoadPath -> FilePath -> IO (Either AstError Program)
loadProgram lp fp = runCache (getProgram fp) lp

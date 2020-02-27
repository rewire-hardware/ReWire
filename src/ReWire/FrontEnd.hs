{-# LANGUAGE Safe #-}
module ReWire.FrontEnd
      ( loadProgram
      , LoadPath
      ) where

import ReWire.Core.Syntax (Program)
import ReWire.Error (SyntaxErrorT)
import ReWire.Crust.Cache (runCache, getProgram, LoadPath)
import ReWire.Flags (Flag (..))

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: [Flag] -> LoadPath -> FilePath -> SyntaxErrorT IO Program
loadProgram flags lp fp = runCache (getProgram flags fp) lp

module ReWire.FrontEnd
      ( loadProgram
      , SrcLoc(..)
      , LoadPath
      ) where

import ReWire.Core.Syntax
import ReWire.Error
import ReWire.FrontEnd.Cache

import Language.Haskell.Exts (SrcLoc(..))

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: LoadPath -> FilePath -> IO (Either Error RWCProgram)
loadProgram lp fp = runCache (getProgram fp) lp


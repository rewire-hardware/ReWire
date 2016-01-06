module ReWire.FrontEnd
      ( loadProgram
      , ParseResult(..)
      , SrcLoc(..)
      , prettyPrint
      , LoadPath
      ) where

import ReWire.Core.Syntax (RWCProgram)
import ReWire.FrontEnd.Cache (runCache, getProgram, LoadPath)

import Language.Haskell.Exts (ParseResult(..), SrcLoc(..), prettyPrint)

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: LoadPath -> FilePath -> IO (ParseResult RWCProgram)
loadProgram lp fp = runCache (getProgram fp) lp


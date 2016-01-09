module ReWire.FrontEnd
      ( loadProgram
      , ParseResult(..)
      , SrcLoc(..)
      , prettyPrint
      , LoadPath
      ) where

import ReWire.Core.Syntax
import ReWire.FrontEnd.Cache

import Language.Haskell.Exts (ParseResult(..), SrcLoc(..), prettyPrint)

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: LoadPath -> FilePath -> IO (ParseResult RWCProgram)
loadProgram lp fp = runCache (getProgram fp) lp


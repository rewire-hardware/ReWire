module ReWire.FrontEnd
      ( loadProgram
      , ParseResult(..)
      , SrcLoc(..)
      , prettyPrint
      ) where

import ReWire.Core.Syntax (RWCProgram)
import ReWire.FrontEnd.Cache (runCache, getModule)

import Language.Haskell.Exts (ParseResult(..), SrcLoc(..), prettyPrint)

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: FilePath -> IO (ParseResult RWCProgram)
loadProgram = runCache . getModule


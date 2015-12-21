module ReWire.FrontEnd
      ( loadModule
      , ParseResult(..)
      , SrcLoc(..)
      , prettyPrint
      ) where

import ReWire.Core.Syntax (RWCModule)
import ReWire.FrontEnd.Cache (runCache, getModule)

import Language.Haskell.Exts (ParseResult(..), SrcLoc(..), prettyPrint)

-- | Opens and parses a file and, recursively, its imports.
loadModule :: FilePath -> IO (ParseResult RWCModule)
loadModule = runCache . getModule


{-# LANGUAGE Safe #-}
module ReWire.FrontEnd
      ( loadProgram
      , LoadPath
      ) where

import ReWire.Config (Config)
import ReWire.Core.Syntax (Program)
import ReWire.Error (SyntaxErrorT, AstError)
import ReWire.ModCache (runCache, getProgram, LoadPath)

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: Config -> LoadPath -> FilePath -> SyntaxErrorT AstError IO Program
loadProgram conf lp fp = runCache (getProgram conf fp) lp

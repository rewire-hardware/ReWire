module ReWire.FrontEnd
      ( loadProgram
      , LoadPath
      ) where

import ReWire.Core.Syntax
import ReWire.Error
import ReWire.FrontEnd.Cache

import Data.Monoid ((<>))
import Data.Functor ((<$>))

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: LoadPath -> FilePath -> IO (Either AstError RWCProgram)
loadProgram lp fp = runCache (getProgram fp) lp

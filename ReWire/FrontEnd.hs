module ReWire.FrontEnd
      ( loadProgram
      , LoadPath
      ) where

import ReWire.Core.Syntax
import ReWire.Error
import ReWire.FrontEnd.Cache
import ReWire.FrontEnd.PrimBasis

import Data.Monoid ((<>))
import Data.Functor ((<$>))

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: LoadPath -> FilePath -> IO (Either AstError RWCProgram)
loadProgram lp fp = runCache ((<> primBasis) . fst <$> getProgram fp) lp

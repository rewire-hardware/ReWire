{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.FrontEnd
      ( loadProgram
      , LoadPath
      ) where

import ReWire.Config (Config)
import ReWire.Core.Syntax (Program)
import ReWire.Error (MonadError, AstError)
import ReWire.ModCache (runCache, getProgram, LoadPath)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState)

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: (MonadFail m, MonadError AstError m, MonadState AstError m, MonadIO m) => Config -> LoadPath -> FilePath -> m Program
loadProgram conf lp fp = runCache (getProgram conf fp) lp

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
module Embedder.FrontEnd
      ( embedFile
      , LoadPath
      ) where

import Embedder.Config (Config, getEmbedFile, getAtmoFile, verbose)
import ReWire.Error (MonadError, AstError, runSyntaxError, failAt)
import Embedder.ModCache (runCache, LoadPath, getModule)
import ReWire.Pretty (Pretty, prettyPrint, fastPrint)
import qualified Embedder.Config         as Config

import Control.Monad (when)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Data.Text.IO        as T
import System.Exit (exitFailure)
import System.IO (stderr)

embedFile :: MonadIO m => Config -> FilePath -> m ()
embedFile  conf filename = do
      when (conf^.verbose) $ liftIO $ T.putStrLn $ "Embedding: " <> pack filename
      runSyntaxError (embedModule conf filename)
            >>= either (liftIO . (>> exitFailure) . T.hPutStrLn stderr . prettyPrint) pure

-- | Opens and parses a file and, recursively, its imports.
embedModule :: (MonadFail m, MonadError AstError m, MonadState AstError m, MonadIO m) => Config -> FilePath -> m ()
embedModule conf fp = runCache $ getModule conf "." fp >>= \ (m,_) -> writeOutput m
      where
            writeOutput :: (MonadError AstError m, MonadIO m, Pretty a) => a -> m ()
            writeOutput a = do
                  let fout = getAtmoFile conf fp
                  liftIO $ T.writeFile fout $ if | conf^.Config.pretty -> prettyPrint a
                                                 | otherwise           -> fastPrint a

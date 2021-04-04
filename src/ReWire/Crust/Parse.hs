{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Crust.Parse (tryParseInDir) where

import ReWire.Error

import qualified Data.Text as Txt
import Language.Haskell.Exts (parseFileWithMode, ParseResult (..), defaultParseMode, ParseMode (..))
import safe Control.Monad.IO.Class (liftIO, MonadIO)
import safe Language.Haskell.Exts.SrcLoc (SrcSpanInfo, SrcLoc (..))
import safe Language.Haskell.Exts.Syntax (Module (..))
import safe System.Directory (getCurrentDirectory, setCurrentDirectory, doesFileExist, doesDirectoryExist)

tryParseInDir :: (MonadIO m, MonadError AstError m) => FilePath -> FilePath -> m (Maybe (Module SrcSpanInfo))
tryParseInDir fp dp = do
      dExists <- liftIO $ doesDirectoryExist dp
      if not dExists then pure Nothing else do
            oldCwd <- liftIO getCurrentDirectory
            liftIO $ setCurrentDirectory dp
            exists <- liftIO $ doesFileExist fp
            result <- if not exists then pure Nothing else do
                  pr <- liftIO parse
                  Just <$> pr2Cache pr
            liftIO $ setCurrentDirectory oldCwd
            pure result

      where pr2Cache :: MonadError AstError m => ParseResult a -> m a
            pr2Cache = \ case
                  ParseOk p                       -> pure p
                  ParseFailed (SrcLoc "" r c) msg -> failAt (SrcLoc fp r c) (Txt.pack msg)
                  ParseFailed l msg               -> failAt l (Txt.pack msg)

            parse :: IO (ParseResult (Module SrcSpanInfo))
            parse = parseFileWithMode defaultParseMode { parseFilename = fp, fixities = Nothing } fp

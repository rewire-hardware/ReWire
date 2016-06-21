{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.FrontEnd.Parse (tryParseInDir) where

import ReWire.Error

import Language.Haskell.Exts.Annotated (parseFileWithMode, ParseResult (..), defaultParseMode, ParseMode (..))
import safe Control.Monad.IO.Class (liftIO, MonadIO)
import safe Language.Haskell.Exts.SrcLoc (SrcSpanInfo, SrcLoc (..))
import safe Language.Haskell.Exts.Annotated.Syntax (Module (..))
import safe System.Directory (getCurrentDirectory, setCurrentDirectory, doesFileExist, doesDirectoryExist)

tryParseInDir :: (MonadIO m, SyntaxError m) => FilePath -> FilePath -> m (Maybe (Module SrcSpanInfo))
tryParseInDir fp dp = do
      dExists <- liftIO $ doesDirectoryExist dp
      if not dExists then return Nothing else do
            oldCwd <- liftIO getCurrentDirectory
            liftIO $ setCurrentDirectory dp
            exists <- liftIO $ doesFileExist fp
            result <- if not exists then return Nothing else do
                  pr <- liftIO parse
                  Just <$> pr2Cache pr
            liftIO $ setCurrentDirectory oldCwd
            return result

      where pr2Cache :: SyntaxError m => ParseResult a -> m a
            pr2Cache = \ case
                  ParseOk p                       -> return p
                  ParseFailed (SrcLoc "" r c) msg -> failAt (SrcLoc fp r c) msg
                  ParseFailed l msg               -> failAt l msg

            parse :: IO (ParseResult (Module SrcSpanInfo))
            parse = parseFileWithMode defaultParseMode { parseFilename = fp, fixities = Nothing } fp

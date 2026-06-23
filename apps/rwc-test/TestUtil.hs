module TestUtil
      ( withHandleTo, withStderrTo, withStdoutTo
      , sq
      ) where

import Control.Exception (finally)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (hClose, hFlush, openFile, stderr, stdout, Handle, IOMode (WriteMode))

-- | Run an action with a handle (stdout/stderr) redirected to a file,
--   restoring the original handle afterward (even on exception).
withHandleTo :: Handle -> FilePath -> IO a -> IO a
withHandleTo hdl f io = do
      saved <- hDuplicate hdl
      h     <- openFile f WriteMode
      hDuplicateTo h hdl
      io `finally` do
            hFlush hdl
            hDuplicateTo saved hdl
            hClose h
            hClose saved

withStderrTo :: FilePath -> IO a -> IO a
withStderrTo = withHandleTo stderr

withStdoutTo :: FilePath -> IO a -> IO a
withStdoutTo = withHandleTo stdout

-- | Strip one layer of surrounding single or double quotes, if present.
sq :: String -> String
sq = \ case
      '"'  : s | last s == '"'  -> init s
      '\'' : s | last s == '\'' -> init s
      s                         -> s

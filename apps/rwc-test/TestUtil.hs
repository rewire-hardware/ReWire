module TestUtil
      ( withHandleTo, withStderrTo, withStdoutTo
      , sq
      , externArgs
      ) where

import Control.Exception (finally)
import Data.List (isSuffixOf)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
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

-- | The simulator argument naming a test's hand-written extern implementations
--   of the given extension (e.g. ".sv" in a "verilog" subdirectory next to the
--   test). Returns the "<dir>/*.<ext>" glob only when the directory exists and
--   holds at least one matching file, otherwise "" -- a test with no externs
--   has no such directory, and handing a non-matching glob to iverilog,
--   verilator, or ghdl makes them fail with "no such file".
externArgs :: FilePath -> String -> IO String
externArgs dir ext = do
      hasFiles <- doesDirectoryExist dir >>= \ case
            False -> pure False
            True  -> any (ext `isSuffixOf`) <$> listDirectory dir
      pure $ if hasFiles then dir </> ("*" <> ext) else ""

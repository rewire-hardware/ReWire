{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | CLI scaffolding shared by the rwc and rwe drivers.
module Driver (driverMain) where

import ReWire.Config (Config, loadPath, verbose)
import ReWire.Flags (Flag)
import qualified ReWire.Config as Config

import Control.Lens ((^.), over)
import Control.Monad (when)
import Data.List (intercalate)
import Data.Text (Text, pack)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr, ArgOrder (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)
import qualified Data.Text.IO as T

import Paths_rewire (getDataFileName)

-- | Shared @main@ for the rwc and rwe executables: parse the command line
--   into a Config, extend the loadpath with the bundled rewire-user sources,
--   then run the per-file action.
driverMain :: String -> [OptDescr Flag] -> (Config -> FilePath -> IO ()) -> IO ()
driverMain prog options act = do
      (flags, filenames, errs) <- getOpt Permute options <$> getArgs

      when (not $ null errs) $ exitUsage' (map pack errs)
      when (null filenames)  $ exitUsage' ["No input files"]

      conf     <- either (exitUsage' . pure) pure $ Config.interpret flags
      when (conf^.verbose) $ do
            putStrLn $ "Debug: Flags: " <> show flags
            putStrLn $ "Debug: Source files: " <> show filenames

      systemLP <- getSystemLoadPath
      let conf' = over loadPath (<> (systemLP <> ["."])) conf

      when (conf'^.verbose) $ putStrLn $ "Debug: loadpath: " <> intercalate "," (conf'^.loadPath)

      mapM_ (act conf') filenames

      where exitUsage :: IO a
            exitUsage = T.hPutStr stderr (pack $ usageInfo ("\nUsage: " <> prog <> " [OPTION...] <filename.hs>") options) >> exitFailure

            -- | Print errors, usage, then exit.
            exitUsage' :: [Text] -> IO a
            exitUsage' errs = do
                  mapM_ (T.hPutStr stderr . ("Error: " <>)) $ filter (/= "") errs
                  exitUsage

getSystemLoadPath :: IO [FilePath]
getSystemLoadPath = pure <$> getDataFileName ("rewire-user" </> "src")

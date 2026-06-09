{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module RWE (main) where

import Embedder.Config (loadPath, verbose)
import ReWire.Flags (Flag (..))
import Embedder.FrontEnd (embedFile)
import qualified Embedder.Config as Config

import Control.Lens ((^.), over)
import Control.Monad (when)
import Data.List (intercalate)
import Data.Text (Text, pack)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)
import qualified Data.Text.IO as T

import Paths_rewire (getDataFileName)

options :: [OptDescr Flag]
options =
       [ Option ['h'] ["help"]          (NoArg  FlagHelp)                          "This help message."
       , Option ['v'] ["verbose"]       (NoArg  FlagVerbose)                       "More verbose output."
       , Option ['d'] ["dump"]          (ReqArg FlagDump        "1,2,...")         "Dump the intermediate form of the corresponding pass number (1-13; see -v output)."
       , Option []    ["flatten"]       (NoArg  FlagFlatten)                       "Flatten RTL output into a single module (currently slow, memory-intensive)."
       , Option ['o'] []                (ReqArg FlagO           "filename.vhdl")   "Name for output file."
       , Option []    ["start"]         (ReqArg FlagStart       "name")            "Symbol to use for the definition of the top-level module (default: Main.start)."
       , Option []    ["top"]           (ReqArg FlagTop         "name")            "Name to use for the top-level module in generated RTL (default: top_level)."
       , Option []    ["loadpath"]      (ReqArg FlagLoadPath    "dir1,dir2,...")   "Additional directories for loadpath."
       , Option []    ["pretty"]        (NoArg  FlagPretty)                        "Attempt to output prettier RTL at the expense of performance."
       ]

exitUsage :: IO a
exitUsage = T.hPutStr stderr (pack $ usageInfo "\nUsage: rwe [OPTION...] <filename.hs>" options) >> exitFailure

-- | Print errors, usage, then exit.
exitUsage' :: [Text] -> IO a
exitUsage' errs = do
      mapM_ (T.hPutStr stderr . ("Error: " <>)) $ filter (/= "") errs
      exitUsage

getSystemLoadPath :: IO [FilePath]
getSystemLoadPath = pure <$> getDataFileName ("rewire-user" </> "src")

main :: IO ()
main = do
      (flags, filenames, errs) <-  getOpt Permute options <$> getArgs

      when (not $ null errs) $ exitUsage' (map pack errs)
      when (null filenames)  $ exitUsage' ["No input files"]

      conf     <- either (exitUsage' . pure) pure $ Config.interpret flags

      systemLP <- getSystemLoadPath
      let conf' = over loadPath (<> (systemLP <> ["."])) conf

      when (conf'^.verbose) $ putStrLn ("loadpath: " <> intercalate "," (conf'^.loadPath))

      mapM_ (embedFile conf') filenames

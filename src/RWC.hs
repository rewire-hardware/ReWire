{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module RWC (main) where

import ReWire.Config (loadPath, verbose)
import ReWire.Flags (Flag (..))
import ReWire.FrontEnd (compileFile)
import qualified ReWire.Config as Config

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
       , Option ['f'] ["firrtl"]        (NoArg  FlagFirrtl)                        "Produce FIRRTL output (experimental)."
       , Option []    ["verilog"]       (NoArg  FlagVerilog)                       "Produce Verilog output (default)."
       , Option []    ["vhdl"]          (NoArg  FlagVhdl)                          "Produce VHDL output (experimental)."
       , Option []    ["from-core"]     (NoArg  FlagFromCore)                      "Ingest ReWire core language files instead of Haskell."
       , Option []    ["core"]          (NoArg  FlagCore)                          "Produce ReWire core language output."
       , Option []    ["invert-reset"]  (NoArg  FlagInvertReset)                   "Invert the implicitly generated reset signal."
       , Option []    ["no-reset"]      (NoArg  FlagNoReset)                       "No implicitly generated reset signal."
       , Option []    ["no-clock"]      (NoArg  FlagNoClock)                       "No implicitly generated clock signal (implies no-reset: generate a purely combinatorial circuit)."
       , Option []    ["sync-reset"]    (NoArg  FlagSyncReset)                     "Only reset on positive clock edge."
       , Option ['d'] ["dump"]          (ReqArg FlagDump        "1,2,...")         "Dump the intermediate form of the corresponding pass number (1-13; see -v output)."
       , Option []    ["flatten"]       (NoArg  FlagFlatten)                       "Flatten RTL output into a single module (currently slow, memory-intensive)."
       , Option ['o'] []                (ReqArg FlagO           "filename.vhdl")   "Name for output file."
       , Option ['p'] ["vhdl-packages"] (ReqArg FlagVhdlPkgs    "pkg1,pkg2,...")   "Packages to use for external VHDL components (e.g., ieee.std_logic_1164.all)."
       , Option []    ["reset"]         (ReqArg FlagResetName   "name")            "Name to use for reset signal in generated RTL."
       , Option []    ["clock"]         (ReqArg FlagClockName   "name")            "Name to use for clock signal in generated RTL."
       , Option []    ["inputs"]        (ReqArg FlagInputNames  "name1,name2,...") "Names to use for input signals in generated RTL."
       , Option []    ["outputs"]       (ReqArg FlagOutputNames "name1,name2,...") "Names to use for output signals in generated RTL."
       , Option []    ["states"]        (ReqArg FlagStateNames  "name1,name2,...") "Names to use for internal state signals."
       , Option []    ["start"]         (ReqArg FlagStart       "name")            "Symbol to use for the definition of the top-level module (default: Main.start)."
       , Option []    ["top"]           (ReqArg FlagTop         "name")            "Name to use for the top-level module in generated RTL (default: top_level)."
       , Option []    ["loadpath"]      (ReqArg FlagLoadPath    "dir1,dir2,...")   "Additional directories for loadpath."
       , Option []    ["interpret"]     (OptArg FlagInterpret   "inputs.yaml")     "Interpret instead of compile, using inputs from the optional argument file (default: inputs.yaml)."
       , Option []    ["cycles"]        (ReqArg FlagCycles      "ncycles")         "Number of cycles to interpret (default: 10)."
       , Option []    ["depth"]         (ReqArg FlagEvalDepth   "depth")           "Partial evaluation depth. Higher values can cause non-termination. (default: 8)."
       , Option []    ["pretty"]        (NoArg  FlagPretty)                        "Attempt to output prettier RTL at the expense of performance."
       ]

exitUsage :: IO a
exitUsage = T.hPutStr stderr (pack $ usageInfo "\nUsage: rwc [OPTION...] <filename.hs>" options) >> exitFailure

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

      mapM_ (compileFile conf') filenames

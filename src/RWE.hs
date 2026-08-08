{-# LANGUAGE Safe #-}
module RWE (main) where

import Driver (driverMain)
import Embedder.FrontEnd (embedFile)
import ReWire.Flags (Flag (..))

import System.Console.GetOpt (OptDescr (..), ArgDescr (..))

options :: [OptDescr Flag]
options =
       [ Option ['h'] ["help"]          (NoArg  FlagHelp)                          "This help message."
       , Option ['v'] ["verbose"]       (NoArg  FlagVerbose)                       "More verbose output."
       , Option ['w'] ["no-warn"]       (NoArg  FlagNoWarn)                        "Suppress warnings."
       , Option ['W'] []                (ReqArg FlagW           "error")           "-Werror: treat warnings as errors."
       , Option ['d'] ["dump"]          (ReqArg FlagDump        "1,2,...")         "Dump the intermediate form of the corresponding pass number (1-3; see -v output)."
       , Option []    ["dump-all"]      (NoArg  FlagDumpAll)                       "Dump the intermediate form of every pass (see -d)."
       , Option ['o'] []                (ReqArg FlagO           "filename.thy")    "Name for output file."
       , Option []    ["start"]         (ReqArg FlagStart       "name")            "Symbol to use for the definition of the top-level module (default: Main.start)."
       , Option []    ["loadpath"]      (ReqArg FlagLoadPath    "dir1,dir2,...")   "Additional directories for loadpath."
       , Option []    ["pretty"]        (NoArg  FlagPretty)                        "Attempt to output a prettier Isabelle theory at the expense of performance."
       ]

main :: IO ()
main = driverMain "rwe" options embedFile

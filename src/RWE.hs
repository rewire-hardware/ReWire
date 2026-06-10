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
       , Option ['d'] ["dump"]          (ReqArg FlagDump        "1,2,...")         "Dump the intermediate form of the corresponding pass number (1-13; see -v output)."
       , Option []    ["flatten"]       (NoArg  FlagFlatten)                       "Flatten RTL output into a single module (currently slow, memory-intensive)."
       , Option ['o'] []                (ReqArg FlagO           "filename.vhdl")   "Name for output file."
       , Option []    ["start"]         (ReqArg FlagStart       "name")            "Symbol to use for the definition of the top-level module (default: Main.start)."
       , Option []    ["top"]           (ReqArg FlagTop         "name")            "Name to use for the top-level module in generated RTL (default: top_level)."
       , Option []    ["loadpath"]      (ReqArg FlagLoadPath    "dir1,dir2,...")   "Additional directories for loadpath."
       , Option []    ["pretty"]        (NoArg  FlagPretty)                        "Attempt to output prettier RTL at the expense of performance."
       ]

main :: IO ()
main = driverMain "rwe" options embedFile

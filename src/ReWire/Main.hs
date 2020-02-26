{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}
module ReWire.Main (main) where

import ReWire.FrontEnd (loadProgram, LoadPath)
import ReWire.Pretty (prettyPrint)
import ReWire.Core.ToMiniHDL (compileProgram)
import ReWire.Flags (Flag (..))
import ReWire.Error (runSyntaxError)

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, unless)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((-<.>))
import System.IO (hPutStr, hPutStrLn, hPrint, stderr)

import Paths_ReWire (getDataFileName)

options :: [OptDescr Flag]
options =
       [ Option ['v'] ["verbose"]           (NoArg FlagV)       "More verbose output."
       , Option []    ["dhask1" , "dpass1"] (NoArg FlagDHask1)  "Dump pass 1: pre-desugar haskell source."
       , Option []    ["dhask2" , "dpass2"] (NoArg FlagDHask2)  "Dump pass 2: post-desugar haskell source."
       , Option []    ["dcrust1", "dpass3"] (NoArg FlagDCrust1) "Dump pass 3: post-desugar crust source."
       , Option []    ["dcrust2", "dpass4"] (NoArg FlagDCrust2) "Dump pass 4: post-inlining crust source."
       , Option []    ["dcrust3", "dpass5"] (NoArg FlagDCrust3) "Dump pass 5: pre-purify crust source."
       , Option []    ["dcrust4", "dpass6"] (NoArg FlagDCrust4) "Dump pass 6: post-purify crust source."
       , Option []    ["dcrust5", "dpass7"] (NoArg FlagDCrust5) "Dump pass 7: post-second-lambda-lifting crust source."
       , Option []    ["dcore"  , "dpass8"] (NoArg FlagDCore)   "Dump pass 8: core source."
       , Option []    ["dtypes"]            (NoArg FlagDTypes)  "Enable extra typechecking after various IR transformations."
       , Option ['o'] []                    (ReqArg FlagO "filename.vhd")
            "Name for VHDL output file."
       , Option []    ["loadpath"]          (ReqArg FlagLoadPath "dir1,dir2,...")
            "Additional directories for loadpath."
       ]

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: rwc [OPTION...] <filename.rw>" options) >> exitFailure

getSystemLoadPath :: IO [FilePath]
getSystemLoadPath = do
      lib <- getDataFileName "src/lib"
      rwlib <- getDataFileName "src/rwlib"
      pure $ "." : [lib, rwlib]

main :: IO ()
main = do
      (flags, filenames, errs) <-  getOpt Permute options <$> getArgs

      unless (null errs) $ do
            mapM_ (hPutStrLn stderr) errs
            exitUsage

      let userLP                   =  concatMap getLoadPathEntries flags
      systemLP                     <- getSystemLoadPath
      let lp                       =  userLP ++ systemLP

      when (FlagV `elem` flags) $ putStrLn ("loadpath: " ++ intercalate "," lp)

      mapM_ (compileFile flags $ userLP ++ systemLP) filenames

      where getOutFile :: [Flag] -> String -> IO String
            getOutFile flags filename = case filter (\ case { FlagO {} -> True; _ -> False }) flags of
                  []        -> pure $ filename -<.> "vhdl"
                  [FlagO o] -> pure o
                  _         -> hPutStrLn stderr "Multiple output files specified on the command line!" >> exitFailure

            getLoadPathEntries :: Flag -> [FilePath]
            getLoadPathEntries (FlagLoadPath ds) =  splitOn "," ds
            getLoadPathEntries _                 =  []

            compileFile :: [Flag] -> LoadPath -> String -> IO ()
            compileFile flags lp filename = do
                  when (FlagV `elem` flags) $ putStrLn $ "Compiling: " ++ filename

                  fout <- getOutFile flags filename

                  runSyntaxError (loadProgram flags lp filename >>= compileProgram >>= liftIO . writeFile fout . prettyPrint)
                        >>= either ((>> exitFailure) . hPrint stderr) pure

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Main (main) where

import ReWire.Annotation (unAnn)
import ReWire.FrontEnd (loadProgram, LoadPath)
import ReWire.Pretty (Pretty, prettyPrint)
import qualified ReWire.Core.Syntax as C
import ReWire.Core.ToMiniHDL (compileProgram)
import ReWire.Core.Transform (removeEmpty)
import ReWire.Crust.Cache (printHeader)
import ReWire.MiniHDL.ToLoFIRRTL (toLoFirrtl)
import ReWire.Flags (Flag (..))
import ReWire.Error (runSyntaxError, SyntaxErrorT)

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, unless)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((-<.>))
import System.IO (stderr)

import TextShow (showt)

import Data.Text (pack)
import qualified Data.Text.IO as T

import Paths_ReWire (getDataFileName)

options :: [OptDescr Flag]
options =
       [ Option ['v'] ["verbose"]           (NoArg FlagV)       "More verbose output."
       , Option ['f'] ["firrtl"]            (NoArg FlagFirrtl)  "Produce FIRRTL output instead of VHDL."
       , Option []    ["dpass1", "dhask1" ] (NoArg FlagDHask1)  "Dump pass 1: pre-desugar haskell source."
       , Option []    ["dpass2", "dhask2" ] (NoArg FlagDHask2)  "Dump pass 2: post-desugar haskell source."
       , Option []    ["dpass3", "dcrust1"] (NoArg FlagDCrust1) "Dump pass 3: post-desugar crust source."
       , Option []    ["dpass4", "dcrust2"] (NoArg FlagDCrust2) "Dump pass 4: post-inlining crust source."
       , Option []    ["dpass5", "dcrust3"] (NoArg FlagDCrust3) "Dump pass 5: pre-purify crust source."
       , Option []    ["dpass6", "dcrust4"] (NoArg FlagDCrust4) "Dump pass 6: post-purify crust source."
       , Option []    ["dpass7", "dcrust5"] (NoArg FlagDCrust5) "Dump pass 7: post-second-lambda-lifting crust source."
       , Option []    ["dpass8", "dcore1"  ] (NoArg FlagDCore1)   "Dump pass 8: core source."
       , Option []    ["dpass9", "dcore2"  ] (NoArg FlagDCore2)   "Dump pass 9: core source after purging empty types."
       , Option []    ["dtypes"]            (NoArg FlagDTypes)  "Enable extra typechecking after various IR transformations."
       , Option ['o'] []                    (ReqArg FlagO "filename.vhd")
            "Name for VHDL output file."
       , Option []    ["loadpath"]          (ReqArg FlagLoadPath "dir1,dir2,...")
            "Additional directories for loadpath."
       ]

exitUsage :: IO ()
exitUsage = T.hPutStr stderr (pack $ usageInfo "Usage: rwc [OPTION...] <filename.hs>" options) >> exitFailure

getSystemLoadPath :: IO [FilePath]
getSystemLoadPath = do
      lib <- getDataFileName "src/lib"
      rwlib <- getDataFileName "src/rwlib"
      pure $ "." : [lib, rwlib]

main :: IO ()
main = do
      (flags, filenames, errs) <-  getOpt Permute options <$> getArgs

      unless (null errs) $ do
            mapM_ (T.hPutStrLn stderr . pack) errs
            exitUsage

      let userLP                   =  concatMap getLoadPathEntries flags
      systemLP                     <- getSystemLoadPath
      let lp                       =  userLP ++ systemLP

      when (FlagV `elem` flags) $ putStrLn ("loadpath: " ++ intercalate "," lp)

      mapM_ (compileFile flags $ userLP ++ systemLP) filenames

      where getOutFile :: [Flag] -> String -> IO String
            getOutFile flags filename = case filter (\ case { FlagO {} -> True; _ -> False }) flags of
                  []        | FlagFirrtl `elem` flags -> pure $ filename -<.> "fir"
                            | otherwise               -> pure $ filename -<.> "vhdl"
                  [FlagO o] -> pure o
                  _         -> T.hPutStrLn stderr "Multiple output files specified on the command line!" >> exitFailure

            getLoadPathEntries :: Flag -> [FilePath]
            getLoadPathEntries (FlagLoadPath ds) =  splitOn "," ds
            getLoadPathEntries _                 =  []

            compileFile :: [Flag] -> LoadPath -> String -> IO ()
            compileFile flags lp filename = do
                  when (FlagV `elem` flags) $ putStrLn $ "Compiling: " ++ filename

                  runSyntaxError (loadProgram flags lp filename >>= compile)
                        >>= either ((>> exitFailure) . T.hPutStrLn stderr . prettyPrint) pure

                  where compile :: C.Program -> SyntaxErrorT IO ()
                        compile a = do
                              b <- removeEmpty a
                              when (FlagDCore2 `elem` flags) $ liftIO $ do
                                    printHeader "Emptied Core" -- TODO(chathhorn): pull this out of Crust.Cache
                                    T.putStrLn $ prettyPrint b
                                    when (FlagV `elem` flags) $ T.putStrLn "\n## Show core:\n"
                                    when (FlagV `elem` flags) $ T.putStrLn $ showt $ unAnn b
                              if FlagFirrtl `elem` flags
                                    then compileProgram b >>= toLoFirrtl >>= writeOutput
                                    else compileProgram a >>= writeOutput

                        writeOutput :: Pretty a => a -> SyntaxErrorT IO ()
                        writeOutput a = do
                              fout <- liftIO $ getOutFile flags filename
                              liftIO $ T.writeFile fout $ prettyPrint a

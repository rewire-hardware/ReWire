{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Main (main) where

import ReWire.Annotation (unAnn)
import ReWire.FrontEnd (loadProgram, LoadPath)
import ReWire.Pretty (Pretty, prettyPrint)
import qualified ReWire.Core.Syntax as C
import ReWire.Core.ToVHDL (compileProgram)
import qualified ReWire.Core.ToVerilog as Verilog
import ReWire.Core.Transform (mergeSlices, purgeUnused, partialEval)
import ReWire.Core.Interp (interp, Ins, run)
import ReWire.Crust.Cache (printHeader)
import ReWire.VHDL.ToLoFIRRTL (toLoFirrtl)
import ReWire.Flags (Flag (..))
import ReWire.Error (runSyntaxError, SyntaxErrorT)

import Control.Monad.IO.Class (liftIO)
import Control.Monad ((>=>), when, unless, msum)
import Data.List (intercalate, foldl')
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((-<.>))
import System.IO (stderr)
import TextShow (showt)
import Data.Text (pack)
import Data.Either (fromRight)
import qualified Data.Text.IO as T
import qualified Data.Yaml as YAML
import qualified Data.HashMap.Strict as Map

import Paths_ReWire (getDataFileName)

options :: [OptDescr Flag]
options =
       [ Option ['v'] ["verbose"]            (NoArg  FlagV)                             "More verbose output."
       , Option ['f'] ["firrtl"]             (NoArg  FlagFirrtl)                        "Produce FIRRTL output instead of VHDL."
       , Option []    ["verilog"]            (NoArg  FlagVerilog)                       "Produce Verilog output instead of VHDL."
       , Option []    ["invert-reset"]       (NoArg  FlagInvertReset)                   "Invert the implicitly generated reset signal."
       , Option []    ["dpass1", "dhask1" ]  (NoArg  FlagDHask1)                        "Dump pass 1: pre-desugar haskell source."
       , Option []    ["dpass2", "dhask2" ]  (NoArg  FlagDHask2)                        "Dump pass 2: post-desugar haskell source."
       , Option []    ["dpass3", "dcrust1"]  (NoArg  FlagDCrust1)                       "Dump pass 3: post-desugar crust source."
       , Option []    ["dpass4", "dcrust2"]  (NoArg  FlagDCrust2)                       "Dump pass 4: post-inlining crust source."
       , Option []    ["dpass5", "dcrust3"]  (NoArg  FlagDCrust3)                       "Dump pass 5: pre-purify crust source."
       , Option []    ["dpass6", "dcrust4"]  (NoArg  FlagDCrust4)                       "Dump pass 6: post-purify crust source."
       , Option []    ["dpass7", "dcrust5"]  (NoArg  FlagDCrust5)                       "Dump pass 7: post-second-lambda-lifting crust source."
       , Option []    ["dpass8", "dcore1"  ] (NoArg  FlagDCore1)                        "Dump pass 8: core source."
       , Option []    ["dpass9", "dcore2"  ] (NoArg  FlagDCore2)                        "Dump pass 9: core source after purging empty types."
       , Option []    ["dtypes"]             (NoArg  FlagDTypes)                        "Enable extra typechecking after various IR transformations."
       , Option []    ["flatten"]            (NoArg  FlagFlatten)                       "Generate a single RTL module."
       , Option ['o'] []                     (ReqArg FlagO           "filename.vhdl")   "Name for RTL output file."
       , Option ['p'] ["packages"]           (ReqArg FlagPkgs        "pkg1,pkg2,...")   "Packages to use for external VHDL components (e.g., ieee.std_logic_1164.all)."
       , Option []    ["inputs"]             (ReqArg FlagInputNames  "name1,name2,...") "Names to use for input signals in generated RTL."
       , Option []    ["outputs"]            (ReqArg FlagOutputNames "name1,name2,...") "Names to use for output signals in generated RTL."
       , Option []    ["loadpath"]           (ReqArg FlagLoadPath    "dir1,dir2,...")   "Additional directories for loadpath."
       , Option []    ["interpret"]          (OptArg FlagInterpret   "inputs.yaml")     "Interpret instead of compile, using inputs from the optional argument file (default: inputs.yaml)."
       , Option []    ["cycles"]             (ReqArg FlagCycles      "ncycles")         "Number of cycles to interpret (default: 10)."
       ]

exitUsage :: IO ()
exitUsage = T.hPutStr stderr (pack $ usageInfo "Usage: rwc [OPTION...] <filename.hs>" options) >> exitFailure

getSystemLoadPath :: IO [FilePath]
getSystemLoadPath = do
      lib   <- getDataFileName "src/lib"
      rwlib <- getDataFileName "src/rwlib"
      pure $ "." : [lib, rwlib]

main :: IO ()
main = do
      (flags, filenames, errs) <-  getOpt Permute options <$> getArgs

      unless (null errs) $ do
            mapM_ (T.hPutStrLn stderr . pack) errs
            exitUsage

      let userLP                = concatMap getLoadPathEntries flags
      systemLP                 <- getSystemLoadPath
      let lp                    = userLP ++ systemLP

      when (FlagV `elem` flags) $ putStrLn ("loadpath: " ++ intercalate "," lp)

      mapM_ (compileFile flags $ userLP ++ systemLP) filenames

      where getOutFile :: [Flag] -> String -> IO String
            getOutFile flags filename = case filter (\ case { FlagO {} -> True; _ -> False }) flags of
                  []        | FlagFirrtl  `elem` flags -> pure $ filename -<.> "fir"
                            | FlagVerilog `elem` flags -> pure $ filename -<.> "v"
                            | flagInterp flags         -> pure $ filename -<.> "yaml"
                            | otherwise                -> pure $ filename -<.> "vhdl"
                  [FlagO o] -> pure o
                  _         -> T.hPutStrLn stderr "Multiple output files specified on the command line!" >> exitFailure

            flagInterp :: [Flag] -> Bool
            flagInterp = or . map (\ case
                  FlagInterpret _ -> True
                  _               -> False)

            interpInput :: [Flag] -> FilePath
            interpInput = fromMaybe "inputs.yaml" . msum . map (\ case
                  FlagInterpret fp -> fp
                  _                -> Nothing)

            ncycles :: [Flag] -> Int
            ncycles = fromMaybe 10 . msum . map (\ case
                  FlagCycles n -> Just $ read n
                  _            -> Nothing)

            getLoadPathEntries :: Flag -> [FilePath]
            getLoadPathEntries = \ case
                  FlagLoadPath ds -> splitOn "," ds
                  _               -> []

            compileFile :: [Flag] -> LoadPath -> String -> IO ()
            compileFile flags lp filename = do
                  when (FlagV `elem` flags) $ putStrLn $ "Compiling: " ++ filename

                  runSyntaxError (loadProgram flags lp filename >>= compile)
                        >>= either ((>> exitFailure) . T.hPutStrLn stderr . prettyPrint) pure

                  where compile :: C.Program -> SyntaxErrorT IO ()
                        compile a = do
                              b <- (mergeSlices >=> mergeSlices >=> partialEval >=> purgeUnused) a -- TODO(chathhorn)
                              when (FlagV `elem` flags) $ liftIO $ putStrLn $ "Debug: [Pass 9] Reduced core."
                              when (FlagDCore2 `elem` flags) $ liftIO $ do
                                    printHeader "Reduced Core" -- TODO(chathhorn): pull this out of Crust.Cache
                                    T.putStrLn $ prettyPrint b
                                    when (FlagV `elem` flags) $ do
                                          T.putStrLn "\n## Show core:\n"
                                          T.putStrLn $ showt $ unAnn b
                              case () of
                                    _ | FlagFirrtl    `elem` flags -> compileProgram flags a >>= toLoFirrtl >>= writeOutput -- TODO(chathhorn): a => b
                                      | FlagVerilog   `elem` flags -> Verilog.compileProgram flags b >>= writeOutput
                                      | flagInterp flags           -> do
                                          ips <- liftIO $ YAML.decodeFileEither $ interpInput flags
                                          let outs = run (interp flags b) (boundInput (ncycles flags) $ fromRight mempty ips)
                                          fout <- liftIO $ getOutFile flags filename
                                          liftIO $ YAML.encodeFile fout outs
                                      | otherwise                  -> compileProgram flags a >>= writeOutput

                        writeOutput :: Pretty a => a -> SyntaxErrorT IO ()
                        writeOutput a = do
                              fout <- liftIO $ getOutFile flags filename
                              liftIO $ T.writeFile fout $ prettyPrint a

-- | Replicates/truncates inputs to fill up exactly ncycles cycles.
boundInput :: Int -> [Ins] -> [Ins]
boundInput ncycles ips = foldl' (\ ms m -> ms <> [Map.union m (last' ms)]) [] ips''
      where ips' :: [Ins]
            ips' = take ncycles ips

            ips'' :: [Ins]
            ips'' = ips' <> replicate (ncycles - length ips') (last' ips')

lastMaybe :: [a] -> Maybe a
lastMaybe = \ case
      []       -> Nothing
      [a]      -> Just a
      (_ : as) -> lastMaybe as

last' :: Monoid a => [a] -> a
last' = fromMaybe mempty . lastMaybe

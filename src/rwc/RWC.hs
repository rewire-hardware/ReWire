{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
module RWC (main) where

import ReWire.Annotation (unAnn)
import ReWire.FrontEnd (loadProgram, LoadPath)
import ReWire.Pretty (Pretty, prettyPrint, fastPrint)
import qualified ReWire.Core.Syntax as C
import qualified ReWire.Core.ToVHDL as VHDL
import qualified ReWire.Core.ToVerilog as Verilog
import ReWire.Core.Transform (mergeSlices, purgeUnused, partialEval, dedupe)
import ReWire.Core.Interp (interp, Ins, run)
import ReWire.ModCache (printHeader)
-- import ReWire.VHDL.ToLoFIRRTL (toLoFirrtl)
import ReWire.Flags (Flag (..))
import ReWire.Error (AstError, runSyntaxError, SyntaxErrorT)

import Control.Monad.IO.Class (liftIO)
import Control.Monad ((>=>), when, msum)
import Data.List (intercalate, foldl')
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((-<.>), (</>))
import System.IO (stderr)
import TextShow (showt)
import Data.Text (pack)
import Data.Either (fromRight)
import qualified Data.Text.IO as T
import qualified Data.Yaml as YAML
import qualified Data.HashMap.Strict as Map

import Paths_rewire (getDataFileName)

options :: [OptDescr Flag]
options =
       [ Option ['h'] ["help"]                (NoArg  FlagH)                             "This help message."
       , Option ['v'] ["verbose"]             (NoArg  FlagV)                             "More verbose output."
       , Option ['f'] ["firrtl"]              (NoArg  FlagFirrtl)                        "Produce FIRRTL output (experimental)."
       , Option []    ["verilog"]             (NoArg  FlagVerilog)                       "Produce Verilog output (default)."
       , Option []    ["vhdl"]                (NoArg  FlagVhdl)                          "Produce VHDL output (experimental)."
       , Option []    ["invert-reset"]        (NoArg  FlagInvertReset)                   "Invert the implicitly generated reset signal."
       , Option []    ["no-reset"]            (NoArg  FlagNoReset)                       "No implicitly generated reset signal."
       , Option []    ["no-clock"]            (NoArg  FlagNoClock)                       "No implicitly generated clock signal (implies no-reset: generate a purely combinatorial circuit)."
       , Option []    ["sync-reset"]          (NoArg  FlagSyncReset)                     "Only reset on positive clock edge."
       , Option []    ["dpass1"]              (NoArg  FlagDPass1)                        "Dump pass 1: pre-desugar haskell source."
       , Option []    ["dpass2"]              (NoArg  FlagDPass2)                        "Dump pass 2: post-desugar haskell source."
       , Option []    ["dpass3"]              (NoArg  FlagDPass3)                        "Dump pass 3: synthetic per-module crust source."
       , Option []    ["dpass4"]              (NoArg  FlagDPass4)                        "Dump pass 4: post-desugar crust source."
       , Option []    ["dpass5"]              (NoArg  FlagDPass5)                        "Dump pass 5: post-inlining crust source."
       , Option []    ["dpass6"]              (NoArg  FlagDPass6)                        "Dump pass 6: post-typechecking crust source."
       , Option []    ["dpass7"]              (NoArg  FlagDPass7)                        "Dump pass 7: pre-simplify crust source."
       , Option []    ["dpass8"]              (NoArg  FlagDPass8)                        "Dump pass 8: post-simplify crust source."
       , Option []    ["dpass9"]              (NoArg  FlagDPass9)                        "Dump pass 9: pre-purify crust source."
       , Option []    ["dpass10"]             (NoArg  FlagDPass10)                       "Dump pass 10: post-purify crust source."
       , Option []    ["dpass11"]             (NoArg  FlagDPass11)                       "Dump pass 11: post-second-lambda-lifting crust source."
       , Option []    ["dpass12"]             (NoArg  FlagDPass12)                       "Dump pass 12: core source."
       , Option []    ["dpass13"]             (NoArg  FlagDPass13)                       "Dump pass 13: core source after purging empty types."
       , Option []    ["flatten"]             (NoArg  FlagFlatten)                       "Flatten RTL output into a single module (flattening is currently slow, memory-intensive)."
       , Option ['o'] []                      (ReqArg FlagO           "filename.vhdl")   "Name for RTL output file."
       , Option ['p'] ["packages"]            (ReqArg FlagPkgs        "pkg1,pkg2,...")   "Packages to use for external VHDL components (e.g., ieee.std_logic_1164.all)."
       , Option []    ["reset"]               (ReqArg FlagResetName   "name")            "Name to use for reset signal in generated RTL."
       , Option []    ["clock"]               (ReqArg FlagClockName   "name")            "Name to use for clock signal in generated RTL."
       , Option []    ["inputs"]              (ReqArg FlagInputNames  "name1,name2,...") "Names to use for input signals in generated RTL."
       , Option []    ["outputs"]             (ReqArg FlagOutputNames "name1,name2,...") "Names to use for output signals in generated RTL."
       , Option []    ["states"]              (ReqArg FlagStateNames  "name1,name2,...") "Names to use for internal state signals."
       , Option []    ["top"]                 (ReqArg FlagTop         "name")            "Symbol to use for the definition of the top-level module (default: Main.start)."
       , Option []    ["loadpath"]            (ReqArg FlagLoadPath    "dir1,dir2,...")   "Additional directories for loadpath."
       , Option []    ["interpret"]           (OptArg FlagInterpret   "inputs.yaml")     "Interpret instead of compile, using inputs from the optional argument file (default: inputs.yaml)."
       , Option []    ["cycles"]              (ReqArg FlagCycles      "ncycles")         "Number of cycles to interpret (default: 10)."
       , Option []    ["depth"]               (ReqArg FlagEvalDepth   "depth")           "Partial evaluation depth. Higher values can cause non-termination. (default: 8)."
       , Option []    ["pretty"]              (NoArg FlagPretty)                         "Attempt to write prettier RTL output at the expense of performance."
       ]

exitUsage :: IO ()
exitUsage = T.hPutStr stderr (pack $ usageInfo "Usage: rwc [OPTION...] <filename.hs>" options) >> exitFailure

getSystemLoadPath :: IO [FilePath]
getSystemLoadPath = do
      lib   <- getDataFileName $ "rewire-user" </> "src"
      pure [lib]

main :: IO ()
main = do
      (flags, filenames, errs) <-  getOpt Permute options <$> getArgs

      when (FlagH `elem` flags || not (null errs)) $ do
            mapM_ (T.hPutStrLn stderr . pack) errs
            exitUsage

      let userLP                = concatMap getLoadPathEntries flags
      systemLP                 <- getSystemLoadPath
      let lp                    = userLP <> systemLP <> ["."]

      when (FlagV `elem` flags) $ putStrLn ("loadpath: " <> intercalate "," lp)

      mapM_ (compileFile flags lp) filenames

      where getOutFile :: [Flag] -> String -> IO String
            getOutFile flags filename = case filter (\ case { FlagO {} -> True; _ -> False }) flags of
                  []        | FlagFirrtl  `elem` flags -> pure $ filename -<.> "fir"
                            | FlagVhdl `elem` flags    -> pure $ filename -<.> "vhdl"
                            | flagInterp flags         -> pure $ filename -<.> "yaml"
                            | otherwise                -> pure $ filename -<.> "v"
                  [FlagO o] -> pure o
                  _         -> T.hPutStrLn stderr "Multiple output files specified on the command line!" >> exitFailure

            flagInterp :: [Flag] -> Bool
            flagInterp = any (\ case
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
                  when (FlagV `elem` flags) $ putStrLn $ "Compiling: " <> filename

                  runSyntaxError (loadProgram flags lp filename >>= compile)
                        >>= either ((>> exitFailure) . T.hPutStrLn stderr . prettyPrint) pure

                  where compile :: C.Program -> SyntaxErrorT AstError IO ()
                        compile a = do
                              b <-  (    mergeSlices
                                    >=> mergeSlices
                                    >=> partialEval
                                    >=> mergeSlices
                                    >=> dedupe
                                    >=> purgeUnused
                                    ) a -- TODO(chathhorn)
                              when (FlagV `elem` flags) $ liftIO $ putStrLn "Debug: [Pass 13] Reduced core."
                              when (FlagDPass13 `elem` flags) $ liftIO $ do
                                    printHeader "[Pass 13] Reduced Core" -- TODO(chathhorn): pull this out of Crust.Cache
                                    T.putStrLn $ prettyPrint b
                                    when (FlagV `elem` flags) $ do
                                          T.putStrLn "\n## Show core:\n"
                                          T.putStrLn $ showt $ unAnn b
                              if | FlagFirrtl    `elem` flags -> liftIO $ putStrLn "FIRRTL backend currently out-of-order. Use '--verilog' or '--interpret'."
                                 -- compileProgram flags a >>= toLoFirrtl >>= writeOutput -- TODO(chathhorn): a => b
                                 | FlagVhdl      `elem` flags -> VHDL.compileProgram flags a >>= writeOutput
                                 | flagInterp flags           -> do
                                     ips  <- liftIO $ YAML.decodeFileEither $ interpInput flags
                                     outs <- run (interp flags b) (boundInput (ncycles flags) $ fromRight mempty ips)
                                     fout <- liftIO $ getOutFile flags filename
                                     liftIO $ YAML.encodeFile fout outs
                                 | otherwise                  -> Verilog.compileProgram flags b >>= writeOutput

                        writeOutput :: Pretty a => a -> SyntaxErrorT AstError IO ()
                        writeOutput a = do
                              fout <- liftIO $ getOutFile flags filename
                              liftIO $ T.writeFile fout $ if | FlagPretty `elem` flags -> prettyPrint a
                                                             | otherwise               -> fastPrint a

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

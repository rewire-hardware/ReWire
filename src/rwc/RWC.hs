{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
module RWC (main) where

import ReWire.Annotation (unAnn)
import ReWire.Config (Config, Target (..), loadPath, getOutFile, verbose, target, dump, cycles, inputsFile)
import ReWire.Core.Interp (interp, Ins, run)
import ReWire.Core.Transform (mergeSlices, purgeUnused, partialEval, dedupe)
import ReWire.Error (AstError, runSyntaxError, SyntaxErrorT)
import ReWire.Flags (Flag (..))
import ReWire.FrontEnd (loadProgram, LoadPath)
import ReWire.ModCache (printHeader)
import ReWire.Pretty (Pretty, prettyPrint, fastPrint, showt)

import qualified ReWire.Config         as Config
import qualified ReWire.Core.Syntax    as C
import qualified ReWire.Core.ToVHDL    as VHDL
import qualified ReWire.Core.ToVerilog as Verilog

import Control.Lens ((^.))
import Control.Monad ((>=>), when)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import Data.List (intercalate, foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)
import qualified Data.Text.IO as T
import qualified Data.Yaml as YAML
import qualified Data.HashMap.Strict as Map

import Paths_rewire (getDataFileName)

options :: [OptDescr Flag]
options =
       [ Option ['h'] ["help"]          (NoArg  FlagHelp)                          "This help message."
       , Option ['v'] ["verbose"]       (NoArg  FlagVerbose)                       "More verbose output."
       , Option ['f'] ["firrtl"]        (NoArg  FlagFirrtl)                        "Produce FIRRTL output (experimental)."
       , Option []    ["verilog"]       (NoArg  FlagVerilog)                       "Produce Verilog output (default)."
       , Option []    ["vhdl"]          (NoArg  FlagVhdl)                          "Produce VHDL output (experimental)."
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
       , Option []    ["top"]           (ReqArg FlagTop         "name")            "Symbol to use for the definition of the top-level module (default: Main.start)."
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
      let lp    = conf^.loadPath <> systemLP <> ["."]

      when (conf^.verbose) $ putStrLn ("loadpath: " <> intercalate "," lp)

      mapM_ (compileFile conf lp) filenames

compileFile :: Config -> LoadPath -> String -> IO ()
compileFile conf lp filename = do
      when (conf^.verbose) $ putStrLn $ "Compiling: " <> filename

      runSyntaxError (loadProgram conf lp filename >>= compile)
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
                  when (conf^.verbose)   $ liftIO $ putStrLn "Debug: [Pass 13] Reduced core."
                  when (conf^.dump $ 13) $ liftIO $ do
                        printHeader "[Pass 13] Reduced Core" -- TODO(chathhorn): pull this out of Crust.Cache
                        T.putStrLn $ prettyPrint b
                        when (conf^.verbose) $ do
                              T.putStrLn "\n## Show core:\n"
                              T.putStrLn $ showt $ unAnn b
                  case conf^.target of
                        FIRRTL    -> liftIO $ putStrLn "FIRRTL backend currently out-of-order. Use '--verilog' or '--interpret'."
                                     -- compileProgram flags a >>= toLoFirrtl >>= writeOutput -- TODO(chathhorn): a => b
                        VHDL      -> VHDL.compileProgram conf a >>= writeOutput
                        Interpret -> do
                              ips  <- liftIO $ YAML.decodeFileEither $ conf^.inputsFile
                              outs <- run (interp conf b) (boundInput (conf^.cycles) $ fromRight mempty ips)
                              let fout = getOutFile conf filename
                              liftIO $ YAML.encodeFile fout outs
                        Verilog   -> Verilog.compileProgram conf b >>= writeOutput

            writeOutput :: Pretty a => a -> SyntaxErrorT AstError IO ()
            writeOutput a = do
                  let fout = getOutFile conf filename
                  liftIO $ T.writeFile fout $ if | conf^.Config.pretty -> prettyPrint a
                                                 | otherwise           -> fastPrint a

-- | Replicates/truncates inputs to fill up exactly ncycles cycles.
boundInput :: Natural -> [Ins] -> [Ins]
boundInput ncycles ips = foldl' (\ ms m -> ms <> [Map.union m (last' ms)]) [] ips'
      where ips' :: [Ins]
            ips' = take (fromIntegral ncycles) $ ips <> repeat (last' ips)

lastMaybe :: [a] -> Maybe a
lastMaybe = \ case
      []       -> Nothing
      [a]      -> Just a
      (_ : as) -> lastMaybe as

last' :: Monoid a => [a] -> a
last' = fromMaybe mempty . lastMaybe

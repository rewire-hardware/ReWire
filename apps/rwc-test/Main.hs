module Main (main) where

import qualified RWC

import Control.Exception (try, finally)
import Control.Monad (unless, when, msum)
import Data.List (isSuffixOf, isInfixOf, stripPrefix, intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Directory (listDirectory, setCurrentDirectory, getCurrentDirectory, doesFileExist)
import System.Environment (getArgs)
import System.Environment (withArgs)
import System.Exit (exitFailure, ExitCode (..))
import System.FilePath ((</>), (-<.>), takeBaseName, takeDirectory)
import System.IO (hPutStr, hPutStrLn, hClose, hFlush, openFile, stderr, stdout, Handle, IOMode (WriteMode))
import System.Process (callCommand)
import Test.Tasty (defaultMain, sequentialTestGroup, TestTree, DependencyType (..))
import Test.Tasty.HUnit (testCase, assertFailure, assertBool)
import Test.Tasty.Golden (goldenVsFileDiff)

import Paths_rewire (getDataFileName)

data Flag = FlagH
          | FlagNoCheck
          | FlagNoDTypes
          | FlagNoGhc
          | FlagV
          | FlagIVerilog String
          | FlagVerilator String
          | FlagVhdl
          | FlagVhdlChecker String
          -- Tasty options.
          | FlagP String
          | FlagQ
          | FlagL
          | FlagT String
          | FlagColor String
      deriving (Eq, Show)

defaultIVerilog :: String
defaultIVerilog = "iverilog -Wall -g2012"

defaultVerilator :: String
defaultVerilator = "verilator --lint-only -Wno-MULTITOP"

options :: [OptDescr Flag]
options =
       [ Option ['h'] ["help"]         (NoArg FlagH)                          "Show this help text."
       , Option ['v'] ["verbose"]      (NoArg FlagV)                          "More verbose output."
       , Option []    ["vhdl"]         (NoArg FlagVhdl)                       "Test VHDL code generation in addition to Verilog."
       , Option []    ["no-check"]     (NoArg FlagNoCheck)                    "Disable verification of output HDL with checker."
       , Option []    ["no-ghc"]       (NoArg FlagNoGhc)                      "Disable running tests through ghc."
       , Option []    ["vhdl-checker"] (ReqArg FlagVhdlChecker "command")     "Set the command to use for checking generated VHDL (default: 'ghdl -s')."
       , Option []    ["iverilog"]     (ReqArg FlagIVerilog "command")      $ "Set the command to use for checking generated Verilog with iverilog (default: " <> defaultIVerilog <> ")."
       , Option []    ["verilator"]    (ReqArg FlagVerilator "command")     $ "Set the command to use for checking generated Verilog with verilator (default: " <> defaultVerilator <> ")."

       -- Tasty arguments.
       , Option ['p'] ["pattern"]      (ReqArg FlagP "PATTERN")               "Select only tests which satisfy a pattern or awk expression."
       , Option ['q'] ["quiet"]        (NoArg FlagQ)                          "Do not produce any output; indicate success only by the exit code."
       , Option ['l'] ["list-tests"]   (NoArg FlagL)                          "Do not run the tests; just print their names."
       , Option ['t'] ["timeout"]      (ReqArg FlagT "DURATION")              "Timeout for individual tests (suffixes: ms, s, m, h; default: s)."
       , Option []    ["color"]        (ReqArg FlagColor "never|always|auto") "When to use colored output (default: auto)."
       ]

testCompiler :: [Flag] -> FilePath -> IO [TestTree]
testCompiler flags fn = do
      let ghcTests =
            -- Test: compile Haskell source with GHC
            (if FlagNoGhc `elem` flags then []
                  else [ testCase (takeBaseName fn <> " (stack ghc)") $ do
                        cdTestdir
                        callCommand $ "stack ghc " <> fn
                  ])

      coreTests <-
            -- Test: compile Haskell to Core with RWC.
            ([ golden "rwc" $ do
                 cdTestdir
                 withArgs (fn : ["--core", "-o", ofile "rwc"] <> extraFlags) RWC.main
            ] <>)
            -- Test: interpret Core.
            <$> maybeGolden "yaml" (do
                  cdTestdir
                  withArgs ((fn -<.> "rwc") : ["--from-core", "--interp", "-o", ofile "yaml"] <> extraFlags) RWC.main)

      let verilogTests =
            -- Test: compile Core to Verilog with RWC.
            [ golden "sv" $ do
                  cdTestdir -- TODO enable extra typechecking
                  withArgs ((fn -<.> "rwc") : ["--from-core", "-o", ofile "sv"] <> extraFlags) RWC.main
               ]
            -- Test: check Verilog output with iverilog.
            <> (if FlagNoCheck `elem` flags then []
               else [ testCase (takeBaseName fn <> " (" <> iverilog <> ")") $ do
                  cdTestdir
                  callCommand $ iverilog <> " " <> verilog <> " " <> ofile "sv"
               ])
            -- Test: check Verilog output with verilator.
            <> (if FlagNoCheck `elem` flags then []
               else [ testCase (takeBaseName fn <> " (" <> verilator <> ")") $ do
                  cdTestdir
                  callCommand $ verilator <> " " <> verilog <> " " <> ofile "sv"
               ])

      vhdlTests <-
            -- Test: compile Core to VHDL with RWC. Opt-in per test: only runs
            -- when a .vhdl golden exists, since the VHDL backend supports less
            -- than the Verilog one.
            maybeGolden "vhdl" (do
                  cdTestdir
                  withArgs ((fn -<.> "rwc") : ["--from-core", "--vhdl", "-o", ofile "vhdl"] <> extraFlags) RWC.main)

      pure $ ghcTests <> coreTests <> verilogTests <> vhdlTests

      where extraFlags :: [String]
            extraFlags = if FlagV `elem` flags then ["-v"] else []

            cdTestdir :: IO ()
            cdTestdir = setCurrentDirectory $ takeDirectory fn

            ofile :: String -> FilePath
            ofile ext = fn -<.> ("out." <> ext)

            iverilog :: String
            iverilog = fromMaybe defaultIVerilog $ msum $ flip map flags $ \ case
                  FlagIVerilog c -> Just $ sq c
                  _              -> Nothing

            verilator :: String
            verilator = fromMaybe defaultVerilator $ msum $ flip map flags $ \ case
                  FlagVerilator c -> Just $ sq c
                  _               -> Nothing

            verilog :: FilePath
            verilog = takeDirectory fn </> "verilog" </> "*.sv"

            golden :: FilePath -> IO () -> TestTree
            golden ext = goldenVsFileDiff (takeBaseName fn <> " (golden " <> ext <> ")") diff gold out
                  where gold = fn -<.> ext
                        out  = ofile ext

            maybeGolden :: FilePath -> IO () -> IO [TestTree]
            maybeGolden ext io = doesFileExist gold >>= \ ex ->
                  pure [goldenVsFileDiff (takeBaseName fn <> " (golden " <> ext <> ")") diff gold out io | ex]
                  where gold = fn -<.> ext
                        out  = ofile ext

            diff :: FilePath -> FilePath -> [String]
            diff ref new = ["diff", "-bu", ref, new]

sq :: String -> String
sq = \ case
      '"'  : s | last s == '"'  -> init s
      '\'' : s | last s == '\'' -> init s
      s                         -> s

-- | A test that must fail to compile with rwc. Each test file declares (one
--   or more of) the expected error with a comment line:
--
-- > -- EXPECT-ERROR: <substring of the expected error message>
--
--   The test asserts that rwc exits with failure and that its stderr
--   contains every expected substring. These files are never run through
--   GHC or the HDL checkers.
testNegative :: FilePath -> TestTree
testNegative fn = testCase (takeBaseName fn <> " (expected error)") $ do
      setCurrentDirectory $ takeDirectory fn
      expected <- expectedErrors <$> readFile fn
      when (null expected) $ assertFailure $ "no \"-- EXPECT-ERROR:\" line in " <> fn
      let errFile = fn -<.> "out.error"
      r      <- withStderrTo errFile $ try $ withArgs [fn, "-o", fn -<.> "out.sv"] RWC.main
      errTxt <- readFile errFile
      case r of
            Left (ExitFailure _) -> mapM_ (\ e ->
                  assertBool ("compilation failed as expected, but the error does not mention " <> show e <> "; stderr:\n" <> errTxt)
                        $ e `isInfixOf` errTxt) expected
            Left ExitSuccess     -> assertFailure "expected compilation to fail, but rwc exited successfully"
            Right ()             -> assertFailure "expected compilation to fail, but it succeeded"

      where expectedErrors :: String -> [String]
            expectedErrors = mapMaybe (stripPrefix "-- EXPECT-ERROR: ") . lines

-- | Run an action with a handle (stdout, stderr) redirected to a file.
withHandleTo :: Handle -> FilePath -> IO a -> IO a
withHandleTo hdl f io = do
      saved <- hDuplicate hdl
      h     <- openFile f WriteMode
      hDuplicateTo h hdl
      io `finally` do
            hFlush hdl
            hDuplicateTo saved hdl
            hClose h
            hClose saved

withStderrTo :: FilePath -> IO a -> IO a
withStderrTo = withHandleTo stderr

withStdoutTo :: FilePath -> IO a -> IO a
withStdoutTo = withHandleTo stdout

-- | Compile fixed test programs with flag combinations the golden tests don't
--   exercise (verbose tracing, pass dumps, alternate signal names, reset/clock
--   variations, interpreter cycle bounds, VHDL target). These only assert that
--   rwc succeeds; outputs are not compared against goldens. Stdout (verbose
--   trace, IR dumps) is redirected to a log file next to the other outputs.
getSmokeTests :: IO TestTree
getSmokeTests = do
      dir <- getDataFileName ("tests" </> "regression")
      pure $ sequentialTestGroup "flags" AllFinish
            [ smoke dir "fibo1.hs" "dumps" "sv"
                  [ "-v", "--flatten", "--pretty", "--sync-reset", "--invert-reset"
                  , "--start", "Main.start", "--top", "smoke_top"
                  , "--inputs", "i0", "--outputs", "o0", "--states", "s0,s1"
                  , "--depth", "4", "--rtl-opt", "2", "--loadpath", "."
                  , "-d", intercalate "," (map show [1 :: Int .. 25])
                  ]
            , smoke dir "onestate.hs" "vhdl"      "vhdl" ["--vhdl", "-p", "ieee.std_logic_1164.all"]
            , smoke dir "fibo1.hs"    "noclock"   "sv"   ["--no-clock", "--no-reset"]
            , smoke dir "fibo1.hs"    "interp"    "yaml" ["--interpret", "--cycles", "3"]
            , smoke dir "fibo1.hs"    "typecheck" "rwc"  ["--debug-typecheck", "--core"]
            ]
      where smoke :: FilePath -> FilePath -> String -> String -> [String] -> TestTree
            smoke dir file name ext args = testCase (takeBaseName file <> " (flags: " <> name <> ")") $ do
                  let fn   = dir </> file
                      out  = fn -<.> ("out.smoke-" <> name <> "." <> ext)
                  setCurrentDirectory dir
                  withStdoutTo (fn -<.> ("out.smoke-" <> name <> ".log")) $
                        withArgs ([fn, "-o", out] <> args) RWC.main

getNegTests :: FilePath -> IO TestTree
getNegTests dirName = do
      dir   <- getDataFileName ("tests" </> dirName)
      files <- map (dir </>) . filter (".hs" `isSuffixOf`) <$> listDirectory dir
      pure $ sequentialTestGroup dirName AllFinish $ map testNegative files

getTests :: [Flag] -> FilePath -> IO TestTree
getTests flags dirName = do
      dir   <- getDataFileName ("tests" </> dirName)
      files <- map (dir </>) . filter (".hs" `isSuffixOf`) <$> listDirectory dir
      sequentialTestGroup dirName AllFinish <$> (msum <$> mapM (testCompiler flags) files)

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: rwc-test [OPTION...]" options) >> exitFailure

main :: IO ()
main = do
      (flags, _, errs) <- getOpt Permute options <$> getArgs

      let testDirs = ["regression"] -- TODO(chathhorn): re-enable integration tests.

      unless (null errs && FlagH `notElem` flags) $ do
            mapM_ (hPutStrLn stderr) errs
            exitUsage

      tests      <- mapM (getTests flags) testDirs
      negTests   <- getNegTests "negative"
      smokeTests <- getSmokeTests

      -- The tests change the working directory, so restore it on exit (HPC
      -- writes its .tix relative to the final working directory).
      cwd0 <- getCurrentDirectory
      withArgs (injectTastyArgs flags) (defaultMain $ sequentialTestGroup "Tests" AllFinish $ tests <> [smokeTests, negTests])
            `finally` setCurrentDirectory cwd0

      where injectTastyArgs :: [Flag] -> [String]
            injectTastyArgs = concatMap toTastyArg

            toTastyArg :: Flag -> [String]
            toTastyArg = \ case
                  FlagP p     -> ["-p", p]
                  FlagQ       -> ["-q"]
                  FlagL       -> ["-l"]
                  FlagT t     -> ["-t", t]
                  FlagColor c -> ["--color", c]
                  _           -> []

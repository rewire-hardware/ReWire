-- | rwc-test: the compiler golden test suite. Each tests/golden/*.hs is a
--   ReWire program with golden files alongside it; this driver compiles it
--   (with GHC and with rwc), interprets it, lowers it to Verilog/VHDL/Cryptol,
--   lints the HDL, and cosimulates the backends against each other (see
--   "Cosim"). It also runs the expected-error (tests/negative) and
--   expected-warning (tests/warning) suites and a flag-combination smoke group.
module Main (main) where

import qualified RWC

import ReWire.Error (runSyntaxError)
import ReWire.Hyle.Parse (parseHyle, parseHyleText)
import ReWire.Pretty (prettyPrint)

import qualified Cosim
import TestUtil (withStderrTo, withStdoutTo, sq)

import Control.Exception (try, finally)
import Control.Monad (unless, when, msum)
import Data.List (isSuffixOf, isInfixOf, stripPrefix, intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Directory (listDirectory, setCurrentDirectory, getCurrentDirectory, doesFileExist)
import System.Environment (getArgs, withArgs)
import System.Exit (exitFailure, ExitCode (..))
import System.FilePath ((</>), (-<.>), takeBaseName, takeDirectory, takeFileName)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process (callCommand)

import qualified Data.Text as T
import Test.Tasty (defaultMain, localOption, testGroup, TestTree)
import Test.Tasty.Runners (NumThreads (..))
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
          | FlagIntegration
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
       , Option []    ["no-check"]     (NoArg FlagNoCheck)                    "Disable verification of output HDL with checker."
       , Option []    ["no-ghc"]       (NoArg FlagNoGhc)                      "Disable running tests through ghc."
       , Option []    ["integration"]  (NoArg FlagIntegration)                "Also run the golden tests in tests/integration (heavyweight examples; off by default)."
       , Option []    ["iverilog"]     (ReqArg FlagIVerilog "command")      $ "Set the command to use for checking generated Verilog with iverilog (default: " <> defaultIVerilog <> ")."
       , Option []    ["verilator"]    (ReqArg FlagVerilator "command")     $ "Set the command to use for checking generated Verilog with verilator (default: " <> defaultVerilator <> ")."

       -- Tasty arguments.
       , Option ['p'] ["pattern"]      (ReqArg FlagP "PATTERN")               "Select only tests which satisfy a pattern or awk expression."
       , Option ['q'] ["quiet"]        (NoArg FlagQ)                          "Do not produce any output; indicate success only by the exit code."
       , Option ['l'] ["list-tests"]   (NoArg FlagL)                          "Do not run the tests; just print their names."
       , Option ['t'] ["timeout"]      (ReqArg FlagT "DURATION")              "Timeout for individual tests (suffixes: ms, s, m, h; default: s)."
       , Option []    ["color"]        (ReqArg FlagColor "never|always|auto") "When to use colored output (default: auto)."
       ]

-- | Build the test tree for one golden test program. There are two kinds of
--   checks: golden tests, which compile/interpret the program and diff each
--   output against a committed golden file (.rwc, .yaml, .sv, .vhdl, .cry); and
--   cosimulation tests (see "Cosim"), which simulate the device through the HDL
--   backends and require them to agree with the rwc interpreter. The
--   .yaml/.vhdl/.cry golden legs run only when that golden exists; the HDL-lint
--   and cosimulation legs are skipped under --no-check.
testCompiler :: [Flag] -> FilePath -> IO [TestTree]
testCompiler flags fn = do
      let ghcTests =
            -- Compile the Haskell source with GHC (the program must be valid
            -- Haskell as well as valid ReWire).
            [ testCase (takeBaseName fn <> " (stack ghc)") (cdTestdir >> callCommand ("stack ghc " <> fn))
            | FlagNoGhc `notElem` flags ]

      let coreTests =
            -- Compile Haskell to the Hyle IR (the .rwc golden) with rwc.
            [ golden "rwc" $ do
                  cdTestdir
                  withArgs (fn : ["--core", "-o", ofile "rwc"] <> extraFlags) RWC.main
            ]

      yamlTests <-
            -- Interpret the Hyle IR and diff against the .yaml golden -- the
            -- reference the cosimulation tests check the HDL backends against.
            -- With a per-test inputs file (<base>.input.yaml), rwc defaults
            -- --cycles to max(10, #inputs), driving the interpreter for one
            -- cycle per listed input (the regenerate_expected_output.sh script
            -- relies on the same default, so the two stay in agreement);
            -- otherwise --interp drives all-zero inputs for the default 10
            -- cycles.
            do
                  let inF = fn -<.> "input.yaml"
                  hasIn <- doesFileExist inF
                  let interpArgs = if hasIn then ["--interpret=" <> takeFileName inF] else ["--interp"]
                  maybeGolden "yaml" $ do
                        cdTestdir
                        withArgs ((fn -<.> "rwc") : ["--from-core"] <> interpArgs <> ["-o", ofile "yaml"] <> extraFlags) RWC.main

      let roundTripTests =
            -- Parse/print round trip on the .rwc golden. The golden is fastPrint
            -- output, so the first parse also covers the compact printer; the
            -- property asserted is that prettyPrint . parse is a fixpoint
            -- (parsing the pretty-printed program and printing it again
            -- reproduces the same text).
            [ testCase (takeBaseName fn <> " (rwc round-trip)") $ do
                  cdTestdir
                  r <- runSyntaxError $ do
                        p1 <- parseHyle $ fn -<.> "rwc"
                        let t1 = prettyPrint p1
                        p2 <- parseHyleText t1 $ fn -<.> "rwc"
                        pure (t1, prettyPrint p2)
                  case r of
                        Left err       -> assertFailure $ "round-trip parse failed: " <> T.unpack (prettyPrint err)
                        Right (t1, t2) -> assertBool "parse . prettyPrint . parse differs from parse" $ t1 == t2
               ]

      let verilogTests =
            -- Compile the Hyle IR to Verilog with rwc.
            [ golden "sv" $ do
                  cdTestdir -- TODO enable extra typechecking
                  withArgs ((fn -<.> "rwc") : ["--from-core", "-o", ofile "sv"] <> extraFlags) RWC.main
            ]
            -- Lint the Verilog with iverilog and verilator (unless --no-check).
            <> [ testCase (takeBaseName fn <> " (" <> iverilog <> ")")  (cdTestdir >> callCommand (iverilog  <> " " <> verilog <> " " <> ofile "sv")) | check ]
            <> [ testCase (takeBaseName fn <> " (" <> verilator <> ")") (cdTestdir >> callCommand (verilator <> " " <> verilog <> " " <> ofile "sv")) | check ]

      vhdlTests <-
            -- Compile the Hyle IR to VHDL with rwc (when a .vhdl golden exists).
            maybeGolden "vhdl" $ do
                  cdTestdir
                  withArgs ((fn -<.> "rwc") : ["--from-core", "--vhdl", "-o", ofile "vhdl"] <> extraFlags) RWC.main

      cryTests <-
            -- Compile the Hyle IR to Cryptol with rwc (when a .cry golden exists).
            maybeGolden "cry" $ do
                  cdTestdir
                  withArgs ((fn -<.> "rwc") : ["--from-core", "--cryptol", "-o", ofile "cry"] <> extraFlags) RWC.main

      cosimTests <-
            -- Cosimulate each backend against the rwc interpreter (see "Cosim"),
            -- one test per backend, unless --no-check.
            if check then Cosim.cosimTests fn else pure []

      pure $ ghcTests <> coreTests <> yamlTests <> roundTripTests <> verilogTests <> vhdlTests <> cryTests <> cosimTests

      where check :: Bool
            check = FlagNoCheck `notElem` flags

            extraFlags :: [String]
            extraFlags = ["-v" | FlagV `elem` flags]

            cdTestdir :: IO ()
            cdTestdir = setCurrentDirectory $ takeDirectory fn

            ofile :: String -> FilePath
            ofile ext = fn -<.> ("out." <> ext)

            iverilog, verilator :: String
            iverilog  = fromMaybe defaultIVerilog  $ msum [ Just (sq c) | FlagIVerilog  c <- flags ]
            verilator = fromMaybe defaultVerilator $ msum [ Just (sq c) | FlagVerilator c <- flags ]

            -- Hand-written extern implementations live next to the test.
            verilog :: FilePath
            verilog = takeDirectory fn </> "verilog" </> "*.sv"

            -- Compare a generated output file against its golden, with a
            -- whitespace-insensitive diff.
            golden :: FilePath -> IO () -> TestTree
            golden ext = goldenVsFileDiff (takeBaseName fn <> " (golden " <> ext <> ")") diff (fn -<.> ext) (ofile ext)

            maybeGolden :: FilePath -> IO () -> IO [TestTree]
            maybeGolden ext io = (\ ex -> [ golden ext io | ex ]) <$> doesFileExist (fn -<.> ext)

            diff :: FilePath -> FilePath -> [String]
            diff ref new = ["diff", "-bu", ref, new]

-- | A test that must fail to compile with rwc. Each test file declares (one or
--   more of) the expected error with a comment line:
--
-- > -- EXPECT-ERROR: <substring of the expected error message>
--
--   The test asserts that rwc exits with failure and that its stderr contains
--   every expected substring. These files are never run through GHC or the HDL
--   checkers.
testNegative :: FilePath -> TestTree
testNegative fn = testCase (takeBaseName fn <> " (expected error)") $ do
      setCurrentDirectory $ takeDirectory fn
      expected <- mapMaybe (stripPrefix "-- EXPECT-ERROR: ") . lines <$> readFile fn
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

-- | A test that must compile successfully while emitting expected warnings,
--   declared with one or more comment lines:
--
-- > -- EXPECT-WARNING: <substring of the expected warning>
--
--   Extra rwc flags may be supplied with a "-- FLAGS: ..." comment line. Each
--   file is compiled three times: with default flags (must succeed, with
--   stderr containing every expected substring), with -Werror (must fail), and
--   with -w (must succeed printing no warnings).
testWarning :: FilePath -> TestTree
testWarning fn = testCase (takeBaseName fn <> " (expected warning)") $ do
      setCurrentDirectory $ takeDirectory fn
      src <- readFile fn
      let expected = mapMaybe (stripPrefix "-- EXPECT-WARNING: ") $ lines src
          flags    = concatMap words $ mapMaybe (stripPrefix "-- FLAGS: ") $ lines src
          run extra ext = let ef = fn -<.> ("out." <> ext) in
                (, ef) <$> withStderrTo ef (try $ withArgs ([fn, "-o", fn -<.> "out.sv"] <> flags <> extra) RWC.main :: IO (Either ExitCode ()))
      when (null expected) $ assertFailure $ "no \"-- EXPECT-WARNING:\" line in " <> fn

      (r1, ef1) <- run [] "warn"
      w1        <- readFile ef1
      case r1 of
            Right () -> mapM_ (\ e ->
                  assertBool ("compilation succeeded, but stderr does not mention " <> show e <> "; stderr:\n" <> w1)
                        $ e `isInfixOf` w1) expected
            Left e   -> assertFailure $ "expected compilation to succeed (with warnings), but rwc exited (" <> show e <> "); stderr:\n" <> w1

      (r2, ef2) <- run ["-Werror"] "werror"
      w2        <- readFile ef2
      case r2 of
            Left (ExitFailure _) -> pure ()
            _                    -> assertFailure $ "expected compilation to fail under -Werror, but it succeeded; stderr:\n" <> w2

      (r3, ef3) <- run ["-w"] "nowarn"
      w3        <- readFile ef3
      case r3 of
            Right () -> assertBool ("expected no warnings under -w; stderr:\n" <> w3) $ not ("Warning" `isInfixOf` w3)
            Left e   -> assertFailure $ "expected compilation to succeed under -w, but rwc exited (" <> show e <> "); stderr:\n" <> w3

-- | Compile fixed test programs with flag combinations the golden tests don't
--   exercise (verbose tracing, pass dumps, alternate signal names, reset/clock
--   variations, interpreter cycle bounds, VHDL target). These only assert that
--   rwc succeeds; outputs are not compared against goldens. Stdout (verbose
--   trace, IR dumps) is redirected to a log file next to the other outputs.
getSmokeTests :: IO TestTree
getSmokeTests = do
      dir <- getDataFileName ("tests" </> "golden")
      pure $ testGroup "flags"
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
                  let fn  = dir </> file
                      out = fn -<.> ("out.smoke-" <> name <> "." <> ext)
                  setCurrentDirectory dir
                  withStdoutTo (fn -<.> ("out.smoke-" <> name <> ".log")) $
                        withArgs ([fn, "-o", out] <> args) RWC.main

-- | Build a test group from every .hs file in tests/<dirName>, using the given
--   per-file test builder.
testsFrom :: FilePath -> (FilePath -> IO [TestTree]) -> IO TestTree
testsFrom dirName build = do
      dir   <- getDataFileName ("tests" </> dirName)
      files <- map (dir </>) . filter (".hs" `isSuffixOf`) <$> listDirectory dir
      testGroup dirName . concat <$> mapM build files

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: rwc-test [OPTION...]" options) >> exitFailure

main :: IO ()
main = do
      (flags, _, errs) <- getOpt Permute options <$> getArgs

      unless (null errs && FlagH `notElem` flags) $ do
            mapM_ (hPutStrLn stderr) errs
            exitUsage

      goldTests  <- testsFrom "golden" (testCompiler flags)
      negTests   <- testsFrom "negative"   (\ f -> pure [testNegative f])
      warnTests  <- testsFrom "warning"    (\ f -> pure [testWarning f])
      smokeTests <- getSmokeTests
      -- The integration directory holds full-program golden tests (same legs as
      -- tests/golden), but they are heavyweight, so they only run under --integration.
      intgTests  <- if FlagIntegration `elem` flags
                          then (: []) <$> testsFrom "integration" (testCompiler flags)
                          else pure []

      -- Every leg runs rwc in-process (withArgs/RWC.main) after cd'ing into the
      -- test's directory, so they all mutate process-global cwd and argv; they
      -- must never run concurrently. We enforce that with NumThreads 1 (serial
      -- execution in tree order) rather than inter-test dependencies, so that
      -- --pattern selects exactly the matching legs instead of dragging in every
      -- predecessor as a dependency. We also restore the working directory on
      -- exit (HPC writes its .tix relative to the final working directory).
      cwd0 <- getCurrentDirectory
      withArgs (concatMap toTastyArg flags)
            (defaultMain $ localOption (NumThreads 1)
                  $ testGroup "Tests" ([goldTests, smokeTests, negTests, warnTests] <> intgTests))
            `finally` setCurrentDirectory cwd0

      where toTastyArg :: Flag -> [String]
            toTastyArg = \ case
                  FlagP p     -> ["-p", p]
                  FlagQ       -> ["-q"]
                  FlagL       -> ["-l"]
                  FlagT t     -> ["-t", t]
                  FlagColor c -> ["--color", c]
                  _           -> []

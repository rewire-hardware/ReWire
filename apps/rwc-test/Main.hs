module Main (main) where

import qualified RWC

import Control.Exception (try, finally)
import Control.Monad (unless, when, msum)
import Data.Bits (xor, shiftL, shiftR)
import Data.List (isSuffixOf, isInfixOf, isPrefixOf, stripPrefix, intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word32)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Data.Char (isHexDigit)
import Numeric (readHex)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Directory (listDirectory, setCurrentDirectory, getCurrentDirectory, doesFileExist, createDirectoryIfMissing, findExecutable)
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
            -- Test: compile Core to VHDL with RWC (when a .vhdl golden exists).
            maybeGolden "vhdl" (do
                  cdTestdir
                  withArgs ((fn -<.> "rwc") : ["--from-core", "--vhdl", "-o", ofile "vhdl"] <> extraFlags) RWC.main)

      cryTests <-
            -- Test: compile Core to Cryptol with RWC (when a .cry golden exists).
            maybeGolden "cry" (do
                  cdTestdir
                  withArgs ((fn -<.> "rwc") : ["--from-core", "--cryptol", "-o", ofile "cry"] <> extraFlags) RWC.main)

      cosimTests <-
            -- Test: cosimulation. Drive the generated Verilog (iverilog/vvp)
            -- and VHDL (ghdl) with identical pseudorandom stimulus and check
            -- that the two simulations produce the same outputs, cycle by
            -- cycle.
            if FlagNoCheck `elem` flags then pure [] else do
                  ex <- doesFileExist $ fn -<.> "vhdl"
                  pure [ testCase (takeBaseName fn <> " (cosim iverilog/ghdl)") (cdTestdir >> runCosim fn) | ex ]

      pure $ ghcTests <> coreTests <> verilogTests <> vhdlTests <> cryTests <> cosimTests

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

-- | A test that must compile successfully while emitting expected warnings,
--   declared with one or more comment lines:
--
-- > -- EXPECT-WARNING: <substring of the expected warning>
--
--   Extra rwc flags may be supplied with a "-- FLAGS: ..." comment line.
--   Each file is compiled three times: with default flags (must succeed,
--   with stderr containing every expected substring), with -Werror (must
--   fail), and with -w (must succeed printing no warnings).
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

-- | Number of cycles to cosimulate.
cosimCycles :: Int
cosimCycles = 20

-- | Three-way agreement check: compile the design with rwc --testbench for
--   both backends, drive both testbenches with the same pseudorandom inputs
--   file, and require that the iverilog/vvp trace, the ghdl trace, and the
--   Mantle interpreter (--interpret) output all agree, cycle by cycle. Assumes
--   the working directory is the test directory and the .out.sv output has
--   been generated. Skipped for degenerate devices with no outputs.
runCosim :: FilePath -> IO ()
runCosim fn = do
      src <- readFile $ ofl "sv"
      let (ins, outs) = parsePorts src
          stim        = stimulus (takeBaseName fn) $ dataIns ins
      unless (null outs) $ do
            writeFile inputsF $ inputsYaml stim
            rwc ["--testbench=" <> inputsF, "-o", ofl "cosim.sv"]
            rwc ["--vhdl", "--testbench=" <> inputsF, "-o", ofl "cosim.vhdl"]
            -- The interpreter cannot evaluate externs, so the check degrades
            -- to two-way (simulator vs. simulator) when it fails.
            iok <- withStderrTo (ofl "cosim.interp.err")
                  (try (rwc ["--interpret=" <> inputsF, "-o", ofl "cosim.interp.yaml"]) :: IO (Either ExitCode ()))
            callCommand $ unwords ["iverilog -g2012 -o", ofl "cosim.vvp", ofl "cosim_tb.sv", ofl "cosim.sv", "verilog/*.sv"]
            callCommand $ unwords ["vvp", ofl "cosim.vvp", ">", ofl "cosim.vtrace"]
            callCommand $ unwords ["rm -rf", wk] -- a stale workdir makes ghdl bind old units
            createDirectoryIfMissing False wk
            callCommand $ unwords ["ghdl -a --std=08 --workdir=" <> wk, ofl "cosim.vhdl", "vhdl/*.vhdl", ofl "cosim_tb.vhdl"]
            callCommand $ unwords ["ghdl -e --std=08 --workdir=" <> wk, "-o", wk </> "tbexe", "tb"]
            callCommand $ unwords [wk </> "tbexe", "--ieee-asserts=disable", ">", ofl "cosim.htrace"]
            tv <- outTrace <$> readFile (ofl "cosim.vtrace")
            th <- outTrace <$> readFile (ofl "cosim.htrace")
            ti <- either (const $ pure tv) (const $ outTrace <$> readFile (ofl "cosim.interp.yaml")) iok
            assertBool (  "cosimulation traces differ (or have the wrong length):"
                       <> "\niverilog: " <> show tv
                       <> "\nghdl:     " <> show th
                       <> "\ninterp:   " <> show ti)
                  $ tv == th && tv == ti && length tv == cosimCycles * length outs
            -- Fourth leg: evaluate the Cryptol backend's rw_device on the same
            -- stimulus with the cryptol interpreter and compare against the
            -- interpreter trace. Externs become uninterpreted parameters in
            -- Cryptol, so this is skipped whenever the interpreter leg failed
            -- (and when cryptol isn't installed).
            mcry <- findExecutable "cryptol"
            case (mcry, iok) of
                  (Just _, Right ()) -> do
                        rwc ["--cryptol", "-o", ofl "cosim.cry"]
                        callCommand $ unwords
                              [ "cryptol", ofl "cosim.cry"
                              , "-c", "':set base=16'"
                              , "-c", "'" <> cryDevice (dataIns ins) stim <> "'"
                              , ">", ofl "cosim.ctrace" ]
                        tc <- hexWords <$> readFile (ofl "cosim.ctrace")
                        let expect = map (concatWord (map snd outs) . map snd) $ chunksOf (length outs) ti
                        assertBool (  "cryptol/interpreter cosimulation traces differ:"
                                   <> "\ncryptol: " <> show tc
                                   <> "\ninterp:  " <> show expect)
                              $ tc == expect
                  _ -> pure ()
      where ofl :: String -> FilePath
            ofl ext = fn -<.> ("out." <> ext)

            inputsF, wk :: FilePath
            inputsF = ofl "cosim.inputs.yaml"
            wk      = ofl "ghdlwork"

            rwc :: [String] -> IO ()
            rwc args = withArgs ((fn -<.> "rwc") : "--from-core" : "--cycles" : show cosimCycles : args) RWC.main

            dataIns :: [(String, Int)] -> [(String, Int)]
            dataIns = filter ((`notElem` ["clk", "rst"]) . fst)

-- | Output records parsed from a simulation trace or interpreter output:
--   lines of the form "- name: '0xHEX'" (or indented continuation lines);
--   anything else (simulator noise) is ignored. Comparing parsed values makes
--   the check robust to hex formatting differences (e.g., ghdl pads, the
--   interpreter does not).
outTrace :: String -> [(String, Integer)]
outTrace = mapMaybe entry . lines
      where entry :: String -> Maybe (String, Integer)
            entry l = case words l of
                  ["-", n, v] | ":" `isSuffixOf` n -> (init n, ) <$> hexVal v
                  [n, v]      | ":" `isSuffixOf` n -> (init n, ) <$> hexVal v
                  _                                -> Nothing

            hexVal :: String -> Maybe Integer
            hexVal v = case dropWhile (/= 'x') v of
                  'x' : rest | [(x, _)] <- readHex $ takeWhile isHexDigit rest -> Just x
                  _                                                           -> Nothing

-- | Deterministic pseudorandom stimulus: one (name, value) pair per data
--   input per cycle, with the inputs in port order.
stimulus :: String -> [(String, Int)] -> [[(String, Integer)]]
stimulus nm ins = go (seed0 nm) cosimCycles
      where go :: Word32 -> Int -> [[(String, Integer)]]
            go _ 0 = []
            go s n = let (s', vs) = foldl draw (s, []) ins in vs : go s' (n - 1)

            draw :: (Word32, [(String, Integer)]) -> (String, Int) -> (Word32, [(String, Integer)])
            draw (s, acc) (n, w) =
                  let chunks      = (w + 31) `div` 32
                      (s', v)     = foldl (\ (sAcc, vAcc) _ -> let sN = xorshift32 sAcc in (sN, vAcc * (2 ^ (32 :: Integer)) + toInteger sN))
                                          (s, 0) [1 .. chunks]
                  in (s', acc <> [(n, v `mod` (2 ^ toInteger w))])

            seed0 :: String -> Word32
            seed0 = foldl (\ h c -> h * 31 + fromIntegral (fromEnum c)) 0x12345678

            xorshift32 :: Word32 -> Word32
            xorshift32 x0 = let x1 = x0 `xor` (x0 `shiftL` 13)
                                x2 = x1 `xor` (x1 `shiftR` 17)
                            in       x2 `xor` (x2 `shiftL` 5)

-- | Renders stimulus as an interp-style inputs file (decimal values).
inputsYaml :: [[(String, Integer)]] -> String
inputsYaml stim
      | all null stim = "[]\n"
      | otherwise     = unlines $ concatMap (zipWith fmt ("- " : repeat "  ")) stim
      where fmt :: String -> (String, Integer) -> String
            fmt pre (n, v) = pre <> n <> ": " <> show v

-- | A cryptol expression evaluating the generated device on the stimulus: one
--   input word per cycle, the data inputs concatenated MSB-first in port
--   order (mirroring how the Mantle interpreter and the testbenches feed them).
cryDevice :: [(String, Int)] -> [[(String, Integer)]] -> String
cryDevice ins stim = "rw_device ([" <> intercalate ", " (map cyc stim) <> "] : "
                  <> "[" <> show (length stim) <> "][" <> show (sum $ map snd ins) <> "])"
      where cyc :: [(String, Integer)] -> String
            cyc = show . concatWord (map snd ins) . map snd

-- | Concatenate per-wire values (given the wire widths) into one word,
--   MSB-first.
concatWord :: [Int] -> [Integer] -> Integer
concatWord ws vs = foldl (\ acc (w, v) -> acc `shiftL` w + v) 0 $ zip ws vs

-- | All hex literals (0x...) in a cryptol trace, in order: the elements of
--   the evaluated output sequence (loader messages and warnings never
--   contain hex literals).
hexWords :: String -> [Integer]
hexWords = \ case
      '0' : 'x' : s | (h, s') <- span isHexDigit s
                    , [(x, _)] <- readHex h -> x : hexWords s'
      _ : s                                 -> hexWords s
      []                                    -> []

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = \ case
      [] -> []
      xs -> take n xs : chunksOf n (drop n xs)

-- | The port list of the generated top_level Verilog module: input and output
--   names with bit widths.
parsePorts :: String -> ([(String, Int)], [(String, Int)])
parsePorts src = go (words $ map unPunct header) ([], [])
      where header = takeWhile (/= ';') $ dropAfter "module top_level" src

            unPunct :: Char -> Char
            unPunct c = if c `elem` ("(),." :: String) then ' ' else c

            go :: [String] -> ([(String, Int)], [(String, Int)]) -> ([(String, Int)], [(String, Int)])
            go ("input"  : "logic" : dim : n : rest) (is, os) = go rest (is <> [(n, dimW dim)], os)
            go ("output" : "logic" : dim : n : rest) (is, os) = go rest (is, os <> [(n, dimW dim)])
            go (_ : rest) acc                                 = go rest acc
            go [] acc                                         = acc

            dimW :: String -> Int
            dimW d = read (takeWhile (/= ':') $ drop 1 d) + 1

            dropAfter :: String -> String -> String
            dropAfter pat s | pat `isPrefixOf` s = drop (length pat) s
            dropAfter _ []                       = []
            dropAfter pat (_ : s)                = dropAfter pat s

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

getWarnTests :: FilePath -> IO TestTree
getWarnTests dirName = do
      dir   <- getDataFileName ("tests" </> dirName)
      files <- map (dir </>) . filter (".hs" `isSuffixOf`) <$> listDirectory dir
      pure $ sequentialTestGroup dirName AllFinish $ map testWarning files

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
      warnTests  <- getWarnTests "warning"
      smokeTests <- getSmokeTests

      -- The tests change the working directory, so restore it on exit (HPC
      -- writes its .tix relative to the final working directory).
      cwd0 <- getCurrentDirectory
      withArgs (injectTastyArgs flags) (defaultMain $ sequentialTestGroup "Tests" AllFinish $ tests <> [smokeTests, negTests, warnTests])
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

module Main (main) where

import qualified RWC

import Control.Exception (try, finally)
import Control.Monad (unless, when, msum)
import Data.Bits (xor)
import Data.List (isSuffixOf, isInfixOf, isPrefixOf, stripPrefix, intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word32)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Numeric (showHex)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Directory (listDirectory, setCurrentDirectory, getCurrentDirectory, doesFileExist, createDirectoryIfMissing)
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

      cosimTests <-
            -- Test: cosimulation. Drive the generated Verilog (iverilog/vvp)
            -- and VHDL (ghdl) with identical pseudorandom stimulus and check
            -- that the two simulations produce the same outputs, cycle by
            -- cycle.
            if FlagNoCheck `elem` flags then pure [] else do
                  ex <- doesFileExist $ fn -<.> "vhdl"
                  pure [ testCase (takeBaseName fn <> " (cosim iverilog/ghdl)") (cdTestdir >> runCosim fn) | ex ]

      pure $ ghcTests <> coreTests <> verilogTests <> vhdlTests <> cosimTests

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

-- | Number of cycles to cosimulate.
cosimCycles :: Int
cosimCycles = 20

-- | Drive the generated Verilog and VHDL with identical pseudorandom inputs
--   and check that the simulators (iverilog/vvp and ghdl) produce identical
--   output traces. Assumes the working directory is the test directory and
--   that the .out.sv and .out.vhdl outputs have been generated. Skipped for
--   degenerate devices with no outputs.
runCosim :: FilePath -> IO ()
runCosim fn = do
      src <- readFile $ ofl "sv"
      let (ins, outs) = parsePorts src
      unless (null outs) $ do
            writeFile tbsv   $ verilogTb ins outs
            writeFile tbvhdl $ vhdlTb ins outs
            callCommand $ unwords ["iverilog -g2012 -o", vvpf, tbsv, ofl "sv", "verilog/*.sv"]
            callCommand $ unwords ["vvp", vvpf, ">", ofl "cosim.vtrace"]
            createDirectoryIfMissing False wk
            callCommand $ unwords ["ghdl -a --std=08 --workdir=" <> wk, ofl "vhdl", "vhdl/*.vhdl", tbvhdl]
            callCommand $ unwords ["ghdl -e --std=08 --workdir=" <> wk, "-o", wk </> "tbexe", "tb"]
            callCommand $ unwords [wk </> "tbexe", "--ieee-asserts=disable", ">", ofl "cosim.htrace"]
            tv <- bitLines <$> readFile (ofl "cosim.vtrace")
            th <- bitLines <$> readFile (ofl "cosim.htrace")
            assertBool (  "cosimulation traces differ (or have the wrong length):"
                       <> "\niverilog: " <> show tv
                       <> "\nghdl:     " <> show th)
                  $ tv == th && length tv == cosimCycles
      where ofl :: String -> FilePath
            ofl ext = fn -<.> ("out." <> ext)

            tbsv, tbvhdl, vvpf, wk :: FilePath
            tbsv   = ofl "cosim.tb.sv"
            tbvhdl = ofl "cosim.tb.vhdl"
            vvpf   = ofl "cosim.vvp"
            wk     = ofl "ghdlwork"

            bitLines :: String -> [String]
            bitLines = filter (not . null) . map (filter (/= ' ')) . filter (all (`elem` ("01xzuXZU " :: String))) . lines

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

-- | Stimulus shared by the two testbenches: a per-input xorshift32 PRNG,
--   stepped once per 32-bit chunk of the input per cycle.
cosimSeed :: Int -> Word32
cosimSeed k = 0x12345678 `xor` (fromIntegral k * 0x9E3779B9)

hex8 :: Word32 -> String
hex8 w = let h = showHex w "" in replicate (8 - length h) '0' <> h

cosimDataIns :: [(String, Int)] -> [(String, Int)]
cosimDataIns = filter ((`notElem` ["clk", "rst"]) . fst)

cosimChunks :: Int -> Int
cosimChunks w = (w + 31) `div` 32

verilogTb :: [(String, Int)] -> [(String, Int)] -> String
verilogTb ins outs = unlines $
      [ "module tb;" ]
      <> map (\ (n, w) -> "  logic [" <> show (w - 1) <> ":0] " <> n <> ";") ins
      <> map (\ (n, w) -> "  wire [" <> show (w - 1) <> ":0] " <> n <> ";") outs
      <> zipWith (\ k _ -> "  reg [31:0] s" <> show k <> " = 32'h" <> hex8 (cosimSeed k) <> ";") [0 :: Int ..] dataIns
      <> [ "  reg [" <> show (maxch * 32 - 1) <> ":0] acc;"
         , "  integer c;"
         , "  top_level dut (" <> intercalate ", " (map fst $ ins <> outs) <> ");"
         , "  function [31:0] xs (input [31:0] x);"
         , "    begin"
         , "      xs = x ^ (x << 13);"
         , "      xs = xs ^ (xs >> 17);"
         , "      xs = xs ^ (xs << 5);"
         , "    end"
         , "  endfunction"
         , "  task drv;"
         , "    begin"
         ]
      <> driveLines
      <> [ "    end"
         , "  endtask"
         , "  initial begin"
         ]
      <> body
      <> [ "    $finish;"
         , "  end"
         , "endmodule"
         ]
      where dataIns = cosimDataIns ins
            clocked = "clk" `elem` map fst ins
            maxch   = maximum $ 1 : map (cosimChunks . snd) dataIns

            driveLines | null dataIns = ["      ;"]
                       | otherwise    = concat $ zipWith drive [0 :: Int ..] dataIns

            drive k (n, w) = ["      acc = 0;"]
                  <> replicate (cosimChunks w) ("      s" <> show k <> " = xs(s" <> show k <> "); acc = (acc << 32) | s" <> show k <> ";")
                  <> ["      " <> n <> " = acc[" <> show (w - 1) <> ":0];"]

            disp = "$display(\"%b\", {" <> intercalate ", " (map fst outs) <> "});"

            body | clocked =
                       [ "    clk = 0; rst = 1;"
                       , "    drv; #5 clk = 1; #5 clk = 0;"
                       , "    drv; #5 clk = 1; #5 clk = 0;"
                       , "    rst = 0;"
                       , "    for (c = 0; c < " <> show cosimCycles <> "; c = c + 1) begin"
                       , "      drv;"
                       , "      #4 " <> disp
                       , "      #1 clk = 1; #5 clk = 0;"
                       , "    end"
                       ]
                 | otherwise =
                       [ "    for (c = 0; c < " <> show cosimCycles <> "; c = c + 1) begin"
                       , "      drv;"
                       , "      #5 " <> disp
                       , "      #5;"
                       , "    end"
                       ]

vhdlTb :: [(String, Int)] -> [(String, Int)] -> String
vhdlTb ins outs = unlines $
      [ "library ieee;"
      , "use ieee.std_logic_1164.all;"
      , "use ieee.numeric_std.all;"
      , "use std.textio.all;"
      , ""
      , "entity tb is"
      , "end entity;"
      , ""
      , "architecture sim of tb is"
      ]
      <> map (\ (n, w) -> "  signal " <> vhdlName n <> " : std_logic_vector (" <> show (w - 1) <> " downto 0);") (ins <> outs)
      <> [ "  function xs (x : unsigned(31 downto 0)) return unsigned is"
         , "    variable r : unsigned(31 downto 0);"
         , "  begin"
         , "    r := x xor shift_left(x, 13);"
         , "    r := r xor shift_right(r, 17);"
         , "    r := r xor shift_left(r, 5);"
         , "    return r;"
         , "  end function;"
         , "begin"
         , "  dut : entity work.top_level port map (" <> intercalate ", " (map (vhdlName . fst) $ ins <> outs) <> ");"
         , "  stim : process"
         ]
      <> zipWith (\ k _ -> "    variable s" <> show k <> " : unsigned(31 downto 0) := x\"" <> hex8 (cosimSeed k) <> "\";") [0 :: Int ..] dataIns
      <> [ "    variable acc : unsigned(" <> show (maxch * 32 - 1) <> " downto 0);"
         , "    variable l : line;"
         , "    procedure drv is"
         , "    begin"
         ]
      <> driveLines
      <> [ "    end procedure;"
         , "  begin"
         ]
      <> body
      <> [ "    std.env.finish;"
         , "  end process;"
         , "end architecture;"
         ]
      where dataIns = cosimDataIns ins
            clocked = "clk" `elem` map fst ins
            maxch   = maximum $ 1 : map (cosimChunks . snd) dataIns

            driveLines | null dataIns = ["      null;"]
                       | otherwise    = concat $ zipWith drive [0 :: Int ..] dataIns

            drive k (n, w) = ["      acc := (others => '0');"]
                  <> replicate (cosimChunks w) ("      s" <> show k <> " := xs(s" <> show k <> "); acc := shift_left(acc, 32) or resize(s" <> show k <> ", " <> show (maxch * 32) <> ");")
                  <> ["      " <> vhdlName n <> " <= std_logic_vector(resize(acc, " <> show w <> "));"]

            disp = "write(l, " <> intercalate " & " (map (\ (n, _) -> "to_string(" <> vhdlName n <> ")") outs) <> "); writeline(output, l);"

            body | clocked =
                       [ "    clk <= \"0\"; rst <= \"1\";"
                       , "    drv; wait for 5 ns; clk <= \"1\"; wait for 5 ns; clk <= \"0\";"
                       , "    drv; wait for 5 ns; clk <= \"1\"; wait for 5 ns; clk <= \"0\";"
                       , "    rst <= \"0\";"
                       , "    for c in 0 to " <> show (cosimCycles - 1) <> " loop"
                       , "      drv;"
                       , "      wait for 4 ns;"
                       , "      " <> disp
                       , "      wait for 1 ns; clk <= \"1\"; wait for 5 ns; clk <= \"0\";"
                       , "    end loop;"
                       ]
                 | otherwise =
                       [ "    for c in 0 to " <> show (cosimCycles - 1) <> " loop"
                       , "      drv;"
                       , "      wait for 5 ns;"
                       , "      " <> disp
                       , "      wait for 5 ns;"
                       , "    end loop;"
                       ]

-- | VHDL identifiers: names that aren't valid (lowercase) basic identifiers
--   are printed as extended identifiers, matching ReWire.VHDL.Syntax.
vhdlName :: String -> String
vhdlName n | basicOk   = n
           | otherwise = "\\" <> n <> "\\"
      where basicOk = case n of
                  c : cs -> c `elem` lc && all (`elem` ('_' : lc <> ['0' .. '9'])) cs
                         && not ("__" `isInfixOf` n) && not ("_" `isSuffixOf` n) && not ("rw_" `isPrefixOf` n)
                  _      -> False
            lc = ['a' .. 'z']

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

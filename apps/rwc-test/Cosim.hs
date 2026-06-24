-- | Cross-backend cosimulation: simulate a compiled golden-test device through
--   each available HDL/Cryptol backend and require it to agree, cycle by cycle,
--   with the Hyle interpreter (the reference). This is what keeps the Verilog,
--   VHDL, Cryptol, and interpreter backends behaviorally aligned. The
--   interpreter is in turn tied to its committed .yaml golden by the separate
--   "golden yaml" test, so agreement here anchors every backend to that golden.
--
--   The inputs and output traces are read and written with the same `yaml`
--   package rewire-core uses to read inputs (--interpret/--testbench) and write
--   the .yaml golden: inputs are per-cycle name->number maps, outputs are
--   per-cycle name->hex-string maps. A per-cycle map (rather than a positional
--   list) makes trace comparison robust to wire ordering, matching the
--   interpreter's `Outs = HashMap Name BV`.
module Cosim (cosimTests) where

import qualified RWC

import TestUtil (withStderrTo)

import Control.Monad (unless)
import Data.Bits (xor, shiftL, shiftR)
import Data.Char (isHexDigit)
import Data.HashMap.Strict (HashMap)
import Data.List (isPrefixOf, isSuffixOf, intercalate)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import Numeric (readHex)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, setCurrentDirectory)
import System.Environment (withArgs)
import System.FilePath ((-<.>), (</>), takeBaseName, takeDirectory)
import System.Process (callCommand)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, assertBool)

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
import qualified Data.Yaml           as Yaml

-- | A backend the rwc interpreter is cosimulated against.
data Backend = IVerilog | Ghdl | Cryptol

-- | A simulation/interpreter output: one wire-name -> value map per cycle.
type Trace = [HashMap Text Integer]

-- | Number of cycles to cosimulate when a test has no inputs file.
cosimCycles :: Int
cosimCycles = 20

-- | The cosimulation tests for one golden test program. There is one test per
--   backend, named "(cosim rwc/<tool>)", each independently gated on its tool
--   being installed and (for ghdl/cryptol) its golden file existing -- so e.g.
--   the cryptol comparison still runs when ghdl is not installed. Every backend
--   is driven by the same stimulus: the per-test <base>.input.yaml when
--   present, otherwise deterministic pseudorandom inputs.
--
--   A device whose interpreter (.yaml) golden is absent is an extern the
--   interpreter cannot evaluate; for those a single "(cosim iverilog/ghdl)"
--   test cross-validates the two simulators instead (cryptol there is a
--   parameterized module that cannot be evaluated standalone). Devices with no
--   outputs are skipped.
--
--   The committed <base>.sv golden (same ports as the generated <base>.out.sv)
--   is read here only to decide which tests to create; the test actions run in
--   the test directory and use the freshly generated outputs.
cosimTests :: FilePath -> IO [TestTree]
cosimTests fn = do
      -- The committed .sv golden gives the port list (nothing to cosimulate
      -- without it); a device with no outputs is skipped below.
      hasSv         <- doesFileExist (fn -<.> "sv")
      (_, outs)     <- if hasSv then parsePorts <$> readFile (fn -<.> "sv") else pure ([], [])
      interpretable <- doesFileExist (fn -<.> "yaml")
      haveVhdl      <- doesFileExist (fn -<.> "vhdl")
      haveCry       <- doesFileExist (fn -<.> "cry")
      haveIVerilog  <- installed "iverilog"
      haveGhdl      <- installed "ghdl"
      haveCryptol   <- installed "cryptol"
      pure $ if null outs then [] else
            if interpretable
                  then concat
                        [ [ cosim "rwc/iverilog" (interpVs IVerilog) | haveIVerilog ]
                        , [ cosim "rwc/ghdl"     (interpVs Ghdl)     | haveGhdl    && haveVhdl ]
                        , [ cosim "rwc/cryptol"  (interpVs Cryptol)  | haveCryptol && haveCry  ]
                        ]
                  else [ cosim "iverilog/ghdl" simsAgree | haveIVerilog && haveGhdl && haveVhdl ]

      where cosim :: String -> IO () -> TestTree
            cosim pair act = testCase (takeBaseName fn <> " (cosim " <> pair <> ")")
                                      (setCurrentDirectory (takeDirectory fn) >> act)

            installed :: String -> IO Bool
            installed = fmap isJust . findExecutable

            ofl :: String -> FilePath
            ofl ext = fn -<.> ("out." <> ext)

            -- Recompile the device for cosimulation (--from-core from the .rwc
            -- golden), for the given number of cycles.
            rwc :: Int -> [String] -> IO ()
            rwc ncyc args = withArgs ((fn -<.> "rwc") : "--from-core" : "--cycles" : show ncyc : args) RWC.main

            -- Stimulus fed to every backend, and the cycle count: the per-test
            -- inputs file if present (read with the yaml package, exactly as
            -- rwc itself reads it), else generated pseudorandom inputs written
            -- back out the same way.
            stimulusFor :: [(String, Int)] -> IO (FilePath, [[(String, Integer)]], Int)
            stimulusFor ins = doesFileExist providedF >>= \ case
                  True  -> do cs <- either (error . Yaml.prettyPrintParseException) (map order) <$> Yaml.decodeFileEither providedF
                              pure (providedF, cs, length cs)
                  False -> do let s = stimulus (takeBaseName fn) ins
                              Yaml.encodeFile (ofl "cosim.input.yaml") (map toCycle s)
                              pure (ofl "cosim.input.yaml", s, cosimCycles)
                  where providedF = fn -<.> "input.yaml"

                        -- The decoded per-cycle map in port order (for cryptol),
                        -- absent wires zero -- matching how the loader pads.
                        order :: HashMap Text Integer -> [(String, Integer)]
                        order m = [ (n, Map.lookupDefault 0 (T.pack n) m) | (n, _) <- ins ]

                        toCycle :: [(String, Integer)] -> HashMap Text Integer
                        toCycle c = Map.fromList [ (T.pack n, v) | (n, v) <- c ]

            -- The reference output trace from the Hyle interpreter.
            interpTrace :: FilePath -> Int -> IO Trace
            interpTrace inputsF ncyc = do
                  withStderrTo (ofl "cosim.interp.err") $ rwc ncyc ["--interpret=" <> inputsF, "-o", ofl "cosim.interp.yaml"]
                  outTrace <$> readFile (ofl "cosim.interp.yaml")

            iverilogTrace :: FilePath -> Int -> IO Trace
            iverilogTrace inputsF ncyc = do
                  rwc ncyc ["--testbench=" <> inputsF, "-o", ofl "cosim.sv"]
                  callCommand $ unwords ["iverilog -g2012 -o", ofl "cosim.vvp", ofl "cosim_tb.sv", ofl "cosim.sv", "verilog/*.sv"]
                  callCommand $ unwords ["vvp", ofl "cosim.vvp", ">", ofl "cosim.vtrace"]
                  outTrace <$> readFile (ofl "cosim.vtrace")

            ghdlTrace :: FilePath -> Int -> IO Trace
            ghdlTrace inputsF ncyc = do
                  rwc ncyc ["--vhdl", "--testbench=" <> inputsF, "-o", ofl "cosim.vhdl"]
                  callCommand $ unwords ["rm -rf", wk] -- a stale workdir makes ghdl bind old units
                  createDirectoryIfMissing False wk
                  callCommand $ unwords ["ghdl -a --std=08 --workdir=" <> wk, ofl "cosim.vhdl", "vhdl/*.vhdl", ofl "cosim_tb.vhdl"]
                  callCommand $ unwords ["ghdl -e --std=08 --workdir=" <> wk, "-o", wk </> "tbexe", "tb"]
                  callCommand $ unwords [wk </> "tbexe", "--ieee-asserts=disable", ">", ofl "cosim.htrace"]
                  outTrace <$> readFile (ofl "cosim.htrace")
                  where wk = ofl "ghdlwork"

            -- Compare one backend against the interpreter, cycle by cycle.
            interpVs :: Backend -> IO ()
            interpVs backend = do
                  (ins, outs) <- parsePorts <$> readFile (ofl "sv")
                  (inputsF, stim, ncyc) <- stimulusFor (dataIns ins)
                  ti <- interpTrace inputsF ncyc
                  case backend of
                        IVerilog -> do
                              tv <- iverilogTrace inputsF ncyc
                              assertBool (mismatch "iverilog" tv "interpreter" ti)
                                    $ tv == ti && length ti == ncyc
                        Ghdl -> do
                              th <- ghdlTrace inputsF ncyc
                              assertBool (mismatch "ghdl" th "interpreter" ti)
                                    $ th == ti && length ti == ncyc
                        Cryptol -> do
                              rwc ncyc ["--cryptol", "-o", ofl "cosim.cry"]
                              -- A non-interpretable extern would make this a
                              -- parameterized module; but those have no .yaml
                              -- golden and so never reach this leg. Guard anyway.
                              parameterized <- any (isPrefixOf "parameter") . lines <$> readFile (ofl "cosim.cry")
                              unless parameterized $ do
                                    callCommand $ unwords
                                          [ "cryptol", ofl "cosim.cry"
                                          , "-c", "':set base=16'"
                                          , "-c", "'" <> cryDevice (dataIns ins) stim <> "'"
                                          , ">", ofl "cosim.ctrace" ]
                                    tc <- hexWords <$> readFile (ofl "cosim.ctrace")
                                    let expect = map (concatWord (map snd outs) . portOrder outs) ti
                                    assertBool (  "cryptol/interpreter cosimulation traces differ:"
                                               <> "\ncryptol:     " <> show tc
                                               <> "\ninterpreter: " <> show expect)
                                          $ tc == expect

            -- For an extern device the interpreter can't evaluate: require the
            -- two simulators to agree with each other.
            simsAgree :: IO ()
            simsAgree = do
                  (ins, _) <- parsePorts <$> readFile (ofl "sv")
                  (inputsF, _, ncyc) <- stimulusFor (dataIns ins)
                  tv <- iverilogTrace inputsF ncyc
                  th <- ghdlTrace inputsF ncyc
                  assertBool (mismatch "iverilog" tv "ghdl" th)
                        $ tv == th && length tv == ncyc

            dataIns :: [(String, Int)] -> [(String, Int)]
            dataIns = filter ((`notElem` ["clk", "rst"]) . fst)

            mismatch :: String -> Trace -> String -> Trace -> String
            mismatch la a lb b = "cosimulation traces differ (or have the wrong length):\n"
                              <> la <> ": " <> show a <> "\n" <> lb <> ": " <> show b

-- | A cycle's output values in output-port order (absent wires zero), for the
--   positional cryptol comparison.
portOrder :: [(String, Int)] -> HashMap Text Integer -> [Integer]
portOrder outs m = [ Map.lookupDefault 0 (T.pack n) m | (n, _) <- outs ]

-- | Parse a trace (the interpreter's .yaml, or a simulator's stdout) into
--   per-cycle output maps. Simulators wrap the YAML in banner/$finish noise, so
--   only the list lines ("- name: 'hex'" and indented continuations) are kept
--   before decoding; the kept lines are a well-formed YAML list. Values are
--   read as their hex strings (e.g. ghdl pads/upper-cases, the interpreter does
--   not) and parsed to integers, so comparison is formatting-insensitive.
outTrace :: String -> Trace
outTrace s = case Yaml.decodeEither' (encodeUtf8 $ T.pack $ unlines $ filter traceLine $ lines s) of
      Right cycles -> map (fmap parseHex) (cycles :: [HashMap Text Text])
      Left _       -> []
      where traceLine :: String -> Bool
            traceLine l = case words l of
                  ["-", n, _] -> ":" `isSuffixOf` n
                  [n, _]      -> ":" `isSuffixOf` n
                  _           -> False

            parseHex :: Text -> Integer
            parseHex t = case readHex $ drop 1 $ dropWhile (/= 'x') $ T.unpack t of
                  [(x, _)] -> x
                  _        -> 0

-- | Deterministic pseudorandom stimulus: one (name, value) pair per data input
--   per cycle, with the inputs in port order.
stimulus :: String -> [(String, Int)] -> [[(String, Integer)]]
stimulus nm ins = go (seed0 nm) cosimCycles
      where go :: Word32 -> Int -> [[(String, Integer)]]
            go _ 0 = []
            go s n = let (s', vs) = foldl draw (s, []) ins in vs : go s' (n - 1)

            draw :: (Word32, [(String, Integer)]) -> (String, Int) -> (Word32, [(String, Integer)])
            draw (s, acc) (n, w) =
                  let chunks  = (w + 31) `div` 32
                      (s', v) = foldl (\ (sAcc, vAcc) _ -> let sN = xorshift32 sAcc in (sN, vAcc * (2 ^ (32 :: Integer)) + toInteger sN))
                                      (s, 0) [1 .. chunks]
                  in (s', acc <> [(n, v `mod` (2 ^ toInteger w))])

            seed0 :: String -> Word32
            seed0 = foldl (\ h c -> h * 31 + fromIntegral (fromEnum c)) 0x12345678

            xorshift32 :: Word32 -> Word32
            xorshift32 x0 = let x1 = x0 `xor` (x0 `shiftL` 13)
                                x2 = x1 `xor` (x1 `shiftR` 17)
                            in       x2 `xor` (x2 `shiftL` 5)

-- | A cryptol expression evaluating the generated device on the stimulus: one
--   input word per cycle, the data inputs concatenated MSB-first in port order
--   (mirroring how the Hyle interpreter and the testbenches feed them).
cryDevice :: [(String, Int)] -> [[(String, Integer)]] -> String
cryDevice ins stim = "rw_device ([" <> intercalate ", " (map cyc stim) <> "] : "
                  <> "[" <> show (length stim) <> "][" <> show (sum $ map snd ins) <> "])"
      where cyc :: [(String, Integer)] -> String
            cyc = show . concatWord (map snd ins) . map snd

-- | Concatenate per-wire values (given the wire widths) into one word,
--   MSB-first. Each value is truncated to its field width, mirroring how the
--   simulators and interpreter drive an over-wide value onto a narrower wire
--   (a provided inputs file may carry such values deliberately).
concatWord :: [Int] -> [Integer] -> Integer
concatWord ws vs = foldl (\ acc (w, v) -> (acc `shiftL` w) + (v `mod` (2 ^ w))) 0 $ zip ws vs

-- | All hex literals (0x...) in a cryptol trace, in order: the elements of the
--   evaluated output sequence (loader messages and warnings never contain hex
--   literals).
hexWords :: String -> [Integer]
hexWords = \ case
      '0' : 'x' : s | (h, s') <- span isHexDigit s
                    , [(x, _)] <- readHex h -> x : hexWords s'
      _ : s                                 -> hexWords s
      []                                    -> []

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

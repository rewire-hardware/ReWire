-- | Cross-backend cosimulation: drive a compiled regression device through
--   every available backend with one shared stimulus and require their output
--   traces to agree, cycle by cycle. This is what keeps the Verilog, VHDL,
--   Cryptol, and interpreter backends behaviorally aligned.
module Cosim (runCosim) where

import qualified RWC

import TestUtil (withStderrTo)

import Control.Exception (try)
import Control.Monad (unless)
import Data.Bits (xor, shiftL, shiftR)
import Data.Char (isHexDigit)
import Data.List (isPrefixOf, isSuffixOf, intercalate, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word32)
import Numeric (readHex)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.Environment (withArgs)
import System.Exit (ExitCode)
import System.FilePath ((-<.>), (</>), takeBaseName)
import System.Process (callCommand)
import Test.Tasty.HUnit (assertBool)

-- | Number of cycles to cosimulate when falling back to generated stimulus.
cosimCycles :: Int
cosimCycles = 20

-- | Agreement check for one regression test. The design is driven by the
--   per-test inputs file (<base>.input.yaml) when it exists, or by
--   deterministic pseudorandom stimulus otherwise; either way every backend
--   sees the same inputs for the same number of cycles. Requires that the
--   iverilog/vvp trace, the ghdl trace, the Hyle interpreter (--interpret)
--   output, and -- when cryptol is installed -- the Cryptol backend's
--   rw_device all agree, cycle by cycle. Assumes the working directory is the
--   test directory and that <base>.out.sv has already been generated. Skipped
--   for degenerate devices with no outputs.
runCosim :: FilePath -> IO ()
runCosim fn = do
      (ins, outs) <- parsePorts <$> readFile (ofl "sv")
      unless (null outs) $ do
            -- One stimulus, fed to every backend: the provided inputs file when
            -- present (so backends are checked against the meaningful, golden
            -- stimulus), otherwise generated pseudorandom inputs.
            provided <- doesFileExist providedF
            (inputsF, stim) <-
                  if provided
                        then (providedF, ) . parseInputs (dataIns ins) <$> readFile providedF
                        else do let s = stimulus (takeBaseName fn) (dataIns ins)
                                writeFile genF (inputsYaml s)
                                pure (genF, s)
            let ncyc = length stim

            rwc ncyc ["--testbench=" <> inputsF, "-o", ofl "cosim.sv"]
            rwc ncyc ["--vhdl", "--testbench=" <> inputsF, "-o", ofl "cosim.vhdl"]
            -- The interpreter cannot evaluate externs, so the check degrades to
            -- simulator-vs-simulator when this leg fails.
            iok <- withStderrTo (ofl "cosim.interp.err")
                  (try (rwc ncyc ["--interpret=" <> inputsF, "-o", ofl "cosim.interp.yaml"]) :: IO (Either ExitCode ()))

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
                  $ tv == th && tv == ti && length tv == ncyc * length outs

            -- Fourth leg: evaluate the Cryptol backend's rw_device on the same
            -- stimulus and compare against the interpreter trace. Skipped when
            -- cryptol isn't installed or the interpreter leg failed (model-less
            -- externs the interpreter reached), and also when the generated
            -- Cryptol is a parameterized module: model-less externs become
            -- uninterpreted `parameter`s, and a parameterized module's
            -- rw_device cannot be evaluated standalone (the interpreter can
            -- still succeed when those externs aren't reached, e.g. Sha256_2,
            -- so the iok check alone isn't enough). The simulator and
            -- interpreter legs still cover those.
            mcry <- findExecutable "cryptol"
            case (mcry, iok) of
                  (Just _, Right ()) -> do
                        rwc ncyc ["--cryptol", "-o", ofl "cosim.cry"]
                        parameterized <- any (isPrefixOf "parameter") . lines <$> readFile (ofl "cosim.cry")
                        unless parameterized $ do
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

            -- The per-test inputs file, and the generated-stimulus fallback.
            providedF, genF, wk :: FilePath
            providedF = fn -<.> "input.yaml"
            genF      = ofl "cosim.input.yaml"
            wk        = ofl "ghdlwork"

            rwc :: Int -> [String] -> IO ()
            rwc ncyc args = withArgs ((fn -<.> "rwc") : "--from-core" : "--cycles" : show ncyc : args) RWC.main

            dataIns :: [(String, Int)] -> [(String, Int)]
            dataIns = filter ((`notElem` ["clk", "rst"]) . fst)

-- | Parse an interp-style inputs file -- the same format inputsYaml writes and
--   rwc --interpret/--testbench read -- into per-cycle wire values in the given
--   port order. A "- name: <decimal>" line starts a cycle; indented
--   "name: <decimal>" lines add further wires to it. Trailing "# ..." comments
--   and blank lines are ignored (matching the YAML loader the simulators and
--   interpreter use). A wire absent from a cycle defaults to zero; the
--   regression inputs files list every data input on every cycle, so this
--   agrees with the loader (which would otherwise carry the previous value
--   forward).
parseInputs :: [(String, Int)] -> String -> [[(String, Integer)]]
parseInputs ports = map order . groupCycles . mapMaybe classify . lines
      where classify :: String -> Maybe (Bool, (String, Integer))
            classify l = case dropWhile (== ' ') $ takeWhile (/= '#') l of
                  ""    -> Nothing
                  s | Just s' <- stripPrefix "- " s -> (True, )  <$> kv s'
                    | otherwise                     -> (False, ) <$> kv s

            kv :: String -> Maybe (String, Integer)
            kv s = case words s of
                  [n, v] | ":" `isSuffixOf` n, [(i, "")] <- reads v -> Just (init n, i)
                  _                                                 -> Nothing

            groupCycles :: [(Bool, (String, Integer))] -> [[(String, Integer)]]
            groupCycles []              = []
            groupCycles ((_, nv) : rest) = (nv : map snd cont) : groupCycles more
                  where (cont, more) = span (not . fst) rest

            order :: [(String, Integer)] -> [(String, Integer)]
            order m = [ (n, fromMaybe 0 $ lookup n m) | (n, _) <- ports ]

-- | Output records parsed from a simulation trace or interpreter output: lines
--   of the form "- name: '0xHEX'" (or indented continuation lines); anything
--   else (simulator noise) is ignored. Comparing parsed values makes the check
--   robust to hex-formatting differences (e.g., ghdl pads, the interpreter does
--   not).
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
                  _                                                            -> Nothing

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

-- | Render stimulus as an interp-style inputs file (decimal values).
inputsYaml :: [[(String, Integer)]] -> String
inputsYaml stim
      | all null stim = "[]\n"
      | otherwise     = unlines $ concatMap (zipWith fmt ("- " : repeat "  ")) stim
      where fmt :: String -> (String, Integer) -> String
            fmt pre (n, v) = pre <> n <> ": " <> show v

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

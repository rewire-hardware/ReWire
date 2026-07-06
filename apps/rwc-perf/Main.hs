-- | Compile-time performance benchmarks for rwc.
--
-- Times rwc end-to-end (in-process, via 'RWC.main') on two kinds of input:
--
--   * Synthetic program families generated at several sizes, so scaling
--     behavior is visible: a reported exponent near 1 means linear in the
--     input size, near 2 quadratic, etc. Each family targets a pipeline
--     stage that has shown super-linear behavior (see the generators below).
--   * The historically slowest tests from the golden test suite, as fixed
--     reference points.
--
-- Run with: stack bench rewire:rwc-perf
--           stack bench rewire:rwc-perf --benchmark-arguments="letchain"
--
-- An argument, if given, selects only cases whose names contain it as a
-- substring. Results are printed as a table; nothing is asserted, so this
-- is for tracking and comparing, not gating.
--
-- Extra rwc flags may be supplied via the RWC_PERF_FLAGS environment
-- variable (whitespace-separated), e.g. to benchmark a flag-gated pipeline:
--           RWC_PERF_FLAGS="--eidos" stack bench rewire:rwc-perf
module Main (main) where

import qualified RWC

import Control.Monad (forM, forM_, unless)
import Data.List (isInfixOf, intercalate)
import Data.Maybe (mapMaybe)
import GHC.Clock (getMonotonicTime)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Environment (getArgs, withArgs, lookupEnv)
import System.FilePath ((</>), (<.>))
import System.IO (hSetBuffering, stdout, BufferMode (..))
import Text.Printf (printf)

import Paths_rewire (getDataFileName)

main :: IO ()
main = do
      hSetBuffering stdout LineBuffering
      pat <- getArgs
      let keep n = null pat || any (`isInfixOf` n) pat

      tmp <- (</> "rwc-perf") <$> getTemporaryDirectory
      createDirectoryIfMissing True tmp

      synth <- forM (filter (keep . famName) families) $ \ fam -> do
            rs <- forM (famSizes fam) $ \ n -> do
                  let f = tmp </> famName fam <> "-" <> show n <.> "hs"
                  writeFile f $ famGen fam n
                  t <- timeRwc f $ tmp </> famName fam <> "-" <> show n <.> "sv"
                  printf "%-24s %8d %10.2fs\n" (famName fam) n t
                  pure (n, t)
            pure (famName fam, rs)

      golden <- fmap (mapMaybe id) $ forM (filter keep goldenCases) $ \ c -> do
            f <- getDataFileName $ "tests" </> "golden" </> c <.> "hs"
            t <- timeRwc f $ tmp </> c <.> "sv"
            printf "%-24s %8s %10.2fs\n" c ("-" :: String) t
            pure $ Just (c, t)

      unless (null synth) $ do
            putStrLn "\n-- Scaling (log-log slope between consecutive sizes; 1 = linear, 2 = quadratic):"
            forM_ synth $ \ (name, rs) ->
                  putStrLn $ "  " <> name <> ": " <> intercalate ", " (map (printf "%.1f") $ slopes rs)

      unless (null golden) $ pure ()

      where slopes :: [(Int, Double)] -> [Double]
            slopes rs = zipWith slope rs $ drop 1 rs

            slope :: (Int, Double) -> (Int, Double) -> Double
            slope (n1, t1) (n2, t2) = logBase (fromIntegral n2 / fromIntegral n1) (t2 / max 1e-3 t1)

-- | Compile a file with rwc (in-process) and return elapsed wall-clock seconds.
timeRwc :: FilePath -> FilePath -> IO Double
timeRwc f out = do
      extra <- maybe [] words <$> lookupEnv "RWC_PERF_FLAGS"
      t0 <- getMonotonicTime
      withArgs ([f, "-o", out] <> extra) RWC.main
      t1 <- getMonotonicTime
      pure $ t1 - t0

data Family = Family
      { famName  :: String
      , famSizes :: [Int]
      , famGen   :: Int -> String
      }

families :: [Family]
families =
      [ Family "letchain"  [32, 64, 128]  genLetChain
      , Family "defchain"  [64, 128, 256] genDefChain
      , Family "statevars" [8, 16, 32]    genStateVars
      , Family "whenchain" [4, 8, 16]     genWhenChain
      ]

goldenCases :: [String]
goldenCases = ["gfmult", "Sha256", "OD19Filter", "cubehash"]

-- | A single definition containing a chain of n let-bindings, each used
--   twice by the next (the gfmult shape). Stresses type inference: the
--   unifier rewrites types under the entire accumulated substitution on
--   every unification, so cost grows super-linearly with chain length.
genLetChain :: Int -> String
genLetChain n = unlines $
      [ "{-# LANGUAGE DataKinds #-}"
      , "import Prelude hiding ((^), (+))"
      , "import ReWire"
      , "import ReWire.Bits"
      , "import ReWire.Vectors ((!))"
      , "import ReWire.Monad (iter, Dev)"
      , ""
      , "f :: W 32 -> W 32"
      , "f x0 ="
      , "    let"
      ]
      <> [ "        x" <> show i <> " = if x" <> show (i - 1) <> " ! (Proxy :: Proxy 0)"
           <> " then x" <> show (i - 1) <> " ^ lit " <> show (i * 17 + 1)
           <> " else x" <> show (i - 1) <> " >>. lit 1"
         | i <- [1 .. n] ]
      <>
      [ "    in x" <> show n
      , ""
      , "start :: Dev (W 32) (W 32)"
      , "start = iter f (lit 0)"
      , ""
      , "main :: IO ()"
      , "main = undefined"
      ]

-- | A chain of n small top-level definitions, each calling the previous.
--   Stresses the inlining/specialization/purge fixpoints, which
--   repeatedly substitute and re-scan the full definition list.
genDefChain :: Int -> String
genDefChain n = unlines $
      [ "{-# LANGUAGE DataKinds #-}"
      , "import Prelude hiding ((^), (+))"
      , "import ReWire"
      , "import ReWire.Bits"
      , "import ReWire.Monad (iter, Dev)"
      , ""
      , "f0 :: W 32 -> W 32"
      , "f0 x = x ^ lit 1"
      ]
      <> concat [ [ ""
                  , "f" <> show i <> " :: W 32 -> W 32"
                  , "f" <> show i <> " x = f" <> show (i - 1) <> " (x ^ lit " <> show (i * 13 + 1) <> ")"
                  ]
                | i <- [1 .. n] ]
      <>
      [ ""
      , "start :: Dev (W 32) (W 32)"
      , "start = iter f" <> show n <> " (lit 0)"
      , ""
      , "main :: IO ()"
      , "main = undefined"
      ]

-- | A reactive loop with a chain of n conditional signal points followed by
--   one unconditional signal (so the recursion is guarded and the program
--   valid). Each conditional pause splits the continuation, so this stresses
--   normalizeBind's case-commuting, purify's state minting, and ToHyle's
--   dispatch construction -- the chained-conditional shape behind the
--   historical whenchain divergence.
genWhenChain :: Int -> String
genWhenChain n = unlines $
      [ "import ReWire"
      , ""
      , "{-# INLINE when #-}"
      , "when :: Monad m => Bit -> m () -> m ()"
      , "when b m = case b of"
      , "      True  -> m"
      , "      False -> return ()"
      , ""
      , "{-# INLINE tick #-}"
      , "tick :: ReacT Bit Bit (StateT Bit Identity) ()"
      , "tick = lift get >>= \\ o -> signal o >>= \\ i -> lift (put i)"
      , ""
      , "loop :: ReacT Bit Bit (StateT Bit Identity) ()"
      , "loop = do"
      , "      b <- lift get"
      ]
      <> replicate n "      when b tick"
      <>
      [ "      tick"
      , "      loop"
      , ""
      , "start :: ReacT Bit Bit Identity ()"
      , "start = extrude loop False"
      , ""
      , "main :: IO ()"
      , "main = undefined"
      ]

-- | A stateful device whose state is an n-tuple of words, updated each
--   cycle. Stresses Purify (state threading) and ToHyle (tuple sizing).
genStateVars :: Int -> String
genStateVars n = unlines $
      [ "{-# LANGUAGE DataKinds #-}"
      , "import Prelude hiding ((^), (+))"
      , "import ReWire"
      , "import ReWire.Bits"
      , "import ReWire.Monad (iterSt, Dev, StateDev)"
      , ""
      , "type S = " <> tupleTy
      , ""
      , "step :: W 32 -> S -> (W 32, S)"
      , "step i " <> tuplePat <> " = (i ^ s0, " <> tupleUpd <> ")"
      , ""
      , "start :: Dev (W 32) (W 32)"
      , "start = extrude dev " <> tupleInit
      , ""
      , "dev :: StateDev (W 32) (W 32) (StateT S Identity)"
      , "dev = iterSt step (lit 0)"
      , ""
      , "main :: IO ()"
      , "main = undefined"
      ]
      where vars      = [ "s" <> show i | i <- [0 .. n - 1] ]
            tupleTy   = "(" <> intercalate ", " (replicate n "W 32") <> ")"
            tuplePat  = "(" <> intercalate ", " vars <> ")"
            tupleUpd  = "(" <> intercalate ", " (drop 1 vars <> ["s0 ^ i"]) <> ")"
            tupleInit = "(" <> intercalate ", " (replicate n "lit 0") <> ")"

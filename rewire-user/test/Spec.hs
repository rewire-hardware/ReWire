{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
-- | Tests for the GHC-compatible implementations of the ReWire primitives
--   (i.e., the behavior of rewire-user programs when compiled with GHC
--   instead of rwc).
module Main (main) where

import ReWire (W, Vec, Finite, Proxy (..), Identity, ReacT, StateT, fromList, len, extern, setRef, Ref (..), signal, lift, get, modify)
import ReWire.Finite (finite)
import ReWire.Monad (iter, iterSt, extrudeDev)

import qualified ReWire.Bits       as B
import qualified ReWire.BitWord    as BW
import qualified ReWire.Finite     as F
import qualified ReWire.FiniteComp as FC
import qualified ReWire.Prelude    as RP
import qualified ReWire.Vectors    as V

import Control.Monad.Identity (runIdentity)
import Control.Monad.Resumption.Reactive (deReacT)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

lit8 :: Integer -> W 8
lit8 = B.lit

lit4 :: Integer -> W 4
lit4 = B.lit

-- | Run a device with a list of inputs, collecting its outputs. The first
--   output is produced before any input from the list is consumed.
runDev :: ReacT i o Identity () -> [i] -> [o]
runDev dev = go (runIdentity (deReacT dev))
      where go (Left ()) _              = []
            go (Right (o, _)) []        = [o]
            go (Right (o, k)) (i : is)  = o : go (runIdentity (deReacT (k i))) is

main :: IO ()
main = defaultMain $ testGroup "rewire-user (GHC compatibility)"
      [ litTests
      , sliceTests
      , arithTests
      , bitwiseTests
      , shiftTests
      , compareTests
      , reductionTests
      , vecTests
      , vecTests2
      , finiteTests
      , preludeTests
      , bitWordTests
      , deviceTests
      ]

litTests :: TestTree
litTests = testGroup "literals and conversions"
      [ testCase "lit/toInteger roundtrip"   $ B.toInteger (lit8 42) @?= 42
      , testCase "lit truncates to fit"      $ B.toInteger (lit8 300) @?= 44
      , testCase "lit zero"                  $ B.toInteger (lit8 0) @?= 0
      , testCase "resize truncates"          $ (B.resize (lit8 0xAB) :: W 4) @?= lit4 0xB
      , testCase "resize zero-pads"          $ (B.resize (lit4 0xF) :: W 8) @?= lit8 0xF
      , testCase "sext on negative"          $ (B.sext (lit4 0x8) :: W 8) @?= lit8 0xF8
      , testCase "sext on positive"          $ (B.sext (lit4 0x7) :: W 8) @?= lit8 0x7
      , testCase "msbit of high bit"         $ B.msbit (lit4 0x8) @?= True
      , testCase "msbit of low bits"         $ B.msbit (lit4 0x7) @?= False
      , testCase "odd"                       $ B.odd (lit8 3) @?= True
      , testCase "even"                      $ B.even (lit8 4) @?= True
      , testCase "concat"                    $ (B.lit 2 :: W 2) B.<> (B.lit 1 :: W 2) @?= lit4 9
      , testCase "zero"                      $ B.zero @?= False
      , testCase "one"                       $ B.one @?= True
      , testCase "bit"                       $ B.bit (B.lit 1 :: W 1) @?= True
      ]

-- | Bits are numbered with the LSB at 0 (Verilog convention): w @@ (j, i)
--   takes bits j (most significant) down to i (least significant).
sliceTests :: TestTree
sliceTests = testGroup "bit indexing and slicing"
      [ testCase "@@ high nibble"      $ (lit8 0xA5 B.@@ (7, 4) :: W 4) @?= lit4 0xA
      , testCase "@@ low nibble"       $ (lit8 0xA5 B.@@ (3, 0) :: W 4) @?= lit4 0x5
      , testCase "@@ mid bits"         $ (lit8 0xA5 B.@@ (5, 2) :: W 4) @?= lit4 0x9
      , testCase "@@ single bit"       $ (lit8 0xA5 B.@@ (0, 0) :: W 1) @?= B.lit 1
      , testCase "@. bit 0 (LSB)"      $ lit8 0xA5 B.@. 0 @?= True
      , testCase "@. bit 1"            $ lit8 0xA5 B.@. 1 @?= False
      , testCase "@. bit 7 (MSB)"      $ lit8 0xA5 B.@. 7 @?= True
      , testCase "bitSlice"            $ (B.bitSlice (lit8 0xA5) 7 4 :: W 4) @?= lit4 0xA
      , testCase "bitIndex"            $ B.bitIndex (lit8 0xA5) 2 @?= True
      , testCase "finBitSlice"         $ (B.finBitSlice (lit8 0xA5) (finite 7) (finite 4) :: W 4) @?= lit4 0xA
      , testCase "finBitIndex"         $ B.finBitIndex (lit8 0xA5) (finite 0) @?= True
      ]

arithTests :: TestTree
arithTests = testGroup "arithmetic (wraps modulo bit width)"
      [ testCase "+"          $ lit8 200 B.+ lit8 100 @?= lit8 44
      , testCase "- wraps"    $ lit8 5 B.- lit8 10 @?= lit8 251
      , testCase "*"          $ lit8 20 B.* lit8 13 @?= lit8 4
      , testCase "/"          $ lit8 100 B./ lit8 7 @?= lit8 14
      , testCase "%"          $ lit8 100 B.% lit8 7 @?= lit8 2
      , testCase "**"         $ lit8 2 B.** lit8 5 @?= lit8 32
      ]

bitwiseTests :: TestTree
bitwiseTests = testGroup "bitwise operations"
      [ testCase ".&."        $ lit4 0xC B..&. lit4 0xA @?= lit4 0x8
      , testCase ".|."        $ lit4 0xC B..|. lit4 0xA @?= lit4 0xE
      , testCase "^"          $ (B.^) (lit4 0xC) (lit4 0xA) @?= lit4 0x6
      , testCase "~^"         $ (B.~^) (lit4 0xC) (lit4 0xA) @?= lit4 0x9
      , testCase "bnot"       $ B.bnot (lit4 0x0) @?= lit4 0xF
      , testCase "xor"        $ B.xor True False @?= True
      , testCase "&&&"        $ (True B.&&& False) @?= False
      , testCase "|||"        $ (True B.||| False) @?= True
      , testCase "lnot"       $ B.lnot (lit8 0) @?= True
      , testCase "&&."        $ (lit8 1 B.&&. lit8 2) @?= True
      , testCase "||."        $ (lit8 0 B.||. lit8 2) @?= True
      ]

shiftTests :: TestTree
shiftTests = testGroup "shifts and rotates"
      [ testCase "<<."        $ lit8 1 B.<<. lit8 3 @?= lit8 8
      , testCase "<<. drops"  $ lit4 0x9 B.<<. lit4 1 @?= lit4 0x2
      , testCase ">>."        $ lit8 8 B.>>. lit8 2 @?= lit8 2
      , testCase ">>> sign"   $ (B.>>>) (lit4 0x8) (lit4 1) @?= lit4 0xC
      , testCase ">>> nosign" $ (B.>>>) (lit4 0x4) (lit4 1) @?= lit4 0x2
      , testCase "rotL"       $ B.rotL (lit4 1) (lit4 0x9) @?= lit4 0x3
      , testCase "rotR"       $ B.rotR (lit4 1) (lit4 0x9) @?= lit4 0xC
      ]

compareTests :: TestTree
compareTests = testGroup "comparisons"
      [ testCase "=="         $ (lit8 5 B.== lit8 5) @?= True
      , testCase "/="         $ (lit8 5 B./= lit8 6) @?= True
      , testCase ">"          $ (lit8 5 B.> lit8 3) @?= True
      , testCase "> not"      $ (lit8 3 B.> lit8 5) @?= False
      , testCase ">= equal"   $ (lit8 5 B.>= lit8 5) @?= True
      , testCase ">= greater" $ (lit8 6 B.>= lit8 5) @?= True
      , testCase ">= not"     $ (lit8 3 B.>= lit8 5) @?= False
      , testCase "<"          $ (lit8 3 B.< lit8 5) @?= True
      , testCase "<= equal"   $ (lit8 5 B.<= lit8 5) @?= True
      , testCase "<= not"     $ (lit8 6 B.<= lit8 5) @?= False
      ]

reductionTests :: TestTree
reductionTests = testGroup "reductions"
      [ testCase "rAnd all ones"  $ B.rAnd (lit4 0xF) @?= True
      , testCase "rAnd not"       $ B.rAnd (lit4 0xE) @?= False
      , testCase "rOr zero"       $ B.rOr (lit4 0) @?= False
      , testCase "rOr nonzero"    $ B.rOr (lit4 2) @?= True
      , testCase "rXOr odd ones"  $ B.rXOr (lit4 0x7) @?= True
      , testCase "rXOr even ones" $ B.rXOr (lit4 0x5) @?= False
      , testCase "rNAnd all ones" $ B.rNAnd (lit4 0xF) @?= False
      , testCase "rNAnd not"      $ B.rNAnd (lit4 0xE) @?= True
      , testCase "rNor zero"      $ B.rNor (lit4 0) @?= True
      , testCase "rNor nonzero"   $ B.rNor (lit4 2) @?= False
      , testCase "rXNor odd ones" $ B.rXNor (lit4 0x7) @?= False
      , testCase "rXNor even"     $ B.rXNor (lit4 0x5) @?= True
      -- Odd-width cases: NOT-of-reduction differs from a pairwise fold here.
      , testCase "rNAnd odd width" $ B.rNAnd (B.lit 7 :: W 3) @?= False
      , testCase "rXNor odd width" $ B.rXNor (B.lit 7 :: W 3) @?= False
      ]

vecTests :: TestTree
vecTests = testGroup "vectors"
      [ testCase "fromList/len"   $ len (fromList [1, 2, 3] :: Vec 3 Int) @?= 3
      , testCase "index"          $ V.index (fromList [1, 2, 3] :: Vec 3 Int) (finite 1) @?= 2
      , testCase "head"           $ V.head (fromList [1, 2, 3] :: Vec 3 Int) @?= 1
      , testCase "last"           $ V.last (fromList [1, 2, 3] :: Vec 3 Int) @?= 3
      , testCase "reverse"        $ V.reverse (fromList [1, 2, 3] :: Vec 3 Int) @?= fromList [3, 2, 1]
      , testCase "map"            $ V.map (+ 1) (fromList [1, 2, 3] :: Vec 3 Int) @?= fromList [2, 3, 4]
      , testCase "replicate"      $ (V.replicate 7 :: Vec 4 Int) @?= fromList [7, 7, 7, 7]
      , testCase "++"             $ (fromList [1, 2] :: Vec 2 Int) V.++ (fromList [3] :: Vec 1 Int) @?= (fromList [1, 2, 3] :: Vec 3 Int)
      , testCase "cons"           $ V.cons 0 (fromList [1, 2] :: Vec 2 Int) @?= (fromList [0, 1, 2] :: Vec 3 Int)
      , testCase "snoc"           $ V.snoc (fromList [1, 2] :: Vec 2 Int) 3 @?= (fromList [1, 2, 3] :: Vec 3 Int)
      , testCase "take"           $ (V.take (fromList [1, 2, 3, 4] :: Vec 4 Int) :: Vec 2 Int) @?= fromList [1, 2]
      , testCase "drop"           $ (V.drop (fromList [1, 2, 3, 4] :: Vec 4 Int) :: Vec 3 Int) @?= fromList [2, 3, 4]
      , testCase "update"         $ V.update (fromList [1, 2, 3] :: Vec 3 Int) (finite 1) 9 @?= fromList [1, 9, 3]
      , testCase "generate"       $ (V.generate fromIntegral :: Vec 3 Int) @?= fromList [0, 1, 2]
      , testCase "tail"           $ (V.tail (fromList [1, 2, 3] :: Vec 3 Int) :: Vec 2 Int) @?= fromList [2, 3]
      , testCase "singleton"      $ V.singleton (1 :: Int) @?= fromList [1]
      , testCase "bit order: msbit is head" $ B.msbit (fromList [True, False] :: W 2) @?= True
      , testCase "bit order: toInteger"     $ B.toInteger (fromList [True, False] :: W 2) @?= 2
      ]

vecTests2 :: TestTree
vecTests2 = testGroup "vectors (slices, packing)"
      [ testCase "slice"          $ (V.slice (Proxy :: Proxy 1) (fromList [1, 2, 3, 4] :: Vec 4 Int) :: Vec 2 Int) @?= fromList [2, 3]
      , testCase "rslice"         $ (V.rslice (Proxy :: Proxy 0) (fromList [1, 2, 3, 4] :: Vec 4 Int) :: Vec 2 Int) @?= fromList [3, 4]
      , testCase "rslice offset"  $ (V.rslice (Proxy :: Proxy 1) (fromList [1, 2, 3, 4] :: Vec 4 Int) :: Vec 2 Int) @?= fromList [2, 3]
      , testCase "index'"         $ V.index' (fromList [1, 2, 3] :: Vec 3 Int) (Proxy :: Proxy 2) @?= 3
      , testCase "!"              $ (fromList [1, 2, 3] :: Vec 3 Int) V.! (Proxy :: Proxy 0) @?= 1
      , testCase "!="             $ ((fromList [1, 2, 3] :: Vec 3 Int) V.!= finite 1) 9 @?= fromList [1, 9, 3]
      , testCase "empty"          $ len (V.empty :: Vec 0 Int) @?= 0
      , testCase "lastIndex"      $ V.lastIndex (fromList [1, 2, 3] :: Vec 3 Int) @?= finite 2
      , testCase "init"           $ (V.init (fromList [1, 2, 3] :: Vec 3 Int) :: Vec 2 Int) @?= fromList [1, 2]
      , testCase "packlo"         $ V.packlo (fromList [1, 2, 3, 4] :: Vec 4 Int) (fromList [5, 6, 7, 8]) @?= fromList [1, 3, 5, 7]
      , testCase "packhi"         $ V.packhi (fromList [1, 2, 3, 4] :: Vec 4 Int) (fromList [5, 6, 7, 8]) @?= fromList [2, 4, 6, 8]
      , testCase "unpacklo"       $ V.unpacklo (fromList [1, 2, 3, 4] :: Vec 4 Int) (fromList [5, 6, 7, 8]) @?= fromList [1, 5, 2, 6]
      , testCase "unpackhi"       $ V.unpackhi (fromList [1, 2, 3, 4] :: Vec 4 Int) (fromList [5, 6, 7, 8]) @?= fromList [3, 7, 4, 8]
      ]

finiteTests :: TestTree
finiteTests = testGroup "finite numbers"
      [ testCase "fromFinite"     $ (F.fromFinite (finite 5 :: Finite 8) :: W 4) @?= lit4 5
      , testCase "toFinite"       $ (F.toFinite (lit4 5) :: Finite 8) @?= finite 5
      , testCase "toFinite' mods" $ (F.toFinite' (lit4 13) :: Finite 8) @?= finite 5
      , testCase "minBound"       $ (F.minBound :: Finite 8) @?= finite 0
      , testCase "maxBound"       $ (F.maxBound :: Finite 8) @?= finite 7
      , testCase "+ wraps"        $ (finite 5 FC.+ finite 5 :: Finite 8) @?= finite 2
      , testCase "- wraps"        $ (finite 3 FC.- finite 5 :: Finite 8) @?= finite 6
      , testCase "*"              $ (finite 3 FC.* finite 5 :: Finite 8) @?= finite 7
      , testCase "div"            $ (finite 7 `FC.div` finite 2 :: Finite 8) @?= finite 3
      , testCase "mod"            $ (finite 7 `FC.mod` finite 2 :: Finite 8) @?= finite 1
      , testCase "=="             $ (finite 3 FC.== (finite 3 :: Finite 8)) @?= True
      , testCase "<"              $ (finite 3 FC.< (finite 5 :: Finite 8)) @?= True
      , testCase "<="             $ (finite 5 FC.<= (finite 5 :: Finite 8)) @?= True
      , testCase ">"              $ (finite 5 FC.> (finite 3 :: Finite 8)) @?= True
      , testCase ">="             $ (finite 3 FC.>= (finite 5 :: Finite 8)) @?= False
      , testCase "even"           $ FC.even (finite 4 :: Finite 8) @?= True
      , testCase "odd"            $ FC.odd (finite 3 :: Finite 8) @?= True
      ]

preludeTests :: TestTree
preludeTests = testGroup "prelude"
      [ testCase "id"             $ RP.id (42 :: Int) @?= 42
      , testCase "&&"             $ (True RP.&& False) @?= False
      , testCase "||"             $ (True RP.|| False) @?= True
      , testCase "not"            $ RP.not False @?= True
      , testCase "otherwise"      $ RP.otherwise @?= True
      , testCase "maybe Just"     $ RP.maybe 0 (+ 1) (RP.Just (2 :: Int)) @?= 3
      , testCase "maybe Nothing"  $ RP.maybe 0 (+ 1) (RP.Nothing :: RP.Maybe Int) @?= 0
      , testCase "either Left"    $ RP.either (+ 1) (* 2) (RP.Left 1 :: RP.Either Int Int) @?= 2
      , testCase "either Right"   $ RP.either (+ 1) (* 2) (RP.Right 3 :: RP.Either Int Int) @?= 6
      , testCase "fst"            $ RP.fst (1 :: Int, 2 :: Int) @?= 1
      , testCase "snd"            $ RP.snd (1 :: Int, 2 :: Int) @?= 2
      , testCase "curry"          $ RP.curry RP.fst (1 :: Int) (2 :: Int) @?= 1
      , testCase "uncurry"        $ RP.uncurry (+) (1 :: Int, 2 :: Int) @?= 3
      ]

bitWordTests :: TestTree
bitWordTests = testGroup "bit-level helpers"
      [ testCase "toInt"          $ (BW.toInt BW.one, BW.toInt BW.zero) @?= (1, 0)
      , testCase "notBit"         $ BW.notBit BW.one @?= False
      , testCase "and gate"       $ (True BW.>&&< False) @?= False
      , testCase "or gate"        $ (True BW.>||< False) @?= True
      , testCase "xor gate"       $ (True BW.>^< False) @?= True
      , testCase "eq gate"        $ (True BW.>==< False) @?= False
      , testCase "nand gate"      $ (True BW.>~&< True) @?= False
      , testCase "nor gate"       $ (False BW.>~|< False) @?= True
      , testCase "xnor gate"      $ (True BW.>~^< True) @?= True
      , testCase "rca sum"        $ BW.rca True False False @?= (True, False)
      , testCase "rca carry"      $ BW.rca True True True @?= (True, True)
      , testCase "fromBool"       $ (BW.fromBool True, BW.fromBool False) @?= (1, 0)
      ]

-- | Run ReWire devices under GHC using the reactive-resumption monad.
deviceTests :: TestTree
deviceTests = testGroup "devices (ReacT/StateT)"
      [ testCase "iter"           $ runDev (iter (B.+ lit8 1) (lit8 0)) [lit8 10, lit8 20]
                                          @?= [lit8 1, lit8 11, lit8 21]
      , testCase "iterSt/extrudeDev" $ runDev (extrudeDev (iterSt acc (lit8 0)) (lit8 5)) [lit8 1, lit8 2]
                                          @?= [lit8 5, lit8 5, lit8 6]
      , testCase "signal/lift/modify" $ runDev (extrudeDev counter (lit8 0)) [(), ()]
                                          @?= [lit8 1, lit8 2, lit8 3]
      , testCase "extern is identity" $ extern "some_module" (42 :: Int) @?= 42
      , testCase "setRef passthrough" $ setRef (Ref "r" :: Ref Int) 1 (2 :: Int) @?= 2
      ]
      where acc :: W 8 -> W 8 -> (W 8, W 8)
            acc i s = (s, s B.+ i)

            counter :: ReacT () (W 8) (StateT (W 8) Identity) ()
            counter = do
                  lift (modify (B.+ lit8 1))
                  s <- lift get
                  _ <- signal s
                  counter

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
-- | Tests for the GHC-compatible implementations of the ReWire primitives
--   (i.e., the behavior of rewire-user programs when compiled with GHC
--   instead of rwc).
module Main (main) where

import ReWire (W, Vec, fromList, len)
import ReWire.Finite (finite)

import qualified ReWire.Bits    as B
import qualified ReWire.Vectors as V

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

lit8 :: Integer -> W 8
lit8 = B.lit

lit4 :: Integer -> W 4
lit4 = B.lit

main :: IO ()
main = defaultMain $ testGroup "rewire-user (GHC compatibility)"
      [ litTests
      , arithTests
      , bitwiseTests
      , shiftTests
      , compareTests
      , reductionTests
      , vecTests
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

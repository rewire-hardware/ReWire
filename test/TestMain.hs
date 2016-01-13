import ReWire.Core.KindChecker
import ReWire.Core.TypeChecker
import ReWire.FrontEnd
import ReWire.FrontEnd.LoadPath
import qualified ReWire.Core.Main as M

import System.Directory (setCurrentDirectory)
import System.Environment (withArgs)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Paths_ReWire

-- TODO: should just snarf these from the dir listing
filesToCompile :: [FilePath]
filesToCompile = ["Fibonacci.hs","MiniISA.hs","UpCounter.hs","dissex.hs",
                  "fibo.hs","funcase.hs","toag.hs","toags.hs",
                  "PreludeTest.hs"]

filesToTC :: [FilePath]
filesToTC = filesToCompile ++
             ["uniquification.hs","pats.hs","Mods.hs"]

filesToParse :: [FilePath]
filesToParse = filesToTC ++ ["Salsa20.hs"]

testParse :: FilePath -> Test
testParse f_ = testCase f_ (do d   <- getDataFileName "test/parser_tests/"
                               setCurrentDirectory d
                               lp  <- getSystemLoadPath
                               res <- loadProgram lp f_
                               case res of
                                 Left e  -> assertFailure $ show e
                                 Right _ -> return ())

testTC :: FilePath -> Test
testTC f_ = testCase f_ (do d   <- getDataFileName "test/parser_tests/"
                            setCurrentDirectory d
                            lp  <- getSystemLoadPath
                            res <- loadProgram lp f_
                            case res of
                              Left e  -> assertFailure $ show e
                              Right m -> case kindcheck m of
                                Left e   -> assertFailure $ show e
                                Right m' -> case typecheck m' of
                                  Left e  -> assertFailure $ show e
                                  Right _ -> return ())

testCompile :: FilePath -> Test
testCompile f_ = testCase f_ (do f <- getDataFileName ("test/parser_tests/" ++ f_)
                                 withArgs ["-d",
                                          "-o","/dev/null",
                                          "--cfg=/dev/null",
                                          "--lcfg=/dev/null",
                                          "--pre=/dev/null",
                                          "--gpre=/dev/null",
                                          f]
                                          M.main)

tests = [testGroup "Files to Parse"     (map testParse filesToParse),
         testGroup "Files to Typecheck" (map testTC filesToTC),
         testGroup "Files to Compile"   (map testCompile filesToCompile)]

main = defaultMain tests

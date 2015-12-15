import ReWire.Core.Parser
import ReWire.Core.KindChecker
import ReWire.Core.TypeChecker
import qualified ReWire.Core.Main as M
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.Environment (withArgs)
import Paths_ReWire

-- TODO: should just snarf these from the dir listing
filesToCompile :: [FilePath]
filesToCompile = ["Fibonacci.hs"]

filesToTC :: [FilePath]
filesToTC = filesToCompile ++
             ["MiniISA.hs","UpCounter.hs",
              "dissex.hs","fibo.hs","funcase.hs","toag.hs",
              "uniquification.hs"]

filesToParse :: [FilePath]
filesToParse = filesToTC ++ ["Salsa20.hs"]

testParse :: FilePath -> Test
testParse f_ = testCase f_ (do f   <- getDataFileName ("test/parser_tests/" ++ f_)
                               res <- parseFile f
                               case res of
                                 ParseFailed _ err -> assertFailure err
                                 ParseOk _         -> return ())

testTC :: FilePath -> Test
testTC f_ = testCase f_ (do f   <- getDataFileName ("test/parser_tests/" ++ f_)
                            res <- parseFile f
                            case res of
                              ParseFailed _ err -> assertFailure err
                              ParseOk p         -> case kindcheck p of
                                Just err -> assertFailure err
                                Nothing  -> case typecheck p of
                                  Left err -> assertFailure err
                                  Right _  -> return ())

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

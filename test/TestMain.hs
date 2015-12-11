import ReWire.Core.Parser
import ReWire.Core.KindChecker
import ReWire.Core.TypeChecker
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Paths_ReWire

-- TODO: should just snarf these from the dir listing
filesToTC :: [FilePath]
filesToTC = ["Fibonacci.hs","MiniISA.hs","UpCounter.hs",
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

tests = [testGroup "Files to Parse"     (map testParse filesToParse),
         testGroup "Files to Typecheck" (map testTC filesToTC)]

main = defaultMain tests

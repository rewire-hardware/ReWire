import ReWire.Core.Parser
import ReWire.Core.KindChecker
import ReWire.Core.TypeChecker
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Paths_ReWire

-- TODO: should just snarf these from the dir listing
filesToTC :: [FilePath]
filesToTC = ["Fibonacci.rw","MiniISA.rw","UpCounter.rw",
             "dissex.rw","fibo.rw","funcase.rw","toag.rw",
             "uniquification.rw"]

filesToParse :: [FilePath]
filesToParse = filesToTC ++ ["Salsa20.rw"]

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

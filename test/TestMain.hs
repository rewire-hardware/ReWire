import ReWire.Core.Parser
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Paths_ReWire

-- TODO: should just snarf these from the dir listing
filesToParse :: [FilePath]
filesToParse = ["Fibonacci.rw","MiniISA.rw","Salsa20.rw","UpCounter.rw",
                "dissex.rw","fibo.rw","funcase.rw","toag.rw",
                "uniquification.rw"]

testParse :: FilePath -> Test
testParse f_ = testCase f_ (do f   <- getDataFileName ("test/parser_tests/" ++ f_)
                               res <- parsefile f
                               case res of
                                 Left err -> assertFailure err
                                 Right _  -> return ())

tests = [testGroup "Files to Parse" (map testParse filesToParse)]

main = defaultMain tests

import qualified ReWire.Main as M

import Data.List (isSuffixOf)
import System.Directory (setCurrentDirectory,getDirectoryContents)
import System.Environment (withArgs)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Paths_ReWire

testCompiler :: FilePath -> FilePath -> Test
testCompiler dn fn = testCase fn $ do
      setCurrentDirectory dn
      withArgs ["-o","/dev/null", dn ++ "/" ++ fn, "-v", "--dtypes"] M.main

tests :: IO [Test]
tests = do
      dirname_c  <- getDataFileName "test/integration"
      files_c    <- filter isHs <$> getDirectoryContents dirname_c
      return [testGroup ("\nFull Compiler Tests:\n" ++ foldr (\ a b -> a ++ " " ++ b) "" files_c) (map (testCompiler dirname_c) files_c)]
      where isHs = isSuffixOf ".hs"

main :: IO ()
main = tests >>= defaultMain

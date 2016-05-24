import ReWire.FrontEnd
import ReWire.FrontEnd.LoadPath
import qualified ReWire.Main as M

import Data.List (isSuffixOf)
import System.Directory (setCurrentDirectory,getDirectoryContents)
import System.Environment (withArgs)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Paths_ReWire

testFE :: FilePath -> FilePath -> Test
testFE dn fn = testCase fn (do setCurrentDirectory dn
                               lp  <- getSystemLoadPath
                               res <- loadProgram lp fn
                               case res of
                                 Left e  -> assertFailure $ show e
                                 Right m -> return ())

testCompiler :: FilePath -> FilePath -> Test
testCompiler dn fn = testCase fn (withArgs ["-o","/dev/null",(dn++"/"++fn)] M.main)

tests = do dirname_fe <- getDataFileName ("test/frontend_tests")
           files_fe   <- filter isHs <$> getDirectoryContents dirname_fe
           dirname_c  <- getDataFileName ("test/compiler_tests")
           files_c    <- filter isHs <$> getDirectoryContents dirname_c
           return [testGroup "Frontend Tests"      (map (testFE dirname_fe) files_fe),
                   testGroup "Full Compiler Tests" (map (testCompiler dirname_c) files_c)]
   where isHs = (".hs" `isSuffixOf`)

main = tests >>= defaultMain

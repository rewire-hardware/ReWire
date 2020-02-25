import qualified ReWire.Main as M

import Control.Monad (unless)
import Data.List (isSuffixOf)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Directory (listDirectory, setCurrentDirectory)
import System.Environment (getArgs)
import System.Environment (withArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), (-<.>), takeBaseName, takeDirectory)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process (callCommand)
import Test.Framework (defaultMainWithArgs, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Paths_ReWire (getDataFileName)

data Flag = FlagV
          | FlagNoGhdl
          | FlagNoDTypes
      deriving (Eq, Show)

options :: [OptDescr Flag]
options =
       [ Option ['v'] ["verbose"]   (NoArg FlagV)        "More verbose output."
       , Option []    ["no-ghdl"]   (NoArg FlagNoGhdl)   "Disable verification of output VHDL with 'ghdl -s' (which requires 'ghdl' in $PATH)."
       , Option []    ["no-dtypes"] (NoArg FlagNoDTypes) "Disable extra type-checking passes."
       ]

testCompiler :: [Flag] -> FilePath -> [Test]
testCompiler flags fn = [testCase (takeBaseName fn) $ do
            setCurrentDirectory $ takeDirectory fn
            withArgs (fn : extraFlags) M.main
      ] ++ if FlagNoGhdl `elem` flags then [] else [testCase (takeBaseName fn ++ " (ghdl -s)") $ do
            setCurrentDirectory $ takeDirectory fn
            callCommand $ "ghdl -s " ++ fn -<.> "vhdl"
      ]
      where extraFlags :: [String]
            extraFlags = if FlagNoDTypes `elem` flags then [] else ["--dtypes"]
                      ++ if FlagV `elem` flags then ["-v"] else []

getTests :: [Flag] -> FilePath -> IO Test
getTests flags dirName = do
      dir   <- getDataFileName ("test" </> dirName)
      files <- map (dir </>) . filter (".hs" `isSuffixOf`) <$> listDirectory dir
      pure $ testGroup dirName $ concatMap (testCompiler flags) files

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: rwc-test [OPTION...]" options) >> exitFailure

main :: IO ()
main = do
      (flags, testDirs, errs) <- getOpt Permute options <$> getArgs

      let testDirs' = if null testDirs then ["regression", "integration"] else testDirs

      unless (null errs) $ do
            mapM_ (hPutStrLn stderr) errs
            exitUsage

      tests <- mapM (getTests flags) testDirs'

      defaultMainWithArgs tests []

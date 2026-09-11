{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath ((</>), takeDirectory, takeExtension)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import System.Exit (ExitCode(ExitSuccess))
import qualified Data.Text as T
import System.IO (hGetLine, Handle, hIsEOF)
import Control.Monad (when)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Test.Tasty.Options (IsOption (..), safeRead)
import System.Environment (getArgs, withArgs, lookupEnv)
import Data.List (isPrefixOf, intersect)
import System.Directory (listDirectory)

-- | Define a custom option for the --rebuild flag
newtype RebuildFlag = RebuildFlag Bool
    deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance IsOption RebuildFlag where
    defaultValue = RebuildFlag False
    parseValue = fmap RebuildFlag . safeRead
    optionName = pure "rebuild"
    optionHelp = pure "Run the prepareIsabelle function before testing"

-- | Define a newtype for --cases flag
newtype CasesFlag = CasesFlag [FilePath]
    deriving (Eq, Ord, Show, Typeable, Generic)

instance IsOption CasesFlag where
    defaultValue = CasesFlag []
    parseValue = parseCasesFlag
    optionName = pure "cases"
    optionHelp = pure "Specify a comma-separated list of files in the 'cases' folder to test"

-- | Parse the value for --cases flag
parseCasesFlag :: String -> Maybe CasesFlag
parseCasesFlag arg = if "--cases=" `isPrefixOf` arg
    then Just $ CasesFlag $ map T.unpack $ T.splitOn "," $ T.pack $ drop (T.length "--cases=") arg
    else Nothing

-- | Function to extract --cases flag argument
extractCasesFlag :: [String] -> Maybe CasesFlag
extractCasesFlag [] = Nothing
extractCasesFlag (arg:args) = case parseCasesFlag arg of
    Just x -> Just x
    Nothing -> extractCasesFlag args

-- | Define a custom option for the --each flag
newtype EachFlag = EachFlag Bool
    deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance IsOption EachFlag where
    defaultValue = EachFlag False
    parseValue = fmap EachFlag . safeRead
    optionName = pure "each"
    optionHelp = pure "Test each Haskell file in the batch individually"

-- | Function to make sure rewire-isabelle is ready for testing
prepareIsabelle :: FilePath -> IO ()
prepareIsabelle workingDir = do
    let prepCmd = workingDir </> "isabelle-test-prep.sh"
    -- Pass AFP path from environment if available
    afpEnv <- lookupEnv "AFP"
    let args = maybe [] (:[]) afpEnv
    (exitCode, _output) <- runCommand prepCmd args
    -- Check that the process exits successfully
    assertEqual "Exit code is zero" ExitSuccess exitCode


-- | Function to execute isabelle-test on a batch of Haskell files
testBatch :: FilePath -> [FilePath] -> IO TestTree
testBatch workingDir fileBatch = do
    let testName = "Batch Testing: " ++ show fileBatch
    return $ testCase testName $ do
        let batchCmd = workingDir </> "isabelle-batch-test.sh"
        let args = fileBatch
        (exitCode, output) <- runCommand batchCmd args
        assertEqual "Exit code is zero" ExitSuccess exitCode
        let outputText = T.pack output
        assertBool "Isabelle typechecks all files" $ "Finished ReWire_Testing" `T.isInfixOf` outputText


-- | Function to execute a command
runCommand :: FilePath -> [String] -> IO (ExitCode, String)
runCommand cmd args = do
    (_, Just hout, _, ph) <- createProcess $ (proc cmd args) { std_out = CreatePipe }
    -- Read and print each line of output as it becomes available
    output <- readOutput hout
    -- Wait for the process to complete and return the exit code along with the complete output
    exitCode <- waitForProcess ph
    return (exitCode, output)

-- | Helper function to read output from a handle and accumulate it into a string
readOutput :: Handle -> IO String
readOutput hout = do
    isEof <- hIsEOF hout
    if isEof
        then return ""
        else do
            line <- hGetLine hout
            putStrLn line -- Print the line as it is read
            rest <- readOutput hout
            return $ line ++ "\n" ++ rest

-- | Main function to run the test suite
main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "args: " ++ show args
    let (rebuildFlag, args') = if "--rebuild" `elem` args
                               then (True, filter (/= "--rebuild") args)
                               else (False, args)
    putStrLn $ "rebuildFlag: " ++ show rebuildFlag
    let (eachFlag, args'') = if "--each" `elem` args'
                             then (True, filter (/= "--each") args')
                             else (False, args')
    putStrLn $ "eachFlag: " ++ show eachFlag

    let (casesFlag, casesFiles, args''') = case extractCasesFlag args'' of
           Nothing -> (False,[],args'')
           Just (CasesFlag casesFiles) -> (True,casesFiles, filter (not . isPrefixOf "--cases=") args'')
    putStrLn $ "casesFlag: " ++ show casesFlag



    -- Print the test directory
    putStrLn $ "Current file: " ++ __FILE__
    let testDir = takeDirectory __FILE__ </> ".." </> ".." </> "rewire-embedder" </> "test" </> "isabelle"
    putStrLn $ "Test Directory: " ++ testDir

    when rebuildFlag $ do
        putStrLn "Preparing Isabelle..."
        prepareIsabelle testDir

    -- Get the batchFiles
    let absFolderPath = testDir </> ".." </> "cases"
    files <- listDirectory absFolderPath
    let haskellFiles = filter (\f -> takeExtension f == ".hs") files
    let batchFiles = if casesFlag
        then casesFiles `intersect` haskellFiles
        else haskellFiles

    putStrLn "batchFiles: "
    mapM_ putStrLn batchFiles

    if eachFlag
        then do
            -- Test each file individually
            putStrLn "Running individual tests..."
            individualTests <- mapM (testBatch testDir . (: [])) batchFiles
            withArgs args''' $ defaultMain $ testGroup "Isabelle Embedder Tests" individualTests
        else do
            -- Test all files together in batch mode
            putStrLn "Running batch test..."
            batchTest <- testBatch testDir batchFiles
            withArgs args''' $ defaultMain $ testGroup "Isabelle Embedder Tests" [batchTest]



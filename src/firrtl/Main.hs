{-# LANGUAGE Trustworthy #-}
module Main (main) where

import ReWire.FIRRTL.Parse (parseFIRRTL)

import Control.Monad (unless, (>=>))
import Data.Text (pack)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)
import TextShow (showt)
import qualified Data.Text.IO as T

data Flag = Flag

options :: [OptDescr Flag]
options = []

exitUsage :: IO ()
exitUsage = T.hPutStr stderr (pack $ usageInfo "Usage: firrtl [OPTION...] <filename.fir>" options) >> exitFailure

main :: IO ()
main = do
      (flags, filenames, errs) <-  getOpt Permute options <$> getArgs

      unless (null errs) $ do
            mapM_ (T.hPutStrLn stderr . pack) errs
            exitUsage

      mapM_ (parseFIRRTL >=> print) filenames

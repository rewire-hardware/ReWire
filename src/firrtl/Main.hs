{-# LANGUAGE Safe #-}
module Main (main) where

import ReWire.FIRRTL.Parse (parseFIRRTL)

import Control.Monad (unless, (>=>))
import Data.Text (pack)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)
import qualified Data.Text.IO as T

type Flag = ()

options :: [OptDescr Flag]
options = []

exitUsage :: IO ()
exitUsage = T.hPutStr stderr (pack $ usageInfo "Usage: firrtl [OPTION...] <filename.fir>" options) >> exitFailure

main :: IO ()
main = do
      (_flags, filenames, errs) <-  getOpt Permute options <$> getArgs

      unless (null errs) $ do
            mapM_ (T.hPutStrLn stderr . pack) errs
            exitUsage

      mapM_ (parseFIRRTL >=> print) filenames

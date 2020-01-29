{-# LANGUAGE Safe #-}
module ReWire.Main (main) where

import ReWire.FrontEnd
import ReWire.FrontEnd.LoadPath
import ReWire.Pretty
import ReWire.Core.ToMiniHDL

import Control.Monad (when,unless)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.FilePath ((-<.>))
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Flag = FlagO String
          | FlagD
          | FlagLoadPath String
          deriving (Eq,Show)

options :: [OptDescr Flag]
options =
       [ Option ['d'] ["debug"]    (NoArg FlagD)
                                   "dump miscellaneous debugging information"
       , Option ['o'] []           (ReqArg FlagO "filename.vhd")
                                   "generate VHDL"
       , Option []    ["loadpath"] (ReqArg FlagLoadPath "dir1,dir2,...")
                                   "additional directories for loadpath"
       ]

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: rwc [OPTION...] <filename.rw>" options) >> exitFailure

main :: IO ()
main = do args                       <- getArgs

          let (flags,filenames,errs) =  getOpt Permute options args

          unless (null errs) (mapM_ (hPutStrLn stderr) errs >> exitUsage)

          when (length filenames /= 1) (hPutStrLn stderr "exactly one source file must be specified" >> exitUsage)

          let filename               =  head filenames
              userLP                 =  concatMap getLoadPathEntries flags
          systemLP                   <- getSystemLoadPath
          let lp                     =  userLP ++ systemLP

          when (FlagD `elem` flags) $
            putStrLn ("loadpath: " ++ intercalate "," lp)

          prog_ <- loadProgram lp filename

          case prog_ of
            Left e     -> hPrint stderr e >> exitFailure
            Right prog -> do
              when (FlagD `elem` flags) $ do
                putStrLn "front end finished"
                writeFile "Debug.hs" (prettyPrint prog)
                putStrLn "debug out finished"
              case compileProgram prog of
                Left e      -> hPrint stderr e >> exitFailure
                Right mprog ->
                  case findOutFile flags of
                    Just f  -> writeFile f (prettyPrint mprog)
                    Nothing -> writeFile (filename -<.> "vhdl") (prettyPrint mprog)

  where findOutFile :: [Flag] -> Maybe FilePath
        findOutFile (FlagO f:_) = Just f
        findOutFile (_:flags)   = findOutFile flags
        findOutFile []          = Nothing

        getLoadPathEntries :: Flag -> [FilePath]
        getLoadPathEntries (FlagLoadPath ds) =  splitOn "," ds
        getLoadPathEntries _                 =  []


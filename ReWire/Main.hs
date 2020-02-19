{-# LANGUAGE Safe #-}
module ReWire.Main (main) where

import ReWire.FrontEnd
import ReWire.Pretty
import ReWire.Core.ToMiniHDL
import ReWire.Flags (Flag (..))

import Control.Monad (when,unless)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.FilePath ((-<.>))
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

options :: [OptDescr Flag]
options =
       [ Option ['d'] ["debug"]    (NoArg FlagD)
                                   "dump miscellaneous debugging information"
       , Option ['v'] ["verbose"]    (NoArg FlagV)
                                   "more verbose debugging output"
       , Option []    ["dcrust1"]    (NoArg FlagDCrust1)
                                   "dump post-desugar crust"
       , Option []    ["dcrust2"]    (NoArg FlagDCrust2)
                                   "dump pre-purify crust"
       , Option []    ["dcrust3"]    (NoArg FlagDCrust3)
                                   "dump post-purify crust"
       , Option []    ["dcore"]    (NoArg FlagDCore)
                                   "dump core"
       , Option []    ["dtypes"]    (NoArg FlagDTypes)
                                   "enable extra typechecking after various IR transformations"
       , Option ['o'] []           (ReqArg FlagO "filename.vhd")
                                   "generate VHDL"
       , Option []    ["loadpath"] (ReqArg FlagLoadPath "dir1,dir2,...")
                                   "additional directories for loadpath"
       ]

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: rwc [OPTION...] <filename.rw>" options) >> exitFailure

main :: IO ()
main = do
      args                         <- getArgs

      let (flags, filenames, errs) =  getOpt Permute options args

      unless (null errs) (mapM_ (hPutStrLn stderr) errs >> exitUsage)

      when (length filenames /= 1) (hPutStrLn stderr "exactly one source file must be specified" >> exitUsage)

      let filename                 =  head filenames
          userLP                   =  concatMap getLoadPathEntries flags
      systemLP                     <- getSystemLoadPath
      let lp                       =  userLP ++ systemLP

      when (FlagD `elem` flags) $ putStrLn ("loadpath: " ++ intercalate "," lp)

      prog_ <- loadProgram flags lp filename

      case prog_ of
            Left e     -> hPrint stderr e >> exitFailure
            Right prog -> do
                  when (FlagD `elem` flags) $ do
                        putStrLn "front end finished"
                        writeFile "Debug.hs" (prettyPrint prog)
                        putStrLn "debug out finished"
                  case compileProgram prog of
                        Left e      -> hPrint stderr e >> exitFailure
                        Right mprog -> case findOutFile flags of
                              Just f  -> writeFile f (prettyPrint mprog)
                              Nothing -> writeFile (filename -<.> "vhdl") (prettyPrint mprog)

  where findOutFile :: [Flag] -> Maybe FilePath
        findOutFile (FlagO f:_) = Just f
        findOutFile (_:flags)   = findOutFile flags
        findOutFile []          = Nothing

        getLoadPathEntries :: Flag -> [FilePath]
        getLoadPathEntries (FlagLoadPath ds) =  splitOn "," ds
        getLoadPathEntries _                 =  []


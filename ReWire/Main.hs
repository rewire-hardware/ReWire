{-# LANGUAGE LambdaCase #-}
module ReWire.Main (main) where

import ReWire.FrontEnd
import ReWire.FrontEnd.LoadPath
import ReWire.Pretty

import Control.Monad (when,unless)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Flag = FlagCFG String
          | FlagLCFG String
          | FlagPre String
          | FlagGPre String
          | FlagO String
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
 , Option []    ["cfg"]      (ReqArg FlagCFG "filename.dot")
                             "generate control flow graph before linearization"
 , Option []    ["lcfg"]     (ReqArg FlagLCFG "filename.dot")
                             "generate control flow graph after linearization"
 , Option []    ["pre"]      (ReqArg FlagPre "filename.phdl")
                             "generate PreHDL before goto elimination"
 , Option []    ["gpre"]     (ReqArg FlagGPre "filename.phdl")
                             "generate PreHDL after goto elimination"
 ]

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: rwc [OPTION...] <filename.rw>" options) >> exitFailure

main :: IO ()
main = do args                       <- getArgs

          let (flags,filenames,errs) =  getOpt Permute options args

          unless (null errs) (mapM_ (hPutStrLn stderr) errs >> exitUsage)

          when (length filenames /= 1) (hPutStrLn stderr "exactly one source file must be specified" >> exitUsage)

          unless (any isActFlag flags) (hPutStrLn stderr "must specify at least one of -o, --cfg, --lcfg, --pre, or --gpre" >> exitUsage)

          let filename               =  head filenames
              userLP                 =  concatMap getLoadPathEntries flags
          systemLP                   <- getSystemLoadPath
          let lp                     =  userLP ++ systemLP

          when (FlagD `elem` flags) $
            putStrLn ("loadpath: " ++ intercalate "," lp)

          m_ <- loadProgram lp filename

          case m_ of
            Left e  -> hPrint stderr e >> exitFailure
            Right m -> do
              when (FlagD `elem` flags) $ do
                putStrLn "front end finished"
                -- writeFile "show.out" (show m)
                -- putStrLn "show out finished"
                writeFile "Debug.hs" (prettyPrint m)
                putStrLn "debug out finished"
                -- TODO(chathhorn): disabling backend because I broke.
                -- let cfg     = cfgFromRW m
                --     cfgDot  = mkDot (gather (eu cfg))
                --     lcfgDot = mkDot (gather (linearize cfg))
                --     pre     = cfgToProg cfg
                --     gpre    = gotoElim pre
                --     vhdl    = toVHDL (elimEmpty gpre)
                --     doDump = \ case
                --       FlagCFG f  -> writeFile f cfgDot
                --       FlagLCFG f -> writeFile f lcfgDot
                --       FlagPre f  -> writeFile f $ show pre
                --       FlagGPre f -> writeFile f $ show gpre
                --       FlagO f    -> writeFile f vhdl
                --       _          -> return ()
                -- mapM_ doDump flags
  where isActFlag = \ case
          FlagCFG {}  -> True
          FlagLCFG {} -> True
          FlagPre {}  -> True
          FlagGPre {} -> True
          FlagO {}    -> True
          _           -> False
        getLoadPathEntries (FlagLoadPath ds) =  splitOn "," ds
        getLoadPathEntries _                 =  []


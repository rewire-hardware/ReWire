module ReWire.Core.Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import ReWire.Core.Syntax
import ReWire.Core.Parser
--import ReWire.Core.PrettyPrint
import ReWire.Core.PrettyPrintHaskell
import ReWire.Core.KindChecker
import ReWire.Core.TypeChecker
import ReWire.Core.Transformations.Interactive

data Flag = FlagCFG String
          | FlagLCFG String
          | FlagPre String
          | FlagGPre String
          | FlagO String
          | FlagI
          deriving (Eq,Show)

options :: [OptDescr Flag]
options =
 [ Option ['i'] []       (NoArg FlagI)
                         "run in interactive mode (overrides all other options)"
 , Option ['o'] []       (ReqArg FlagO "filename.vhd")
                         "generate VHDL"
 , Option []    ["cfg"]  (ReqArg FlagCFG "filename.dot")
                         "generate control flow graph before linearization"
 , Option []    ["lcfg"] (ReqArg FlagLCFG "filename.dot")
                         "generate control flow graph after linearization"
 , Option []    ["pre"]  (ReqArg FlagPre "filename.phdl")
                         "generate PreHDL before goto elimination"
 , Option []    ["gpre"] (ReqArg FlagGPre "filename.phdl")
                         "generate PreHDL after goto elimination"
 ]

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: rwc [OPTION...] <filename.rw>" options) >> exitFailure

main :: IO ()
main = do args <- getArgs
          let (flags,filenames,errs) = getOpt Permute options args
          if not (null errs)
             then mapM_ (hPutStrLn stderr) errs >> exitUsage
             else if length filenames /= 1
                     then hPutStrLn stderr "exactly one source file must be specified" >> exitUsage
                     else do
                       let filename = head filenames
                       if FlagI `elem` flags
                          then interactiveLoop filename
                          else do
                             res_p <- parseFile filename
                             case res_p of
                               ParseFailed loc m -> hPutStrLn stderr (prettyPrint loc ++ ":\n\t" ++ m) >> exitFailure
                               ParseOk p ->
                                 case kindcheck p of
                                   Just e  -> hPutStrLn stderr e >> exitFailure
                                   Nothing ->
                                     case typecheck p of
                                       Left e   -> hPutStrLn stderr e >> exitFailure
                                       Right p' -> putStrLn "<<FIXME: bonk>>"

interactiveLoop :: String -> IO ()
interactiveLoop filename = do
  res_p <- parseFile filename
  case res_p of
    ParseFailed loc m -> hPutStrLn stderr (prettyPrint loc ++ ":\n\t" ++ m) >> exitFailure
    ParseOk p -> do
      putStrLn "parse finished"
      writeFile "show.out" (show p)
      putStrLn "show out finished"
      writeFile "Debug.hs" (show $ ppHaskellWithName p "Debug")
      putStrLn "debug out finished"
      case kindcheck p of
        Just e  -> hPutStrLn stderr e
        Nothing -> do putStrLn "kc finished"
                      case typecheck p of
                        Left e   -> hPutStrLn stderr e
                        Right p' -> do putStrLn "tc finished"
                                       writeFile "tc.out" (show p')
                                       putStrLn "tc debug print finished"
                                       trans p'

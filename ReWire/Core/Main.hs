module ReWire.Core.Main where

import System.IO
import System.Environment
import ReWire.Core.Syntax
import ReWire.Core.Parser
--import ReWire.Core.PrettyPrint
import ReWire.Core.PrettyPrintHaskell
import ReWire.Core.KindChecker
import ReWire.Core.TypeChecker
import ReWire.Core.Transformations.Interactive

main :: IO ()
main = do args <- getArgs
          if length args /= 1
             then do n <- getProgName
                     hPutStrLn stderr $ "Syntax: " ++ n ++ " [filename.core]"
             else do let filename = head args
                     res_p <- parsefile filename
                     case res_p of
                       Left e  -> hPutStrLn stderr e
                       Right p -> do putStrLn "parse finished"
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

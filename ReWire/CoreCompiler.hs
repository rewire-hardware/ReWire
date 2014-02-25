module ReWire.CoreCompiler where

import System.IO
import System.Environment
import ReWire.Core
import ReWire.CoreParser
import ReWire.CoreKC
import ReWire.CoreTC
import ReWire.CorePP
import ReWire.Eval

main :: IO ()
main = do args <- getArgs
          if length args /= 1
             then do n <- getProgName
                     hPutStrLn stderr $ "Syntax: " ++ n ++ " [filename.core]"
             else do let filename = head args
                     res_p <- parsefile filename
                     case res_p of
                       Left e  -> hPutStrLn stderr e
                       Right p -> case kindcheck p of
                                    Just e  -> hPutStrLn stderr e
                                    Nothing -> case typecheck p of
                                                 Left e   -> hPutStrLn stderr e
                                                 Right p' -> doPE p'

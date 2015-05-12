{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings  #-}

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
--GHCJS
import ReWire.PreHDL.CFG
import ReWire.PreHDL.GotoElim
import ReWire.PreHDL.ElimEmpty
import ReWire.PreHDL.ToVHDL

import ReWire.Core.Transformations.ToPreHDL
import qualified GHCJS.Types as T
import GHCJS.Marshal
import GHCJS.Foreign

import qualified Data.Text as Txt

foreign import javascript unsafe "console.log($1)" jlog :: T.JSString -> IO ()
--jlog = putStrLn
--toJSString = id

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


rwcStr :: String -> IO ()
rwcStr str = do
               let res_p = parsewithname "Visual ReWire Diagram" str 
               case res_p of
                  Left e -> jlog "Parse Failed" --Parse Failed
                  Right p -> case kindcheck p of
                                  Just e  -> jlog "Kind Check Failed" --KC Failed
                                  Nothing -> case typecheck p of
                                                  Left e   -> jlog "Typecheck failed" --Typecheck failed
                                                  Right p' -> do
                                                                let res = toVHDL (elimEmpty $ gotoElim $ cfgToProg (cfgFromRW p'))
                                                                let res = toVHDL (elimEmpty $ gotoElim $ cfgToProg (cfgFromRW p'))
                                                                jlog (toJSString res)
rewire :: T.JSRef T.JSString -> IO ()
rewire jref = do
                t <- fromJSRef jref
                case t of
                     Just jstr -> rwcStr (fromJSString jstr) 
                     Nothing   -> jlog "Couldn't unmarshal JRef."

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Interactive environment for transforming the program.

module ReWire.Transform (TransCommand,trans) where

import Prelude hiding (sequence,mapM)
import ReWire.Core
import ReWire.CorePP (pp)
import ReWire.CorePPHaskell (ppHaskell)
import Unbound.LocallyNameless
import Text.Parsec (runParser,eof)
import Control.Monad hiding (sequence,mapM)
import Data.List (isInfixOf,find,intercalate)
import Control.Monad.Reader hiding (sequence,mapM)
import Control.Monad.Identity hiding (sequence,mapM)
import Data.Traversable (sequence,mapM)
import Data.Maybe (catMaybes,isNothing,fromJust)
import Data.Char
import ReWire.Eval (cmdExpand)
import ReWire.TransformTypes

import Debug.Trace (trace)

-- Here are the commands available.
type CommandTable = [(String,TransCommand)]

cmdPrint :: TransCommand
cmdPrint _ p = (Just p,Nothing)

cmdHelp :: TransCommand
cmdHelp _ _ = (Nothing,Just (intercalate ", " (map fst cmdTable)))

cmdTable :: CommandTable
cmdTable = [(":p",cmdPrint),
            (":?",cmdHelp),
            ("expand",cmdExpand)]

-- The "repl" for the translation environment.
trans :: RWCProg -> IO ()
trans p = do print (ppHaskell p)
             loop p
   where loop p = do putStr "> "
                     n <- getLine
                     let (cmd,n') = break isSpace n
                         args     = dropWhile isSpace n'
                     if cmd==":q" then return ()
                     else case lookup cmd cmdTable of
                               Just f  -> do let (mp,ms) = f args p
                                             case ms of
                                               Just s  -> putStrLn s
                                               Nothing -> return ()
                                             case mp of
                                               Just p' -> do print (ppHaskell p')
                                                             loop p'
                                               Nothing -> loop p
                               Nothing -> do if not (null n) then putStrLn $ "Invalid command: " ++ cmd else return ()
                                             loop p
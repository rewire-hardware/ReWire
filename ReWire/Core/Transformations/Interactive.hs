{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Interactive environment for transforming ReWire Core programs.

module ReWire.Core.Transformations.Interactive (TransCommand,trans) where

import Prelude hiding (sequence,mapM)
import ReWire.Core.Syntax
import ReWire.Core.PrettyPrintHaskell (ppHaskell)
import Control.Monad hiding (sequence,mapM)
import Data.List (isInfixOf,find,intercalate)
import Control.Monad.Reader hiding (sequence,mapM)
import Control.Monad.Identity hiding (sequence,mapM)
import Data.Traversable (sequence,mapM)
import Data.Maybe (catMaybes,isNothing,fromJust)
import Data.Char
import ReWire.Core.Transformations.Expand (cmdExpand)
import ReWire.Core.Transformations.Reduce (cmdReduce)
import ReWire.Core.Transformations.Purge (cmdPurge,cmdOccurs)
import ReWire.Core.Transformations.ToPreHDL (cmdToCFG,cmdToPre,cmdToVHDL,cmdToSCFG,cmdToPreG)
import ReWire.Core.Transformations.Uniquify (cmdUniquify)
import ReWire.Core.Transformations.DeUniquify (cmdDeUniquify)
import ReWire.Core.Transformations.Types
import System.IO

-- Table of available commands.
type CommandTable = [(String,TransCommand)]

cmdPrint :: TransCommand
cmdPrint _ p = (Nothing,Just $ show $ ppHaskell p)

cmdHelp :: TransCommand
cmdHelp _ _ = (Nothing,Just (intercalate ", " (map fst cmdTable)))

--cmdPrintDebug :: TransCommand
--cmdPrintDebug _ p = (Nothing,Just (show $ pp p))

cmdPrintShow :: TransCommand
cmdPrintShow _ p = (Nothing,Just (show p))

-- Here are the commands available.
cmdTable :: CommandTable
cmdTable = [
            (":p",cmdPrint),
--            (":pd",cmdPrintDebug),
            (":ps",cmdPrintShow),
            (":?",cmdHelp),
            ("uniquify",cmdUniquify),
            ("deuniquify",cmdDeUniquify),
            ("expand",cmdExpand),
            ("reduce",cmdReduce),
            ("purge",cmdPurge),
--            ("ll",lambdaLift),
--            ("status",cmdStatus),
            ("occurs",cmdOccurs),
--            ("uses", cmdUses),
--            ("checknf",cmdCheckNF),
            ("tovhdl",cmdToVHDL),
            ("toscfg",cmdToSCFG),
            ("tocfg",cmdToCFG),
            ("topre",cmdToPre),
            ("topreg",cmdToPreG)
--            ("topseudo",cmdToPseudo)
           ]

-- The "repl" for the translation environment.
trans :: RWCModule -> IO ()
trans m = do print (ppHaskell m)
             loop m
   where loop m = do putStr "> "
                     hFlush stdout
                     n <- getLine
                     let (cmd,n') = break isSpace n
                         args     = dropWhile isSpace n'
                     unless (cmd == ":q") $
                         case lookup cmd cmdTable of
                               Just f  -> do let (mp,ms) = f args m
                                             case ms of
                                               Just s  -> putStrLn s >> writeFile "rewire.cmd.out" s
                                               Nothing -> return ()
                                             case mp of
                                               Just m' -> do print (ppHaskell m')
                                                             loop m'
                                               Nothing -> loop m
                               Nothing -> do if not (null n) then putStrLn $ "Invalid command: " ++ cmd else return ()
                                             loop m

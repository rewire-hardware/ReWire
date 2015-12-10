{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Reduce where

import Prelude hiding (sequence,mapM)
import ReWire.Scoping
import ReWire.Core.Syntax
import Control.Monad hiding (sequence,mapM)
import Data.List (isInfixOf,find)
import Control.Monad.Reader hiding (sequence,mapM)
import Control.Monad.Identity hiding (sequence,mapM)
import Data.Traversable (sequence,mapM)
import Data.Maybe (catMaybes,isNothing,fromJust)
import ReWire.Core.Transformations.Monad
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Uniquify (uniquify)
import ReWire.Core.Transformations.DeUniquify (deUniquify)

import Debug.Trace (trace)

reduce :: Monad m => RWCExp -> RWT m RWCExp
reduce (RWCApp e1 e2)     = do e1' <- reduce e1
                               e2' <- reduce e2
                               case e1' of
                                 RWCLam n t b -> do b' <- fsubstE n e2' b
                                                    reduce b'
                                 _            -> return (RWCApp e1' e2')
reduce (RWCLam n t e)      = do e' <- reduce e
                                return (RWCLam n t e')
reduce (RWCLet n el eb)    = do el' <- reduce el
                                eb' <- reduce eb
                                return (RWCLet n el' eb')
reduce e@(RWCVar _ _)      = return e
reduce e@(RWCCon _ _)      = return e
reduce e@(RWCLiteral _)    = return e
reduce (RWCCase esc alts)  = do esc'  <- reduce esc
                                alts' <- mapM redalt alts
                                sr    <- redcase esc' alts
                                case sr of
                                  Just e  -> return e
                                  Nothing -> return (RWCCase esc' alts')
reduce e@(RWCNativeVHDL{}) = return e

redalt :: Monad m => RWCAlt -> RWT m RWCAlt
redalt (RWCAlt p eb) = do eb' <- reduce eb
                          return (RWCAlt p eb')

redcase :: Monad m => RWCExp -> [RWCAlt] -> RWT m (Maybe RWCExp)
redcase esc (RWCAlt p eb:alts) = do mr <- matchpat esc p
                                    case mr of
                                      MatchYes sub -> do eb' <- fsubstsE sub eb
                                                         liftM Just $ reduce eb'
                                      MatchMaybe   -> return Nothing
                                      MatchNo      -> redcase esc alts
redcase esc []                 = return Nothing -- FIXME: should return undefined?

data MatchResult = MatchYes [(Id RWCExp,RWCExp)]
                 | MatchMaybe
                 | MatchNo
                 deriving Show

mergematches :: [MatchResult] -> MatchResult
mergematches []     = MatchYes []
mergematches (m:ms) = case mr of
                        MatchYes bs -> case m of
                                         MatchYes bs' -> MatchYes (bs'++bs)
                                         MatchNo      -> MatchNo
                                         MatchMaybe   -> MatchMaybe
                        MatchNo     -> MatchNo
                        MatchMaybe  -> case m of
                                         MatchYes _ -> MatchMaybe
                                         MatchNo    -> MatchNo
                                         MatchMaybe -> MatchMaybe
  where mr = mergematches ms

matchpat :: Monad m => RWCExp -> RWCPat -> RWT m MatchResult
matchpat e (RWCPatCon i pats) = case flattenApp e of
                                  (RWCCon c _:es) | c == i && length es == length pats -> do ms <- zipWithM matchpat es pats
                                                                                             return (mergematches ms)
                                                  | otherwise                          -> return MatchNo
                                  _                                                    -> return MatchMaybe
matchpat e (RWCPatVar n _)    = return (MatchYes [(n,e)])
matchpat e (RWCPatLiteral l)  = case e of
                                  RWCLiteral l' | l == l'   -> return (MatchYes [])
                                                | otherwise -> return MatchNo
                                  _                         -> return MatchMaybe
matchpat e RWCPatWild         = return (MatchYes [])

reddefn :: Monad m => RWCDefn -> RWT m RWCDefn
reddefn (RWCDefn n pt e) = do e' <- reduce e
                              return (RWCDefn n pt e')

redprog :: RWCProg -> RWCProg
redprog p_ = deUniquify $ runRW ctr p (do ds' <- mapM reddefn (defns p)
                                          return (p { defns = ds' }))
  where (p,ctr) = uniquify 0 p_

cmdReduce :: TransCommand
cmdReduce _ p = (Just (redprog p),Nothing)

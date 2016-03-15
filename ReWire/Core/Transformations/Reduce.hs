{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Reduce
  ( redmod
  , cmdReduce
  ) where

import Prelude hiding (sequence,mapM)
import ReWire.Scoping
import ReWire.Core.Syntax
import Control.Monad hiding (sequence,mapM)
--import Data.List (isInfixOf,find)
--import Control.Monad.Reader hiding (sequence,mapM)
--import Control.Monad.Identity hiding (sequence,mapM)
import Data.Traversable (mapM)
--import Data.Maybe (catMaybes,isNothing,fromJust)
import ReWire.Core.Transformations.Monad
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Uniquify (uniquify)
import ReWire.Core.Transformations.DeUniquify (deUniquify)

reduce :: Monad m => RWCExp -> RWT m RWCExp
reduce (RWCApp an e1 e2)     = do e1' <- reduce e1
                                  e2' <- reduce e2
                                  case e1' of
                                    RWCLam _ n _ b -> do b' <- fsubstE n e2' b
                                                         reduce b'
                                    _              -> return (RWCApp an e1' e2')
reduce (RWCLam an n t e)      = do e' <- reduce e
                                   return (RWCLam an n t e')
reduce e@RWCVar {}            = return e
reduce e@RWCCon {}            = return e
reduce e@RWCLiteral {}        = return e
reduce (RWCCase an e p e1 e2) = do e' <- reduce e
                                   mr <- matchpat e' p
                                   case mr of
                                     MatchYes sub -> do
                                       e1' <- fsubstsE sub e1
                                       reduce e1'
                                     MatchMaybe ->
                                       liftM2 (RWCCase an e' p) (reduce e1) (reduce e2)
                                     MatchNo -> reduce e2
reduce e@RWCNativeVHDL {}     = return e
reduce e@RWCError {}          = return e


data MatchResult = MatchYes [(Id RWCExp,RWCExp)]
                 | MatchMaybe
                 | MatchNo
                 deriving Show

mergematches :: [MatchResult] -> MatchResult
mergematches []     = MatchYes []
mergematches (m:ms) = case mergematches ms of
                        MatchYes bs -> case m of
                                         MatchYes bs' -> MatchYes (bs'++bs)
                                         MatchNo      -> MatchNo
                                         MatchMaybe   -> MatchMaybe
                        MatchNo     -> MatchNo
                        MatchMaybe  -> case m of
                                         MatchYes _ -> MatchMaybe
                                         MatchNo    -> MatchNo
                                         MatchMaybe -> MatchMaybe

matchpat :: Monad m => RWCExp -> RWCPat -> RWT m MatchResult
matchpat e (RWCPatCon _ i pats) = case flattenApp e of
                                    (RWCCon _ c _:es) | c == i && length es == length pats -> do ms <- zipWithM matchpat es pats
                                                                                                 return (mergematches ms)
                                                      | otherwise                          -> return MatchNo
                                    _                                                      -> return MatchMaybe
matchpat e (RWCPatVar _ n _)    = return (MatchYes [(n,e)])
matchpat e (RWCPatLiteral _ l)  = case e of
                                    RWCLiteral _ l' | l == l'   -> return (MatchYes [])
                                                    | otherwise -> return MatchNo
                                    _                           -> return MatchMaybe

reddefn :: Monad m => RWCDefn -> RWT m RWCDefn
reddefn (RWCDefn an n pt b e) = do e' <- reduce e
                                   return (RWCDefn an n pt b e')

redmod :: RWCProgram -> RWCProgram
redmod m_ = deUniquify $ runRW ctr m (do ds' <- mapM reddefn (defns m)
                                         return (m { defns = ds' }))
  where (m,ctr) = uniquify 0 m_

cmdReduce :: TransCommand
cmdReduce _ p = (Just (redmod p),Nothing)

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Reduce where

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
reduce e@RWCVar {}      = return e
reduce e@RWCCon {}      = return e
reduce e@RWCLiteral {}    = return e
reduce (RWCCase an esc alts)  = do esc'  <- reduce esc
                                   alts' <- mapM redalt alts
                                   sr    <- redcase esc' alts
                                   case sr of
                                     Just e  -> return e
                                     Nothing -> return (RWCCase an esc' alts')
reduce e@RWCNativeVHDL {} = return e

redalt :: Monad m => RWCAlt -> RWT m RWCAlt
redalt (RWCAlt an p eb) = do eb' <- reduce eb
                             return (RWCAlt an p eb')

redcase :: Monad m => RWCExp -> [RWCAlt] -> RWT m (Maybe RWCExp)
redcase esc (RWCAlt _ p eb:alts) = do mr <- matchpat esc p
                                      case mr of
                                        MatchYes sub -> do eb' <- fsubstsE sub eb
                                                           liftM Just $ reduce eb'
                                        MatchMaybe   -> return Nothing
                                        MatchNo      -> redcase esc alts
redcase _ []                   = return Nothing -- FIXME: should return undefined?

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

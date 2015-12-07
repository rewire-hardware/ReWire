{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.CBN where

import Prelude hiding (sequence,mapM)
import ReWire.Core.Syntax
import Unbound.LocallyNameless
import Control.Monad hiding (sequence,mapM)
import Data.List (isInfixOf,find)
import Control.Monad.Reader hiding (sequence,mapM)
import Control.Monad.Identity hiding (sequence,mapM)
import Data.Traversable (sequence,mapM)
import Data.Maybe (catMaybes,isNothing,fromJust)
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad

import Debug.Trace (trace)

data MatchResult = MatchYes [(Name RWCExp,RWCExp)]
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

matchpat :: MonadReWire m => RWCExp -> RWCPat -> m MatchResult
matchpat e_ (RWCPatCon i pats) = do e <- evalexpr e_
                                    case flattenApp e of
                                      (RWCCon _ c:es) | c == i && length es == length pats -> do ms <- zipWithM matchpat es pats
                                                                                                 return (mergematches ms)
                                                      | otherwise                          -> return MatchNo
                                      _                                                    -> return MatchMaybe --Lam (can't happen), Var, Literal (can't happen), Case
matchpat e (RWCPatVar _ n)     = return (MatchYes [(n,e)])
matchpat e_ (RWCPatLiteral l)  = do e <- evalexpr e_
                                    case e of
                                      RWCLiteral _ l' | l == l'   -> return (MatchYes [])
                                                      | otherwise -> return MatchNo
                                      _                           -> return MatchMaybe

evalexpr :: MonadReWire m => RWCExp -> m RWCExp
evalexpr (RWCApp t e1_ e2)  = do e1 <- evalexpr e1_
                                 case e1 of
                                   RWCLam tl b -> lunbind b (\(n,e) -> evalexpr (subst n e2 e))
                                   _           -> return (RWCApp t e1 e2)
evalexpr e@(RWCLam {})      = return e
evalexpr (RWCVar t n)       = do e <- askvar t n
                                 if e `aeq` RWCVar t n
                                    then return e
                                    else evalexpr e
evalexpr e@(RWCCon {})      = return e
evalexpr e@(RWCLiteral {})  = return e
evalexpr (RWCCase t e alts) = do me <- evalcase e alts
                                 case me of
                                   Just e' -> evalexpr e'
                                   Nothing -> return (RWCCase t e alts)

evalcase :: MonadReWire m => RWCExp -> [RWCAlt] -> m (Maybe RWCExp)
evalcase escrut (RWCAlt b:alts) = lunbind b (\(p,ebody) ->
                                    do mr <- matchpat escrut p
                                       case mr of
                                         MatchYes sub -> return (Just $ substs sub ebody)
                                         MatchMaybe   -> return Nothing
                                         MatchNo      -> evalcase escrut alts)
evalcase escrut []              = return Nothing

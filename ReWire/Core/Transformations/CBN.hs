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

import Debug.Trace (trace)

-- FIXME: begin stuff that may need to be moved out to a module
type M = ReaderT [RWCDefn] (LFreshMT Identity)

runM :: M a -> a
runM m = runIdentity (runLFreshMT (runReaderT m []))

askDefns :: M [RWCDefn]
askDefns = ask

mergesubs :: Monad m => [(Name RWCTy,RWCTy)] -> [(Name RWCTy,RWCTy)] -> m [(Name RWCTy,RWCTy)]
mergesubs ((n,t):sub) sub' = case lookup n sub' of
                               Just t' -> if t `aeq` t' then mergesubs sub sub'
                                                        else fail "mergesubs failed"
                               Nothing -> do sub'' <- mergesubs sub sub'
                                             return ((n,t):sub'')
mergesubs [] sub'          = return sub'

matchty :: Monad m => [(Name RWCTy,RWCTy)] -> RWCTy -> RWCTy -> m [(Name RWCTy,RWCTy)]
matchty sub (RWCTyVar n) t                         = case lookup n sub of
                                                       Nothing -> return ((n,t):sub)
                                                       Just t' -> if t `aeq` t' then return sub
                                                                                else fail "matchty failed (variable inconsistency)"
matchty sub (RWCTyCon i1) (RWCTyCon i2) | i1 == i2 = return sub
matchty sub (RWCTyApp t1 t2) (RWCTyApp t1' t2')    = do sub1 <- matchty [] t1 t1'
                                                        sub2 <- matchty [] t2 t2'
                                                        mergesubs sub1 sub2
matchty _ t1 t2                                    = fail $ "matchty failed (constructor head): " ++ show t1 ++ ", " ++ show t2

askvar :: RWCTy -> Name RWCExp -> M RWCExp
askvar t n = do ds <- askDefns
                case find (\ (RWCDefn n' _) -> n == n') ds of
                  Just (RWCDefn _ (Embed b)) -> lunbind b (\(tvs,(t',e)) ->
                                                 do sub <- matchty [] t' t
                                                    return (substs sub e))
                  _                          -> return (RWCVar t n)

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

matchpat :: RWCExp -> RWCPat -> M MatchResult
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

flattenApp :: RWCExp -> [RWCExp]
flattenApp (RWCApp _ e e') = flattenApp e++[e']
flattenApp e               = [e]
-- FIXME: end stuff that may need to be moved out to a module

evalexpr :: RWCExp -> M RWCExp
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

evalcase :: RWCExp -> [RWCAlt] -> M (Maybe RWCExp)
evalcase escrut (RWCAlt b:alts) = lunbind b (\(p,ebody) ->
                                    do mr <- matchpat escrut p
                                       case mr of
                                         MatchYes sub -> return (Just $ substs sub ebody)
                                         MatchMaybe   -> return Nothing
                                         MatchNo      -> evalcase escrut alts)
evalcase escrut []              = return Nothing

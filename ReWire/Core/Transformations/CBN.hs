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

-- FIXME: begin stuff that needs to be moved out to a module
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
-- FIXME: end stuff that needs to be moved out to a module

evalexpr :: RWCExp -> M RWCExp
evalexpr (RWCApp t e1_ e2)  = do e1 <- evalexpr e1_
                                 case e1 of
                                   RWCLam tl b -> lunbind b (\(n,e) -> evalexpr (subst n e2 e))
                                   _           -> return (RWCApp t e1 e2)
evalexpr e@(RWCLam {})      = return e
evalexpr (RWCVar t n)       = askvar t n
evalexpr e@(RWCCon {})      = return e
evalexpr e@(RWCLiteral {})  = return e
evalexpr (RWCCase t e alts) = evalcase e alts

evalcase = undefined

{-
evalcase :: RWCExp -> [RWCAlt] -> M (Either (RWCExp,[RWCAlt]) RWCExp)
evalcase e (alt:alts) = do res <- evalalt e alt
                           case res of
                             MatchYes e' -> return (Right e')
                             MatchNo     -> evalcase e alts
                             MatchMaybe  -> do er <- evalcase e alts
                                               case er of
                                                 -}
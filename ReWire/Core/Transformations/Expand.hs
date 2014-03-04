{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Expand (cmdExpand) where

import Prelude hiding (sequence,mapM)
import ReWire.Core.Syntax
import Unbound.LocallyNameless
import Control.Monad hiding (sequence,mapM)
import Data.List (isInfixOf,find)
import Data.Traversable (sequence,mapM)
import Data.Maybe (catMaybes,isNothing,fromJust)
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad

import Debug.Trace (trace)

expandalt :: Name RWCExp -> RWCAlt -> RW RWCAlt
expandalt nexp (RWCAlt b) = lunbind b (\(p,eb) ->
                             do eb' <- expandexpr nexp eb
                                return (RWCAlt (bind p eb')))

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

askvar :: RWCTy -> Name RWCExp -> RW RWCExp
askvar t n = do ds <- askDefns
                case find (\ (RWCDefn n' _) -> n == n') ds of
                  Just (RWCDefn _ (Embed b)) -> lunbind b (\(tvs,(t',e)) ->
                                                 do sub <- matchty [] t' t
                                                    return (substs sub e))
                  _                          -> return (RWCVar t n)

expandexpr :: Name RWCExp -> RWCExp -> RW RWCExp
expandexpr nexp (RWCApp t e1 e2)         = liftM2 (RWCApp t) (expandexpr nexp e1) (expandexpr nexp e2)
expandexpr nexp (RWCLam t b)             = lunbind b (\(n,e) ->
                                            do e' <- expandexpr nexp e
                                               return (RWCLam t (bind n e')))
expandexpr nexp (RWCVar t n) | nexp == n = askvar t n
                             | otherwise = return (RWCVar t n)
expandexpr nexp e@(RWCCon {})            = return e
expandexpr nexp e@(RWCLiteral {})        = return e
expandexpr nexp (RWCCase t e alts)       = do e'    <- expandexpr nexp e
                                              alts' <- mapM (expandalt nexp) alts
                                              return (RWCCase t e' alts')

expanddefn :: Name RWCExp -> RWCDefn -> RW RWCDefn
expanddefn nexp (RWCDefn n (Embed b)) = lunbind b (\(tvs,(t,e)) ->
                                         do e' <- expandexpr nexp e
                                            return (RWCDefn n (Embed (setbind tvs (t,e')))))

pe :: Name RWCExp -> RW [RWCDefn]
pe n = do ds <- askDefns
          mapM (expanddefn n) ds

cmdExpand :: TransCommand
cmdExpand n p = let ds = runRW p (pe (s2n n))
                    p' = p { defns = trec ds }
                in  (Just p',Nothing)

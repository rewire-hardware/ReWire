{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
--
-- This type checker is based loosely on Mark Jones's "Typing Haskell in
-- Haskell", though since we don't have type classes in core it is much
-- simpler.
--

module ReWire.Core.TypeChecker (typecheck) where

import ReWire.Scoping
import ReWire.Core.Syntax
import ReWire.Core.PrimBasis
import Control.DeepSeq
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except
--import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
--import Debug.Trace (trace,traceShow)
--import ReWire.Core.Prims

-- Type checker for core.

type TySub = Map (Id RWCTy) RWCTy
data TCEnv = TCEnv { as  :: Map (Id RWCExp) (Poly RWCTy),
                     cas :: Map DataConId (Poly RWCTy) } deriving Show
data TCState = TCState { tySub :: TySub,
                         ctr   :: Int } deriving Show

type Assump  = (Id RWCExp,Poly RWCTy)
type CAssump = (DataConId,Poly RWCTy)

type TCM = ReaderT TCEnv (StateT TCState (ExceptT String Identity))

localAssumps :: (Map (Id RWCExp) (Poly RWCTy) -> Map (Id RWCExp) (Poly RWCTy)) -> TCM a -> TCM a
localAssumps f = local (\ tce -> tce { as = f (as tce) })

askAssumps :: TCM (Map (Id RWCExp) (Poly RWCTy))
askAssumps = ask >>= \ tce -> return (as tce)

localCAssumps :: (Map DataConId (Poly RWCTy) -> Map DataConId (Poly RWCTy)) -> TCM a -> TCM a
localCAssumps f = local (\ tce -> tce { cas = f (cas tce) })

askCAssumps :: TCM (Map DataConId (Poly RWCTy))
askCAssumps = ask >>= \ tce -> return (cas tce)

getTySub :: TCM TySub
getTySub = get >>= return . tySub

updTySub :: (TySub -> TySub) -> TCM ()
updTySub f = get >>= \ s -> put (s { tySub = f (tySub s) })

putTySub :: TySub -> TCM ()
putTySub sub = get >>= \ s -> put (s { tySub = sub })

getCtr :: TCM Int
getCtr = get >>= return . ctr

putCtr :: Int -> TCM ()
putCtr c = get >>= \ s -> put (s { ctr = c })

freshv :: TCM (Id RWCTy)
freshv = do ctr   <- getCtr
            putCtr (ctr+1)
            let n =  mkId $ "?" ++ show ctr
            updTySub (Map.insert n (RWCTyVar noAnn n))
            return n

{-
initPat :: RWCPat -> TCM RWCPat
initPat (RWCPatCon i ps_) = do ps <- mapM initPat ps_
                               return (RWCPatCon i ps)
initPat (RWCPatLiteral l) = return (RWCPatLiteral l)
initPat (RWCPatVar n _)   = do tv <- freshv
                               return (RWCPatVar n (RWCTyVar tv))

initAlt :: RWCAlt -> TCM RWCAlt
initAlt (RWCAlt p_ e_) = do p       <- initPat p_
                            e       <- initExp e_
                            return (RWCAlt p e)

initExp :: RWCExp -> TCM RWCExp
initExp (RWCApp e1_ e2_)   = do e1 <- initExp e1_
                                e2 <- initExp e2_
                                return (RWCApp e1 e2)
initExp (RWCLam x _ e_)    = do e  <- initExp e_
                                tv <- freshv
                                return (RWCLam x (RWCTyVar tv) e)
initExp (RWCVar n _)       = do tv <- freshv
                                return (RWCVar n (RWCTyVar tv))
initExp (RWCCon n _)       = do tv <- freshv
                                return (RWCCon n (RWCTyVar tv))
initExp (RWCLiteral l)     = return (RWCLiteral l)
initExp (RWCCase e_ alts_) = do e    <- initExp e_
                                alts <- mapM initAlt alts_
                                return (RWCCase e alts)

initDefn :: RWCDefn -> TCM RWCDefn
initDefn (RWCDefn n (tvs :-> t) e) = do e'          <- initExp e
                                        return (RWCDefn n (tvs :-> t) e')
-}

defnAssump :: RWCDefn -> Assump
defnAssump (RWCDefn _ n pt _ _) = (n,pt)

dataConAssump :: [Id RWCTy] -> RWCTy -> RWCDataCon -> CAssump
dataConAssump tvs rt (RWCDataCon _ i ts) = (i,tvs :-> foldr mkArrow rt ts)

dataDeclAssumps :: RWCData -> [CAssump]
dataDeclAssumps (RWCData _ i tvs _ dcs) = let rt = foldl (RWCTyApp noAnn) (RWCTyCon noAnn i) (map (RWCTyVar noAnn) tvs)
                                          in  map (dataConAssump tvs rt) dcs

(@@) :: TySub -> TySub -> TySub
s1@@s2 = {-s1 `deepseq` s2 `deepseq` force-} s
         where s = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

isFlex :: Id a -> Bool
isFlex = (=='?') . head . deId

varBind :: Monad m => Id RWCTy -> RWCTy -> m TySub
varBind u t | t `aeq` RWCTyVar noAnn u = return Map.empty
            | u `elem` fv t            = fail $ "occurs check fails: " ++ show u ++ ", " ++ show t
            | otherwise                = return (Map.singleton u t)

mgu :: Monad m => RWCTy -> RWCTy -> m TySub
mgu (RWCTyApp _ tl tr) (RWCTyApp _ tl' tr')                = do s1 <- mgu tl tl'
                                                                s2 <- mgu (subst s1 tr) (subst s1 tr')
                                                                return (s2@@s1)
mgu (RWCTyVar _ u) t | isFlex u                            = varBind u t
mgu t (RWCTyVar _ u) | isFlex u                            = varBind u t
mgu (RWCTyCon _ c1) (RWCTyCon _ c2) | c1 == c2             = return Map.empty
mgu (RWCTyVar _ v) (RWCTyVar _ u) | not (isFlex v) && v==u = return Map.empty
mgu (RWCTyComp _ m t) (RWCTyComp _ m' t')                  = do s1 <- mgu m m'
                                                                s2 <- mgu (subst s1 t) (subst s1 t')
                                                                return (s2@@s1)
mgu t1 t2                                                  = fail $ "types do not unify: " ++ show t1 ++ ", " ++ show t2

unify :: RWCTy -> RWCTy -> TCM ()
unify t1 t2 = do s <- getTySub
                 u <- mgu (subst s t1) (subst s t2)
                 updTySub (u@@)

inst :: Poly RWCTy -> TCM RWCTy
inst (tvs :-> t) = do sub <- liftM Map.fromList $ mapM (\ tv -> freshv >>= \ v -> return (tv,RWCTyVar noAnn v)) tvs
                      return (subst sub t)

patassumps :: RWCPat -> [Assump]
patassumps (RWCPatCon _ _ ps)  = concatMap patassumps ps
patassumps (RWCPatLiteral _ _) = []
patassumps (RWCPatVar _ n t)   = [(n,[] :-> t)]

tcPat :: RWCTy -> RWCPat -> TCM RWCPat
tcPat t p@(RWCPatLiteral _ l) = do case l of
                                     RWCLitInteger _ -> unify t (RWCTyCon noAnn (TyConId "Integer"))
                                     RWCLitFloat _   -> unify t (RWCTyCon noAnn (TyConId "Float"))
                                     RWCLitChar _    -> unify t (RWCTyCon noAnn (TyConId "Char"))
                                   return p
tcPat t (RWCPatVar an x _)  = return (RWCPatVar an x t)
tcPat t (RWCPatCon an i ps) = do cas     <- askCAssumps
                                 let mpt =  Map.lookup i cas
                                 case mpt of
                                   Nothing -> fail $ "Unknown constructor: " ++ deDataConId i
                                   Just pta  -> do ta               <- inst pta
                                                   let (targs,tres) =  flattenArrow ta
                                                   if length ps /= length targs
                                                     then fail "Pattern is not applied to enough arguments"
                                                     else do ps' <- zipWithM tcPat targs ps
                                                             unify t tres
                                                             return (RWCPatCon an i ps')
tcAlt :: RWCTy -> RWCTy -> RWCAlt -> TCM RWCAlt
tcAlt tres tscrut (RWCAlt an p e) = do p'      <- tcPat tscrut p
                                       let as  =  Map.fromList (patassumps p')
                                       (e',te) <- localAssumps (as `Map.union`) (tcExp e)
                                       unify tres te
                                       return (RWCAlt an p' e')

tcExp :: RWCExp -> TCM (RWCExp,RWCTy)
tcExp e_@(RWCApp an _ _)     = do let (ef:es)  =  flattenApp e_
                                  (ef',tf)     <- tcExp ef
                                  ress         <- mapM tcExp es
                                  let es'      =  map fst ress
                                      tes      =  map snd ress
                                  tv           <- freshv
                                  unify tf (foldr mkArrow (RWCTyVar noAnn tv) tes)
                                  return (foldl (RWCApp an) ef' es',RWCTyVar noAnn tv)
tcExp (RWCLam an x _ e)      = do tvx     <- freshv
                                  tvr     <- freshv
                                  (e',te) <- localAssumps (Map.insert x ([] :-> RWCTyVar noAnn tvx)) (tcExp e)
                                  unify (RWCTyVar noAnn tvr) (RWCTyVar noAnn tvx `mkArrow` te)
                                  return (RWCLam an x (RWCTyVar noAnn tvx) e',RWCTyVar noAnn tvr)
tcExp (RWCVar an v _)        = do as      <- askAssumps
                                  let mpt =  Map.lookup v as
                                  case mpt of
                                    Nothing -> fail $ "Unknown variable: " ++ show v
                                    Just pt -> do t <- inst pt
                                                  return (RWCVar an v t,t)
tcExp (RWCCon an i _)        = do cas     <- askCAssumps
                                  let mpt =  Map.lookup i cas
                                  case mpt of
                                    Nothing -> fail $ "Unknown constructor: " ++ deDataConId i
                                    Just pt -> do t <- inst pt
                                                  return (RWCCon an i t,t)
tcExp e@(RWCLiteral _ l)     = case l of
                                  RWCLitInteger _ -> return (e,RWCTyCon noAnn (TyConId "Integer"))
                                  RWCLitFloat _   -> return (e,RWCTyCon noAnn (TyConId "Float"))
                                  RWCLitChar _    -> return (e,RWCTyCon noAnn (TyConId "Char"))
tcExp (RWCCase an e alts)    = do (e',te) <- tcExp e
                                  tv      <- freshv
                                  alts'   <- mapM (tcAlt (RWCTyVar noAnn tv) te) alts
                                  return (RWCCase an e' alts',RWCTyVar noAnn tv)
tcExp (RWCNativeVHDL an n e) = do (e',te) <- tcExp e
                                  return (RWCNativeVHDL an n e',te)

tcDefn :: RWCDefn -> TCM RWCDefn
tcDefn d  = do putTySub Map.empty
--               d <- initDefn d_
               let RWCDefn noAnn n (tvs :-> t) b e = force d
               (e',te) <- tcExp e
               unify t te
               s       <- getTySub
               putTySub Map.empty
               let d' = RWCDefn noAnn n (tvs :-> t) b (subst s e')
               --traceShow n $ d' `deepseq` return d'
               d' `deepseq` return d'

tc :: [RWCProgram] -> RWCProgram -> TCM RWCProgram
tc ms m = do let as  =  Map.fromList $ concatMap (map defnAssump . defns) (m:ms)
                 cas =  Map.fromList $ concatMap (concatMap dataDeclAssumps . dataDecls) (m:ms)
             ds'     <- localAssumps (as `Map.union`) (localCAssumps (cas `Map.union`) (mapM tcDefn (defns m)))
             return (m { defns = ds' })

typecheck :: RWCProgram -> Either String RWCProgram
typecheck m = fmap fst $ runIdentity (runExceptT (runStateT (runReaderT (tc [primBasis] m) (TCEnv Map.empty Map.empty)) (TCState Map.empty 0)))

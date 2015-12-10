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
import Control.DeepSeq
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Debug.Trace (trace,traceShow)
import ReWire.Core.Prims

-- Type checker for core.

type TySub = Map (Id RWCTy) RWCTy
data TCEnv = TCEnv { as  :: Map (Id RWCExp) (Poly RWCTy),
                     cas :: Map DataConId (Poly RWCTy) } deriving Show
data TCState = TCState { tySub :: TySub,
                         ctr   :: Int } deriving Show

type Assump  = (Id RWCExp,Poly RWCTy)
type CAssump = (DataConId,Poly RWCTy)

type TCM = ReaderT TCEnv (StateT TCState (ExceptT String Identity))

localAssumps f = local (\ tce -> tce { as = f (as tce) })
askAssumps = ask >>= \ tce -> return (as tce)
localCAssumps f = local (\ tce -> tce { cas = f (cas tce) })
askCAssumps = ask >>= \ tce -> return (cas tce)
getTySub = get >>= return . tySub
updTySub f = get >>= \ s -> put (s { tySub = f (tySub s) })
putTySub sub = get >>= \ s -> put (s { tySub = sub })
getCtr = get >>= return . ctr
updCtr f = get >>= \ s -> put (s { ctr = f (tySub s) })
putCtr c = get >>= \ s -> put (s { ctr = c })

freshv :: TCM (Id RWCTy)
freshv = do ctr   <- getCtr
            putCtr (ctr+1)
            let n =  mkId $ "?" ++ show ctr
            updTySub (Map.insert n (RWCTyVar n))
            return n

{-
initPat :: RWCPat -> TCM RWCPat
initPat (RWCPatCon i ps_) = do ps <- mapM initPat ps_
                               return (RWCPatCon i ps)
initPat (RWCPatLiteral l) = return (RWCPatLiteral l)
initPat (RWCPatVar n _)   = do tv <- freshv
                               return (RWCPatVar n (RWCTyVar tv))
initPat RWCPatWild        = return RWCPatWild

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
defnAssump (RWCDefn n pt _) = (n,pt)

dataConAssump :: [Id RWCTy] -> RWCTy -> RWCDataCon -> CAssump
dataConAssump tvs rt (RWCDataCon i ts) = (i,tvs :-> foldr mkArrow rt ts)

dataDeclAssumps :: RWCData -> TCM [CAssump]
dataDeclAssumps (RWCData i tvs dcs) = do let rt = foldl RWCTyApp (RWCTyCon i) (map RWCTyVar tvs)
                                         return $ map (dataConAssump tvs rt) dcs

(@@) :: TySub -> TySub -> TySub
s1@@s2 = {-s1 `deepseq` s2 `deepseq` force-} s
         where s = Map.mapWithKey (\ u t -> subst s1 t) s2 `Map.union` s1

isFlex :: Id a -> Bool
isFlex = (=='?') . head . deId

varBind :: Monad m => Id RWCTy -> RWCTy -> m TySub
varBind u t | t `aeq` RWCTyVar u = return Map.empty
            | u `elem` fv t      = fail $ "occurs check fails: " ++ show u ++ ", " ++ show t
            | otherwise          = return (Map.singleton u t)

mgu :: Monad m => RWCTy -> RWCTy -> m TySub
mgu (RWCTyApp tl tr) (RWCTyApp tl' tr')                = do s1 <- mgu tl tl'
                                                            s2 <- mgu (subst s1 tr) (subst s1 tr')
                                                            return (s2@@s1)
mgu (RWCTyVar u) t | isFlex u                          = varBind u t
mgu t (RWCTyVar u) | isFlex u                          = varBind u t
mgu (RWCTyCon c1) (RWCTyCon c2) | c1 == c2             = return Map.empty
mgu (RWCTyVar v) (RWCTyVar u) | not (isFlex v) && v==u = return Map.empty
mgu (RWCTyComp m t) (RWCTyComp m' t')                  = do s1 <- mgu m m'
                                                            s2 <- mgu (subst s1 t) (subst s1 t')
                                                            return (s2@@s1)
mgu t1 t2                                              = fail $ "types do not unify: " ++ show t1 ++ ", " ++ show t2

unify :: RWCTy -> RWCTy -> TCM ()
unify t1 t2 = do s <- getTySub
                 u <- mgu (subst s t1) (subst s t2)
                 updTySub (u@@)

inst :: Poly RWCTy -> TCM RWCTy
inst (tvs :-> t) = do sub <- liftM Map.fromList $ mapM (\ tv -> freshv >>= \ v -> return (tv,RWCTyVar v)) tvs
                      return (subst sub t)

patassumps :: RWCPat -> [Assump]
patassumps (RWCPatCon i ps)  = concatMap patassumps ps
patassumps (RWCPatLiteral _) = []
patassumps (RWCPatVar n t)   = [(n,[] :-> t)]
patassumps RWCPatWild        = []

tcPat :: RWCTy -> RWCPat -> TCM RWCPat
tcPat t p@(RWCPatLiteral l) = do case l of
                                   RWCLitInteger _ -> unify t (RWCTyCon (TyConId "Integer"))
                                   RWCLitFloat _   -> unify t (RWCTyCon (TyConId "Float"))
                                   RWCLitChar _    -> unify t (RWCTyCon (TyConId "Char"))
                                 return p
tcPat t (RWCPatVar x _)   = return (RWCPatVar x t)
tcPat t (RWCPatCon i ps)  = do cas     <- askCAssumps
                               let mpt =  Map.lookup i cas
                               case mpt of
                                 Nothing -> fail $ "Unknown constructor: " ++ deDataConId i
                                 Just pta  -> do ta               <- inst pta
                                                 let (targs,tres) =  flattenArrow ta
                                                 if length ps /= length targs
                                                   then fail "Pattern is not applied to enough arguments"
                                                   else do ps' <- zipWithM tcPat targs ps
                                                           unify t tres
                                                           return (RWCPatCon i ps')
tcPat t RWCPatWild        = return RWCPatWild

tcAlt :: RWCTy -> RWCTy -> RWCAlt -> TCM RWCAlt
tcAlt tres tscrut (RWCAlt p e) = do p'      <- tcPat tscrut p
                                    let as  =  Map.fromList (patassumps p')
                                    (e',te) <- localAssumps (as `Map.union`) (tcExp e)
                                    unify tres te
                                    return (RWCAlt p' e')

tcExp :: RWCExp -> TCM (RWCExp,RWCTy)
tcExp e_@(RWCApp {})      = do let (ef:es)  =  flattenApp e_
                               (ef',tf)     <- tcExp ef
                               ress         <- mapM tcExp es
                               let es'      =  map fst ress
                                   tes      =  map snd ress
                               tv           <- freshv
                               unify tf (foldr mkArrow (RWCTyVar tv) tes)
                               return (foldl RWCApp ef' es',RWCTyVar tv)
tcExp (RWCLam x _ e)      = do tvx     <- freshv
                               tvr     <- freshv
                               (e',te) <- localAssumps (Map.insert x ([] :-> RWCTyVar tvx)) (tcExp e)
                               unify (RWCTyVar tvr) (RWCTyVar tvx `mkArrow` te)
                               return (RWCLam x (RWCTyVar tvx) e',RWCTyVar tvr)
tcExp (RWCLet x e1 e2)    = do (e1',te1) <- tcExp e1
                               (e2',te2) <- localAssumps (Map.insert x ([] :-> te1)) (tcExp e2)
                               return (RWCLet x e1' e2',te2)
tcExp (RWCVar v _)        = do as      <- askAssumps
                               let mpt =  Map.lookup v as
                               case mpt of
                                 Nothing -> fail $ "Unknown variable: " ++ show v
                                 Just pt -> do t <- inst pt
                                               return (RWCVar v t,t)
tcExp (RWCCon i _)        = do cas     <- askCAssumps
                               let mpt =  Map.lookup i cas
                               case mpt of
                                 Nothing -> fail $ "Unknown constructor: " ++ deDataConId i
                                 Just pt -> do t <- inst pt
                                               return (RWCCon i t,t)
tcExp e@(RWCLiteral l)    = case l of
                              RWCLitInteger _ -> return (e,RWCTyCon (TyConId "Integer"))
                              RWCLitFloat _   -> return (e,RWCTyCon (TyConId "Float"))
                              RWCLitChar _    -> return (e,RWCTyCon (TyConId "Char"))
tcExp (RWCCase e alts)    = do (e',te) <- tcExp e
                               tv      <- freshv
                               alts'   <- mapM (tcAlt (RWCTyVar tv) te) alts
                               return (RWCCase e' alts',RWCTyVar tv)
tcExp (RWCNativeVHDL n e) = do (e',te) <- tcExp e
                               return (RWCNativeVHDL n e',te)

tcDefn :: RWCDefn -> TCM RWCDefn
tcDefn d  = do putTySub Map.empty
--               d <- initDefn d_
               let RWCDefn n (tvs :-> t) e = force d
               (e',te) <- tcExp e
               unify t te
               s       <- getTySub
               putTySub Map.empty
               let d' = RWCDefn n (tvs :-> t) (subst s e')
               traceShow n $ d' `deepseq` return d'

tc :: RWCProg -> TCM RWCProg
tc p = do let as_ =  Map.fromList $ map defnAssump (defns p)
              as  =  as_ `Map.union` as0
              as0 =  Map.fromList prims
          cas     <- liftM (Map.fromList . concat) $ mapM dataDeclAssumps (dataDecls p)
          ds'     <- localAssumps (as `Map.union`) (localCAssumps (cas `Map.union`) (mapM tcDefn (defns p)))
          return (p { defns = ds' })

typecheck :: RWCProg -> Either String RWCProg
typecheck p = fmap fst $ runIdentity (runExceptT (runStateT (runReaderT (tc p) (TCEnv Map.empty Map.empty)) (TCState Map.empty 0)))

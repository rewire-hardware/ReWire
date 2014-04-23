{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
--
-- This type checker is based loosely on Mark Jones's "Typing Haskell in
-- Haskell", though since we don't have type classes in core it is much
-- simpler.
--

module ReWire.Core.TypeChecker (typecheck) where

import ReWire.Core.Syntax
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Error
import Data.List (nub)
import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace (trace)

-- Type checker for core.

type TySub = Map (Id RWCTy) RWCTy
data TCEnv = TCEnv { as  :: Map (Id RWCExp) (Poly RWCTy),
                     cas :: Map ConId (Poly RWCTy) } deriving Show
data TCState = TCState { tySub :: TySub, 
                         ctr   :: Int } deriving Show
                         
type Assump  = (Id RWCExp,Poly RWCTy)
type CAssump = (ConId,Poly RWCTy)

type TCM = ReaderT TCEnv (StateT TCState (ErrorT String Identity))

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
            let n =  Id $ "?" ++ show ctr
            updTySub (Map.insert n (RWCTyVar n))
            return n

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

defnAssump :: RWCDefn -> TCM Assump
defnAssump (RWCDefn n (tvs :-> t) _) = return (n,tvs :-> t)

dataConAssump :: [Id RWCTy] -> RWCTy -> RWCDataCon -> CAssump
dataConAssump tvs rt (RWCDataCon i ts) = (i,tvs :-> foldr mkArrow rt ts)

dataDeclAssumps :: RWCData -> TCM [CAssump]
dataDeclAssumps (RWCData i tvs dcs) = do let rt = foldl RWCTyApp (RWCTyCon i) (map RWCTyVar tvs)
                                         return $ map (dataConAssump tvs rt) dcs

(@@) :: TySub -> TySub -> TySub
--s1@@s2 = Map.fromList $ [(u,subst s1 t) | (u,t) <- l2] ++ l1
--  where l1 = Map.toList s1
--        l2 = Map.toList s2
s1@@s2 = Map.mapWithKey (\ u t -> subst s1 t) s2 `Map.union` s1

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

tcPat :: RWCTy -> RWCPat -> TCM ()
tcPat t (RWCPatLiteral l) = case l of
                              RWCLitInteger _ -> unify t (RWCTyCon "Integer")
                              RWCLitFloat _   -> unify t (RWCTyCon "Float")
                              RWCLitChar _    -> unify t (RWCTyCon "Char")
tcPat t (RWCPatVar _ tp)  = unify t tp
tcPat t (RWCPatCon i ps)  = do cas     <- askCAssumps
                               let mpt =  Map.lookup i cas
                               case mpt of
                                 Nothing -> fail $ "Unknown constructor: " ++ i
                                 Just pta  -> do ta               <- inst pta
                                                 let (targs,tres) =  flattenArrow ta
                                                 if length ps /= length targs
                                                   then fail "Pattern is not applied to enough arguments"
                                                   else do zipWithM_ tcPat targs ps
                                                           unify t tres

tcAlt :: RWCTy -> RWCTy -> RWCAlt -> TCM ()
tcAlt tres tscrut (RWCAlt p e) = do tcPat tscrut p
                                    let as = Map.fromList (patassumps p)
                                    te <- localAssumps (as `Map.union`) (tcExp e)
                                    unify tres te

tcExp :: RWCExp -> TCM RWCTy
tcExp (RWCApp e1 e2)   = do tv <- freshv
                            t1 <- tcExp e1
                            t2 <- tcExp e2
                            unify t1 (t2 `mkArrow` RWCTyVar tv)
                            return (RWCTyVar tv)
tcExp (RWCLam x t e)   = do tv <- freshv
                            te <- localAssumps (Map.insert x ([] :-> t)) (tcExp e)
                            unify (RWCTyVar tv) (t `mkArrow` te)
                            return (RWCTyVar tv)
tcExp (RWCVar v t)     = do as      <- askAssumps
                            let mpt =  Map.lookup v as
                            case mpt of
                              Nothing -> fail $ "Unknown variable: " ++ show v
                              Just pt -> do ta <- inst pt
                                            unify t ta
                                            return t
tcExp (RWCCon i t)     = do cas     <- askCAssumps
                            let mpt =  Map.lookup i cas
                            case mpt of
                              Nothing -> fail $ "Unknown constructor: " ++ i
                              Just pt -> do ta <- inst pt
                                            unify t ta
                                            return t
tcExp (RWCLiteral l)   = do tv <- freshv
                            case l of
                              RWCLitInteger _ -> unify (RWCTyVar tv) (RWCTyCon "Integer")
                              RWCLitFloat _   -> unify (RWCTyVar tv) (RWCTyCon "Float")
                              RWCLitChar _    -> unify (RWCTyVar tv) (RWCTyCon "Char")
                            return (RWCTyVar tv)
tcExp (RWCCase e alts) = do tv <- freshv
                            te <- tcExp e
                            mapM_ (tcAlt (RWCTyVar tv) te) alts
                            return (RWCTyVar tv)

tcDefn :: RWCDefn -> TCM RWCDefn
tcDefn d = do putTySub (Map.empty)
              RWCDefn n (tvs :-> t) e <- initDefn d
              te <- tcExp e
              unify t te
              s  <- getTySub
              putTySub (Map.empty)
              trace (show n) $ trace (show (length (Map.keys s))) return (RWCDefn n (tvs :-> t) (subst s e))

tc :: RWCProg -> TCM RWCProg
tc p = do as  <- liftM Map.fromList $ mapM defnAssump (defns p)
          cas <- liftM (Map.fromList . concat) $ mapM dataDeclAssumps (dataDecls p)
          ds' <- localAssumps (as `Map.union`) (localCAssumps (cas `Map.union`) (mapM tcDefn (defns p)))
          return (p { defns = ds' })

typecheck :: RWCProg -> Either String RWCProg
typecheck p = fmap fst $ runIdentity (runErrorT (runStateT (runReaderT (tc p) (TCEnv Map.empty Map.empty)) (TCState Map.empty 0)))

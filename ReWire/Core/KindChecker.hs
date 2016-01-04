{-# LANGUAGE MultiParamTypeClasses,FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.KindChecker (kindcheck) where

import ReWire.Scoping
import ReWire.Core.PrimBasis
import ReWire.Core.Syntax
import ReWire.Core.Kinds
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except
--import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map,(!))
--import Control.DeepSeq
--import Data.ByteString.Char8 (pack)

-- Kind checking for Core.
type KiSub = Map (Id Kind) Kind
data KCEnv = KCEnv { as  :: Map (Id RWCTy) Kind,
                     cas :: Map TyConId Kind } deriving Show
data KCState = KCState { kiSub :: KiSub, ctr :: Int } deriving Show

type KCM = ReaderT KCEnv (StateT KCState (ExceptT String Identity))

localAssumps :: (Map (Id RWCTy) Kind -> Map (Id RWCTy) Kind) -> KCM a -> KCM a
localAssumps f = local (\ kce -> kce { as = f (as kce) })

askAssumps :: KCM (Map (Id RWCTy) Kind)
askAssumps = ask >>= \ kce -> return (as kce)

localCAssumps :: (Map TyConId Kind -> Map TyConId Kind) -> KCM a -> KCM a
localCAssumps f = local (\ kce -> kce { cas = f (cas kce) })

askCAssumps :: KCM (Map TyConId Kind)
askCAssumps = ask >>= \ kce -> return (cas kce)

getKiSub :: KCM KiSub
getKiSub = get >>= return . kiSub

updKiSub :: (KiSub -> KiSub) -> KCM ()
updKiSub f = get >>= \ s -> put (s { kiSub = f (kiSub s) })

putKiSub :: KiSub -> KCM ()
putKiSub sub = get >>= \ s -> put (s { kiSub = sub })

getCtr :: KCM Int
getCtr = get >>= return . ctr

putCtr :: Int -> KCM ()
putCtr c = get >>= \ s -> put (s { ctr = c })

freshkv :: KCM (Id Kind)
freshkv = do ctr   <- getCtr
             putCtr (ctr+1)
             let n =  mkId $ "?" ++ show ctr
             updKiSub (Map.insert n (Kvar n))
             return n

initDataDecl :: RWCData -> KCM (TyConId,Kind)
initDataDecl (RWCData i _ _ _) = do v <- freshkv
                                    return (i,Kvar v)

varBind :: Monad m => Id Kind -> Kind -> m KiSub
varBind u k | k `aeq` Kvar u = return Map.empty
            | u `elem` fv k  = fail $ "occurs check fails in kind checking: " ++ show u ++ ", " ++ show k
            | otherwise      = return (Map.singleton u k)

(@@) :: KiSub -> KiSub -> KiSub
s1@@s2 = {-s1 `deepseq` s2 `deepseq` force -} s
         where s = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

mgu :: Monad m => Kind -> Kind -> m KiSub
mgu (Kfun kl kr) (Kfun kl' kr') = do s1 <- mgu kl kl'
                                     s2 <- mgu (subst s1 kr) (subst s1 kr')
                                     return (s2@@s1)
mgu (Kvar u) k                  = varBind u k
mgu k (Kvar u)                  = varBind u k
mgu Kstar Kstar                 = return Map.empty
mgu Kmonad Kmonad               = return Map.empty
mgu k1 k2                       = fail $ "kinds do not unify: " ++ show k1 ++ ", " ++ show k2

unify :: Kind -> Kind -> KCM ()
unify k1 k2 = do s <- getKiSub
                 u <- mgu (subst s k1) (subst s k2)
                 updKiSub (u@@)

kcTy :: RWCTy -> KCM Kind
kcTy (RWCTyApp t1 t2)  = do k1 <- kcTy t1
                            k2 <- kcTy t2
                            k  <- freshkv
                            unify k1 (Kfun k2 (Kvar k))
                            return (Kvar k)
kcTy (RWCTyCon i)      = do cas <- askCAssumps
                            let mk = Map.lookup i cas
                            case mk of
                              Nothing -> fail $ "Unknown type constructor: " ++ deTyConId i
                              Just k  -> return k
kcTy (RWCTyVar v)      = do as <- askAssumps
                            let mk = Map.lookup v as
                            case mk of
                              Nothing -> fail $ "Unknown type variable: " ++ show v
                              Just k  -> return k
kcTy (RWCTyComp tm tv) = do km <- kcTy tm
                            kv <- kcTy tv
                            unify km Kmonad
                            unify kv Kstar
                            return Kstar

kcDataCon :: RWCDataCon -> KCM ()
kcDataCon (RWCDataCon _ ts) = do ks <- mapM kcTy ts
                                 mapM_ (unify Kstar) ks

kcDataDecl :: RWCData -> KCM ()
kcDataDecl (RWCData i tvs _ dcs) = do cas   <- askCAssumps
                                      let k =  cas ! i
                                      kvs   <- replicateM (length tvs) freshkv
                                      unify k (foldr Kfun Kstar (map Kvar kvs))
                                      as    <- liftM Map.fromList $ mapM (\ tv -> freshkv >>= \ v -> return (tv,Kvar v)) tvs
                                      localAssumps (as `Map.union`) (mapM_ kcDataCon dcs)

kcDefn :: RWCDefn -> KCM ()
kcDefn (RWCDefn _ (tvs :-> t) _ _) = do oldsub      <- getKiSub
                                        as          <- liftM Map.fromList $ mapM (\ tv -> freshkv >>= \ v -> return (tv,Kvar v)) tvs
                                        k           <- localAssumps (as `Map.union`) (kcTy t)
                                        unify k Kstar
                                        sub         <- getKiSub
                                        let newsub  =  Map.mapWithKey (\ k _ -> sub ! k) oldsub
                                        putKiSub newsub

-- There is, IIRC, a weird little corner case in the Haskell Report that says
-- if an inferred kind is underconstrained it defaults to *. Not sure if this
-- can ever happen in ReWire, but might as well be safe.
monoize :: Kind -> Kind
monoize (Kfun k1 k2) = Kfun (monoize k1) (monoize k2)
monoize Kstar        = Kstar
monoize Kmonad       = Kmonad
monoize (Kvar _)     = Kstar

redecorate :: KiSub -> RWCData -> KCM RWCData
redecorate s (RWCData i tvs _ dcs) = do cas <- askCAssumps
                                        case Map.lookup i cas of
                                          Just k  -> return (RWCData i tvs (monoize (subst s k)) dcs)
                                          Nothing -> fail $ "redecorate: no such assumption: " ++ show i

basisCAssumps :: RWCProgram -> [(TyConId,Kind)]
basisCAssumps m = map (\ (RWCData i _ k _) -> (i,k)) (dataDecls m)

kc :: [RWCProgram] -> RWCProgram -> KCM RWCProgram
kc ms m = do ncas     <- mapM initDataDecl (dataDecls m)
             let bcas =  concatMap basisCAssumps ms
                 cas  =  Map.fromList (ncas++bcas)
             localCAssumps (cas `Map.union`) $ do
               mapM_ kcDataDecl (dataDecls m)
               mapM_ kcDefn (defns m)
               s   <- getKiSub
               dds <- mapM (redecorate s) (dataDecls m)
               return (m { dataDecls = dds })

kindcheck :: RWCProgram -> Either String RWCProgram
kindcheck m = fmap fst $ runIdentity (runExceptT (runStateT (runReaderT (kc [primBasis] m) (KCEnv Map.empty Map.empty)) (KCState Map.empty 0)))

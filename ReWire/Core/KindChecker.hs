{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.KindChecker (kindcheck) where

import ReWire.Scoping
import ReWire.Core.Syntax
import ReWire.Core.Kinds
--import ReWire.Core.Parser
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Error
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map,(!))
import Control.DeepSeq
import Data.ByteString.Char8 (pack)
import ReWire.Core.PrimTypes

-- Kind checking for Core.
type KiSub = Map (Id Kind) Kind
data KCEnv = KCEnv { as  :: Map (Id RWCTy) Kind, 
                     cas :: Map TyConId Kind } deriving Show
data KCState = KCState { kiSub :: KiSub, ctr :: Int } deriving Show
type Assump = (Id RWCTy,Kind)
type CAssump = (TyConId,Kind)

type KCM = ReaderT KCEnv (StateT KCState (ErrorT String Identity))

localAssumps f = local (\ kce -> kce { as = f (as kce) })
askAssumps = ask >>= \ kce -> return (as kce)
localCAssumps f = local (\ kce -> kce { cas = f (cas kce) })
askCAssumps = ask >>= \ kce -> return (cas kce)
getKiSub = get >>= return . kiSub
updKiSub f = get >>= \ s -> put (s { kiSub = f (kiSub s) })
putKiSub sub = get >>= \ s -> put (s { kiSub = sub })
getCtr = get >>= return . ctr
updCtr f = get >>= \ s -> put (s { ctr = f (ctr s) })
putCtr c = get >>= \ s -> put (s { ctr = c })

freshkv :: KCM (Id Kind)
freshkv = do ctr   <- getCtr
             putCtr (ctr+1)
             let n =  mkId $ "?" ++ show ctr
             updKiSub (Map.insert n (Kvar n))
             return n

initDataDecl :: RWCData -> KCM CAssump
initDataDecl (RWCData i _ _) = do v <- freshkv
                                  return (i,Kvar v)

varBind :: Monad m => Id Kind -> Kind -> m KiSub
varBind u k | k `aeq` Kvar u = return Map.empty
            | u `elem` fv k  = fail $ "occurs check fails in kind checking: " ++ show u ++ ", " ++ show k
            | otherwise      = return (Map.singleton u k)

(@@) :: KiSub -> KiSub -> KiSub
s1@@s2 = {-s1 `deepseq` s2 `deepseq` force -} s
         where s = Map.mapWithKey (\ u t -> subst s1 t) s2 `Map.union` s1

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
kcDataDecl (RWCData i tvs dcs) = do cas   <- askCAssumps
                                    let k =  cas ! i
                                    kvs   <- replicateM (length tvs) freshkv
                                    unify k (foldr Kfun Kstar (map Kvar kvs))
                                    as    <- liftM Map.fromList $ mapM (\ tv -> freshkv >>= \ v -> return (tv,Kvar v)) tvs
                                    localAssumps (as `Map.union`) (mapM_ kcDataCon dcs)

kcDefn :: RWCDefn -> KCM ()
kcDefn (RWCDefn _ (tvs :-> t) _) = do oldsub      <- getKiSub
                                      let oldkeys =  Map.keys oldsub
                                      as          <- liftM Map.fromList $ mapM (\ tv -> freshkv >>= \ v -> return (tv,Kvar v)) tvs
                                      k           <- localAssumps (as `Map.union`) (kcTy t)
                                      unify k Kstar
                                      sub         <- getKiSub
                                      let newsub  =  Map.mapWithKey (\ k _ -> sub ! k) oldsub
                                      putKiSub newsub

kc :: RWCProg -> KCM ()
kc p = do cas <- liftM Map.fromList $ mapM initDataDecl (dataDecls p)
          localCAssumps (cas `Map.union`) (mapM_ kcDataDecl (dataDecls p))
          localCAssumps (cas `Map.union`) (mapM_ kcDefn (defns p))

kindcheck :: RWCProg -> Maybe String
kindcheck p = l2m $ runIdentity (runErrorT (runStateT (runReaderT (kc p) (KCEnv Map.empty as)) (KCState Map.empty 0)))
  where as = Map.fromList [(TyConId "(->)",   Kfun Kstar (Kfun Kstar Kstar)),
                           
                           (TyConId "ReT",    Kfun Kstar (Kfun Kstar (Kfun Kmonad Kmonad))),
                           (TyConId "StT",    Kfun Kstar (Kfun Kmonad Kmonad)),
                           (TyConId "I",      Kmonad),
                           
                           (TyConId "Tuple2", Kfun Kstar (Kfun Kstar Kstar))]
        l2m (Left x)  = Just x
        l2m (Right _) = Nothing

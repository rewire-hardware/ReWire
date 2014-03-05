{-# LANGUAGE TemplateHaskell,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts,UndecidableInstances #-}

module ReWire.Core.KindChecker (kindcheck) where

import ReWire.Core.Syntax
import ReWire.Core.Parser
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Error
import Unbound.LocallyNameless
import Unbound.LocallyNameless.Types (SetPlusBind)
import Data.Maybe (fromJust)

-- Kind checking for Core.

-- Syntax for kinds is not exported; it's only used inside the kind checker.
data Kind = Kvar (Name Kind) | Kstar | Kfun Kind Kind deriving (Eq,Show)

instance Alpha Kind where

instance Subst Kind Kind where
  isvar (Kvar n) = Just (SubstName n)
  isvar _        = Nothing

$(derive [''Kind])

type KiSub = [(Name Kind,Kind)]
data KCEnv = KCEnv { as  :: [Assump], 
                     cas :: [CAssump] } deriving Show
data KCState = KCState { kiSub :: KiSub } deriving Show
type Assump = (Name RWCTy,SetPlusBind [Name Kind] Kind)
type CAssump = (Identifier,SetPlusBind [Name Kind] Kind)

type KCM = FreshMT (ReaderT KCEnv (StateT KCState (ErrorT String Identity)))

localAssumps f = local (\ kce -> kce { as = f (as kce) })
askAssumps = ask >>= \ kce -> return (as kce)
localCAssumps f = local (\ kce -> kce { cas = f (cas kce) })
askCAssumps = ask >>= \ kce -> return (cas kce)
getKiSub = get >>= return . kiSub
updKiSub f = get >>= \ s -> put (s { kiSub = f (kiSub s) })
putKiSub sub = get >>= \ s -> put (s { kiSub = sub })

freshkv :: KCM (Name Kind)
freshkv = do n   <- fresh (s2n "?")
             sub <- getKiSub
             updKiSub ((n,Kvar n):)
             return n

initDataDecl :: RWCData -> KCM CAssump
initDataDecl (RWCData i _) = do v <- freshkv
                                return (i,setbind [] (Kvar v))

varBind :: Monad m => Name Kind -> Kind -> m KiSub
varBind u k | k `aeq` Kvar u = return []
            | u `elem` fv k  = fail $ "occurs check fails in kind checking: " ++ show u ++ ", " ++ show k
            | otherwise      = return [(u,k)]

(@@) :: KiSub -> KiSub -> KiSub
s1@@s2 = [(u,substs s1 t) | (u,t) <- s2] ++ s1

mgu :: Monad m => Kind -> Kind -> m KiSub
mgu (Kfun kl kr) (Kfun kl' kr') = do s1 <- mgu kl kl'
                                     s2 <- mgu (substs s1 kr) (substs s1 kr')
                                     return (s2@@s1)
mgu (Kvar u) k                  = varBind u k
mgu k (Kvar u)                  = varBind u k
mgu Kstar Kstar                 = return []
mgu k1 k2                       = fail $ "kinds do not unify: " ++ show k1 ++ ", " ++ show k2

unify :: Kind -> Kind -> KCM ()
unify k1 k2 = do s <- getKiSub
                 u <- mgu (substs s k1) (substs s k2)
                 updKiSub (u@@)

inst :: [Name Kind] -> Kind -> KCM Kind
inst kvs k = do sub <- mapM (\ kv -> freshkv >>= \ v -> return (kv,Kvar v)) kvs
                return (substs sub k)

kcTy :: RWCTy -> KCM Kind
kcTy (RWCTyApp t1 t2) = do k1 <- kcTy t1
                           k2 <- kcTy t2
                           k  <- freshkv
                           unify k1 (Kfun k2 (Kvar k))
                           return (Kvar k)
kcTy (RWCTyCon i)     = do cas <- askCAssumps
                           let mpk = lookup i cas
                           case mpk of
                             Nothing -> fail $ "Unknown type constructor: " ++ i
                             Just b  -> do (kvs,ka_) <- unbind b
                                           inst kvs ka_
kcTy (RWCTyVar v)     = do as <- askAssumps
                           let mpk = lookup v as
                           case mpk of
                             Nothing -> fail $ "Unknown type variable: " ++ show v
                             Just b  -> do (kvs,ka_) <- unbind b
                                           inst kvs ka_

kcDataCon :: RWCDataCon -> KCM ()
kcDataCon (RWCDataCon _ ts) = do ks <- mapM kcTy ts
                                 mapM_ (unify Kstar) ks

kcDataDecl :: RWCData -> KCM ()
kcDataDecl (RWCData i b) = do (tvs,dcs)  <- unbind b
                              cas        <- askCAssumps
                              let pk     =  fromJust $ lookup i cas
                              (kvs_,k_)  <- unbind pk
                              k          <- inst kvs_ k_
                              kvs        <- replicateM (length tvs) freshkv
                              unify k (foldr Kfun Kstar (map Kvar kvs))
                              as        <- mapM (\ tv -> freshkv >>= \ v -> return (tv,setbind [] (Kvar v))) tvs
                              localAssumps (as++) (mapM_ kcDataCon dcs)

kcDefn :: RWCDefn -> KCM ()
kcDefn (RWCDefn _ (Embed b)) = do (tvs,(t,_)) <- unbind b
                                  as          <- mapM (\ tv -> freshkv >>= \ v -> return (tv,setbind [] (Kvar v))) tvs
                                  k           <- localAssumps (as++) (kcTy t)
                                  unify k Kstar

kc :: RWCProg -> KCM ()
kc p = do ds  <- untrec (defns p)
          cas <- mapM initDataDecl (dataDecls p)
          localCAssumps (cas++) (mapM_ kcDataDecl (dataDecls p))
          localCAssumps (cas++) (mapM_ kcDefn ds)

kindcheck :: RWCProg -> Maybe String
kindcheck p = l2m $ runIdentity (runErrorT (runStateT (runReaderT (runFreshMT (kc p)) (KCEnv [] cas)) (KCState [])))
  where cas      = [("(->)",setbind [] (Kfun Kstar (Kfun Kstar Kstar)))]
        l2m (Left x)  = Just x
        l2m (Right _) = Nothing

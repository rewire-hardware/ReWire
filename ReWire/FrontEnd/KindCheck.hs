{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, LambdaCase, TupleSections #-}

module ReWire.FrontEnd.KindCheck (kindCheck) where

import ReWire.Annotation
import ReWire.Error
import ReWire.FrontEnd.Syntax
import ReWire.Pretty

import Control.Monad.Reader (ReaderT (..), ask, local)
import Control.Monad (replicateM, when)
import Control.Monad.State (StateT (..), get, put, modify)
import Data.Map.Strict (Map, (!))

import qualified Data.Map.Strict as Map

import Unbound.Generics.LocallyNameless (fresh, substs, aeq, Subst, string2Name)

subst :: Subst b a => Map (Name b) b -> a -> a
subst ss = substs (Map.assocs ss)

-- Kind checking for Core.
type KiSub = Map (Name Kind) Kind
data KCEnv = KCEnv
      { as  :: Map (Name RWMTy) Kind
      , cas :: Map TyConId Kind
      } deriving Show

type KCM m = ReaderT KCEnv (StateT KiSub m)

localAssumps :: SyntaxError m => (Map (Name RWMTy) Kind -> Map (Name RWMTy) Kind) -> KCM m a -> KCM m a
localAssumps f = local $ \ kce -> kce { as = f (as kce) }

askAssumps :: SyntaxError m => KCM m (Map (Name RWMTy) Kind)
askAssumps = ask >>= \ kce -> return (as kce)

localCAssumps :: SyntaxError m => (Map TyConId Kind -> Map TyConId Kind) -> KCM m a -> KCM m a
localCAssumps f = local $ \ kce -> kce { cas = f (cas kce) }

askCAssumps :: SyntaxError m => KCM m (Map TyConId Kind)
askCAssumps = ask >>= \ kce -> return $ cas kce

freshkv :: (Fresh m, SyntaxError m) => KCM m Kind
freshkv = do
      n <- fresh $ string2Name "?"
      modify $ Map.insert n $ KVar n
      return $ KVar n

varBind :: (Fresh m, SyntaxError m) => Annote -> Name Kind -> Kind -> KCM m KiSub
varBind an u k
      | k `aeq` KVar u = return mempty
      | u `elem` fv k = failAt an $ "Occurs check fails in kind checking: " ++ show u ++ ", " ++ prettyPrint k
      | otherwise      = return $ Map.singleton u k

(@@) :: KiSub -> KiSub -> KiSub
s1 @@ s2 = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

mgu :: (Fresh m, SyntaxError m) => Annote -> Kind -> Kind -> KCM m KiSub
mgu an (KFun kl kr) (KFun kl' kr') = do
      s1 <- mgu an kl kl'
      s2 <- mgu an (subst s1 kr) $ subst s1 kr'
      return $ s2 @@ s1
mgu an (KVar u) k                  = varBind an u k
mgu an k (KVar u)                  = varBind an u k
mgu _ KStar KStar                  = return mempty
mgu _ KMonad KMonad                = return mempty
mgu an k1 k2                       = failAt an $ "Kinds do not unify: " ++ prettyPrint k1 ++ ", " ++ prettyPrint k2

unify :: (Fresh m, SyntaxError m) => Annote -> Kind -> Kind -> KCM m ()
unify an k1 k2 = do
      s <- get
      u <- mgu an (subst s k1) $ subst s k2
      modify (u @@)

kcTy :: (Fresh m, SyntaxError m) => RWMTy -> KCM m Kind
kcTy = \ case
      RWMTyApp an t1 t2  -> do
            k1 <- kcTy t1
            k2 <- kcTy t2
            k  <- freshkv
            unify an k1 $ KFun k2 k
            return k
      RWMTyCon an i      -> do
            cas <- askCAssumps
            case Map.lookup i cas of
                  Nothing -> failAt an "Unknown type constructor"
                  Just k  -> return k
      RWMTyVar an v      -> do
            as <- askAssumps
            case Map.lookup v as of
                  Nothing -> failAt an "Unknown type variable"
                  Just k  -> return k
      RWMTyComp an tm tv -> do
            km <- kcTy tm
            kv <- kcTy tv
            unify an km KMonad
            unify an kv KStar
            return KStar
      RWMTyBlank an      -> failAt an "Something went wrong in the kind checker"

kcDataCon :: (Fresh m, SyntaxError m) => RWMDataCon -> KCM m ()
kcDataCon (RWMDataCon an _ ts) = do
      ks <- mapM kcTy ts
      mapM_ (unify an KStar) ks

kcDataDecl :: (Fresh m, SyntaxError m) => RWMData -> KCM m ()
kcDataDecl (RWMData an i tvs dk dcs) = do
      cas   <- askCAssumps
      let k =  cas ! i
      kvs   <- replicateM (length tvs) freshkv
      -- Skip builtins.
      when (dk == kblank) $ do
            unify an k $ foldr KFun KStar kvs
            as    <- Map.fromList <$> mapM (\ tv -> freshkv >>= \ v -> return (tv, v)) tvs
            localAssumps (as `Map.union`) (mapM_ kcDataCon dcs)

kcDefn :: (Fresh m, SyntaxError m) => RWMDefn -> KCM m ()
kcDefn (RWMDefn an _ (Embed (Poly pt)) _ _) = do
      (tvs, t)    <- unbind pt
      oldsub      <- get
      as          <- Map.fromList <$> mapM (\ tv -> freshkv >>= \ v -> return (tv, v)) tvs
      k           <- localAssumps (as `Map.union`) $ kcTy t
      unify an k KStar
      sub         <- get
      let newsub  =  Map.mapWithKey (\ k _ -> sub ! k) oldsub
      put newsub

-- There is, IIRC, a weird little corner case in the Haskell Report that says
-- if an inferred kind is underconstrained it defaults to *. Not sure if this
-- can ever happen in ReWire, but might as well be safe.
monoize :: Kind -> Kind
monoize = \ case
      KFun k1 k2 -> KFun (monoize k1) $ monoize k2
      KStar      -> KStar
      KMonad     -> KMonad
      KVar _     -> KStar
      KBlank     -> KStar

redecorate :: SyntaxError m => KiSub -> RWMData -> KCM m RWMData
redecorate s (RWMData an i tvs _ dcs) = do
      cas <- askCAssumps
      case Map.lookup i cas of
            Just k  -> return $ RWMData an i tvs (monoize $ subst s k) dcs
            Nothing -> failAt an $ "Redecorate: no such assumption: " ++ show i

assump :: (Fresh m, SyntaxError m) => RWMData -> KCM m (TyConId, Kind)
assump (RWMData _ i _ k _)
      | k /= kblank = return (i, k)
      | otherwise   = (i,) <$> freshkv

kc :: (Fresh m, SyntaxError m) => RWMProgram -> KCM m RWMProgram
kc p = do
      cas      <- Map.fromList <$> (mapM assump $ dataDecls p)
      localCAssumps (cas `Map.union`) $ do
            defns' <- untrec $ defns p
            mapM_ kcDataDecl $ dataDecls p
            mapM_ kcDefn defns'
            s   <- get
            dds <- mapM (redecorate s) $ dataDecls p
            return p { dataDecls = dds }

kindCheck :: (Fresh m, SyntaxError m) => RWMProgram -> m RWMProgram
kindCheck m = fmap fst $ runStateT (runReaderT (kc m) (KCEnv mempty mempty)) mempty

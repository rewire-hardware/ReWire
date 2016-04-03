{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, LambdaCase #-}

module ReWire.FrontEnd.KindCheck (kindcheck) where

import ReWire.Annotation
import ReWire.Error
import ReWire.FrontEnd.Kinds
import ReWire.FrontEnd.PrimBasis
import ReWire.FrontEnd.Syntax
import ReWire.Pretty
import ReWire.Scoping

import Control.Monad (replicateM)
import Control.Monad.Reader (ReaderT (..), ask, local)
import Control.Monad.State (StateT (..), get, put)
import Data.Map.Strict (Map, (!))

import qualified Data.Map.Strict as Map

-- Kind checking for Core.
type KiSub = Map (Id Kind) Kind
data KCEnv = KCEnv
      { as  :: Map (Id RWCTy) Kind
      , cas :: Map TyConId Kind
      } deriving Show
data KCState = KCState { kiSub :: KiSub, ctr :: Int } deriving Show

type KCM m = ReaderT KCEnv (StateT KCState m)

localAssumps :: SyntaxError m => (Map (Id RWCTy) Kind -> Map (Id RWCTy) Kind) -> KCM m a -> KCM m a
localAssumps f = local $ \ kce -> kce { as = f (as kce) }

askAssumps :: SyntaxError m => KCM m (Map (Id RWCTy) Kind)
askAssumps = ask >>= \ kce -> return (as kce)

localCAssumps :: SyntaxError m => (Map TyConId Kind -> Map TyConId Kind) -> KCM m a -> KCM m a
localCAssumps f = local $ \ kce -> kce { cas = f (cas kce) }

askCAssumps :: SyntaxError m => KCM m (Map TyConId Kind)
askCAssumps = ask >>= \ kce -> return $ cas kce

getKiSub :: SyntaxError m => KCM m KiSub
getKiSub = get >>= return . kiSub

updKiSub :: SyntaxError m => (KiSub -> KiSub) -> KCM m ()
updKiSub f = get >>= \ s -> put s { kiSub = f $ kiSub s }

putKiSub :: SyntaxError m => KiSub -> KCM m ()
putKiSub sub = get >>= \ s -> put s { kiSub = sub }

getCtr :: SyntaxError m => KCM m Int
getCtr = get >>= return . ctr

putCtr :: SyntaxError m => Int -> KCM m ()
putCtr c = get >>= \ s -> put s { ctr = c }

freshkv :: SyntaxError m => KCM m (Id Kind)
freshkv = do
      ctr <- getCtr
      putCtr $ ctr + 1
      let n = mkId $ "?" ++ show ctr
      updKiSub $ Map.insert n $ Kvar n
      return n

initDataDecl :: SyntaxError m => RWMData -> KCM m (TyConId, Kind)
initDataDecl (RWMData _ i _ _ _) = do
      v <- freshkv
      return (i, Kvar v)

varBind :: SyntaxError m => Annote -> Id Kind -> Kind -> KCM m KiSub
varBind an u k
      | k `aeq` Kvar u = return Map.empty
      | u `elem` fv k  = failAt an $ "Occurs check fails in kind checking: " ++ prettyPrint u ++ ", " ++ prettyPrint k
      | otherwise      = return $ Map.singleton u k

(@@) :: KiSub -> KiSub -> KiSub
s1 @@ s2 = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

mgu :: SyntaxError m => Annote -> Kind -> Kind -> KCM m KiSub
mgu an (Kfun kl kr) (Kfun kl' kr') = do
      s1 <- mgu an kl kl'
      s2 <- mgu an (subst s1 kr) $ subst s1 kr'
      return $ s2 @@ s1
mgu an (Kvar u) k                  = varBind an u k
mgu an k (Kvar u)                  = varBind an u k
mgu _ Kstar Kstar                  = return Map.empty
mgu _ Kmonad Kmonad                = return Map.empty
mgu an k1 k2                       = failAt an $ "Kinds do not unify: " ++ prettyPrint k1 ++ ", " ++ prettyPrint k2

unify :: SyntaxError m => Annote -> Kind -> Kind -> KCM m ()
unify an k1 k2 = do
      s <- getKiSub
      u <- mgu an (subst s k1) (subst s k2)
      updKiSub (u @@)

kcTy :: SyntaxError m => RWCTy -> KCM m Kind
kcTy = \ case
      RWCTyApp an t1 t2  -> do
            k1 <- kcTy t1
            k2 <- kcTy t2
            k  <- freshkv
            unify an k1 $ Kfun k2 $ Kvar k
            return $ Kvar k
      RWCTyCon an i      -> do
            cas <- askCAssumps
            case Map.lookup i cas of
                  Nothing -> failAt an "Unknown type constructor"
                  Just k  -> return k
      RWCTyVar an v      -> do
            as <- askAssumps
            case Map.lookup v as of
                  Nothing -> failAt an "Unknown type variable"
                  Just k  -> return k
      RWCTyComp an tm tv -> do
            km <- kcTy tm
            kv <- kcTy tv
            unify an km Kmonad
            unify an kv Kstar
            return Kstar

kcDataCon :: SyntaxError m => RWCDataCon -> KCM m ()
kcDataCon (RWCDataCon an _ ts) = do
      ks <- mapM kcTy ts
      mapM_ (unify an Kstar) ks

kcDataDecl :: SyntaxError m => RWMData -> KCM m ()
kcDataDecl (RWMData an i tvs _ dcs) = do
      cas   <- askCAssumps
      let k =  cas ! i
      kvs   <- replicateM (length tvs) freshkv
      unify an k $ foldr (Kfun . Kvar) Kstar kvs
      as    <- Map.fromList <$> mapM (\ tv -> freshkv >>= \ v -> return (tv, Kvar v)) tvs
      localAssumps (as `Map.union`) (mapM_ kcDataCon dcs)

kcDefn :: SyntaxError m => RWMDefn -> KCM m ()
kcDefn (RWMDefn an _ (tvs :-> t) _ _ _) = do
      oldsub      <- getKiSub
      as          <- Map.fromList <$> mapM (\ tv -> freshkv >>= \ v -> return (tv, Kvar v)) tvs
      k           <- localAssumps (as `Map.union`) $ kcTy t
      unify an k Kstar
      sub         <- getKiSub
      let newsub  =  Map.mapWithKey (\ k _ -> sub ! k) oldsub
      putKiSub newsub

-- There is, IIRC, a weird little corner case in the Haskell Report that says
-- if an inferred kind is underconstrained it defaults to *. Not sure if this
-- can ever happen in ReWire, but might as well be safe.
monoize :: Kind -> Kind
monoize = \ case
      Kfun k1 k2 -> Kfun (monoize k1) $ monoize k2
      Kstar      -> Kstar
      Kmonad     -> Kmonad
      Kvar _     -> Kstar

redecorate :: SyntaxError m => KiSub -> RWMData -> KCM m RWMData
redecorate s (RWMData an i tvs _ dcs) = do
      cas <- askCAssumps
      case Map.lookup i cas of
            Just k  -> return $ RWMData an i tvs (monoize $ subst s k) dcs
            Nothing -> failAt an $ "Redecorate: no such assumption: " ++ show i

basisCAssumps :: RWMProgram -> [(TyConId, Kind)]
basisCAssumps m = map (\ (RWMData _ i _ k _) -> (i, k)) $ dataDecls m

kc :: SyntaxError m => [RWMProgram] -> RWMProgram -> KCM m RWMProgram
kc ms m = do
      ncas      <- mapM initDataDecl $ dataDecls m
      let  bcas =  concatMap basisCAssumps ms
           cas  =  Map.fromList $ ncas ++ bcas
      localCAssumps (cas `Map.union`) $ do
            mapM_ kcDataDecl $ dataDecls m
            mapM_ kcDefn $ defns m
            s   <- getKiSub
            dds <- mapM (redecorate s) $ dataDecls m
            return m { dataDecls = dds }

kindcheck :: (SyntaxError m, Functor m) => RWMProgram -> m RWMProgram
kindcheck m = fmap fst $ runStateT (runReaderT (kc [primBasis] m) (KCEnv Map.empty Map.empty)) $ KCState Map.empty 0

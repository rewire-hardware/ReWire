{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Crust.KindCheck (kindCheck) where

import safe ReWire.Annotation
import safe ReWire.Error
import safe ReWire.Crust.Syntax
import safe ReWire.Unbound (fresh, substs, aeq, Subst, s2n, n2s)
import safe ReWire.Pretty

import safe Control.Monad.Reader (ReaderT (..), local, asks)
import safe Control.Monad.State (evalStateT, StateT (..), get, modify)
import safe Data.Map.Strict (Map)
import TextShow (showt)

import safe qualified Data.Map.Strict as Map

subst :: Subst b a => Map (Name b) b -> a -> a
subst = substs . Map.assocs

-- Kind checking for Core.
type KiSub = Map (Name Kind) Kind
newtype KCEnv = KCEnv { cas :: Map (Name TyConId) Kind }
      deriving Show

type KCM m = ReaderT KCEnv (StateT KiSub m)

localCAssumps :: MonadError AstError m => (Map (Name TyConId) Kind -> Map (Name TyConId) Kind) -> KCM m a -> KCM m a
localCAssumps f = local $ \ kce -> kce { cas = f (cas kce) }

askCAssumps :: MonadError AstError m => KCM m (Map (Name TyConId) Kind)
askCAssumps = asks cas

freshkv :: (Fresh m, MonadError AstError m) => KCM m Kind
freshkv = do
      n <- fresh $ s2n "?"
      modify $ Map.insert n $ KVar n
      pure $ KVar n

varBind :: (Fresh m, MonadError AstError m) => Annote -> Name Kind -> Kind -> KCM m KiSub
varBind an u k
      | k `aeq` KVar u = pure mempty
      | u `elem` fv k  = failAt an $ "Occurs check fails in kind checking: " <> showt u <> ", " <> prettyPrint k
      | otherwise      = pure $ Map.singleton u k

(@@) :: KiSub -> KiSub -> KiSub
s1 @@ s2 = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

mgu :: (Fresh m, MonadError AstError m) => Annote -> Kind -> Kind -> KCM m KiSub
mgu an (KFun kl kr) (KFun kl' kr') = do
      s1 <- mgu an kl kl'
      s2 <- mgu an (subst s1 kr) $ subst s1 kr'
      pure $ s2 @@ s1
mgu an (KVar u) k                  = varBind an u k
mgu an k (KVar u)                  = varBind an u k
mgu _ KStar KStar                  = pure mempty
mgu an k1 k2                       = failAt an $ "Kinds do not unify: " <> prettyPrint k1 <> ", " <> prettyPrint k2

unify :: (Fresh m, MonadError AstError m) => Annote -> Kind -> Kind -> KCM m ()
unify an k1 k2 = do
      s <- get
      u <- mgu an (subst s k1) $ subst s k2
      modify (u @@)

kcTy :: (Fresh m, MonadError AstError m) => Ty -> KCM m Kind
kcTy = \ case
      TyApp an t1 t2  -> do
            k1 <- kcTy t1
            k2 <- kcTy t2
            k  <- freshkv
            unify an k1 $ KFun k2 k
            pure k
      TyCon an i      -> case n2s i of
            -- This special case really shouldn't be
            -- necessary (bug?).
            "->" -> pure $ KFun KStar $ KFun KStar KStar
            _    -> do
                  cas <- askCAssumps
                  maybe (failAt an $ "Unknown type constructor: " <> n2s i) pure
                        $ Map.lookup i cas
      TyVar _ k _     -> pure k
      TyBlank _       -> freshkv

-- | Only needed for debugging.
-- kcDataCon :: (Fresh m, MonadError AstError m) => DataCon -> KCM m ()
-- kcDataCon (DataCon an _ (Embed (Poly t))) = do
--       (_, t') <- unbind t
--       k       <- kcTy t'
--       unify an k KStar

kcDataDecl :: (Fresh m, MonadError AstError m) => DataDefn -> KCM m ()
-- kcDataDecl (DataDefn _ _ _ cs) = mapM_ kcDataCon cs
kcDataDecl _ = pure ()

kcDefn :: (Fresh m, MonadError AstError m) => Defn -> KCM m ()
kcDefn (Defn an _ (Embed (Poly t)) _ _) = do
      (_, t') <- unbind t
      k       <- kcTy t'
      unify an k KStar

-- There is, IIRC, a weird little corner case in the Haskell Report that says
-- if an inferred kind is underconstrained it defaults to *. Not sure if this
-- can ever happen in ReWire, but might as well be safe.
monoize :: Kind -> Kind
monoize = \ case
      KFun k1 k2 -> KFun (monoize k1) $ monoize k2
      KStar      -> KStar
      KVar _     -> KStar

redecorate :: MonadError AstError m => KiSub -> DataDefn -> KCM m DataDefn
redecorate s (DataDefn an i _ co cs) = do
      cas <- askCAssumps
      case Map.lookup i cas of
            Just k  -> pure $ DataDefn an i (monoize $ subst s k) co cs
            Nothing -> failAt an $ "Redecorate: no such assumption: " <> showt i

assump :: (Fresh m, MonadError AstError m) => DataDefn -> KCM m (Name TyConId, Kind)
assump = \ case
      DataDefn _ i k False _ -> pure (i, k)
      DataDefn _ i k True _  -> (i,) <$> estimate k -- TODO(chathhorn): need the real kind here.

estimate :: (Fresh m, MonadError AstError m) => Kind -> KCM m Kind
estimate = \ case
      KFun k1 k2  -> KFun k1 <$> estimate k2
      _           -> freshkv

kc :: (Fresh m, MonadError AstError m) => FreeProgram -> KCM m FreeProgram
kc (ts, vs) = do
      cas  <-  Map.fromList <$> mapM assump ts
      localCAssumps (cas `Map.union`) $ do
            mapM_ kcDataDecl ts
            mapM_ kcDefn vs
            s   <- get
            ts' <- mapM (redecorate s) ts
            pure (ts', vs)

kindCheck :: (Fresh m, MonadError AstError m) => FreeProgram -> m FreeProgram
kindCheck m = evalStateT (runReaderT (kc m) $ KCEnv mempty) mempty

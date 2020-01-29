{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, LambdaCase #-}
{-# LANGUAGE Safe #-}
module ReWire.FrontEnd.KindCheck (kindCheck) where

import ReWire.Annotation
import ReWire.Error
import ReWire.FrontEnd.Syntax
import ReWire.FrontEnd.Unbound (fresh, substs, aeq, Subst, string2Name, name2String, runFreshMT)
import ReWire.Pretty

import Control.Monad.Reader (ReaderT (..), ask, local)
import Control.Monad.State (evalStateT, StateT (..), get, modify)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map

subst :: Subst b a => Map (Name b) b -> a -> a
subst ss = substs (Map.assocs ss)

-- Kind checking for Core.
type KiSub = Map (Name Kind) Kind
newtype KCEnv = KCEnv { cas :: Map (Name TyConId) Kind }
      deriving Show

type KCM m = ReaderT KCEnv (StateT KiSub m)

localCAssumps :: MonadError AstError m => (Map (Name TyConId) Kind -> Map (Name TyConId) Kind) -> KCM m a -> KCM m a
localCAssumps f = local $ \ kce -> kce { cas = f (cas kce) }

askCAssumps :: MonadError AstError m => KCM m (Map (Name TyConId) Kind)
askCAssumps = ask >>= \ kce -> return $ cas kce

freshkv :: (Fresh m, MonadError AstError m) => KCM m Kind
freshkv = do
      n <- fresh $ string2Name "?"
      modify $ Map.insert n $ KVar n
      return $ KVar n

varBind :: (Fresh m, MonadError AstError m) => Annote -> Name Kind -> Kind -> KCM m KiSub
varBind an u k
      | k `aeq` KVar u = return mempty
      | u `elem` fv k  = failAt an $ "Occurs check fails in kind checking: " ++ show u ++ ", " ++ prettyPrint k
      | otherwise      = return $ Map.singleton u k

(@@) :: KiSub -> KiSub -> KiSub
s1 @@ s2 = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

mgu :: (Fresh m, MonadError AstError m) => Annote -> Kind -> Kind -> KCM m KiSub
mgu an (KFun kl kr) (KFun kl' kr') = do
      s1 <- mgu an kl kl'
      s2 <- mgu an (subst s1 kr) $ subst s1 kr'
      return $ s2 @@ s1
mgu an (KVar u) k                  = varBind an u k
mgu an k (KVar u)                  = varBind an u k
mgu _ KStar KStar                  = return mempty
mgu an k1 k2                       = failAt an $ "Kinds do not unify: " ++ prettyPrint k1 ++ ", " ++ prettyPrint k2

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
            return k
      TyCon an i      -> case name2String i of
                              -- This special case really shouldn't be
                              -- necessary (bug?).
                              "->" -> return (KFun KStar (KFun KStar KStar))
                              _    -> do
                                cas <- askCAssumps
                                case Map.lookup i cas of
                                     Nothing -> failAt an $ "Unknown type constructor: " ++ name2String i
                                     Just k  -> return k
      TyVar _ k _     -> return k
      TyBlank an      -> failAt an "Something went wrong in the kind checker"

-- | Only needed for debugging.
-- kcDataCon :: (Fresh m, MonadError AstError m) => DataCon -> KCM m ()
-- kcDataCon (DataCon an _ (Embed (Poly t))) = do
--       (_, t') <- unbind t
--       k       <- kcTy t'
--       unify an k KStar

kcDataDecl :: (Fresh m, MonadError AstError m) => DataDefn -> KCM m ()
-- kcDataDecl (DataDefn _ _ _ cs) = mapM_ kcDataCon cs
kcDataDecl _ = return ()

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
redecorate s (DataDefn an i _ cs) = do
      cas <- askCAssumps
      case Map.lookup i cas of
            Just k  -> return $ DataDefn an i (monoize $ subst s k) cs
            Nothing -> failAt an $ "Redecorate: no such assumption: " ++ show i

assump :: DataDefn -> (Name TyConId, Kind)
assump (DataDefn _ i k _) = (i, k)

kc :: (Fresh m, MonadError AstError m) => FreeProgram -> KCM m FreeProgram
kc (ts, vs) = do
      let cas  =  Map.fromList $ map assump ts
      localCAssumps (cas `Map.union`) $ do
            mapM_ kcDataDecl ts
            mapM_ kcDefn vs
            s   <- get
            ts' <- mapM (redecorate s) ts
            return (ts', vs)

kindCheck :: MonadError AstError m => FreeProgram -> m FreeProgram
kindCheck m = runFreshMT $ evalStateT (runReaderT (kc m) $ KCEnv mempty) mempty

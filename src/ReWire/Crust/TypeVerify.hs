{-# LANGUAGE FlexibleContexts, LambdaCase, TupleSections #-}
{-# LANGUAGE Safe #-}
--
-- This type checker is based loosely on Mark Jones's "Typing Haskell in
-- Haskell", though since we don't have type classes in core it is much
-- simpler.
--
module ReWire.Crust.TypeVerify (typeVerify) where

import ReWire.Annotation
import ReWire.Error
import ReWire.Unbound
      ( fresh, substs, aeq, Subst
      , n2s, s2n
      )
import ReWire.Pretty
import ReWire.Crust.Syntax

import Control.Monad.Reader (ReaderT (..), local, asks)
import Control.Monad.State (evalStateT, StateT (..), get, put, modify)
import Control.Monad (zipWithM_)
import Data.List (foldl')
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as Map

subst :: Subst b a => HashMap (Name b) b -> a -> a
subst = substs . Map.toList

-- Type checker for core.

type TySub = HashMap (Name Ty) Ty
data TCEnv = TCEnv
      { as  :: HashMap (Name Exp) Poly
      , cas :: HashMap (Name DataConId) Poly
      } deriving Show

type TCM m = ReaderT TCEnv (StateT TySub m)

typeVerify :: (Fresh m, MonadError AstError m) => FreeProgram -> m FreeProgram
typeVerify (ts, vs) = evalStateT (runReaderT (tc (ts, vs)) $ TCEnv mempty mempty) mempty

localAssumps :: MonadError AstError m => (HashMap (Name Exp) Poly -> HashMap (Name Exp) Poly) -> TCM m a -> TCM m a
localAssumps f = local (\ tce -> tce { as = f (as tce) })

localCAssumps :: MonadError AstError m => (HashMap (Name DataConId) Poly -> HashMap (Name DataConId) Poly) -> TCM m a -> TCM m a
localCAssumps f = local (\ tce -> tce { cas = f (cas tce) })

freshv :: (Fresh m, MonadError AstError m) => TCM m Ty
freshv = do
      n <- fresh $ s2n "?"
      let tv = TyVar (MsgAnnote "TypeVerify: freshv") kblank n
      modify $ Map.insert n tv
      pure tv

(@@) :: TySub -> TySub -> TySub
s1 @@ s2 = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

isFlex :: Name a -> Bool
isFlex = (== '?') . head . n2s

varBind :: MonadError AstError m => Annote -> Name Ty -> Ty -> TCM m TySub
varBind an u t | t `aeq` TyVar noAnn kblank u = pure mempty
               | u `elem` fv t                = failAt an $ "TypeVerify: occurs check fails: " ++ show u ++ ", " ++ prettyPrint t
               | otherwise                    = pure $ Map.singleton u t

mgu :: MonadError AstError m => Annote -> Ty -> Ty -> TCM m TySub
mgu an (TyApp _ tl tr) (TyApp _ tl' tr')                        = do
      s1 <- mgu an tl tl'
      s2 <- mgu an (subst s1 tr) $ subst s1 tr'
      pure $ s2 @@ s1
mgu an (TyVar _ _ u)   t             | isFlex u         = varBind an u t
mgu an t               (TyVar _ _ u) | isFlex u         = varBind an u t
mgu _  (TyCon _ c1)    (TyCon _ c2)  | n2s c1 == n2s c2 = pure mempty
mgu _  TyVar {}   TyVar {}                              = pure mempty -- TODO(chathhorn): maybe something more could be done here.
mgu _  TyBlank {}  _                                    = pure mempty -- TODO(chathhorn): maybe something more could be done here.
mgu _  _           TyBlank {}                           = pure mempty -- TODO(chathhorn): maybe something more could be done here.
mgu an t1 t2 = failAt an $ "TypeVerify: types do not unify: " ++ prettyPrint t1 ++ ", " ++ prettyPrint t2

unify :: MonadError AstError m => Annote -> Ty -> Ty -> TCM m ()
unify an t1 t2 = do
      s <- get
      u <- mgu an (subst s t1) $ subst s t2
      modify (u@@)

inst :: (Fresh m, MonadError AstError m) => Poly -> TCM m Ty
inst (Poly pt) = do
      (tvs, t) <- unbind pt
      sub      <- Map.fromList <$> mapM (\ tv -> (tv,) <$> freshv) tvs
      pure $ subst sub t

patAssumps :: Pat -> HashMap (Name Exp) Poly
patAssumps = flip patAssumps' mempty
      where patAssumps' :: Pat -> HashMap (Name Exp) Poly -> HashMap (Name Exp) Poly
            patAssumps' = \ case
                  PatCon _ _ _ ps      -> flip (foldr patAssumps') ps
                  PatVar _ (Embed t) n -> Map.insert n $ [] `poly` t

patHoles :: Fresh m => MatchPat -> m (HashMap (Name Exp) Poly)
patHoles = flip patHoles' $ pure mempty
      where patHoles' :: Fresh m => MatchPat -> m (HashMap (Name Exp) Poly) -> m (HashMap (Name Exp) Poly)
            patHoles' = \ case
                  MatchPatCon _ _ _ ps -> flip (foldr patHoles') ps
                  MatchPatVar _ t      -> (flip Map.insert ([] `poly` t) <$> fresh (s2n "PHOLE") <*>)

tcPat :: (Fresh m, MonadError AstError m) => Ty -> Pat -> TCM m ()
tcPat t = \ case
      PatVar {}                -> pure ()
      PatCon an _ (Embed i) ps -> do
            cas     <- asks cas
            case Map.lookup i cas of
                  Nothing  -> failAt an $ "TypeVerify: unknown constructor: " ++ prettyPrint i
                  Just pta -> do
                        ta               <- inst pta
                        let (targs, tres) = flattenArrow ta
                        if length ps /= length targs
                        then failAt an "TypeVerify: pattern is not applied to enough arguments"
                        else do
                              zipWithM_ tcPat targs ps
                              unify an t tres

tcMatchPat :: (Fresh m, MonadError AstError m) => Ty -> MatchPat -> TCM m ()
tcMatchPat t = \ case
      MatchPatVar _ _ -> pure ()
      MatchPatCon an _ i ps -> do
            cas     <- asks cas
            case Map.lookup i cas of
                  Nothing  -> failAt an $ "TypeVerify: unknown constructor: " ++ prettyPrint i
                  Just pta -> do
                        ta               <- inst pta
                        let (targs, tres) = flattenArrow ta
                        if length ps /= length targs
                        then failAt an "TypeVerify: pattern is not applied to enough arguments"
                        else do
                              zipWithM_ tcMatchPat targs ps
                              unify an t tres

tcExp :: (Fresh m, MonadError AstError m) => Exp -> TCM m ()
tcExp = \ case
      e@App {}                -> do
            let (ef : es) =  flattenApp e
            tcExp ef
            mapM_ tcExp es
            tv            <- freshv
            let tf'       =  foldr (arr . typeOf) tv es
            unify (ann e) (typeOf ef) tf'
      Lam an t e              -> do
            (x, e') <- unbind e
            tvx     <- freshv
            localAssumps (Map.insert x ([] `poly` tvx)) $ tcExp e'
            unify an t tvx
      Var an t v              -> do
            as <- asks as
            case Map.lookup v as of
                  Nothing -> failAt an $ "TypeVerify: unknown variable: " ++ show v
                  Just pt -> inst pt >>= unify an t
      Con an t i              -> do
            cas <- asks cas
            case Map.lookup i cas of
                  Nothing -> failAt an $ "TypeVerify: unknown constructor: " ++ prettyPrint i
                  Just pt -> inst pt >>= unify an t
      Case an t e e1 e2       -> do
            (p, e1') <- unbind e1
            tcExp e
            tcPat (typeOf e) p
            localAssumps (patAssumps p `Map.union`) $ tcExp e1'
            unify an t $ typeOf e1'
            case e2 of
                  Nothing -> pure ()
                  Just e2 -> tcExp e2 >> unify an t (typeOf e2)
      Match an t e p f as e2  -> do
            tcExp e
            tcMatchPat (typeOf e) p
            holes <- patHoles p
            let e' = mkApp an f as $ map fst $ Map.toList holes
            localAssumps (holes `Map.union`) $ tcExp e'
            unify an t $ typeOf e'
            case e2 of
                  Nothing -> pure ()
                  Just e2 -> tcExp e2 >> unify an t (typeOf e2)
      NativeVHDL _ _ Error {} -> pure ()
      NativeVHDL _ _ e        -> tcExp e
      Error {}                -> pure ()

mkApp :: Annote -> Exp -> [Exp] -> [Name Exp] -> Exp
mkApp an f as holes = foldl' (App an) f
      $ as ++ map (Var an $ TyBlank an) holes

tcDefn :: (Fresh m, MonadError AstError m) => Defn -> TCM m ()
tcDefn (Defn an _ (Embed (Poly pt)) _ (Embed e)) = do
      put mempty
      (_, t)   <- unbind pt
      (vs, e') <- unbind e
      localAssumps (Map.union $ Map.fromList $ zip vs $ map (poly []) $ paramTys t) $ tcExp e'
      unify an (iterate arrowRight t !! length vs) (typeOf e')
      put mempty

tc :: (Fresh m, MonadError AstError m) => FreeProgram -> TCM m FreeProgram
tc (ts, vs) = do
      let as   =  foldr defnAssump mempty vs
          cas  =  foldr dataDeclAssumps mempty ts
      localAssumps (as `Map.union`) $ localCAssumps (cas `Map.union`) $ mapM_ tcDefn vs
      pure (ts, vs)

      where defnAssump :: Defn -> HashMap (Name Exp) Poly -> HashMap (Name Exp) Poly
            defnAssump (Defn _ n (Embed pt) _ _) = Map.insert n pt

            dataDeclAssumps :: DataDefn -> HashMap (Name DataConId) Poly -> HashMap (Name DataConId) Poly
            dataDeclAssumps (DataDefn _ _ _ cs) = flip (foldr dataConAssump) cs

            dataConAssump :: DataCon -> HashMap (Name DataConId) Poly -> HashMap (Name DataConId) Poly
            dataConAssump (DataCon _ i (Embed t)) = Map.insert i t

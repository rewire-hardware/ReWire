{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
--
-- This type checker is based loosely on Mark Jones's "Typing Haskell in
-- Haskell", though since we don't have type classes in core it is much
-- simpler.
--
module ReWire.Crust.TypeCheck (typeCheck, typeCheckDefn, untype) where

import ReWire.Annotation
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Fix (fixOn)
import ReWire.Unbound (fresh, substs, aeq, Subst, n2s, s2n, unsafeUnbind)
import ReWire.Pretty
import ReWire.Crust.Syntax
import ReWire.SYB (runPureT, transform, runQ, query)

import Control.DeepSeq (deepseq, force)
import Control.Monad (zipWithM, foldM)
import Control.Monad.Catch (MonadCatch (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (ReaderT (..), local, asks)
import Control.Monad.State (evalStateT, StateT (..), get, put, modify, MonadState)
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import TextShow (TextShow (..))

import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

subst :: Subst b a => HashMap (Name b) b -> a -> a
subst = substs . Map.toList

-- Type checker for core.

type TySub = HashMap (Name Ty) Ty
type Concretes = HashMap (Name Exp, Ty) (Name Exp)
data TCEnv = TCEnv
      { as        :: !(HashMap (Name Exp) Poly)
      , cas       :: !(HashMap (Name DataConId) Poly)
      } deriving Show

instance Semigroup TCEnv where
      TCEnv a b <> TCEnv a' b' = TCEnv (a <> a') $ b <> b'

instance Monoid TCEnv where
      mempty = TCEnv mempty mempty

type TCM m = ReaderT TCEnv (StateT TySub m)

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll a = map snd . filter ((== a) . fst)

typeCheckDefn :: (Fresh m, MonadError AstError m) => [DataDefn] -> [Defn] -> Defn -> m Defn
typeCheckDefn ts vs d = evalTCM $ withAssumps ts vs $ tcDefn d

evalTCM :: Monad m => TCM m a -> m a
evalTCM m = evalStateT (runReaderT m mempty) mempty

typeCheck :: (Fresh m, MonadError AstError m, MonadCatch m) => FreeProgram -> m FreeProgram
typeCheck (ts, syns, vs) = (ts, syns, ) <$> evalTCM tc
      where conc :: Concretes -> Defn -> [Defn]
            conc cs (Defn an n _ b e) = mapMaybe conc' $ lookupAll n $ Map.keys cs
                  where conc' :: Ty -> Maybe Defn
                        conc' t = do
                              n' <- Map.lookup (n, t) cs
                              pure $ Defn an n' ([] |-> t) b e

            tc :: (MonadCatch m, Fresh m, MonadError AstError m) => TCM m [Defn]
            tc = do
                  vs' <- withAssumps ts vs $ mapM tcDefn vs
                  cs  <- concretes vs' -- TODO(chathhorn): don't think this needs to use fix.
                  fst . fst <$> fixOn (Map.keys . snd) "polymorphic function instantiation" 10 tc' ((vs', mempty), cs)

            tc' :: (MonadCatch m, Fresh m, MonadError AstError m) => (([Defn], Concretes), Concretes) -> TCM m (([Defn], Concretes), Concretes)
            tc' ((acc, cs), cs') = do
                  let ep = concatMap (conc cs') polyDefs
                  ep' <- concretize (cs <> cs') <$> withAssumps ts (acc <> ep) (mapM tcDefn ep)
                  ((concretize (cs <> cs') acc <> ep', cs <> cs'), ) <$> concretes ep'

            concretes :: Fresh m => [Defn] -> m Concretes
            concretes = foldM (\ m c -> Map.insert c <$> fresh (fst c) <*> pure m) mempty . uses

            polys :: Set (Name Exp)
            polys = foldr polys' mempty vs
                  where polys' :: Defn -> Set (Name Exp) -> Set (Name Exp)
                        polys' d | isPoly d  = Set.insert $ defnName d
                                 | otherwise = id

            polyDefs :: [Defn]
            polyDefs = foldr polys' mempty vs
                  where polys' :: Defn -> [Defn] -> [Defn]
                        polys' d | isPoly d  = (d :)
                                 | otherwise = id

            isPoly :: Defn -> Bool
            isPoly (Defn _ _ (Embed (Poly (unsafeUnbind -> (_, t)))) _ _) = not $ concrete t

            uses :: Data a => a -> Set (Name Exp, Ty)
            uses = runQ (query $ \ case
                  Var _ t n | concrete t, Set.member n polys -> Set.singleton (n, unAnn t)
                  _                                          -> mempty)

            concretize :: Data d => Concretes -> d -> d
            concretize cs = runIdentity . runPureT (transform $ \ case
                  v@(Var an t n) -> pure $ maybe v (Var an t) $ Map.lookup (n, unAnn t) cs
                  e              -> pure e)

freshv :: (MonadState TySub m, Fresh m) => m Ty
freshv = do
      n <- fresh $ s2n "?"
      let tv = TyVar (MsgAnnote "TypeCheck: freshv") kblank n
      modify $ Map.insert n tv
      pure tv

(@@) :: TySub -> TySub -> TySub
s1 @@ s2 = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

isFlex :: Name a -> Bool -- TODO(chahthorn): !!
isFlex = (== '?') . T.head . n2s

varBind :: Monad m => Name Ty -> Ty -> TCM m (Maybe TySub)
varBind u t | t `aeq` TyVar noAnn kblank u = pure $ Just mempty
            | u `elem` fv t                = pure Nothing
            | otherwise                    = pure $ Just $ Map.singleton u t

mgu :: Monad m => Ty -> Ty -> TCM m (Maybe TySub)
mgu (TyApp _ tl tr) (TyApp _ tl' tr')                        = do
      s1 <- mgu tl tl'
      s2 <- maybe (pure Nothing) (\ s1' -> mgu (subst s1' tr) $ subst s1' tr') s1
      case (s1, s2) of
            (Just s1', Just s2') -> pure $ Just $ s2' @@ s1'
            _                    -> pure Nothing
mgu (TyCon _ c1)    (TyCon _ c2)  | n2s c1 == n2s c2              = pure $ Just mempty
mgu (TyVar _ _ u)   t             | isFlex u                      = varBind u t
mgu t               (TyVar _ _ u) | isFlex u                      = varBind u t
mgu (TyVar _ _ v)   (TyVar _ _ u) | not (isFlex v) && v == u      = pure $ Just mempty
mgu _ _                                                           = pure Nothing

unify :: MonadError AstError m => Annote -> Ty -> Ty -> TCM m ()
unify an t1 t2 = do
      s  <- get
      mgu (subst s t1) (subst s t2) >>= maybe
            (failAt an $ "Types do not unify. Expected and got, respectively:\n" <> prettyPrint (subst s t1) <> "\n" <> prettyPrint (subst s t2))
            (modify . (@@))

inst :: (MonadState TySub m, Fresh m) => Poly -> m Ty
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
                  PatWildCard _ _      -> id

patHoles :: Fresh m => MatchPat -> m (HashMap (Name Exp) Poly)
patHoles = flip patHoles' $ pure mempty
      where patHoles' :: Fresh m => MatchPat -> m (HashMap (Name Exp) Poly) -> m (HashMap (Name Exp) Poly)
            patHoles' = \ case
                  MatchPatCon _ _ _ ps -> flip (foldr patHoles') ps
                  MatchPatVar _ t      -> (flip Map.insert ([] `poly` t) <$> fresh (s2n "PHOLE") <*>)
                  MatchPatWildCard _ _ -> id

tcPat :: (Fresh m, MonadError AstError m) => Ty -> Pat -> TCM m Pat
tcPat t = \ case
      PatCon an _ (Embed i) ps -> do
            cas     <- asks cas
            case Map.lookup i cas of
                  Nothing  -> failAt an $ "Unknown constructor: " <> prettyPrint i
                  Just pta -> do
                        ta               <- inst pta
                        let (targs, tres) = flattenArrow ta
                        if length ps /= length targs
                        then failAt an "Pattern is not applied to enough arguments"
                        else do
                              ps' <- zipWithM tcPat targs ps
                              unify an tres t
                              pure $ PatCon an (Embed t) (Embed i) ps'
      PatVar an _ x            -> pure $ PatVar an (Embed t) x
      PatWildCard an _         -> pure $ PatWildCard an (Embed t)

tcMatchPat :: (Fresh m, MonadError AstError m) => Ty -> MatchPat -> TCM m MatchPat
tcMatchPat t = \ case
      MatchPatCon an _ i ps -> do
            cas     <- asks cas
            case Map.lookup i cas of
                  Nothing  -> failAt an $ "Unknown constructor: " <> prettyPrint i
                  Just pta -> do
                        ta               <- inst pta
                        let (targs, tres) = flattenArrow ta
                        if length ps /= length targs
                        then failAt an "Pattern is not applied to enough arguments"
                        else do
                              ps' <- zipWithM tcMatchPat targs ps
                              unify an tres t
                              pure $ MatchPatCon an t i ps'
      MatchPatVar an _      -> pure $ MatchPatVar an t
      MatchPatWildCard an _ -> pure $ MatchPatWildCard an t

tcExp :: (Fresh m, MonadError AstError m) => Exp -> TCM m (Exp, Ty)
tcExp = \ case
      App an e1 e2 -> do
            (e1', te1) <- tcExp e1
            (e2', te2) <- tcExp e2
            tv         <- freshv
            unify an te1 $ te2 `arr` tv
            pure (App an e1' e2', tv)
      Lam an _ e -> do
            (x, e')   <- unbind e
            tvx       <- freshv
            tvr       <- freshv
            (e'', te) <- localAssumps (Map.insert x ([] `poly` tvx)) $ tcExp e'
            unify an tvr $ arr tvx te
            pure (Lam an tvx $ bind x e'', tvr)
      Var an _ v -> do
            as <- asks as
            case Map.lookup v as of
                  Nothing -> failAt an $ "Unknown variable: " <> showt v
                  Just pt -> do
                        t <- inst pt
                        pure (Var an t v, t)
      Con an _ i -> do
            cas <- asks cas
            case Map.lookup i cas of
                  Nothing -> failAt an $ "Unknown constructor: " <> prettyPrint i
                  Just pt -> do
                        t <- inst pt
                        pure (Con an t i, t)
      Case an _ e e1 e2 -> do
            (p, e1')    <- unbind e1
            (e', te)    <- tcExp e
            tv          <- freshv
            p'          <- tcPat te p
            let as      = patAssumps p'
            (e1'', te1) <- localAssumps (as `Map.union`) $ tcExp e1'
            unify an tv te1
            case e2 of
                  Nothing -> pure (Case an tv e' (bind p' e1'') Nothing, tv)
                  Just e2 -> do
                        (e2', te2)  <- tcExp e2
                        unify an tv te2
                        pure (Case an tv e' (bind p' e1'') (Just e2'), tv)
      Match an _ e p f e2 -> do
            (e', te) <- tcExp e
            tv       <- freshv
            p'       <- tcMatchPat te p
            holes    <- patHoles p'
            (_, te1) <- localAssumps (holes `Map.union`) $ tcExp $ mkApp an f $ map fst $ Map.toList holes
            unify an tv te1
            case e2 of
                  Nothing -> pure (Match an tv e' p' f Nothing, tv)
                  Just e2 -> do
                        (e2', te2)  <- tcExp e2
                        unify an tv te2
                        pure (Match an tv e' p' f (Just e2'), tv)
      Extern an _ -> do
            tv <- freshv
            let t = listTy an paramTy `arr` strTy an `arr` listTy an paramTy `arr` listTy an paramTy `arr` strTy an `arr` tv `arr` strTy an `arr` tv
            pure (Extern an t, t)
            where paramTy :: Ty
                  paramTy = pairTy an (strTy an) (intTy an)
      Bit an _ -> do
            tv <- freshv
            let t = tv `arr` intTy an `arr` bitTy an
            pure (Bit an t, t)
      Bits an _ -> do
            tv  <- freshv
            tv' <- freshv
            let t = tv `arr` intTy an `arr` intTy an `arr` tv'
            pure (Bits an t, t)
      SetRef an _ -> do
            ta  <- freshv
            tb  <- freshv
            let t = refTy an ta `arr` ta `arr` tb `arr` tb
            pure (SetRef an t, t)
      GetRef an _ -> do
            ta  <- freshv
            let t = refTy an ta `arr` ta
            pure (GetRef an t, t)
      e@LitInt {} -> pure (e, typeOf e)
      e@LitStr {} -> pure (e, typeOf e)
      LitList an _ es -> do
            tv  <- freshv
            es' <- mapM tcExp es
            mapM_ (unify an tv) (snd <$> es')
            let t = listTy an tv
            pure (LitList an t (fst <$> es'), t)
      Error an _ m -> do
            tv <- freshv
            pure (Error an tv m, tv)
      TypeAnn an pt e -> do
            (e', te) <- tcExp e
            t        <- inst pt
            unify an t te
            pure (TypeAnn an pt e', te)

mkApp :: Annote -> Exp -> [Name Exp] -> Exp
mkApp an f holes = foldl' (App an) f $ map (Var an $ TyBlank an) holes

tcDefn :: (Fresh m, MonadError AstError m) => Defn -> TCM m Defn
tcDefn d  = do
      put mempty
      let Defn an n (Embed (Poly pt)) b (Embed e) = force d
      (tvs, t) <- unbind pt
      (vs, e') <- unbind e
      let (targs, _) = flattenArrow t
      (e'', te) <- localAssumps (Map.union $ Map.fromList $ zip vs $ map (poly []) targs) $ tcExp e'
      let te' = iterate arrowRight t !! length vs
      unify an te' te
      s <- get
      put mempty
      let d' = Defn an n (tvs |-> t) b $ Embed $ bind vs $ subst s e''
      d' `deepseq` pure d'

withAssumps :: MonadError AstError m => [DataDefn] -> [Defn] -> TCM m a -> TCM m a
withAssumps ts vs = localAssumps (as `Map.union`) . localCAssumps (cas `Map.union`)
      where as  = foldr defnAssump mempty vs
            cas = foldr dataDeclAssumps mempty ts

            defnAssump :: Defn -> HashMap (Name Exp) Poly -> HashMap (Name Exp) Poly
            defnAssump (Defn _ n (Embed pt) _ _) = Map.insert n pt

            dataDeclAssumps :: DataDefn -> HashMap (Name DataConId) Poly -> HashMap (Name DataConId) Poly
            dataDeclAssumps (DataDefn _ _ _ cs) = flip (foldr dataConAssump) cs

            dataConAssump :: DataCon -> HashMap (Name DataConId) Poly -> HashMap (Name DataConId) Poly
            dataConAssump (DataCon _ i (Embed t)) = Map.insert i t

localAssumps :: MonadError AstError m => (HashMap (Name Exp) Poly -> HashMap (Name Exp) Poly) -> TCM m a -> TCM m a
localAssumps f = local (\ tce -> tce { as = f (as tce) })

localCAssumps :: MonadError AstError m => (HashMap (Name DataConId) Poly -> HashMap (Name DataConId) Poly) -> TCM m a -> TCM m a
localCAssumps f = local (\ tce -> tce { cas = f (cas tce) })

untype :: Data d => d -> d
untype = runIdentity . runPureT (transform $ \ (t :: Ty) -> pure $ TyBlank $ ann t)

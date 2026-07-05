{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Substitution and binder-refreshing for Eidos (doc/eidos.md §2, G2).
--
--   The uniqueness discipline makes substitution an environment map — no
--   capture is possible while the invariant holds — and concentrates the
--   invariant's maintenance in ONE primitive: 'refreshExp'/'refreshDefn',
--   the audited clone. Every pass that duplicates a term (inlining,
--   specialization, beta reduction, case-of-known-constructor) must route
--   the duplicated copy through a refresh; the linter's uniqueness rule
--   re-checks the invariant globally under --debug-lint.
--
--   Contracts:
--
--   * 'substVars' inserts each payload AS IS: sound only when each payload
--     lands at most once (or is binder-free). For the general case use
--     'substVarsRefreshing', which refreshes every inserted copy.
--   * 'refreshExp' freshens every binder in the term (term binders, join
--     labels, and — via 'refreshDefn' — signature type variables,
--     propagating the renaming through every type in the body) and leaves
--     free names untouched.
--   * Supplies: passes obtain fresh uniques from a state seeded above the
--     program's maximum ('nextUniq').
module ReWire.Eidos.Subst
      ( maxUniq, nextUniq
      , refreshExp, refreshDefn, instantiateDefn
      , substVars, substVarsRefreshing
      , occCounts
      ) where

import ReWire.Eidos.Syntax
import ReWire.Eidos.Types (substTv)
import ReWire.SYB (queryWith)

import Control.Monad.State.Strict (MonadState, get, put)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict  as IM

-- | The largest unique occurring anywhere in a program (binders and
--   occurrences; the primitive basis' negative uniques never win).
maxUniq :: Program -> Uniq
maxUniq p = maximum $ 0 : ids <> tvs
      where ids :: [Uniq]
            ids = queryWith (\ x -> [idUniq x]) p

            tvs :: [Uniq]
            tvs = queryWith (\ v -> [tvUniq v]) p

-- | A safe starting value for a pass's unique supply.
nextUniq :: Program -> Uniq
nextUniq = succ . maxUniq

freshU :: MonadState Uniq m => m Uniq
freshU = do
      u <- get
      put $ u + 1
      pure u

---
--- Refreshing (the audited clone primitive).
---

-- | The renaming environment: term binders, join labels, and signature
--   type variables (as a type substitution).
data Renames = Renames
      { rnVars  :: !(IM.IntMap Id)
      , rnJoins :: !(IM.IntMap JoinId)
      , rnTvs   :: !(HashMap TyVar Ty)
      }

rn0 :: Renames
rn0 = Renames mempty mempty mempty

-- | Freshen every binder in an expression; free names (and all types, when
--   no signature variables are being renamed) are untouched.
refreshExp :: MonadState Uniq m => Exp -> m Exp
refreshExp = rex rn0

-- | Clone a definition with fresh binders throughout: a fresh definition
--   name (self-references in the body follow it), fresh signature type
--   variables (the renaming propagates through every type in the
--   parameters and body), and fresh parameter and local binders.
refreshDefn :: MonadState Uniq m => Defn -> m Defn
refreshDefn (Defn an x params body attr orig) = do
      let Sig tvs t = idSig x
      tvs' <- mapM freshTv tvs
      let rtv  = Map.fromList $ zip tvs $ map (TyVarT an) tvs'
          sig' = Sig tvs' $ substTv rtv t
      x'   <- freshLike rtv x { idSig = sig' }
      let r0 = rn0 { rnVars = IM.singleton (idUniq x) x', rnTvs = rtv }
      (r1, params') <- bindIds r0 params
      body' <- rex r1 body
      pure $ Defn an x' params' body' attr orig
      where freshTv :: MonadState Uniq m => TyVar -> m TyVar
            freshTv v = do
                  u <- freshU
                  pure v { tvUniq = u }

-- | Clone a definition at a type instantiation (the specializer's flavor
--   of the audited clone): the signature's variables are substituted away
--   by the given type arguments (the clone is monomorphic when they are
--   closed), every type in the parameters and body follows, and every
--   binder is refreshed. The clone is named by the given occurrence text
--   with a fresh unique; self-references in the body are NOT remapped —
--   they still name the origin at its instantiated type arguments, for
--   the caller's spine rewrite to resolve.
instantiateDefn :: MonadState Uniq m => Text -> [Ty] -> Defn -> m Defn
instantiateDefn occ ts (Defn an x params body attr orig) = do
      let Sig tvs t = idSig x
          rtv       = Map.fromList $ zip tvs ts
      u <- freshU
      let x' = x { idOcc = occ, idUniq = u, idSig = Sig [] $ substTv rtv t }
      (r1, params') <- bindIds rn0 { rnTvs = rtv } params
      body' <- rex r1 body
      pure $ Defn an x' params' body' attr orig

-- | A fresh Id with the same occurrence text and a renamed signature.
freshLike :: MonadState Uniq m => HashMap TyVar Ty -> Id -> m Id
freshLike rtv x = do
      u <- freshU
      let Sig tvs t = idSig x
      pure x { idUniq = u, idSig = Sig tvs $ substTv rtv t }

bindId :: MonadState Uniq m => Renames -> Id -> m (Renames, Id)
bindId r x = do
      x' <- freshLike (rnTvs r) x
      pure (r { rnVars = IM.insert (idUniq x) x' $ rnVars r }, x')

bindIds :: MonadState Uniq m => Renames -> [Id] -> m (Renames, [Id])
bindIds = go []
      where go acc r []       = pure (r, reverse acc)
            go acc r (x : xs) = do
                  (r', x') <- bindId r x
                  go (x' : acc) r' xs

rex :: MonadState Uniq m => Renames -> Exp -> m Exp
rex r e = case e of
      Var an x        -> pure $ Var an $ occ x
      Con an t c      -> pure $ Con an (ty t) c
      Prim an t p     -> pure $ Prim an (ty t) p
      LitInt an t n   -> pure $ LitInt an (ty t) n
      LitStr {}       -> pure e
      LitList an t es -> LitList an (ty t) <$> mapM (rex r) es
      LitVec an t es  -> LitVec an (ty t) <$> mapM (rex r) es
      App an f a      -> App an <$> rex r f <*> arg a
      Lam an x b      -> do
            (r', x') <- bindId r x
            Lam an x' <$> rex r' b
      Let an b body   -> do
            (r', b') <- bnd b
            Let an b' <$> rex r' body
      Jump an j es    -> Jump an (jn j) <$> mapM (rex r) es
      Case an t s cb alts -> do
            s'        <- rex r s
            (r', cb') <- bindId r cb
            alts'     <- mapM (alt r') alts
            pure $ Case an (ty t) s' cb' alts'
      where occ :: Id -> Id
            occ x = case IM.lookup (idUniq x) $ rnVars r of
                  Just x'                       -> x'
                  Nothing | Map.null (rnTvs r)  -> x
                          | otherwise           -> x { idSig = Sig tvs $ substTv (rnTvs r) t }
                        where Sig tvs t = idSig x

            jn :: JoinId -> JoinId
            jn j = case IM.lookup (idUniq $ jpId j) $ rnJoins r of
                  Just j' -> j'
                  Nothing -> j

            ty :: Ty -> Ty
            ty | Map.null (rnTvs r) = id
               | otherwise          = substTv (rnTvs r)

            arg :: MonadState Uniq m => Arg -> m Arg
            arg = \ case
                  EArg x -> EArg <$> rex r x
                  TArg t -> pure $ TArg $ ty t

            bnd :: MonadState Uniq m => Bind -> m (Renames, Bind)
            bnd = \ case
                  NonRec x rhs -> do
                        rhs'     <- rex r rhs
                        (r', x') <- bindId r x
                        pure (r', NonRec x' rhs')
                  Rec bs       -> do
                        (r', xs') <- bindIds r $ map fst bs
                        rhss'     <- mapM (rex r' . snd) bs
                        pure (r', Rec $ zip xs' rhss')
                  -- The label scopes over the body of the LET (joins are
                  -- non-recursive; the renaming is threaded through the
                  -- join's own body too, where it is inert on lint-clean
                  -- input); the params scope only over the join body.
                  Join j ps b  -> do
                        (rj, jx') <- bindId r $ jpId j
                        let j'  = JoinId jx' $ jpArity j
                            rj' = rj { rnJoins = IM.insert (idUniq $ jpId j) j' $ rnJoins rj }
                        (rp, ps') <- bindIds rj' ps
                        b'        <- rex rp b
                        pure (rj', Join j' ps' b')

            alt :: MonadState Uniq m => Renames -> Alt -> m Alt
            alt r' (Alt an c xs body) = do
                  (r'', xs') <- bindIds r' xs
                  Alt an c xs' <$> rex r'' body

---
--- Substitution.
---

-- | Substitute expressions for variable occurrences (by unique). Each
--   payload is inserted as is: use only when each payload can land at most
--   once, or is binder-free; otherwise use 'substVarsRefreshing'.
substVars :: IM.IntMap Exp -> Exp -> Exp
substVars s = go
      where go :: Exp -> Exp
            go e = case e of
                  Var _ x         -> IM.findWithDefault e (idUniq x) s
                  Con {}          -> e
                  Prim {}         -> e
                  LitInt {}       -> e
                  LitStr {}       -> e
                  LitList an t es -> LitList an t $ map go es
                  LitVec an t es  -> LitVec an t $ map go es
                  App an f a      -> App an (go f) $ goArg a
                  Lam an x b      -> Lam an x $ go b
                  Let an b body   -> Let an (goBind b) $ go body
                  Jump an j es    -> Jump an j $ map go es
                  Case an t sc cb alts -> Case an t (go sc) cb [ Alt aan c xs (go b) | Alt aan c xs b <- alts ]

            goArg :: Arg -> Arg
            goArg = \ case
                  EArg e -> EArg $ go e
                  t      -> t

            goBind :: Bind -> Bind
            goBind = \ case
                  NonRec x rhs -> NonRec x $ go rhs
                  Rec bs       -> Rec [ (x, go rhs) | (x, rhs) <- bs ]
                  Join j ps b  -> Join j ps $ go b

-- | Substitute expressions for variable occurrences, refreshing every
--   inserted copy (the uniqueness-preserving form).
substVarsRefreshing :: MonadState Uniq m => IM.IntMap Exp -> Exp -> m Exp
substVarsRefreshing s = go
      where go :: MonadState Uniq m => Exp -> m Exp
            go e = case e of
                  Var _ x
                        | Just e' <- IM.lookup (idUniq x) s -> refreshExp e'
                        | otherwise                         -> pure e
                  Con {}          -> pure e
                  Prim {}         -> pure e
                  LitInt {}       -> pure e
                  LitStr {}       -> pure e
                  LitList an t es -> LitList an t <$> mapM go es
                  LitVec an t es  -> LitVec an t <$> mapM go es
                  App an f a      -> App an <$> go f <*> goArg a
                  Lam an x b      -> Lam an x <$> go b
                  Let an b body   -> Let an <$> goBind b <*> go body
                  Jump an j es    -> Jump an j <$> mapM go es
                  Case an t sc cb alts -> do
                        sc'   <- go sc
                        alts' <- mapM (\ (Alt aan c xs b) -> Alt aan c xs <$> go b) alts
                        pure $ Case an t sc' cb alts'

            goArg :: MonadState Uniq m => Arg -> m Arg
            goArg = \ case
                  EArg e -> EArg <$> go e
                  t      -> pure t

            goBind :: MonadState Uniq m => Bind -> m Bind
            goBind = \ case
                  NonRec x rhs -> NonRec x <$> go rhs
                  Rec bs       -> Rec <$> mapM (\ (x, rhs) -> (x, ) <$> go rhs) bs
                  Join j ps b  -> Join j ps <$> go b

---
--- Occurrence analysis.
---

-- | Variable- and jump-occurrence counts by unique (an occurrence of a
--   join label at a jump counts for the label's Id). Dead binders are
--   absent from the map.
occCounts :: Exp -> IM.IntMap Int
occCounts = go
      where go :: Exp -> IM.IntMap Int
            go = \ case
                  Var _ x         -> IM.singleton (idUniq x) 1
                  Jump _ j es     -> IM.unionsWith (+) $ IM.singleton (idUniq $ jpId j) 1 : map go es
                  App _ f a       -> IM.unionWith (+) (go f) $ goArg a
                  Lam _ _ b       -> go b
                  Let _ b body    -> IM.unionWith (+) (goBind b) $ go body
                  Case _ _ s _ as -> IM.unionsWith (+) $ go s : [ go b | Alt _ _ _ b <- as ]
                  LitList _ _ es  -> IM.unionsWith (+) $ map go es
                  LitVec _ _ es   -> IM.unionsWith (+) $ map go es
                  _               -> mempty

            goArg :: Arg -> IM.IntMap Int
            goArg = \ case
                  EArg e -> go e
                  _      -> mempty

            goBind :: Bind -> IM.IntMap Int
            goBind = \ case
                  NonRec _ rhs -> go rhs
                  Rec bs       -> IM.unionsWith (+) $ map (go . snd) bs
                  Join _ _ b   -> go b

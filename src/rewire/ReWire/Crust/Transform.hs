{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs, OverloadedStrings, MultiWayIf #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module ReWire.Crust.Transform
      ( inline, expandTypeSynonyms, reduce
      , neuterExterns
      , shiftLambdas
      , liftLambdas
      , fullyApplyDefs
      , purgeUnused
      , prePurify
      , simplify
      , specialize
      , removeExpTypeAnn
      , freeTyVarsToNil
      ) where

import ReWire.Config (Config, depth)
import ReWire.Unbound
      ( Fresh (..), s2n, n2s
      , substs, subst, unembed
      , isFreeName, runFreshM
      , Name (..)
      , unsafeUnbind
      , Subst (..), Alpha
      )
import ReWire.Annotation (Annote (..), Annotated (..), unAnn)
import ReWire.Crust.Syntax
import ReWire.Crust.TypeCheck (typeCheckDefn, unify, TySub)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Fix (fix, fix', boundedFix)
import ReWire.SYB

import Control.Lens ((^.))
import Control.Arrow (first, (&&&))
import Control.Monad (foldM_, zipWithM, replicateM, (>=>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.State (MonadState, State, evalStateT, execState, StateT (..), get, gets, modify)
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Data (Data)
import Data.Either (lefts)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (hash))
import Data.List (find, sort)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Data.Set (Set, union, (\\))
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set

-- | Inlines defs marked for inlining. Must run before lambda lifting.
inline :: MonadError AstError m => FreeProgram -> m FreeProgram
inline (ts, syns, ds) = (ts, syns, ) . flip substs ds <$> subs
      where inlineDefs :: [Defn]
            inlineDefs = filter mustInline ds

            subs :: MonadError AstError m => m [(Name Exp, Exp)]
            subs = map defnSubst <$> fix "INLINE definition expansion" 100 (pure . substs (map defnSubst inlineDefs)) inlineDefs

defnSubst :: Defn -> (Name Exp, Exp)
defnSubst (Defn _ n (Embed pt) _ (Embed e)) = runFreshM $ unbind e >>= \ case
      ([], e') -> pure (n, tyAnn (ann e') pt e')
      _        -> error "Inlining: definition not inlinable (this shouldn't happen)"

-- | Expands type synonyms.
expandTypeSynonyms :: (MonadCatch m, MonadError AstError m, Fresh m) => FreeProgram -> m FreeProgram
expandTypeSynonyms (ts, syns0, ds) = (,,) <$> expandSyns ts <*> syns' <*> expandSyns ds
      where toSubst :: TypeSynonym -> (Name TyConId, Bind [Name Ty] Ty)
            toSubst (TypeSynonym _ n (Embed (Poly t))) = (n, t)

            expandSyns :: (MonadCatch m, MonadError AstError m, Fresh m, Data d) => d -> m d
            expandSyns d = subs' >>= flip substs' d

            subs' :: (MonadCatch m, MonadError AstError m, Fresh m) => m [(Name TyConId, Bind [Name Ty] Ty)]
            subs' = map toSubst <$> syns'

            -- | First expand type synonyms in type synonym definitions.
            syns' :: (MonadCatch m, MonadError AstError m, Fresh m) => m [TypeSynonym]
            syns' = do
                  foldM_ checkDupe [] syns0
                  fix "Type synonym expansion" 100 (substs' $ map toSubst syns0) syns0
                  where checkDupe :: MonadError AstError m => [Text] -> TypeSynonym -> m [Text]
                        checkDupe ss (TypeSynonym an n _)
                              | n2s n `elem` ss = failAt an $ "Duplicate type synonym: " <> n2s n
                              | otherwise       = pure $ n2s n : ss

            substs' :: (MonadCatch m, MonadError AstError m, Fresh m, Data d) => [(Name TyConId, Bind [Name Ty] Ty)] -> d -> m d
            substs' subs' = runT (transform $ \ case
                  TyCon _ n -> case lookup n subs' of
                        Just pt -> do
                              (vs, t') <- unbind pt
                              case length vs of
                                    0 -> pure t'
                  TyApp _ a b -> case findTyCon a of
                        Just (n, args) -> case lookup n subs' of
                              Just pt -> do
                                    (vs, t') <- unbind pt
                                    let args' = args <> [b]
                                    case length vs == length args' of
                                          True -> pure $ substs (zip vs args') t'
                  )

            findTyCon :: Ty -> Maybe (Name TyConId, [Ty])
            findTyCon = \ case
                  TyCon _ n   -> pure (n, [])
                  TyApp _ a b -> do
                        (n, args) <- findTyCon a
                        pure (n, args <> [b])
                  _           -> Nothing

-- | Replaces the second argument to Extern so we don't descend into it
--   during other transformations.
neuterExterns :: MonadCatch m => FreeProgram -> m FreeProgram
neuterExterns = runT $ transform $ \ case
      App an t ex e | isExtern ex -> pure $ App an t ex
                                          $ tyAnn (ann e) (poly' $ typeOf e)
                                          $ mkError (ann e) (typeOf e) "Extern expression placeholder"
      where isExtern :: Exp -> Bool
            isExtern e = case flattenApp' e of
                  [Builtin _ _ Extern, _ , _, _, _, _] -> True
                  _                                    -> False

-- | Removes type annotations on expressions.
removeExpTypeAnn :: MonadCatch m => FreeProgram -> m FreeProgram
removeExpTypeAnn = runT $ transform $ \ (TypeAnn _ _ e) -> pure e

-- | Shifts vars bound by top-level lambdas into defs.
-- > g = \ x1 -> \ x2 -> e
--   becomes
-- > g x1 x2 = e
shiftLambdas :: (Fresh m, MonadCatch m) => FreeProgram -> m FreeProgram
shiftLambdas (ts, syns, vs) = (ts, syns, ) <$> mapM shiftLambdas' vs
      where shiftLambdas' :: Fresh m => Defn -> m Defn
            shiftLambdas' (Defn an n t inl (Embed e)) = Defn an n t inl . Embed <$> mash e

            mash :: Fresh m => Bind [Name Exp] Exp -> m (Bind [Name Exp] Exp)
            mash e = unbind e >>= \ case
                  (vs, Lam _ _ b) -> do
                        (v, b') <- unbind b
                        mash $ bind (vs <> [v]) b'
                  _               -> pure e

-- | Inlines everything to the left of ">>=" and
-- > (m >>= f) >>= g
-- > (m >>= (\ x -> s)) >>= g
-- becomes
-- > m >>= (\ x -> f x >>= g)
-- > m >>= (\ x -> s >>= g)
-- also
-- > m >>= f
-- becomes (to trigger lambda lifting)
-- > m >>= \ x -> f x
prePurify :: (Fresh m, MonadCatch m, MonadError AstError m) => FreeProgram -> m FreeProgram
prePurify (ts, syns, ds) = (ts, syns, ) <$> mapM ppDefn ds
      where ppDefn :: (MonadError AstError m, Fresh m) => Defn -> m Defn
            ppDefn d@Defn { defnBody = Embed e } = do
                  (xs, e') <- unbind e
                  e''      <- ppExp e'
                  pure $ d { defnBody = Embed (bind xs e'') }

            ppExp :: (MonadError AstError m, Fresh m) => Exp -> m Exp
            ppExp = \ case
                  e | needsRejiggering e -> rejiggerBind e
                  -- Inline everything on the LHS.
                  (dstBind -> Just (a, e1, e2)) -> do
                        e1' <- flatten (filter (\ d -> isReacT d && inlineable d) ds) e1
                        rejiggerBind $ mkBind a e1' e2
                  App an t e1 e2 -> App an t <$> ppExp e1 <*> ppExp e2
                  Lam an t  e -> do
                        (x, e') <- unbind e
                        Lam an t . bind x <$> ppExp e'
                  Case an t disc e els -> do
                        (p, e') <- unbind e
                        Case an t <$> ppExp disc <*> (bind p <$> ppExp e') <*> mapM ppExp els
                  Match an t disc p e els -> Match an t <$> ppExp disc <*> pure p <*> ppExp e <*> mapM ppExp els
                  LitList an t es -> LitList an t <$> mapM ppExp es
                  TypeAnn an pt e -> TypeAnn an pt <$> ppExp e
                  e               -> pure e

            needsRejiggering :: Exp -> Bool
            needsRejiggering = \ case
                  (dstBind -> Just (_, dstBind -> Just (_, _, _), _)) -> True
                  (dstBind -> Just (_, _, e2)) | not (isLambda e2)    -> True
                  _                                                   -> False

            rejiggerBind :: (MonadError AstError m, Fresh m) => Exp -> m Exp
            rejiggerBind = \ case
                  -- Associate bind to the right (case with a lambda on the right).
                  -- ma :: m a
                  -- mb :: m b
                  -- fb :: b -> m c
                  -- (>>=1) :: m a -> (a -> m b) -> m b
                  -- (>>=2) :: m b -> (b -> m c) -> m c
                  -- (ma >>=1 \ a -> mb) >>=2 fb
                  -- becomes
                  -- (>>=1) :: m a -> (a -> m c) -> m c
                  -- (>>=2) :: m b -> (b -> m c) -> m c
                  -- ma >>=1 (\ a -> (mb >>=2 fb))
                  (dstBind -> Just (a, dstBind -> Just (_, e1, unTyAnn -> Lam _ tx e2), e3)) -> do
                        -- TODO(chathhorn): note eliding type annotation.
                        (x, e2') <- unbind e2
                        let e' = mkBind a e2' e3
                        ppExp $ mkBind a e1
                              $ Lam (ann e2') tx (bind x e')

                  -- Associate bind to the right.
                  -- fa :: a -> m b
                  -- fb :: b -> m c
                  -- (>>=1) :: m a -> (a -> m b) -> m b
                  -- (>>=2) :: m b -> (b -> m c) -> m c
                  -- (m >>=1 fa) >>=2 fb
                  -- becomes
                  -- (>>=1) :: m a -> (a -> m c) -> m c
                  -- (>>=2) :: m b -> (b -> m c) -> m c
                  -- m >>=1 (\ x -> fa x >>=2 fb)
                  (dstBind -> Just (a, dstBind -> Just (_, e1, e2), e3)) -> do
                        let tx = arrowLeft $ typeOf e2
                        x <- fresh $ s2n "rabind"
                        let e' = mkBind a (mkApp (ann e2) e2 [Var (ann e2) tx x]) e3
                        ppExp $ mkBind a e1
                              $ Lam (ann e2) tx (bind x e')
                  -- Lambda-abstract the right side (so it will get lambda-lifted).
                  (dstBind -> Just (a, e1, e2)) | not (isLambda e2) -> do
                        let tx = arrowLeft $ typeOf e2
                        x <- fresh $ s2n "sbind"
                        ppExp $ mkBind a e1
                              $ Lam (ann e2) tx
                              $ bind x $ mkApp (ann e2) e2 [Var (ann e2) tx x]
                  e -> pure e

            isLambda :: Exp -> Bool
            isLambda e = case unTyAnn e of
                  Lam {}        -> True
                  _             -> False

            dstBind :: Exp -> Maybe (Annote, Exp, Exp)
            dstBind e = case flattenApp' e of
                  [Builtin a _ Bind, e1, e2] -> Just (a, e1, e2)
                  _                          -> Nothing

            mkBind :: Annote -> Exp -> Exp -> Exp
            mkBind a e1 e2 = mkApp a (Builtin a (typeOf e1 `arr` typeOf e2 `arr` arrowRight (typeOf e2)) Bind) [e1, e2]

            flatten :: (Fresh m, MonadError AstError m) => [Defn] -> Exp -> m Exp
            flatten ds = fix "Bind LHS definition expansion" 100 (pure . substs (map defnSubst ds))

            isReacT :: Defn -> Bool
            isReacT Defn { defnPolyTy = Embed (Poly (unsafeUnbind -> (_, t))) } = isResMonad t

-- | So if e :: a -> b, then
-- > g = e
--   becomes
-- > g x0 = e x0
-- except with Match, we add x0 to the list of local ids.
fullyApplyDefs :: Fresh m => FreeProgram -> m FreeProgram
fullyApplyDefs (ts, syns, vs) = (ts, syns, ) <$> mapM fullyApplyDefs' vs
      where fullyApplyDefs' :: Fresh m => Defn -> m Defn
            fullyApplyDefs' (Defn an n t inl (Embed e)) = Defn an n t inl . Embed <$> fullyApply e

            fullyApply :: Fresh m => Bind [Name Exp] Exp -> m (Bind [Name Exp] Exp)
            fullyApply e = do
                  (vs, e') <- unbind e
                  case typeOf e' of
                        TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t) _ -> do
                              x <- fresh $ s2n "$x"
                              fullyApply $ bind (vs <> [x]) $ appl t (Var (ann e') t x) e'
                        _                                             -> pure e

            appl :: Ty -> Exp -> Exp -> Exp
            appl t x = \ case
                  Match an t' e1 p e Nothing    -> Match an (arrowRight t') (mkPair an e1 x) (mkPairMPat an p $ MatchPatVar an t) e Nothing
                  Match an t' e1 p e (Just els) -> Match an (arrowRight t') (mkPair an e1 x) (mkPairMPat an p $ MatchPatVar an t) e $ Just $ appl t x els
                  e                             -> mkApp (ann e) e [x]

-- | Lifts lambdas and case/match into a top level fun def.
-- TODO(chathhorn): a lot of duplicated code here.
-- TODO(chathhorn): use Writer instead of State
liftLambdas :: (Fresh m, MonadCatch m) => FreeProgram -> m FreeProgram
liftLambdas p = evalStateT (runT liftLambdas' p) []
      where liftLambdas' :: (MonadCatch m, Fresh m) => Transform (StateT [Defn] m)
            liftLambdas' =  \ case
                  Lam an t b -> do
                        (x, e)    <- unbind b
                        -- The only free names should be globally-bound
                        -- variables (and x) at this point and we can also
                        -- assume every bound name in e' was bound at a level above e'.
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr arr (typeOf e') $ map snd bvs <> [t]
                        f     <- fresh $ s2n "$LL.lambda"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind (fvs <> [x]) e')
                        pure $ mkApp an (Var an t' f) $ map (toVar an . first promote) bvs
                  Case an t e1 b e2 -> do
                        (p, e)    <- unbind b
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let pvs = patVars p

                        let t' = foldr arr (typeOf e) $ map snd pvs <> map snd bvs
                        f     <- fresh $ s2n "$LL.case"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind (map fst pvs <> fvs) e')
                        let lvars = map (toVar an . first promote) bvs
                        pure $ Match an t (mkTuple an $ [e1] <> lvars)
                                    (mkTupleMPat an $  [transPat p] <> map (MatchPatVar an . typeOf) lvars)
                                    (Var an t' f) e2
                  -- TODO(chathhorn): This case is really just normalizing Match, consider moving to ToCore.
                  Match an t e1 p e els | liftable e -> do
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (arr . snd) (typeOf e) bvs
                        f     <- fresh $ s2n "$LL.match"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind fvs e')
                        let lvars = map (toVar an) bvs
                        pure $ Match an t (mkTuple an $ [e1] <> lvars)
                                    (mkTupleMPat an $ [p] <> map (MatchPatVar an . typeOf) lvars)
                                    (Var an t' f) els
                  -- Lifts matches in the operator position of an application.
                  -- TODO(chathhorn): move somewhere else?
                  App an _ e@Match {} arg -> do
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (arr . snd) (typeOf e) bvs
                        f     <- fresh $ s2n "$LL.matchapp"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind fvs e')
                        pure $ mkApp an (Var an t' f) $ map (toVar an) bvs <> [arg]
                  -- Lifts the first argument to extrude (required for purification).
                  App an t ex@(Builtin _ _ Extrude) e | liftable e && not (isExtrude e) -> do
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (arr . snd) (typeOf e) bvs
                        f     <- fresh $ s2n "$LL.extrude"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind fvs e')
                        pure $ App an t ex (mkApp an (Var an t' f) $ map (toVar an) bvs)
                  ||> (\ ([] :: [Defn]) -> get) -- this is cute!
                  ||> TId

            isExtrude :: Exp -> Bool
            isExtrude e = case flattenApp' e of
                  [Builtin _ _ Extrude, _, _] -> True
                  _                           -> False

            liftable :: Exp -> Bool
            liftable e = case unTyAnn e of
                  Var {}             -> False
                  _                  -> True

            toVar :: Annote -> (Name Exp, Ty) -> Exp
            toVar an (v, vt) = Var an vt v

            freshen :: (MonadCatch m, Fresh m) => Exp -> m ([Name Exp], Exp)
            freshen e = do
                  let bvs  = bv e
                  fvs      <- replicateM (length bvs) $ fresh $ s2n "$LL"
                  e'       <- substs' (zip (map fst bvs) fvs) e
                  pure (fvs, e')

            substs' :: MonadCatch m => [(Name Exp, Name Exp)] -> Exp -> m Exp
            substs' subs = runT (transform $ \ n -> pure $ fromMaybe n (lookup n subs))

            bv :: Data a => a -> [(Name Exp, Ty)]
            bv = nubOrdOn fst . runQ (query $ \ e -> case unTyAnn e of
                  Var _ t n               | not $ isFreeName n -> [(n, t)]
                  _                                            -> [])

            patVars :: Pat -> [(Name Exp, Ty)]
            patVars = \ case
                  PatCon _ _ _ ps      -> concatMap patVars ps
                  PatVar _ (Embed t) x -> [(x, t)]
                  _                    -> []

            promote :: Name a -> Name a
            promote = \ case
                  Bn l k | l >= 0 -> Bn (l - 1) k
                  b               -> b

            transPat :: Pat -> MatchPat
            transPat = \ case
                  PatCon an (Embed t) (Embed c) ps -> MatchPatCon an t c $ map transPat ps
                  PatVar an (Embed t) _            -> MatchPatVar an t
                  PatWildCard an (Embed t)         -> MatchPatWildCard an t

-- | Remove all definitions unused by those in the given list.
purgeUnused :: [Text] -> FreeProgram -> FreeProgram
purgeUnused except (ts, syns, vs) = (inuseData (fix' extendWithCtorParams $ externCtors vs') (fv $ trec vs') ts, syns, vs')
      where vs' :: [Defn]
            vs' = inuseDefn except vs

            inuseDefn :: [Text] -> [Defn] -> [Defn]
            inuseDefn except ds = map toDefn $ Set.elems $ execState (inuseDefn' ds') ds'
                  where inuseDefn' :: Set (Name Exp) -> State (Set (Name Exp)) ()
                        inuseDefn' ns | Set.null ns = pure () -- TODO(chathhorn): rewrite using fix?
                                      | otherwise   = do
                              inuse  <- get
                              modify $ union $ fvs ns
                              inuse' <- get
                              inuseDefn' $ inuse' \\ inuse

                        ds' :: Set (Name Exp)
                        ds' = Set.fromList $ filter (flip elem except . n2s) $ map defnName ds

                        fvs :: Set (Name Exp) -> Set (Name Exp)
                        fvs = Set.fromList . concatMap (fv . unembed . defnBody . toDefn) . Set.elems

                        toDefn :: Name Exp -> Defn
                        toDefn n | Just d <- find ((== n) . defnName) ds = d
                                 | otherwise                             = error $ "Something went wrong: can't find symbol: " <> show n

            inuseData :: [Name TyConId] -> [Name DataConId] -> [DataDefn] -> [DataDefn]
            inuseData ts ns = filter (not . null . dataCons) . map (inuseData' ts ns)

            inuseData' :: [Name TyConId] -> [Name DataConId] -> DataDefn -> DataDefn
            inuseData' ts ns d@(DataDefn an n k cs)
                  | n     `elem` ts           = d
                  | n2s n `elem` reservedData = d
                  | otherwise                 = DataDefn an n k $ filter ((`Set.member` Set.fromList ns) . dataConName) cs

            reservedData :: [Text]
            reservedData =
                         [ "PuRe"
                         , "(,)"
                         , "()"
                         , "Bool"
                         ]

            -- | Also treat as used: all ctors for types returned by externs and ReacT inputs.
            externCtors :: Data a => a -> [Name TyConId]
            externCtors = runQ $ (\ case
                        e@Builtin {} -> ctorNames $ flattenAllTyApp $ rangeTy $ typeOf e
                        _            -> [])
                  ||? (\ case
                        Defn _ (n2s -> n) (Embed (Poly (unsafeUnbind -> (_, t)))) _ _
                              | n `elem` except -> maybe [] (ctorNames . flattenAllTyApp) $ resInputTy t
                        _                       -> [])
                  ||? QEmpty

            extendWithCtorParams :: [Name TyConId] -> [Name TyConId]
            extendWithCtorParams = nubOrd . sort . foldr extend' []
                  where extend' :: Name TyConId -> [Name TyConId] -> [Name TyConId]
                        extend' n = (<> concatMap (concatMap ctorTypes . dataCons) (filter ((== n2s n) . n2s . dataName) ts))

                        ctorTypes :: DataCon -> [Name TyConId]
                        ctorTypes (DataCon _ _ (Embed (Poly (unsafeUnbind -> (_, t))))) = ctorNames $ flattenAllTyApp t

            dataConName :: DataCon -> Name DataConId
            dataConName (DataCon _ n _) = n

            ctorNames :: [Ty] -> [Name TyConId]
            ctorNames = \ case
                  TyCon _ n : cs -> n : ctorNames cs
                  _ : cs         -> ctorNames cs
                  _              -> []

-- | Repeatedly calls "reduce" and "specialize" -- attempts to remove
-- higher-order functions by partially evaluating them.
simplify :: (Fresh m, MonadError AstError m) => Config -> FreeProgram -> m FreeProgram
simplify conf = flip evalStateT mempty . boundedFix tst (conf^.depth) (specialize >=> reduce)
      where tst :: FreeProgram -> FreeProgram -> Bool
            tst (_, _, vs) (_, _, vs') = hash (unAnn vs) == hash (unAnn vs')

type SpecMap = HashMap (Name Exp, AppSig) Defn
type AppSig = [Maybe Exp]

-- | Replaces all free type variables with "()". We presume polymorphic
--   arguments that haven't been inferred to have a more concrete type,
--   must be unused.
freeTyVarsToNil :: FreeProgram -> FreeProgram
freeTyVarsToNil (ts, syns, vs) = (ts, syns, map upd vs)
      where upd :: Defn -> Defn
            upd d@Defn
                  { defnPolyTy = Embed (Poly (unsafeUnbind -> (_, t)))
                  , defnBody   = Embed b
                  } = d { defnPolyTy = Embed $ poly [] $ sub t
                        , defnBody   = Embed $ sub b
                        }

            sub :: (Alpha a, Subst Ty a) => a -> a
            sub v = substs (map (, nilTy) $ nubOrd $ fv v) v

-- | If b only has global variables (not lambda-bound), then
-- > f :: A -> X
-- > f = \ a -> g a b
-- > g :: A -> B -> X
-- > g = g_rhs
--   becomes
-- > f :: A -> X
-- > f = \ a -> g' a
-- > g :: A -> B -> X
-- > g = g_rhs
-- > g' :: A -> X
-- > g' = \ a' -> g_rhs a' b
specialize :: (MonadError AstError m, Fresh m, MonadState SpecMap m) => FreeProgram -> m FreeProgram
specialize (ts, syns, vs) = do
      vs'     <- mapM specDefn vs
      newDefs <- gets $ filter isNewDefn . Map.elems
      pure (ts, syns, vs' <> newDefs)
      where gs :: HashMap (Name Exp) Defn
            gs = Map.fromList $ map (defnName &&& id) vs

            isNewDefn :: Defn -> Bool
            isNewDefn = not . isGlobal . defnName

            isGlobal :: Name Exp -> Bool
            isGlobal = flip Map.member gs

            specDefn :: (MonadError AstError m, Fresh m, MonadState SpecMap m) => Defn -> m Defn
            specDefn (Defn ann n pt inl (Embed body)) = do
                  (vs, body') <- unbind body
                  Defn ann n pt inl <$> Embed <$> bind vs <$> specExp body'

            specExp :: (MonadError AstError m, Fresh m, MonadState SpecMap m) => Exp -> m Exp
            specExp = \ case
                  e@(App an t e' a') | Var _ _ g : args <- flattenApp' e
                                     , Just d           <- Map.lookup g gs
                                     , inlineable d
                                               -> do
                        args' <- mapM specExp args
                        let s = sig args'
                        if | all isNothing s -> App an t <$> specExp e' <*> specExp a'
                           | otherwise       -> do
                              d'   <- gets (Map.lookup (g, s)) >>= \ case
                                    Just d'' -> pure d''
                                    _        -> do
                                          d'' <- mkDefn s d
                                          modify $ Map.insert (g, s) d''
                                          pure d''
                              t'   <- getTy d'
                              pure $ mkGApp an (defnName d') t' args' s
                  App an t e arg               -> App an t <$> specExp e <*> specExp arg
                  Lam an t b                   -> do
                        (vs, b') <- unbind b
                        Lam an t . bind vs <$> specExp b'
                  Case an t e p (Just e')      -> do
                        (ps, p') <- unbind p
                        Case an t <$> specExp e <*> (bind ps <$> specExp p') <*> (Just <$> specExp e')
                  Case an t e p Nothing        -> do
                        (ps, p') <- unbind p
                        Case an t <$> specExp e <*> (bind ps <$> specExp p') <*> pure Nothing
                  Match an t e p e' (Just e'') -> Match an t <$> specExp e <*> pure p <*> specExp e' <*> (Just <$> specExp e'')
                  Match an t e p e' Nothing    -> Match an t <$> specExp e <*> pure p <*> specExp e' <*> pure Nothing
                  LitList an t es              -> LitList an t <$> mapM specExp es
                  LitVec an t es               -> LitVec an t <$> mapM specExp es
                  TypeAnn an pt e              -> TypeAnn an pt <$> specExp e
                  e@LitInt {}                  -> pure e
                  e@LitStr {}                  -> pure e
                  e@Var {}                     -> pure e
                  e@Con {}                     -> pure e
                  e@Builtin {}                 -> pure e

            sig :: [Exp] -> AppSig
            sig = map $ \ e -> if | all isGlobal $ fv e -> Just $ unAnn e
                                  | otherwise           -> Nothing

            getTy :: Fresh m => Defn -> m Ty
            getTy Defn { defnPolyTy = Embed (Poly pt) } = snd <$> unbind pt

            mkDefn :: (MonadError AstError m, Fresh m) => AppSig -> Defn -> m Defn
            mkDefn s (Defn an g (Embed (Poly bgt)) inl (Embed body)) = do
                  g'              <- fresh g
                  gt              <- snd <$> unbind bgt
                  (tinfo, t')     <- mkTy s gt
                  (bodyvs, body') <- unbind body
                  typeCheckDefn ts vs
                       $ Defn an g' (Embed $ Poly $ bind (fv t') t') inl
                       $ Embed $ bind []
                               $ mkLam (lefts tinfo)
                               $ mkApp an (mkLam (zip (fst $ flattenArrow gt) bodyvs) (tyAnn (ann body') (Poly bgt) body'))
                               $ map (either (uncurry $ Var an) id) tinfo

                  where mkLam :: [(Ty, Name Exp)] -> Exp -> Exp
                        mkLam vs b = foldr (\ (t, v) e -> Lam an t $ bind v e) b vs

            mkTy :: (Fresh m, MonadError AstError m) => AppSig -> Ty -> m ([Either (Ty, Name Exp) Exp], Ty)
            mkTy s (flattenArrow -> (gtas, gtr)) = do
                  (gtas', subs) <- runStateT (zipWithM mkTy' gtas s) mempty
                  pure $ substs (Map.toList subs) (gtas', foldr arr gtr $ map fst (lefts gtas') <> drop (length gtas') gtas)
                  where mkTy' :: (Fresh m, MonadState TySub m, MonadError AstError m) => Ty -> Maybe Exp -> m (Either (Ty, Name Exp) Exp)
                        mkTy' t = \ case
                              Just e -> do
                                    t' <- unify (ann e) t $ typeOf e
                                    pure $ Right $ tyAnn (ann e) (poly' t') e
                              Nothing -> do
                                    n <- fresh $ s2n "sp"
                                    pure $ Left (t, n)

            mkGApp :: Annote -> Name Exp -> Ty -> [Exp] -> AppSig -> Exp
            mkGApp an g t es = mkApp an (Var an t g) . catMaybes . zipWith mkGApp' es
                  where mkGApp' :: Exp -> Maybe Exp -> Maybe Exp
                        mkGApp' e = maybe (Just e) (const Nothing)

data MatchResult = MatchYes ![(Name Exp, Exp)]
                 | MatchMaybe
                 | MatchNo
      deriving Show

-- | Partially evaluate expressions.
reduce :: (Fresh m, MonadError AstError m) => FreeProgram -> m FreeProgram
reduce (ts, syns, vs) = (ts, syns, ) <$> mapM reduceDefn vs
      where reduceDefn :: (Fresh m, MonadError AstError m) => Defn -> m Defn
            reduceDefn (Defn an n pt b (Embed e)) = unbind e >>= \ case
                  (vs, e') -> (Defn an n pt b . Embed) . bind vs <$> reduceExp e'

            reduceExp :: (Fresh m, MonadError AstError m) => Exp -> m Exp
            reduceExp = \ case
                  App an t e1 e2      -> do
                        e1' <- reduceExp e1
                        e2' <- reduceExp e2
                        case unTyAnn e1' of
                              Lam an' t' e -> do
                                    (x, e') <- unbind e
                                    reduceExp $ subst x (tyAnn an' (poly' t') e2') e'
                              _              -> pure $ App an t e1' e2'
                  Lam an t e        -> do
                        (x, e') <- unbind e
                        Lam an t . bind x <$> reduceExp e'
                  Case an t e e1 e2 -> do
                        (p, e1') <- unbind e1
                        e' <- reduceExp e
                        case matchPat e' p of
                              MatchYes sub -> reduceExp $ substs sub e1'
                              MatchMaybe   -> case e2 of
                                    Nothing  -> Case an t e' <$> (bind p <$> reduceExp e1') <*> pure Nothing
                                    Just e2' -> Case an t e' <$> (bind p <$> reduceExp e1') <*> (Just <$> reduceExp e2')
                              MatchNo      -> case e2 of
                                    Nothing  -> pure $ mkError an t "Pattern match failure (reduced)"
                                    Just e2' -> reduceExp e2'
                  -- TODO(chathhorn): handle match?
                  Match an t e p e' Nothing    -> Match an t <$> reduceExp e <*> pure p <*> reduceExp e' <*> pure Nothing
                  Match an t e p e' (Just e'') -> Match an t <$> reduceExp e <*> pure p <*> reduceExp e' <*> (Just <$> reduceExp e'')
                  LitList an t es              -> LitList an t <$> mapM reduceExp es
                  LitVec an t es               -> LitVec an t <$> mapM reduceExp es
                  TypeAnn an pt e              -> TypeAnn an pt <$> reduceExp e
                  e@LitInt {}                  -> pure e
                  e@LitStr {}                  -> pure e
                  e@Var {}                     -> pure e
                  e@Con {}                     -> pure e
                  e@Builtin {}                 -> pure e

            mergeMatches :: [MatchResult] -> MatchResult
            mergeMatches []     = MatchYes []
            mergeMatches (m:ms) = case mergeMatches ms of
                  MatchYes bs -> case m of
                        MatchYes bs' -> MatchYes $ bs' <> bs
                        MatchNo      -> MatchNo
                        MatchMaybe   -> MatchMaybe
                  MatchNo     -> MatchNo
                  MatchMaybe  -> case m of
                        MatchYes _ -> MatchMaybe
                        MatchNo    -> MatchNo
                        MatchMaybe -> MatchMaybe

            matchPat :: Exp -> Pat -> MatchResult
            matchPat e = \ case
                  PatCon _ _ (Embed i) pats -> case flattenApp' e of
                        Con _ _ c : es
                              | c == i && length es == length pats -> mergeMatches $ zipWith matchPat es pats
                              | otherwise                          -> MatchNo
                        _                                          -> MatchMaybe
                  PatVar _ _ x              -> MatchYes [(x, e)]
                  PatWildCard _ _           -> MatchYes []

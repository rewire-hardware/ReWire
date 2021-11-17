{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs, OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module ReWire.Crust.Transform
      ( inline, expandTypeSynonyms, reduce
      , neuterPrims
      , shiftLambdas
      , liftLambdas
      , fullyApplyDefs
      , purgeUnused
      ) where

import ReWire.Error
import ReWire.Unbound
      ( Fresh (..), s2n, n2s
      , substs, subst, unembed
      , isFreeName, runFreshM
      , Name (..)
      )
import ReWire.Annotation (Annote (..), Annotated (..), noAnn)
import ReWire.SYB
import ReWire.Crust.Syntax

import Control.Arrow (first)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.State (State, evalStateT, execState, StateT (..), get, modify)
import Control.Monad (replicateM)
import Data.Data (Data)
import Data.List (find, foldl')
import Data.Maybe (fromJust, fromMaybe)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Hashable (Hashable (..))
import Data.Text (Text)

import Data.Set (Set, union, (\\))
import qualified Data.Set as Set

-- | Inlines defs marked for inlining. Must run before lambda lifting.
inline :: MonadError AstError m => FreeProgram -> m FreeProgram
inline (ts, syns, ds) = (ts, syns, ) . flip substs ds <$> subs
      where toSubst :: Defn -> (Name Exp, Exp)
            toSubst (Defn _ n _ _ (Embed e)) = runFreshM $ unbind e >>= \ case
                  ([], e') -> pure (n, e')
                  _        -> error "Inlining: definition not inlinable (this shouldn't happen)"

            inlineDefs :: [Defn]
            inlineDefs = filter defnInline ds

            subs :: MonadError AstError m => m [(Name Exp, Exp)]
            subs = map toSubst <$> fix "INLINE definition" 100 (pure . (substs $ map toSubst inlineDefs)) inlineDefs

-- | Expands type synonyms.
expandTypeSynonyms :: (MonadCatch m, MonadError AstError m, Fresh m) => FreeProgram -> m FreeProgram
expandTypeSynonyms (ts, syns, ds) = (,,) <$> expandSyns ts <*> syns' <*> expandSyns ds
      where toSubst :: TypeSynonym -> (Name TyConId, Bind [Name Ty] Ty)
            toSubst (TypeSynonym _ n (Embed (Poly t))) = (n, t)

            expandSyns :: (MonadCatch m, MonadError AstError m, Fresh m, Data d) => d -> m d
            expandSyns d = subs >>= flip substs' d

            subs :: (MonadCatch m, MonadError AstError m, Fresh m) => m [(Name TyConId, Bind [Name Ty] Ty)]
            subs = map toSubst <$> syns'

            syns' :: (MonadCatch m, MonadError AstError m, Fresh m) => m [TypeSynonym]
            syns' = fix "Type synonym" 100 (substs' $ map toSubst syns) syns

            substs' :: (MonadCatch m, MonadError AstError m, Fresh m, Data d) => [(Name TyConId, Bind [Name Ty] Ty)] -> d -> m d
            substs' subs' = runT (transform $ \ case
                  t@(TyCon an n) -> case lookup n subs' of
                        Just pt -> do
                              (vs, t') <- unbind pt
                              if length vs == 0
                                    then pure t'
                                    else pure t
                  t@(TyApp an a b) -> case findTyCon a of
                        Just (n, args) -> case lookup n subs' of
                              Just pt -> do
                                    (vs, t') <- unbind pt
                                    let args' = args ++ [b]
                                    if length vs == length args'
                                          then pure $ substs (zip vs args') t'
                                          else pure t)

            findTyCon :: Ty -> Maybe (Name TyConId, [Ty])
            findTyCon = \ case
                  TyCon _ n   -> pure (n, [])
                  TyApp _ a b -> do
                        (n, args) <- findTyCon a
                        pure (n, args ++ [b])
                  _           -> Nothing

fix :: (MonadError AstError m, Hashable a) => Text -> Int -> (a -> m a) -> a -> m a
fix m 0 _ _ = failAt noAnn $ m <> " expansion not terminating (mutually recursive definitions?)."
fix m n f a = do
      a' <- f a
      if hash a' == hash a then pure a else fix m (n - 1) f a'

-- | Replaces the expression in NativeVHDL so we don't descend into it
--   during other transformations.
neuterPrims :: MonadCatch m => FreeProgram -> m FreeProgram
neuterPrims = runT $ transform $
      \ (NativeVHDL an s e) -> pure $ NativeVHDL an s (Error an (typeOf e) "nativeVHDL expression placeholder")

-- | Shifts vars bound by top-level lambdas into defs.
-- > g = \ x1 -> \ x2 -> e
--   becomes
-- > g x1 x2 = e
shiftLambdas :: Fresh m => FreeProgram -> m FreeProgram
shiftLambdas (ts, syns, vs) = (ts, syns, ) <$> mapM shiftLambdas' vs
      where shiftLambdas' :: Fresh m => Defn -> m Defn
            shiftLambdas' (Defn an n t inl (Embed e)) = Defn an n t inl . Embed <$> mash e

            mash :: Fresh m => Bind [Name Exp] Exp -> m (Bind [Name Exp] Exp)
            mash e = unbind e >>= \ case
                  (vs, Lam _ _ b) -> do
                        (v, b') <- unbind b
                        mash $ bind (vs ++ [v]) b'
                  _ -> pure e

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
                              fullyApply $ bind (vs ++ [x]) $ appl (Var (ann e') t x) e'
                        _                                             -> pure e

            appl :: Exp -> Exp -> Exp
            appl x = \ case
                  Match an t e1 p e lvars Nothing    -> Match an (arrowRight t) e1 p e (lvars ++ [x]) Nothing
                  Match an t e1 p e lvars (Just els) -> Match an (arrowRight t) e1 p e (lvars ++ [x]) $ Just $ appl x els
                  e                                  -> App (ann e) e x

-- | Lifts lambdas and case/match into a top level fun def.
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

                        let t' = foldr arr (typeOf e') $ map snd bvs ++ [t]
                        f     <- fresh $ s2n "$LL.lambda"

                        modify $ (:) $ Defn an f (fv t' |-> t') False (Embed $ bind (fvs ++ [x]) e')
                        pure $ foldl' (App an) (Var an t' f) $ map (toVar an . first promote) bvs
                  Case an t e1 b e2 -> do
                        (p, e)    <- unbind b
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let pvs = patVars p

                        let t' = foldr arr (typeOf e) $ map snd bvs ++ map snd pvs
                        f     <- fresh $ s2n "$LL.case"

                        modify $ (:) $ Defn an f (fv t' |-> t') False (Embed $ bind (fvs ++ map fst pvs) e')
                        pure $ Match an t e1 (transPat p) (Var an t' f) (map (toVar an . first promote) bvs) e2
                  -- TODO(chathhorn): This case is really just normalizing Match, consider moving to ToCore.
                  Match an t e1 p e lvars els | liftable e -> do
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (arr . snd) (typeOf e) bvs
                        f     <- fresh $ s2n "$LL.match"

                        modify $ (:) $ Defn an f (fv t' |-> t') False (Embed $ bind fvs e')
                        pure $ Match an t e1 p (Var an t' f) (map (toVar an) bvs ++ lvars) els
                  -- Lifts matches in the operator position of an application.
                  -- TODO(chathhorn): move somewhere else?
                  App an e@Match {} arg -> do
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (arr . snd) (typeOf e) bvs
                        f     <- fresh $ s2n "$LL.matchapp"

                        modify $ (:) $ Defn an f (fv t' |-> t') False (Embed $ bind fvs e')
                        pure $ App an
                              (foldl' (App an) (Var an t' f) $ map (toVar an) bvs)
                              arg
                  ||> (\ ([] :: [Defn]) -> get) -- this is cute!
                  ||> TId

            liftable :: Exp -> Bool
            liftable = \ case
                  Var _ _ _ -> False
                  _         -> True

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
            bv = nubOrdOn fst . runQ (query $ \ case
                  Var _ t n | not $ isFreeName n -> [(n, t)]
                  _                              -> [])

            patVars :: Pat -> [(Name Exp, Ty)]
            patVars = \ case
                  PatCon _ _ _ ps      -> concatMap patVars ps
                  PatVar _ (Embed t) x -> [(x, t)]

            promote :: Name a -> Name a
            promote (Bn l k) | l >= 0 = Bn (l - 1) k
            promote b = b

            transPat :: Pat -> MatchPat
            transPat = \ case
                  PatCon an (Embed t) (Embed c) ps -> MatchPatCon an t c $ map transPat ps
                  PatVar an (Embed t) _            -> MatchPatVar an t

-- | Remove unused definitions.
purgeUnused :: FreeProgram -> FreeProgram
purgeUnused (ts, syns, vs) = (inuseData (fv $ trec $ inuseDefn vs) ts, syns, inuseDefn vs)
      where inuseData :: [Name DataConId] -> [DataDefn] -> [DataDefn]
            inuseData ns = filter (not . null . dataCons) . map (inuseData' ns)

            inuseData' :: [Name DataConId] -> DataDefn -> DataDefn
            inuseData' ns d@(DataDefn an n k cs)
                  | n2s n `elem` reservedData = d
                  | otherwise                 = DataDefn an n k $ filter ((`Set.member` Set.fromList ns) . dataConName) cs

            reservedData :: [Text]
            reservedData =
                         [ "PuRe"
                         , "(,)"
                         , "()"
                         ]

            dataConName :: DataCon -> Name DataConId
            dataConName (DataCon _ n _) = n

inuseDefn :: [Defn] -> [Defn]
inuseDefn ds = map toDefn $ Set.elems $ execState (inuseDefn' ds') ds'
      where inuseDefn' :: Set (Name Exp) -> State (Set (Name Exp)) ()
            inuseDefn' ns | Set.null ns = pure ()
                          | otherwise   = do
                  inuse  <- get
                  modify $ union (fvs ns)
                  inuse' <- get
                  inuseDefn' $ inuse' \\ inuse

            reservedDefn :: [Text]
            reservedDefn =
                         [ "Main.start"
                         , "unfold"
                         ]

            ds' :: Set (Name Exp)
            ds' = Set.fromList $ filter (flip elem reservedDefn . n2s) $ map defnName ds

            fvs :: Set (Name Exp) -> Set (Name Exp)
            fvs = Set.fromList . concatMap (fv . unembed . defnBody . toDefn) . Set.elems

            toDefn :: Name Exp -> Defn
            toDefn n = fromJust $ find ((==n) . defnName) ds

-- | Partially evaluate expressions.
reduce :: (Fresh m, MonadError AstError m) => FreeProgram -> m FreeProgram
reduce (ts, syns, vs) = (ts, syns, ) <$> mapM reduceDefn vs

reduceDefn :: (Fresh m, MonadError AstError m) => Defn -> m Defn
reduceDefn (Defn an n pt b (Embed e)) = unbind e >>= \ case
      (vs, e') -> (Defn an n pt b . Embed) . bind vs <$> reduceExp e'

reduceExp :: (Fresh m, MonadError AstError m) => Exp -> m Exp
reduceExp = \ case
      App an e1 e2      -> do
            e1' <- reduceExp e1
            e2' <- reduceExp e2
            case e1' of
                  Lam _ _ e -> do
                        (x, e') <- unbind e
                        reduceExp $ subst x e2' e'
                  _              -> pure $ App an e1' e2'
      Lam an t e      -> do
            (x, e') <- unbind e
            Lam an t . bind x <$> reduceExp e'
      Case an t e e1 e2 -> do
            (p, e1') <- unbind e1
            e' <- reduceExp e
            let mr = matchPat e' p
            case mr of
                  MatchYes sub -> reduceExp $ substs sub e1'
                  MatchMaybe   -> case e2 of
                        Nothing  -> Case an t e' <$> (bind p <$> reduceExp e1') <*> pure Nothing
                        Just e2' -> Case an t e' <$> (bind p <$> reduceExp e1') <*> (Just <$> reduceExp e2')
                  MatchNo      -> case e2 of
                        Nothing  -> pure $ Error an t "Pattern match failure (reduced)"
                        Just e2' -> reduceExp e2'
      e -> pure e

data MatchResult = MatchYes ![(Name Exp, Exp)]
                 | MatchMaybe
                 | MatchNo
                 deriving Show

mergeMatches :: [MatchResult] -> MatchResult
mergeMatches []     = MatchYes []
mergeMatches (m:ms) = case mergeMatches ms of
      MatchYes bs -> case m of
            MatchYes bs' -> MatchYes $ bs' ++ bs
            MatchNo      -> MatchNo
            MatchMaybe   -> MatchMaybe
      MatchNo     -> MatchNo
      MatchMaybe  -> case m of
            MatchYes _ -> MatchMaybe
            MatchNo    -> MatchNo
            MatchMaybe -> MatchMaybe

matchPat :: Exp -> Pat -> MatchResult
matchPat e = \ case
      PatCon _ _ (Embed i) pats -> case flattenApp e of
            Con _ _ c : es
                  | c == i && length es == length pats -> mergeMatches $ zipWith matchPat es pats
                  | otherwise                          -> MatchNo
            _                                          -> MatchMaybe
      PatVar _ _ x            -> MatchYes [(x, e)]

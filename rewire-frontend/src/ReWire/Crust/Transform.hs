{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ReWire.Crust.Transform
      ( reduce
      , neuterExterns
      , shiftLambdas
      , liftLambdas
      , etaAbsDefs
      , purgeAll
      ) where

import ReWire.Annotation (Annote (..), Annotated (..))
import ReWire.Crust.Syntax (Exp (..), Kind (..), Ty (..), Poly (..), Pat (..), Defn (..), FreeProgram, DataCon (..), DataConId, TyConId, DataDefn (..), Builtin (..), flattenApp)
import ReWire.Crust.TypeCheck (unify')
import ReWire.Crust.Types (typeOf, tyAnn, setTyAnn, dstArrow, poly', arr, ctorNames, resInputTy, codomTy, (|->), arrowRight, prettyTy, synthable, higherOrder, fundamental, concrete)
import ReWire.Crust.Util (mkApp, mkError, mkLam, patVars, toVar)
import ReWire.Error (AstError, MonadError, failInternal, Warning (..))
import ReWire.Fix (fix')
import ReWire.SYB (transform, query, queryWith)
import ReWire.Unbound (freshVar, fv, Fresh, s2n, n2s, substs, subst, unembed, isFreeName, runFreshM, runFreshMT, Name (..), unsafeUnbind, bind, unbind, Subst (..), Embed (Embed), Bind)

import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Control.Monad.State (MonadState, execState, StateT (..), get, modify)
import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet, union, difference)
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Text (Text, isPrefixOf)
import Data.Tuple (swap)

import qualified Data.Text           as Text
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set

-- | Decides, per extern, whether the user-supplied Haskell implementation
--   (rwPrimExtern's seventh argument) can serve as a model for the
--   interpreter, and replaces it with an inert placeholder otherwise (so
--   later transformations don't descend into it). An implementation is kept
--   only when it is a reference to a top-level definition -- other than the
--   extern's own enclosing definition, since @f = extern "f" f@ is the
--   conventional way to declare an extern with no model -- whose reachable
--   definitions are all non-recursive, first-order, monomorphic, and free of
--   un-synthesizable types; kept references survive to ToHyle, which
--   attaches them to the Hyle extern declaration. Implementations that look like real models but
--   fail these checks are neutered with a warning.
--
--   Right after typechecking, an extern application is still a beta-redex
--   from inlining the extern/externWithSig wrappers (with the implementation
--   bound to a lambda parameter), so defns containing externs are
--   beta-reduced first to expose the implementation argument. The reduction
--   runs in an isolated FreshMT (safe, since every name it unbinds is
--   re-bound in the result), so the pipeline's freshness counter -- and
--   hence the generated names in every non-extern program -- is unaffected.
neuterExterns :: MonadError AstError m => FreeProgram -> m (FreeProgram, [Warning])
neuterExterns (ts, syns, ds) = do
      ds' <- mapM reduce1 ds
      pure $ neuterExterns' (ts, syns, ds')
      where reduce1 :: MonadError AstError m => Defn -> m Defn
            reduce1 d@(Defn an n pt b (Embed e))
                  | hasExtern d = runFreshMT $ unbind e >>= \ (vs, e') -> Defn an n pt b . Embed . bind vs <$> reduceExp e'
                  | otherwise   = pure d

            hasExtern :: Defn -> Bool
            hasExtern d = or [ True | Builtin _ _ _ Extern <- query d :: [Exp] ]

neuterExterns' :: FreeProgram -> (FreeProgram, [Warning])
neuterExterns' (ts, syns, ds) = ((ts, syns, ds2), warns)
      where -- Stage 1: neuter self-referential implementations (silently:
            -- the no-model idiom), so they don't appear as spurious cycles in
            -- stage 2's recursion check.
            ds1 :: [Defn]
            ds1 = map (\ d -> transform (neuterImpl (isSelf d)) d) ds

            -- Stage 2: neuter everything else that isn't a usable model.
            ds2 :: [Defn]
            ds2 = map (transform (neuterImpl (isJust . disposition))) ds1

            isSelf :: Defn -> Exp -> Bool
            isSelf d = \ case
                  Var _ _ _ x -> x == defnName d
                  _           -> False

            -- | Nothing: keep as a model. Just Nothing: neuter silently
            --   (already a placeholder, or a lambda-bound name -- meaning
            --   this is the body of a generic wrapper like rewire-user's
            --   "extern", not a use site). Just (Just r): neuter, warning r.
            disposition :: Exp -> Maybe (Maybe Text)
            disposition = \ case
                  e | isPlaceholder e        -> Just Nothing
                  Var _ _ _ x | isFreeName x -> Just <$> verdict mempty (n2s x)
                              | otherwise    -> Just Nothing
                  _                          -> Just $ Just "only a reference to a top-level definition can be used as a model"

            warns :: [Warning]
            warns = [ Warning (ann d) $ "Ignoring the Haskell model for extern '" <> exName ex <> "': " <> reason
                          <> "; the interpreter will not be able to evaluate this extern."
                    | d <- ds1
                    , App _ _ _ ex e <- query d :: [Exp]
                    , isExtern ex
                    , Just (Just reason) <- [disposition e]
                    ]

            -- | DFS from a defn: every reachable defn must exist, have a
            --   usable type, and the call graph must be acyclic. Returns the
            --   first reason for rejection, or Nothing if usable as a model.
            verdict :: HashSet Text -> Text -> Maybe Text
            verdict stack x
                  | x `Set.member` stack = Just $ x <> " is recursive"
                  | otherwise            = case Map.lookup x dmap of
                        Nothing -> Just $ x <> " does not refer to a top-level definition"
                        Just d  -> case typeReason d of
                              Just r  -> Just r
                              Nothing -> foldr (orElse . verdict (Set.insert x stack)) Nothing $ succs d
                  where orElse :: Maybe a -> Maybe a -> Maybe a
                        orElse a b = maybe b Just a

            typeReason :: Defn -> Maybe Text
            typeReason (Defn _ n (Embed (Poly pt)) _ _)
                  | higherOrder t       = reason "has a higher-order type"
                  | not (fundamental t) = reason "has String or Integer arguments"
                  | not (concrete t)    = reason "is polymorphic"
                  | not (synthable t)   = reason "has an unsynthesizable type"
                  | otherwise           = Nothing
                  where t :: Ty
                        t = runFreshM $ snd <$> unbind pt

                        reason :: Text -> Maybe Text
                        reason r = Just $ n2s n <> " " <> r

            -- | Free variables of the defn body (n.b., fv on the Embed
            --   wrapper itself, a pattern-position type, finds nothing).
            succs :: Defn -> [Text]
            succs d = nubOrd $ map n2s $ filter isFreeName (fv (unembed $ defnBody d) :: [Name Exp])

            dmap :: HashMap Text Defn
            dmap = Map.fromList $ map ((n2s . defnName) &&& id) ds1

            neuterImpl :: (Exp -> Bool) -> Exp -> Exp
            neuterImpl p = \ case
                  App an tan t ex e | isExtern ex, p e, not (isPlaceholder e)
                              -> App an tan t ex
                                       $ setTyAnn (poly' <$> typeOf e)
                                       $ mkError (ann e) (typeOf e) "Extern expression placeholder"
                  e           -> e

            isPlaceholder :: Exp -> Bool
            isPlaceholder e = case flattenApp e of
                  (Builtin _ _ _ Error, _) -> True
                  _                        -> False

            exName :: Exp -> Text
            exName e = case flattenApp e of
                  (_, [_, _, _, _, _, LitStr _ _ s]) -> s
                  _                                  -> "<unknown>"

            isExtern :: Exp -> Bool
            isExtern e = case flattenApp e of
                  (Builtin _ _ _ Extern, [_, _, _, _, _, _]) -> True
                  _                                          -> False

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
                  (vs, Lam _ _ _ b) -> do
                        (v, b') <- unbind b
                        mash $ bind (vs <> [v]) b'
                  _               -> pure e

-- | So if e :: a -> b, then
-- > g = e
--   becomes
-- > g x0 = e x0
-- except with Match, we add x0 to the list of local ids.
etaAbsDefs :: Fresh m => FreeProgram -> m FreeProgram
etaAbsDefs (ts, syns, vs) = (ts, syns, ) <$> mapM etaAbsDefs' vs
      where etaAbsDefs' :: Fresh m => Defn -> m Defn
            etaAbsDefs' (Defn an n t inl (Embed e)) = Defn an n t inl . Embed <$> etaAbs e

            etaAbs :: Fresh m => Bind [Name Exp] Exp -> m (Bind [Name Exp] Exp)
            etaAbs e = do
                  (vs, e') <- unbind e
                  case typeOf e' of
                        Just (dstArrow -> Just (t, _)) -> do
                              x   <- freshVar "$x"
                              e'' <- appl t (Var (ann e') Nothing (Just t) x) e'
                              etaAbs $ bind (vs <> [x]) e''
                        _                                                    -> pure e

            appl :: Fresh m => Ty -> Exp -> Exp -> m Exp
            appl t x = \ case
                  -- Push the applied argument into each case branch (under its
                  -- binder) and the default.
                  Case an tan t' disc bnd els -> do
                        (p, e) <- unbind bnd
                        e'     <- appl t x e
                        els'   <- mapM (appl t x) els
                        pure $ Case an tan (arrowRight <$> t') disc (bind p e') els'
                  e                           -> pure $ mkApp (ann e) e [x]

-- | Lifts lambdas and case/match into a top level fun def.
--   E.g., lifts
--   `\ x -> \y -> ... -> e`
--   to a global definition
--   `g = \ x -> \ y -> ... -> e`
--   Doesn't lift anything with a higher-order type.
liftLambdas :: (Fresh m, MonadError AstError m) => FreeProgram -> m FreeProgram
liftLambdas (ts, syns, ds) = (ts, syns,) . uncurry (<>) <$> runStateT (mapM llDefn ds) []

-- | Don't lambda-lift initial lambdas on RHS of top-level binding.
llDefn :: (Fresh m, MonadState [Defn] m, MonadError AstError m) => Defn -> m Defn
llDefn d@Defn { defnName = dn, defnBody = Embed e } = do
      (bvs, e') <- unbind e
      e''       <- llDefnBody (n2s dn) (Set.fromList bvs) e'
      pure $ d { defnBody = Embed (bind bvs e'') }

      where llDefnBody :: (Fresh m, MonadState [Defn] m, MonadError AstError m) => Text -> HashSet (Name Exp) -> Exp -> m Exp
            llDefnBody dn bvs = \ case
                  Lam an pt t e -> do
                        (v, e') <- unbind e
                        e''     <- llDefnBody dn (Set.insert v bvs) e'
                        pure $ Lam an pt t $ bind v e''
                  e             -> llExp dn bvs e

llExp :: (MonadState [Defn] m, MonadError AstError m, Fresh m) => Text -> HashSet (Name Exp) -> Exp -> m Exp
llExp dn bvs =  \ case
      Lam an tan t b -> do
            (x, b') <- unbind b
            case b' of
                  -- Eta-reduce instead of lift (duplicated from reduceExp).
                  App _ _ _ e1 (Var _ _ _ x')
                        | not (isBuiltin e1) -- TODO: ugly special case to accomodate purify.
                        , x == x'
                        , x `notElem` fv e1 -> llExp' e1
                  _                         -> do
                        b'' <- llExp dn (Set.insert x bvs) b'
                        lift' $ Lam an tan t $ bind x b''

      -- Recurse under the case binder, lifting genuine lambdas in the branch;
      -- the discriminant and pattern variables are left in place for ToHyle.
      Case an tan t disc bnd els -> do
            (p, e) <- unbind bnd
            Case an tan t <$> llExp' disc <*> (bind p <$> llExp dn (Set.fromList (snd <$> patVars p) <> bvs) e) <*> mapM llExp' els
      App an tan t e1 e2  -> App an tan t <$> llExp' e1 <*> llExp' e2
      LitList an tan t es -> LitList an tan t <$> mapM llExp' es
      LitVec an tan t es  -> LitVec an tan t <$> mapM llExp' es
      e                   -> pure e

      where llExp' :: (Fresh m, MonadState [Defn] m, MonadError AstError m) => Exp -> m Exp
            llExp' = llExp dn bvs

            lift' :: (Fresh m, MonadState [Defn] m, MonadError AstError m) => Exp -> m Exp
            lift' = lift dn bvs

            isBuiltin :: Exp -> Bool
            isBuiltin = \ case
                  Builtin {} -> True
                  _          -> False

-- | Lifts an expression to a definition and returns an application (to pass
--   any free variables). Argument is a list of non-global free variables.
lift :: (Fresh m, MonadState [Defn] m, MonadError AstError m) => Text -> HashSet (Name Exp) -> Exp -> m Exp
lift dn bvs e | Just t  <- typeOf e
              , not $ isGlobal e = do
      fvs    <- filter ((`Set.member` bvs) . snd) <$> freevars e
      let t'  = foldr arr t $ fst <$> fvs
          an  = ann e

      if not $ synthable t' then pure e else do
            f      <- freshVar $ prefix dn
            d      <- llDefn $ Defn an f (fv t' |-> t') Nothing $ Embed $ bind [] $ mkLam an fvs e
            modify (d :)
            pure $ setTyAnn (tyAnn e) $ mkApp an (Var an Nothing (Just t') f) $ toVar an <$> fvs

      where -- | Get well-typed free variables.
            freevars :: (MonadError AstError m, Data a) => a -> m [(Ty, Name Exp)]
            freevars a = map swap . Map.toList <$> mapM (foldM unifyVs (TyVar an KStar $ s2n "?lift")) fvs'
                  where fvs' :: HashMap (Name Exp) [Maybe Ty]
                        fvs' = Map.fromListWith (<>) [(n, [t]) | Var _ _ t n <- query a, isFreeName n]

                        unifyVs :: MonadError AstError m => Ty -> Maybe Ty -> m Ty
                        unifyVs t1 = \ case
                              Nothing -> pure t1
                              Just t2 -> maybe (failInternal (ann t1) $ "Mis-typed variables; could not unify: " <> prettyTy t1 <> " with " <> prettyTy t2) pure
                                       $ unify' t1 t2

                        an :: Annote
                        an = MsgAnnote "Lift: dummy type variable"

            isGlobal :: Exp -> Bool
            isGlobal = \ case
                  Var _ _ _ n -> not (n `Set.member` bvs)
                  _           -> False

            prefix :: Text -> Text
            prefix x | pre `isPrefixOf` x = x
                     | "$" `isPrefixOf` x = pre <> Text.drop 1 x
                     | otherwise          = pre <> x

            pre :: Text
            pre = "$LL."
lift _ _ e                                            = pure e

-- | Definitions transitively reachable from the given roots.
liveDefns :: HashSet (Name Exp) -> [Defn] -> [Defn]
liveDefns roots ds = map toDefn $ Set.toList $ execState (live ds') ds'
      where live :: MonadState (HashSet (Name Exp)) m => HashSet (Name Exp) -> m ()
            live ns | Set.null ns = pure () -- TODO(chathhorn): rewrite using fix?
                    | otherwise   = do
                  inuse  <- get
                  modify $ union $ fvs ns
                  inuse' <- get
                  live $ inuse' `difference` inuse

            ds' :: HashSet (Name Exp)
            ds' = Set.fromList $ filter (`Set.member` roots) $ map defnName ds

            fvs :: HashSet (Name Exp) -> HashSet (Name Exp)
            fvs = Set.fromList . concatMap (fv . unembed . defnBody . toDefn) . Set.toList

            byName :: HashMap (Name Exp) Defn
            byName = Map.fromList $ map (\ d -> (defnName d, d)) ds

            toDefn :: Name Exp -> Defn
            toDefn n | Just d <- Map.lookup n byName = d
                     | otherwise                     = error $ "Something went wrong: can't find symbol: " <> show n

-- | Remove all definitions and types unused by those in the given lists.
purgeAll :: Applicative m => Name Exp -> FreeProgram -> m FreeProgram
purgeAll start = pure . purgeUnused [start] []

purgeUnused :: [Name Exp] -> [Name TyConId] -> FreeProgram -> FreeProgram
-- Note: collect ctor names with a query instead of `fv $ trec vs'`: no
-- binder can capture a DataConId, so every occurrence is free, and closing
-- over the whole program (trec) is quadratic in the number of binders.
purgeUnused except exceptTs (ts, syns, vs) = (inuseData (fix' extendWithCtorParams $ externCtors vs') (query vs' :: [Name DataConId]) ts, syns, vs')
      where vs' :: [Defn]
            vs' = liveDefns exceptSet vs

            exceptSet :: HashSet (Name Exp)
            exceptSet = Set.fromList except

            exceptTsSet :: HashSet (Name TyConId)
            exceptTsSet = Set.fromList exceptTs

            inuseData :: [Name TyConId] -> [Name DataConId] -> [DataDefn] -> [DataDefn]
            inuseData (Set.fromList -> ts) (Set.fromList -> ns) = filter (\ DataDefn {dataName = n, dataCons = cs} -> not (null cs) || n `Set.member` exceptTsSet)
                            . map (inuseData' ts ns)

            inuseData' :: HashSet (Name TyConId) -> HashSet (Name DataConId) -> DataDefn -> DataDefn
            inuseData' ts ns d@(DataDefn an n k cs)
                  | n `Set.member` ts          = d
                  | n `Set.member` exceptTsSet = d
                  | otherwise                  = DataDefn an n k $ filter ((`Set.member` ns) . dataConName) cs

            -- | Also treat as used: all ctors for types returned by externs and ReacT inputs.
            --   Collected with the single-sweep queryWith: an expression-typed
            --   query re-extracts every node's children per level, which is
            --   quadratic on the deeply bind-nested bodies this pass sees
            --   mid-pipeline (and was most of rwc's total compile time on
            --   large programs).
            externCtors :: [Defn] -> [Name TyConId]
            externCtors ds = concat $ queryWith builtinCtors ds
                  <> [maybe [] ctorNames $ resInputTy t | Defn _ n (Embed (Poly (unsafeUnbind -> (_, t)))) _ _ <- ds, n `Set.member` exceptSet]
                  where builtinCtors :: Exp -> [[Name TyConId]]
                        builtinCtors = \ case
                              e@Builtin {} -> [maybe [] (ctorNames . codomTy) $ typeOf e]
                              _            -> []

            -- Note: concatMap, not a fold of appends: a left-nested (<>)
            -- chain is quadratic in the (occurrence-many, un-nubbed) name
            -- list this receives from externCtors.
            extendWithCtorParams :: [Name TyConId] -> [Name TyConId]
            extendWithCtorParams = nubOrd . sort . concatMap (\ n -> Map.lookupDefault [] (n2s n) ctorParams)

            -- | Type constructors appearing in ctor fields, by datatype name.
            ctorParams :: HashMap Text [Name TyConId]
            ctorParams = Map.fromListWith (<>) [(n2s $ dataName d, concatMap ctorTypes $ dataCons d) | d <- ts]
                  where ctorTypes :: DataCon -> [Name TyConId]
                        ctorTypes (DataCon _ _ (Embed (Poly (unsafeUnbind -> (_, t))))) = ctorNames t

            dataConName :: DataCon -> Name DataConId
            dataConName (DataCon _ n _) = n

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
      App an tan t e1 e2      -> do
            e1' <- reduceExp e1
            e2' <- reduceExp e2
            case e1' of
                  Lam _ _ tx e -> do
                        (x, e') <- unbind e
                        beta <- reduceExp $ subst x (setTyAnn (poly' <$> tx) e2') e' -- TODO(chathhorn): ad-hoc promoting current type to annotation.
                        pure $ setTyAnn tan beta
                  _              -> pure $ App an tan t e1' e2'
      Lam an tan t e        -> do
            (x, e') <- unbind e
            e''     <- reduceExp e'
            -- Eta reduce.
            case e'' of
                  App _ _ _ e1 (Var _ _ _ x')
                        | x == x'
                        , x `notElem` fv e1 -> reduceExp e1
                  _                         -> pure $ Lam an tan t $ bind x e''
      Case an tan t e e1 e2 -> do
            (p, e1') <- unbind e1
            e'       <- reduceExp e
            case matchPat e' p of
                  MatchYes sub -> reduceExp $ substs sub e1'
                  MatchMaybe   -> Case an tan t e' <$> (bind p <$> reduceExp e1') <*> mapM reduceExp e2
                  MatchNo      -> maybe (pure $ mkError an t "Pattern match failure: non-exhaustive patterns in case") reduceExp e2
      LitList an tan t es              -> LitList an tan t <$> mapM reduceExp es
      LitVec an tan t es               -> LitVec an tan t <$> mapM reduceExp es
      e@LitInt {}                      -> pure e
      e@LitStr {}                      -> pure e
      e@Var {}                         -> pure e
      e@Con {}                         -> pure e
      e@Builtin {}                     -> pure e

      where mergeMatches :: [MatchResult] -> MatchResult
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
                  PatCon _ _ _ (Embed i) pats -> case flattenApp e of
                        (Con _ _ _ c, es)
                              | c == i && length es == length pats -> mergeMatches $ zipWith matchPat es pats
                              | otherwise                          -> MatchNo
                        _                                          -> MatchMaybe
                  PatVar _ _ _ x              -> MatchYes [(x, e)]
                  PatWildCard {}              -> MatchYes []

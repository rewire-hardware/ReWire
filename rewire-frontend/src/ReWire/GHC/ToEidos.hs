{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The Core -> Eidos bridge: translates the -O0 desugared Core of the
--   whole home module graph into an Eidos-P 'Program' (doc/eidos.md §3) by
--   near-transliteration. Where the retired Crust bridge lowered lets to
--   beta-redexes (with demand-order rescheduling), crushed cases to
--   single-arm cascades, and discarded type instantiations for the
--   typechecker to re-infer, this bridge PRESERVES them: Core lets become
--   'Let's (recursive groups included), join-tagged binders become 'Join'
--   binds with 'Jump's at their call sites, cases stay n-ary with their
--   case binder, and type arguments ride along as 'TArg's (driving the
--   substitution-based specializer). Types are bridged fail-fast: an
--   untranslatable type is a located error at the occurrence, never a
--   silent Nothing.
--
--   Inherited from the retired Crust bridge (unchanged): the reachability walk
--   from the start symbol + rwPrim* roots, evidence erasure (dictionary
--   arguments/binders/binds erased; user-class dictionaries kept as data),
--   rwPrim* recognition, the reactive-monad class-op recognition (Bind and
--   Return at ReacT/StateT/Identity or at an unresolved monad type),
--   Integer/String/list literal folding, INLINE-pragma ride-along, and the
--   base vocabulary. Name and type classification lives in
--   "ReWire.GHC.Recognize".
--
--   Uniques: every Eidos binder gets a fresh unique from a supply threaded
--   through the translation (GHC Core does not guarantee globally unique
--   binders); occurrences resolve through the context maps.
module ReWire.GHC.ToEidos (toEidos) where

import ReWire.Annotation (Annote, noAnn)
import ReWire.Config (Config, start)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Eidos.PrimBasis (addPrims)
import ReWire.Eidos.Types (flattenArrow, flattenTyApp)
import ReWire.GHC.Recognize (uKey, spanAnnote, varAnnote, isPrimModule, isPrimVar, homeishMod, qualName, conName, tupleName, splitStart, erasedArg, erasedEv, userPred, tyConModule, tyConKey, tyConTable, vocabTable)

import qualified ReWire.Builtins     as B
import qualified ReWire.Eidos.Syntax as E

import Control.Lens ((^.))
import Control.Monad (unless, zipWithM)
import Control.Monad.State.Strict (StateT, evalStateT, get, put, lift)
import Data.List (elemIndex)
import Data.Text (Text, pack)

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import qualified Data.Text.Encoding as TE

import GHC (ModuleName, moduleName, moduleNameString)
import GHC.Builtin.Types (integerISDataCon, integerIPDataCon, integerINDataCon, listTyCon, nilDataCon, consDataCon, unitTyCon, boolTyCon, charTyCon, integerTyCon, naturalTyCon, unboxedUnitTyCon, unboxedUnitDataCon)
import GHC.Builtin.Types.Literals (typeNatAddTyCon, typeNatSubTyCon, typeNatMulTyCon)
import GHC.Core (CoreExpr, CoreBind, Expr (..), Bind (..), Alt (..), AltCon (..), collectArgs)
import GHC.Core.Class (classAllSelIds, classTyCon)
import GHC.Core.DataCon (DataCon, dataConOrigArgTys, dataConUnivTyVars, dataConExTyCoVars, dataConTheta)
import GHC.Core.Predicate (isEvVarType)
import GHC.Core.TyCo.Rep (Type (..), TyLit (..), scaledThing)
import GHC.Core.TyCon (TyCon, tyConName, tyConKind, tyConDataCons, tyConTyVars, isClassTyCon, isAlgTyCon, isTypeSynonymTyCon, isBoxedTupleTyCon, tyConArity, isNewTyCon, tyConSingleDataCon)
import GHC.Core.Type (expandTypeSynonyms, mkTyVarTy, mkTyVarTys, splitForAllTyCoVars, substTyWith)
import GHC.Core.Utils (exprType)
import GHC.Types.Basic (InlineSpec (..), inlinePragmaSpec, JoinPointHood (..))
import GHC.Types.Id (isClassOpId_maybe, idInlinePragma, isDataConId_maybe, isDFunId, idJoinPointHood)
import GHC.Types.Id.Make (voidPrimId)
import GHC.Types.Literal (Literal (..))
import GHC.Types.Name (getOccString, nameModule_maybe, nameSrcSpan)
import GHC.Types.Var (Var, varName, varType, isTyVar)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHC.Utils.Outputable (showSDocUnsafe, ppr)

-- | The unique supply.
type BM m = StateT E.Uniq m

freshU :: Monad m => BM m E.Uniq
freshU = do
      u <- get
      put $ u + 1
      pure u

freshId :: Monad m => Text -> E.Sig -> BM m E.Id
freshId occ sig = do
      u <- freshU
      pure $ E.Id occ u sig

-- | Translation context.
data Ctx = Ctx
      { ctxTops   :: IM.IntMap E.Id     -- ^ top-level GHC unique -> Eidos Id.
      , ctxLocals :: IM.IntMap E.Id     -- ^ local GHC unique -> Eidos Id.
      , ctxJoins  :: IM.IntMap E.JoinId -- ^ join-point GHC unique -> Eidos JoinId.
      , ctxTyVars :: IM.IntMap E.TyVar  -- ^ in-scope GHC tyvar unique -> Eidos TyVar.
      , ctxVocab  :: [(Text, E.Id)]     -- ^ base-vocabulary name -> synthesized defn Id.
      }

bindLocal :: Var -> E.Id -> Ctx -> Ctx
bindLocal v x ctx = ctx { ctxLocals = IM.insert (uKey v) x $ ctxLocals ctx }

bindJoin :: Var -> E.JoinId -> Ctx -> Ctx
bindJoin v j ctx = ctx { ctxJoins = IM.insert (uKey v) j $ ctxJoins ctx }

bindTyVar :: Var -> E.TyVar -> Ctx -> Ctx
bindTyVar v tv ctx = ctx { ctxTyVars = IM.insert (uKey v) tv $ ctxTyVars ctx }

toEidos :: MonadError AstError m => Config -> [ModGuts] -> m E.Program
toEidos conf gutss = evalStateT go 0
      where go :: MonadError AstError m => BM m E.Program
            go = do
                  let binds :: [(Var, CoreExpr, ModuleName)]
                      binds = [ (b, rhs, moduleName $ mg_module g)
                              | g <- gutss, bnd <- mg_binds g, (b, rhs) <- flattenBind bnd ]

                      bindMap :: IM.IntMap (Var, CoreExpr, ModuleName)
                      bindMap = IM.fromList [ (uKey b, t) | t@(b, _, _) <- binds ]

                      (startMod, startOcc) = splitStart $ conf^.start

                      isStartV :: (Var, CoreExpr, ModuleName) -> Bool
                      isStartV (b, _, mn) = moduleNameString mn == startMod && getOccString b == startOcc

                  startV <- case filter isStartV binds of
                        ((b, _, _) : _) -> pure b
                        _               -> failAt noAnn $ "ghc-frontend: no definition for the start symbol (" <> conf^.start <> ")."

                  -- rwPrim* binds are roots besides the start symbol (their
                  -- signatures are the builtins' type assumptions).
                  let roots = startV : [ b | (b, _, _) <- binds, isPrimVar b ]
                      reach = reachable bindMap roots
                      keep  = [ t | t@(b, _, _) <- binds
                                  , uKey b `IS.member` reach
                                  , not (erasedEv $ varType b) ]

                  -- Pre-mint every kept top-level Id (so use sites of
                  -- module-internal ids resolve to the same Id), and the
                  -- vocabulary definitions.
                  tops   <- IM.fromList <$> mapM (\ (b, _, mn) -> (uKey b, ) <$> topId b mn) keep
                  vocab  <- vocabDefns

                  let ctx = Ctx { ctxTops   = tops
                                , ctxLocals = mempty
                                , ctxJoins  = mempty
                                , ctxTyVars = mempty
                                , ctxVocab  = [ (E.idOcc $ E.defnId d, E.defnId d) | d <- vocab ]
                                }

                  defs  <- mapM (bridgeDefn ctx) keep
                  datas <- concat <$> mapM harvestDatas gutss
                  top   <- case IM.lookup (uKey startV) tops of
                        Just x  -> pure x
                        Nothing -> failAt noAnn "ghc-frontend: start symbol not bridged (rwc bug)."
                  pure $ addPrims E.Program
                        { E.progDatas = datas
                        , E.progDefns = defs <> vocab
                        , E.progProcs = []
                        , E.progTop   = top
                        }

            topId :: MonadError AstError m => Var -> ModuleName -> BM m E.Id
            topId b mn = do
                  let an         = varAnnote b
                      (tvs, rho) = splitForAllTyCoVars $ expandTypeSynonyms $ varType b
                  (tvs', tvm) <- mintTyVars an $ filter isTyVar tvs
                  t           <- bridgeTy tvm an rho
                  freshId (qualName mn b) $ E.Sig tvs' t

-- | Mint Eidos type variables for a quantifier list, returning them plus
--   the GHC-unique-keyed map for resolving occurrences.
mintTyVars :: MonadError AstError m => Annote -> [Var] -> BM m ([E.TyVar], IM.IntMap E.TyVar)
mintTyVars _ vs = do
      tvs <- mapM (\ v -> do
                  u <- freshU
                  pure (v, E.TyVar (pack $ getOccString v) u (bridgeKind $ varType v)))
            vs
      pure (map snd tvs, IM.fromList [ (uKey v, tv) | (v, tv) <- tvs ])

flattenBind :: CoreBind -> [(Var, CoreExpr)]
flattenBind = \ case
      NonRec b rhs -> [(b, rhs)]
      Rec bs       -> bs

-- | Transitive closure of top-level binds reachable from the roots, with
--   the same erasures the translation performs. (Identical to the Crust
--   bridge's walk; it dies with that bridge.)
reachable :: IM.IntMap (Var, CoreExpr, ModuleName) -> [Var] -> IS.IntSet
reachable bindMap = go mempty
      where go :: IS.IntSet -> [Var] -> IS.IntSet
            go seen []       = seen
            go seen (v : vs)
                  | k `IS.member` seen = go seen vs
                  | otherwise          = case IM.lookup k bindMap of
                        Just (_, rhs, _) | not (isPrimVar v) -> go (IS.insert k seen) (refs rhs <> vs)
                        _                                    -> go (IS.insert k seen) vs
                  where k = uKey v

            refs :: CoreExpr -> [Var]
            refs = \ case
                  Var v               -> [v]
                  Lit _               -> []
                  App f a
                        | erasedArg a -> refs f
                        | otherwise   -> refs f <> refs a
                  Lam _ e             -> refs e
                  Let (NonRec x r) e
                        | erasedEv (varType x) -> refs e
                        | otherwise   -> refs r <> refs e
                  Let (Rec bs) e      -> concatMap (refs . snd) bs <> refs e
                  Case s _ _ alts     -> refs s <> concatMap (\ (Alt _ _ e') -> refs e') alts
                  Cast e' _           -> refs e'
                  Tick _ e'           -> refs e'
                  Type _              -> []
                  Coercion _          -> []

---
--- Definitions.
---

bridgeDefn :: MonadError AstError m => Ctx -> (Var, CoreExpr, ModuleName) -> BM m E.Defn
bridgeDefn ctx (b, rhs, mn) = do
      x <- maybe (failAt (varAnnote b) "ghc-frontend: unbridged top-level id (rwc bug).") pure
            $ IM.lookup (uKey b) $ ctxTops ctx
      let an              = varAnnote b
          E.Sig sigTvs _  = E.idSig x
          ghcTvs          = filter isTyVar $ fst $ splitForAllTyCoVars $ expandTypeSynonyms $ varType b
      -- The defn's leading type/dictionary lambdas correspond to its
      -- signature: map the type binders (positionally) onto the minted
      -- signature variables and skip the dictionary binders. An eta-reduced
      -- wrapper (e.g. @put = rwPrimPut@) has fewer type lambdas than its
      -- signature quantifies; the leftover signature variables are
      -- type-applied to the body (type-level eta expansion), using the
      -- signature's own binders.
      (ctx', body, nPeeled) <- peelHead an ctx sigTvs rhs
      let leftoverGhc = drop nPeeled ghcTvs
          leftoverTvs = drop nPeeled sigTvs
          body''      = foldl (\ acc gv -> App acc $ Type $ mkTyVarTy gv) body leftoverGhc
          ctx''       = foldr (uncurry bindTyVar) ctx' $ zip leftoverGhc leftoverTvs
      body' <- if isPrimVar b
            then pure $ mkErrorE an (E.sigTy $ E.idSig x) $ "Prim: " <> qualName mn b
            else bridgeExp ctx'' an body''
      pure E.Defn
            { E.defnAnnote = an
            , E.defnId     = x
            , E.defnParams = []
            , E.defnBody   = body'
            , E.defnAttr   = if isDFunId b || userPred (snd $ splitForAllTyCoVars $ expandTypeSynonyms $ varType b)
                             then Just E.Inline
                             else inlAttr $ inlinePragmaSpec $ idInlinePragma b
            , E.defnOrigin = Nothing
            }
      where peelHead :: MonadError AstError m => Annote -> Ctx -> [E.TyVar] -> CoreExpr -> BM m (Ctx, CoreExpr, Int)
            peelHead an c tvs = \ case
                  Lam v e | isTyVar v -> case tvs of
                        (tv : tvs') -> do
                              (c', e', n) <- peelHead an (bindTyVar v tv c) tvs' e
                              pure (c', e', n + 1)
                        []          -> failAt an "ghc-frontend: unsupported nested type abstraction."
                  Lam v e | erasedEv (varType v) -> peelHead an c tvs e
                  e                   -> pure (c, e, 0)

            inlAttr :: InlineSpec -> Maybe E.DefnAttr
            inlAttr = \ case
                  Inline {}    -> Just E.Inline
                  Inlinable {} -> Just E.Inline
                  NoInline {}  -> Just E.NoInline
                  Opaque {}    -> Just E.NoInline
                  _            -> Nothing

---
--- Types (fail-fast: no Maybe slots to degrade into).
---

bridgeKind :: Type -> E.Kind
bridgeKind = go . expandTypeSynonyms -- N.B. Nat is a synonym for Natural.
      where go = \ case
                  FunTy _ _ a r                      -> E.KFun (go a) (go r)
                  TyConApp tc _ | tc == naturalTyCon -> E.KNat
                  _                                  -> E.KStar

-- | Core Type -> Eidos Ty. Callers are expected to expandTypeSynonyms
--   first. Constraint (invisible) arrows are dropped; user-class
--   constraint arrows become value arrows (the dictionary is data).
bridgeTy :: MonadError AstError m => IM.IntMap E.TyVar -> Annote -> Type -> m E.Ty
bridgeTy tvm an = go
      where go :: MonadError AstError m => Type -> m E.Ty
            go = \ case
                  TyVarTy v            -> case IM.lookup (uKey v) tvm of
                        Just tv -> pure $ E.TyVarT an tv
                        Nothing -> failAt an $ "ghc-frontend: out-of-scope type variable: " <> pack (getOccString v)
                  AppTy a b            -> E.TyApp an <$> go a <*> go b
                  TyConApp tc args     -> bridgeTyConApp tvm an tc args
                  ForAllTy _ _         -> failAt an "ghc-frontend: unsupported higher-rank type."
                  FunTy _ _ a r
                        | userPred a    -> E.Arrow an <$> go a <*> go r
                        | isEvVarType a -> go r
                        | otherwise     -> E.Arrow an <$> go a <*> go r
                  LitTy (NumTyLit n)   -> pure $ E.TyNat an $ fromInteger n
                  LitTy l              -> failAt an $ "ghc-frontend: unsupported type-level literal: " <> pack (showSDocUnsafe $ ppr l)
                  CastTy t _           -> go t
                  CoercionTy _         -> failAt an "ghc-frontend: unsupported coercion in type position."

bridgeTyConApp :: MonadError AstError m => IM.IntMap E.TyVar -> Annote -> TyCon -> [Type] -> m E.Ty
bridgeTyConApp tvm an tc args
      -- [Char] is Eidos's String.
      | tc == listTyCon, [TyConApp c []] <- args, c == charTyCon
                              = pure $ E.TyCon an "String"
      | tc == listTyCon       = appN "[_]"
      | tc == boolTyCon       = appN "Bool"
      | tc == unboxedUnitTyCon = appN' [] "()"
      | tc == unitTyCon       = appN "()"
      | tc == integerTyCon    = appN "Integer"
      | tc == naturalTyCon    = appN "Integer" -- Natural at the type level: treat as Integer.
      | isBoxedTupleTyCon tc  = appN $ tupleName $ tyConArity tc
      | tc == typeNatAddTyCon = appN "+"
      | tc == typeNatMulTyCon = appN "*"
      | tc == typeNatSubTyCon = appN "-"
      -- The function tycon in prefix position: [mult, rep1, rep2, a, b].
      | getOccString (tyConName tc) == "FUN"
                              = case reverse args of
            (b : a : _) -> E.Arrow an <$> bridgeTy tvm an a <*> bridgeTy tvm an b
            _           -> failAt an "ghc-frontend: unsaturated function type constructor."
      | otherwise             = case lookup (tyConKey tc) tyConTable of
            Just (n, dropN) -> appN' (drop dropN args) n
            Nothing
                  | Just mn <- tyConModule tc, isPrimModule mn
                              -> appN $ pack $ getOccString $ tyConName tc
                  | Just mn <- tyConModule tc, homeishMod (Just mn)
                              -> appN $ pack (moduleNameString mn) <> "." <> pack (getOccString $ tyConName tc)
                  | otherwise -> failAt an $ "ghc-frontend: type not in the ReWire vocabulary: "
                                    <> pack (maybe "?" moduleNameString (tyConModule tc)) <> "." <> pack (getOccString $ tyConName tc)
      where appN :: MonadError AstError m => Text -> m E.Ty
            appN = appN' args

            appN' :: MonadError AstError m => [Type] -> Text -> m E.Ty
            appN' as n = foldl (E.TyApp an) (E.TyCon an n) <$> mapM (bridgeTy tvm an) as

---
--- Expressions.
---

mkErrorE :: Annote -> E.Ty -> Text -> E.Exp
mkErrorE an t m = E.App an (E.Prim an (E.Arrow an (E.TyCon an "String") t) B.Error) $ E.EArg $ E.LitStr an m

mkApp :: Annote -> E.Exp -> [E.Arg] -> E.Exp
mkApp an = foldl $ E.App an

eargs :: [E.Exp] -> [E.Arg]
eargs = map E.EArg

-- | The bridged, instantiated type of a Core expression, resolving type
--   variables in the enclosing definition's scope.
instTy :: MonadError AstError m => Ctx -> Annote -> CoreExpr -> m E.Ty
instTy ctx an e = bridgeTy (ctxTyVars ctx) an $ expandTypeSynonyms $ exprType e

bridgeExp :: MonadError AstError m => Ctx -> Annote -> CoreExpr -> BM m E.Exp
bridgeExp ctx an e = case e of
      App {}             -> bridgeApp ctx an e
      Var {}             -> bridgeApp ctx an e

      Lit (LitString bs) -> pure $ E.LitStr an $ TE.decodeUtf8 bs
      Lit l              -> failAt an $ "ghc-frontend: unsupported literal (" <> pack (showSDocUnsafe $ ppr l) <> ")."

      Lam v body
            | isTyVar v            -> failAt an "ghc-frontend: unsupported nested type abstraction."
            | erasedEv (varType v) -> bridgeExp ctx an body
            | otherwise            -> do
                  t     <- bridgeTy (ctxTyVars ctx) an $ expandTypeSynonyms $ varType v
                  x     <- freshId (pack $ getOccString v) $ E.monoSig t
                  body' <- bridgeExp (bindLocal v x ctx) an body
                  pure $ E.Lam an x body'

      -- Lets transliterate (evidence binds erased); a join-tagged binder
      -- becomes a Join bind whose call sites are Jumps.
      Let (NonRec v r) body
            | erasedEv (varType v)      -> bridgeExp ctx an body
            | JoinPoint n <- idJoinPointHood v -> bridgeJoin ctx an v n r body
            | otherwise                 -> do
                  t     <- bridgeTy (ctxTyVars ctx) an $ expandTypeSynonyms $ varType v
                  x     <- freshId (pack $ getOccString v) $ E.monoSig t
                  r'    <- bridgeExp ctx an r
                  body' <- bridgeExp (bindLocal v x ctx) an body
                  pure $ E.Let an (E.NonRec x r') body'

      Let (Rec bs) body
            | any (\ (v, _) -> case idJoinPointHood v of { JoinPoint _ -> True; NotJoinPoint -> False }) bs
                        -> failAt an "ghc-frontend: unsupported recursive join point."
            | otherwise -> do
                  xs <- mapM (\ (v, _) -> do
                              t <- bridgeTy (ctxTyVars ctx) an $ expandTypeSynonyms $ varType v
                              freshId (pack $ getOccString v) $ E.monoSig t)
                        bs
                  let ctx' = foldr (\ ((v, _), x) -> bindLocal v x) ctx $ zip bs xs
                  rs <- mapM (bridgeExp ctx' an . snd) bs
                  body' <- bridgeExp ctx' an body
                  pure $ E.Let an (E.Rec $ zip xs rs) body'

      Case scrut b ty alts -> bridgeCase ctx an scrut b ty alts

      Cast e' _            -> bridgeExp ctx an e'
      Tick _ e'            -> bridgeExp ctx an e'
      Type _               -> failAt an "ghc-frontend: unexpected type in expression position."
      Coercion _           -> failAt an "ghc-frontend: unexpected coercion in expression position."

-- | A join point: the binder's arity-many leading lambdas become the
--   join's parameters; type/evidence binders among them are unsupported
--   (join points over erased binders do not survive erasure cleanly).
bridgeJoin :: MonadError AstError m => Ctx -> Annote -> Var -> Int -> CoreExpr -> CoreExpr -> BM m E.Exp
bridgeJoin ctx an v n rhs body = do
      (vs, jbody) <- peel n rhs
      unless (all (\ pv -> not (isTyVar pv) && not (erasedEv $ varType pv)) vs)
            $ failAt an "ghc-frontend: unsupported type- or evidence-binding join point."
      params <- mapM (\ pv -> do
                  t <- bridgeTy (ctxTyVars ctx) an $ expandTypeSynonyms $ varType pv
                  freshId (pack $ getOccString pv) $ E.monoSig t)
            vs
      resT   <- bridgeTy (ctxTyVars ctx) an $ expandTypeSynonyms $ exprType jbody
      let jt = foldr (E.Arrow an . E.sigTy . E.idSig) resT params
      jx     <- freshId (pack $ getOccString v) $ E.monoSig jt
      let j    = E.JoinId jx $ length params
          ctxJ = bindJoin v j ctx
          ctxP = foldr (\ (pv, x) -> bindLocal pv x) ctxJ $ zip vs params
      jbody' <- bridgeExp ctxP an jbody
      body'  <- bridgeExp ctxJ an body
      pure $ E.Let an (E.Join j params jbody') body'
      where peel :: MonadError AstError m => Int -> CoreExpr -> BM m ([Var], CoreExpr)
            peel 0 e' = pure ([], e')
            peel k (Lam pv e') = do
                  (pvs, e'') <- peel (k - 1) e'
                  pure (pv : pvs, e'')
            peel _ _ = failAt an "ghc-frontend: join point of unexpected arity."

---
--- Application spines.
---

bridgeApp :: MonadError AstError m => Ctx -> Annote -> CoreExpr -> BM m E.Exp
bridgeApp ctx an e = do
      let (f, args) = collectArgs e
          vargs     = filter (not . erasedArg) args
      case f of
            Var v -> bridgeVarApp ctx an v args vargs
            _     -> do
                  f'  <- bridgeExp ctx an f
                  as' <- mapM (bridgeExp ctx an) vargs
                  pure $ mkApp an f' $ eargs as'

-- | The bridged type arguments of a spine (the leading Type args), for
--   heads that keep them ('TArg's on Var heads).
spineTArgs :: MonadError AstError m => Ctx -> Annote -> [CoreExpr] -> m [E.Ty]
spineTArgs ctx an args = mapM (bridgeTy (ctxTyVars ctx) an . expandTypeSynonyms)
      [ t | Type t <- takeWhile isTypeArg args ]
      where isTypeArg :: CoreExpr -> Bool
            isTypeArg = \ case
                  Type _ -> True
                  _      -> False

-- | The Var-headed application dispatch: joins, locals, builtins,
--   constructors, class ops, home ids, base vocabulary.
bridgeVarApp :: MonadError AstError m => Ctx -> Annote -> Var -> [CoreExpr] -> [CoreExpr] -> BM m E.Exp
bridgeVarApp ctx an v args vargs
      -- The void primitive (a Void#-typed unit value): the boxed unit.
      | v == voidPrimId = pure $ E.Con an (E.TyCon an "()") "()"

      -- Join point call site: a saturated Jump.
      | Just j <- IM.lookup (uKey v) $ ctxJoins ctx = do
            unless (length vargs == E.jpArity j)
                  $ failAt an "ghc-frontend: unsaturated jump to a join point."
            E.Jump an j <$> mapM (bridgeExp ctx an) vargs

      -- Local binder (lambda, case, let).
      | Just x <- IM.lookup (uKey v) $ ctxLocals ctx
            = mkApp an (E.Var an x) . eargs <$> mapM (bridgeExp ctx an) vargs

      -- rwPrim* -> Prim, by occurrence name, at its instantiated type.
      | isPrimVar v = case lookup (pack $ getOccString v) B.builtins of
            Just b  -> do
                  t <- headTy
                  mkApp an (E.Prim an t b) . eargs <$> mapM (bridgeExp ctx an) vargs
            Nothing -> failAt an $ "ghc-frontend: unknown primitive: " <> pack (getOccString v)

      -- Data constructor (worker or wrapper).
      | Just dc <- isDataConId_maybe v = bridgeConApp ctx an v dc args vargs

      -- Class method selector.
      | Just _ <- isClassOpId_maybe v = bridgeClassOp ctx an v args vargs

      -- Home top-level id: reference with its type arguments.
      | Just x <- IM.lookup (uKey v) $ ctxTops ctx = do
            targs <- lift $ spineTArgs ctx an args
            as'   <- mapM (bridgeExp ctx an) vargs
            pure $ mkApp an (E.Var an x) $ map E.TArg targs <> eargs as'

      -- Base vocabulary.
      | otherwise = bridgeBaseVocab ctx an v args vargs

      where headTy :: MonadError AstError m => BM m E.Ty
            headTy = lift $ instTy ctx an $ foldl App (Var v) $ takeWhile erasedArg args

-- | Data constructor applications, with the literal folds.
bridgeConApp :: MonadError AstError m => Ctx -> Annote -> Var -> DataCon -> [CoreExpr] -> [CoreExpr] -> BM m E.Exp
bridgeConApp ctx an v dc args vargs
      -- The unboxed unit (nullary join points take a (# #) argument): treat
      -- as the boxed unit, matching the type-level mapping.
      | dc == unboxedUnitDataCon = pure $ E.Con an (E.TyCon an "()") "()"

      -- Boxed integer literals: IS 5# / IP bigNat# / IN bigNat#.
      | dc == integerISDataCon, [Lit (LitNumber _ n)] <- vargs = litInt n
      | dc == integerIPDataCon, [Lit (LitNumber _ n)] <- vargs = litInt n
      | dc == integerINDataCon, [Lit (LitNumber _ n)] <- vargs = litInt $ negate n
      | dc `elem` [integerISDataCon, integerIPDataCon, integerINDataCon]
            = failAt an "ghc-frontend: Integer constructor applied to a non-literal."

      -- List literals: cons chains and nil.
      | dc == nilDataCon || dc == consDataCon = do
            elems <- listElems $ foldl App (Var v) args
            ty    <- lift $ instTy ctx an $ foldl App (Var v) args
            case (elems, ty) of
                  ([], E.TyCon _ "String") -> pure $ E.LitStr an ""
                  _                        -> E.LitList an ty <$> mapM (bridgeExp ctx an) elems

      | otherwise = do
            t <- lift $ instTy ctx an $ foldl App (Var v) $ takeWhile erasedArg args
            mkApp an (E.Con an t $ conName dc) . eargs <$> mapM (bridgeExp ctx an) vargs

      where litInt :: MonadError AstError m => Integer -> BM m E.Exp
            litInt n = pure $ E.LitInt an (E.TyCon an "Integer") n

            listElems :: MonadError AstError m => CoreExpr -> BM m [CoreExpr]
            listElems e = case collectArgs e of
                  (Var v', as)
                        | Just dc' <- isDataConId_maybe v', dc' == nilDataCon  -> pure []
                        | Just dc' <- isDataConId_maybe v', dc' == consDataCon
                        , [hd, tl] <- filter (not . erasedArg) as              -> (hd :) <$> listElems tl
                  _ -> failAt an "ghc-frontend: unsupported non-literal list (lists must be literal cons chains)."

-- | Class method dispatch: user-class methods project the field out of the
--   dictionary (ordinary data, via a single-alternative case); >>=, >>,
--   return, pure at ReacT/StateT/Identity or at an unresolved monad type
--   become Bind/Return primitives (dictionary discarded; a concrete monad
--   off the reactive stack is rejected); == at Integer becomes the Eq
--   primitive.
bridgeClassOp :: MonadError AstError m => Ctx -> Annote -> Var -> [CoreExpr] -> [CoreExpr] -> BM m E.Exp
bridgeClassOp ctx an v args vargs
      | Just cls <- isClassOpId_maybe v
      , homeishMod (tyConModule $ classTyCon cls) = case vargs of
            (d : rest) -> do
                  d' <- bridgeExp ctx an d
                  if isNewTyCon (classTyCon cls)
                        -- A newtype dictionary IS its method.
                        then mkApp an d' . eargs <$> mapM (bridgeExp ctx an) rest
                        else do
                              let dcls = tyConSingleDataCon $ classTyCon cls
                                  sels = classAllSelIds cls
                              i      <- maybe (failAt an $ "ghc-frontend: unknown class method: " <> occ) pure
                                          $ elemIndex v sels
                              -- Field types: the dictionary constructor's
                              -- theta (superclasses) and method fields,
                              -- instantiated at the class's type arguments.
                              let clsArgs = [ t | Type t <- takeWhile isTypeArg args ]
                                  fieldTs = substTyWith (dataConUnivTyVars dcls) clsArgs
                                            <$> (dataConTheta dcls <> map scaledThing (dataConOrigArgTys dcls))
                              fieldTs' <- lift $ mapM (bridgeTy (ctxTyVars ctx) an . expandTypeSynonyms) fieldTs
                              flds     <- zipWithM (\ k t -> freshId ("dictField" <> pack (show (k :: Int))) $ E.monoSig t)
                                                [0 ..] fieldTs'
                              fldTy    <- maybe (failAt an "ghc-frontend: class method index out of range (rwc bug).") pure
                                          $ lookup i $ zip [0 ..] fieldTs'
                              dTy      <- lift $ instTy ctx an d
                              cb       <- freshId "dict" $ E.monoSig dTy
                              let proj = E.Case an fldTy d' cb
                                          [ E.Alt an (E.DataAlt $ conName dcls) flds
                                              $ E.Var an $ flds !! i ]
                              mkApp an proj . eargs <$> mapM (bridgeExp ctx an) rest
            _          -> failAt an $ "ghc-frontend: unapplied class method: " <> occ
      | occ `elem` ([">>=", ">>", "return", "pure"] :: [Text]) = case tyArgHead of
            Just tcn | tcn `notElem` (["ReacT", "StateT", "Identity"] :: [Text])
                     -> failAt an $ "ghc-frontend: monadic operator at unsupported monad: " <> tcn
            -- A reactive-stack monad or one not yet resolved (e.g., a type
            -- variable in the body of a monad-polymorphic helper): emit the
            -- polymorphic primitive; the specializer resolves it.
            _        -> case occ of
                  ">>=" -> do
                        t <- opTy
                        mkApp an (E.Prim an t B.Bind) . eargs <$> mapM (bridgeExp ctx an) vargs
                  ">>"  -> case vargs of
                        [e1, e2] -> do
                              -- e1 >> e2 becomes bind e1 (\ _ -> e2): the
                              -- primitive's type is built from (>>)'s
                              -- instantiated m a -> m b -> m b.
                              t <- opTy
                              (ma, mb) <- case flattenArrow t of
                                    ([ta, tb], _) -> pure (ta, tb)
                                    _             -> failAt an "ghc-frontend: unexpected type for (>>)."
                              a <- case flattenTyApp ma of
                                    (_, as) | not (null as) -> pure $ last as
                                    _                       -> failAt an "ghc-frontend: unexpected monadic type for (>>)."
                              let bindT = E.Arrow an ma $ E.Arrow an (E.Arrow an a mb) mb
                              e1' <- bridgeExp ctx an e1
                              e2' <- bridgeExp ctx an e2
                              k   <- freshId "_unused" $ E.monoSig a
                              pure $ mkApp an (E.Prim an bindT B.Bind) $ eargs [e1', E.Lam an k e2']
                        _        -> failAt an "ghc-frontend: unsaturated (>>)."
                  _     -> do -- return/pure
                        t <- opTy
                        mkApp an (E.Prim an t B.Return) . eargs <$> mapM (bridgeExp ctx an) vargs
      | occ == "==", Just "Integer" <- tyArgHead = do
            t <- opTy
            mkApp an (E.Prim an t B.Eq) . eargs <$> mapM (bridgeExp ctx an) vargs
      | otherwise = failAt an $ "ghc-frontend: unsupported use of a type class method: " <> occ
      where occ :: Text
            occ = pack $ getOccString v

            -- The op's instantiated type (constraint arrows dropped by the
            -- type bridge).
            opTy :: MonadError AstError m => BM m E.Ty
            opTy = lift $ instTy ctx an $ foldl App (Var v) $ takeWhile erasedArg args

            isTypeArg :: CoreExpr -> Bool
            isTypeArg = \ case
                  Type _ -> True
                  _      -> False

            -- The head tycon of the first type argument.
            tyArgHead :: Maybe Text
            tyArgHead = case [ t | Type t <- args ] of
                  (t : _) -> case expandTypeSynonyms t of
                        TyConApp tc _ | tc == integerTyCon -> Just "Integer"
                                      | otherwise          -> Just $ headName tc
                        _             -> Nothing
                  _       -> Nothing

            headName :: TyCon -> Text
            headName tc = case lookup (tyConKey tc) tyConTable of
                  Just (n, _) -> n
                  Nothing     -> pack $ getOccString $ tyConName tc

-- | The base vocabulary: error functions map to the Error primitive;
--   string unpacking folds to LitStr; the small closed set of Prelude
--   combinators maps to the synthesized vocabulary definitions.
bridgeBaseVocab :: MonadError AstError m => Ctx -> Annote -> Var -> [CoreExpr] -> [CoreExpr] -> BM m E.Exp
bridgeBaseVocab ctx an v args vargs
      | occ `elem` (["unpackCString#", "unpackCStringUtf8#"] :: [Text]) = case vargs of
            [Lit (LitString bs)] -> pure $ E.LitStr an $ TE.decodeUtf8 bs
            _                    -> failAt an "ghc-frontend: unpackCString# applied to a non-literal."

      | occ `elem` (["error", "errorWithoutStackTrace", "undefined"] :: [Text]) = do
            t   <- lift $ instTy ctx an $ foldl App (Var v) args
            as' <- mapM (bridgeExp ctx an) vargs
            case (occ, as') of
                  ("undefined", _) -> pure $ mkErrorE an t "Prelude.undefined"
                  (_, [s])         -> pure $ E.App an (E.Prim an (E.Arrow an (E.TyCon an "String") t) B.Error) $ E.EArg s
                  _                -> failAt an "ghc-frontend: unsupported use of error."

      | occ `elem` ([ "patError", "absentError", "nonExhaustiveGuardsError"
                    , "noMethodBindingError", "recSelError", "typeError" ] :: [Text]) = do
            t <- lift $ instTy ctx an $ foldl App (Var v) args
            pure $ mkErrorE an t "Pattern match failure: non-exhaustive patterns in case"

      | Just n <- lookup occ =<< flip lookup vocabTable =<< modName
      , Just x <- lookup n $ ctxVocab ctx = do
            -- Drop leading runtime-rep/levity type arguments BEFORE bridging
            -- (they are not in the Eidos vocabulary): the vocabulary
            -- signatures quantify only the value type variables (the last k
            -- type arguments).
            let rawTs       = [ t | Type t <- takeWhile isTypeArgV args ]
                E.Sig tvs _ = E.idSig x
                k           = length tvs
            unless (length rawTs >= k)
                  $ failAt an $ "ghc-frontend: under-instantiated vocabulary reference: " <> occ
            targs <- lift $ mapM (bridgeTy (ctxTyVars ctx) an . expandTypeSynonyms) $ drop (length rawTs - k) rawTs
            as' <- mapM (bridgeExp ctx an) vargs
            pure $ mkApp an (E.Var an x) $ map E.TArg targs <> eargs as'

      | otherwise = failAt an $ "ghc-frontend: not in the ReWire vocabulary: "
            <> maybe "?" id modName <> "." <> occ

      where occ :: Text
            occ = pack $ getOccString v

            isTypeArgV :: CoreExpr -> Bool
            isTypeArgV = \ case
                  Type _ -> True
                  _      -> False

            modName :: Maybe Text
            modName = pack . moduleNameString . moduleName <$> nameModule_maybe (varName v)

---
--- Case expressions (n-ary, transliterated; Core orders DEFAULT first).
---

bridgeCase :: MonadError AstError m => Ctx -> Annote -> CoreExpr -> Var -> Type -> [Alt Var] -> BM m E.Exp
bridgeCase ctx an scrut b ty alts = do
      scrut'  <- bridgeExp ctx an scrut
      resTy   <- lift $ bridgeTy (ctxTyVars ctx) an $ expandTypeSynonyms ty
      scrutTy <- lift $ instTy ctx an scrut
      case alts of
            -- Zero-alt case: the scrutinee is bottom (e.g. patError).
            [] -> pure $ mkErrorE an resTy "Pattern match failure: non-exhaustive patterns in case"
            _  -> do
                  cb    <- freshId (pack $ getOccString b) $ E.monoSig scrutTy
                  let ctx' = bindLocal b cb ctx
                  alts' <- mapM (bridgeAlt ctx') alts
                  pure $ E.Case an resTy scrut' cb alts'
      where bridgeAlt :: MonadError AstError m => Ctx -> Alt Var -> BM m E.Alt
            bridgeAlt ctx' (Alt con vs rhs) = case con of
                  DEFAULT    -> E.Alt an E.DefaultAlt [] <$> bridgeExp ctx' an rhs
                  LitAlt (LitNumber _ n) -> E.Alt an (E.LitAlt n) [] <$> bridgeExp ctx' an rhs
                  LitAlt l   -> failAt an $ "ghc-frontend: unsupported literal pattern (" <> pack (showSDocUnsafe $ ppr l) <> ")."
                  DataAlt dc -> do
                        let flds = filter (\ pv -> not (isTyVar pv) && not (erasedEv $ varType pv)) vs
                            -- Field types: instantiate at the scrutinee's
                            -- type arguments.
                            scrutArgs = case expandTypeSynonyms $ exprType scrut of
                                  TyConApp _ as -> as
                                  _             -> []
                            fieldTs   = substTyWith (dataConUnivTyVars dc) scrutArgs
                                        <$> (dataConTheta dc <> map scaledThing (dataConOrigArgTys dc))
                        unless (length fieldTs == length flds)
                              $ failAt an $ "ghc-frontend: constructor field arity mismatch in case alternative: " <> conName dc
                              <> " (" <> pack (show (length flds)) <> " binders, " <> pack (show (length fieldTs)) <> " fields)."
                        fieldTs' <- lift $ mapM (bridgeTy (ctxTyVars ctx') an . expandTypeSynonyms) fieldTs
                        xs       <- zipWithM (\ pv t -> freshId (pack $ getOccString pv) $ E.monoSig t) flds fieldTs'
                        let ctx'' = foldr (\ (pv, x) -> bindLocal pv x) ctx' $ zip flds xs
                        E.Alt an (E.DataAlt $ conName dc) xs <$> bridgeExp ctx'' an rhs

---
--- Datatypes.
---

-- | User DataDefns from a module's tycons; RWC.Primitives' tycons are
--   supplied by the primitive basis and skipped here.
harvestDatas :: MonadError AstError m => ModGuts -> BM m [E.DataDefn]
harvestDatas g
      | isPrimModule mn = pure []
      | otherwise       = mapM harvest $ filter wanted $ mg_tcs g
      where mn = moduleName $ mg_module g

            wanted :: TyCon -> Bool
            wanted tc = isAlgTyCon tc && not (isTypeSynonymTyCon tc) && not (isBoxedTupleTyCon tc)
                  && not (isClassTyCon tc && isNewTyCon tc)

            harvest :: MonadError AstError m => TyCon -> BM m E.DataDefn
            harvest tc = do
                  -- The datatype's parameters are minted once and shared by
                  -- every constructor signature (the lint requires the same
                  -- quantifiers across a datatype's constructors).
                  (tvs, _) <- mintTyVars an $ tyConTyVars tc
                  E.DataDefn an name (bridgeKind $ tyConKind tc) <$> mapM (dataCon tvs) (tyConDataCons tc)
                  where an   = spanAnnote $ nameSrcSpan $ tyConName tc
                        name = pack (moduleNameString mn) <> "." <> pack (getOccString $ tyConName tc)

                        dataCon :: MonadError AstError m => [E.TyVar] -> DataCon -> BM m E.DataCon
                        dataCon tvs dc | not (null $ dataConExTyCoVars dc) = failAt an "ghc-frontend: unsupported existential data constructor."
                                       | otherwise = do
                              -- A vanilla constructor's universals coincide
                              -- with the datatype's parameters; map them
                              -- positionally onto the shared variables.
                              let univs = dataConUnivTyVars dc
                              unless (length univs == length tvs)
                                    $ failAt an $ "ghc-frontend: unsupported non-vanilla data constructor: " <> conName dc
                              let tvm = IM.fromList $ zip (map uKey univs) tvs
                              argTys <- lift $ mapM (bridgeTy tvm an . expandTypeSynonyms)
                                            $ dataConTheta dc <> map scaledThing (dataConOrigArgTys dc)
                              resTy  <- lift $ bridgeTy tvm an $ TyConApp tc $ mkTyVarTys univs
                              pure $ E.DataCon an (conName dc) $ E.Sig tvs $ foldr (E.Arrow an) resTy argTys

---
--- The base vocabulary's synthesized definitions ($, ., id, not, &&, ||,
--- fst, snd), mirroring "ReWire.GHC.Vocab" on the Eidos side.
---

vocabDefns :: MonadError AstError m => BM m [E.Defn]
vocabDefns = sequence
      [ do -- ($) :: forall a b. (a -> b) -> a -> b
            (a, b, _) <- tvs3
            let ta = tv a; tb = tv b
            f <- freshId "f" $ E.monoSig $ E.Arrow va ta tb
            x <- freshId "x" $ E.monoSig ta
            mkVocab "GHC.Internal.Base.$" [a, b] (E.Arrow va (E.Arrow va ta tb) $ E.Arrow va ta tb)
                  $ E.Lam va f $ E.Lam va x $ E.App va (E.Var va f) $ E.EArg $ E.Var va x
      , do -- (.) :: forall a b c. (b -> c) -> (a -> b) -> a -> c
            (a, b, c) <- tvs3
            let ta = tv a; tb = tv b; tc = tv c
            f <- freshId "f" $ E.monoSig $ E.Arrow va tb tc
            g <- freshId "g" $ E.monoSig $ E.Arrow va ta tb
            x <- freshId "x" $ E.monoSig ta
            -- N.B. GHC's (.) quantifies forall b c a. — the vocabulary
            -- signature must match that instantiation order.
            mkVocab "GHC.Internal.Base.." [b, c, a] (E.Arrow va (E.Arrow va tb tc) $ E.Arrow va (E.Arrow va ta tb) $ E.Arrow va ta tc)
                  $ E.Lam va f $ E.Lam va g $ E.Lam va x
                  $ E.App va (E.Var va f) $ E.EArg $ E.App va (E.Var va g) $ E.EArg $ E.Var va x
      , do -- id :: forall a. a -> a
            (a, _, _) <- tvs3
            let ta = tv a
            x <- freshId "x" $ E.monoSig ta
            mkVocab "GHC.Internal.Base.id" [a] (E.Arrow va ta ta)
                  $ E.Lam va x $ E.Var va x
      , do -- not :: Bool -> Bool
            b <- freshId "b" $ E.monoSig boolT
            cb <- freshId "s" $ E.monoSig boolT
            mkVocab "GHC.Classes.not" [] (E.Arrow va boolT boolT)
                  $ E.Lam va b $ E.Case va boolT (E.Var va b) cb
                        [ E.Alt va E.DefaultAlt [] $ conE "True"
                        , E.Alt va (E.DataAlt "True") [] $ conE "False" ]
      , do -- (&&) :: Bool -> Bool -> Bool
            x <- freshId "x" $ E.monoSig boolT
            y <- freshId "y" $ E.monoSig boolT
            cb <- freshId "s" $ E.monoSig boolT
            mkVocab "GHC.Classes.&&" [] (E.Arrow va boolT $ E.Arrow va boolT boolT)
                  $ E.Lam va x $ E.Lam va y $ E.Case va boolT (E.Var va x) cb
                        [ E.Alt va E.DefaultAlt [] $ conE "False"
                        , E.Alt va (E.DataAlt "True") [] $ E.Var va y ]
      , do -- (||) :: Bool -> Bool -> Bool
            x <- freshId "x" $ E.monoSig boolT
            y <- freshId "y" $ E.monoSig boolT
            cb <- freshId "s" $ E.monoSig boolT
            mkVocab "GHC.Classes.||" [] (E.Arrow va boolT $ E.Arrow va boolT boolT)
                  $ E.Lam va x $ E.Lam va y $ E.Case va boolT (E.Var va x) cb
                        [ E.Alt va E.DefaultAlt [] $ E.Var va y
                        , E.Alt va (E.DataAlt "True") [] $ conE "True" ]
      , do -- fst :: forall a b. (a, b) -> a
            (a, b, _) <- tvs3
            let ta = tv a; tb = tv b; pt = pairT ta tb
            p  <- freshId "p" $ E.monoSig pt
            cb <- freshId "s" $ E.monoSig pt
            xa <- freshId "x" $ E.monoSig ta
            xb <- freshId "y" $ E.monoSig tb
            mkVocab "GHC.Internal.Data.Tuple.fst" [a, b] (E.Arrow va pt ta)
                  $ E.Lam va p $ E.Case va ta (E.Var va p) cb
                        [ E.Alt va (E.DataAlt "(,)") [xa, xb] $ E.Var va xa ]
      , do -- snd :: forall a b. (a, b) -> b
            (a, b, _) <- tvs3
            let ta = tv a; tb = tv b; pt = pairT ta tb
            p  <- freshId "p" $ E.monoSig pt
            cb <- freshId "s" $ E.monoSig pt
            xa <- freshId "x" $ E.monoSig ta
            xb <- freshId "y" $ E.monoSig tb
            mkVocab "GHC.Internal.Data.Tuple.snd" [a, b] (E.Arrow va pt tb)
                  $ E.Lam va p $ E.Case va tb (E.Var va p) cb
                        [ E.Alt va (E.DataAlt "(,)") [xa, xb] $ E.Var va xb ]
      ]
      where va :: Annote
            va = noAnn

            boolT :: E.Ty
            boolT = E.TyCon va "Bool"

            tv :: E.TyVar -> E.Ty
            tv = E.TyVarT va

            pairT :: E.Ty -> E.Ty -> E.Ty
            pairT a b = E.TyApp va (E.TyApp va (E.TyCon va "(,)") a) b

            conE :: E.DataConId -> E.Exp
            conE c = E.Con va boolT c

            tvs3 :: Monad m => BM m (E.TyVar, E.TyVar, E.TyVar)
            tvs3 = do
                  ua <- freshU
                  ub <- freshU
                  uc <- freshU
                  pure ( E.TyVar "a" ua E.KStar
                       , E.TyVar "b" ub E.KStar
                       , E.TyVar "c" uc E.KStar )

            mkVocab :: Monad m => Text -> [E.TyVar] -> E.Ty -> E.Exp -> BM m E.Defn
            mkVocab n tvs' t body = do
                  x <- freshId n $ E.Sig tvs' t
                  pure E.Defn
                        { E.defnAnnote = va
                        , E.defnId     = x
                        , E.defnParams = []
                        , E.defnBody   = body
                        , E.defnAttr   = Just E.Inline
                        , E.defnOrigin = Nothing
                        }

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The Core -> Crust bridge: translates the -O0 desugared Core of the whole
--   home module graph (one 'ModGuts' per module, from "ReWire.GHC.Session")
--   into a Crust 'FreeProgram' for the existing mid/back pipeline.
--
--   Shape of the translation (v1: class-free user programs; the reactive
--   monad and KnownNat are handled, other classes are rejected):
--
--   * Only the transitive closure of top-level binds reachable from the
--     start symbol is translated. This prunes @main@ (and its HasCallStack
--     machinery), Typeable plumbing (@$trModule@/@$tc*@/@$krep*@), derived
--     instance methods, and the rwPrim* GHC-compat helper module
--     (ReWire.BitWord), which freely uses Haskell outside the ReWire
--     vocabulary.
--
--   * Everything evidence-typed is erased: dictionary arguments, dictionary
--     lambda binders, and dictionary top-level binds. Class constraints do
--     not exist in Crust types (constraint arrows are dropped by the type
--     translation); the reactive monad ops are recognized as class
--     selectors applied at ReacT/StateT/Identity and become Builtins.
--
--   * rwPrim* references become 'M.Builtin's by occurrence name; their
--     GHC-compat bodies are never translated ('addPrims' supplies the
--     Crust-side definitions). GHC INLINE/NOINLINE pragmas ride along as
--     Crust defn attributes, so the retained inlineAnnotated pass collapses
--     the rewire-user wrappers onto the builtins.
--
--   * Integer literals arrive constructor-boxed (@IS 5#@) and are folded to
--     'M.LitInt' program-wide; strings arrive as @unpackCString#@
--     applications (or @[] \@Char@) and are folded to 'M.LitStr'; list
--     literals arrive as cons chains and are folded to 'M.LitList'.
--
--   * A small base vocabulary (Prelude names that the HSE front end used to
--     silently rebind to rewire-user definitions) is supported by
--     synthesizing INLINE Crust definitions ("ReWire.GHC.Vocab");
--     error/undefined and GHC's pattern-match failure functions map to the
--     Error builtin.
--
--   Name and type classification (primitives, user classes, erasable
--   evidence, the external tycon and vocabulary tables) lives in
--   "ReWire.GHC.Recognize".
module ReWire.GHC.ToCrust (coreToCrust, purgeTyAnns) where

import ReWire.Annotation (Annote, noAnn)
import ReWire.Config (Config, start)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Crust.Types (poly, poly', arr, strTy, listTy, mkArrowTy, plusTy, negTy, concrete, setTyAnn, tyAnn, (|->))
import ReWire.Crust.Util (mkApp, mkError)
import ReWire.GHC.Recognize (uKey, spanAnnote, varAnnote, isPrimModule, isPrimVar, homeishMod, qualName, conName, tupleName, splitStart, erasedArg, erasedEv, userPred, tyConModule, tyConKey, tyConTable, vocabTable)
import ReWire.GHC.Vocab (vocabDatas, vocabDefns)
import ReWire.SYB (transform)
import ReWire.Unbound (Fresh, fresh, s2n, n2s, Embed (..), bind, unsafeUnbind)

import qualified ReWire.Crust.Syntax as M

import Control.Arrow (first)
import Control.Lens ((^.))
import Control.Monad (foldM)
import Control.Monad.Except (catchError)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import GHC (ModuleName, moduleName, moduleNameString)
import GHC.Builtin.Types (integerISDataCon, integerIPDataCon, integerINDataCon, listTyCon, nilDataCon, consDataCon, unitTyCon, boolTyCon, charTyCon, integerTyCon, naturalTyCon)
import GHC.Builtin.Types.Literals (typeNatAddTyCon, typeNatSubTyCon, typeNatMulTyCon)
import GHC.Core (CoreExpr, CoreBind, Expr (..), Bind (..), Alt (..), AltCon (..), collectArgs)
import GHC.Core.DataCon (DataCon, dataConOrigArgTys, dataConUnivTyVars, dataConExTyCoVars, dataConTheta)
import GHC.Core.Class (classAllSelIds, classTyCon)
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.Predicate (isEvVarType)
import GHC.Core.TyCo.Rep (Type (..), TyLit (..), scaledThing)
import GHC.Core.TyCon (TyCon, tyConName, tyConKind, tyConDataCons, isClassTyCon, isAlgTyCon, isTypeSynonymTyCon, isBoxedTupleTyCon, tyConArity, isNewTyCon, tyConSingleDataCon)
import GHC.Core.Type (expandTypeSynonyms, mkTyVarTys, splitForAllTyCoVars)
import GHC.Core.Utils (exprType)
import GHC.Types.Basic (InlineSpec (..), inlinePragmaSpec)
import GHC.Types.Id (isClassOpId_maybe, idInlinePragma, isDataConId_maybe, isDFunId)
import GHC.Types.Literal (Literal (..))
import GHC.Types.Name (getOccString, nameModule_maybe, nameSrcSpan)
import GHC.Types.Var (Var, varName, varType, isTyVar)
import GHC.Types.Var.Set (elemVarSet)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHC.Utils.Outputable (showSDocUnsafe, ppr)

-- | Translation context: names of all home top-level binders (keyed by
--   unique), so use sites of (possibly non-exported, module-internal) ids
--   resolve to the same qualified Crust name as their definitions; plus
--   replacement expressions for local binders in scope.
data Ctx = Ctx
      { ctxTops   :: IM.IntMap Text  -- ^ top-level binder unique -> qualified Crust name.
      , ctxLocals :: IM.IntMap M.Exp -- ^ local binder unique -> replacement expression.
      }

-- | Strip most of the bridge's type annotations. The bridge annotates
--   nodes liberally so the typechecker keeps GHC's instantiation decisions
--   (tyAnn is the one slot tcExp honors), but annotations are
--   unbound-generics binders that later Alpha traversals (fv, hashing,
--   unbind) pay for on every node -- ruinous during partial evaluation.
--   The exception: Nat-bearing annotations, which pin widths that later
--   re-inference (e.g. inside the specializer's per-copy typeCheckDefn)
--   cannot always re-derive -- a width can be entirely internal to an
--   expression (@Proxy :: Proxy 127@ has no value-level witness at all;
--   @toFinite' (fromFinite x :: W 7)@ mentions 7 nowhere in its ends).
--   This mirrors the HSE path, where the corresponding *source
--   ascriptions* persist as annotations forever. Run after the typecheck
--   pass, which has consumed the rest.
purgeTyAnns :: M.FreeProgram -> M.FreeProgram
purgeTyAnns (ts, syns, ds) = (ts, syns, transform noPolyE $ transform noPolyP ds)
      where noPolyE :: M.Exp -> M.Exp
            noPolyE e | keep (tyAnn e) = e
                      | otherwise      = setTyAnn Nothing e

            noPolyP :: M.Pat -> M.Pat
            noPolyP = setTyAnn Nothing

            keep :: Maybe M.Poly -> Bool
            keep = \ case
                  Just (M.Poly (unsafeUnbind -> (_, t))) -> hasNat t && not (monadic t)
                  Nothing                                -> False

            hasNat :: M.Ty -> Bool
            hasNat = \ case
                  M.TyNat {}    -> True
                  M.TyApp _ a b -> hasNat a || hasNat b
                  _             -> False

            -- Types that purification rewrites: annotations mentioning them
            -- would go stale and fail --debug-typecheck's re-inference.
            monadic :: M.Ty -> Bool
            monadic = \ case
                  M.TyCon _ (n2s -> n) -> n `elem` (["ReacT", "StateT", "Identity", "PuRe", "A_", "R_"] :: [Text])
                  M.TyApp _ a b           -> monadic a || monadic b
                  _                       -> False

coreToCrust :: (Fresh m, MonadError AstError m) => Config -> [ModGuts] -> m M.FreeProgram
coreToCrust conf gutss = do
      let binds :: [(Var, CoreExpr, ModuleName)]
          binds = [ (b, rhs, moduleName $ mg_module g)
                  | g <- gutss, bnd <- mg_binds g, (b, rhs) <- flattenBind bnd ]

          tops :: IM.IntMap Text
          tops = IM.fromList [ (uKey b, qualName mn b) | (b, _, mn) <- binds ]

          bindMap :: IM.IntMap (Var, CoreExpr, ModuleName)
          bindMap = IM.fromList [ (uKey b, t) | t@(b, _, _) <- binds ]

          (startMod, startOcc) = splitStart $ conf^.start

          isStartV :: (Var, CoreExpr, ModuleName) -> Bool
          isStartV (b, _, mn) = moduleNameString mn == startMod && getOccString b == startOcc

      startV <- case filter isStartV binds of
            ((b, _, _) : _) -> pure b
            _               -> failAt noAnn $ "ghc-frontend: no definition for the start symbol (" <> conf^.start <> ")."

      -- All rwPrim* binds are roots besides the start symbol: builtins
      -- produced by recognition (Bind/Return from class selectors, Error,
      -- Eq) never reference them by Var, but the typechecker reads builtin
      -- type assumptions off their (body-elided) Defns. Unused ones are
      -- purged later.
      let roots = startV : [ b | (b, _, _) <- binds, isPrimVar b ]
          reach = reachable bindMap roots
          keep  = [ t | t@(b, _, _) <- binds
                      , uKey b `IS.member` reach
                      , not (prunedBind t) ]

      defs  <- mapM (bridgeDefn Ctx { ctxTops = tops, ctxLocals = mempty }) keep
      datas <- concat <$> mapM harvestDatas gutss
      pure (datas <> vocabDatas, [], defs <> vocabDefns)

flattenBind :: CoreBind -> [(Var, CoreExpr)]
flattenBind = \ case
      NonRec b rhs -> [(b, rhs)]
      Rec bs       -> bs

-- | Top-level binds never translated even when reachable: evidence-typed
--   binds (floated dictionaries; erased everywhere), except user-class
--   dictionaries (DFuns), which are kept as INLINE data definitions.
--   (rwPrim* binds ARE kept -- with elided bodies -- because the
--   typechecker reads the builtins' type assumptions off their Defn
--   signatures.)
prunedBind :: (Var, CoreExpr, ModuleName) -> Bool
prunedBind (b, _, _) = erasedEv (varType b)

-- | Transitive closure of top-level binds reachable from the roots, with
--   the same erasures the translation performs (type/coercion arguments,
--   evidence-typed subexpressions, rwPrim* leaves).
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
                  Var v                  -> [v]
                  Lit _                  -> []
                  App f a
                        | erasedArg a    -> refs f
                        | otherwise      -> refs f <> refs a
                  Lam _ e                -> refs e
                  Let (NonRec x r) e
                        | erasedEv (varType x) -> refs e
                        | otherwise      -> refs r <> refs e
                  Let (Rec bs) e         -> concatMap (refs . snd) bs <> refs e
                  Case s _ _ alts        -> refs s <> concatMap (\ (Alt _ _ e') -> refs e') alts
                  Cast e' _              -> refs e'
                  Tick _ e'              -> refs e'
                  Type _                 -> []
                  Coercion _             -> []

-- | Bridge a type, returning Nothing if it falls outside the vocabulary
--   (Crust's Maybe Ty slots are optional; the retained typechecker
--   re-infers).
tryTy :: (Fresh m, MonadError AstError m) => Annote -> GHC.Core.TyCo.Rep.Type -> m (Maybe M.Ty)
tryTy an t = catchError (Just <$> bridgeTy an (expandTypeSynonyms t)) $ const $ pure Nothing

---
--- Definitions.
---

bridgeDefn :: (Fresh m, MonadError AstError m) => Ctx -> (Var, CoreExpr, ModuleName) -> m M.Defn
bridgeDefn ctx (b, rhs, mn) = do
      let an         = varAnnote b
          (tvs, rho) = splitForAllTyCoVars $ expandTypeSynonyms $ varType b
      t    <- bridgeTy an rho
      -- The rwPrim* GHC-compat bodies are never translated (as on the HSE
      -- path): the Defn carries the signature (the typechecker's builtin
      -- assumption) with an error body.
      body <- if isPrimVar b
            then pure $ mkError an (Just t) $ "Prim: " <> qualName mn b
            else bridgeExp ctx an rhs
      pure M.Defn
            { M.defnAnnote = an
            , M.defnName   = s2n $ qualName mn b
            , M.defnPolyTy = Embed $ poly (map (s2n . pack . getOccString) $ filter isTyVar tvs) t
            -- Dictionary definitions (DFuns) are always inlined, so
            -- case-of-known-constructor can resolve method projections.
            , M.defnAttr   = if isDFunId b || userPred rho then Just M.Inline
                             else inlAttr $ inlinePragmaSpec $ idInlinePragma b
            , M.defnBody   = Embed $ bind [] body
            }
      where inlAttr :: InlineSpec -> Maybe M.DefnAttr
            inlAttr = \ case
                  Inline {}    -> Just M.Inline
                  Inlinable {} -> Just M.Inline
                  NoInline {}  -> Just M.NoInline
                  Opaque {}    -> Just M.NoInline
                  _            -> Nothing

---
--- Types.
---

bridgeKind :: GHC.Core.TyCo.Rep.Type -> M.Kind
bridgeKind = go . expandTypeSynonyms -- N.B. Nat is a synonym for Natural.
      where go = \ case
                  FunTy _ _ a r                      -> M.KFun (go a) (go r)
                  TyConApp tc _ | tc == naturalTyCon -> M.KNat
                  _                                  -> M.KStar

-- | Core Type -> Crust Ty. Callers are expected to expandTypeSynonyms
--   first (Core types are not synonym-free). Constraint (invisible) arrows
--   are dropped: Crust types have no contexts.
bridgeTy :: MonadError AstError m => Annote -> GHC.Core.TyCo.Rep.Type -> m M.Ty
bridgeTy an = go
      where go :: MonadError AstError m => GHC.Core.TyCo.Rep.Type -> m M.Ty
            go = \ case
                  TyVarTy v            -> pure $ M.TyVar an (bridgeKind $ varType v) $ s2n $ pack $ getOccString v
                  AppTy a b            -> M.TyApp an <$> go a <*> go b
                  TyConApp tc args     -> bridgeTyConApp an tc args
                  ForAllTy _ t         -> go t -- Quantifiers live in the Poly.
                  FunTy _ _ a r
                        -- User-class constraint arrows become value arrows
                        -- (the dictionary is data); built-in evidence is
                        -- dropped (Crust types have no contexts).
                        | userPred a    -> arr <$> go a <*> go r
                        | isEvVarType a -> go r
                        | otherwise     -> arr <$> go a <*> go r
                  LitTy (NumTyLit n)   -> pure $ M.TyNat an $ fromInteger n
                  LitTy l              -> failAt an $ "ghc-frontend: unsupported type-level literal: " <> pack (showSDocUnsafe $ ppr l)
                  CastTy t _           -> go t
                  CoercionTy _         -> failAt an "ghc-frontend: unsupported coercion in type position."

bridgeTyConApp :: MonadError AstError m => Annote -> TyCon -> [GHC.Core.TyCo.Rep.Type] -> m M.Ty
bridgeTyConApp an tc args
      -- [Char] is Crust's String.
      | tc == listTyCon, [TyConApp c []] <- args, c == charTyCon
                              = pure $ strTy an
      | tc == listTyCon, [a] <- args
                              = listTy an <$> bridgeTy an a
      | tc == listTyCon       = appN "[_]"
      | tc == boolTyCon       = appN "Bool"
      | tc == unitTyCon       = appN "()"
      | tc == integerTyCon    = appN "Integer"
      | tc == naturalTyCon    = appN "Integer" -- Natural at the type level: treat as Integer.
      | isBoxedTupleTyCon tc  = appN $ tupleName $ tyConArity tc
      | tc == typeNatAddTyCon = appN "+"
      | tc == typeNatMulTyCon = appN "*"
      | tc == typeNatSubTyCon = case args of
            [a, b] -> plusTy an <$> bridgeTy an a <*> (negTy <$> bridgeTy an b)
            _      -> failAt an "ghc-frontend: unsaturated type-level subtraction."
      -- The function tycon in prefix position: [mult, rep1, rep2, a, b].
      | getOccString (tyConName tc) == "FUN"
                              = case reverse args of
            (b : a : _) -> arr <$> bridgeTy an a <*> bridgeTy an b
            _           -> failAt an "ghc-frontend: unsaturated function type constructor."
      | otherwise             = case lookup (tyConKey tc) tyConTable of
            Just (crustName, dropN) -> appN' (drop dropN args) crustName
            Nothing
                  | Just mn <- tyConModule tc, isPrimModule mn
                              -> appN $ pack $ getOccString $ tyConName tc
                  | Just mn <- tyConModule tc, homeish mn
                              -> appN $ pack (moduleNameString mn) <> "." <> pack (getOccString $ tyConName tc)
                  | otherwise -> failAt an $ "ghc-frontend: type not in the ReWire vocabulary: "
                                    <> pack (maybe "?" moduleNameString (tyConModule tc)) <> "." <> pack (getOccString $ tyConName tc)
      where appN :: MonadError AstError m => Text -> m M.Ty
            appN = appN' args

            appN' :: MonadError AstError m => [GHC.Core.TyCo.Rep.Type] -> Text -> m M.Ty
            appN' as n = foldl (M.TyApp an) (M.TyCon an $ s2n n) <$> mapM (bridgeTy an) as

            -- Home modules (loaded from source) get qualified names; known
            -- external tycons are in the table; anything else external is
            -- out of vocabulary. There is no unit map here, so approximate:
            -- well-known external namespace prefixes are rejected,
            -- everything else is assumed to be a home module.
            homeish :: ModuleName -> Bool
            homeish mn = not $ any (`T.isPrefixOf` pack (moduleNameString mn))
                  (["GHC.", "Data.", "Control.", "System.", "Foreign.", "Text.", "Unsafe."] :: [Text])

---
--- Expressions.
---

bridgeExp :: (Fresh m, MonadError AstError m) => Ctx -> Annote -> CoreExpr -> m M.Exp
bridgeExp ctx an e = case e of
      App {}                   -> bridgeApp ctx an e
      Var {}                   -> bridgeApp ctx an e

      Lit (LitString bs)       -> pure $ M.LitStr an Nothing $ TE.decodeUtf8 bs
      Lit l                    -> failAt an $ "ghc-frontend: unsupported literal (" <> pack (showSDocUnsafe $ ppr l) <> ")."

      Lam b body
            | isTyVar b               -> bridgeExp ctx an body
            | erasedEv (varType b) -> bridgeExp ctx an body
            | otherwise               -> do
                  n     <- fresh $ s2n $ pack $ getOccString b
                  argTy <- tryTy an $ varType b
                  body' <- bridgeExp (bindLocal b (M.Var an Nothing argTy n) ctx) an body
                  pure $ M.Lam an Nothing argTy $ bind n body'

      -- Crust has no Let: lower to a beta-redex, (\ x -> body) rhs -- the
      -- HSE desugarer's let-encoding. N.B. NOT a single-arm PatVar case:
      -- liftLambdas lifts redex lambdas to top-level defns (which the
      -- partial evaluator treats as opaque calls), while it leaves case
      -- arms alone and reduce substitutes through PatVar arms
      -- unconditionally -- flattening deep let chains exponentially.
      --
      -- Nested-let telescopes are re-scheduled by demand first: GHC's
      -- desugarer emits a source let-group's bindings clustered in SCC
      -- dependency order, destroying the source's interleaving; under a
      -- long two-strand chain (e.g. a manually unrolled loop), every
      -- binding of one strand is then still live at the other strand's
      -- bindings, so the lambda-lifted closures take one parameter per
      -- chain element and partial evaluation grinds. Demand-order
      -- placement (a binding is placed innermost as soon as all its
      -- consumers are placed) restores minimal live ranges. Dead bindings
      -- (unreachable from the body) are dropped.
      Let (NonRec {}) _ -> do
            let (bs, body) = collectLets e
            n2e   <- mapM (\ (x, _) -> do
                        nm <- fresh $ s2n $ pack $ getOccString x
                        t  <- tryTy an $ varType x
                        pure (x, (nm, t)))
                     bs
            let ctx' = foldr (\ (x, (nm, t)) c -> bindLocal x (M.Var an Nothing t nm) c) ctx n2e
            body'  <- bridgeExp ctx' an body
            bodyTy <- tryTy an $ exprType body
            let order = scheduleLets (map fst bs) body $ map snd bs
            foldM (\ acc i -> do
                        let (x, rhs) = bs !! i
                            (nm, t)  = fromMaybe (error "ghc-frontend: scheduleLets") $ lookup x n2e
                        rhs' <- bridgeExp ctx' an rhs
                        pure $ M.App an Nothing bodyTy (M.Lam an Nothing t $ bind nm acc) rhs')
                  body' order

      Let (Rec _) _            -> failAt an "ghc-frontend: unsupported local recursive binding."

      Case scrut b ty alts     -> bridgeCase ctx an scrut b ty alts

      Cast e' _                -> bridgeExp ctx an e'
      Tick _ e'                -> bridgeExp ctx an e'
      Type _                   -> failAt an "ghc-frontend: unexpected type in expression position."
      Coercion _               -> failAt an "ghc-frontend: unexpected coercion in expression position."

bindLocal :: Var -> M.Exp -> Ctx -> Ctx
bindLocal v e ctx = ctx { ctxLocals = IM.insert (uKey v) e $ ctxLocals ctx }

-- | Collect a maximal telescope of nested non-recursive lets (dropping
--   evidence binders, whose uses are erased anyway).
collectLets :: CoreExpr -> ([(Var, CoreExpr)], CoreExpr)
collectLets = \ case
      Let (NonRec x r) e
            | erasedEv (varType x) -> collectLets e
            | otherwise               -> first ((x, r) :) $ collectLets e
      e                               -> ([], e)

-- | Order a let-telescope's bindings innermost-first by demand: place a
--   binding as soon as no still-unplaced binding's rhs references it (all
--   its consumers are already inside its scope, so placement is
--   scope-correct by construction); break ties by most recent demand,
--   which interleaves chained strands and keeps live ranges minimal.
--   Bindings unreachable from the body are dropped.
scheduleLets :: [Var] -> CoreExpr -> [CoreExpr] -> [Int]
scheduleLets xs body rhss = loop live bodyDeps []
      where idxs :: [Int]
            idxs = [0 .. length xs - 1]

            deps :: Int -> IS.IntSet
            deps i = IS.fromList [ j | j <- idxs, j /= i, (xs !! j) `elemVarSet` (fvss !! i) ]
                  where fvss = map exprFreeVars rhss

            bodyDeps :: [Int]
            bodyDeps = [ j | j <- idxs, (xs !! j) `elemVarSet` exprFreeVars body ]

            live :: IS.IntSet
            live = go (IS.fromList bodyDeps) bodyDeps
                  where go seen []       = seen
                        go seen (i : is) = let new = IS.toList $ deps i IS.\\ seen
                                           in go (seen <> deps i) (new <> is)

            loop :: IS.IntSet -> [Int] -> [Int] -> [Int]
            loop remaining stack out
                  | IS.null remaining = reverse out
                  | otherwise         = loop (IS.delete pick remaining) stack' (pick : out)
                  where ready :: Int -> Bool
                        ready c = c `IS.member` remaining
                              && not (any (\ j -> j /= c && c `IS.member` deps j) (IS.toList remaining))

                        pick :: Int
                        pick = case filter ready stack of
                              (c : _) -> c
                              _       -> case filter ready (IS.toList remaining) of
                                    (c : _) -> c
                                    _       -> error "ghc-frontend: scheduleLets: cyclic let telescope."

                        stack' :: [Int]
                        stack' = IS.toList (deps pick) <> filter (/= pick) stack

-- | Translate an application spine (or a bare head). The whole
--   application's concrete result type is pinned as a type annotation on
--   the outermost node: annotations on wrapper heads are lost when
--   inlineAnnotated substitutes their bodies (before the typechecker
--   runs), but the enclosing application node survives, preserving GHC's
--   instantiation decisions (e.g. the target width of fromFinite/resize).
bridgeApp :: (Fresh m, MonadError AstError m) => Ctx -> Annote -> CoreExpr -> m M.Exp
bridgeApp ctx an e = do
      let (f, args) = collectArgs e
          vargs     = filter (not . erasedArg) args
      e' <- case f of
            Var v -> bridgeVarApp ctx an v args vargs
            _     -> do
                  f'  <- bridgeExp ctx an f
                  as' <- mapM (bridgeExp ctx an) vargs
                  pure $ mkApp an f' as'
      if null vargs then pure e' else do
            wt <- tryTy an $ exprType e
            pure $ maybeAnn wt e'

-- | The Var-headed application dispatch: locals, builtins, constructors,
--   class ops, home ids, base vocabulary.
bridgeVarApp :: (Fresh m, MonadError AstError m) => Ctx -> Annote -> Var -> [CoreExpr] -> [CoreExpr] -> m M.Exp
bridgeVarApp ctx an v args vargs
      -- Local binder (lambda, case, let).
      | Just e' <- IM.lookup (uKey v) $ ctxLocals ctx
            = apply e'

      -- rwPrim* -> Builtin, by occurrence name.
      | isPrimVar v
            = case lookup (pack $ getOccString v) M.builtins of
                  Just b  -> do
                        t <- headTy
                        apply $ maybeAnn t $ M.Builtin an Nothing t b
                  Nothing -> failAt an $ "ghc-frontend: unknown primitive: " <> pack (getOccString v)

      -- Data constructor (worker or wrapper).
      | Just dc <- isDataConId_maybe v = bridgeConApp ctx an v dc args vargs

      -- Class method selector: the reactive monad ops and Integer equality.
      | Just _ <- isClassOpId_maybe v = bridgeClassOp ctx an v args vargs

      -- Home top-level id.
      | Just n <- IM.lookup (uKey v) $ ctxTops ctx = do
            t <- headTy
            apply $ maybeAnn t $ M.Var an Nothing t $ s2n n

      -- Base vocabulary.
      | otherwise = bridgeBaseVocab ctx an v vargs

      where apply :: (Fresh m, MonadError AstError m) => M.Exp -> m M.Exp
            apply h = mkApp an h <$> mapM (bridgeExp ctx an) vargs

            -- The head's instantiated type (after the leading type/dict
            -- application prefix), when it bridges cleanly.
            headTy :: (Fresh m, MonadError AstError m) => m (Maybe M.Ty)
            headTy = tryTy an $ exprType $ foldl App (Var v) $ takeWhile erasedArg args

-- | Annotate a head with its concrete instantiated type, so the retained
--   typechecker (which re-infers node types but honors tyAnn) keeps GHC's
--   instantiation decisions.
maybeAnn :: Maybe M.Ty -> M.Exp -> M.Exp
maybeAnn (Just t) e | concrete t = setTyAnn (Just $ poly' t) e
maybeAnn _        e              = e

-- | Data constructor applications, with the literal folds: Integer boxes
--   to LitInt, cons chains to LitList, Char nil to LitStr, and the special
--   constructor name table.
bridgeConApp :: (Fresh m, MonadError AstError m) => Ctx -> Annote -> Var -> DataCon -> [CoreExpr] -> [CoreExpr] -> m M.Exp
bridgeConApp ctx an v dc args vargs
      -- Boxed integer literals: IS 5# / IP bigNat# / IN bigNat#.
      | dc == integerISDataCon, [Lit (LitNumber _ n)] <- vargs = pure $ M.LitInt an Nothing n
      | dc == integerIPDataCon, [Lit (LitNumber _ n)] <- vargs = pure $ M.LitInt an Nothing n
      | dc == integerINDataCon, [Lit (LitNumber _ n)] <- vargs = pure $ M.LitInt an Nothing $ negate n
      | dc `elem` [integerISDataCon, integerIPDataCon, integerINDataCon]
            = failAt an "ghc-frontend: Integer constructor applied to a non-literal."

      -- List literals: cons chains and nil.
      | dc == nilDataCon || dc == consDataCon = do
            elems <- listElems $ foldl App (Var v) args
            ty    <- tryTy an $ exprType $ foldl App (Var v) args
            case (elems, ty) of
                  ([], Just t) | t == strTy an -> pure $ M.LitStr an Nothing ""
                  _                            -> M.LitList an Nothing ty <$> mapM (bridgeExp ctx an) elems

      | otherwise = do
            t <- tryTy an $ exprType $ foldl App (Var v) $ takeWhile erasedArg args
            mkApp an (maybeAnn t $ M.Con an Nothing t $ s2n $ conName dc) <$> mapM (bridgeExp ctx an) vargs

      where listElems :: (Fresh m, MonadError AstError m) => CoreExpr -> m [CoreExpr]
            listElems e = case collectArgs e of
                  (Var v', as)
                        | Just dc' <- isDataConId_maybe v', dc' == nilDataCon  -> pure []
                        | Just dc' <- isDataConId_maybe v', dc' == consDataCon
                        , [hd, tl] <- filter (not . erasedArg) as              -> (hd :) <$> listElems tl
                  _ -> failAt an "ghc-frontend: unsupported non-literal list (lists must be literal cons chains)."

-- | Class method dispatch: user-class methods project the field out of
--   the dictionary (which is ordinary data); >>=, >>, return, pure at
--   ReacT/StateT/Identity or at an unresolved monad type become Bind/Return
--   builtins (dictionary discarded; a concrete monad off the reactive stack
--   is rejected); == at Integer becomes the Eq builtin. Anything else is
--   out of vocabulary.
bridgeClassOp :: (Fresh m, MonadError AstError m) => Ctx -> Annote -> Var -> [CoreExpr] -> [CoreExpr] -> m M.Exp
bridgeClassOp ctx an v args vargs
      | Just cls <- isClassOpId_maybe v
      , homeishMod (tyConModule $ classTyCon cls) = case vargs of
            (d : rest) -> do
                  d' <- bridgeExp ctx an d
                  if isNewTyCon (classTyCon cls)
                        -- A newtype dictionary IS its method (casts are
                        -- erased): selection is the identity.
                        then mkApp an d' <$> mapM (bridgeExp ctx an) rest
                        else do
                              let sels = classAllSelIds cls
                              i   <- maybe (failAt an $ "ghc-frontend: unknown class method: " <> occ) pure
                                          $ elemIndex v sels
                              fld <- fresh $ s2n "dictField"
                              let pats = [ if j == i then M.PatVar an (Embed Nothing) (Embed Nothing) fld
                                                     else M.PatWildCard an (Embed Nothing) (Embed Nothing)
                                         | j <- [0 .. length sels - 1] ]
                                  pat  = M.PatCon an (Embed Nothing) (Embed Nothing) (Embed $ s2n $ conName $ tyConSingleDataCon $ classTyCon cls) pats
                                  proj = M.Case an Nothing Nothing d' (bind pat $ M.Var an Nothing Nothing fld) Nothing
                              mkApp an proj <$> mapM (bridgeExp ctx an) rest
            _          -> failAt an $ "ghc-frontend: unapplied class method: " <> occ
      | occ `elem` ([">>=", ">>", "return", "pure"] :: [Text]) = case tyArgHead of
            Just tcn | tcn `notElem` (["ReacT", "StateT", "Identity"] :: [Text])
                     -> failAt an $ "ghc-frontend: monadic operator at unsupported monad: " <> tcn
            -- A reactive-stack monad or one not yet resolved (e.g., a type
            -- variable in the body of a monad-polymorphic helper): emit the
            -- polymorphic builtin and let the Crust typechecker resolve it
            -- when the helper is monomorphized.
            _        -> case occ of
                  ">>=" -> apply $ M.Builtin an Nothing Nothing M.Bind
                  ">>"  -> case vargs of
                        [e1, e2] -> do
                              e1' <- bridgeExp ctx an e1
                              e2' <- bridgeExp ctx an e2
                              k   <- fresh $ s2n "_unused"
                              pure $ mkApp an (M.Builtin an Nothing Nothing M.Bind) [e1', M.Lam an Nothing Nothing $ bind k e2']
                        _        -> failAt an "ghc-frontend: unsaturated (>>)."
                  _     -> apply $ M.Builtin an Nothing Nothing M.Return -- return/pure
      | occ == "==", Just "Integer" <- tyArgHead = apply $ M.Builtin an Nothing Nothing M.Eq
      | otherwise = failAt an $ "ghc-frontend: unsupported use of a type class method: " <> occ
      where occ :: Text
            occ = pack $ getOccString v

            apply :: (Fresh m, MonadError AstError m) => M.Exp -> m M.Exp
            apply h = mkApp an h <$> mapM (bridgeExp ctx an) vargs

            -- The head tycon of the first type argument, as a Crust name.
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

-- | The base vocabulary: error functions map to the Error builtin; string
--   unpacking folds to LitStr; a small closed set of Prelude combinators
--   maps to synthesized definitions (see vocabDefns).
bridgeBaseVocab :: (Fresh m, MonadError AstError m) => Ctx -> Annote -> Var -> [CoreExpr] -> m M.Exp
bridgeBaseVocab ctx an v vargs
      | occ `elem` (["unpackCString#", "unpackCStringUtf8#"] :: [Text]) = case vargs of
            [Lit (LitString bs)] -> pure $ M.LitStr an Nothing $ TE.decodeUtf8 bs
            _                    -> failAt an "ghc-frontend: unpackCString# applied to a non-literal."

      | occ `elem` (["error", "errorWithoutStackTrace"] :: [Text]) = do
            t  <- resTy
            as <- mapM (bridgeExp ctx an) vargs
            pure $ mkApp an (M.Builtin an Nothing (arr (strTy an) <$> t) M.Error) as

      | occ == "undefined" = do
            t <- resTy
            pure $ mkError an t "Prelude.undefined"

      | occ `elem` ([ "patError", "absentError", "nonExhaustiveGuardsError"
                    , "noMethodBindingError", "recSelError", "typeError" ] :: [Text]) = do
            t <- resTy
            pure $ mkError an t "Pattern match failure: non-exhaustive patterns in case"

      | Just n <- lookup occ =<< flip lookup vocabTable =<< modName = do
            as <- mapM (bridgeExp ctx an) vargs
            pure $ mkApp an (M.Var an Nothing Nothing $ s2n n) as

      | otherwise = failAt an $ "ghc-frontend: not in the ReWire vocabulary: "
            <> fromMaybe "?" modName <> "." <> occ

      where occ :: Text
            occ = pack $ getOccString v

            modName :: Maybe Text
            modName = pack . moduleNameString . moduleName <$> nameModule_maybe (varName v)

            resTy :: (Fresh m, MonadError AstError m) => m (Maybe M.Ty)
            resTy = case vargs of
                  [] -> tryTy an $ varType v -- crude; only for arity-0 uses
                  _  -> pure Nothing

---
--- Case expressions.
---

-- | Lower a Core case to Crust's crushed 1-2-alt cascade. Core orders the
--   DEFAULT alternative first. The scrutinee is bound once (with the
--   let-encoding: a PatVar single-arm case) and the cascade scrutinizes
--   the bound variable -- naive nesting would duplicate the scrutinee
--   expression per alternative (exponential after GHC's if-then-else
--   two-DataAlt cases chain up; the shared binder is also how the case
--   binder's occurrences are handled).
bridgeCase :: (Fresh m, MonadError AstError m) => Ctx -> Annote -> CoreExpr -> Var -> GHC.Core.TyCo.Rep.Type -> [Alt Var] -> m M.Exp
bridgeCase ctx an scrut b ty alts = do
      scrut'  <- bridgeExp ctx an scrut
      resTy   <- tryTy an ty
      scrutTy <- tryTy an $ exprType scrut
      case alts of
            -- Zero-alt case: the scrutinee is bottom (e.g. patError).
            []                     -> pure $ mkError an resTy "Pattern match failure: non-exhaustive patterns in case"
            -- seq-like: case scrut of b { DEFAULT -> rhs }: a let.
            [Alt DEFAULT [] rhs]   -> do
                  n    <- fresh $ s2n $ pack $ getOccString b
                  rhs' <- bridgeExp (bindLocal b (M.Var an Nothing scrutTy n) ctx) an rhs
                  pure $ M.App an Nothing resTy (M.Lam an Nothing scrutTy $ bind n rhs') scrut'
            -- Single non-default alternative: no sharing needed (the one
            -- pattern arm consumes the scrutinee; the case binder still
            -- gets a shared name only if it could occur, which the direct
            -- form handles by substituting the scrutinee -- safe, since
            -- there is no duplication with a single arm).
            [Alt con vs rhs]       -> do
                  (pat, ctx') <- bridgePat (bindLocal b scrut' ctx) con vs
                  rhs'        <- bridgeExp ctx' an rhs
                  pure $ M.Case an Nothing resTy scrut' (bind pat rhs') Nothing
            _                      -> do
                  n <- fresh $ s2n $ pack $ getOccString b
                  let sv   = M.Var an Nothing scrutTy n
                      ctx' = bindLocal b sv ctx
                  body <- case alts of
                        (Alt DEFAULT [] rhs : rest) -> do
                              mdef <- Just <$> bridgeExp ctx' an rhs
                              cascade ctx' sv resTy mdef rest
                        rest                        -> cascade ctx' sv resTy Nothing rest
                  -- Share the scrutinee with a let (a beta-redex; see the
                  -- Let comment above).
                  pure $ M.App an Nothing resTy (M.Lam an Nothing scrutTy $ bind n body) scrut'
      where -- Build the cascade over the shared scrutinee variable. When
            -- the alternatives are exhaustive (no Core DEFAULT) and the
            -- last one binds nothing, it becomes the innermost *default*
            -- rather than a final re-test: the re-test would duplicate the
            -- scrutinee once reduce substitutes through the shared binder
            -- (exponential across chained if-then-elses, which GHC
            -- desugars to two-DataAlt cases).
            cascade :: (Fresh m, MonadError AstError m) => Ctx -> M.Exp -> Maybe M.Ty -> Maybe M.Exp -> [Alt Var] -> m M.Exp
            cascade ctx' scrut' resTy mdef alts' = do
                  (arms, innermost) <- case (mdef, reverse alts') of
                        (Just d, _) -> pure (alts', Just d)
                        (Nothing, Alt (DataAlt _) vs rhs : rest@(_ : _))
                              | all skippableBinder vs -> do
                                    d <- bridgeExp ctx' an rhs
                                    pure (reverse rest, Just d)
                        _           -> pure (alts', Nothing)
                  go arms innermost
                  where go :: (Fresh m, MonadError AstError m) => [Alt Var] -> Maybe M.Exp -> m M.Exp
                        go [] _ = failAt an "ghc-frontend: empty case alternatives."
                        go [Alt con vs rhs] md = do
                              (pat, ctx'') <- bridgePat ctx' con vs
                              rhs'         <- bridgeExp ctx'' an rhs
                              pure $ M.Case an Nothing resTy scrut' (bind pat rhs') md
                        go (Alt con vs rhs : rest') md = do
                              (pat, ctx'') <- bridgePat ctx' con vs
                              rhs'         <- bridgeExp ctx'' an rhs
                              rest''       <- go rest' md
                              pure $ M.Case an Nothing resTy scrut' (bind pat rhs') $ Just rest''

                        skippableBinder :: Var -> Bool
                        skippableBinder v = isTyVar v || erasedEv (varType v)

            bridgePat :: (Fresh m, MonadError AstError m) => Ctx -> AltCon -> [Var] -> m (M.Pat, Ctx)
            bridgePat ctx' con vs = case con of
                  DataAlt dc -> do
                        (ps, ctx'') <- foldM patVar ([], ctx') $ filter (\ v -> not (isTyVar v) && not (erasedEv $ varType v)) vs
                        pure (M.PatCon an (Embed Nothing) (Embed Nothing) (Embed $ s2n $ conName dc) $ reverse ps, ctx'')
                  LitAlt _   -> failAt an "ghc-frontend: unsupported literal pattern."
                  DEFAULT    -> failAt an "ghc-frontend: unexpected default alternative."

            patVar :: (Fresh m, MonadError AstError m) => ([M.Pat], Ctx) -> Var -> m ([M.Pat], Ctx)
            patVar (ps, ctx') pv = do
                  n <- fresh $ s2n $ pack $ getOccString pv
                  t <- tryTy an $ varType pv
                  pure (M.PatVar an (Embed Nothing) (Embed t) n : ps, bindLocal pv (M.Var an Nothing t n) ctx')

---
--- Datatypes.
---

-- | User DataDefns from a module's tycons; RWC.Primitives' tycons are
--   supplied by addPrims and skipped here.
harvestDatas :: (Fresh m, MonadError AstError m) => ModGuts -> m [M.DataDefn]
harvestDatas g
      | isPrimModule mn = pure []
      | otherwise       = mapM harvest $ filter wanted $ mg_tcs g
      where mn = moduleName $ mg_module g

            -- User datatypes; also data-dictionary classes (the dictionary
            -- constructor's fields are the superclass dictionaries and
            -- method types, which the generic path below translates).
            -- Newtype-dictionary classes (single method, no superclass)
            -- have no data constructor: their dictionaries erase to the
            -- method value via cast-erasure.
            wanted :: TyCon -> Bool
            wanted tc = isAlgTyCon tc && not (isTypeSynonymTyCon tc) && not (isBoxedTupleTyCon tc)
                  && not (isClassTyCon tc && isNewTyCon tc)

            harvest :: (Fresh m, MonadError AstError m) => TyCon -> m M.DataDefn
            harvest tc = M.DataDefn an (s2n name) (bridgeKind $ tyConKind tc) <$> mapM dataCon (tyConDataCons tc)
                  where an   = spanAnnote $ nameSrcSpan $ tyConName tc
                        name = pack (moduleNameString mn) <> "." <> pack (getOccString $ tyConName tc)

                        dataCon :: (Fresh m, MonadError AstError m) => DataCon -> m M.DataCon
                        dataCon dc | not (null $ dataConExTyCoVars dc) = failAt an "ghc-frontend: unsupported existential data constructor."
                                   | otherwise = do
                              -- The theta part holds a class dictionary
                              -- constructor's superclass dictionaries
                              -- (empty for vanilla datatypes).
                              argTys <- mapM (bridgeTy an . expandTypeSynonyms)
                                            $ dataConTheta dc <> map scaledThing (dataConOrigArgTys dc)
                              resTy  <- bridgeTy an $ TyConApp tc $ mkTyVarTys $ dataConUnivTyVars dc
                              pure $ M.DataCon an (s2n $ conName dc)
                                   $ map (s2n . pack . getOccString) (dataConUnivTyVars dc) |-> mkArrowTy argTys resTy


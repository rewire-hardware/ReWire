{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
-- | Well-formedness checking for Eidos programs (doc/eidos.md §4): global
--   binder uniqueness, scoping, the spine discipline, bidirectional
--   (synthesis-plus-comparison) type checking, the join point discipline,
--   and the definition and program rules — all with located diagnostics.
--
--   The linter runs in one of four cumulative modes ('LintMode'),
--   corresponding to the pipeline's invariant stages (§4.1):
--
--   * 'LintPoly' (post-bridge): the rules of §4.2–§4.4.
--   * 'LintMono' (post-specialization): additionally, every definition
--     signature is monomorphic and every type is nat-closed. Datatypes stay
--     parametric through specialization (§3.6), so constructor signatures
--     are exempt from the mono rules; 'Con' occurrences carry instantiated
--     types, which are not. Builtin-named definitions (rwPrim*) are the
--     builtins' type assumptions riding as polymorphic signature carriers
--     and check in poly mode. Value binders may still be higher-order here:
--     first-orderization is the partial evaluator's job, downstream of
--     specialization, so the first-order rule belongs to mono+ANF.
--   * 'LintMonoANF' and 'LintMachine': reserved for later stages of the
--     Eidos migration (§6, §7.4); requesting them is an error for now.
--
--   There is no inference and no unification anywhere: every binder carries
--   its type, so every expression synthesizes, and checking an expression
--   against a type is synthesis followed by structural comparison after
--   'natNorm'. This is the monadic, located-diagnostic twin of
--   'ReWire.Eidos.Types.typeOf', which is total on programs this module
--   accepts.
--
--   TODO(eidos): mono+ANF mode (when it lands) must enforce the full
--   representable-closure type grammar of §4.1 (a permit-list of type
--   constructors — Vec, Finite, Bool, (), tuples, monomorphic ADTs,
--   Integer, Proxy, String in literal positions — plus
--   ReacT/StateT/Identity until purification); mono mode checks
--   no-polymorphism and nat-closure.
--   TODO(eidos): type arguments and constructor fields are not kind-checked
--   (there is no kind table for built-in type constructors); type-variable
--   occurrences are checked against their binders' kinds.
--   TODO(eidos): there is no built-in datatype basis yet — 'Con' occurrences
--   of (), the tuple family, and Bool lint only if the program declares
--   them; the bridge stage supplies the prim basis (as Crust's addPrims
--   does today).
--   TODO(eidos): primitive occurrence types are trusted (scope/closedness
--   checked, but not compared against a builtin signature table); the
--   bridge stage carries the rwPrim* signatures and should land the table.
module ReWire.Eidos.Lint (LintMode (..), lint, lintDefn) where

import ReWire.Annotation (Annote, ann, noAnn)
import ReWire.Builtins (builtins)
import ReWire.Error (AstError, MonadError, failAt, failAtWith, failInternal)
import ReWire.Eidos.Pretty ()
import ReWire.Eidos.Syntax
import ReWire.Eidos.Types (natNorm, evalNat, instantiate, substTv, flattenApp, flattenArrow, flattenTyApp)
import ReWire.Pretty (prettyPrint, showt)

import Control.Monad (foldM, foldM_, unless, when, zipWithM_)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set

-- | The linter's mode: which stage of the pipeline's cumulative static
--   discipline to enforce (doc/eidos.md §4.1). Modes are ordered by
--   strength.
data LintMode = LintPoly | LintMono | LintMonoANF | LintMachine
      deriving (Eq, Ord, Show)

modeName :: LintMode -> Text
modeName = \ case
      LintPoly    -> "poly"
      LintMono    -> "mono"
      LintMonoANF -> "mono+ANF"
      LintMachine -> "machine"

-- | Check a whole program in the given mode; succeeds exactly when every
--   rule of the mode holds.
lint :: MonadError AstError m => LintMode -> Program -> m ()
lint mode p = do
      checkImplemented mode
      checkProgram (mkEnv mode p) p

-- | Check a single definition against a program's global context: every
--   rule except the whole-program ones (global binder uniqueness, global
--   name distinctness, datatype well-formedness, and the @top@ rule).
lintDefn :: MonadError AstError m => LintMode -> Program -> Defn -> m ()
lintDefn mode p d = do
      checkImplemented mode
      checkDefn (mkEnv mode p) d

checkImplemented :: MonadError AstError m => LintMode -> m ()
checkImplemented m = when (m > LintMono) $ failInternal noAnn
      $ "Eidos lint: mode " <> modeName m <> " is not yet implemented"

---
--- Environments.
---

-- | A join point visible in the current scope: its declared 'JoinId', its
--   parameter types, and its result type (the type of its scope).
data JoinDef = JoinDef !JoinId ![Ty] !Ty

data Env = Env
      { envMode  :: LintMode
      , envCons  :: HashMap DataConId (TyConId, Sig) -- data constructors, by name
      , envScope :: HashMap Uniq Id                  -- term binders in scope
      , envTVs   :: HashMap Uniq TyVar               -- signature type variables in scope
      , envJoins :: HashMap Uniq JoinDef             -- join points lexically visible
      , envTail  :: HashSet Uniq                     -- joins jumpable from here (tail position of their scopes)
      }

mkEnv :: LintMode -> Program -> Env
mkEnv mode (Program datas defns _) = Env
      { envMode  = mode
      , envCons  = Map.fromList [ (c, (dataName d, sig)) | d <- datas, DataCon _ c sig <- dataCons d ]
      , envScope = Map.fromList [ (idUniq $ defnId d, defnId d) | d <- defns ]
      , envTVs   = mempty
      , envJoins = mempty
      , envTail  = mempty
      }

bindVar :: Id -> Env -> Env
bindVar x env = env { envScope = Map.insert (idUniq x) x $ envScope env }

-- | Make a join point lexically visible without making it jumpable (used
--   for its own body: join points are not recursive).
scopeJoin :: Id -> JoinDef -> Env -> Env
scopeJoin x jd env = env { envJoins = Map.insert (idUniq x) jd $ envJoins env }

-- | Make a join point visible and jumpable (used for its scope: the let
--   body, whose tail is the join's tail).
bindJoin :: Id -> JoinDef -> Env -> Env
bindJoin x jd env = (scopeJoin x jd env) { envTail = Set.insert (idUniq x) $ envTail env }

-- | Entering a non-tail position: no join is jumpable from here (they stay
--   visible, for diagnostics), though joins bound inside are jumpable
--   within their own scopes.
nonTail :: Env -> Env
nonTail env = env { envTail = mempty }

---
--- Programs.
---

checkProgram :: MonadError AstError m => Env -> Program -> m ()
checkProgram env p@(Program datas defns top) = do
      checkDistinct $ uniqSites p
      checkDistinct [ (dataName d, dataAnnote d, "datatype name " <> dataName d) | d <- datas ]
      checkDistinct [ (c, an, "data constructor name " <> c) | d <- datas, DataCon an c _ <- dataCons d ]
      mapM_ (checkDataDefn env) datas
      mapM_ (checkDefn env) defns
      checkTop env defns top

-- | @top@ resolves to a definition, its occurrence signature matches the
--   binder's, and (in mono mode) the definition has the device type
--   @ReacT i o Identity a@ (doc/eidos.md §4.3; the result type is
--   unconstrained — a non-halting device never produces it).
checkTop :: MonadError AstError m => Env -> [Defn] -> Id -> m ()
checkTop env defns top = case [ d | d <- defns, idUniq (defnId d) == idUniq top ] of
      []    -> failAt noAnn $ "top: designated device root " <> prettyPrint top <> " does not name a definition"
      d : _ -> do
            checkOccSig (defnAnnote d) top $ defnId d
            when (envMode env >= LintMono) $ checkDeviceTy (defnAnnote d) $ sigTy $ idSig $ defnId d
      where checkDeviceTy :: MonadError AstError m => Annote -> Ty -> m ()
            checkDeviceTy an t = case flattenTyApp $ natNorm t of
                  (TyCon _ "ReacT", [_, _, TyCon _ "Identity", _]) -> pure ()
                  _ -> failAt an $ "the top definition must have type ReacT i o Identity a, not " <> prettyPrint t

---
--- Global binder uniqueness (doc/eidos.md §2, §4.4).
---

-- | Fold a list of keyed sites, reporting the first duplicate with both
--   locations.
checkDistinct :: forall k m. (MonadError AstError m, Eq k, Hashable k) => [(k, Annote, Text)] -> m ()
checkDistinct = foldM_ ins mempty
      where ins :: HashMap k (Annote, Text) -> (k, Annote, Text) -> m (HashMap k (Annote, Text))
            ins seen (k, an, what) = case Map.lookup k seen of
                  Just (an', what') -> failAtWith an ("duplicate " <> what)
                        [(an', "first introduced here: " <> what')] []
                  Nothing           -> pure $ Map.insert k (an, what) seen

-- | Every binding site in the program, in deterministic order: definition
--   names, signature type variables, parameters, all local binders, and
--   datatype parameters. Occurrences (which share their binder's unique)
--   contribute nothing.
uniqSites :: Program -> [(Uniq, Annote, Text)]
uniqSites (Program datas defns _) = concatMap dataSites datas <> concatMap defnSites defns
      where -- Constructors of one datatype share the datatype's parameter
            -- uniques ('checkDataDefn' enforces that their quantifier lists
            -- coincide), so only the first constructor's list contributes
            -- binding sites.
            dataSites :: DataDefn -> [(Uniq, Annote, Text)]
            dataSites d = case dataCons d of
                  DataCon an _ (Sig tvs _) : _ -> map (tvSite an "datatype parameter") tvs
                  []                           -> []

            defnSites :: Defn -> [(Uniq, Annote, Text)]
            defnSites (Defn an x ps body _ _) =
                  idSite an "definition name" x
                        : map (tvSite an "signature type variable") (sigTVs $ idSig x)
                        <> map (idSite an "parameter") ps
                        <> expSites body

            expSites :: Exp -> [(Uniq, Annote, Text)]
            expSites = \ case
                  Var {}             -> []
                  Con {}             -> []
                  Prim {}            -> []
                  LitInt {}          -> []
                  LitStr {}          -> []
                  LitList _ _ es     -> concatMap expSites es
                  LitVec _ _ es      -> concatMap expSites es
                  App _ e a          -> expSites e <> argSites a
                  Lam an x e         -> idSite an "lambda parameter" x : expSites e
                  Let an b e         -> bindSites an b <> expSites e
                  Jump _ _ es        -> concatMap expSites es
                  Case an _ e x alts -> expSites e <> (idSite an "case binder" x : concatMap altSites alts)

            argSites :: Arg -> [(Uniq, Annote, Text)]
            argSites = \ case
                  EArg e -> expSites e
                  TArg _ -> []

            bindSites :: Annote -> Bind -> [(Uniq, Annote, Text)]
            bindSites an = \ case
                  NonRec x e  -> idSite an "let binder" x : expSites e
                  Rec bs      -> concatMap (\ (x, e) -> idSite an "recursive let binder" x : expSites e) bs
                  Join j xs e -> idSite an "join point" (jpId j)
                        : map (idSite an "join point parameter") xs
                        <> expSites e

            altSites :: Alt -> [(Uniq, Annote, Text)]
            altSites (Alt an _ xs e) = map (idSite an "pattern binder") xs <> expSites e

            idSite :: Annote -> Text -> Id -> (Uniq, Annote, Text)
            idSite an what x = (idUniq x, an, "binding unique #" <> showt (idUniq x) <> " (" <> what <> " " <> prettyPrint x <> ")")

            tvSite :: Annote -> Text -> TyVar -> (Uniq, Annote, Text)
            tvSite an what v = (tvUniq v, an, "binding unique #" <> showt (tvUniq v) <> " (" <> what <> " " <> prettyPrint v <> ")")

---
--- Datatypes (doc/eidos.md §3.6, §4.3).
---

-- | The datatype's kind constructs @*@ from its parameter kinds; every
--   constructor quantifies exactly the datatype's parameters (the same
--   type variables, in the same order, across all constructors) and
--   constructs exactly the datatype applied to them.
checkDataDefn :: forall m. MonadError AstError m => Env -> DataDefn -> m ()
checkDataDefn env (DataDefn an t k cs) = do
      let (doms, kres) = kindSpine k
      unless (kres == KStar) $ failAt an $ "datatype " <> t <> ": kind must construct *"
      case cs of
            []                              -> pure ()
            DataCon _ _ (Sig tvs0 _) : _    -> mapM_ (checkCtor doms tvs0) cs
      where checkCtor :: [Kind] -> [TyVar] -> DataCon -> m ()
            checkCtor doms tvs0 (DataCon an' c (Sig tvs ty)) = do
                  unless (tvs == tvs0) $ failAt an'
                        $ "constructor " <> c <> ": quantified type variables differ across constructors of " <> t
                  unless (length tvs == length doms) $ failAt an'
                        $ "constructor " <> c <> ": quantifies " <> showt (length tvs)
                        <> " type variables but the kind of " <> t <> " has " <> showt (length doms) <> " parameters"
                  zipWithM_ (\ v kd -> unless (tvKind v == kd) $ failAt an'
                              $ "constructor " <> c <> ": the kind of type variable " <> prettyPrint v
                              <> " does not match the corresponding parameter of the kind of " <> t)
                        tvs doms
                  -- Datatypes stay parametric through specialization, so
                  -- constructor signatures see only the scoping rules (no
                  -- mono-mode closedness).
                  checkTyScope (env { envTVs = Map.fromList [ (tvUniq v, v) | v <- tvs ] }) an' ty
                  case flattenTyApp $ snd $ flattenArrow ty of
                        (TyCon _ t', args) | t' == t, args == map (TyVarT an') tvs -> pure ()
                        _ -> failAt an' $ "constructor " <> c <> " must construct " <> t
                              <> " applied to exactly its quantified type variables"

            kindSpine :: Kind -> ([Kind], Kind)
            kindSpine = \ case
                  KFun k1 k2 -> let (ks, r) = kindSpine k2 in (k1 : ks, r)
                  k'         -> ([], k')

---
--- Definitions (doc/eidos.md §3.5, §4.3).
---

-- | Parameters match a prefix of the signature's arrow spine; the body
--   checks against the remainder. In mono mode the signature quantifies
--   nothing — except the builtin-named definitions (rwPrim*), which are
--   the builtins' type assumptions riding to the retained pipeline as
--   polymorphic signature carriers (error-stub bodies, never referenced
--   as variables); they check in poly mode until the Eidos-level builtin
--   signature table lands.
checkDefn :: MonadError AstError m => Env -> Defn -> m ()
checkDefn env0 d@(Defn _ x0 _ _ _ _)
      | envMode env0 >= LintMono, Set.member (idOcc x0) primNames = checkDefn' (env0 { envMode = LintPoly }) d
      | otherwise                                                 = checkDefn' env0 d
      where primNames :: HashSet Text
            primNames = Set.fromList $ map fst builtins

checkDefn' :: MonadError AstError m => Env -> Defn -> m ()
checkDefn' env (Defn an x ps body _ _) = do
      let Sig tvs sigT = idSig x
      when (envMode env >= LintMono) $ unless (null tvs) $ failAt an
            $ "definition " <> prettyPrint x <> " has a polymorphic signature (mono mode)"
      let env' = env { envTVs = Map.fromList [ (tvUniq v, v) | v <- tvs ] }
      checkTy env' an sigT
      let (doms, res) = flattenArrow sigT
      when (length ps > length doms) $ failAt an
            $ "definition " <> prettyPrint x <> " has more parameters than its signature has arrows"
      zipWithM_ (checkParam env' an x) ps doms
      checkAgainst (foldr bindVar env' ps) body $ foldr (Arrow an) res $ drop (length ps) doms
      where checkParam :: MonadError AstError m => Env -> Annote -> Id -> Id -> Ty -> m ()
            checkParam env' an' f p dom = do
                  checkValueBinder env' an' "parameter" p
                  unless (tyEq (sigTy $ idSig p) dom) $ failAt an'
                        $ "parameter " <> prettyPrint p <> " of " <> prettyPrint f
                        <> ": type does not match the signature's arrow prefix (expected " <> prettyPrint dom <> ")"

---
--- Local binders.
---

-- | Rules common to every local binder: a monomorphic signature (§3.2) over
--   a well-scoped (and, in mono mode, closed) type.
checkLocalBinder :: MonadError AstError m => Env -> Annote -> Text -> Id -> m ()
checkLocalBinder env an what x = do
      let Sig tvs t = idSig x
      unless (null tvs) $ failAt an
            $ what <> " " <> prettyPrint x <> " has a polymorphic signature (local binders are monomorphic)"
      checkTy env an t

-- | A local *value* binder (parameter, lambda/let/case/pattern binder):
--   additionally first-order in mono+ANF mode (higher-order binders
--   survive specialization; the partial evaluator eliminates them before
--   the ANF stage). Join point labels are exempt — a label's signature is
--   its continuation's function type, and a label is not a value.
checkValueBinder :: MonadError AstError m => Env -> Annote -> Text -> Id -> m ()
checkValueBinder env an what x = do
      checkLocalBinder env an what x
      when (envMode env >= LintMonoANF && hasArrow (sigTy $ idSig x)) $ failAt an
            $ what <> " " <> prettyPrint x <> " has a function type (higher-order binders are not representable past the ANF stage)"
      where hasArrow :: Ty -> Bool
            hasArrow = \ case
                  Arrow {}    -> True
                  TyApp _ a b -> hasArrow a || hasArrow b
                  _           -> False

---
--- Expressions (doc/eidos.md §4.2): synthesis with all checks inline.
---

-- | Check an expression and return its type — the located, monadic twin of
--   'ReWire.Eidos.Types.typeOf'.
checkExp :: MonadError AstError m => Env -> Exp -> m Ty
checkExp env = \ case
      e@App {}           -> checkSpine env e
      Var an x           -> do
            xB <- lookupVar env an x
            unless (null $ sigTVs $ idSig xB) $ failAt an
                  $ "unsaturated reference to polymorphic " <> prettyPrint x
                  <> " (type arguments must saturate the quantifier list)"
            pure $ sigTy $ idSig xB
      Con an t c         -> do
            checkTy env an t
            checkCon env an t c
            pure t
      Prim an t _        -> do
            -- No signature table for builtins exists at the Eidos level;
            -- the carried instantiated type is trusted.
            checkTy env an t
            pure t
      LitInt an t n      -> do
            checkTy env an t
            checkLitInt env an t n
            pure t
      LitStr an _        -> pure $ TyCon an "String"
      LitList an t es    -> do
            checkTy env an t
            case dstListTy t of
                  Just et -> mapM_ (\ e -> checkAgainst (nonTail env) e et) es
                  Nothing -> failAt an $ "list literal at a non-list type: " <> prettyPrint t
            pure t
      LitVec an t es     -> do
            checkTy env an t
            case flattenTyApp $ natNorm t of
                  (TyCon _ "Vec", [n, et]) -> do
                        mapM_ (\ e -> checkAgainst (nonTail env) e et) es
                        case evalNat n of
                              Just k  -> unless (k == fromIntegral (length es)) $ failAt an
                                    $ "vector literal has " <> showt (length es) <> " elements but type " <> prettyPrint t
                              Nothing -> pure () -- open length: mono mode rejects the type instead.
                  _ -> failAt an $ "vector literal at a non-Vec type: " <> prettyPrint t
            pure t
      Lam an x e         -> do
            checkValueBinder env an "lambda parameter" x
            Arrow an (sigTy $ idSig x) <$> checkExp (bindVar x $ nonTail env) e
      Let an b e         -> checkLet env an b e
      Jump an j es       -> checkJump env an j es
      Case an t e x alts -> checkCase env an t e x alts

-- | Check an expression against an expected type: synthesize and compare
--   after 'natNorm'.
checkAgainst :: MonadError AstError m => Env -> Exp -> Ty -> m ()
checkAgainst env e t = do
      t' <- checkExp env e
      unless (tyEq t t') $ failAt (ann e)
            $ "expression has type " <> prettyPrint t' <> " but " <> prettyPrint t <> " is expected"

-- | The spine discipline (§4.2): type arguments only on 'Var' heads, in
--   prefix position, saturating the head's quantifier list; then one arrow
--   peeled per term argument, each argument checking against its domain.
checkSpine :: forall m. MonadError AstError m => Env -> Exp -> m Ty
checkSpine env e = do
      let (h, args)  = flattenApp e
          (tas, eas) = span isTArg args
          tys        = [ t | TArg t <- tas ]
      when (any isTArg eas) $ failAt (ann e) "type arguments must form a prefix of the application spine"
      mapM_ (checkTy env $ ann e) tys
      ht <- case h of
            Var an x | not $ null tys -> do
                  xB <- lookupVar env an x
                  let sig = idSig xB
                  unless (length (sigTVs sig) == length tys) $ failAt an
                        $ prettyPrint x <> " expects " <> showt (length $ sigTVs sig)
                        <> " type arguments, applied to " <> showt (length tys)
                  pure $ instantiate sig tys
            _ | not $ null tys        -> failAt (ann e) "type argument applied to a non-variable head"
              | otherwise             -> checkExp (nonTail env) h
      foldM app ht eas
      where app :: Ty -> Arg -> m Ty
            app t = \ case
                  EArg a -> do
                        ta <- checkExp (nonTail env) a
                        case t of
                              Arrow _ dom cod -> do
                                    unless (tyEq dom ta) $ failAt (ann a)
                                          $ "argument has type " <> prettyPrint ta
                                          <> " but the function expects " <> prettyPrint dom
                                    pure cod
                              _               -> failAt (ann a)
                                    $ "term argument applied to a non-arrow (head type " <> prettyPrint t <> ")"
                  TArg _ -> failAt (ann e) "type arguments must form a prefix of the application spine"

            isTArg :: Arg -> Bool
            isTArg = \ case
                  TArg _ -> True
                  _      -> False

-- | A 'Var' occurrence: bound in scope, with the binder's signature (§4.4).
--   Returns the binder.
lookupVar :: MonadError AstError m => Env -> Annote -> Id -> m Id
lookupVar env an x = case Map.lookup (idUniq x) $ envScope env of
      Just xB -> checkOccSig an x xB >> pure xB
      Nothing
            | Map.member (idUniq x) $ envJoins env -> failAt an
                  $ "join point " <> prettyPrint x <> " used as a value (labels may only be jump targets)"
            | otherwise                            -> failAt an $ "unbound variable: " <> prettyPrint x

-- | An occurrence's signature equals its binder's: the same quantified
--   variables (uniques and kinds) over 'natNorm'-structurally equal types.
checkOccSig :: MonadError AstError m => Annote -> Id -> Id -> m ()
checkOccSig an occ bnd = do
      let Sig tvs t   = idSig occ
          Sig tvs' t' = idSig bnd
      unless (tvs == tvs' && map tvKind tvs == map tvKind tvs' && tyEq t t') $ failAt an
            $ "occurrence of " <> prettyPrint occ <> " carries signature " <> prettyPrint (idSig occ)
            <> " but its binder carries " <> prettyPrint (idSig bnd)

-- | A 'Con' occurrence's carried type instantiates its signature: the
--   instantiation is read off the carried type's result by first-order
--   matching against @T as@ (§3.6), then the whole type must agree.
checkCon :: MonadError AstError m => Env -> Annote -> Ty -> DataConId -> m ()
checkCon env an t c = do
      (tcon, sig) <- lookupCon env an c
      let (_, res) = flattenArrow t
      fields      <- dconFieldTys an c tcon sig res
      unless (tyEq t $ foldr (Arrow an) res fields) $ failAt an
            $ "constructor " <> c <> " at type " <> prettyPrint t
            <> " does not instantiate its signature " <> prettyPrint sig

lookupCon :: MonadError AstError m => Env -> Annote -> DataConId -> m (TyConId, Sig)
lookupCon env an c = maybe (failAt an $ "unknown data constructor: " <> c) pure $ Map.lookup c $ envCons env

-- | The instantiated field types of a constructor at a fully-applied
--   datatype type @T ts@ (the scrutinee's type, or a 'Con' occurrence's
--   result type).
dconFieldTys :: MonadError AstError m => Annote -> DataConId -> TyConId -> Sig -> Ty -> m [Ty]
dconFieldTys an c tcon (Sig tvs sigT) t = case flattenTyApp $ natNorm t of
      (TyCon _ t', args) | t' == tcon ->
            if length args == length tvs
                  then pure $ map (substTv $ Map.fromList $ zip tvs args) $ fst $ flattenArrow sigT
                  else failAt an $ "constructor " <> c <> ": datatype " <> tcon
                        <> " applied to " <> showt (length args) <> " arguments (expected " <> showt (length tvs) <> ")"
      _ -> failAt an $ "constructor " <> c <> " of datatype " <> tcon <> " used at incompatible type " <> prettyPrint t

---
--- Bindings and the join point discipline (doc/eidos.md §3.4, §4.2).
---

checkLet :: MonadError AstError m => Env -> Annote -> Bind -> Exp -> m Ty
checkLet env an b body = case b of
      NonRec x e  -> do
            checkValueBinder env an "let binder" x
            checkAgainst (nonTail env) e $ sigTy $ idSig x
            checkExp (bindVar x env) body
      Rec bs      -> do
            mapM_ (checkValueBinder env an "recursive let binder" . fst) bs
            let env' = foldr (bindVar . fst) env bs
            mapM_ (\ (x, e) -> checkAgainst (nonTail env') e $ sigTy $ idSig x) bs
            checkExp env' body
      Join j xs e -> do
            let x = jpId j
            checkLocalBinder env an "join point" x
            mapM_ (checkValueBinder env an "join point parameter") xs
            unless (length xs == jpArity j) $ failAt an
                  $ "join point " <> prettyPrint x <> " declares arity " <> showt (jpArity j)
                  <> " but binds " <> showt (length xs) <> " parameters"
            let (doms, res) = flattenArrow $ sigTy $ idSig x
            unless (length doms >= jpArity j) $ failAt an
                  $ "join point " <> prettyPrint x <> ": signature has fewer arrows than its arity"
            let ptys  = take (jpArity j) doms
                resTy = foldr (Arrow an) res $ drop (jpArity j) doms
                jd    = JoinDef j ptys resTy
            zipWithM_ (\ p pt -> unless (tyEq (sigTy $ idSig p) pt) $ failAt an
                        $ "join point parameter " <> prettyPrint p
                        <> ": type does not match the join point's signature (expected " <> prettyPrint pt <> ")")
                  xs ptys
            -- The join body and the scope check against the same type (the
            -- join's result is the scope's result). Outer joins remain
            -- jumpable from the body's tail (it is transitively a tail of
            -- their scopes); the join itself is not (no recursion).
            checkAgainst (foldr bindVar (scopeJoin x jd env) xs) e resTy
            tb <- checkExp (bindJoin x jd env) body
            unless (tyEq tb resTy) $ failAt an
                  $ "the scope of join point " <> prettyPrint x <> " has type " <> prettyPrint tb
                  <> " but the join point returns " <> prettyPrint resTy
            pure tb

-- | A jump: to a join point bound in an enclosing let, from tail position
--   of that join's scope, saturating its arity, each argument checking
--   against the corresponding parameter type.
checkJump :: MonadError AstError m => Env -> Annote -> JoinId -> [Exp] -> m Ty
checkJump env an j args = case Map.lookup (idUniq $ jpId j) $ envJoins env of
      Just (JoinDef jB ptys res) -> do
            unless (idUniq (jpId j) `Set.member` envTail env) $ failAt an
                  $ "jump to join point " <> prettyPrint (jpId j)
                  <> " outside the tail of its scope (jumps are tail-only, and join points are not recursive)"
            checkOccSig an (jpId j) $ jpId jB
            unless (jpArity j == jpArity jB) $ failAt an
                  $ "jump to " <> prettyPrint (jpId j) <> " carries arity " <> showt (jpArity j)
                  <> " but the join point declares " <> showt (jpArity jB)
            unless (length args == jpArity jB) $ failAt an
                  $ "jump to " <> prettyPrint (jpId j) <> " supplies " <> showt (length args)
                  <> " arguments (arity " <> showt (jpArity jB) <> ")"
            zipWithM_ (checkAgainst $ nonTail env) args ptys
            pure res
      Nothing
            | Map.member (idUniq $ jpId j) $ envScope env -> failAt an
                  $ "jump target " <> prettyPrint (jpId j) <> " is a value binder, not a join point"
            | otherwise                                   -> failAt an
                  $ "jump to unbound join point " <> prettyPrint (jpId j) <> " (labels do not escape their scope)"

---
--- Case expressions (doc/eidos.md §4.2).
---

checkCase :: MonadError AstError m => Env -> Annote -> Ty -> Exp -> Id -> [Alt] -> m Ty
checkCase env an t scrut x alts = do
      checkTy env an t
      ts <- checkExp (nonTail env) scrut
      checkValueBinder env an "case binder" x
      unless (tyEq (sigTy $ idSig x) ts) $ failAt an
            $ "case binder " <> prettyPrint x <> " has type " <> prettyPrint (sigTy $ idSig x)
            <> " but the scrutinee has type " <> prettyPrint ts
      when (null alts) $ failAt an "case expression with no alternatives"
      case [ an' | Alt an' DefaultAlt _ _ <- drop 1 alts ] of
            an' : _ -> failAt an' "the default case alternative must come first"
            []      -> pure ()
      checkDistinct [ (c, an', "case alternative for constructor " <> c) | Alt an' (DataAlt c) _ _ <- alts ]
      checkDistinct [ (n, an', "case alternative for literal " <> showt n) | Alt an' (LitAlt n) _ _ <- alts ]
      mapM_ (checkAlt (bindVar x env) t ts) alts
      pure t

-- | One alternative: fields bound at the constructor's instantiated field
--   types; the body (a tail position) checks against the carried result
--   type.
checkAlt :: MonadError AstError m => Env -> Ty -> Ty -> Alt -> m ()
checkAlt env t ts (Alt an con xs body) = case con of
      DefaultAlt -> do
            unless (null xs) $ failAt an "default case alternative binds fields"
            checkAgainst env body t
      LitAlt n   -> do
            unless (null xs) $ failAt an "literal case alternative binds fields"
            case litRep ts of
                  RepBad -> failAt an $ "literal case alternative on a scrutinee of type " <> prettyPrint ts
                        <> " (must be Integer, a bit vector, or Finite)"
                  rep    -> when (envMode env >= LintMono) $ unless (fitsRep rep n) $ failAt an
                        $ "literal " <> showt n <> " is not representable at the scrutinee type " <> prettyPrint ts
            checkAgainst env body t
      DataAlt c  -> do
            (tcon, sig) <- lookupCon env an c
            fields      <- dconFieldTys an c tcon sig ts
            unless (length xs == length fields) $ failAt an
                  $ "case alternative for " <> c <> " binds " <> showt (length xs)
                  <> " fields (the constructor has " <> showt (length fields) <> ")"
            mapM_ (checkValueBinder env an "pattern binder") xs
            zipWithM_ (\ p ft -> unless (tyEq (sigTy $ idSig p) ft) $ failAt an
                        $ "pattern binder " <> prettyPrint p
                        <> ": type does not match the constructor's field type " <> prettyPrint ft)
                  xs fields
            checkAgainst (foldr bindVar env xs) body t

---
--- Integer literals (doc/eidos.md §4.2): representability, mono mode only
--- (widths may be open in poly mode).
---

data LitRep = RepInteger | RepBits !Natural | RepFinite !Natural | RepOpen | RepBad

litRep :: Ty -> LitRep
litRep t = case flattenTyApp $ natNorm t of
      (TyCon _ "Integer", [])              -> RepInteger
      (TyCon _ "Vec", [w, TyCon _ "Bool"]) -> maybe RepOpen RepBits $ evalNat w
      (TyCon _ "Finite", [w])              -> maybe RepOpen RepFinite $ evalNat w
      _                                    -> RepBad

fitsRep :: LitRep -> Integer -> Bool
fitsRep rep n = case rep of
      RepInteger  -> True
      RepOpen     -> True -- unreachable in mono mode (types are nat-closed).
      RepBits w   | n >= 0    -> n < 2 ^ w
                  | otherwise -> w > 0 && n >= negate (2 ^ (w - 1))
      RepFinite w -> n >= 0 && n < toInteger w
      RepBad      -> False

checkLitInt :: MonadError AstError m => Env -> Annote -> Ty -> Integer -> m ()
checkLitInt env an t n = when (envMode env >= LintMono) $ case litRep t of
      RepBad -> failAt an $ "integer literal at unrepresentable type " <> prettyPrint t
            <> " (must be Integer, a bit vector, or Finite)"
      rep    -> unless (fitsRep rep n) $ failAt an
            $ "literal " <> showt n <> " is not representable at type " <> prettyPrint t

---
--- Types.
---

-- | Structural equality after normalization — the compiler-wide notion of
--   type equality (doc/eidos.md §5).
tyEq :: Ty -> Ty -> Bool
tyEq t t' = natNorm t == natNorm t'

-- | Scoping (every type variable bound, at its binder's kind) plus, in
--   mono mode, closedness.
checkTy :: MonadError AstError m => Env -> Annote -> Ty -> m ()
checkTy env an t = do
      checkTyScope env an t
      when (envMode env >= LintMono) $ checkClosed an $ natNorm t

checkTyScope :: MonadError AstError m => Env -> Annote -> Ty -> m ()
checkTyScope env an = go
      where go :: MonadError AstError m => Ty -> m ()
            go = \ case
                  TyVarT _ v    -> case Map.lookup (tvUniq v) $ envTVs env of
                        Just v' | tvKind v' == tvKind v -> pure ()
                                | otherwise             -> failAt an
                              $ "type variable " <> prettyPrint v <> ": occurrence kind does not match its binder's"
                        Nothing                         -> failAt an $ "unbound type variable: " <> prettyPrint v
                  TyApp _ t1 t2 -> go t1 >> go t2
                  Arrow _ t1 t2 -> go t1 >> go t2
                  _             -> pure ()

-- | On a 'natNorm'-normalized type: no type variables, and no residual
--   type-level arithmetic (every nat-closed subterm has already been folded
--   to a literal, so any surviving arithmetic constructor is open).
checkClosed :: MonadError AstError m => Annote -> Ty -> m ()
checkClosed an = go
      where go :: MonadError AstError m => Ty -> m ()
            go = \ case
                  TyVarT _ v    -> failAt an $ "type variable " <> prettyPrint v <> " in mono mode (types must be closed)"
                  TyCon _ c     | c `elem` (["+", "-", "*"] :: [Text]) -> failAt an
                        "type-level arithmetic does not evaluate to a literal (types must be nat-closed in mono mode)"
                  TyApp _ t1 t2 -> go t1 >> go t2
                  Arrow _ t1 t2 -> go t1 >> go t2
                  _             -> pure ()

-- | The list type constructor applied to an element type. The bridge is
--   not yet landed, so both the Crust convention (@[_]@) and the source
--   spelling (@[]@) are accepted here.
dstListTy :: Ty -> Maybe Ty
dstListTy = \ case
      TyApp _ (TyCon _ c) et | c == "[_]" || c == "[]" -> Just et
      _                                                -> Nothing

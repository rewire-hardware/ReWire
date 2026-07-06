{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The Eidos-P -> Crust shim: lowers an Eidos program onto the retained
--   Crust pipeline, reproducing the encodings the deleted Core->Crust
--   bridge tail produced, so the Eidos front end can be validated
--   differentially against the regular path while Crust passes remain in
--   service. The shim moves down the pipeline as passes are ported to
--   Eidos, and dies with the Crust tail.
--
--   Encodings reproduced here (see ReWire.GHC.ToCrust for their rationale):
--
--   * Lets lower to beta-redexes -- NOT single-arm cases -- with nested
--     telescopes re-scheduled in demand order first ('scheduleLets': GHC's
--     desugarer clusters let groups in SCC order, which destroys source
--     interleaving and makes partial evaluation grind). Join points join
--     the telescopes as ordinary bindings (their lambdas are lifted and
--     shared by the retained liftLambdas), and jumps become applications.
--
--   * The n-ary case re-crushes to Crust's 1-2-alt cascade over a
--     beta-redex-shared scrutinee, with the two exponential-avoidance
--     conventions: an exhaustive alternative list's final binder-free
--     constructor alternative becomes the innermost default, and the
--     Core-ordered leading default becomes the innermost arm.
--
--   * The input is monomorphic (specialization and INLINE inlining have
--     run at the Eidos level), so every lowered type slot is concrete and
--     the type annotations (tyAnn) pinned on heads and spines are exactly
--     the concrete ones the retained pipeline's annotation purge would
--     keep. Integer literals get their types pinned as annotations too —
--     Crust literal nodes have no type slot, nothing downstream re-infers,
--     and the widths are not otherwise recoverable.
--
--   * The output enters the retained pipeline at the extern-neutering
--     pass, with the Crust primitive basis applied here (a retained-tail
--     requirement: the R_/A_/PuRe placeholders and friends).
module ReWire.Eidos.ToCrust (eidosToCrust, lowerExp, lowerTy, Locals, bindLocals) where

import ReWire.Annotation (Annote)
import ReWire.Builtins (Builtin (VecFromList))
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Crust.PrimBasis (addPrims)
import ReWire.Crust.Types (poly, poly', arr, plusTy, negTy, concrete, reacOrStateT, setTyAnn)
import ReWire.Crust.Util (mkApp, mkError)
import ReWire.Eidos.Naming (liftedJoinName)
import ReWire.Eidos.Subst (nextUniq, freeUniqs, occIds)
import ReWire.Eidos.Syntax
import ReWire.Eidos.Types (typeOf, flattenApp, instantiate)
import ReWire.Unbound (Fresh, fresh, s2n, bind, Embed (..), Name)

import qualified ReWire.Crust.Syntax as M

import Control.Monad (foldM)
import Control.Monad.State.Strict (State, runState, get, put)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)

import qualified Data.IntMap.Strict  as IM
import qualified Data.IntSet         as IS

-- | Lower a lint-clean, monomorphic Eidos-P program to a Crust
--   FreeProgram with the Crust primitive basis applied, ready to enter the
--   retained pipeline. Join points are lifted first, to top-level
--   definitions with occurrence-stable names (ReWire.Eidos.Naming): the
--   retained purify keys resumption states by continuation name, so
--   continuations must reach it as named definitions, and stable names
--   keep regenerated goldens from churning. (This lift belongs to the
--   shim epoch: the machine-level pipeline consumes joins directly.)
eidosToCrust :: (Fresh m, MonadError AstError m) => Program -> m M.FreeProgram
eidosToCrust (liftJoins -> Program datas defns _procs _top) = do
      let tops = IS.fromList $ map (idUniq . defnId) defns
      ds <- mapM (lowerDefn tops) defns
      pure $ addPrims (map lowerData datas, [], ds)

---
--- Lifting join points to occurrence-stably named definitions.
---

data LSt = LSt { lsSup :: !Uniq, lsOrd :: !Int, lsNew :: ![Defn] }

liftJoins :: Program -> Program
liftJoins p@(Program datas defns procs top) = Program datas defns' procs top
      where tops :: IS.IntSet
            tops = IS.fromList $ map (idUniq . defnId) defns

            -- Uniques at or above the initial supply are lifted
            -- definitions, not capturable locals.
            sup0 :: Uniq
            sup0 = nextUniq p

            defns' :: [Defn]
            defns' = fst $ foldl step ([], sup0) defns

            step :: ([Defn], Uniq) -> Defn -> ([Defn], Uniq)
            step (acc, sup) d =
                  let (b', st) = runState (lj (idOcc $ defnId d) (defnBody d)) $ LSt sup 1 []
                  in (acc <> (d { defnBody = b' } : reverse (lsNew st)), lsSup st)

            lj :: Text -> Exp -> State LSt Exp
            lj focc = go
                  where go :: Exp -> State LSt Exp
                        go e = case e of
                              Let an (Join j ps b) body -> do
                                    b' <- go b
                                    let capUs = IS.filter (< sup0) (freeUniqs b' IS.\\ tops)
                                                      IS.\\ IS.fromList (map idUniq ps)
                                        caps  = mapMaybe (`IM.lookup` occIds b') $ IS.toList capUs
                                        t     = foldr (Arrow an . sigTy . idSig) (typeOf b') $ caps <> ps
                                    st <- get
                                    let u  = lsSup st
                                        i  = lsOrd st
                                        x  = Id { idOcc  = liftedJoinName focc (idOcc $ jpId j) i
                                                , idUniq = u
                                                , idSig  = Sig [] t
                                                }
                                    put st { lsSup = u + 1, lsOrd = i + 1
                                           , lsNew = Defn an x (caps <> ps) b' Nothing Nothing : lsNew st }
                                    go $ rejump (idUniq $ jpId j) x caps body
                              App an f a       -> App an <$> go f <*> goArg a
                              Lam an x b       -> Lam an x <$> go b
                              Let an b body    -> Let an <$> goBind b <*> go body
                              Jump an j es     -> Jump an j <$> mapM go es
                              Case an t s cb alts -> do
                                    s'    <- go s
                                    alts' <- mapM (\ (Alt aan c xs b) -> Alt aan c xs <$> go b) alts
                                    pure $ Case an t s' cb alts'
                              LitList an t es  -> LitList an t <$> mapM go es
                              LitVec an t es   -> LitVec an t <$> mapM go es
                              _                -> pure e

                        goArg :: Arg -> State LSt Arg
                        goArg = \ case
                              EArg e -> EArg <$> go e
                              t      -> pure t

                        goBind :: Bind -> State LSt Bind
                        goBind = \ case
                              NonRec x rhs -> NonRec x <$> go rhs
                              Rec bs       -> Rec <$> mapM (\ (x, rhs) -> (x, ) <$> go rhs) bs
                              Join j ps b  -> Join j ps <$> go b

            -- Rewrite every jump to the lifted join into a call, deeply
            -- (jumps to an outer join from a transitively-tail position
            -- sit inside nested join bindings' bodies too).
            rejump :: Uniq -> Id -> [Id] -> Exp -> Exp
            rejump ju x caps = go
                  where go :: Exp -> Exp
                        go e = case e of
                              Jump an j es
                                    | idUniq (jpId j) == ju ->
                                          foldl (App an) (Var an x) $ map (EArg . Var an) caps <> map (EArg . go) es
                                    | otherwise             -> Jump an j $ map go es
                              App an f a       -> App an (go f) $ goArg a
                              Lam an y b       -> Lam an y $ go b
                              Let an b body    -> Let an (goBind b) $ go body
                              Case an t s cb alts -> Case an t (go s) cb [ Alt aan c xs (go b) | Alt aan c xs b <- alts ]
                              LitList an t es  -> LitList an t $ map go es
                              LitVec an t es   -> LitVec an t $ map go es
                              _                -> e

                        goArg :: Arg -> Arg
                        goArg = \ case
                              EArg e -> EArg $ go e
                              t      -> t

                        goBind :: Bind -> Bind
                        goBind = \ case
                              NonRec y rhs -> NonRec y $ go rhs
                              Rec bs       -> Rec [ (y, go rhs) | (y, rhs) <- bs ]
                              Join j ps b  -> Join j ps $ go b

---
--- Names and types.
---

-- | Crust names for Eidos ids: top-level names are the occurrence text
--   (stable, dotted); local names are freshened from the occurrence text
--   and memoized per unique by the lowering context.
topName :: Id -> Name M.Exp
topName = s2n . idOcc

-- | Local bindings in scope during expression lowering: Eidos unique to
--   the (freshened) Crust expression that replaces occurrences. Exported
--   for the machine-level adapter, which lowers block bodies with its own
--   parameter and store bindings.
type Locals = IM.IntMap M.Exp

-- | Bind locals for lowering: fresh Crust names, occurrences mapped.
bindLocals :: Fresh m => Annote -> Locals -> [Id] -> m ([Name M.Exp], Locals)
bindLocals an = go []
      where go :: Fresh m => [Name M.Exp] -> Locals -> [Id] -> m ([Name M.Exp], Locals)
            go acc lcl []       = pure (reverse acc, lcl)
            go acc lcl (x : xs) = do
                  n <- fresh $ s2n $ idOcc x
                  let t = Just $ lowerTy $ sigTy $ idSig x
                  go (n : acc) (IM.insert (idUniq x) (M.Var an Nothing t n) lcl) xs

lowerTy :: Ty -> M.Ty
lowerTy = \ case
      TyCon an c
            | c == "[]" -> M.TyCon an $ s2n "[_]"
            | otherwise -> M.TyCon an $ s2n c
      TyApp an (TyApp _ (TyCon _ "-") a) b
                        -> plusTy an (lowerTy a) $ negTy $ lowerTy b
      TyApp an t u      -> M.TyApp an (lowerTy t) (lowerTy u)
      TyVarT an v       -> M.TyVar an (lowerKind $ tvKind v) $ s2n $ tvOcc v
      TyNat an n        -> M.TyNat an n
      Arrow _ t u       -> arr (lowerTy t) (lowerTy u)

lowerKind :: Kind -> M.Kind
lowerKind = \ case
      KStar     -> M.KStar
      KNat      -> M.KNat
      KFun a b  -> M.KFun (lowerKind a) (lowerKind b)

lowerSig :: Sig -> M.Poly
lowerSig (Sig tvs t) = poly (map (s2n . tvOcc) tvs) $ lowerTy t

---
--- Definitions and datatypes.
---

lowerDefn :: (Fresh m, MonadError AstError m) => IS.IntSet -> Defn -> m M.Defn
lowerDefn tops (Defn an x params body attr _) = do
      body' <- lowerExp tops mempty $ foldr (Lam an) body params
      pure M.Defn
            { M.defnAnnote = an
            , M.defnName   = topName x
            , M.defnPolyTy = Embed $ lowerSig $ idSig x
            , M.defnAttr   = lowerAttr <$> attr
            , M.defnBody   = Embed $ bind [] body'
            }
      where lowerAttr :: DefnAttr -> M.DefnAttr
            lowerAttr = \ case
                  Inline   -> M.Inline
                  NoInline -> M.NoInline

lowerData :: DataDefn -> M.DataDefn
lowerData (DataDefn an n k cs) = M.DataDefn an (s2n n) (lowerKind k) $ map lowerCon cs
      where lowerCon :: DataCon -> M.DataCon
            lowerCon (DataCon a c sig) = M.DataCon a (s2n c) $ Embed $ lowerSig sig

---
--- Expressions.
---

lowerExp :: (Fresh m, MonadError AstError m) => IS.IntSet -> Locals -> Exp -> m M.Exp
lowerExp tops locals e0 = go locals e0
      where go :: (Fresh m, MonadError AstError m) => Locals -> Exp -> m M.Exp
            go lcl e = case e of
                  App {}    -> spine lcl e
                  Var {}    -> spine lcl e
                  Jump {}   -> spine lcl e
                  Con {}    -> spine lcl e
                  Prim {}   -> spine lcl e
                  LitInt {} -> spine lcl e

                  LitStr an s     -> pure $ M.LitStr an Nothing s
                  LitList an t es -> M.LitList an Nothing (Just $ lowerTy t) <$> mapM (go lcl) es
                  LitVec an t es  -> M.LitVec an Nothing (Just $ lowerTy t) <$> mapM (go lcl) es

                  Lam an x b -> do
                        (n, lcl') <- bindLocal an lcl x
                        b'        <- go lcl' b
                        pure $ M.Lam an Nothing (Just $ lowerTy $ sigTy $ idSig x) $ bind n b'

                  Let an _ _ -> lowerLets lcl an e

                  Case an t scrut cb alts -> lowerCase lcl an t scrut cb alts

            -- Application spines (and bare heads): erase type arguments,
            -- annotate the head and (when applied) the outermost node with
            -- their instantiated types. A saturated fromList of a list
            -- literal becomes a vector literal (the retained typechecker's
            -- embedded rule, reproduced here while the shim enters the
            -- pipeline downstream of it).
            spine :: (Fresh m, MonadError AstError m) => Locals -> Exp -> m M.Exp
            spine lcl e = do
                  let (h, args)  = flattenApp e
                      eas        = [ a | EArg a <- args ]
                  case (h, eas) of
                        (Prim an _ p, [arg']) | p == VecFromList -> case arg' of
                              LitList _ _ els -> M.LitVec an Nothing (Just $ lowerTy $ typeOf e) <$> mapM (go lcl) els
                              _               -> failAt an "fromList: argument not a list literal."
                        _ -> do
                              h'  <- lowerHead lcl h [ t | TArg t <- args ]
                              as' <- mapM (go lcl) eas
                              -- Rebuilt spine nodes carry the spine's own
                              -- annotation (the use site), not the head's:
                              -- backend diagnostics read it (e.g. extern
                              -- errors must point at the extern's use, not
                              -- at the inlined rewire-user wrapper).
                              let app = mkApp (annOf e) h' as'
                              if null eas then pure app else do
                                    let t = lowerTy $ typeOf e
                                    pure $ maybeAnn t app

            lowerHead :: (Fresh m, MonadError AstError m) => Locals -> Exp -> [Ty] -> m M.Exp
            lowerHead lcl h targs = case h of
                  Var an x
                        | Just e' <- IM.lookup (idUniq x) lcl -> pure e'
                        | idUniq x `IS.member` tops -> do
                              let t = lowerTy $ instTy (idSig x) targs
                              pure $ maybeAnn t $ M.Var an Nothing (Just t) $ topName x
                        | otherwise -> failAt an "eidos-shim: unbound local variable (rwc bug)."
                  Jump an j args -> do
                        -- A jump is an application of the join's binding
                        -- (annotated like any other application).
                        jv  <- lowerHead lcl (Var an $ jpId j) []
                        as' <- mapM (go lcl) args
                        let app = mkApp an jv as'
                        if null args then pure app
                        else pure $ maybeAnn (lowerTy $ typeOf h) app
                  Con an t c  -> do
                        let t' = lowerTy t
                        pure $ maybeAnn t' $ M.Con an Nothing (Just t') $ s2n c
                  Prim an t p -> do
                        let t' = lowerTy t
                        pure $ maybeAnn t' $ M.Builtin an Nothing (Just t') p
                  -- Pin the literal's type: Crust literal nodes have no
                  -- type slot, and nothing downstream re-infers widths.
                  LitInt an t n -> pure $ maybeAnn (lowerTy t) $ M.LitInt an Nothing n
                  _ -> go lcl h
                  where instTy :: Sig -> [Ty] -> Ty
                        instTy sig ts
                              | null ts   = sigTy sig
                              | otherwise = instantiate sig ts

            -- Let telescopes: collect the maximal chain (join bindings
            -- become ordinary lambda bindings; recursive groups are
            -- unsupported, as on the deleted path), re-schedule in demand
            -- order, and lower to beta-redexes.
            lowerLets :: (Fresh m, MonadError AstError m) => Locals -> Annote -> Exp -> m M.Exp
            lowerLets lcl an e = do
                  (bs, body) <- collectLets an e
                  -- Bind all the names first (a later binding's rhs may
                  -- reference an earlier one).
                  (lcl', n2e) <- foldM (\ (l, acc) (x, _) -> do
                              (n, l') <- bindLocal an l x
                              pure (l', acc <> [(idUniq x, n)]))
                        (lcl, []) bs
                  body'  <- go lcl' body
                  let bodyTy = Just $ lowerTy $ typeOf body
                      order  = scheduleLets (map fst bs) body $ map snd bs
                  foldM (\ acc i -> do
                              let (x, rhs) = bs !! i
                                  n        = fromMaybe (error "eidos-shim: scheduleLets") $ lookup (idUniq x) n2e
                                  t        = Just $ lowerTy $ sigTy $ idSig x
                              rhs' <- go lcl' rhs
                              pure $ M.App an Nothing bodyTy (M.Lam an Nothing t $ bind n acc) rhs')
                        body' order

            collectLets :: MonadError AstError m => Annote -> Exp -> m ([(Id, Exp)], Exp)
            collectLets an = \ case
                  Let _ (NonRec x r) e    -> do
                        (bs, body) <- collectLets an e
                        pure ((x, r) : bs, body)
                  Let a (Join j ps r) e  -> do
                        (bs, body) <- collectLets an e
                        pure ((jpId j, foldr (Lam a) r ps) : bs, body)
                  Let _ (Rec _) _         -> failAt an "eidos-shim: unsupported local recursive binding."
                  e                       -> pure ([], e)

            -- The n-ary case re-crushed to the 1-2-alt cascade (see the
            -- module comment). The case binder shares the scrutinee via a
            -- beta-redex when there is more than one alternative.
            lowerCase :: (Fresh m, MonadError AstError m) => Locals -> Annote -> Ty -> Exp -> Id -> [Alt] -> m M.Exp
            lowerCase lcl an t scrut cb alts = do
                  scrut' <- go lcl scrut
                  let resTy   = Just $ lowerTy t
                      scrutTy = Just $ lowerTy $ sigTy $ idSig cb
                  case alts of
                        [] -> pure $ mkError an resTy "Pattern match failure: non-exhaustive patterns in case"
                        [Alt _ DefaultAlt [] rhs] -> do
                              (n, lcl') <- bindLocal an lcl cb
                              rhs'      <- go lcl' rhs
                              pure $ M.App an Nothing resTy (M.Lam an Nothing scrutTy $ bind n rhs') scrut'
                        [Alt aan (DataAlt c) xs rhs] -> do
                              let lcl0 = IM.insert (idUniq cb) scrut' lcl
                              (pat, lcl') <- lowerPat aan lcl0 (sigTy $ idSig cb) c xs
                              rhs'        <- go lcl' rhs
                              pure $ M.Case an Nothing resTy scrut' (bind pat rhs') Nothing
                        _ -> do
                              (n, lcl') <- bindLocal an lcl cb
                              let sv = fromMaybe (error "eidos-shim: case binder") $ IM.lookup (idUniq cb) lcl'
                              body <- case alts of
                                    (Alt _ DefaultAlt [] rhs : rest) -> do
                                          mdef <- Just <$> go lcl' rhs
                                          cascade lcl' an sv (sigTy $ idSig cb) resTy mdef rest
                                    rest -> cascade lcl' an sv (sigTy $ idSig cb) resTy Nothing rest
                              pure $ M.App an Nothing resTy (M.Lam an Nothing scrutTy $ bind n body) scrut'

            cascade :: (Fresh m, MonadError AstError m) => Locals -> Annote -> M.Exp -> Ty -> Maybe M.Ty -> Maybe M.Exp -> [Alt] -> m M.Exp
            cascade lcl an sv patTy resTy mdef alts = do
                  (arms, innermost) <- case (mdef, reverse alts) of
                        (Just d, _) -> pure (alts, Just d)
                        (Nothing, Alt _ (DataAlt _) [] rhs : rest@(_ : _)) -> do
                              d <- go lcl rhs
                              pure (reverse rest, Just d)
                        _           -> pure (alts, Nothing)
                  goCascade arms innermost
                  where goCascade :: (Fresh m, MonadError AstError m) => [Alt] -> Maybe M.Exp -> m M.Exp
                        goCascade [] _ = failAt an "eidos-shim: empty case alternatives."
                        goCascade [Alt aan c xs rhs] md = do
                              (pat, lcl') <- lowerAltPat aan c xs
                              rhs'        <- go lcl' rhs
                              pure $ M.Case an Nothing resTy sv (bind pat rhs') md
                        goCascade (Alt aan c xs rhs : rest) md = do
                              (pat, lcl') <- lowerAltPat aan c xs
                              rhs'        <- go lcl' rhs
                              rest'       <- goCascade rest md
                              pure $ M.Case an Nothing resTy sv (bind pat rhs') $ Just rest'

                        lowerAltPat :: (Fresh m, MonadError AstError m) => Annote -> AltCon -> [Id] -> m (M.Pat, Locals)
                        lowerAltPat aan c xs = case c of
                              DataAlt dc -> lowerPat aan lcl patTy dc xs
                              LitAlt _   -> failAt aan "eidos-shim: unsupported literal pattern."
                              DefaultAlt -> failAt aan "eidos-shim: unexpected default alternative."

            -- The pattern's type slot holds the constructed (scrutinee)
            -- type, as the retained typechecker's tcPat fills it; the
            -- backend reads constructor tags off it.
            lowerPat :: (Fresh m, MonadError AstError m) => Annote -> Locals -> Ty -> DataConId -> [Id] -> m (M.Pat, Locals)
            lowerPat an lcl pt c xs = do
                  (ps, lcl') <- foldM patVar ([], lcl) xs
                  pure (M.PatCon an (Embed Nothing) (Embed $ Just $ lowerTy pt) (Embed $ s2n c) $ reverse ps, lcl')
                  where patVar :: (Fresh m, MonadError AstError m) => ([M.Pat], Locals) -> Id -> m ([M.Pat], Locals)
                        patVar (ps, l) x = do
                              (n, l') <- bindLocal an l x
                              let t = Just $ lowerTy $ sigTy $ idSig x
                              pure (M.PatVar an (Embed Nothing) (Embed t) n : ps, l')

            bindLocal :: Fresh m => Annote -> Locals -> Id -> m (Name M.Exp, Locals)
            bindLocal an lcl x = do
                  n <- fresh $ s2n $ idOcc x
                  let t = Just $ lowerTy $ sigTy $ idSig x
                  pure (n, IM.insert (idUniq x) (M.Var an Nothing t n) lcl)

-- | Order a let-telescope's bindings innermost-first by demand (the
--   deleted bridge's algorithm, re-keyed on Eidos uniques): place a
--   binding as soon as no still-unplaced binding's rhs references it;
--   break ties by most recent demand. Bindings unreachable from the body
--   are dropped.
scheduleLets :: [Id] -> Exp -> [Exp] -> [Int]
scheduleLets xs body rhss = loop live bodyDeps []
      where idxs :: [Int]
            idxs = [0 .. length xs - 1]

            fvss :: [IS.IntSet]
            fvss = map occUniqs rhss

            deps :: Int -> IS.IntSet
            deps i = IS.fromList [ j | j <- idxs, j /= i, idUniq (xs !! j) `IS.member` (fvss !! i) ]

            bodyDeps :: [Int]
            bodyDeps = [ j | j <- idxs, idUniq (xs !! j) `IS.member` occUniqs body ]

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
                                    _       -> error "eidos-shim: scheduleLets: cyclic let telescope."

                        stack' :: [Int]
                        stack' = IS.toList (deps pick) <> filter (/= pick) stack

-- | The uniques of all variable and jump occurrences in an expression.
occUniqs :: Exp -> IS.IntSet
occUniqs = go
      where go :: Exp -> IS.IntSet
            go = \ case
                  Var _ x         -> IS.singleton $ idUniq x
                  Jump _ j es     -> IS.insert (idUniq $ jpId j) $ IS.unions $ map go es
                  App _ e a       -> go e <> goArg a
                  Lam _ _ e       -> go e
                  Let _ b e       -> goBind b <> go e
                  Case _ _ s _ as -> go s <> IS.unions [ go e | Alt _ _ _ e <- as ]
                  LitList _ _ es  -> IS.unions $ map go es
                  LitVec _ _ es   -> IS.unions $ map go es
                  _               -> mempty

            goArg :: Arg -> IS.IntSet
            goArg = \ case
                  EArg e -> go e
                  TArg _ -> mempty

            goBind :: Bind -> IS.IntSet
            goBind = \ case
                  NonRec _ e  -> go e
                  Rec bs      -> IS.unions $ map (go . snd) bs
                  Join _ _ e  -> go e

---
--- Small helpers.
---

-- | Pin a type annotation when it carries information the retained
--   pipeline can use: concrete (widths are not re-derivable) and
--   non-reactive. Reactive-typed annotations would go stale when purify
--   re-signs the definitions they mention, breaking the --debug-typecheck
--   re-check downstream of it (and no width ever hides in a ReacT type).
maybeAnn :: M.Ty -> M.Exp -> M.Exp
maybeAnn t e | concrete t, not (reacOrStateT t) = setTyAnn (Just $ poly' t) e
             | otherwise                        = e

annOf :: Exp -> Annote
annOf = \ case
      Var an _        -> an
      Con an _ _      -> an
      Prim an _ _     -> an
      LitInt an _ _   -> an
      LitStr an _     -> an
      LitList an _ _  -> an
      LitVec an _ _   -> an
      App an _ _      -> an
      Lam an _ _      -> an
      Let an _ _      -> an
      Jump an _ _     -> an
      Case an _ _ _ _ -> an

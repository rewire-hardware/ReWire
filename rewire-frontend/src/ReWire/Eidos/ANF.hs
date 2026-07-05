{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A-normalization to procify's input form (doc/eidos.md §6): every
--   definition body becomes a let chain over simple right-hand sides
--   ending in an atom (or a jump), by the spec's small ordered ruleset —
--   eta-expansion (definitions reach their signature arity, parameters in
--   the telescope), argument naming (non-atom arguments of pure spines
--   are let-bound), subject naming (case scrutinees are atoms),
--   let-flattening (no lets in right-hand sides), and alternative
--   flattening (alternative bodies are themselves ANF).
--
--   The *reactive* fragment is the deliberate exemption — it is what
--   procify consumes, and its structure must survive:
--
--   * a spine whose type mentions the reactive stack stays a spine; its
--     pure non-atom arguments are named, its lambda arguments (bind
--     continuations) stay in place with A-normalized bodies, and its
--     reactive arguments normalize recursively in place (a pure let may
--     wrap them);
--   * a case with a reactive result type stays in tail position (its
--     scrutinee is named, its alternatives normalize as tails) — procify
--     turns it into a terminator case; a pure-resulted case is let-bound
--     like any other computation.
--
--   Runs after the partial evaluator, behind @--procify@; the retained
--   shim lowers the introduced lets like any others (beta-redexes decoded
--   by the retained lift), which is how the lowering is validated against
--   the trace oracle before procify exists.
module ReWire.Eidos.ANF (normalize, hasJump) where

import ReWire.Annotation (Annote)
import ReWire.Error (AstError, MonadError)
import ReWire.Eidos.Subst (nextUniq)
import ReWire.Eidos.Syntax
import ReWire.Eidos.Types (typeOf, flattenApp, flattenArrow, hasArrow, reacOrStateT)

import Control.Monad.State.Strict (StateT, evalStateT, get, put)

normalize :: forall m. MonadError AstError m => Program -> m Program
normalize p@(Program datas defns procs top) = evalStateT go $ nextUniq p
      where go :: StateT Uniq m Program
            go = do
                  defns' <- mapM normDefn defns
                  pure $ Program datas defns' procs top

type NM m = StateT Uniq m

freshId :: Monad m => Ty -> NM m Id
freshId t = do
      u <- get
      put $ u + 1
      pure $ Id { idOcc = "$a", idUniq = u, idSig = monoSig t }

-- | Eta-expand to signature arity (leading body lambdas promote into the
--   parameter telescope; missing parameters are minted and applied), then
--   normalize the body as a tail.
normDefn :: MonadError AstError m => Defn -> NM m Defn
normDefn d = do
      let (doms, _)  = flattenArrow $ sigTy $ idSig $ defnId d
          (ps, body) = peel (length doms - length (defnParams d)) $ defnBody d
          params     = defnParams d <> ps
          an         = defnAnnote d
      etaPs <- mapM freshId $ drop (length params) doms
      let body' = foldl (\ e x -> App an e $ EArg $ Var an x) body etaPs
      nb <- normTail body'
      pure d { defnParams = params <> etaPs, defnBody = nb }
      where peel :: Int -> Exp -> ([Id], Exp)
            peel n (Lam _ x e) | n > 0 = let (xs, e') = peel (n - 1) e in (x : xs, e')
            peel _ e                   = ([], e)

-- | Hoisted bindings, innermost last.
type Hoist = [(Annote, Id, Exp)]

wrapLets :: Hoist -> Exp -> Exp
wrapLets bs e = foldr (\ (an, x, r) acc -> Let an (NonRec x r) acc) e bs

-- | Is the expression already an atom (§6)? Variables and literals;
--   nullary constructor and bare primitive occurrences; list and vector
--   literals count as literals once their elements are atoms.
isAtom :: Exp -> Bool
isAtom = \ case
      Var {}          -> True
      LitInt {}       -> True
      LitStr {}       -> True
      Con _ t _       -> null $ fst $ flattenArrow t
      Prim _ t _      -> null $ fst $ flattenArrow t
      LitList _ _ es  -> all isAtom es
      LitVec _ _ es   -> all isAtom es
      _               -> False

reactive :: Exp -> Bool
reactive = reacOrStateT . typeOf

-- | Does the expression contain a jump (anywhere)? Jumps are tail-only
--   on lint-clean input, so a jump-containing case is a join point's
--   scope and must stay in tail position.
hasJump :: Exp -> Bool
hasJump = \ case
      Jump {}         -> True
      App _ f a       -> hasJump f || (case a of { EArg x -> hasJump x; _ -> False })
      Lam _ _ b       -> hasJump b
      Let _ b body    -> bindJump b || hasJump body
      Case _ _ s _ as -> hasJump s || or [ hasJump b | Alt _ _ _ b <- as ]
      LitList _ _ es  -> any hasJump es
      LitVec _ _ es   -> any hasJump es
      _               -> False
      where bindJump :: Bind -> Bool
            bindJump = \ case
                  NonRec _ rhs -> hasJump rhs
                  Rec bs       -> any (hasJump . snd) bs
                  Join _ _ b   -> hasJump b

-- | Normalize a tail position: a let chain ending in an atom, a jump, a
--   reactive spine, or a reactive case.
normTail :: forall m. MonadError AstError m => Exp -> NM m Exp
normTail e = case e of
      _ | isAtom e   -> do
            (bs, a) <- atomize e -- names non-atom literal elements
            pure $ wrapLets bs a
      Lam an x b     -> Lam an x <$> normTail b -- residual (reactive continuation) lambda
      Let an (NonRec x rhs) body -> do
            (bs, r) <- normR rhs
            body'   <- normTail body
            pure $ wrapLets bs $ Let an (NonRec x r) body'
      Let an (Join j ps b) body -> do
            b'    <- normTail b
            body' <- normTail body
            pure $ Let an (Join j ps b') body'
      Let an (Rec bs) body -> do
            -- Local recursion never survives the simplifier on the
            -- pipeline; normalize structurally for fixture robustness.
            bs'   <- mapM (\ (x, rhs) -> (x, ) <$> normTail rhs) bs
            body' <- normTail body
            pure $ Let an (Rec bs') body'
      Jump an j es   -> do
            (bs, as) <- atomizeMany es
            pure $ wrapLets bs $ Jump an j as
      -- A reactive-resulted case (a terminator case, after procify) and a
      -- case whose alternatives jump (the scope of a join point — jumps
      -- are tail-only, so the case cannot be let-bound) stay in tail
      -- position.
      Case an t s cb alts
            | reacOrStateT t || hasJump e -> do
                  (bs, sa) <- atomize s
                  alts'    <- mapM (\ (Alt aan c xs b) -> Alt aan c xs <$> normTail b) alts
                  pure $ wrapLets bs $ Case an t sa cb alts'
            | otherwise -> named
      App {}
            | reactive e || hasArrow (typeOf e) -> do
                  (bs, e') <- normSpine e
                  pure $ wrapLets bs e'
            | otherwise  -> named
      _ -> named
      where -- A pure computation in tail position is named: let a = r in a.
            named :: NM m Exp
            named = do
                  (bs, r) <- normR e
                  if isAtom r then pure $ wrapLets bs r else do
                        x <- freshId $ typeOf r
                        pure $ wrapLets bs $ Let (ann' r) (NonRec x r) $ Var (ann' r) x

-- | Normalize to a right-hand side (§6 r-forms), hoisting bindings.
normR :: forall m. MonadError AstError m => Exp -> NM m (Hoist, Exp)
normR e = case e of
      _ | isAtom e -> atomize e
      Lam {}       -> pure ([], e) -- residual lambda (never let-bound on the pipeline)
      Let _ (NonRec x rhs) body -> do
            -- Let-flattening: the binding hoists out of the right-hand side.
            (bs, r)    <- normR rhs
            (bs', r'') <- normR body
            pure (bs <> [(ann' rhs, x, r)] <> bs', r'')
      Let {}       -> ([], ) <$> normTail e -- join/rec in rhs position: structural
      Jump {}      -> ([], ) <$> normTail e
      Case an t s cb alts -> do
            (bs, sa) <- atomize s
            alts'    <- mapM (\ (Alt aan c xs b) -> Alt aan c xs <$> normTail b) alts
            pure (bs, Case an t sa cb alts')
      App {}       -> normSpine e
      LitList {}   -> atomize e
      LitVec {}    -> atomize e
      _            -> pure ([], e)

-- | A spine normalized in place: representable non-atom arguments are
--   named and hoisted (argument naming); lambda arguments (continuations,
--   pure higher-order primitive arguments) keep their place with
--   normalized bodies; function-typed arguments (partial applications,
--   definition references) keep their place with their own arguments
--   normalized; reactive arguments normalize recursively in place.
normSpine :: forall m. MonadError AstError m => Exp -> NM m (Hoist, Exp)
normSpine e = do
      let (h, args) = flattenApp e
      (bs, args') <- go args
      pure (bs, foldl (App $ ann' e) h args')
      where go :: [Arg] -> NM m (Hoist, [Arg])
            go []             = pure ([], [])
            go (TArg t : as)  = fmap (TArg t :) <$> go as
            go (EArg a : as)  = do
                  (bs, a')   <- one a
                  (bs', as') <- go as
                  pure (bs <> bs', EArg a' : as')

            one :: Exp -> NM m (Hoist, Exp)
            one a | isAtom a               = atomize a
                  | Lam {} <- a            = ([], ) <$> normTail a
                  -- Primitive applications are naming-transparent: the
                  -- backends pattern-match primitive idioms (bit slices,
                  -- finite literals), so a primitive-headed argument
                  -- normalizes in place.
                  | (Prim {}, _) <- flattenApp a = normSpine a
                  | App {} <- a
                  , hasArrow $ typeOf a    = normSpine a
                  | hasArrow $ typeOf a    = pure ([], a)
                  | reactive a             = ([], ) <$> normTail a
                  | otherwise              = atomize a

-- | Normalize to an atom, hoisting a binding when the result is not
--   already one.
atomize :: forall m. MonadError AstError m => Exp -> NM m (Hoist, Exp)
atomize e = case e of
      Var {}         -> pure ([], e)
      LitInt {}      -> pure ([], e)
      LitStr {}      -> pure ([], e)
      Con _ t _  | null $ fst $ flattenArrow t -> pure ([], e)
      Prim _ t _ | null $ fst $ flattenArrow t -> pure ([], e)
      LitList an t es -> do
            (bs, as) <- atomizeMany es
            pure (bs, LitList an t as)
      LitVec an t es  -> do
            (bs, as) <- atomizeMany es
            pure (bs, LitVec an t as)
      _              -> do
            (bs, r) <- normR e
            if isAtom r then pure (bs, r) else do
                  x <- freshId $ typeOf r
                  pure (bs <> [(ann' r, x, r)], Var (ann' r) x)

atomizeMany :: MonadError AstError m => [Exp] -> NM m (Hoist, [Exp])
atomizeMany []       = pure ([], [])
atomizeMany (e : es) = do
      (bs, a)   <- atomize e
      (bs', as) <- atomizeMany es
      pure (bs <> bs', a : as)

ann' :: Exp -> Annote
ann' = \ case
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

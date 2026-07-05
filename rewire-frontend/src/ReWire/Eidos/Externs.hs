{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Extern neutering on Eidos (the retired Crust pass's successor; see
--   ReWire.Crust.Transform.neuterExterns for the policy's rationale).
--   Decides, per extern, whether the user-supplied Haskell implementation
--   (rwPrimExtern's seventh argument) can serve as a model for the
--   interpreter, and replaces it with an inert error placeholder
--   otherwise, so the simplifier never descends into extern
--   implementations. An implementation is kept only when it is a
--   reference to a top-level definition — other than the extern's own
--   enclosing definition, since @f = extern "f" f@ is the conventional
--   no-model idiom — whose reachable definitions are all non-recursive
--   and representable. Implementations that look like real models but
--   fail the checks are neutered with a warning.
--
--   After INLINE inlining, an extern application is still wrapped in the
--   beta redexes left by inlining the extern/externWithSig wrappers, so
--   definitions containing externs are reduced first to expose the
--   implementation argument. (On Eidos this needs no isolated freshness
--   counter: the pass's supply is seeded above the program and perturbs
--   nothing else.) This pass must run before the simplifier.
module ReWire.Eidos.Externs (neuterExterns) where

import ReWire.Annotation (Annote, ann)
import ReWire.Builtins (Builtin (Error, Extern))
import ReWire.Error (AstError, MonadError, Warning (..))
import ReWire.Eidos.Simplify (SimpT, runSimpT, reduceExp)
import ReWire.Eidos.Subst (freeUniqs)
import ReWire.Eidos.Syntax
import ReWire.Eidos.Types (typeOf, flattenApp, higherOrder, fundamental, synthable)

import Data.HashSet (HashSet)
import Data.Maybe (isJust)
import Data.Text (Text)

import qualified Data.HashSet       as Set
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS

-- | Returns the neutered program and the warnings for discarded models
--   (emitted by the caller, which holds the Config).
neuterExterns :: forall m. MonadError AstError m => Program -> m (Program, [Warning])
neuterExterns p@(Program datas defns procs top) = do
      defns' <- runSimpT p $ mapM reduce1 defns
          -- Stage 1: neuter self-referential implementations silently
          -- (the no-model idiom), so they don't appear as spurious cycles
          -- in stage 2's recursion check.
      let ds1   = map (\ d -> d { defnBody = neuterImpl (isSelf d) $ defnBody d }) defns'
          dmap1 = IM.fromList [ (idUniq $ defnId d, d) | d <- ds1 ]
          -- Stage 2: neuter everything else that isn't a usable model.
          ds2   = map (\ d -> d { defnBody = neuterImpl (isJust . disposition dmap1) $ defnBody d }) ds1
          warns = [ Warning (defnAnnote d) $ "Ignoring the Haskell model for extern '" <> exName ex
                        <> "': " <> reason <> "; the interpreter will not be able to evaluate this extern."
                  | d <- ds1
                  , (ex, e) <- externApps $ defnBody d
                  , Just (Just reason) <- [disposition dmap1 e]
                  ]
      pure (Program datas ds2 procs top, warns)
      where tops :: IS.IntSet
            tops = IS.fromList $ map (idUniq . defnId) defns

            reduce1 :: Defn -> SimpT m Defn
            reduce1 d
                  | hasExtern d = (\ b -> d { defnBody = b }) <$> reduceExp tops (defnBody d)
                  | otherwise   = pure d

            hasExtern :: Defn -> Bool
            hasExtern = go . defnBody
                  where go :: Exp -> Bool
                        go = \ case
                              Prim _ _ Extern -> True
                              App _ f a       -> go f || goArg a
                              Lam _ _ b       -> go b
                              Let _ b body    -> goBind b || go body
                              Jump _ _ es     -> any go es
                              Case _ _ s _ as -> go s || or [ go b | Alt _ _ _ b <- as ]
                              LitList _ _ es  -> any go es
                              LitVec _ _ es   -> any go es
                              _               -> False

                        goArg :: Arg -> Bool
                        goArg = \ case
                              EArg e -> go e
                              _      -> False

                        goBind :: Bind -> Bool
                        goBind = \ case
                              NonRec _ rhs -> go rhs
                              Rec bs       -> any (go . snd) bs
                              Join _ _ b   -> go b

            isSelf :: Defn -> Exp -> Bool
            isSelf d = \ case
                  Var _ x -> idUniq x == idUniq (defnId d)
                  _       -> False

            -- | Nothing: keep as a model. Just Nothing: neuter silently
            --   (already a placeholder, or a local name — the body of a
            --   generic wrapper, not a use site). Just (Just r): neuter,
            --   warning r.
            disposition :: IM.IntMap Defn -> Exp -> Maybe (Maybe Text)
            disposition dmap = \ case
                  e | isPlaceholder e -> Just Nothing
                  Var _ x | IS.member (idUniq x) tops -> Just <$> verdict dmap mempty x
                          | otherwise                 -> Just Nothing
                  _ -> Just $ Just "only a reference to a top-level definition can be used as a model"

            -- | DFS from a definition: every reachable definition must
            --   exist, have a usable type, and the call graph must be
            --   acyclic. Returns the first reason for rejection, or
            --   Nothing if usable as a model.
            verdict :: IM.IntMap Defn -> HashSet Uniq -> Id -> Maybe Text
            verdict dmap stack x
                  | Set.member (idUniq x) stack = Just $ idOcc x <> " is recursive"
                  | otherwise = case IM.lookup (idUniq x) dmap of
                        Nothing -> Just $ idOcc x <> " does not refer to a top-level definition"
                        Just d  -> case typeReason d of
                              Just r  -> Just r
                              Nothing -> foldr (orElse . verdict dmap (Set.insert (idUniq x) stack)) Nothing $ succs d
                  where orElse :: Maybe a -> Maybe a -> Maybe a
                        orElse a b = maybe b Just a

            typeReason :: Defn -> Maybe Text
            typeReason d
                  | higherOrder t       = reason "has a higher-order type"
                  | not (fundamental t) = reason "has String or Integer arguments"
                  | not (null tvs)      = reason "is polymorphic"
                  | not (synthable t)   = reason "has an unsynthesizable type"
                  | otherwise           = Nothing
                  where Sig tvs t = idSig $ defnId d

                        reason :: Text -> Maybe Text
                        reason r = Just $ idOcc (defnId d) <> " " <> r

            -- | Top-level definitions referenced by a definition's body,
            --   as occurrence Ids (any occurrence serves — it carries the
            --   binder's signature).
            succs :: Defn -> [Id]
            succs d = IM.elems $ IM.restrictKeys (occIds $ defnBody d) $ freeUniqs (defnBody d) `IS.intersection` tops
                  where occIds :: Exp -> IM.IntMap Id
                        occIds = \ case
                              Var _ v         -> IM.singleton (idUniq v) v
                              App _ f a       -> occIds f <> occIdsArg a
                              Lam _ _ b       -> occIds b
                              Let _ b body    -> occIdsBind b <> occIds body
                              Jump _ _ es     -> IM.unions $ map occIds es
                              Case _ _ s _ as -> occIds s <> IM.unions [ occIds b | Alt _ _ _ b <- as ]
                              LitList _ _ es  -> IM.unions $ map occIds es
                              LitVec _ _ es   -> IM.unions $ map occIds es
                              _               -> mempty

                        occIdsArg :: Arg -> IM.IntMap Id
                        occIdsArg = \ case
                              EArg e -> occIds e
                              _      -> mempty

                        occIdsBind :: Bind -> IM.IntMap Id
                        occIdsBind = \ case
                              NonRec _ rhs -> occIds rhs
                              Rec bs       -> IM.unions $ map (occIds . snd) bs
                              Join _ _ b   -> occIds b

            -- | Rewrite every extern implementation argument selected by
            --   the predicate into an inert placeholder.
            neuterImpl :: (Exp -> Bool) -> Exp -> Exp
            neuterImpl sel = go
                  where go :: Exp -> Exp
                        go e = case e of
                              App an ex (EArg impl) | isExtern ex, sel impl, not (isPlaceholder impl) ->
                                    App an (goE ex) $ EArg $ mkError (ann impl) (typeOf impl) "Extern expression placeholder"
                              App an f a      -> App an (goE f) $ goA a
                              _               -> goE e

                        goE :: Exp -> Exp
                        goE e = case e of
                              App {}          -> go e
                              Lam an x b      -> Lam an x $ go b
                              Let an b body   -> Let an (goB b) $ go body
                              Jump an j es    -> Jump an j $ map go es
                              Case an t s cb alts -> Case an t (go s) cb [ Alt aan c xs (go b) | Alt aan c xs b <- alts ]
                              LitList an t es -> LitList an t $ map go es
                              LitVec an t es  -> LitVec an t $ map go es
                              _               -> e

                        goA :: Arg -> Arg
                        goA = \ case
                              EArg e -> EArg $ go e
                              t      -> t

                        goB :: Bind -> Bind
                        goB = \ case
                              NonRec x rhs -> NonRec x $ go rhs
                              Rec bs       -> Rec [ (x, go rhs) | (x, rhs) <- bs ]
                              Join j ps b  -> Join j ps $ go b

            -- | All (extern-application, implementation-argument) pairs.
            externApps :: Exp -> [(Exp, Exp)]
            externApps e = here <> below
                  where here = case e of
                              App _ ex (EArg impl) | isExtern ex -> [(ex, impl)]
                              _                                  -> []

                        below = case e of
                              App _ f a       -> externApps f <> (case a of { EArg x -> externApps x; _ -> [] })
                              Lam _ _ b       -> externApps b
                              Let _ b body    -> bindApps b <> externApps body
                              Jump _ _ es     -> concatMap externApps es
                              Case _ _ s _ as -> externApps s <> concat [ externApps b | Alt _ _ _ b <- as ]
                              LitList _ _ es  -> concatMap externApps es
                              LitVec _ _ es   -> concatMap externApps es
                              _               -> []

                        bindApps :: Bind -> [(Exp, Exp)]
                        bindApps = \ case
                              NonRec _ rhs -> externApps rhs
                              Rec bs       -> concatMap (externApps . snd) bs
                              Join _ _ b   -> externApps b

            -- | A saturated extern application (six arguments; the
            --   seventh is the implementation).
            isExtern :: Exp -> Bool
            isExtern e = case flattenApp e of
                  (Prim _ _ Extern, args) -> length [ () | EArg _ <- args ] == 6
                  _                       -> False

            isPlaceholder :: Exp -> Bool
            isPlaceholder e = case flattenApp e of
                  (Prim _ _ Error, _) -> True
                  _                   -> False

            exName :: Exp -> Text
            exName e = case flattenApp e of
                  (_, [_, _, _, _, _, EArg (LitStr _ s)]) -> s
                  _                                       -> "<unknown>"

mkError :: Annote -> Ty -> Text -> Exp
mkError an t m = App an (Prim an (Arrow an (TyCon an "String") t) Error) $ EArg $ LitStr an m

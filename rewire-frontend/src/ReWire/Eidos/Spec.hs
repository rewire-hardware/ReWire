{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Monomorphization by specialization (doc/eidos.md §4.1, §5): a worklist
--   specializer driven by the type arguments the bridge keeps on
--   application spines. There is no inference and no unification: a
--   monomorphic definition's body mentions a polymorphic definition only
--   as a spine head saturated with closed type arguments (the linter's
--   poly-mode guarantee), so each such spine is a specialization request;
--   the clone is pure type substitution through the audited clone
--   primitive ('ReWire.Eidos.Subst.instantiateDefn'), memoized on the
--   normalized argument list. Clones carry their provenance ('SpecOrigin')
--   and are named @origin$i@ with a per-origin counter, in discovery
--   order (deterministic).
--
--   Polymorphic definitions are templates: they are dropped from the
--   output (their instantiations replace them), so the result is
--   monomorphic ('ReWire.Eidos.Lint' mono mode) — except the
--   builtin-named definitions (rwPrim*), whose polymorphic signatures
--   carry the builtins' type assumptions to the Eidos-to-Hyle fold (their
--   bodies are error stubs, and they are never referenced as variables —
--   references become 'Prim' occurrences at the bridge); they ride
--   through unchanged. The worklist runs in generations; an instantiation
--   chain deeper than the budget (the historical typechecker bound,
--   raised by @--depth@) is rejected with the retired specializer's
--   diagnostic.
module ReWire.Eidos.Spec (specialize) where

import ReWire.Annotation (noAnn)
import ReWire.Builtins (builtins)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Eidos.Subst (instantiateDefn, nextUniq)
import ReWire.Eidos.Syntax
import ReWire.Eidos.Types (natNorm, flattenApp)
import ReWire.Pretty (showt)

import Control.Monad (foldM)
import Control.Monad.State.Strict (StateT, evalStateT)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List (partition)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.IntMap.Strict  as IM

-- | A specialization request: a polymorphic definition (by unique) at a
--   closed, normalized type-argument list.
type Key = (Uniq, [Ty])

specialize :: forall m. MonadError AstError m => Natural -> Program -> m Program
specialize bound p@(Program datas defns procs top) = evalStateT go $ nextUniq p
      where templates :: [Defn]
            kept      :: [Defn]
            (templates, kept) = partition isTemplate defns

            -- Templates are the polymorphic definitions except the builtin
            -- signature carriers.
            isTemplate :: Defn -> Bool
            isTemplate d = isPoly (defnId d) && not (isPrimName $ idOcc $ defnId d)

            polys :: IM.IntMap Defn
            polys = IM.fromList [ (idUniq $ defnId d, d) | d <- templates ]

            isPoly :: Id -> Bool
            isPoly = not . null . sigTVs . idSig

            isPrimName :: Text -> Bool
            isPrimName = flip Set.member primNames

            primNames :: HashSet Text
            primNames = Set.fromList $ map fst builtins

            go :: StateT Uniq m Program
            go = do
                  (table, clones) <- rounds 0 (mempty, mempty) [] $ concatMap (requests . defnBody) kept
                  pure $ Program datas (map (rewrite table) $ kept <> clones) procs top

            -- One worklist generation per round.
            rounds :: Natural -> (HashMap Key Id, IM.IntMap Int) -> [Defn] -> [Key] -> StateT Uniq m (HashMap Key Id, [Defn])
            rounds n (table, counters) clones reqs
                  | null new   = pure (table, clones)
                  | n >= bound = failAt noAnn "polymorphic function instantiation not terminating (mutually recursive definitions?)."
                  | otherwise  = do
                        ((table', counters'), batch) <- foldM step ((table, counters), []) new
                        let batch' = reverse batch
                        rounds (n + 1) (table', counters') (clones <> batch') $ concatMap (requests . defnBody) batch'
                  where new = dedupe table reqs

            step :: ((HashMap Key Id, IM.IntMap Int), [Defn]) -> Key -> StateT Uniq m ((HashMap Key Id, IM.IntMap Int), [Defn])
            step ((table, counters), acc) k@(u, ts) = case IM.lookup u polys of
                  Nothing -> pure ((table, counters), acc) -- unreachable on lint-clean input
                  Just d  -> do
                        let i   = IM.findWithDefault 0 u counters + 1
                            occ = idOcc (defnId d) <> "$" <> showt i
                        d' <- instantiateDefn occ ts d
                        let d'' = d' { defnOrigin = Just $ SpecOrigin (idOcc $ defnId d) ts }
                        pure ((Map.insert k (defnId d'') table, IM.insert u i counters), d'' : acc)

            -- New keys, in discovery order.
            dedupe :: HashMap Key Id -> [Key] -> [Key]
            dedupe table = go' mempty
                  where go' :: HashMap Key () -> [Key] -> [Key]
                        go' _ [] = []
                        go' seen (k : ks)
                              | Map.member k table || Map.member k seen = go' seen ks
                              | otherwise                               = k : go' (Map.insert k () seen) ks

            -- Specialization requests in an expression, in traversal order:
            -- every spine headed by a saturated polymorphic top-level
            -- reference.
            requests :: Exp -> [Key]
            requests e = case e of
                  App {}          -> spine e
                  Var {}          -> []
                  Con {}          -> []
                  Prim {}         -> []
                  LitInt {}       -> []
                  LitStr {}       -> []
                  LitList _ _ es  -> concatMap requests es
                  LitVec _ _ es   -> concatMap requests es
                  Lam _ _ b       -> requests b
                  Let _ b body    -> bindReqs b <> requests body
                  Jump _ _ es     -> concatMap requests es
                  Case _ _ s _ as -> requests s <> concat [ requests b | Alt _ _ _ b <- as ]
                  where spine :: Exp -> [Key]
                        spine se = hk <> concatMap requests eas
                              where (h, args) = flattenApp se
                                    tys       = map natNorm [ t | TArg t <- takeWhile isTArg args ]
                                    eas       = [ a | EArg a <- args ]
                                    hk        = case h of
                                          Var _ x | not (null tys), IM.member (idUniq x) polys -> [(idUniq x, tys)]
                                          Var {}                                               -> []
                                          _                                                    -> requests h

                        bindReqs :: Bind -> [Key]
                        bindReqs = \ case
                              NonRec _ rhs -> requests rhs
                              Rec bs       -> concatMap (requests . snd) bs
                              Join _ _ b   -> requests b

            -- Rewrite every specialized spine: the head becomes the clone
            -- and the type arguments are erased.
            rewrite :: HashMap Key Id -> Defn -> Defn
            rewrite table d = d { defnBody = rw $ defnBody d }
                  where rw :: Exp -> Exp
                        rw e = case e of
                              App an _ _ ->
                                    let (h, args)  = flattenApp e
                                        (tas, eas) = span isTArg args
                                        tys        = map natNorm [ t | TArg t <- tas ]
                                    in case h of
                                          Var van x | not (null tys), Just x' <- Map.lookup (idUniq x, tys) table ->
                                                foldl (App an) (Var van x') [ EArg $ rw a | EArg a <- eas ]
                                          _ -> foldl (App an) (rw h) $ map rwArg args
                              Var {}          -> e
                              Con {}          -> e
                              Prim {}         -> e
                              LitInt {}       -> e
                              LitStr {}       -> e
                              LitList an t es -> LitList an t $ map rw es
                              LitVec an t es  -> LitVec an t $ map rw es
                              Lam an x b      -> Lam an x $ rw b
                              Let an b body   -> Let an (rwBind b) $ rw body
                              Jump an j es    -> Jump an j $ map rw es
                              Case an t s cb alts -> Case an t (rw s) cb [ Alt aan c xs (rw b) | Alt aan c xs b <- alts ]

                        rwArg :: Arg -> Arg
                        rwArg = \ case
                              EArg e -> EArg $ rw e
                              t      -> t

                        rwBind :: Bind -> Bind
                        rwBind = \ case
                              NonRec x rhs -> NonRec x $ rw rhs
                              Rec bs       -> Rec [ (x, rw rhs) | (x, rhs) <- bs ]
                              Join j ps b  -> Join j ps $ rw b

            isTArg :: Arg -> Bool
            isTArg = \ case
                  TArg _ -> True
                  _      -> False

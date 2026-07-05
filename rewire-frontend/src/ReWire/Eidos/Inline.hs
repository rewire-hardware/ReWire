{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The INLINE-attribute inliner (the retired Crust pass's successor):
--   every occurrence of an INLINE-annotated definition is replaced by its
--   body (as a lambda telescope over its parameters — application sites
--   become beta redexes for the downstream partial evaluator), with every
--   inserted copy refreshed through the audited clone primitive. Runs on
--   monomorphic programs (after 'ReWire.Eidos.Spec.specialize'): inlining
--   under a type-argument spine would strand the arguments on a
--   non-variable head.
--
--   INLINE definitions referencing other INLINE definitions are expanded
--   to closed form first (depth-first, memoized); a reference cycle among
--   them is rejected with the retired pass's diagnostic. The definitions
--   themselves are kept (dead ones fall to the downstream purge).
module ReWire.Eidos.Inline (inlineAnnotated) where

import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Eidos.Subst (nextUniq, occCounts, substVarsRefreshing)
import ReWire.Eidos.Syntax

import Control.Monad (foldM)
import Control.Monad.State.Strict (StateT, evalStateT)

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS

inlineAnnotated :: forall m. MonadError AstError m => Program -> m Program
inlineAnnotated p@(Program datas defns procs top) = evalStateT go $ nextUniq p
      where inls :: IM.IntMap Defn
            inls = IM.fromList [ (idUniq $ defnId d, d) | d <- defns, defnAttr d == Just Inline ]

            go :: StateT Uniq m Program
            go = do
                  table  <- foldM (\ acc u -> expand acc mempty u) mempty $ IM.keys inls
                  defns' <- mapM (\ d -> (\ b -> d { defnBody = b }) <$> substVarsRefreshing table (defnBody d)) defns
                  pure $ Program datas defns' procs top

            -- The definition as a substitution payload: a lambda telescope
            -- over its parameters.
            payload :: Defn -> Exp
            payload d = foldr (Lam $ defnAnnote d) (defnBody d) $ defnParams d

            -- Expand one INLINE definition to closed form (no INLINE
            -- references remain), depth-first over its INLINE dependencies.
            expand :: IM.IntMap Exp -> IS.IntSet -> Uniq -> StateT Uniq m (IM.IntMap Exp)
            expand table stack u
                  | IM.member u table = pure table
                  | IS.member u stack = failAt (defnAnnote d) "INLINE definition expansion not terminating (mutually recursive definitions?)."
                  | otherwise         = do
                        let deps = IS.toList $ IM.keysSet (occCounts $ payload d) `IS.intersection` IM.keysSet inls
                        table' <- foldM (\ acc v -> expand acc (IS.insert u stack) v) table deps
                        e'     <- substVarsRefreshing (IM.restrictKeys table' $ IS.fromList deps) $ payload d
                        pure $ IM.insert u e' table'
                  where d = inls IM.! u

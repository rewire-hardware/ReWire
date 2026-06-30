{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module Embedder.HSE.Desugar (desugar, addMainModuleHead) where

import ReWire.Annotation (Annote)
import ReWire.Error (MonadError, AstError, failAt)
import ReWire.HSE.Desugar
      ( Desugar (..), pass, Fresh, fresh, addMainModuleHead
      , desugarNegs, desugarDos, desugarInfix, flattenLambdas, depatLambdas
      , lambdasToCases, desugarGuards, desugarIfs, wheresToLets
      , desugarNegLitPats, normIds, deparenify, normTyContext, desugarTyFuns
      , desugarAsPats
      )
import ReWire.HSE.Rename (Renamer)
import ReWire.SYB (Tr (TM), transform)

import Control.Monad (replicateM, (>=>), void)
import Control.Monad.State (evalStateT, MonadState)
import Data.Foldable (foldrM)
import Data.Text (pack)
import Language.Haskell.Exts.Syntax

-- | Desugar into lambdas then normalize the lambdas. This differs from the
--   rewire-frontend pipeline: tuples, records, case flattening, and discriminator
--   lifting are deferred to the Atmo IR, and unguarded multi-clause function
--   bindings are kept intact (see 'desugarFuns').
desugar :: MonadError AstError m => Renamer -> Module Annote -> m (Module Annote)
desugar _rn = flip evalStateT 0 .
      ( pure
      >=> pass desugarInfix
      >=> pass
            ( desugarNegs
           <> desugarDos
           <> desugarInfix
           <> desugarFuns
            )
      >=> pass flattenLambdas
      >=> pass
            ( depatLambdas
           <> lambdasToCases
            )
      >=> pass desugarGuards
      >=> pass
            ( desugarIfs
           <> wheresToLets
            )
      >=> pass
            ( desugarLets
           <> desugarNegLitPats
           <> normIds
           <> deparenify
           <> normTyContext
           <> desugarTyFuns
            )
      >=> pass desugarAsPats
      )

-- | Like rewire-frontend's desugarFuns, except that unguarded multi-clause
--   function bindings are kept intact (Atmo represents them directly) and
--   pattern variables are annotated with their declared types.
-- > f p1 p2 = rhs1
-- > f q1 q2 = rhs2
-- becomes
-- > f = \ $1 $2 -> case ($1, $2) of { (p1, p2) -> rhs1; (q1, q2) -> rhs2 }
desugarFuns :: (MonadState Fresh m, MonadError AstError m) => Desugar m
desugarFuns = mempty
      { dsModule = TM $ \ case
            Module man hd prags imps ds -> Module man hd prags imps <$> mapM (desugarFun $ tySigMap ds) ds
            m                           -> pure m
      , dsBinds = TM $ \ case
            BDecls ban ds               -> BDecls ban <$> mapM (desugarFun $ tySigMap ds) ds
            b                           -> pure b
      }
      where desugarFun :: (MonadState Fresh m, MonadError AstError m) => [(Name Annote, Type Annote)] -> Decl Annote -> m (Decl Annote)
            desugarFun ts = \ case
                  FunBind l ms@(Match {}:_) | allUnguarded ms ->
                        pure $ FunBind l ms
                  FunBind l ms@(Match l' name pats _ _:_) -> do
                        alts <- mapM (toAlt ts) ms
                        e    <- buildLambda l alts $ length pats
                        pure $ PatBind l (PVar l' name) (UnGuardedRhs l e) Nothing
                  -- Turn guards on PatBind into guards on case (of unit) alts.
                  PatBind l p rhs@(GuardedRhss l' _) binds -> pure $ PatBind l p (UnGuardedRhs l' $ Case l' (Con l' $ Special l' $ UnitCon l') [Alt l' (PWildCard l') rhs binds]) Nothing
                  d                                        -> pure d

            buildLambda :: (MonadState Fresh m, MonadError AstError m) => Annote -> [Alt Annote] -> Int -> m (Exp Annote)
            buildLambda l alts = \ case
                  1     -> do
                        x <- fresh l
                        -- NOTE: can't type-annotate params without expanding type synonyms.
                        pure $ Lambda l [PVar l x] $ Case l (Var l $ UnQual l x) alts
                  arity -> do
                        xs <- replicateM arity (fresh l)
                        -- NOTE: can't type-annotate params without expanding type synonyms.
                        pure $ Lambda l (PVar l <$> xs) $ Case l (Tuple l Boxed (map (Var l . UnQual l) xs)) alts

            toAlt :: (MonadState Fresh m, MonadError AstError m) => [(Name Annote, Type Annote)] -> Match Annote -> m (Alt Annote)
            toAlt ts = \ case
                  -- NOTE: can't type-annotate params without expanding type synonyms.
                  Match l' _ [p] rhs binds -> pure $ Alt l' (annotatePVars ts p) rhs binds
                  Match l' _ ps  rhs binds -> pure $ Alt l' (PTuple l' Boxed $ map (annotatePVars ts) ps) rhs binds
                  m                        -> failAt (ann m) $ "Unsupported decl syntax: " <> pack (show $ void m)

            allUnguarded :: [Match l] -> Bool
            allUnguarded [] = True
            allUnguarded (Match _ _ _ (UnGuardedRhs {}) _ : ms) = allUnguarded ms
            allUnguarded (Match _ _ _ (GuardedRhss {}) _ : _) = False
            allUnguarded _ = error "Infix should be desugered by now."

-- TODO(chathhorn): recursive bindings?
-- | Like rewire-frontend's desugarLets, but annotates pattern variables with
--   their declared types. Turns Lets into Cases. Assumes functions in Lets
--   are already desugared. E.g.:
-- > let p = e1
-- >     q = e2
-- > in e3
-- becomes
-- > case e1 of { p -> (case e2 of { q -> e3 } }
desugarLets :: (MonadState Fresh m, MonadError AstError m) => Desugar m
desugarLets = mempty { dsExp = TM $ \ case
      Let _ (BDecls _ ds) e -> foldrM (transLet $ tySigMap ds) e $ filter isPatBind ds
      n@Let{}               -> failAt (ann n) "Unsupported let syntax"
      e                     -> pure e}
      where transLet :: (MonadState Fresh m, MonadError AstError m) => [(Name Annote, Type Annote)] -> Decl Annote -> Exp Annote -> m (Exp Annote)
            transLet ts (PatBind l p (UnGuardedRhs l' e1) Nothing) inner = pure $ Case l e1 [Alt l (annotatePVars ts p) (UnGuardedRhs l' inner) Nothing]
            transLet _ n _                                               = failAt (ann n) "Unsupported syntax in a let binding"

            isPatBind :: Decl Annote -> Bool
            isPatBind PatBind {} = True
            isPatBind _          = False

tySigMap :: [Decl Annote] -> [(Name Annote, Type Annote)]
tySigMap = concatMap tySig
      where tySig :: Decl Annote -> [(Name Annote, Type Annote)]
            tySig = \ case
                  TypeSig _ ns t -> (,t) <$> ns
                  _              -> []

annotatePVars :: [(Name Annote, Type Annote)] -> Pat Annote -> Pat Annote
annotatePVars ts = transform $ \ case
      PVar an n | Just t <- lookup n ts -> PatTypeSig an (PVar an n) t
      n                                 -> n

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | Type utilities for Eidos: the total synthesizing 'typeOf', type
--   substitution (trivially capture-free: 'Ty' has no binders), arrow and
--   application spines, and type-level-natural evaluation.
--
--   'typeOf' follows the Core-Lint convention: it is total on well-formed
--   programs (doc/eidos.md §5) and calls 'error' on ill-formed ones, which
--   the linter rejects with a located diagnostic before any pass consults
--   types.
module ReWire.Eidos.Types
      ( typeOf
      , substTv
      , instantiate
      , mkArrow, dstArrow, flattenArrow
      , flattenTyApp, mkTyApp
      , flattenApp
      , evalNat, natNorm
      ) where

import ReWire.Annotation (Annote, ann)
import ReWire.Eidos.Syntax

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map

-- | Substitution of types for type variables. No renaming is ever needed:
--   types contain no binders.
substTv :: HashMap TyVar Ty -> Ty -> Ty
substTv s = go
      where go :: Ty -> Ty
            go = \ case
                  t@(TyVarT _ v)  -> Map.lookupDefault t v s
                  TyApp an t u    -> TyApp an (go t) (go u)
                  Arrow an t u    -> Arrow an (go t) (go u)
                  t               -> t

-- | Instantiate a signature at type arguments (must saturate the
--   quantifier list; the linter enforces this at occurrences).
instantiate :: Sig -> [Ty] -> Ty
instantiate (Sig tvs t) ts
      | length tvs == length ts = substTv (Map.fromList $ zip tvs ts) t
      | otherwise               = error $ "Eidos.instantiate: signature quantifies "
            <> show (length tvs) <> " variables, applied to " <> show (length ts)

mkArrow :: Annote -> Ty -> Ty -> Ty
mkArrow = Arrow

dstArrow :: Ty -> Maybe (Ty, Ty)
dstArrow = \ case
      Arrow _ t u -> Just (t, u)
      _           -> Nothing

-- | An arrow spine: @flattenArrow (a -> b -> c) == ([a, b], c)@.
flattenArrow :: Ty -> ([Ty], Ty)
flattenArrow = \ case
      Arrow _ t u -> let (ts, r) = flattenArrow u in (t : ts, r)
      t           -> ([], t)

-- | A type application spine: @flattenTyApp (T a b) == (T, [a, b])@.
flattenTyApp :: Ty -> (Ty, [Ty])
flattenTyApp = go []
      where go :: [Ty] -> Ty -> (Ty, [Ty])
            go acc = \ case
                  TyApp _ t u -> go (u : acc) t
                  t           -> (t, acc)

mkTyApp :: Annote -> Ty -> [Ty] -> Ty
mkTyApp an = foldl' $ TyApp an

-- | A term application spine: head and arguments, outermost last.
flattenApp :: Exp -> (Exp, [Arg])
flattenApp = go []
      where go :: [Arg] -> Exp -> (Exp, [Arg])
            go acc = \ case
                  App _ e a -> go (a : acc) e
                  e         -> (e, acc)

-- | The type of an expression. Total on lint-clean programs; 'error' (with
--   the offending annotation) otherwise. Type arguments may only be applied
--   to 'Var' heads, must precede all term arguments, and must saturate the
--   head's quantifier list (doc/eidos.md §5). One tolerance: a bare
--   (argument-less) reference to a polymorphic name returns its open
--   signature type — the linter rejects such references outside
--   fully-instantiated spines, but the specializer reads them.
typeOf :: Exp -> Ty
typeOf e = case e of
      Var _ x         -> headTy (idSig x) []
      Con _ t _       -> t
      Prim _ t _      -> t
      LitInt _ t _    -> t
      LitStr an _     -> TyCon an "String"
      LitList _ t _   -> t
      LitVec _ t _    -> t
      Lam an x b      -> Arrow an (sigTy $ idSig x) $ typeOf b
      Let _ _ b       -> typeOf b
      Jump an j args  -> peel an (length args) $ sigTy $ idSig $ jpId j
      Case _ t _ _ _  -> t
      App an _ _      -> spineTy an
      where spineTy :: Annote -> Ty
            spineTy an = case flattenApp e of
                  (Var _ x, args)       -> peelArgs an (headTy (idSig x) $ targs args) $ eargs args
                  (h, args)
                        | null (targs args) -> peelArgs an (typeOf h) $ eargs args
                        | otherwise         -> ill an "type argument applied to a non-variable head"

            headTy :: Sig -> [Ty] -> Ty
            headTy sig@(Sig tvs t) ts
                  | null tvs && null ts       = t
                  | length tvs == length ts   = instantiate sig ts
                  | null ts                   = t -- under-instantiated reference: free sig variables
                  | otherwise                 = ill (ann e) "unsaturated type application"

            targs :: [Arg] -> [Ty]
            targs as = [ t | TArg t <- takeWhile isTArg as ]

            eargs :: [Arg] -> [Exp]
            eargs as | any isTArg as' = ill (ann e) "type argument after term arguments"
                     | otherwise      = [ x | EArg x <- as' ]
                  where as' = dropWhile isTArg as

            isTArg :: Arg -> Bool
            isTArg = \ case
                  TArg _ -> True
                  _      -> False

            peelArgs :: Annote -> Ty -> [Exp] -> Ty
            peelArgs an t es = peel an (length es) t

            peel :: Annote -> Int -> Ty -> Ty
            peel _ 0 t  = t
            peel an n t = case dstArrow t of
                  Just (_, u) -> peel an (n - 1) u
                  Nothing     -> ill an "term argument applied to a non-arrow"

            ill :: Annote -> String -> a
            ill an msg = error $ "Eidos.typeOf: ill-formed expression (" <> msg <> ") at " <> show an

-- | Evaluate a closed type-level natural: literals and the built-in
--   arithmetic constructors over evaluable operands.
evalNat :: Ty -> Maybe Natural
evalNat t = case t of
      TyNat _ n -> pure n
      _         -> case flattenTyApp t of
            (TyCon _ op, [a, b]) -> do
                  x <- evalNat a
                  y <- evalNat b
                  natOp op x y
            _                    -> Nothing
      where natOp :: Text -> Natural -> Natural -> Maybe Natural
            natOp op x y = case op of
                  "+" -> pure $ x + y
                  "*" -> pure $ x * y
                  "-" | x >= y -> pure $ x - y
                  _   -> Nothing

-- | Normalize a type by folding every closed type-level-natural subterm to
--   its literal. Type equality throughout the compiler is structural
--   equality after 'natNorm' (annotations are already ignored by 'Eq Ty').
natNorm :: Ty -> Ty
natNorm t
      | Just n <- evalNat t = TyNat (ann' t) n
      | otherwise           = case t of
            TyApp an a b -> TyApp an (natNorm a) (natNorm b)
            Arrow an a b -> Arrow an (natNorm a) (natNorm b)
            _            -> t
      where ann' :: Ty -> Annote
            ann' = \ case
                  TyNat an _ -> an
                  ty         -> ann ty

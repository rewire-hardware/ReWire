{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module Embedder.Atmo.Desugar (desugarAtmo) where

import ReWire.Annotation (Annote (..))
import ReWire.Error (MonadError, AstError, failAt)
import ReWire.HSE.Rename (Renamer, Exports)
import ReWire.SYB (Tr (TId, TM, T), transformTr, transform)

import Control.Monad (replicateM, (>=>))
import Control.Monad.State (evalStateT, MonadState (..), modify)
import Data.Text (pack, Text)

import Embedder.Atmo.Syntax (Exp (..), Module (..), Pat (..), Ty (..), Binds (..), PatBind (..), FunBinding (..), Defn (..), Poly (..))

import Embedder.Atmo.Util (mkTupleCtor, isPrim)

-- ! Record of Changes
-- 1. Name -> Text
-- 2. Exp Annote -> Exp
-- 3. Module Annote -> Module
-- 4. Pat Annote -> Pat
-- 5. Type Annote -> Ty
-- 6. ConDecl Annote -> DataCon
-- 7. Define DataHeader [just (name,vars) of DataDefn], and DeclHead Annote -> DataHeader
-- 8. Define Binds = [Defn], Binds Annote -> Binds
-- 9. Define PatBind = PatBind Pat Exp, PatBind Annote -> PatBind
-- 10. Define FunBinding = FunBinding [Text] Exp, Match Annote -> FunBinding
-- 11. Define Rhs/GuardedRhs/etc.
-- 12. Add If, Let to Atmo.Exp
-- 13. Add [record declarations] to Atmo, [record ops] to Atmo.Exp
--
-- ... Split Defn in Isabelle.Syntax into Fun, Definition, Function (Not sure why this would be directly relevant to this Atmo pass)
-- ... Break Decl into TypeSynonym, Defn, and DataDefn.

-- Need a replacement for Decl Annote for PatBind
-- Need a replacement for Decl Annote for TypeSig

data Desugar m = Desugar
      { dsModule   :: Tr m Module
      , dsPat      :: Tr m Pat
      , dsExp      :: Tr m Exp
      , dsType     :: Tr m Ty
      , dsBinds    :: Tr m Binds
      }

instance Monad m => Semigroup (Desugar m) where
      (<>) (Desugar f1 f2 f3 f4 f5)
           (Desugar g1 g2 g3 g4 g5)
            = Desugar (f1 <> g1)
                      (f2 <> g2)
                      (f3 <> g3)
                      (f4 <> g4)
                      (f5 <> g5)

instance Monad m => Monoid (Desugar m) where
      mempty = Desugar TId TId TId TId TId

pass :: Monad m => Desugar m -> Module -> m Module
pass (Desugar f1 f2 f3 f4 f5)
      =   transformTr f5
      >=> transformTr f4
      >=> transformTr f3
      >=> transformTr f2
      >=> transformTr f1

-- | Desugar into lambdas then normalize the lambdas.
desugarAtmo :: MonadError AstError m => Renamer -> (Module, Exports) -> m (Module, Exports)
desugarAtmo _ = liftDesugar $ flip evalStateT 0 .
      ( pure
        >=> pass desugarFuns
        >=> pass desugarTuples
        >=> pass flattenAlts -- again ... 1.b. remove this?
        >=> pass liftDiscriminator
      )
      where
      liftDesugar :: Monad m => (Module -> m Module) -> (Module,Exports) -> m (Module, Exports)
      liftDesugar f (m,exs) =  do
                                 m' <- f m
                                 return (m',exs)

type Fresh = Int

fresh :: (MonadState Fresh m, Monad m) => Annote -> m Text
fresh _l = do
      x <- get
      modify (+ 1)
      pure $ "ec" <> pack (show x)


-- AFTER: desugarFuns
-- | Turns tuples into applications of a TupleN constructor (also in types and pats):
-- > (x, y, z)
-- becomes
-- > (Tuple3 x y z)
desugarTuples :: Monad m => Desugar m
desugarTuples = mempty
      { dsExp = T $ \ case
            Tuple l _ _ es   -> App l Nothing Nothing (Con l Nothing  Nothing $ mkTupleCtor $ length es) es
            e              -> e
      , dsType = T $ \ case
            TyTuple l ts -> TyApp l (TyCon l $ mkTupleCtor $ length ts) ts
            t              -> t
      , dsPat = T $ \ case
            PatTuple l _ _ ps  -> PatCon l Nothing Nothing (mkTupleCtor $ length ps) ps
            p              -> p
      }

-- BEFORE: desugarTyFuns
-- AFTER: desugarInfix
-- | Turns piece-wise function definitions into a single PatBind with a lambda
--   and case expression on the RHS. E.g.:
-- > f p1 p2 = rhs1
-- > f q1 q2 = rhs2
-- becomes
-- > f = \ $1 $2 -> case ($1, $2) of { (p1, p2) -> rhs1; (q1, q2) -> rhs2 }
desugarFuns :: (MonadState Fresh m, MonadError AstError m) => Desugar m
desugarFuns = mempty
      { dsModule = TM $ \ case
            Module prags recs imps ds -> Module prags recs imps <$> mapM (desugarFun $ tySigMap ds) ds
      , dsBinds = TM $ \ case
            BDefs ds               -> BDefs <$> mapM (desugarFun $ tySigMap ds) ds
      }
      where desugarFun :: (MonadState Fresh m, MonadError AstError m) => [(Text, Poly)] -> Defn -> m Defn
            desugarFun ts = \ case
                  Defn l name pt mattr bs | isPrim name -> do
                        fb <- desugarPrimFunBinding l bs
                        pure $ Defn l name pt mattr [fb]
                  Defn l name pt mattr bs -> do
                        fb <- desugarFunBinding l ts bs
                        pure $ Defn l name pt mattr [fb]
            desugarFunBinding :: (MonadState Fresh m, MonadError AstError m) => Annote -> [(Text, Poly)] -> [FunBinding] -> m FunBinding
            desugarFunBinding l ts fbs@(FunBinding _ pats _ : _) = do
                        pbs <- mapM (toPatBind ts) fbs
                        e    <- buildLambda l pbs $ length pats
                        pure $ FunBinding l [] e
            desugarFunBinding l _ [] = failAt l "desugarFuns: empty function binding group"

            desugarPrimFunBinding :: (MonadState Fresh m, MonadError AstError m) => Annote -> [FunBinding] -> m FunBinding
            desugarPrimFunBinding _ (fb : _) = pure fb
            desugarPrimFunBinding l []       = failAt l "desugarFuns: empty function binding group for a primitive"


            buildLambda :: (MonadState Fresh m, MonadError AstError m) => Annote -> [PatBind] -> Int -> m Exp
            buildLambda l alts = \ case
                  1     -> do
                        x <- fresh l
                        -- NOTE: can't type-annotate params without expanding type synonyms.
                        pure $ Lam l Nothing Nothing [x] $ Case l Nothing Nothing (Var l Nothing Nothing x) alts
                  arity -> do
                        xs <- replicateM arity (fresh l)
                        -- NOTE: can't type-annotate params without expanding type synonyms.
                        pure $ Lam l Nothing Nothing xs $ Case l Nothing Nothing (Tuple l Nothing Nothing (map (Var l Nothing Nothing) xs)) alts

            toPatBind :: (MonadState Fresh m, MonadError AstError m) => [(Text, Poly)] -> FunBinding -> m PatBind
            toPatBind ts = \ case
                  -- NOTE: can't type-annotate params without expanding type synonyms.
                  FunBinding _  [p] rhs -> pure $ PatBind (annotatePVars ts p) rhs
                  FunBinding l' ps  rhs -> pure $ PatBind (PatTuple l' Nothing Nothing $ map (annotatePVars ts) ps) rhs

-- | Turns
-- > case e of {...}
-- into
-- > (\ x -> case x of {...}) e
liftDiscriminator :: MonadState Fresh m => Desugar m
liftDiscriminator = mempty {dsExp = TM $ \ case
      Case l mp mt e alts -> do
            x <- fresh l
            pure $ App l mp mt (Lam l Nothing Nothing [x] $ Case l mp mt (Var l Nothing Nothing x) alts) [e]
      e             -> pure e}


-- | Turn cases with multiple alts into cases with two alts: an alt with a
-- pattern and another with a default, wildcard branch.
-- > case x of
-- >   p1 -> e1
-- >   p2 -> e2
-- >   p3 -> e3
-- becomes
-- > case x of
-- >   p1 -> e1
-- >   _  -> case x of
-- >           p2 -> e2
-- >            _ -> case x of
-- >                   p3 -> e3
-- >                    _ -> undefined
flattenAlts :: Monad m => Desugar m
flattenAlts = mempty {dsExp = T $ \ case
      Case l mp mt e alts -> Case l mp mt e $ flatten l e alts
      e             -> e}
      where flatten :: Annote -> Exp -> [PatBind] -> [PatBind]
            flatten l e = \ case
                  [a@PatBind {}]                           -> [ a ]
                  as@[PatBind {}, PatBind (PatWildCard {}) _] -> as
                  (PatBind p' e' : as)         -> [PatBind p' e', PatBind (PatWildCard l Nothing Nothing) (Case l Nothing Nothing e $ flatten l e as)]
                  as                                   -> as

tySigMap :: [Defn] -> [(Text, Poly)]
tySigMap = map tySig
      where tySig :: Defn -> (Text, Poly)
            tySig = \ case
                  Defn _ n t _ _ -> (n,t)

-- TODO: this polytype is busted
annotatePVars :: [(Text, Poly)] -> Pat -> Pat
annotatePVars ts = transform $ \ case
      PatVar l _ _ n | Just t <- lookup n ts -> PatVar l (Just t) Nothing n
      n                                 -> n


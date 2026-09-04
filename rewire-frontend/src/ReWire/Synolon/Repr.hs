{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | The representable closure (doc/synolon.md §3.1): the fixed bit width
--   of a Synolon type. Computed once, here, for the two consumers that must
--   agree — the machine lint, which rejects a binder, cell, port, or halt
--   answer whose type has no width, and the fold, which lays values out at
--   that width — so the two agree on which types have a width (a type in a
--   position only the fold sizes, such as a definition's codomain or a
--   primitive's instantiation, gets the same diagnostic there).
--
--   Widths: @Vec n τ@ is @n · |τ|@ (@n@ nat-closed); @Finite n@ is
--   @nbits n@; @Integer@ is 128; @Proxy@ is 0; a tuple is the sum of its
--   components; a declared datatype is @nbits (#constructors)@ plus the
--   widest constructor payload (the sum of its field widths at the
--   instantiation); a type variable is 0 (the fold only sees closed types);
--   a recursive datatype, an open width, and a function type have no width.
module ReWire.Synolon.Repr (DataEnv (..), dataEnv, Sizes, sizeOf, sizeOfM, isTupleCon) where

import ReWire.BitVector (nbits)
import ReWire.Eidos.Pretty ()
import ReWire.Eidos.Syntax
import ReWire.Eidos.Types (flattenArrow, flattenTyApp, evalNat, natNorm, substTv)
import ReWire.Pretty (prettyPrint)

import Control.Monad.State.Strict (StateT, evalStateT, get, modify, lift)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List (genericLength)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.Text           as T
import qualified ReWire.Annotation   as Ann

-- | The datatype table sizing consults: each datatype's constructors in
--   declaration order (a constructor's tag is its index) and each
--   constructor's declared signature.
data DataEnv = DataEnv
      { deCtors   :: HashMap TyConId [DataConId]
      , deCtorSig :: HashMap DataConId Sig
      }

dataEnv :: [DataDefn] -> DataEnv
dataEnv datas = DataEnv
      { deCtors   = Map.fromList [ (dataName d, [ c | DataCon _ c _ <- dataCons d ]) | d <- datas ]
      , deCtorSig = Map.fromList [ (c, sig) | d <- datas, DataCon _ c sig <- dataCons d ]
      }

-- | The sizing memo (types compare structurally; annotations are ignored).
type Sizes = HashMap Ty Natural

-- | The width of a type, or the reason it has none.
sizeOf :: DataEnv -> Ty -> Either Text Natural
sizeOf de t = evalStateT (sizeOfM de t) mempty

-- | 'sizeOf' over an explicit memo (the fold sizes many types per program).
sizeOfM :: DataEnv -> Ty -> StateT Sizes (Either Text) Natural
sizeOfM de = go depth0 mempty
      where -- The visited set holds the datatype instances on the current
            -- descent: meeting one again is a recursive datatype. Types
            -- are compared after 'natNorm' (as the validator compares
            -- them), so an instance recursive through type arithmetic
            -- (@T (n + 0)@ under @T n@) is met again; the depth bound
            -- covers an unfolding that never revisits an instance.
            go :: Int -> HashSet Ty -> Ty -> StateT Sizes (Either Text) Natural
            go depth visited (natNorm -> t)
                  | depth <= 0 = lift $ Left $ "datatype unfolding too deep (is the datatype recursive through its type arguments?): " <> pp t
                  | otherwise  = do
                  m <- get
                  case Map.lookup t m of
                        Just sz -> pure sz
                        Nothing -> do
                              sz <- case flattenTyApp t of
                                    (TyCon _ "Vec", [n, te])
                                          | Just k <- evalNat n -> (k *) <$> go depth visited te
                                          | otherwise           -> lift $ Left $ "can't determine the size of a Vec. (" <> pp t <> ")"
                                    (TyCon _ "Finite", [n])
                                          | Just k <- evalNat n -> pure $ nbits k
                                          | otherwise           -> lift $ Left $ "can't determine the size of a Finite. (" <> pp t <> ")"
                                    (TyCon _ "Integer", [])     -> pure 128
                                    (TyCon _ "Proxy", _)        -> pure 0
                                    (TyCon _ c, args)
                                          | isTupleCon c           -> sum <$> mapM (go depth visited) args
                                          | t `Set.member` visited -> lift $ Left $ "can't determine the size of a recursive datatype: " <> c
                                          | Just ctors <- Map.lookup c (deCtors de) -> do
                                                ws <- mapM (ctorWidth (depth - 1) (Set.insert t visited) t) ctors
                                                pure $ nbits (genericLength ctors) + maximum (0 : ws)
                                    (TyVarT {}, _)              -> pure 0
                                    _                           -> lift $ Left $ "couldn't calculate the size of a type: " <> pp t
                              modify $ Map.insert t sz
                              pure sz

            -- A constructor's payload width at a concrete instance of its
            -- datatype: the field types, with the datatype's parameters
            -- matched against the instance.
            ctorWidth :: Int -> HashSet Ty -> Ty -> DataConId -> StateT Sizes (Either Text) Natural
            ctorWidth depth visited t d = case Map.lookup d (deCtorSig de) of
                  Just (Sig _ ct) -> do
                        let (targs, tres) = flattenArrow ct
                        sub <- lift $ matchTy tres t
                        sum <$> mapM (go depth visited . substTv sub) targs
                  Nothing         -> pure 0

            -- Nesting depth of datatype unfoldings, far beyond any real
            -- program's.
            depth0 :: Int
            depth0 = 10000

            pp :: Ty -> Text
            pp = prettyPrint . Ann.unAnn

-- | Match a constructor's declared result type against a concrete instance.
matchTy :: Ty -> Ty -> Either Text (HashMap TyVar Ty)
matchTy (TyApp _ t1 t2) (TyApp _ t1' t2') = do
      s1 <- matchTy t1 t1'
      s2 <- matchTy t2 t2'
      if and (Map.intersectionWith (==) s1 s2)
            then pure $ Map.union s1 s2
            else Left "inconsistent assignment of a type variable in a constructor signature (rwc bug)"
matchTy (TyVarT _ v) t = pure $ Map.singleton v t
matchTy _ _            = pure mempty

-- | The tuple type constructors, @()@ included (the zero-component tuple).
isTupleCon :: Text -> Bool
isTupleCon c = T.length c >= 2 && T.head c == '(' && T.last c == ')'
            && T.all (== ',') (T.init $ T.tail c)

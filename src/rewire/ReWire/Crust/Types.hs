{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.Types
      ( TypeAnnotated (typeOf, tyAnn, setTyAnn), tupleTy
      , arr, intTy, strTy, nilTy, refTy, kmonad, (|->)
      , rangeTy, flattenArrow, pairTy, arrowRight, arrowLeft
      , higherOrder, fundamental, concrete, paramTys
      , proxyNat, vecSize, vecElemTy, vecTy, evalNat
      , mkArrowTy, poly, poly', listTy, kblank, plusTy, plus
      , isResMonad, isStateMonad, flattenAllTyApp, resInputTy
      ) where

import ReWire.Annotation (Annote (MsgAnnote), Annotated (ann))
import ReWire.Crust.Syntax (Exp (..), Ty (..), Poly (Poly), Pat (..), Kind (..), MatchPat (..), flattenTyApp)
import ReWire.Unbound (Name, Embed (Embed), unsafeUnbind, n2s, s2n, fv, bind)

import Data.Containers.ListUtils (nubOrd)
import Numeric.Natural (Natural)

--- TypeAnnotated instances

class TypeAnnotated a where
      typeOf   :: a -> Maybe Ty
      tyAnn    :: a -> Maybe Poly
      setTyAnn :: Maybe Poly -> a -> a

instance TypeAnnotated Exp where
      typeOf = \ case
            App _ _ t _ _         -> t
            Lam _ _ t e           -> arr <$> t <*> typeOf (snd $ unsafeUnbind e)
            Var _ _ t _           -> t
            Con _ _ t _           -> t
            Case _ _ t _ _ _      -> t
            Match _ _ t _ _ _ _   -> t
            Builtin _ _ t _       -> t
            LitInt a _ _          -> pure $ intTy a
            LitStr a _ _          -> pure $ strTy a
            LitList _ _ t _       -> t
            LitVec _ _ t _        -> t
      tyAnn = \ case
            App _ pt _ _ _        -> pt
            Lam _ pt _ _          -> pt
            Var _ pt _ _          -> pt
            Con _ pt _ _          -> pt
            Case _ pt _ _ _ _     -> pt
            Match _ pt _ _ _ _ _  -> pt
            Builtin _ pt _ _      -> pt
            LitInt _ pt _         -> pt
            LitStr _ pt _         -> pt
            LitList _ pt _ _      -> pt
            LitVec _ pt _ _       -> pt
      setTyAnn pt = \ case
            App a _ t e1 e2       -> App a pt t e1 e2
            Lam a _ t e           -> Lam a pt t e
            Var a _ t e           -> Var a pt t e
            Con a _ t e           -> Con a pt t e
            Case a _ t e e1 e2    -> Case a pt t e e1 e2
            Match a _ t e p e1 e2 -> Match a pt t e p e1 e2
            Builtin a _ t b       -> Builtin a pt t b
            LitInt a _ n          -> LitInt a pt n
            LitStr a _ n          -> LitStr a pt n
            LitList a _ t n       -> LitList a pt t n
            LitVec a _ t n        -> LitVec a pt t n

instance TypeAnnotated Pat where
      typeOf = \ case
            PatCon _ _ (Embed t) _ _   -> t
            PatVar _ _ (Embed t) _     -> t
            PatWildCard _ _ (Embed t)  -> t
      tyAnn = \ case
            PatCon _ (Embed pt) _ _ _  -> pt
            PatVar _ (Embed pt) _ _    -> pt
            PatWildCard _ (Embed pt) _ -> pt
      setTyAnn pt = \ case
            PatCon a _ t c ps          -> PatCon a (Embed pt) t c ps
            PatVar a _ t x             -> PatVar a (Embed pt) t x
            PatWildCard a _ t          -> PatWildCard a (Embed pt) t

instance TypeAnnotated MatchPat where
      typeOf = \ case
            MatchPatCon _ _ t _ _   -> t
            MatchPatVar _ _ t       -> t
            MatchPatWildCard _ _ t  -> t
      tyAnn = \ case
            MatchPatCon _ pt _ _ _  -> pt
            MatchPatVar _ pt _      -> pt
            MatchPatWildCard _ pt _ -> pt
      setTyAnn pt = \ case
            MatchPatCon a _ t c ps -> MatchPatCon a pt t c ps
            MatchPatVar a _ t      -> MatchPatVar a pt t
            MatchPatWildCard a _ t -> MatchPatWildCard a pt t

---

kblank :: Kind
kblank = KVar $ s2n "_"

kmonad :: Kind
kmonad = KStar `KFun` KStar

arr :: Ty -> Ty -> Ty
arr t = TyApp (ann t) (TyApp (ann t) (TyCon (ann t) $ s2n "->") t)

infixr 1 `arr`

intTy :: Annote -> Ty
intTy an = TyCon an $ s2n "Integer"

strTy :: Annote -> Ty
strTy an = TyCon an $ s2n "String"

poly :: [Name Ty] -> Ty -> Poly
poly vs t = Poly $ bind vs t

poly' :: Ty -> Poly
poly' t = poly (nubOrd $ fv t) t

(|->) :: [Name Ty] -> Ty -> Embed Poly
vs |-> t = Embed $ poly vs t

infix 1 |->

flattenArrow :: Ty -> ([Ty], Ty)
flattenArrow = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2 -> (t1 : ts, t)
            where (ts, t) = flattenArrow t2
      t                                               -> ([], t)

paramTys :: Ty -> [Ty]
paramTys = fst . flattenArrow

rangeTy :: Ty -> Ty
rangeTy = snd . flattenArrow

flattenAllTyApp :: Ty -> [Ty]
flattenAllTyApp = \ case
      TyApp _ t t' -> flattenAllTyApp t <> flattenAllTyApp t'
      t            -> [t]

listTy :: Annote -> Ty -> Ty
listTy an = TyApp an $ TyCon an $ s2n "[_]"

refTy :: Annote -> Ty -> Ty
refTy an = TyApp an $ TyCon an $ s2n "Ref"

vecTy :: Annote -> Ty -> Ty -> Ty
vecTy an n = TyApp an $ TyApp an (TyCon an $ s2n "Vec") n

vecElemTy :: Ty -> Maybe Ty
vecElemTy t = case flattenTyApp t of
      TyCon _ (n2s -> "Vec") : [_, c] -> pure c
      _                               -> Nothing

vecSize :: Ty -> Maybe Natural
vecSize t = case flattenTyApp t of
      TyCon _ (n2s -> "Vec") : [n, _] -> evalNat n
      _                               -> Nothing

proxyNat :: Ty -> Maybe Natural
proxyNat t = case flattenTyApp t of
      TyCon _ (n2s -> "Proxy") : [n] -> evalNat n
      _                              -> Nothing

plusTy :: Annote -> Ty -> Ty -> Ty
plusTy an n = TyApp an $ TyApp an (TyCon an $ s2n "+") n

plus :: Ty -> Maybe (Ty, Ty)
plus = \ case
      TyApp _ (TyApp _ c t1) t2 | isPlus c -> pure (t1, t2)
      _                                    -> Nothing
      where isPlus :: Ty -> Bool
            isPlus = \ case
                  TyCon _ (n2s -> "+") -> True
                  _                    -> False

evalNat :: Ty -> Maybe Natural
evalNat = \ case
      TyNat _ n               -> pure n
      (plus -> Just (n1, n2)) -> (+) <$> evalNat n1 <*> evalNat n2
      _                       -> Nothing

-- | Given 'a -> (b -> c)' returns 'b -> c'.
arrowRight :: Ty -> Ty
arrowRight = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) t' -> t'
      t                                              -> t

-- | Given 'a -> (b -> c)' returns 'a'.
arrowLeft :: Ty -> Ty
arrowLeft = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t') _ -> t'
      t                                              -> t

-- | Takes [T1, ..., Tn-1] Tn and returns (T1 -> (T2 -> ... (T(n-1) -> Tn) ...))
mkArrowTy :: [Ty] -> Ty -> Ty
mkArrowTy ps = foldr1 arr . (ps ++) . (: [])

nilTy :: Ty
nilTy = TyCon (MsgAnnote "nilTy") (s2n "()")

pairTy :: Annote -> Ty -> Ty -> Ty
pairTy an t = TyApp an $ TyApp an (TyCon an $ s2n "(,)") t

tupleTy :: Annote -> [Ty] -> Ty
tupleTy an = foldr (pairTy an) nilTy

isResMonad :: Ty -> Bool
isResMonad ty = case rangeTy ty of
      TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) _) _) _) _ -> True
      _                                                                        -> False

resInputTy :: Ty -> Maybe Ty
resInputTy ty = case rangeTy ty of
      TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) ip) _) _) _ -> Just ip
      _                                                                         -> Nothing

isStateMonad :: Ty -> Bool
isStateMonad ty = case rangeTy ty of
      TyApp an (TyApp _ (TyApp _ (TyCon _ (n2s -> "StateT")) _) m) a -> isStateMonad (TyApp an m a)
      TyApp _ (TyCon _ (n2s -> "Identity")) _                        -> True
      _                                                              -> False

-- | Types containing no type variables (or blanks).
concrete :: Ty -> Bool
concrete = \ case
      TyVar {}    -> False
      TyCon {}    -> True
      TyNat {}    -> True
      TyApp _ a b -> concrete a && concrete b

-- | Types with no built-ins (Strings, Integers, lists).
fundamental :: Ty -> Bool
fundamental = \ case
      TyCon _ (n2s -> "String")  -> False
      TyCon _ (n2s -> "Integer") -> False
      TyCon _ (n2s -> "[_]")     -> False
      TyNat {}                   -> True
      TyCon {}                   -> True
      TyVar {}                   -> True
      TyApp _ a b                -> fundamental a && fundamental b

higherOrder :: Ty -> Bool
higherOrder (flattenArrow -> (ats, rt)) = any isArrow $ rt : ats

isArrow :: Ty -> Bool
isArrow = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) _ -> True
      _                                             -> False

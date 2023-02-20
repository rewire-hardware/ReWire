{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.Types
      ( TypeAnnotated (typeOf, tyAnn, setTyAnn), tupleTy
      , arr, intTy, strTy, nilTy, refTy, kmonad, (|->)
      , rangeTy, flattenArrow, pairTy, arrowRight, arrowLeft
      , higherOrder, fundamental, concrete, paramTys
      , proxyNat, vecSize, vecElemTy, vecTy, evalNat
      , mkArrowTy, poly, poly', listTy, kblank, plusTy, plus
      , isReacT, isStateT, ctorNames, resInputTy
      , dstArrow, dstStateT, dstTyApp, dstReacT, proxyTy
      , dstNegTy, negTy, dstPoly1, Poly1, minusP1, zeroP1, pickVar, poly1Ty
      ) where

import ReWire.Annotation (Annote (MsgAnnote), Annotated (ann), noAnn)
import ReWire.Crust.Syntax (Exp (..), Ty (..), Poly (Poly), Pat (..), Kind (..), MatchPat (..), flattenTyApp, TyConId)
import ReWire.Unbound (Name, Embed (Embed), unsafeUnbind, n2s, s2n, fv, bind)
import ReWire.Pretty (Pretty (pretty), Doc, hsep, text, punctuate, parens, showt)

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (hash))
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust)
import Data.Ratio (numerator, denominator, (%))
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map

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

arr :: Ty -> Ty -> Ty
arr t = TyApp (ann t) (TyApp (ann t) (TyCon (ann t) $ s2n "->") t)

infixr 1 `arr`

flattenArrow :: Ty -> ([Ty], Ty)
flattenArrow = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2 -> (t1 : ts, t)
            where (ts, t) = flattenArrow t2
      t                                               -> ([], t)

paramTys :: Ty -> [Ty]
paramTys = fst . flattenArrow

rangeTy :: Ty -> Ty
rangeTy = snd . flattenArrow

isArrow :: Ty -> Bool
isArrow = isJust . dstArrow

dstArrow :: Ty -> Maybe (Ty, Ty)
dstArrow = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2 -> pure (t1, t2)
      _                                               -> Nothing

-- | Given 'a -> (b -> c)' returns 'b -> c'.
arrowRight :: Ty -> Ty
arrowRight t = fromMaybe t $ snd <$> dstArrow t

-- | Given 'a -> (b -> c)' returns 'a'.
arrowLeft :: Ty -> Ty
arrowLeft t = fromMaybe t $ fst <$> dstArrow t

ctorNames :: Ty -> [Name TyConId]
ctorNames = \ case
      TyCon _ n    -> [n]
      TyApp _ t t' -> ctorNames t <> ctorNames t'
      _            -> []

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

proxyTy :: Annote -> Natural -> Ty
proxyTy an n = TyApp an (TyCon an (s2n "Proxy")) $ TyNat an n

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

dstNegTy :: Ty -> Maybe Ty
dstNegTy = \ case
      TyApp _ c t | isNeg c -> pure t
      _                     -> Nothing
      where isNeg :: Ty -> Bool
            isNeg = \ case
                  TyCon _ (n2s -> "-") -> True
                  _                    -> False

negTy :: Ty -> Ty
negTy = TyApp noAnn (TyCon noAnn $ s2n "-")

evalNat :: Ty -> Maybe Natural
evalNat t | Just (Poly1 r cs) <- dstPoly1 t
          , r >= 0, denominator r == 1, cs == mempty = pure $ fromIntegral $ numerator r
          | otherwise                                = Nothing

-- | Takes [T1, ..., Tn-1] Tn and returns (T1 -> (T2 -> ... (T(n-1) -> Tn) ...))
mkArrowTy :: [Ty] -> Ty -> Ty
mkArrowTy ps = foldr1 arr . (ps ++) . (: [])

nilTy :: Ty
nilTy = TyCon (MsgAnnote "nilTy") (s2n "()")

pairTy :: Annote -> Ty -> Ty -> Ty
pairTy an t = TyApp an $ TyApp an (TyCon an $ s2n "(,)") t

tupleTy :: Annote -> [Ty] -> Ty
tupleTy an = foldr (pairTy an) nilTy

isReacT :: Ty -> Bool
isReacT = isJust . dstReacT . rangeTy

resInputTy :: Ty -> Maybe Ty
resInputTy ty = case rangeTy ty of
      TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) ip) _) _) _ -> Just ip
      _                                                                         -> Nothing

isStateT :: Ty -> Bool
isStateT ty = case rangeTy ty of
      TyApp an (TyApp _ (TyApp _ (TyCon _ (n2s -> "StateT")) _) m) a -> isStateT (TyApp an m a)
      TyApp _ (TyCon _ (n2s -> "Identity")) _                        -> True
      _                                                              -> False

-- | This takes a type of the form
-- >  StateT S1 (StateT S2 (... (StateT Sm I)))
-- and returns
-- >  [S1, ..., Sm]
dstStateT :: Ty -> Maybe [Ty]
dstStateT = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "StateT")) s) m -> (s :) <$> dstStateT m
      TyCon _ (n2s -> "Identity")                       -> pure []
      _                                                 -> Nothing

-- | This takes a type of the form
-- >  m a
-- and returns
-- >  Just (m, a)
dstTyApp :: Ty -> Maybe (Ty, Ty)
dstTyApp = \ case
      TyApp _ m a -> pure (m, a)
      _           -> Nothing

dstTyBinOp :: Ty -> Maybe (Name TyConId, Ty, Ty)
dstTyBinOp = \ case
      TyApp _ (TyApp _ (TyCon _ op) t1) t2 -> pure (op, t1, t2)
      _                                    -> Nothing

-- | This takes a type of the form
-- >  ReacT In Out (StateT S1 (StateT S2 (... (StateT Sm I)))) T
-- and returns
-- >  (In, Out, [S1, ..., Sm], T)
dstReacT :: Ty -> Maybe (Ty, Ty, [Ty], Ty)
dstReacT = \ case
      TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) i) o) m) a -> dstStateT m >>= \ ms -> pure (i, o, ms, a)
      _                                                                        -> Nothing


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

-- Degree-1 polynomial with rational coefficients.
data Poly1 = Poly1 Rational (HashMap (Name Ty) Rational)
      deriving (Show)

instance Eq Poly1 where
      (normP1 -> Poly1 r cs) == (normP1 -> Poly1 r' cs') = r == r' && cs == cs'

instance Pretty Poly1 where
      pretty = \ case
            (normP1 -> Poly1 r (Map.toList -> cs)) | r == 0 -> hsep $ punctuate (text " +") $ map pretty' cs
            (normP1 -> Poly1 r (Map.toList -> cs))          -> hsep $ punctuate (text " +") $ rat r : map pretty' cs
            where pretty' :: (Name Ty, Rational) -> Doc a
                  pretty' (x, cx) | cx == 1   = pretty x
                                  | otherwise = rat cx <> pretty x

                  rat :: Rational -> Doc a
                  rat r | denominator r == 1 = text $ showt $ numerator r
                        | otherwise          = parens $ text $ showt r

zeroP1 :: Poly1
zeroP1 = Poly1 0 mempty

normP1 :: Poly1 -> Poly1
normP1 (Poly1 r cs) = Poly1 r $ Map.filter (/= 0) cs

monoP1 :: Rational -> Maybe (Name Ty) -> Poly1
monoP1 r = \ case
      Just x  -> Poly1 0 $ Map.singleton x r
      Nothing -> Poly1 r mempty

minusP1 :: Poly1 -> Poly1 -> Poly1
minusP1 p1 p2 = plusP1 p1 $ multP1 (-1) p2

plusP1 :: Poly1 -> Poly1 -> Poly1
plusP1 (Poly1 r cs) (Poly1 r' cs') = normP1 $ Poly1 (r + r') $ Map.unionWith (+) cs cs'

multP1 :: Rational -> Poly1 -> Poly1
multP1 r' (Poly1 r cs) = Poly1 (r' * r) $ Map.map (r' *) cs

pickVar :: Poly1 -> Maybe (Name Ty, Poly1)
pickVar = pickVar' . normP1
      where pickVar' :: Poly1 -> Maybe (Name Ty, Poly1)
            pickVar' p | ((x, cx) : cs) <- vars p = pure (x, multP1 ((-1) / cx) $ setCoeffs (Map.fromList cs) p)
                       | otherwise                = Nothing

            vars :: Poly1 -> [(Name Ty, Rational)]
            vars (Poly1 _ cs) = sort' $ Map.toList cs

            setCoeffs :: HashMap (Name Ty) Rational -> Poly1 -> Poly1
            setCoeffs cs (Poly1 r _) = Poly1 r cs

            sort' :: [(Name Ty, Rational)] -> [(Name Ty, Rational)]
            sort' = sortOn (hash . show . fst)

poly1Ty :: Poly1 -> Ty
poly1Ty (Poly1 r cs) | (c : cs') <- Map.toList cs, r == 0 = foldr plus' (mono c) $ map mono cs'
                     | (c : cs') <- Map.toList cs         = ratTy r `plus'` foldr plus' (mono c) (map mono cs')
                     | otherwise                          = ratTy r
      where mono :: (Name Ty, Rational) -> Ty
            mono (x, cx) | cx == 1   = tyVar x
                         | otherwise = multTy cx $ tyVar x

            plus' :: Ty -> Ty -> Ty
            plus' = plusTy noAnn

            tyVar :: Name Ty -> Ty
            tyVar x = TyVar noAnn KNat x

ratTy :: Rational -> Ty
ratTy r | r < 0              = negTy $ posRatTy (-r)
        | otherwise          = posRatTy r
      where posRatTy :: Rational -> Ty
            posRatTy r' | denominator r' == 1 = TyNat noAnn $ fromIntegral $ numerator r'
                        | otherwise           = TyApp noAnn (TyApp noAnn (TyCon noAnn $ s2n "/") $ TyNat noAnn $ fromIntegral $ numerator r') $ TyNat noAnn $ fromIntegral $ denominator r'

multTy :: Rational -> Ty -> Ty
multTy r = TyApp noAnn $ TyApp noAnn (TyCon noAnn $ s2n "*") $ ratTy r

dstRat :: Ty -> Maybe Rational
dstRat = \ case
      TyNat _ n             -> pure $ fromIntegral n
      (dstNegTy -> Just t') -> ((-1) *) <$> dstRat t'
      (dstTyBinOp -> Just (n2s -> "/", TyNat _ a, TyNat _ b))
                            -> pure $ fromIntegral a % fromIntegral b
      _                     -> Nothing

dstPoly1 :: Ty -> Maybe Poly1
dstPoly1 = \ case
      TyNat _ n               -> pure $ monoP1 (fromIntegral n) Nothing
      TyVar _ _ x             -> pure $ monoP1 1 $ Just x
      (dstNegTy -> Just t)    -> multP1 (-1) <$> dstPoly1 t
      (plus -> Just (t1, t2)) -> plusP1 <$> dstPoly1 t1 <*> dstPoly1 t2
      (rmult -> Just (r, t))  -> multP1 r <$> dstPoly1 t
      (rdiv -> Just (t, r))   -> multP1 (1/r) <$> dstPoly1 t
      _                       -> Nothing
      where rmult :: Ty -> Maybe (Rational, Ty)
            rmult = \ case
                  (dstTyBinOp -> Just (n2s -> "*", dstRat -> Just r, t)) -> pure (r, t)
                  _                                                      -> Nothing

            rdiv :: Ty -> Maybe (Ty, Rational)
            rdiv = \ case
                  (dstTyBinOp -> Just (n2s -> "/", t, dstRat -> Just r)) -> pure (t, r)
                  _                                                      -> Nothing

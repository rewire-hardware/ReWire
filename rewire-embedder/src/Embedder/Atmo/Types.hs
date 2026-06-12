{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module Embedder.Atmo.Types
      ( TypeAnnotated (typeOf, tyAnn, setTyAnn), tupleTy
      , arr, intTy, strTy, nilTy, refTy, (|->)
      , rangeTy, sig, flattenSig, pairTy, arrowRight, arrowLeft
      , higherOrder, fundamental, concrete, paramTys
      , finMax, proxyNat, finiteTy, vecSize, vecElemTy, vecTy, evalNat
      , mkArrowTy, mkTyApp, poly, poly', listTy, plusTy, dstPlusTy
      , isReacT, isStateT, ctorNames, resInputTy
      , dstArrow, dstStateT, dstTyApp, dstReacT, proxyTy
      , dstNegTy, negTy, dstPoly1, Poly1, minusP1, zeroP1, pickVar, poly1Ty
      ) where

import ReWire.Annotation (Annote (MsgAnnote), Annotated (ann), noAnn)
import Embedder.Atmo.Syntax (Exp (..), Ty (..), TyBuiltin (..), Poly (Poly), Pat (..))
import Embedder.Builtins (tb2s)
import ReWire.Pretty (Pretty (pretty), Doc, hsep, text, punctuate, parens, showt)

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (hash))
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Ratio (numerator, denominator, (%))
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import Data.Text (Text)

--- TypeAnnotated instances

-- Name Handling:
      -- remove Embed
      -- remove s2n and n2s
      -- Replace bindings with vs and body
      -- fix defs of `poly` and `|->`
      -- define fv for types

class TypeAnnotated a where
      typeOf   :: a -> Maybe Ty
      tyAnn    :: a -> Maybe Poly
      setTyAnn :: Maybe Poly -> a -> a

instance TypeAnnotated Exp where
      typeOf = \ case
            App _ _ t _ _         -> t
            Lam _ _ t _ _        -> t
            Var _ _ t _           -> t
            Con _ _ t _           -> t
            Case _ _ t _ _        -> t
            RWUser _ _ t _        -> t
            LitInt a _ _          -> pure $ intTy a
            LitStr a _ _          -> pure $ strTy a
            LitList _ _ t _       -> t
            LitVec _ _ t _        -> t
            Tuple _ _ t _         -> t
            If _ _ t _ _ _        -> t
            Let _ _ t _ _         -> t
            RecVal _ _ t _        -> t
            RecUpd _ _ t _ _      -> t
            RecSel _ _ t _ _      -> t
      
      tyAnn = \ case
            App _ pt _ _ _        -> pt
            Lam _ pt _ _ _        -> pt
            Var _ pt _ _          -> pt
            Con _ pt _ _          -> pt
            Case _ pt _ _ _       -> pt
            RWUser _ pt _ _       -> pt
            LitInt _ pt _         -> pt
            LitStr _ pt _         -> pt
            LitList _ pt _ _      -> pt
            LitVec _ pt _ _       -> pt
            Tuple _ pt _ _        -> pt
            If _ pt _ _ _ _       -> pt
            Let _ pt _ _ _        -> pt
            RecVal _ pt _ _       -> pt
            RecUpd _ pt _ _ _     -> pt
            RecSel _ pt _ _ _     -> pt
      setTyAnn pt = \ case
            App a _ t e1 e2       -> App a pt t e1 e2
            Lam a _ t v e         -> Lam a pt t v e
            Var a _ t e           -> Var a pt t e
            Con a _ t e           -> Con a pt t e
            Case a _ t e pbs      -> Case a pt t e pbs
            RWUser a _ t b        -> RWUser a pt t b
            LitInt a _ n          -> LitInt a pt n
            LitStr a _ n          -> LitStr a pt n
            LitList a _ t n       -> LitList a pt t n
            LitVec a _ t n        -> LitVec a pt t n
            Tuple a _ t n         -> Tuple a pt t n
            If a _ t tst con alt       -> If a pt t tst con alt
            Let a _ t b e         -> Let a pt t b e
            RecVal a _ t fs       -> RecVal a pt t fs
            RecUpd a _ t e fs     -> RecUpd a pt t e fs
            RecSel a _ t f e      -> RecSel a pt t f e

instance TypeAnnotated Pat where
      typeOf = \ case
            PatCon _ _ t _ _   -> t
            PatVar _ _ t _     -> t
            PatWildCard _ _ t  -> t
            PatTuple _ _ t _   -> t
            PatAs _ _ t _ _    -> t
            PatRec _ _ t _     -> t
      tyAnn = \ case
            PatCon _ pt _ _ _  -> pt
            PatVar _ pt _ _    -> pt
            PatWildCard _ pt _ -> pt
            PatTuple _ pt _ _  -> pt
            PatAs _ pt _ _ _   -> pt
            PatRec _ pt _ _    -> pt
      setTyAnn pt = \ case
            PatCon a _ t c ps  -> PatCon a pt t c ps
            PatVar a _ t x     -> PatVar a pt t x
            PatWildCard a _ t  -> PatWildCard a pt t
            PatTuple a _ t ps  -> PatTuple a pt t ps
            PatAs a _ t n p    -> PatAs a pt t n p
            PatRec a _ t fs    -> PatRec a pt t fs

---

intTy :: Annote -> Ty
intTy an = TyCon an "Integer"

strTy :: Annote -> Ty
strTy an = TyCon an "String"

poly :: [Text] -> Ty -> Poly
poly = Poly

fv :: Ty -> [Text]
fv = \ case
      TyApp _a t1 ts -> fv t1 ++ concatMap fv ts
      TyVar _a n -> [n]
      _ -> []

poly' :: Ty -> Poly
poly' t = poly (nubOrd $ fv t) t

(|->) :: [Text] -> Ty -> Poly
vs |-> t = poly vs t

infix 1 |->

arr :: Ty -> Ty -> Ty
arr t1 t2 = TyApp (ann t1) (TyBuiltin (ann t1) TyFun) [t1, t2]

infixr 1 `arr`

sig :: [Ty] -> Ty -> Ty
sig [] u = u
sig (t:ts) u = TyApp (ann t) (TyBuiltin (ann t) TyFun) [t,sig ts u]

flattenSig :: Ty -> ([Ty], Ty)
flattenSig = \ case
      TyApp _ (TyBuiltin _ TyFun) [t1,t2] -> (t1 : ts, t)
            where (ts, t) = flattenSig t2
      t                                               -> ([], t)

paramTys :: Ty -> [Ty]
paramTys = fst . flattenSig

rangeTy :: Ty -> Ty
rangeTy = snd . flattenSig

isArrow :: Ty -> Bool
isArrow = isJust . dstArrow

dstArrow :: Ty -> Maybe (Ty, Ty)
dstArrow = \ case
      TyApp _ (TyBuiltin _ TyFun) [t1,t2] -> pure (t1, t2)
      _                                   -> Nothing

-- | Given 'a -> (b -> c)' returns 'b -> c'.
arrowRight :: Ty -> Ty
arrowRight t = maybe t snd (dstArrow t)

-- | Given 'a -> (b -> c)' returns 'a'.
arrowLeft :: Ty -> Ty
arrowLeft t = maybe t fst (dstArrow t)

ctorNames :: Ty -> [Text]
ctorNames = \ case
      TyCon _ n    -> [n]
      TyApp _ t ts -> ctorNames t <> concatMap ctorNames ts
      _            -> []

unop :: TyBuiltin -> Annote -> Ty -> Ty
unop tb a b = TyApp a (TyBuiltin a tb) [b]

binop :: TyBuiltin -> Annote -> Ty -> Ty -> Ty
binop tb a b c = TyApp a (TyBuiltin a tb) [b,c]


listTy :: Annote -> Ty -> Ty
listTy = unop TyList

refTy :: Annote -> Ty -> Ty
refTy = unop TyRef

vecTy :: Annote -> Ty -> Ty -> Ty
vecTy = binop TyVec

vecElemTy :: Ty -> Maybe Ty
vecElemTy = \ case
      TyApp _ (TyBuiltin _ TyVec) [_, c] -> pure c
      _                                  -> Nothing

vecSize :: Ty -> Maybe Natural
vecSize = \ case
      TyApp _ (TyBuiltin _ TyVec) [n, _] -> evalNat n
      _                                  -> Nothing

proxyNat :: Ty -> Maybe Natural
proxyNat = \ case
      TyApp _ (TyBuiltin _ TyProxy) [n] -> evalNat n
      _                                 -> Nothing

finMax :: Ty -> Maybe Natural
finMax = \ case
      TyApp _ (TyBuiltin _ TyFin) [n] -> evalNat n
      _                               -> Nothing

proxyTy :: Annote -> Natural -> Ty
proxyTy an n = unop TyProxy an $ TyNat an n

finiteTy :: Annote -> Natural -> Ty
finiteTy an n = unop TyFin an $ TyNat an n

plusTy' :: Annote -> Ty -> Ty -> Ty
plusTy' = binop TyPlus

plusTy :: Annote -> Natural -> Natural -> Ty
plusTy an m n = plusTy' an (TyNat an m) (TyNat an n)

dstPlusTy :: Ty -> Maybe (Ty, Ty)
dstPlusTy = \ case
      TyApp _ c [t1,t2] | isPlus c -> pure (t1, t2)
      _                                    -> Nothing
      where isPlus :: Ty -> Bool
            isPlus = \ case
                  TyCon _ "+" -> True
                  TyBuiltin _ TyPlus -> True
                  _                    -> False

negTy :: Annote -> Ty -> Ty
negTy an t = TyApp an (TyCon an "-") [t]

dstNegTy :: Ty -> Maybe Ty
dstNegTy = \ case
      TyApp _ c [t] | isNeg c -> pure t
      _                     -> Nothing
      where isNeg :: Ty -> Bool
            isNeg = \ case
                  TyCon _ "-" -> True
                  _                    -> False



evalNat :: Ty -> Maybe Natural
evalNat t | Just (Poly1 r cs) <- dstPoly1 t
          , r >= 0, denominator r == 1, cs == mempty = pure $ fromIntegral $ numerator r
          | otherwise                                = Nothing

-- | Takes [T1, ..., Tn-1] Tn and returns (T1 -> (T2 -> ... (T(n-1) -> Tn) ...))
mkArrowTy :: [Ty] -> Ty -> Ty
mkArrowTy ps = foldr1 arr . (ps ++) . (: [])

mkTyApp :: Annote -> Ty -> [Ty] -> Ty
mkTyApp _ f []     = f
mkTyApp ann f args = foldl (\acc x -> TyApp ann acc [x]) f args

nilTy :: Ty
nilTy = TyBuiltin (MsgAnnote "nilTy") TyUnit

pairTy :: Annote -> Ty -> Ty -> Ty
pairTy an t1 t2 = TyApp an (TyBuiltin an TyProd) [t1,t2]

tupleTy :: Annote -> [Ty] -> Ty
tupleTy = TyTuple

isReacT :: Ty -> Bool
isReacT = isJust . dstReacT . rangeTy

resInputTy :: Ty -> Maybe Ty
resInputTy ty = case rangeTy ty of
      TyApp _ (TyBuiltin _ TyReacT) (i : _) -> Just i
      _                                     -> Nothing

isStateT :: Ty -> Bool
isStateT ty = case rangeTy ty of
      TyApp an (TyBuiltin _ TyStateT) [_,m,a] -> isStateT (TyApp an m [a])
      TyApp _ (TyBuiltin _ TyIdentity) _      -> True
      _                                       -> False

-- | This takes a type of the form
-- >  StateT S1 (StateT S2 (... (StateT Sm I)))
-- and returns
-- >  [S1, ..., Sm]
dstStateT :: Ty -> Maybe [Ty]
dstStateT = \ case
      TyApp _ (TyBuiltin _ TyStateT) (s:m:_) -> (s :) <$> dstStateT m
      TyBuiltin _ TyIdentity                 -> pure []
      _                                      -> Nothing

-- | This takes a type of the form
-- >  m a
-- and returns
-- >  Just (m, a)
dstTyApp :: Ty -> Maybe (Ty, Ty)
dstTyApp = \ case
      TyApp _ m [a]     -> pure (m,a)
      TyApp an m (a:as) -> pure (m,TyApp an a as)
      _                 -> Nothing

dstTyBinOp :: Ty -> Maybe (Text, Ty, Ty)
dstTyBinOp = \ case
      TyApp _ (TyCon _ op) [t1,t2]         -> pure (op, t1, t2)
      TyApp an (TyCon _ op) (t1:t2:ts)     -> pure (op, t1, TyApp an t2 ts)
      TyApp _ (TyBuiltin _ op) [t1,t2]     -> pure (tb2s op, t1, t2)
      TyApp an (TyBuiltin _ op) (t1:t2:ts) -> pure (tb2s op, t1, TyApp an t2 ts)
      _                                    -> Nothing

-- | This takes a type of the form
-- >  ReacT In Out (StateT S1 (StateT S2 (... (StateT Sm I)))) T
-- and returns
-- >  (In, Out, [S1, ..., Sm], T)
dstReacT :: Ty -> Maybe (Ty, Ty, [Ty], Ty)
dstReacT = \ case
      TyApp _ (TyBuiltin _ TyReacT) [i, o, m, a] -> dstStateT m >>= \ ms -> pure (i, o, ms, a)
      _                                          -> Nothing


-- | Types containing no type variables (or blanks).
concrete :: Ty -> Bool
concrete = \ case
      TyVar {}    -> False
      TyCon {}    -> True
      TyBuiltin {} -> True
      TyNat {}    -> True
      TyTuple _ ts -> all concrete ts
      TyApp _ a bs -> concrete a && all concrete bs

-- | Types with no built-ins (Strings, Integers, lists).
fundamental :: Ty -> Bool
fundamental = \ case
      TyBuiltin _ TyString  -> False
      TyBuiltin _ TyInteger -> False
      TyBuiltin _ TyList    -> False
      TyNat {}              -> True
      TyCon {}              -> True
      TyBuiltin {}          -> True
      TyVar {}              -> True
      TyTuple _ ts          -> all fundamental ts
      TyApp _ a bs          -> fundamental a && all fundamental bs

higherOrder :: Ty -> Bool
higherOrder (flattenSig -> (ats, rt)) = any isArrow $ rt : ats

-- Degree-1 polynomial with rational coefficients.
data Poly1 = Poly1 Rational (HashMap Text Rational)
      deriving (Show)

instance Eq Poly1 where
      (normP1 -> Poly1 r cs) == (normP1 -> Poly1 r' cs') = r == r' && cs == cs'

instance Pretty Poly1 where
      pretty = \ case
            (normP1 -> Poly1 r (Map.toList -> cs)) | r == 0 -> hsep $ punctuate (text " +") $ map pretty' cs
            (normP1 -> Poly1 r (Map.toList -> cs))          -> hsep $ punctuate (text " +") $ rat r : map pretty' cs
            where pretty' :: (Text, Rational) -> Doc a
                  pretty' (x, cx) | cx == 1   = pretty x
                                  | otherwise = rat cx <> pretty x

                  rat :: Rational -> Doc a
                  rat r | denominator r == 1 = text $ showt $ numerator r
                        | otherwise          = parens $ text $ showt r

zeroP1 :: Poly1
zeroP1 = Poly1 0 mempty

normP1 :: Poly1 -> Poly1
normP1 (Poly1 r cs) = Poly1 r $ Map.filter (/= 0) cs

monoP1 :: Rational -> Maybe Text -> Poly1
monoP1 r = \ case
      Just x  -> Poly1 0 $ Map.singleton x r
      Nothing -> Poly1 r mempty

minusP1 :: Poly1 -> Poly1 -> Poly1
minusP1 p1 p2 = plusP1 p1 $ multP1 (-1) p2

plusP1 :: Poly1 -> Poly1 -> Poly1
plusP1 (Poly1 r cs) (Poly1 r' cs') = normP1 $ Poly1 (r + r') $ Map.unionWith (+) cs cs'

multP1 :: Rational -> Poly1 -> Poly1
multP1 r' (Poly1 r cs) = Poly1 (r' * r) $ Map.map (r' *) cs

pickVar :: Poly1 -> Maybe (Text, Poly1)
pickVar = pickVar' . normP1
      where pickVar' :: Poly1 -> Maybe (Text, Poly1)
            pickVar' p | ((x, cx) : cs) <- vars p = pure (x, multP1 ((-1) / cx) $ setCoeffs (Map.fromList cs) p)
                       | otherwise                = Nothing

            vars :: Poly1 -> [(Text, Rational)]
            vars (Poly1 _ cs) = sort' $ Map.toList cs

            setCoeffs :: HashMap Text Rational -> Poly1 -> Poly1
            setCoeffs cs (Poly1 r _) = Poly1 r cs

            sort' :: [(Text, Rational)] -> [(Text, Rational)]
            sort' = sortOn (hash . show . fst)

poly1Ty :: Poly1 -> Ty
poly1Ty (Poly1 r cs) | (c : cs') <- Map.toList cs, r == 0 = foldr (plus' . mono) (mono c) cs'
                     | (c : cs') <- Map.toList cs         = ratTy r `plus'` foldr (plus' . mono) (mono c) cs'
                     | otherwise                          = ratTy r
      where mono :: (Text, Rational) -> Ty
            mono (x, cx) | cx == 1   = tyVar x
                         | otherwise = multTy cx $ tyVar x

            plus' :: Ty -> Ty -> Ty
            plus' = plusTy' noAnn

            tyVar :: Text -> Ty
            tyVar = TyVar noAnn

ratTy :: Rational -> Ty
ratTy r | r < 0              = negTy noAnn $ posRatTy (-r)
        | otherwise          = posRatTy r
      where posRatTy :: Rational -> Ty
            posRatTy r' | denominator r' == 1 = TyNat noAnn $ fromIntegral $ numerator r'
                        | otherwise           = TyApp noAnn (TyCon noAnn "/") 
                                                  [TyNat noAnn $ fromIntegral $ numerator r'
                                                  , TyNat noAnn $ fromIntegral $ denominator r']

multTy :: Rational -> Ty -> Ty
multTy r t = TyApp noAnn (TyCon noAnn "*") [ratTy r,t]

dstRat :: Ty -> Maybe Rational
dstRat = \ case
      TyNat _ n             -> pure $ fromIntegral n
      (dstNegTy -> Just t') -> ((-1) *) <$> dstRat t'
      (dstTyBinOp -> Just ("/", TyNat _ a, TyNat _ b))
                            -> pure $ fromIntegral a % fromIntegral b
      _                     -> Nothing

dstPoly1 :: Ty -> Maybe Poly1
dstPoly1 = \ case
      TyNat _ n               -> pure $ monoP1 (fromIntegral n) Nothing
      TyVar _ x             -> pure $ monoP1 1 $ Just x
      (dstNegTy -> Just t)    -> multP1 (-1) <$> dstPoly1 t
      (dstPlusTy -> Just (t1, t2)) -> plusP1 <$> dstPoly1 t1 <*> dstPoly1 t2
      (rmult -> Just (r, t))  -> multP1 r <$> dstPoly1 t
      (rdiv -> Just (t, r))   -> multP1 (1/r) <$> dstPoly1 t
      _                       -> Nothing
      where rmult :: Ty -> Maybe (Rational, Ty)
            rmult = \ case
                  (dstTyBinOp -> Just ("*", dstRat -> Just r, t)) -> pure (r, t)
                  _                                                      -> Nothing

            rdiv :: Ty -> Maybe (Ty, Rational)
            rdiv = \ case
                  (dstTyBinOp -> Just ("/", t, dstRat -> Just r)) -> pure (t, r)
                  _                                                      -> Nothing

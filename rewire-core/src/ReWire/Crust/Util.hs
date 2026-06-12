{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.Util
      ( Fresh, Name, Embed (..), TRec, Bind, FieldId
      , paramTys, isPrim, inlinable, nil, isExtrude, extrudeDefn
      , mkTuple, mkTuplePat, mkTupleMPat, synthableDefn
      , mkPair, mkPairPat, mkPairMPat, flattenLam, mkLam
      , mkApp, mkError, builtin, proxy, patVars, toVar, toPatVar, transPat, transMPat
      ) where

import ReWire.Annotation (Annote (MsgAnnote))
import ReWire.Crust.Syntax (Exp (..), Ty (..), MatchPat (..), Pat (..), Defn (..), DefnAttr (..), Builtin (..), Poly (..), FieldId, builtins, flattenApp)
import ReWire.Crust.Types (proxyTy, nilTy, strTy, arr, typeOf, arrowRight, pairTy, mkArrowTy, paramTys, synthable)
import ReWire.Pretty (pretty)
import ReWire.SYB (query)
import ReWire.Unbound (freshVar, Name, Fresh, Embed (Embed), unbind, bind, s2n, unsafeUnbind, Bind, TRec)

import Data.Text (Text)
import Numeric.Natural (Natural)

builtin :: Text -> Maybe Builtin
builtin b = lookup b builtins

isPrim :: Show a => a -> Bool
isPrim = notElem '.' . show

inlinable :: Defn -> Bool
inlinable d = case defnAttr d of
      Just Inline   -> True
      Just NoInline -> False
      Nothing       -> not $ isPrim $ defnName d

extrudeDefn :: Defn -> Bool
extrudeDefn (Defn { defnBody = Embed (unsafeUnbind -> (_, b)) }) = hasExtrude b
      where hasExtrude :: Exp -> Bool
            hasExtrude (query -> xs) = Extrude `elem` xs

synthableDefn :: Defn -> Bool
synthableDefn = \ case
      Defn { defnPolyTy = Embed (Poly (unsafeUnbind -> (_, t)))
           , defnName   = n
           } -> isPrim n || synthable t

isExtrude :: Exp -> Bool
isExtrude e = case flattenApp e of
      (Builtin _ _ _ Extrude, [_, _]) -> True
      _                               -> False

nil :: Exp
nil = Con (MsgAnnote "nil") Nothing (Just nilTy) (s2n "()")

proxy :: Natural -> Exp
proxy n = Con an Nothing (Just $ proxyTy an n) $ s2n "Proxy"
      where an :: Annote
            an = MsgAnnote "Proxy"

nilPat :: Pat
nilPat = PatCon (MsgAnnote "nilPat") (Embed Nothing) (Embed $ Just nilTy) (Embed $ s2n "()") []

nilMPat :: MatchPat
nilMPat = MatchPatCon (MsgAnnote "nilMPat") Nothing (Just nilTy) (s2n "()") []

-- | Get well-typed pat variables.
patVars :: Pat -> [(Ty, Name Exp)]
patVars = \ case
      PatCon _ _ _ _ ps             -> concatMap patVars ps
      PatVar _ _ (Embed (Just t)) x -> [(t, x)]
      PatVar _ _ (Embed Nothing) x  -> error $ "Untyped pat var (rwc bug; shouldn't happen): " <> show (pretty x)
      _                             -> []

toVar :: Annote -> (Ty, Name Exp) -> Exp
toVar an (vt, v) = Var an Nothing (Just vt) v

toPatVar :: Annote -> (Ty, Name Exp) -> Pat
toPatVar an (vt, v) = PatVar an (Embed Nothing) (Embed $ Just vt) v

transPat :: Pat -> MatchPat
transPat = \ case
      PatCon an (Embed tan) (Embed t) (Embed c) ps -> MatchPatCon an tan t c $ map transPat ps
      PatVar an (Embed tan) (Embed t) _            -> MatchPatVar an tan t
      PatWildCard an (Embed tan) (Embed t)         -> MatchPatWildCard an tan t

transMPat :: Fresh m => MatchPat -> m Pat
transMPat = \ case
      MatchPatCon an tan t c ps -> PatCon an (Embed tan) (Embed t) (Embed c) <$> mapM transMPat ps
      MatchPatVar an tan t      -> PatVar an (Embed tan) (Embed t) <$> freshVar "m2c"
      MatchPatWildCard an tan t -> pure $ PatWildCard an (Embed tan) (Embed t)

mkPair :: Annote -> Exp -> Exp -> Exp
mkPair an e1 e2 = mkApp an (Con an Nothing t (s2n "(,)")) [e1, e2]
      where t :: Maybe Ty
            t = do
                  t1 <- typeOf e1
                  t2 <- typeOf e2
                  pure $ mkArrowTy [t1, t2] $ pairTy an t1 t2

mkPairPat :: Annote -> Pat -> Pat -> Pat
mkPairPat an p1 p2 = PatCon an (Embed Nothing) (Embed $ pairTy an <$> typeOf p1 <*> typeOf p2) (Embed (s2n "(,)")) [p1, p2]

mkPairMPat :: Annote -> MatchPat -> MatchPat -> MatchPat
mkPairMPat an p1 p2 = MatchPatCon an Nothing (pairTy an <$> typeOf p1 <*> typeOf p2) (s2n "(,)") [p1, p2]

mkTuple :: Annote -> [Exp] -> Exp
mkTuple an = foldr (mkPair an) nil

mkTuplePat :: Annote -> [Pat] -> Pat
mkTuplePat an = foldr (mkPairPat an) nilPat

mkTupleMPat :: Annote -> [MatchPat] -> MatchPat
mkTupleMPat an = foldr (mkPairMPat an) nilMPat

mkApp :: Annote -> Exp -> [Exp] -> Exp
mkApp an = foldl' $ \ e -> App an Nothing (arrowRight <$> typeOf e) e

mkError :: Annote -> Maybe Ty -> Text -> Exp
mkError an t err = App an Nothing t (Builtin an Nothing (arr (strTy an) <$> t) Error) $ LitStr an Nothing err

flattenLam :: Fresh m => Exp -> m ([Name Exp], Exp)
flattenLam = \ case
      Lam _ _ _ e -> do
            (x, e')   <- unbind e
            (xs, e'') <- flattenLam e'
            pure (x : xs, e'')
      e           -> pure ([], e)

mkLam :: Annote -> [(Ty, Name Exp)] -> Exp -> Exp
mkLam an vs b = foldr (\ (t, v) e -> Lam an Nothing (Just t) $ bind v e) b vs

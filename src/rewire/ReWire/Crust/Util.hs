{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.Util
      ( Fresh, Name, Embed (..), TRec, Bind, FieldId
      , paramTys, isPrim, inlineable, mustInline, nil
      , mkTuple, mkTuplePat, mkTupleMPat
      , mkPair, mkPairPat, mkPairMPat, flattenLam, mkLam
      , mkApp, mkError, builtin
      ) where

import ReWire.Annotation (Annote (MsgAnnote))
import ReWire.Crust.Syntax (Exp (..), Ty (..), MatchPat (..), Pat (..), Defn (..), DefnAttr (..), Builtin (..), Poly (..), FieldId, builtins)
import ReWire.Crust.Types (nilTy, strTy, arr, typeOf, arrowRight, pairTy, fundamental, mkArrowTy, paramTys)
import ReWire.Unbound (Name, Fresh, Embed (Embed), unbind, bind, s2n, unsafeUnbind, Bind, TRec)

import Data.List (foldl')
import Data.Text (Text)

builtin :: Text -> Maybe Builtin
builtin b = lookup b builtins

isPrim :: Show a => a -> Bool
isPrim = notElem '.' . show

inlineable :: Defn -> Bool
inlineable d = case defnAttr d of
      Just Inline   -> True
      Just NoInline -> False
      Nothing       -> not $ isPrim $ defnName d

mustInline :: Defn -> Bool
mustInline = \ case
      Defn { defnAttr = Just Inline }                    -> True
      Defn { defnPolyTy = Embed (Poly (unsafeUnbind -> (_, t)))
           , defnName   = n }                            -> not (isPrim n || fundamental t)

nil :: Exp
nil = Con (MsgAnnote "nil") Nothing (Just nilTy) (s2n "()")

nilPat :: Pat
nilPat = PatCon (MsgAnnote "nilPat") (Embed Nothing) (Embed $ Just nilTy) (Embed $ s2n "()") []

nilMPat :: MatchPat
nilMPat = MatchPatCon (MsgAnnote "nilMPat") Nothing (Just nilTy) (s2n "()") []

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

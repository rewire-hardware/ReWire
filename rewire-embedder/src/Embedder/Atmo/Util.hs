{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module Embedder.Atmo.Util
      ( FieldId
      , paramTys, isPrim, inlineable, mustInline, nil
      , flattenTuple, mkTuple, mkTuplePat
      , mkPair, mkPairPat, flattenLam, mkLam
      , mkApp, mkError, builtin, proxy, tybuiltin, userBuiltin, mkTupleCtor, isTupleCtor
      , mkRecVal, mkRecUpd, mkRecSel, mkPatRec
      ) where

import ReWire.Annotation (Annote (MsgAnnote))
import Embedder.Atmo.Syntax (Exp (..), Ty (..), Pat (..), Defn (..), DefnAttr (..), Poly (..), FieldId, TyBuiltin)
import Embedder.Atmo.Types (proxyTy, nilTy, strTy, arr, typeOf, arrowRight, pairTy, fundamental, mkArrowTy, paramTys)
import Embedder.Builtins ( builtins, Builtin (..), tybuiltins, builtinUserQName, RWUserOp (..) )

import Data.Text (Text, unpack)
import Data.Maybe(fromMaybe)
import Numeric.Natural (Natural)
import Data.Tuple (swap)
import qualified Data.Text as T


-- Name Handling:
      -- Remove Embed
      -- Remove bindings (e.g. Poly)
      -- Remove Name from Lam helpers
      -- Remove s2n and n2s
      -- Remove Unbound exports

builtin :: Text -> Maybe Builtin
builtin b = lookup b builtins

tybuiltin :: Text -> Maybe TyBuiltin
tybuiltin b = lookup b tybuiltins

userBuiltin :: Text -> Maybe Builtin
userBuiltin b = lookup b (map swap builtinUserQName)

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
      Defn { defnPolyTy = Poly _ t
           , defnName   = n }                            -> not (isPrim n || fundamental t)

nil :: Exp
nil = Con (MsgAnnote "nil") Nothing (Just nilTy) "()"

proxy :: Natural -> Exp
proxy n = Con an Nothing (Just $ proxyTy an n) "Proxy"
      where an :: Annote
            an = MsgAnnote "Proxy"

nilPat :: Pat
nilPat = PatCon (MsgAnnote "nilPat") Nothing (Just nilTy) "()" []

mkPair :: Annote -> Exp -> Exp -> Exp
mkPair an e1 e2 = mkApp an (Con an Nothing t "(,)") [e1, e2]
      where t :: Maybe Ty
            t = do
                  t1 <- typeOf e1
                  t2 <- typeOf e2
                  pure $ mkArrowTy [t1, t2] $ pairTy an t1 t2

flattenPair :: Exp -> Maybe (Exp,Exp)
flattenPair = \ case
      Tuple _ _ _ [e1,e2] -> Just (e1,e2)
      _ -> Nothing

mkPairPat :: Annote -> Pat -> Pat -> Pat
mkPairPat an p1 p2 = PatCon an Nothing (pairTy an <$> typeOf p1 <*> typeOf p2) "(,)" [p1, p2]

mkTupleCtor :: Int -> Text
mkTupleCtor n = "(" <> T.replicate (n-1) "," <> ")"

-- Function to match the specified pattern and return Maybe Int
isTupleCtor :: T.Text -> Bool
isTupleCtor c = c == "(" <> T.replicate (length (unpack c) - 2) "," <> ")"

mkTuple :: Annote -> [Exp] -> Exp
mkTuple an = foldr (mkPair an) nil

flattenTuple :: Exp -> Maybe [Exp]
flattenTuple e = case flattenPair e of
      Nothing -> Nothing
      -- Can't remove unit without changing type. Just (e1,Con _ _ _ (n2s -> "()")) -> Just [e1]
      Just (e1,e2) -> Just $ e1 : fromMaybe [e2] (flattenTuple e2)

mkTuplePat :: Annote -> [Pat] -> Pat
mkTuplePat an = foldr (mkPairPat an) nilPat

mkApp :: Annote -> Exp -> [Exp] -> Exp
mkApp an e es = App an Nothing (foldl' (\ b _ -> fmap arrowRight b) (typeOf e) es) e es

mkError :: Annote -> Maybe Ty -> Text -> Exp
mkError an t err = App an Nothing t (RWUser an Nothing (arr (strTy an) <$> t) (RWBuiltin Error)) [LitStr an Nothing err]

flattenLam :: Monad m => Exp -> m ([Text], Exp)
flattenLam = \ case
      Lam _ _ _ xs e -> do
            (xs', e') <- flattenLam e
            pure (xs ++ xs', e')
      e              -> pure ([], e)

mkLam :: Annote -> Ty -> [Text] -> Exp -> Exp
mkLam an t = Lam an Nothing (Just t)



-- Record helpers for construction
mkRecVal :: Annote -> [(Text, Exp)] -> Exp
mkRecVal an = RecVal an Nothing Nothing

mkRecUpd :: Annote -> Exp -> [(Text, Exp)] -> Exp
mkRecUpd an = RecUpd an Nothing Nothing

mkRecSel :: Annote -> Text -> Exp -> Exp
mkRecSel an = RecSel an Nothing Nothing

-- Record pattern helper
mkPatRec :: Annote -> [(Text, Pat)] -> Pat
mkPatRec an = PatRec an Nothing Nothing


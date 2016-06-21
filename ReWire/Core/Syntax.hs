{-# LANGUAGE DeriveDataTypeable,DeriveGeneric,LambdaCase #-}
{-# LANGUAGE Safe #-}

module ReWire.Core.Syntax
  ( DataConId(..),TyConId(..)
  , Ty(..)
  , Exp(..)
  , Pat(..)
  , Defn(..)
  , DataCon(..)
  , Program(..)
  , mkArrow,arrowRight
  , flattenArrow,flattenTyApp
  , flattenApp,typeOf
  , GId, LId, TyId
  ) where

import ReWire.Pretty
import ReWire.Annotation

import Data.Data (Typeable,Data(..))
import Data.List (nub)
import Text.PrettyPrint
import GHC.Generics (Generic)

newtype DataConId = DataConId { deDataConId :: String } deriving (Eq,Ord,Generic,Show,Typeable,Data)
newtype TyConId   = TyConId   { deTyConId :: String } deriving (Eq,Ord,Generic,Show,Typeable,Data)

type GId  = String
type LId  = Int
type TyId = String

instance Pretty DataConId where
  pretty = text . deDataConId

instance Pretty TyConId where
  pretty = text . deTyConId

---

data Ty = TyApp  Annote Ty Ty
        | TyCon  Annote TyConId
        | TyComp Annote Ty Ty
        | TyVar  Annote TyId
        deriving (Eq,Generic,Show,Typeable,Data)

instance Annotated Ty where
  ann (TyApp a _ _)  = a
  ann (TyCon a _)    = a
  ann (TyComp a _ _) = a
  ann (TyVar a _)    = a

instance Pretty Ty where
  pretty (TyApp _ (TyApp _ (TyCon _ (TyConId "->")) t1) t2) = ppTyArrowL t1 <+> text "->" <+> pretty t2
    where ppTyArrowL t@(TyApp _ (TyApp _ (TyCon _ (TyConId "->")) _) _) = parens $ pretty t
          ppTyArrowL t                                                  = pretty t
  pretty (TyApp _ t1 t2)  = pretty t1 <+> ppTyAppR t2
  pretty (TyCon _ n)      = text (deTyConId n)
  pretty (TyComp _ t1 t2) = pretty t1 <+> ppTyAppR t2
  pretty (TyVar _ x)      = text x

ppTyAppR :: Ty -> Doc
ppTyAppR t@TyApp {} = parens $ pretty t
ppTyAppR t          = pretty t

---

data Exp = App        Annote Exp Exp
         | Prim       Annote Ty  GId
         | GVar       Annote Ty  GId
         | LVar       Annote Ty  LId
         | Con        Annote Ty  DataConId
         | Match      Annote Ty  Exp Pat GId [LId] (Maybe Exp)
         | NativeVHDL Annote Ty  String
         deriving (Eq,Show,Typeable,Data)

instance Annotated Exp where
  ann (App a _ _)           = a
  ann (Prim a _ _)          = a
  ann (GVar a _ _)          = a
  ann (LVar a _ _)          = a
  ann (Con a _ _)           = a
  ann (Match a _ _ _ _ _ _) = a
  ann (NativeVHDL a _ _)    = a

instance Pretty Exp where
  pretty (App _ e1 e2)                 = parens $ hang (pretty e1) 4 (pretty e2)
  pretty (Con _ _ n)                   = text (deDataConId n)
  pretty (Prim _ _ n)                  = text n
  pretty (GVar _ _ n)                  = text n
  pretty (LVar _ _ n)                  = text $ "$" ++ show n
  pretty (Match _ _ e p e1 as Nothing) = parens $
                                 foldr ($+$) empty
                                   [ text "match" <+> pretty e <+> text "of"
                                   , nest 4 (braces $ vcat $ punctuate (space <> text ";" <> space)
                                     -- TODO(chathhorn)
                                     [ parens (pretty p) <+> text "->" <+> text e1 <+> hsep (map (pretty . LVar undefined undefined)as)
                                     ])
                                   ]
  pretty (Match _ _ e p e1 as (Just e2)) = parens $
                                 foldr ($+$) empty
                                   [ text "match" <+> pretty e <+> text "of"
                                   , nest 4 (braces $ vcat $ punctuate (space <> text ";" <> space)
                                     -- TODO(chathhorn)
                                     [ parens (pretty p) <+> text "->" <+> text e1 <+> hsep (map (pretty . LVar undefined undefined)as)
                                     , text "_" <+> text "->" <+> pretty e2
                                     ])
                                   ]
  pretty (NativeVHDL _ _ n)             = parens (text "nativeVHDL" <+> doubleQuotes (text n))

---

data Pat = PatCon Annote Ty DataConId [Pat]
         | PatVar Annote Ty
         deriving (Eq,Show,Typeable,Data)

instance Annotated Pat where
  ann (PatCon a _ _ _) = a
  ann (PatVar a _)   = a

instance Pretty Pat where
  pretty (PatCon _ t n ps) = parens (text (deDataConId n) <+> hsep (map pretty ps) <+> text "::" <+> pretty t)
  pretty (PatVar _ _)      = text "*"

---

data Defn = Defn { defnAnnote :: Annote,
                   defnName   :: GId,
                   defnTy     :: Ty, -- params given by the arity.
                   defnBody   :: Exp }
               deriving (Eq,Show,Typeable,Data)

instance Annotated Defn where
  ann (Defn a _ _ _) = a

instance Pretty Defn where
  pretty (Defn _ n ty e) = foldr ($+$) empty
                        (  [text n <+> text "::" <+> pretty ty]
                        ++ [text n <+> hsep (map (text . ("$"++) . show) [0..arity ty - 1]) <+> text "=", nest 4 $ pretty e])

---

data DataCon = DataCon Annote DataConId Int Ty
                  deriving (Generic,Eq,Show,Typeable,Data)

instance Annotated DataCon where
  ann (DataCon a _ _ _) = a

instance Pretty DataCon where
  pretty (DataCon _ n _ t) = text (deDataConId n) <+> text "::" <+> pretty t

---

data Program = Program { ctors  :: [DataCon],
                         defns  :: [Defn] }
                  deriving (Eq,Show,Typeable,Data)

instance Monoid Program where
  mempty = Program mempty mempty
  mappend (Program ts vs) (Program ts' vs') = Program (nub $ ts ++ ts') $ nub $ vs ++ vs'

instance Pretty Program where
  pretty p = ppDataDecls (ctors p) $+$ ppDefns (defns p)
    where ppDefns = foldr ($+$) empty . map pretty
          ppDataDecls = foldr ($+$) empty . map pretty

---

arity :: Ty -> Int
arity = \ case
  TyApp _ (TyApp _ (TyCon _ (TyConId "->")) _) t2 -> 1 + arity t2
  _                                               -> 0

flattenArrow :: Ty -> [Ty]
flattenArrow (TyApp _ (TyApp _ (TyCon _ (TyConId "->")) tl) tr) = tl : flattenArrow tr
flattenArrow t                                                  = [t]

flattenTyApp :: Ty -> [Ty]
flattenTyApp (TyApp _ t t') = flattenTyApp t ++ [t']
flattenTyApp t              = [t]

flattenApp :: Exp -> [Exp]
flattenApp (App _ e e') = flattenApp e++[e']
flattenApp e            = [e]

mkArrow :: Ty -> Ty -> Ty
mkArrow t = TyApp (ann t) (TyApp (ann t) (TyCon (ann t) (TyConId "->")) t)

infixr `mkArrow`

arrowRight :: Ty -> Ty
arrowRight (TyApp _ (TyApp _ (TyCon _ (TyConId "->")) _) t2) = t2
arrowRight t                                                 = error $ "arrowRight: got non-arrow type: " ++ show t

typeOf :: Exp -> Ty
typeOf = \ case
  App _ e _           -> arrowRight (typeOf e)
  Prim _ t _          -> t
  GVar _ t _          -> t
  LVar _ t _          -> t
  Con _ t _           -> t
  Match _ t _ _ _ _ _ -> t
  NativeVHDL _ t _    -> t

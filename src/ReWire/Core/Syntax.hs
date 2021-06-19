{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DerivingVia, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Syntax
  ( DataConId (..), TyConId (..)
  , Ty (..)
  , Exp (..)
  , Pat (..)
  , Defn (..)
  , DataCon (..)
  , Program (..)
  , arrowRight, sizeof
  , flattenArrow, flattenTyApp
  , flattenApp
  , GId, LId, TyId
  , TypeAnnotated (..)
  , dummyTy
  ) where

import ReWire.Pretty
import ReWire.Annotation

import Data.Data (Typeable, Data(..))
import Data.List (intersperse)
import Data.Text (Text)
import Prettyprinter (Pretty (..), Doc, vcat, (<+>), nest, hsep, parens, braces, dquotes)
import GHC.Generics (Generic)
import Data.Containers.ListUtils (nubOrd)

import TextShow (TextShow (..), showt)
import TextShow.Generic (FromGeneric (..))

class TypeAnnotated a where
      typeof :: a -> Ty

class Parenless a where
      -- | Parts that never need to be wrapped in parens during pretty printing.
      parenless :: a -> Bool

mparen :: (Pretty a, Parenless a) => a -> Doc ann
mparen a = if parenless a then pretty a else parens $ pretty a

newtype DataConId = DataConId { deDataConId :: Text } deriving (Eq, Ord, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric DataConId
newtype TyConId   = TyConId   { deTyConId :: Text } deriving (Eq, Ord, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric TyConId

type GId  = Text
type LId  = Int
type TyId = Text

instance Pretty DataConId where
      pretty = pretty . deDataConId

instance Pretty TyConId where
      pretty = pretty . deTyConId

---

data Ty = TyApp Annote !Int !Ty !Ty
        | TyCon Annote !TyConId !Int
        | TyVar Annote !TyId !Int
        deriving (Eq, Ord, Generic, Show, Typeable, Data)
        deriving TextShow via FromGeneric Ty

dummyTy :: Ty
dummyTy = TyVar noAnn "dummy" 0

instance Annotated Ty where
      ann = \ case
            TyApp a _ _ _  -> a
            TyCon a _ _    -> a
            TyVar a _ _    -> a

instance Pretty Ty where
      pretty = \ case
            TyApp _ _ (TyApp _ _ (TyCon _ c _) t1) t2
                  | deTyConId c == "->"  -> ppTyArrowL t1 <+> text "->" <+> pretty t2
                  | deTyConId c == "(,)" -> parens $ ppTyArrowL t1 <> (text "," <+> pretty t2)
                  where ppTyArrowL :: Ty -> Doc ann
                        ppTyArrowL = \ case
                              t@(TyApp _ _ (TyApp _ _ (TyCon _ (TyConId "->") _) _) _) -> parens $ pretty t
                              t                                                  -> pretty t
            TyApp _ _ t1 t2                  -> pretty t1 <+> ppTyAppR t2
                  where ppTyAppR :: Ty -> Doc ann
                        ppTyAppR = \ case
                              t@(TyApp _ _ (TyApp _ _ (TyCon _ (TyConId "(,)") _) _) _) -> pretty t
                              t@TyApp {}                                          -> parens $ pretty t
                              t                                                   -> pretty t
            TyCon _ n _                      -> pretty $ deTyConId n
            TyVar _ n _                      -> pretty n

-- For arrow types, returns the size of the result.
sizeof :: Ty -> Int
sizeof t = case snd $ flattenArrow t of
      TyApp _ s _ _     -> s
      TyCon _ (TyConId "ReT") _   -> 1 -- TODO
      TyCon _ _ s       -> s
      TyVar _ _ s       -> s

---

data Exp = App        Annote !Exp !Exp
         | Prim       Annote !Ty  !GId
         | GVar       Annote !Ty  !GId
         | LVar       Annote !Ty  !LId
         | Con        Annote !Ty  !Int !DataConId
         | Match      Annote !Ty  !Exp !Pat !GId ![LId] !(Maybe Exp)
         | NativeVHDL Annote !Ty  !Text
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Exp

instance TypeAnnotated Exp where
      typeof = \ case
            App _ e _           -> arrowRight (typeof e)
            Prim _ t _          -> t
            GVar _ t _          -> t
            LVar _ t _          -> t
            Con _ t _ _         -> t
            Match _ t _ _ _ _ _ -> t
            NativeVHDL _ t _    -> t

instance Annotated Exp where
      ann = \ case
            App a _ _           -> a
            Prim a _ _          -> a
            GVar a _ _          -> a
            LVar a _ _          -> a
            Con a _ _ _           -> a
            Match a _ _ _ _ _ _ -> a
            NativeVHDL a _ _    -> a

instance Parenless Exp where
      parenless = \ case -- simple (non-compound?) expressions
            App _ (App _ (Con _ _ _ (DataConId "(,)")) _) _ -> True
            Con {}                                          -> True
            GVar {}                                         -> True
            LVar {}                                         -> True
            Prim {}                                         -> True
            _                                               -> False

instance Pretty Exp where
      pretty = \ case
            App _ (App _ (Con _ _ _ (DataConId "(,)")) e1) e2 -> parens $ pretty e1 <> (text "," <+> pretty e2)
            App _ e1@App {} e2                                -> hang (pretty e1) 2 $ mparen e2
            App _ e1 e2                                       -> hang (mparen e1) 2 $ mparen e2
            Con _ t _ (DataConId n)                           -> pretty n <+> braces (pretty t)
            Prim _ _ n                                        -> pretty n
            GVar _ t n                                        -> pretty n <+> braces (pretty t)
            LVar _ _ n                                        -> text $ "$" <> showt n
            Match _ t e p e1 as Nothing                       -> foldr ($+$) empty
                  [ text "match" <+> braces (pretty t) <+> pretty e <+> text "of"
                  , nest 2 (vcat
                       [ pretty p <+> text "->" <+> text e1
                             <+> hsep (map (pretty . LVar noAnn dummyTy) as) ])
                  ]
            Match _ t e p e1 as (Just e2)                     -> foldr ($+$) empty
                  [ text "match"  <+> braces (pretty t) <+> pretty e <+> text "of"
                  , nest 2 (vcat
                        [ pretty p <+> text "->"
                              <+> text e1 <+> hsep (map (pretty . LVar noAnn dummyTy) as)
                        , text "_" <+> text "->" <+> pretty e2
                        ])
                  ]
            NativeVHDL _ _ n                                  -> text "nativeVHDL" <+> dquotes (text n)

---

data Pat = PatCon Annote !Ty !DataConId ![Pat]
         | PatVar Annote !Ty
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Pat

instance TypeAnnotated Pat where
      typeof = \ case
            PatCon _ t _ _ -> t
            PatVar _ t     -> t

instance Annotated Pat where
      ann = \ case
            PatCon a _ _ _ -> a
            PatVar a _     -> a

instance Parenless Pat where
      parenless = \ case
            PatCon _ _ (DataConId "(,)") _ -> True
            PatCon _ _ _ []                -> True
            PatVar {}                      -> True
            _                              -> False

instance Pretty Pat where
      pretty = \ case
            PatCon _ _ (DataConId "(,)") [p1, p2] -> parens $ pretty p1 <> (text "," <+> pretty p2)
            PatCon _ _ (DataConId n) ps           -> text n <+> hsep (map mparen ps)
            PatVar _ t                            -> braces $ pretty t

---

data Defn = Defn { defnAnnote :: Annote,
                   defnName   :: !GId,
                   defnTy     :: !Ty, -- params given by the arity.
                   defnBody   :: !Exp }
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Defn

instance TypeAnnotated Defn where
      typeof (Defn _ _ t _) = t

instance Annotated Defn where
      ann (Defn a _ _ _) = a

instance Pretty Defn where
      pretty (Defn _ n ty e) = foldr ($+$) empty
                             ( (text n <+> text "::" <+> pretty ty)
                             : [text n <+> hsep (map (text . ("$" <>) . showt) [0 .. arity ty - 1]) <+> text "=", nest 2 $ pretty e])

---

-- | annotation, id, ctor index (in the range [0, nctors)), nctors, type
data DataCon = DataCon Annote !DataConId !Int !Int !Ty
      deriving (Generic, Eq, Ord, Show, Typeable, Data)
      deriving TextShow via FromGeneric DataCon

instance TypeAnnotated DataCon where
      typeof (DataCon _ _ _ _ t) = t

instance Annotated DataCon where
      ann (DataCon a _ _ _ _) = a

instance Pretty DataCon where
      pretty (DataCon _ n _ _ t) = text (deDataConId n) <+> text "::" <+> pretty t

---

data Program = Program { ctors :: ![DataCon],
                         defns :: ![Defn] }
      deriving (Generic, Eq, Ord, Show, Typeable, Data)
      deriving TextShow via FromGeneric Program

instance Semigroup Program where
      (Program ts vs) <> (Program ts' vs') = Program (nubOrd $ ts ++ ts') $ nubOrd $ vs ++ vs'

instance Monoid Program where
      mempty = Program mempty mempty

instance Pretty Program where
      pretty p = ppDataDecls (ctors p) $$ text "" $$ ppDefns (defns p)
            where ppDefns = vcat . intersperse (text "") . map pretty
                  ppDataDecls = vcat . intersperse (text "") . map pretty

---
-- TODO(chathhorn): should rewrite these to return ([x], x) or non-empty list.
arity :: Ty -> Int
arity = \ case
      TyApp _ _ (TyApp _ _ (TyCon _ (TyConId "->") _) _) t2 -> 1 + arity t2
      _                                               -> 0

flattenArrow :: Ty -> ([Ty], Ty)
flattenArrow = \ case
      TyApp _ _ (TyApp _ _ (TyCon _ (TyConId "->") _) t1) t2 -> (t1 : ts, t)
            where (ts, t) = flattenArrow t2
      t                                               -> ([], t)

flattenTyApp :: Ty -> [Ty]
flattenTyApp = \ case
      TyApp _ _ t t' -> flattenTyApp t ++ [t']
      t              -> [t]

flattenApp :: Exp -> [Exp]
flattenApp = \ case
      App _ e e' -> flattenApp e ++ [e']
      e          -> [e]

arrowRight :: Ty -> Ty
arrowRight = \ case
      TyApp _ _ (TyApp _  _ (TyCon _ (TyConId "->") _) _) t2 -> t2
      t                                               -> error $ "arrowRight: got non-arrow type: " ++ show t

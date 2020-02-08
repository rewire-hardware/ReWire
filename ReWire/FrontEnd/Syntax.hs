{-# LANGUAGE MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , DeriveDataTypeable
           , DeriveGeneric
           , Rank2Types
           , GADTs
           , ScopedTypeVariables
           , StandaloneDeriving
           , LambdaCase
           , ViewPatterns
           #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReWire.FrontEnd.Syntax
      ( DataConId (..), TyConId
      , Ty (..), Exp (..), Pat (..), MatchPat (..)
      , Defn (..), DataDefn (..), DataCon (..)
      , FreeProgram, Program (..)
      , Kind (..), kblank
      , flattenApp, arr, arrowRight
      , fv, fvAny
      , Fresh, Name, Embed (..), TRec, Bind
      , FieldId
      , trec, untrec, bind, unbind
      , Poly (..), (|->), poly
      , rangeTy
      , kmonad, tycomp
      , TypeAnnotated (..)
      ) where

import ReWire.Annotation
import ReWire.FrontEnd.Unbound
      ( Fresh (..), runFreshM
      , Embed (..)
      , TRec (..), trec, untrec
      , Name (..), AnyName (..), SubstName (..)
      , Bind (..), bind, unbind
      , Alpha, aeq, Subst (..)
      , toListOf
      , n2s, s2n
      )
import ReWire.Pretty
import qualified ReWire.FrontEnd.Unbound as UB (fv, fvAny)

import Control.DeepSeq (NFData (..), deepseq)
import Data.Data (Typeable, Data (..))
import GHC.Generics (Generic)
import Text.PrettyPrint
      ( Doc, text, nest, hsep, punctuate, parens, doubleQuotes
      , space, hang, braces, vcat, (<+>), ($+$)
      )

fv :: (Alpha a, Typeable b) => a -> [Name b]
fv = toListOf UB.fv

fvAny :: Alpha a => a -> [AnyName]
fvAny = toListOf UB.fvAny

kblank :: Kind
kblank = KVar $ s2n "_"

kmonad :: Kind
kmonad = KStar `KFun` KStar

tycomp :: Annote -> Ty -> Ty -> Ty
tycomp = TyApp

class TypeAnnotated a where
      typeOf :: a -> Ty

newtype DataConId = DataConId String
      deriving (Generic, Typeable, Data)
newtype TyConId = TyConId String
      deriving (Generic, Typeable, Data)
newtype FieldId  = FieldId String
      deriving (Generic, Typeable, Data)

data DataCon = DataCon Annote (Name DataConId) (Embed Poly)
      deriving (Generic, Eq, Show, Typeable, Data)

instance Alpha DataCon

instance Annotated DataCon where
      ann (DataCon a _ _)  = a

instance Pretty DataCon where
      pretty (DataCon _ n t)  = pretty n <+> text "::" <+> pretty t

instance NFData DataCon

data Kind = KStar
          | KFun Kind Kind
          | KVar (Name Kind)
      deriving (Generic, Ord, Eq, Show, Typeable, Data)

infixr `KFun`

instance Alpha Kind

instance Subst Kind Kind where
      isvar (KVar x) = Just $ SubstName x
      isvar _        = Nothing

instance NFData Kind

instance Pretty Kind where
      pretty = \ case
            KStar            -> text "*"
            KVar n           -> text $ show n
            KFun a@KFun {} b -> parens (pretty a) <+> text "->" <+> pretty b
            KFun a b         -> pretty a <+> text "->" <+> pretty b

newtype Poly = Poly (Bind [Name Ty] Ty)
      deriving (Generic, Show, Typeable, Data)

instance NFData Poly

poly :: [Name Ty] -> Ty -> Poly
poly vs t = Poly $ bind vs t

(|->) :: [Name Ty] -> Ty -> Embed Poly
vs |-> t = Embed $ poly vs t

infix 1 |->

instance Alpha Poly

instance Eq Poly where
      (==) = aeq

instance Pretty Poly where
      pretty (Poly pt) = runFreshM $ do
            (_, t) <- unbind pt
            return $ pretty t

data Ty = TyApp Annote Ty Ty
        | TyCon Annote (Name TyConId)
        | TyVar Annote Kind (Name Ty)
        | TyBlank Annote
           deriving (Eq, Generic, Typeable, Data, Show)

instance Alpha Ty

instance Subst Ty Ty where
      isvar (TyVar _ _ x) = Just $ SubstName x
      isvar _                = Nothing
instance Subst Ty Annote where
      subst _ _ x = x
      substs _ x = x
instance Subst Ty Kind

instance Annotated Ty where
      ann = \ case
            TyApp a _ _  -> a
            TyCon a _    -> a
            TyVar a _ _  -> a
            TyBlank a    -> a

instance Pretty Ty where
      pretty = \ case
            TyApp _ (TyApp _ (TyCon _ c) t1) t2
                  | n2s c == "->"  -> ppTyArrowL t1 <+> text "->" <+> pretty t2
                  | n2s c == "(,)" -> parens (ppTyArrowL t1 <+> text "," <+> pretty t2)
                  where ppTyArrowL t@(TyApp _ (TyApp _ (TyCon _ c) _) _)
                              | n2s c == "->" = parens $ pretty t
                        ppTyArrowL t          = pretty t
            TyApp _ t1 t2                  -> pretty t1 <+> ppTyAppR t2
                  where ppTyAppR :: Ty -> Doc
                        ppTyAppR t@TyApp {} = parens $ pretty t
                        ppTyAppR t          = pretty t
            TyCon _ n                      -> pretty n
            TyVar _ _ n                    -> pretty n
            TyBlank _                      -> text "_"

instance NFData Ty

----

data Exp = App        Annote Exp Exp
         | Lam        Annote Ty (Bind (Name Exp) Exp)
         | Var        Annote Ty (Name Exp)
         | Con        Annote Ty (Name DataConId)
         | Case       Annote Ty Exp (Bind Pat Exp) (Maybe Exp)
         | Match      Annote Ty Exp MatchPat Exp [Exp] (Maybe Exp)
         | NativeVHDL Annote String Exp
         | Error      Annote Ty String
         deriving (Generic, Show, Typeable, Data)

instance Alpha Exp

instance Subst Exp Exp where
      isvar (Var _ _ x) = Just $ SubstName x
      isvar _              = Nothing
instance Subst Exp Annote where
      subst _ _ x = x
      substs _ x = x
instance Subst Exp Ty
instance Subst Exp Kind
instance Subst Exp Pat
instance Subst Exp Defn
instance Subst Exp Poly

instance Subst Ty Exp
instance Subst Ty Pat

instance NFData Exp

instance TypeAnnotated Exp where
      typeOf = \ case
            App _ e _           -> arrowRight $ typeOf e
            Lam _ t e           -> arr' t e
            Var _ t _           -> t
            Con _ t _           -> t
            Case _ t _ _ _      -> t
            Match _ t _ _ _ _ _ -> t
            NativeVHDL _ _ e    -> typeOf e
            Error _ t _         -> t

instance Annotated Exp where
      ann = \ case
            App a _ _           -> a
            Lam a _ _           -> a
            Var a _ _           -> a
            Con a _ _           -> a
            Case a _ _ _ _      -> a
            Match a _ _ _ _ _ _ -> a
            NativeVHDL a _ _    -> a
            Error a _ _         -> a

instance Pretty Exp where
      pretty = \ case
            App _ (App _ (Con _ _ (n2s -> "(,)")) e1) e2 -> parens $ pretty e1 <+> text "," <+> pretty e2
            App _ e1 e2                                  -> parens $ hang (pretty e1) 4 $ pretty e2
            Con _ _ n                                    -> pretty n
            Var _ _ n                                    -> text $ show n {- <+> text "::" <+> pretty t -}
            Lam _ _ e                                    -> runFreshM $ do
                  (p, e') <- unbind e
                  return $ parens $ text "\\" <+> text (show p) <+> text "->" <+> pretty e'
            Case _ _ e e1 e2                             -> runFreshM $ do
                  (p, e1') <- unbind e1
                  return $ parens $
                        foldr ($+$) mempty
                        [ text "case" <+> pretty e <+> text "of"
                        , nest 4 (braces $ vcat $ punctuate (space <> text ";")
                              [ pretty p <+> text "->" <+> pretty e1' ]
                              ++ maybe [] (\ e2' -> [text "_" <+> text "->" <+> pretty e2']) e2
                              )
                        ]
            Match _ _ e p e1 as e2                       -> runFreshM $
                  return $ parens $
                        foldr ($+$) mempty
                        [ text "case" <+> pretty e <+> text "of"
                        , nest 4 (braces $ vcat $ punctuate (space <> text ";")
                              [ pretty p <+> text "->" <+> pretty e1 <+> hsep (map pretty as) ]
                              ++ maybe [] (\ e2' -> [text "_" <+> text "->" <+> pretty e2']) e2
                              )
                        ]
            NativeVHDL _ n e                             -> parens (text "nativeVHDL" <+> doubleQuotes (text n) <+> parens (pretty e))
            Error _ t m                                  -> parens (text "primError" <+> doubleQuotes (text m) <+> text "::" <+> pretty t)

---

data Pat = PatCon Annote (Embed Ty) (Embed (Name DataConId)) [Pat]
         | PatVar Annote (Embed Ty) (Name Exp)
            deriving (Show, Generic, Typeable, Data)

instance Alpha Pat

instance NFData Pat

instance TypeAnnotated Pat where
      typeOf = \ case
            PatCon  _ (Embed t) _ _ -> t
            PatVar  _ (Embed t) _   -> t

instance Annotated Pat where
      ann = \ case
            PatCon  a _ _ _ -> a
            PatVar  a _ _   -> a

instance Pretty Pat where
      pretty = \ case
            PatCon _ _ (Embed (n2s -> "(,)")) [p1, p2] -> parens $ pretty p1 <+> text "," <+> pretty p2
            PatCon _ _ (Embed n) ps                    -> parens $ pretty n <+> hsep (map pretty ps)
            PatVar _ _ n                               -> text $ show n


data MatchPat = MatchPatCon Annote Ty (Name DataConId) [MatchPat]
              | MatchPatVar Annote Ty
                 deriving (Show, Generic, Typeable, Data)

instance Alpha MatchPat

instance Subst Exp MatchPat
instance Subst Ty MatchPat

instance NFData MatchPat

instance TypeAnnotated MatchPat where
      typeOf = \ case
            MatchPatCon _ t _ _  -> t
            MatchPatVar _ t      -> t

instance Annotated MatchPat where
      ann = \ case
            MatchPatCon a _ _ _  -> a
            MatchPatVar a _      -> a

instance Pretty MatchPat where
      pretty = \ case
            MatchPatCon _ _ (n2s -> "(,)") [p1, p2] -> parens $ pretty p1 <+> text "," <+> pretty p2
            MatchPatCon _ _ n ps                    -> parens $ pretty n <+> hsep (map pretty ps)
            MatchPatVar _ t                         -> parens $ text "*" <+> text "::" <+> pretty t

---

data Defn = Defn
      { defnAnnote :: Annote
      , defnName   :: Name Exp
      , defnPolyTy :: Embed Poly
      , defnInline :: Bool
      , defnBody   :: Embed (Bind [Name Exp] Exp)
      } deriving (Generic, Show, Typeable, Data)

instance Alpha Defn

instance NFData Defn

instance Annotated Defn where
      ann (Defn a _ _ _ _) = a

instance Pretty Defn where
      pretty (Defn _ n t b (Embed e)) = runFreshM $ do
            (vs, e') <- unbind e
            return $ foldr ($+$) mempty
                  $  [ text (show n) <+> text "::" <+> pretty t ]
                  ++ [ text "{-# INLINE" <+> text (show n) <+> text "#-}" | b ]
                  ++ [ text (show n) <+> hsep (map (text . show) vs) <+> text "=", nest 4 $ pretty e' ]

---

data DataDefn = DataDefn
      { dataAnnote :: Annote
      , dataName   :: Name TyConId
      , dataKind   :: Kind
      , dataCons   :: [DataCon]
      } deriving (Generic, Show, Typeable, Data)

instance Alpha DataDefn

instance NFData DataDefn

instance Annotated DataDefn where
      ann (DataDefn a _ _ _) = a

instance Pretty DataDefn where
      pretty (DataDefn _ n k cs) = foldr ($+$) mempty $
                  (text "data" <+> pretty n <+> text "::" <+> pretty k <+> text "where")
                  : map (nest 4 . pretty) cs

---

type FreeProgram = ([DataDefn], [Defn])

newtype Program = Program (TRec ([DataDefn], [Defn]))
      deriving (Generic, Show, Typeable)

instance Alpha Program

instance NFData Program where
      rnf (Program p) = p `deepseq` ()

instance Pretty Program where
      pretty (Program p) = runFreshM $ do
            (ts, vs) <- untrec p
            return $ foldr (($+$) . pretty) mempty ts $+$ foldr (($+$) . pretty) mempty vs
---

flattenApp :: Exp -> [Exp]
flattenApp (App _ e e') = flattenApp e ++ [e']
flattenApp e            = [e]

rangeTy :: Ty -> Ty
rangeTy (TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) t') = rangeTy t'
rangeTy t                                                = t

arr :: Ty -> Ty -> Ty
arr t = TyApp (ann t) (TyApp (ann t) (TyCon (ann t) $ s2n "->") t)

arr' :: Ty -> Bind (Name Exp) Exp -> Ty
arr' t b = runFreshM $ do
      (_, e) <- unbind b
      return $ arr t $ typeOf e

arrowRight :: Ty -> Ty
arrowRight (TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) t') = t'
arrowRight t                                                = t

-- Orphans.

instance NFData a => NFData (TRec a) where
      rnf (TRec r) = r `deepseq` ()

deriving instance Data a => Data (Embed a)
deriving instance Data a => Data (Name a)
deriving instance (Data a, Data b) => Data (Bind a b)

instance Pretty AnyName where
      pretty (AnyName n) = pretty n

instance Pretty (Name a) where
      pretty n = text $ show n

instance Pretty a => Pretty (Embed a) where
      pretty (Embed a) = pretty a

{-# LANGUAGE MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , DeriveDataTypeable
           , DeriveGeneric
           , DerivingVia
           , Rank2Types
           , GADTs
           , ScopedTypeVariables
           , StandaloneDeriving
           , OverloadedStrings
           #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReWire.Crust.Syntax
      ( DataConId (..), TyConId
      , Ty (..), Exp (..), Pat (..), MatchPat (..)
      , Defn (..), DataDefn (..), DataCon (..)
      , FreeProgram, Program (..)
      , Kind (..), kblank
      , flattenApp, flattenArrow, flattenTyApp, arr, arrowRight
      , fv, fvAny
      , Fresh, Name, Embed (..), TRec, Bind
      , FieldId
      , trec, untrec, bind, unbind
      , Poly (..), (|->), poly
      , rangeTy, paramTys, isPrim
      , kmonad, tycomp
      , TypeAnnotated (..)
      ) where

import safe ReWire.Annotation
import safe ReWire.Unbound
      ( Fresh (..), runFreshM
      , Embed (..)
      , TRec (..), trec, untrec
      , Name, AnyName (..), SubstName (..)
      , Bind (..), bind, unbind
      , Alpha (..), aeq, Subst (..)
      , toListOf
      , n2s, s2n
      )
import safe qualified ReWire.Unbound as UB (fv, fvAny)

import safe Control.DeepSeq (NFData (..), deepseq)
import safe Data.Data (Typeable, Data (..))
import safe Data.List (intersperse)
import safe Data.Hashable (Hashable)
import safe Data.Text (Text)
import safe GHC.Generics (Generic (..))
import safe Prettyprinter
      ( Doc, nest, hsep, parens, dquotes
      , braces, vcat, (<+>), Pretty (..)
      )
import safe ReWire.Pretty (($+$), empty, text, hang)
import TextShow (TextShow (..), fromString)
import TextShow.Generic (FromGeneric (..), genericShowbPrec)

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

class Parenless a where
      -- | Parts that never need to be wrapped in parens during pretty printing.
      parenless :: a -> Bool

mparen :: (Pretty a, Parenless a) => a -> Doc ann
mparen a = if parenless a then pretty a else parens $ pretty a

newtype DataConId = DataConId Text
      deriving (Eq, Generic, Typeable, Data)
newtype TyConId = TyConId Text
      deriving (Eq, Generic, Typeable, Data)
newtype FieldId  = FieldId Text
      deriving (Eq, Generic, Typeable, Data)

instance Hashable DataConId
instance Hashable TyConId
instance Hashable FieldId

data DataCon = DataCon Annote !(Name DataConId) !(Embed Poly)
      deriving (Generic, Eq, Show, Typeable, Data)
      deriving TextShow via FromGeneric DataCon

instance Alpha DataCon

instance Annotated DataCon where
      ann (DataCon a _ _)  = a

instance Pretty DataCon where
      pretty (DataCon _ n t)  = pretty n <+> text "::" <+> pretty t

instance NFData DataCon

data Kind = KStar
          | KFun !Kind !Kind
          | KVar !(Name Kind)
      deriving (Generic, Ord, Eq, Show, Typeable, Data)
      deriving TextShow via FromGeneric Kind

infixr `KFun`

instance Hashable Kind

instance Alpha Kind

instance Subst Kind Kind where
      isvar (KVar x) = Just $ SubstName x
      isvar _        = Nothing

instance NFData Kind

instance Pretty Kind where
      pretty = \ case
            KStar            -> text "*"
            KVar n           -> text $ showt n
            KFun a@KFun {} b -> parens (pretty a) <+> text "->" <+> pretty b
            KFun a b         -> pretty a <+> text "->" <+> pretty b

newtype Poly = Poly (Bind [Name Ty] Ty)
      deriving (Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Poly

instance Hashable Poly

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
      pretty (Poly pt) = runFreshM (pretty <$> snd <$> unbind pt)

data Ty = TyApp Annote !Ty !Ty
        | TyCon Annote !(Name TyConId)
        | TyVar Annote !Kind !(Name Ty)
        | TyBlank Annote
      deriving (Eq, Ord, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Ty

instance Hashable Ty

instance Alpha Ty

instance Subst Ty Ty where
      isvar (TyVar _ _ x) = Just $ SubstName x
      isvar _             = Nothing
instance Subst Ty Annote where
      subst _ _ x = x
      substs _ x  = x
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
                  | n2s c == "(,)" -> parens $ ppTyArrowL t1 <> (text "," <+> pretty t2)
                  where ppTyArrowL :: Ty -> Doc ann
                        ppTyArrowL = \ case
                              t@(TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) _) -> parens $ pretty t
                              t                                                 -> pretty t
            TyApp _ t1 t2                  -> pretty t1 <+> ppTyAppR t2
                  where ppTyAppR :: Ty -> Doc ann
                        ppTyAppR = \ case
                              t@(TyApp _ (TyApp _ (TyCon _ (n2s -> "(,)")) _) _) -> pretty t
                              t@TyApp {}                                         -> parens $ pretty t
                              t                                                  -> pretty t
            TyCon _ n                      -> pretty n
            TyVar _ _ n                    -> pretty n
            TyBlank _                      -> text "_"

instance NFData Ty

----

instance (TextShow a, TextShow b) => TextShow (Bind a b) where
      showbPrec = genericShowbPrec

data Exp = App        Annote !Exp !Exp
         | Lam        Annote !Ty !(Bind (Name Exp) Exp)
         | Var        Annote !Ty !(Name Exp)
         | Con        Annote !Ty !(Name DataConId)
         | Case       Annote !Ty !Exp !(Bind Pat Exp) !(Maybe Exp)
         | Match      Annote !Ty !Exp !MatchPat !Exp ![Exp] !(Maybe Exp)
         | NativeVHDL Annote !Text !Exp
         | Error      Annote !Ty !Text
      deriving (Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Exp

instance Hashable Exp

instance Alpha Exp

instance Subst Exp Text where
      isvar _ = Nothing
      subst _ _ x = x
      substs _ x  = x
instance Subst Ty Text where
      isvar _ = Nothing
      subst _ _ x = x
      substs _ x  = x

instance Subst Exp Exp where
      isvar (Var _ _ x) = Just $ SubstName x
      isvar _           = Nothing
instance Subst Exp Annote where
      isvar _ = Nothing
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


instance Parenless Exp where
      parenless = \ case -- simple (non-compound?) expressions
            App _ (App _ (Con _ _ (n2s -> "(,)")) _) _ -> True
            Con {}                                     -> True
            Var {}                                     -> True
            _                                          -> False

instance Pretty Exp where
      pretty = \ case
            App _ (App _ (Con _ _ (n2s -> "(,)")) e1) e2 -> parens $ pretty e1 <> (text "," <+> pretty e2)
            App _ e1@App {} e2                           -> hang (pretty e1) 2 $ mparen e2
            App _ e1 e2                                  -> hang (mparen e1) 2 $ mparen e2
            Con _ t n                                    -> pretty n <+> braces (pretty t)
            Var _ t n                                    -> text (showt n) <+> braces (pretty t)
            Lam _ _ e                                    -> runFreshM $ do
                  (p, e') <- unbind e
                  pure $ text "\\" <+> text (showt p) <+> text "->" <+> pretty e'
            Case _ t e e1 e2                             -> runFreshM $ do
                  (p, e1') <- unbind e1
                  pure $ foldr ($+$) empty
                        [ text "case" <+> braces (pretty t) <+> pretty e <+> text "of"
                        , nest 2 (vcat $
                              ( pretty p <+> text "->" <+> pretty e1' )
                              : maybe [] (\ e2' -> [text "_" <+> text "->" <+> pretty e2']) e2
                              )
                        ]
            Match _ t e p e1 as e2                       -> runFreshM $
                  pure $ foldr ($+$) empty
                        [ text "match" <+> braces (pretty t) <+> pretty e <+> text "of"
                        , nest 2 (vcat $
                              ( pretty p <+> text "->" <+> pretty e1 <+> hsep (map pretty as) )
                              : maybe [] (\ e2' -> [text "_" <+> text "->" <+> pretty e2']) e2
                              )
                        ]
            NativeVHDL _ n e                             -> text "nativeVhdl" <+> dquotes (pretty n) <+> mparen e
            Error _ t m                                  -> text "error" <+> braces (pretty t) <+> dquotes (pretty m)

---

data Pat = PatCon Annote !(Embed Ty) !(Embed (Name DataConId)) ![Pat]
         | PatVar Annote !(Embed Ty) !(Name Exp)
      deriving (Eq, Show, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Pat

instance Hashable Pat

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

instance Parenless Pat where
      parenless = \ case
            PatCon _ _ (Embed (n2s -> "(,)")) _ -> True
            PatCon _ _ _ []                     -> True
            PatVar {}                           -> True
            _                                   -> False

instance Pretty Pat where
      pretty = \ case
            PatCon _ _ (Embed (n2s -> "(,)")) [p1, p2] -> parens $ pretty p1 <> (text "," <+> pretty p2)
            PatCon _ _ (Embed n) ps                    -> pretty n <+> hsep (map mparen ps)
            PatVar _ _ n                               -> text $ showt n


data MatchPat = MatchPatCon Annote !Ty !(Name DataConId) ![MatchPat]
              | MatchPatVar Annote !Ty
      deriving (Eq, Show, Generic, Typeable, Data)
      deriving TextShow via FromGeneric MatchPat

instance Hashable MatchPat

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

instance Parenless MatchPat where
      parenless = \ case
            MatchPatCon _ _ (n2s -> "(,)") _ -> True
            MatchPatCon _ _ _ []             -> True
            MatchPatVar {}                   -> True
            _                                -> False

instance Pretty MatchPat where
      pretty = \ case
            MatchPatCon _ _ (n2s -> "(,)") [p1, p2] -> parens $ pretty p1 <> (text "," <+> pretty p2)
            MatchPatCon _ _ n ps                    -> pretty n <+> hsep (map mparen ps)
            MatchPatVar _ t                         -> text "*" <+> braces (pretty t)

---

data Defn = Defn
      { defnAnnote :: Annote
      , defnName   :: !(Name Exp)
      , defnPolyTy :: !(Embed Poly)
      , defnInline :: !Bool
      , defnBody   :: !(Embed (Bind [Name Exp] Exp))
      }
      deriving (Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Defn

instance Hashable Defn

instance Alpha Defn

instance NFData Defn

instance Annotated Defn where
      ann (Defn a _ _ _ _) = a

instance Pretty Defn where
      pretty (Defn _ n t b (Embed e)) = runFreshM $ do
            (vs, e') <- unbind e
            pure $ foldr ($+$) empty
                  $  [ text (showt n) <+> text "::" <+> pretty t ]
                  ++ [ text "{-# INLINE" <+> text (showt n) <+> text "#-}" | b ]
                  ++ [ text (showt n) <+> hsep (map (text . showt) vs) <+> text "=" <+> pretty e' ]

---

data DataDefn = DataDefn
      { dataAnnote :: Annote
      , dataName   :: !(Name TyConId)
      , dataKind   :: !Kind
      , dataCoerce :: !Bool
      , dataCons   :: ![DataCon]
      }
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric DataDefn

instance Alpha DataDefn

instance NFData DataDefn

instance Annotated DataDefn where
      ann (DataDefn a _ _ _ _) = a

instance Pretty DataDefn where
      pretty (DataDefn _ n k _ cs) = foldr ($+$) empty $
                  (text "data" <+> pretty n <+> text "::" <+> pretty k <+> text "where")
                  : map (nest 2 . pretty) cs

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
            pure $ vcat (intersperse (text "") $ map pretty ts) $+$ text "" $+$ vcat (intersperse (text "") $ map pretty vs)
---

flattenApp :: Exp -> [Exp]
flattenApp = \ case
      App _ e e' -> flattenApp e ++ [e']
      e          -> [e]

flattenArrow :: Ty -> ([Ty], Ty)
flattenArrow = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2 -> (t1 : ts, t)
            where (ts, t) = flattenArrow t2
      t                                               -> ([], t)

flattenTyApp :: Ty -> [Ty]
flattenTyApp = \ case
      TyApp _ t t' -> flattenTyApp t ++ [t']
      t            -> [t]

rangeTy :: Ty -> Ty
rangeTy = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) t' -> rangeTy t'
      t                                              -> t

arr :: Ty -> Ty -> Ty
arr t = TyApp (ann t) (TyApp (ann t) (TyCon (ann t) $ s2n "->") t)

arr' :: Ty -> Bind (Name Exp) Exp -> Ty
arr' t b = runFreshM (arr t . typeOf <$> (snd <$> unbind b))

arrowRight :: Ty -> Ty
arrowRight = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) t' -> t'
      t                                              -> t

paramTys :: Ty -> [Ty]
paramTys = paramTys' []
      where paramTys' :: [Ty] -> Ty -> [Ty]
            paramTys' acc = \ case
                  TyApp    _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2  -> paramTys' (t1 : acc) t2
                  _                                                   -> reverse acc

isPrim :: Show a => a -> Bool
isPrim = notElem '.' . show

-- Orphans.

instance Alpha Text where
      aeq' _ctx i j = i == j
      fvAny' _ctx _nfn i = pure i
      close _ctx _b i = i
      open _ctx _b i = i
      isPat _ = mempty
      isTerm _ = mempty
      nthPatFind _ = mempty
      namePatFind _ = mempty
      swaps' _ctx _p i = i
      freshen' _ctx i = return (i, mempty)
      lfreshen' _ctx i cont = cont i mempty
      acompare' _ctx i j = compare i j

instance NFData a => NFData (TRec a) where
      rnf (TRec r) = r `deepseq` ()

instance Pretty AnyName where
      pretty (AnyName n) = pretty n

instance Pretty (Name a) where
      pretty n = text $ showt n

instance TextShow (Name a) where
      showb n = fromString $ show n

instance TextShow a => TextShow (Embed a) where
      showbPrec = genericShowbPrec

instance Pretty a => Pretty (Embed a) where
      pretty (Embed a) = pretty a

instance Hashable e => Hashable (Embed e)
instance Hashable e => Hashable (Name e)
instance (Hashable a, Hashable b) => Hashable (Bind a b)

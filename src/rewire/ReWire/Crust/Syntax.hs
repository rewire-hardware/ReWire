{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Crust.Syntax
      ( Module (..), DataConId (..), TyConId
      , Ty (..), Exp (..), Pat (..), MatchPat (..), Builtin (..)
      , Defn (..), DefnAttr (..), DataDefn (..), TypeSynonym (..), DataCon (..)
      , FreeProgram, Program (..)
      , Kind (..), FieldId
      , Poly (..)
      , prettyFP, flattenApp, flattenTyApp, builtins, builtinName
      ) where

import Prelude hiding (replicate)

import ReWire.Annotation (Annote, Annotated (ann))
import ReWire.Orphans ()
import ReWire.Pretty (empty, text, TextShow (showt), FromGeneric (..), Doc, nest, hsep, parens, dquotes, comma, brackets, vsep, (<+>), Pretty (pretty), punctuate)
import ReWire.Unbound(runFreshM, Embed (..), TRec (..), untrec, Name, SubstName (..), Bind (..), unbind, Alpha (..), aeq, Subst (..), n2s)

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData (..), deepseq)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Data (Typeable, Data (..))
import Data.Hashable (Hashable (..))
import Data.List (intersperse)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, unpack, replicate)
import Data.Tuple (swap)
import GHC.Generics (Generic (..))
import Numeric.Natural (Natural)

data Module = Module ![DataDefn] ![TypeSynonym] ![Defn]
      deriving (Show, Generic)
      deriving TextShow via FromGeneric Module

instance Pretty Module where
      pretty (Module cs ts ds) = nest 2 $ vsep (text "module" : map pretty cs <> map pretty ts <> map pretty ds)

instance Semigroup Module where
      -- TODO(chathhorn): shouldn't be necessary
      (Module a b c) <> (Module a' b' c') = Module (nubOrdOn (n2s . dataName) $ a <> a') (nubOrdOn (n2s . typeSynName) $ b <> b') (nubOrdOn (n2s . defnName) $ c <> c')

instance Monoid Module where
      mempty = Module [] [] []

---

class Parenless a where
      -- | Parts that never need to be wrapped in parens during pretty printing.
      parenless :: a -> Bool

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
      pretty (DataCon _ n t)  = text (n2s n) <+> text "::" <+> pretty t

instance NFData DataCon

data Kind = KStar
          | KNat
          | KFun !Kind !Kind
          | KVar !(Name Kind)
      deriving (Generic, Ord, Eq, Show, Typeable, Data)
      deriving TextShow via FromGeneric Kind

infixr `KFun`

instance Hashable Kind

instance Alpha Kind

instance Subst Kind Kind where
      isvar = \ case
            KVar x -> Just $ SubstName x
            _      -> Nothing

instance NFData Kind

instance Pretty Kind where
      pretty = \ case
            KStar            -> text "*"
            KNat             -> text "Nat"
            KVar n           -> text $ showt n
            KFun a@KFun {} b -> parens (pretty a) <+> text "->" <+> pretty b
            KFun a b         -> pretty a <+> text "->" <+> pretty b

newtype Poly = Poly (Bind [Name Ty] Ty)
      deriving (Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Poly

instance Hashable Poly

instance NFData Poly

instance Alpha Poly

instance Eq Poly where
      (==) = aeq

instance Pretty Poly where
      pretty (Poly pt) = runFreshM (pretty . snd <$> unbind pt)

data Ty = TyApp Annote !Ty !Ty
        | TyCon Annote !(Name TyConId)
        | TyVar Annote !Kind !(Name Ty)
        | TyNat Annote !Natural
      deriving (Eq, Ord, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Ty

instance Hashable Ty

instance Alpha Ty

instance Subst Ty Ty where
      isvar = \ case
            TyVar _ _ x -> Just $ SubstName x
            _           -> Nothing
instance Subst Ty Annote where
      subst _ _ x = x
      substs _ x  = x
instance Subst Ty Natural where
      subst _ _ x = x
      substs _ x  = x
instance Subst Ty Text where
      subst _ _ x = x
      substs _ x = x
instance Subst Ty Kind where
      subst _ _ x = x
      substs _ x = x
instance Subst Ty Builtin where
      subst _ _ x = x
      substs _ x = x
instance Subst Ty Exp
instance Subst Ty Pat
instance Subst Ty MatchPat
instance Subst Ty Poly

instance Annotated Ty where
      ann = \ case
            TyApp a _ _  -> a
            TyCon a _    -> a
            TyVar a _ _  -> a
            TyNat a _    -> a

instance Parenless Ty where
      parenless t = case flattenTyApp t of
            (TyCon _ (n2s -> c) : _)  | isTupleCtor c -> True
            [TyCon {}]                                -> True
            [TyVar {}]                                -> True
            [TyNat {}]                                -> True
            _                                         -> False

instance Pretty Ty where
      pretty t = case flattenTyApp t of
            (TyCon _ (n2s -> c)     : ts)
                  | isTupleCtor c               -> parens $ hsep $ punctuate comma $ map pretty ts
            (TyCon _ (n2s -> "->")  : [t1, t2])
                  | needsParens t1              -> parens (pretty t1) <+> text "->" <+> pretty t2
            (TyCon _ (n2s -> "->")  : [t1, t2]) -> pretty t1 <+> text "->" <+> pretty t2
            (TyCon _ (n2s -> "[_]") : [t'])     -> brackets $ pretty t'
            [TyCon _ n]                         -> text $ n2s n
            [TyVar _ _ n]                       -> text $ showt n
            [TyNat _ n]                         -> text $ showt n
            ts                                  -> hsep $ map mparens ts
            where needsParens :: Ty -> Bool
                  needsParens t = case flattenTyApp t of
                        (TyCon _ (n2s -> "->") : _) -> True
                        _                           -> False

instance NFData Ty

----

data Builtin = Error | Extern
             | SetRef | GetRef
             | Bind | Return
             | Put | Get
             | Signal | Lift | Extrude | Unfold
             | VecFromList | VecReplicate | VecReverse | VecSlice | VecRSlice | VecIndex | VecConcat
             | VecMap | VecFoldR | VecFoldL
             | Bits | Resize | BitSlice | BitIndex
             | Add | Sub | Mul | Div | Mod | Pow
             | LAnd | LOr
             | And | Or
             | XOr | XNor
             | LShift | RShift | RShiftArith
             | Eq | Gt | GtEq | Lt | LtEq
             | LNot | Not
             | RAnd | RNAnd | ROr | RNor | RXOr | RXNor
             | MSBit
      deriving (Eq, Generic, Show, Typeable, Data, Bounded, Enum)
      deriving TextShow via FromGeneric Builtin

instance Hashable Builtin
instance Alpha Builtin
instance NFData Builtin

instance Pretty Builtin where
      pretty = pretty . builtinName

----

data Exp = App     Annote !(Maybe Poly) !(Maybe Ty) !Exp !Exp
         | Lam     Annote !(Maybe Poly) !(Maybe Ty) !(Bind (Name Exp) Exp)
         | Var     Annote !(Maybe Poly) !(Maybe Ty) !(Name Exp)
         | Con     Annote !(Maybe Poly) !(Maybe Ty) !(Name DataConId)
         | Case    Annote !(Maybe Poly) !(Maybe Ty) !Exp !(Bind Pat Exp) !(Maybe Exp)
         | Match   Annote !(Maybe Poly) !(Maybe Ty) !Exp !MatchPat !Exp !(Maybe Exp)
         | Builtin Annote !(Maybe Poly) !(Maybe Ty) !Builtin
         | LitInt  Annote !(Maybe Poly) !Integer
         | LitStr  Annote !(Maybe Poly) !Text
         | LitVec  Annote !(Maybe Poly) !(Maybe Ty) ![Exp]
         | LitList Annote !(Maybe Poly) !(Maybe Ty) ![Exp]
      deriving (Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Exp

instance Hashable Exp

instance Eq Exp where
      a == b = hash a == hash b

instance Alpha Exp

instance Subst Exp Exp where
      isvar = \ case
            Var _ _ _ x -> Just $ SubstName x
            _           -> Nothing
instance Subst Exp Annote where
      subst _ _ x = x
      substs _ x = x
instance Subst Exp Natural where
      subst _ _ x = x
      substs _ x = x
instance Subst Exp Text where
      subst _ _ x = x
      substs _ x = x
instance Subst Exp Ty where
      subst _ _ x = x
      substs _ x = x
instance Subst Exp Kind where
      subst _ _ x = x
      substs _ x = x
instance Subst Exp Pat where
      subst _ _ x = x
      substs _ x = x
instance Subst Exp MatchPat where
      subst _ _ x = x
      substs _ x = x
instance Subst Exp DefnAttr where
      subst _ _ x = x
      substs _ x = x
instance Subst Exp Poly where
      subst _ _ x = x
      substs _ x = x
instance Subst Exp Builtin where
      subst _ _ x = x
      substs _ x = x
instance Subst Exp Defn

instance NFData Exp

instance Annotated Exp where
      ann = \ case
            App a _ _ _ _       -> a
            Lam a _ _ _         -> a
            Var a _ _ _         -> a
            Con a _ _ _         -> a
            Case a _ _ _ _ _    -> a
            Match a _ _ _ _ _ _ -> a
            Builtin a _ _ _     -> a
            LitInt a _ _        -> a
            LitStr a _ _        -> a
            LitList a _ _ _     -> a
            LitVec a _ _ _      -> a

-- | Does this exp have a type annotation? Avoids depending on ReWire.Crust.Types.
typeAnnotated :: Exp -> Bool
typeAnnotated = isJust . \ case
      App _ t _ _ _       -> t
      Lam _ t _ _         -> t
      Var _ t _ _         -> t
      Con _ t _ _         -> t
      Case _ t _ _ _ _    -> t
      Match _ t _ _ _ _ _ -> t
      Builtin _ t _ _     -> t
      LitInt _ t _        -> t
      LitStr _ t _        -> t
      LitList _ t _ _     -> t
      LitVec _ t _ _      -> t

instance Parenless Exp where
      parenless e = case flattenApp e of
            -- TODO(chathhorn): rework pretty printing to handle type annotations on applications.
            (Con _ Nothing _ (n2s -> c) : _)  | isTupleCtor c -> True
            [e'] | typeAnnotated e'                           -> False
            [Con {}]                                          -> True
            [Var {}]                                          -> True
            [Builtin {}]                                      -> True
            [LitInt {}]                                       -> True
            [LitStr {}]                                       -> True
            [LitList {}]                                      -> True
            [LitVec {}]                                       -> True
            _                                                 -> False

instance Pretty Exp where
      pretty e = case flattenApp e of
            (Con _ pt _ (n2s -> c) : es) | isTupleCtor c -> ppTyAnn pt $ parens $ hsep $ punctuate comma $ map pretty es
            [Con _ pt _ n]                               -> ppTyAnn pt $ text $ n2s n
            [Var _ pt _ n]                               -> ppTyAnn pt $ text $ showt n
            [Lam _ pt _ e]                               -> ppTyAnn pt $ runFreshM $ do
                  (p, e') <- unbind e
                  pure $ text "\\" <+> text (showt p) <+> text "->" <+> pretty e'
            [Case _ pt _ e e1 e2]                        -> ppTyAnn pt $ runFreshM $ do
                  (p, e1') <- unbind e1
                  pure $ nest 2 $ vsep $
                        [ text "case" <+> pretty e <+> text "of"
                        , pretty p <+> text "->" <+> pretty e1'
                        ] ++ maybe [] (\ e2' -> [text "_" <+> text "->" <+> pretty e2']) e2
            [Match _ pt _ e p e1 e2]                     -> ppTyAnn pt $ runFreshM $
                  pure $ nest 2 $ vsep $
                        [ text "match" <+> pretty e <+> text "of"
                        , pretty p <+> text "->" <+> pretty e1
                        ] ++ maybe [] (\ e2' -> [text "_" <+> text "->" <+> pretty e2']) e2
            [Builtin _ pt _ b]                           -> ppTyAnn pt $ pretty b
            [LitInt _ pt v]                              -> ppTyAnn pt $ pretty v
            [LitStr _ pt v]                              -> ppTyAnn pt $ dquotes $ pretty v
            [LitList _ pt _ vs]                          -> ppTyAnn pt $ brackets $ hsep $ punctuate comma $ map pretty vs
            [LitVec _ pt _ vs]                           -> ppTyAnn pt $ brackets $ hsep $ punctuate comma $ map pretty vs
            es                                           -> nest 2 $ hsep $ map mparens es

---

data Pat = PatCon      Annote !(Embed (Maybe Poly)) !(Embed (Maybe Ty)) !(Embed (Name DataConId)) ![Pat]
         | PatVar      Annote !(Embed (Maybe Poly)) !(Embed (Maybe Ty)) !(Name Exp)
         | PatWildCard Annote !(Embed (Maybe Poly)) !(Embed (Maybe Ty))
      deriving (Eq, Show, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Pat

instance Hashable Pat

instance Alpha Pat

instance NFData Pat

instance Annotated Pat where
      ann = \ case
            PatCon a _ _ _ _  -> a
            PatVar a _ _ _    -> a
            PatWildCard a _ _ -> a

instance Parenless Pat where
      parenless = \ case
            PatCon _ _ _ (Embed (n2s -> c)) _ | isTupleCtor c -> True
            PatCon _ _ _ _ []                                 -> True
            PatVar {}                                         -> True
            PatWildCard {}                                    -> True
            _                                                 -> False

instance Pretty Pat where
      pretty = \ case
            PatCon _ (Embed pt) _ (Embed (n2s -> c)) ps | isTupleCtor c -> ppTyAnn pt $ parens $ hsep $ punctuate comma $ map pretty ps
            PatCon _ (Embed pt) _ (Embed n) ps                          -> ppTyAnn pt $ text (n2s n) <+> hsep (map mparens ps)
            PatVar _ (Embed pt) _ n                                     -> ppTyAnn pt $ text $ showt n
            PatWildCard _ (Embed pt) _                                  -> ppTyAnn pt $ text "_"

data MatchPat = MatchPatCon Annote !(Maybe Poly) !(Maybe Ty) !(Name DataConId) ![MatchPat]
              | MatchPatVar Annote !(Maybe Poly) !(Maybe Ty)
              | MatchPatWildCard Annote !(Maybe Poly) !(Maybe Ty)
      deriving (Eq, Show, Generic, Typeable, Data)
      deriving TextShow via FromGeneric MatchPat

instance Hashable MatchPat

instance Alpha MatchPat

instance NFData MatchPat

instance Annotated MatchPat where
      ann = \ case
            MatchPatCon a _ _ _ _  -> a
            MatchPatVar a _ _      -> a
            MatchPatWildCard a _ _ -> a

instance Parenless MatchPat where
      parenless = \ case
            MatchPatCon _ _ _ (n2s -> c) _ | isTupleCtor c -> True
            MatchPatCon _ _ _ _ []                         -> True
            MatchPatVar {}                                 -> True
            MatchPatWildCard {}                            -> True
            _                                              -> False

instance Pretty MatchPat where
      pretty = \ case
            MatchPatCon _ pt _ (n2s -> c) ps | isTupleCtor c -> ppTyAnn pt $ parens $ hsep $ punctuate comma $ map pretty ps
            MatchPatCon _ pt _ n ps                          -> ppTyAnn pt $ text (n2s n) <+> hsep (map mparens ps)
            MatchPatVar _ pt t                               -> ppTyAnn pt $ parens $ text "*" <+> text "::" <+> pretty t
            MatchPatWildCard _ pt t                          -> ppTyAnn pt $ parens $ text "_" <+> text "::" <+> pretty t

---

data Defn = Defn
      { defnAnnote :: Annote
      , defnName   :: !(Name Exp)
      , defnPolyTy :: !(Embed Poly)
      , defnAttr   :: !(Maybe DefnAttr)
      , defnBody   :: !(Embed (Bind [Name Exp] Exp))
      }
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Defn

instance Hashable Defn

instance Alpha Defn

instance NFData Defn

instance Annotated Defn where
      ann (Defn a _ _ _ _) = a

instance Pretty Defn where
      pretty (Defn _ n t b (Embed e)) = runFreshM $ do
            (vs, e') <- unbind e
            pure $ vsep $ map (nest 2)
                 $  [ text "{-# INLINE" <+> text (showt n) <+> text "#-}"   | b == Just Inline   ]
                 <> [ text "{-# NOINLINE" <+> text (showt n) <+> text "#-}" | b == Just NoInline ]
                 <> [ text (showt n) <+> text "::" <+> pretty t ]
                 <> [ text (showt n) <+> hsep (map (text . showt) vs) <+> text "=" <+> pretty e' ]

---

data DefnAttr = Inline | NoInline
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric DefnAttr

instance Hashable DefnAttr

instance Alpha DefnAttr

instance NFData DefnAttr

---

data DataDefn = DataDefn
      { dataAnnote :: Annote
      , dataName   :: !(Name TyConId)
      , dataKind   :: !Kind
      , dataCons   :: ![DataCon]
      }
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric DataDefn

instance Alpha DataDefn

instance NFData DataDefn

instance Annotated DataDefn where
      ann (DataDefn a _ _ _) = a

instance Pretty DataDefn where
      pretty (DataDefn _ n k cs) = nest 2 $ vsep
            $ (text "data" <+> text (n2s n) <+> text "::" <+> pretty k <+> text "where")
            : map (nest 2 . pretty) cs

---

data TypeSynonym = TypeSynonym
      { typeSynAnnote :: Annote
      , typeSynName   :: !(Name TyConId)
      , typeSynType   :: !(Embed Poly)
      }
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric TypeSynonym

instance Hashable TypeSynonym

instance Alpha TypeSynonym

instance NFData TypeSynonym

instance Annotated TypeSynonym where
      ann (TypeSynonym a _ _) = a

instance Pretty TypeSynonym where
      pretty (TypeSynonym _ n (Embed (Poly t))) = runFreshM $ do
            (tvs, t') <- unbind t
            pure (text "type" <+> text (n2s n) <+> hsep (map (text . n2s) tvs) <+> "=" <+> pretty t')
---

type FreeProgram = ([DataDefn], [TypeSynonym], [Defn])

newtype Program = Program (TRec ([DataDefn], [TypeSynonym], [Defn]))
      deriving (Generic, Show, Typeable)

instance Alpha Program

instance NFData Program where
      rnf (Program p) = p `deepseq` ()

instance Pretty Program where
      pretty (Program p) = runFreshM $ prettyFP <$> untrec p

---

mparens :: (Pretty a, Parenless a) => a -> Doc ann
mparens a = if parenless a then pretty a else parens $ pretty a

ppTyAnn :: Maybe Poly -> Doc ann -> Doc ann
ppTyAnn Nothing d = d
ppTyAnn (Just pt) d = d <+> text "::" <+> pretty pt

-- TODO(chathhorn): make FreeProgram newtype.
prettyFP :: FreeProgram -> Doc ann
prettyFP (ts, syns, vs) = vsep $ intersperse empty $ map pretty ts <> map pretty syns <> map pretty vs

isTupleCtor :: Text -> Bool
isTupleCtor c = c == "(" <> replicate (length (unpack c) - 2) "," <> ")"

-- | Turns:
-- > (App (App (App v e1) e2) e3)
-- into
-- > [v, e1, e2, e3]
flattenApp :: Exp -> [Exp]
flattenApp = \ case
      App _ _ _ e e' -> flattenApp e <> [e']
      e              -> [e]

flattenTyApp :: Ty -> [Ty]
flattenTyApp = \ case
      TyApp _ t t' -> flattenTyApp t <> [t']
      t            -> [t]

builtinName :: Builtin -> Text
builtinName b = fromMaybe "" $ lookup b $ map swap builtins

builtins :: [(Text, Builtin)]
builtins = map ((("rwPrim" <>) . showt) &&& id) [minBound .. maxBound]

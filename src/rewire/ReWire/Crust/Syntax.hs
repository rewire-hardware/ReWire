{-# LANGUAGE MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , DeriveDataTypeable
           , DeriveGeneric
           , DerivingVia
           , Rank2Types
           , GADTs
           , ScopedTypeVariables
           , OverloadedStrings
           #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReWire.Crust.Syntax
      ( Module (..), DataConId (..), TyConId
      , Ty (..), Exp (..), Pat (..), MatchPat (..), Builtin (..), builtins, builtin, builtinName
      , Defn (..), DefnAttr (..), DataDefn (..), TypeSynonym (..), DataCon (..)
      , FreeProgram, Program (..)
      , Kind (..), kblank
      , flattenApp, flattenArrow, flattenTyApp, arr, arrowRight, arrowLeft
      , fv, fvAny
      , Fresh, Name, Embed (..), TRec, Bind, FieldId
      , trec, untrec, bind, unbind
      , Poly (..), (|->), poly, poly'
      , rangeTy, paramTys, isPrim, inlineable, mustInline
      , mkArrowTy, nil, isResMonad, isStateMonad
      , strTy, intTy, boolTy, listTy, pairTy, refTy, vecTy, plusTy, plus, nilTy, vecElemTy, vecSize, proxyNat
      , evalNat, flattenAllTyApp, resInputTy
      , mkTuple, mkTuplePat, mkTupleMPat, tupleTy
      , mkPair, mkPairPat, mkPairMPat
      , kmonad, tycomp, concrete, higherOrder, fundamental, mkApp, mkError
      , TypeAnnotated (..), prettyFP
      ) where

import safe ReWire.Annotation
import safe ReWire.Unbound
      ( Fresh (..), runFreshM
      , Embed (..)
      , TRec (..), trec, untrec
      , Name, AnyName (..), SubstName (..)
      , Bind (..), bind, unbind, unsafeUnbind
      , Alpha (..), aeq, Subst (..)
      , toListOf
      , n2s, s2n
      )
import safe qualified ReWire.Unbound as UB (fv, fvAny)

import Prelude hiding (replicate)

import safe Control.Arrow ((&&&))
import safe Control.DeepSeq (NFData (..), deepseq)
import safe Data.Containers.ListUtils (nubOrd, nubOrdOn)
import safe Data.Data (Typeable, Data (..))
import safe Data.Hashable (Hashable (..))
import safe Data.List (intersperse, foldl')
import safe Data.Maybe (fromMaybe)
import safe Data.Text (Text, unpack, replicate)
import safe Data.Tuple (swap)
import safe GHC.Generics (Generic (..))
import safe Prettyprinter
      ( Doc, nest, hsep, parens, dquotes, comma, brackets
      , vsep, (<+>), Pretty (..), punctuate
      )
import safe ReWire.Pretty (empty, text)
import safe Numeric.Natural (Natural)
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

class TypeAnnotated a where
      typeOf   :: a -> Maybe Ty
      tyAnn    :: a -> Maybe Poly
      setTyAnn :: Maybe Poly -> a -> a

class Parenless a where
      -- | Parts that never need to be wrapped in parens during pretty printing.
      parenless :: a -> Bool

mparens :: (Pretty a, Parenless a) => a -> Doc ann
mparens a = if parenless a then pretty a else parens $ pretty a

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

poly :: [Name Ty] -> Ty -> Poly
poly vs t = Poly $ bind vs t

poly' :: Ty -> Poly
poly' t = poly (nubOrd $ fv t) t

(|->) :: [Name Ty] -> Ty -> Embed Poly
vs |-> t = Embed $ poly vs t

infix 1 |->

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

builtins :: [(Text, Builtin)]
builtins = map ((("rwPrim" <>) . showt) &&& id) [minBound .. maxBound]

builtin :: Text -> Maybe Builtin
builtin b = lookup b builtins

builtinName :: Builtin -> Text
builtinName b = fromMaybe "" $ lookup b $ map swap builtins

----

instance (TextShow a, TextShow b) => TextShow (Bind a b) where
      showbPrec = genericShowbPrec

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

instance TypeAnnotated Exp where
      typeOf = \ case
            App _ _ t _ _         -> t
            Lam _ _ t e           -> arr <$> t <*> (typeOf $ snd $ unsafeUnbind e)
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

instance Parenless Exp where
      parenless e = case flattenApp e of
            -- TODO(chathhorn): rework pretty printing to handle type annotations on applications.
            (Con _ Nothing _ (n2s -> c) : _)  | isTupleCtor c -> True
            [e'] | Just _ <- tyAnn e'                         -> False
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

ppTyAnn :: Maybe Poly -> Doc ann -> Doc ann
ppTyAnn Nothing d = d
ppTyAnn (Just pt) d = d <+> text "::" <+> pretty pt

-- TODO(chathhorn): make FreeProgram newtype.
prettyFP :: FreeProgram -> Doc ann
prettyFP (ts, syns, vs) = vsep $ intersperse empty $ map pretty ts <> map pretty syns <> map pretty vs

newtype Program = Program (TRec ([DataDefn], [TypeSynonym], [Defn]))
      deriving (Generic, Show, Typeable)

instance Alpha Program

instance NFData Program where
      rnf (Program p) = p `deepseq` ()

instance Pretty Program where
      pretty (Program p) = runFreshM $ prettyFP <$> untrec p

---

-- | Turns:
-- > (App (App (App v e1) e2) e3)
-- into
-- > [v, e1, e2, e3]
flattenApp :: Exp -> [Exp]
flattenApp = \ case
      App _ _ _ e e' -> flattenApp e <> [e']
      e              -> [e]

flattenArrow :: Ty -> ([Ty], Ty)
flattenArrow = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2 -> (t1 : ts, t)
            where (ts, t) = flattenArrow t2
      t                                               -> ([], t)

paramTys :: Ty -> [Ty]
paramTys = fst . flattenArrow

rangeTy :: Ty -> Ty
rangeTy = snd . flattenArrow

flattenTyApp :: Ty -> [Ty]
flattenTyApp = \ case
      TyApp _ t t' -> flattenTyApp t <> [t']
      t            -> [t]

flattenAllTyApp :: Ty -> [Ty]
flattenAllTyApp = \ case
      TyApp _ t t' -> flattenAllTyApp t <> flattenAllTyApp t'
      t            -> [t]

arr :: Ty -> Ty -> Ty
arr t = TyApp (ann t) (TyApp (ann t) (TyCon (ann t) $ s2n "->") t)

infixr 1 `arr`

intTy :: Annote -> Ty
intTy an = TyCon an $ s2n "Integer"

strTy :: Annote -> Ty
strTy an = TyCon an $ s2n "String"

listTy :: Annote -> Ty -> Ty
listTy an = TyApp an $ TyCon an $ s2n "[_]"

refTy :: Annote -> Ty -> Ty
refTy an = TyApp an $ TyCon an $ s2n "Ref"

boolTy :: Annote -> Ty
boolTy an = TyCon an $ s2n "Bool"

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

evalNat :: Ty -> Maybe Natural
evalNat = \ case
      TyNat _ n               -> pure n
      (plus -> Just (n1, n2)) -> (+) <$> evalNat n1 <*> evalNat n2
      _                       -> Nothing

-- | Given 'a -> (b -> c)' returns 'b -> c'.
arrowRight :: Ty -> Ty
arrowRight = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) t' -> t'
      t                                              -> t

-- | Given 'a -> (b -> c)' returns 'a'.
arrowLeft :: Ty -> Ty
arrowLeft = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t') _ -> t'
      t                                              -> t

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

-- | Takes [T1, ..., Tn-1] Tn and returns (T1 -> (T2 -> ... (T(n-1) -> Tn) ...))
mkArrowTy :: [Ty] -> Ty -> Ty
mkArrowTy ps = foldr1 arr . (ps ++) . (: [])

nilTy :: Ty
nilTy = TyCon (MsgAnnote "nilTy") (s2n "()")

nil :: Exp
nil = Con (MsgAnnote "nil") Nothing (Just nilTy) (s2n "()")

nilPat :: Pat
nilPat = PatCon (MsgAnnote "nilPat") (Embed Nothing) (Embed $ Just nilTy) (Embed $ s2n "()") []

nilMPat :: MatchPat
nilMPat = MatchPatCon (MsgAnnote "nilMPat") Nothing (Just nilTy) (s2n "()") []

pairTy :: Annote -> Ty -> Ty -> Ty
pairTy an t = TyApp an $ TyApp an (TyCon an $ s2n "(,)") t

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

tupleTy :: Annote -> [Ty] -> Ty
tupleTy an = foldr (pairTy an) $ nilTy

mkTuplePat :: Annote -> [Pat] -> Pat
mkTuplePat an = foldr (mkPairPat an) nilPat

mkTupleMPat :: Annote -> [MatchPat] -> MatchPat
mkTupleMPat an = foldr (mkPairMPat an) nilMPat

isTupleCtor :: Text -> Bool
isTupleCtor c = c == "(" <> replicate (length (unpack c) - 2) "," <> ")"

isResMonad :: Ty -> Bool
isResMonad ty = case rangeTy ty of
      TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) _) _) _) _ -> True
      _                                                                        -> False

resInputTy :: Ty -> Maybe Ty
resInputTy ty = case rangeTy ty of
      TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) ip) _) _) _ -> Just ip
      _                                                                         -> Nothing

isStateMonad :: Ty -> Bool
isStateMonad ty = case rangeTy ty of
      TyApp an (TyApp _ (TyApp _ (TyCon _ (n2s -> "StateT")) _) m) a -> isStateMonad (tycomp an m a)
      TyApp _ (TyCon _ (n2s -> "Identity")) _                        -> True
      _                                                              -> False

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

isArrow :: Ty -> Bool
isArrow = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) _ -> True
      _                                             -> False

mkApp :: Annote -> Exp -> [Exp] -> Exp
mkApp an = foldl' $ \ e -> App an Nothing (arrowRight <$> typeOf e) e

mkError :: Annote -> Maybe Ty -> Text -> Exp
mkError an t err = App an Nothing t (Builtin an Nothing (arr (strTy an) <$> t) Error) $ LitStr an Nothing err

-- Orphans.

instance (Eq a, Eq b) => Eq (Bind a b) where
      (B a b) == (B a' b') = a == a' && b == b'

instance Alpha Text where
      aeq' _ctx i j = i == j
      fvAny' _ctx _nfn = pure
      close _ctx _b i = i
      open _ctx _b i = i
      isPat _ = mempty
      isTerm _ = mempty
      nthPatFind _ = mempty
      namePatFind _ = mempty
      swaps' _ctx _p i = i
      freshen' _ctx i = return (i, mempty)
      lfreshen' _ctx i cont = cont i mempty
      acompare' _ctx = compare

instance Alpha Natural where
      aeq' _ctx i j = i == j
      fvAny' _ctx _nfn = pure
      close _ctx _b i = i
      open _ctx _b i = i
      isPat _ = mempty
      isTerm _ = mempty
      nthPatFind _ = mempty
      namePatFind _ = mempty
      swaps' _ctx _p i = i
      freshen' _ctx i = return (i, mempty)
      lfreshen' _ctx i cont = cont i mempty
      acompare' _ctx = compare

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

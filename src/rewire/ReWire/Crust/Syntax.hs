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
      ( DataConId (..), TyConId
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
      , strTy, intTy, bitTy, listTy, pairTy, refTy, vecTy, plusTy
      , flattenAllTyApp, resInputTy
      , mkTuple, mkTuplePat, mkTupleMPat, tupleTy
      , mkPair, mkPairPat, mkPairMPat
      , kmonad, tycomp, concrete, higherOrder, fundamental
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

import safe Control.DeepSeq (NFData (..), deepseq)
import safe Data.Containers.ListUtils (nubOrd)
import safe Data.Data (Typeable, Data (..))
import safe Data.Hashable (Hashable (..))
import safe Data.List (intersperse)
import safe Data.Maybe (fromMaybe)
import safe Data.Text (Text, unpack, replicate)
import safe Data.Tuple (swap)
import safe GHC.Generics (Generic (..))
import safe Prettyprinter
      ( Doc, nest, hsep, parens, dquotes, comma, brackets
      , braces, vsep, (<+>), Pretty (..), punctuate
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

class TypeAnnotated a where
      typeOf :: a -> Ty

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
        | TyBlank Annote
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
            TyBlank a    -> a
            TyNat a _    -> a

instance Parenless Ty where
      parenless t = case flattenTyApp t of
            (TyCon _ (n2s -> c) : _)  | isTupleCtor c -> True
            [TyCon {}]                                -> True
            [TyVar {}]                                -> True
            [TyBlank {}]                              -> True
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
            [TyVar _ _ n]                       -> text $ n2s n
            [TyBlank _]                         -> text "_"
            [TyNat _ n]                         -> text $ showt n
            ts                                  -> hsep $ map mparens ts
            where needsParens :: Ty -> Bool
                  needsParens t = case flattenTyApp t of
                        (TyCon _ (n2s -> "->") : _) -> True
                        _                           -> False

instance NFData Ty

----

data Builtin = Extern
             | BitIndex
             | BitSlice
             | SetRef | GetRef
             | Put    | Get
             | Bind   | Return  | Lift
             | Signal | Extrude | Unfold
             | Resize | Bits
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Builtin

instance Hashable Builtin
instance Alpha Builtin
instance NFData Builtin

instance Pretty Builtin where
      pretty = pretty . builtinName

builtins :: [(Text, Builtin)]
builtins =
      [ ("externWithSig" , Extern)
      , ("bitIndex"      , BitIndex)
      , ("bitSlice"      , BitSlice)
      , ("setRef"        , SetRef)
      , ("getRef"        , GetRef)
      , ("put"           , Put)
      , ("get"           , Get)
      , ("rwBind"        , Bind)
      , ("rwReturn"      , Return)
      , ("lift"          , Lift)
      , ("signal"        , Signal)
      , ("extrude"       , Extrude)
      , ("unfold"        , Unfold)
      , ("resize"        , Resize)
      , ("bits"          , Bits)
      ]

builtin :: Text -> Maybe Builtin
builtin b = lookup b builtins

builtinName :: Builtin -> Text
builtinName b = fromMaybe "" $ lookup b $ map swap builtins

----

instance (TextShow a, TextShow b) => TextShow (Bind a b) where
      showbPrec = genericShowbPrec

data Exp = App     Annote !Exp      !Exp
         | Lam     Annote !Ty       !(Bind (Name Exp) Exp)
         | Var     Annote !Ty       !(Name Exp)
         | Con     Annote !Ty       !(Name DataConId)
         | Case    Annote !Ty       !Exp !(Bind Pat Exp) !(Maybe Exp)
         | Match   Annote !Ty       !Exp !MatchPat !Exp !(Maybe Exp)
         | Builtin Annote !Ty       !Builtin
         | LitInt  Annote !Integer
         | LitStr  Annote !Text
         | LitVec  Annote !Ty       ![Exp]
         | LitList Annote !Ty       ![Exp]
         | Error   Annote !Ty       !Text
         | TypeAnn Annote !Poly     !Exp
      deriving (Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Exp

instance Hashable Exp

instance Eq Exp where
      a == b = hash a == hash b

instance Alpha Exp

instance Subst Exp Exp where
      isvar = \ case
            Var _ _ x -> Just $ SubstName x
            _         -> Nothing
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
            App _ e _         -> arrowRight $ typeOf e
            Lam _ t e         -> arr' t e
            Var _ t _         -> t
            Con _ t _         -> t
            Case _ t _ _ _    -> t
            Match _ t _ _ _ _ -> t
            Builtin _ t _     -> t
            LitInt a _        -> intTy a
            LitStr a _        -> strTy a
            LitList _ t _     -> t
            LitVec _ t _      -> t
            Error _ t _       -> t
            TypeAnn _ _ e     -> typeOf e

instance Annotated Exp where
      ann = \ case
            App a _ _         -> a
            Lam a _ _         -> a
            Var a _ _         -> a
            Con a _ _         -> a
            Case a _ _ _ _    -> a
            Match a _ _ _ _ _ -> a
            Builtin a _ _     -> a
            LitInt a _        -> a
            LitStr a _        -> a
            LitList a _ _     -> a
            LitVec a _ _      -> a
            Error a _ _       -> a
            TypeAnn a _ _     -> a

instance Parenless Exp where
      parenless e = case flattenApp e of
            (Con _ _ (n2s -> c) : _)  | isTupleCtor c -> True
            [Con {}]                                  -> True
            [Var {}]                                  -> True
            [Builtin {}]                              -> True
            [LitInt {}]                               -> True
            [LitStr {}]                               -> True
            [LitList {}]                              -> True
            [LitVec {}]                               -> True
            _                                         -> False

instance Pretty Exp where
      pretty e = case flattenApp e of
            (Con _ _ (n2s -> c) : es) | isTupleCtor c    -> parens $ hsep $ punctuate comma $ map pretty es
            [Con _ t n]                                  -> tyAnn (text $ n2s n) t
            [Var _ t n]                                  -> tyAnn (text $ showt n) t
            [Lam _ _ e]                                  -> runFreshM $ do
                  (p, e') <- unbind e
                  pure $ text "\\" <+> text (showt p) <+> text "->" <+> pretty e'
            [Case _ t e e1 e2]                           -> runFreshM $ do
                  (p, e1') <- unbind e1
                  pure $ nest 2 $ vsep $
                        [ tyAnn (text "case") t <+> pretty e <+> text "of"
                        , pretty p <+> text "->" <+> pretty e1'
                        ] ++ maybe [] (\ e2' -> [text "_" <+> text "->" <+> pretty e2']) e2
            [Match _ t e p e1 e2]                        -> runFreshM $
                  pure $ nest 2 $ vsep $
                        [ tyAnn (text "match") t <+> pretty e <+> text "of"
                        , pretty p <+> text "->" <+> pretty e1
                        ] ++ maybe [] (\ e2' -> [text "_" <+> text "->" <+> pretty e2']) e2
            [Builtin _ t b]                              -> tyAnn (pretty b) t
            [LitInt _ v]                                 -> pretty v
            [LitStr _ v]                                 -> dquotes $ pretty v
            [LitList _ _ vs]                             -> brackets $ hsep $ punctuate comma $ map pretty vs
            [LitVec _ _ vs]                              -> brackets $ hsep $ punctuate comma $ map pretty vs
            [Error _ t m]                                -> tyAnn (text "error") t <+> dquotes (pretty m)
            [TypeAnn _ pt e]                             -> pretty e <+> text "::" <+> pretty pt
            es                                           -> nest 2 $ hsep $ map mparens es
            where tyAnn :: Doc ann -> Ty -> Doc ann
                  tyAnn d = \ case
                        t | not $ isBlank t -> d <+> braces (pretty t)
                        _                   -> d
---

data Pat = PatCon      Annote !(Embed Ty) !(Embed (Name DataConId)) ![Pat]
         | PatVar      Annote !(Embed Ty) !(Name Exp)
         | PatWildCard Annote !(Embed Ty)
      deriving (Eq, Show, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Pat

instance Hashable Pat

instance Alpha Pat

instance NFData Pat

instance TypeAnnotated Pat where
      typeOf = \ case
            PatCon  _ (Embed t) _ _  -> t
            PatVar  _ (Embed t) _    -> t
            PatWildCard  _ (Embed t) -> t

instance Annotated Pat where
      ann = \ case
            PatCon  a _ _ _  -> a
            PatVar  a _ _    -> a
            PatWildCard  a _ -> a

instance Parenless Pat where
      parenless = \ case
            PatCon _ _ (Embed (n2s -> c)) _ | isTupleCtor c -> True
            PatCon _ _ _ []                                 -> True
            PatVar {}                                       -> True
            PatWildCard {}                                  -> True
            _                                               -> False

instance Pretty Pat where
      pretty = \ case
            PatCon _ _ (Embed (n2s -> c)) ps | isTupleCtor c -> parens $ hsep $ punctuate comma $ map pretty ps
            PatCon _ _ (Embed n) ps                          -> text (n2s n) <+> hsep (map mparens ps)
            PatVar _ _ n                                     -> text $ showt n
            PatWildCard _ _                                  -> text "_"


data MatchPat = MatchPatCon Annote !Ty !(Name DataConId) ![MatchPat]
              | MatchPatVar Annote !Ty
              | MatchPatWildCard Annote !Ty
      deriving (Eq, Show, Generic, Typeable, Data)
      deriving TextShow via FromGeneric MatchPat

instance Hashable MatchPat

instance Alpha MatchPat

instance NFData MatchPat

instance TypeAnnotated MatchPat where
      typeOf = \ case
            MatchPatCon _ t _ _  -> t
            MatchPatVar _ t      -> t
            MatchPatWildCard _ t -> t

instance Annotated MatchPat where
      ann = \ case
            MatchPatCon a _ _ _  -> a
            MatchPatVar a _      -> a
            MatchPatWildCard a _ -> a

instance Parenless MatchPat where
      parenless = \ case
            MatchPatCon _ _ (n2s -> c) _ | isTupleCtor c -> True
            MatchPatCon _ _ _ []                         -> True
            MatchPatVar {}                               -> True
            MatchPatWildCard {}                          -> True
            _                                            -> False

instance Pretty MatchPat where
      pretty = \ case
            MatchPatCon _ _ (n2s -> c) ps | isTupleCtor c -> parens $ hsep $ punctuate comma $ map pretty ps
            MatchPatCon _ _ n ps                          -> text (n2s n) <+> hsep (map mparens ps)
            MatchPatVar _ t                               -> parens $ text "*" <+> text "::" <+> pretty t
            MatchPatWildCard _ t                          -> parens $ text "_" <+> text "::" <+> pretty t

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
-- Descends through type annotations.
flattenApp :: Exp -> [Exp]
flattenApp = \ case
      App _ e e'    -> flattenApp e <> [e']
      TypeAnn _ _ e -> flattenApp e
      e             -> [e]

flattenArrow :: Ty -> ([Ty], Ty)
flattenArrow = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2 -> (t1 : ts, t)
            where (ts, t) = flattenArrow t2
      t                                               -> ([], t)

paramTys :: Ty -> [Ty]
paramTys = fst . flattenArrow

flattenTyApp :: Ty -> [Ty]
flattenTyApp = \ case
      TyApp _ t t' -> flattenTyApp t <> [t']
      t            -> [t]

flattenAllTyApp :: Ty -> [Ty]
flattenAllTyApp = \ case
      TyApp _ t t' -> flattenAllTyApp t <> flattenAllTyApp t'
      t            -> [t]

rangeTy :: Ty -> Ty
rangeTy = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) t' -> rangeTy t'
      t                                              -> t

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

bitTy :: Annote -> Ty
bitTy an = TyCon an $ s2n "Bit"

vecTy :: Annote -> Ty -> Ty -> Ty
vecTy an n t = TyApp an (TyApp an (TyCon an $ s2n "Vec") n) t

plusTy :: Annote -> Ty -> Ty -> Ty
plusTy an n t = TyApp an (TyApp an (TyCon an $ s2n "+") n) t

arr' :: Ty -> Bind (Name Exp) Exp -> Ty
arr' t b = runFreshM (arr t . typeOf <$> (snd <$> unbind b))

arrowRight :: Ty -> Ty
arrowRight = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) t' -> t'
      t                                              -> t

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
nil = Con (MsgAnnote "nil") nilTy (s2n "()")

nilPat :: Pat
nilPat = PatCon (MsgAnnote "nilPat") (Embed nilTy) (Embed $ s2n "()") []

nilMPat :: MatchPat
nilMPat = MatchPatCon (MsgAnnote "nilMPat") nilTy (s2n "()") []

pairTy :: Annote -> Ty -> Ty -> Ty
pairTy an t = TyApp an $ TyApp an (TyCon an $ s2n "(,)") t

mkPair :: Annote -> Exp -> Exp -> Exp
mkPair an e1 e2 = App an (App an (Con an t (s2n "(,)")) e1) e2
      where t = mkArrowTy [typeOf e1, typeOf e2] $ pairTy an (typeOf e1) $ typeOf e2

mkPairPat :: Annote -> Pat -> Pat -> Pat
mkPairPat an p1 p2 = PatCon an (Embed $ pairTy an (typeOf p1) (typeOf p2)) (Embed (s2n "(,)")) [p1, p2]

mkPairMPat :: Annote -> MatchPat -> MatchPat -> MatchPat
mkPairMPat an p1 p2 = MatchPatCon an (pairTy an (typeOf p1) (typeOf p2)) (s2n "(,)") [p1, p2]

mkTuple :: Annote -> [Exp] -> Exp
mkTuple an = foldr (mkPair an) nil

tupleTy :: Annote -> [Ty] -> Ty
tupleTy an = foldr (pairTy an) nilTy

mkTuplePat :: Annote -> [Pat] -> Pat
mkTuplePat an = foldr (mkPairPat an) nilPat

mkTupleMPat :: Annote -> [MatchPat] -> MatchPat
mkTupleMPat an = foldr (mkPairMPat an) nilMPat

isTupleCtor :: Text -> Bool
isTupleCtor c = c == "(" <> replicate (length (unpack c) - 2) "," <> ")"

isResMonad :: Ty -> Bool
isResMonad ty = case rangeTy ty of
      TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReT")) _) _) _) _ -> True
      _                                                                      -> False

resInputTy :: Ty -> Maybe Ty
resInputTy ty = case rangeTy ty of
      TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReT")) ip) _) _) _ -> Just ip
      _                                                                       -> Nothing

isStateMonad :: Ty -> Bool
isStateMonad ty = case rangeTy ty of
      TyApp an (TyApp _ (TyApp _ (TyCon _ (n2s -> "StT")) _) m) a -> isStateMonad (tycomp an m a)
      TyApp _ (TyCon _ (n2s -> "I")) _                            -> True
      _                                                           -> False

isBlank :: Ty -> Bool
isBlank = \ case
      TyBlank _ -> True
      _         -> False

-- | Types containing no type variables (or blanks).
concrete :: Ty -> Bool
concrete = \ case
      TyBlank {}  -> False
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
      TyBlank {}                 -> True
      TyVar {}                   -> True
      TyApp _ a b                -> fundamental a && fundamental b

higherOrder :: Ty -> Bool
higherOrder (flattenArrow -> (ats, rt)) = any isArrow $ rt : ats

isArrow :: Ty -> Bool
isArrow = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) _) _ -> True
      _                                             -> False

-- Orphans.

instance (Eq a, Eq b) => Eq (Bind a b) where
      (B a b) == (B a' b') = a == a' && b == b'

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

instance Alpha Natural where
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

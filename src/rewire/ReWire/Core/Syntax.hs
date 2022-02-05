{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DerivingVia, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReWire.Core.Syntax
  ( Sig (..)
  , Exp (..)
  , Pat (..)
  , StartDefn (..), Defn (..)
  , Wiring (..)
  , Program (..)
  , Target (..)
  , Size, Index, Name, Value, GId, LId
  , SizeAnnotated (..)
  , bvTrue, bvFalse
  ) where

import ReWire.Pretty
import ReWire.Annotation

import Data.Data (Typeable, Data(..))
import Data.List (intersperse, genericLength)
import Data.Text (Text, pack)
import Prettyprinter (Pretty (..), Doc, vsep, (<+>), nest, hsep, parens, braces, punctuate, comma, dquotes)
import GHC.Generics (Generic)
import TextShow (TextShow (..), showt)
import TextShow.Generic (FromGeneric (..))
import Data.BitVector (BV (..), width, showHex, zeros, ones)

class SizeAnnotated a where
      sizeOf :: a -> Size

type Value = Integer
type Size  = Word
type Index = Int
type LId   = Word
type GId   = Text
type Name  = Text

bvTrue :: BV
bvTrue = ones 1

bvFalse :: BV
bvFalse = zeros 1

instance TextShow BV where
      showb = showb . showHex

data Target = Global !GId
            | Extern !Sig !Name
            | Id
            | Const !BV
      deriving (Eq, Ord, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Target

instance Pretty Target where
      pretty = \ case
            Global n   -> text n
            Extern _ n -> text "extern" <+> dquotes (text n)
            Id         -> text "id"
            Const bv   -> ppBV [Lit noAnn bv]

ppBV :: Pretty a => [a] -> Doc an
ppBV = ppBV' . map pretty

ppBV' :: [Doc an] -> Doc an
ppBV' = parens . hsep . punctuate comma

---

data Sig = Sig Annote ![Size] !Size -- Function ty with sizes of arguments and size of result.
        deriving (Eq, Ord, Generic, Show, Typeable, Data)
        deriving TextShow via FromGeneric Sig

instance Annotated Sig where
      ann (Sig a _ _) = a

instance SizeAnnotated Sig where
      sizeOf (Sig _ _ s) = s

instance Pretty Sig where
      pretty = \ case
            Sig _ [] res   -> text "BV" <> pretty res
            Sig _ args res -> parens (hsep $ punctuate comma $ map ((text "BV" <>) . pretty) args) <+> text "->" <+> text "BV" <> pretty res

---

data Exp = Lit  Annote !BV
         | LVar Annote !Size !LId
         | Call Annote !Size !Target ![Exp] ![Pat] ![Exp]
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Exp

instance SizeAnnotated Exp where
      sizeOf = \ case
            LVar _ s _       -> s
            Lit _ bv         -> fromIntegral $ width bv
            Call _ s _ _ _ _ -> s

instance Annotated Exp where
      ann = \ case
            LVar a _ _       -> a
            Lit a _          -> a
            Call a _ _ _ _ _ -> a

instance Pretty Exp where
      pretty = \ case
            Lit _ bv             -> text (pack $ showHex bv) <> text "::BV" <> pretty (width bv)
            LVar _ _ n           -> text $ "$" <> showt n
            Call _ _ f es ps [] -> nest 2 $ vsep
                  [ text "case" <+> ppBV es <+> text "of"
                  , ppBV' (ppPats ps) <+> text "->" <+> pretty f <+> ppBV' (ppArgs ps)
                  ]
            Call _ _ f es ps es2 -> nest 2 $ vsep
                  [ text "case" <+> ppBV es <+> text "of"
                  , ppBV' (ppPats ps) <+> text "->" <+> pretty f <+> ppBV' (ppArgs ps)
                  , text "_" <+> text "->" <+> ppBV es2
                  ]

ppPats :: [Pat] -> [Doc an]
ppPats = zipWith ppPats' [0::Index ..]
      where ppPats' :: Index -> Pat -> Doc an
            ppPats' i = \ case
                  PatVar _ sz      -> text "p" <> pretty i <> text "::" <> text "BV" <> pretty sz
                  PatWildCard _ sz -> text "_" <> text "::" <> text "BV" <> pretty sz
                  PatLit _ bv      -> text (pack $ showHex bv) <> text "::" <> text "BV" <> pretty (width bv)

ppArgs :: [Pat] -> [Doc an]
ppArgs = map (uncurry ppArgs') . filter (isPatVar . snd) . zip [0::Index ..]
      where ppArgs' :: Index -> Pat -> Doc an
            ppArgs' i = \ case
                  PatVar _ _ -> text "p" <> pretty i
                  _          -> mempty

            isPatVar :: Pat -> Bool
            isPatVar = \ case
                  PatVar _ _ -> True
                  _          -> False

---

data Pat = PatVar      Annote !Size
         | PatWildCard Annote !Size
         | PatLit      Annote !BV
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Pat

instance SizeAnnotated Pat where
      sizeOf = \ case
            PatVar      _ s  -> s
            PatWildCard _ s  -> s
            PatLit      _ bv -> fromIntegral $ width bv

instance Annotated Pat where
      ann = \ case
            PatVar      a _ -> a
            PatWildCard a _ -> a
            PatLit      a _ -> a

instance Pretty Pat where
      pretty = \ case
            PatVar _ s       -> braces $ text "BV" <> pretty s
            PatWildCard _ s  -> text "_" <> text "BV" <> pretty s <> text "_"
            PatLit      _ bv -> text (pack $ showHex bv) <> text "::BV" <> pretty (width bv)

---

-- | Names for input, output, state signals, res type, (loop, loop ty), (state0, state0 ty).
data StartDefn = StartDefn Annote !Wiring !GId !GId
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric StartDefn

instance Annotated StartDefn where
      ann (StartDefn a _ _ _) = a

instance Pretty StartDefn where
      pretty (StartDefn _ w loop state0) = vsep
            [ text "Main.start" <+> text "::" <+> text "ReT" <+> ppBV (map snd $ inputWires w) <+> ppBV (map snd $ outputWires w)
            , text "Main.start" <+> text "=" <+> nest 2 (text "unfold" <+> pretty loop <+> pretty state0)
            ]

---

data Wiring = Wiring
      { inputWires  :: ![(Name, Size)]
      , outputWires :: ![(Name, Size)]
      , stateWires  :: ![(Name, Size)]
      , sigLoop     :: !Sig
      , sigState0   :: !Sig
      }
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Wiring

---

data Defn = Defn
      { defnAnnote :: Annote
      , defnName   :: !GId
      , defnSig    :: !Sig -- params given by the arity.
      , defnBody   :: ![Exp]
      }
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Defn

instance SizeAnnotated Defn where
      sizeOf (Defn _ _ (Sig _ _ s) _) = s

instance Annotated Defn where
      ann (Defn a _ _ _) = a

instance Pretty Defn where
      pretty (Defn _ n sig es) = vsep
            [ text n <+> text "::" <+> pretty sig
            , text n <+> hsep (map (text . ("$" <>) . showt) [0 .. arity sig - 1]) <+> text "=" <+> nest 2 (ppBV es)
            ]
            where arity :: Sig -> Int
                  arity (Sig _ args _) = genericLength args

---

data Program = Program
      { start :: !StartDefn
      , defns :: ![Defn]
      }
      deriving (Generic, Eq, Ord, Show, Typeable, Data)
      deriving TextShow via FromGeneric Program

instance Pretty Program where
      pretty p = vsep $ intersperse (text "") $ pretty (start p) : map pretty (defns p)


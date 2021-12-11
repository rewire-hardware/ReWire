{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DerivingVia, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Syntax
  ( Sig (..)
  , Exp (..)
  , Pat (..)
  , StartDefn (..), Defn (..)
  , Program (..)
  , GId, LId
  , SizeAnnotated (..)
  ) where

import ReWire.Pretty
import ReWire.Annotation

import Data.Data (Typeable, Data(..))
import Data.List (intersperse)
import Data.Text (Text)
import Prettyprinter (Pretty (..), Doc, vsep, (<+>), nest, hsep, parens, braces, brackets, dquotes, punctuate, comma)
import GHC.Generics (Generic)

import TextShow (TextShow (..), showt)
import TextShow.Generic (FromGeneric (..))

class SizeAnnotated a where
      sizeOf :: a -> Size

type Size = Int
type GId  = Text
type LId  = Int

pBV :: Pretty a => [a] -> Doc an
pBV = brackets . hsep . punctuate comma . map pretty

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
            Sig _ [] res   -> pretty res
            Sig _ args res -> pBV args <+> text "->" <+> pretty res

---

data Exp = Lit                 Annote !Size !Int
         | LVar                Annote !Size !LId
         | Match               Annote !Size !GId ![Exp] ![Pat] !(Maybe [Exp])
         | NativeVHDL          Annote !Size !Text              ![Exp]
         | NativeVHDLComponent Annote !Size !Text              ![Exp]
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Exp

instance SizeAnnotated Exp where
      sizeOf = \ case
            LVar _ s _                  -> s
            Lit _ s _                   -> s
            Match _ s _ _ _ _           -> s
            NativeVHDL _ s _ _          -> s
            NativeVHDLComponent _ s _ _ -> s

instance Annotated Exp where
      ann = \ case
            LVar a _ _                  -> a
            Lit a _ _                   -> a
            Match a _ _ _ _ _           -> a
            NativeVHDL a _ _ _          -> a
            NativeVHDLComponent a _ _ _ -> a

instance Pretty Exp where
      pretty = \ case
            Lit _ w v                       -> text "LIT_" <> pretty v <> text "_" <> pretty w
            LVar _ _ n                      -> text $ "$" <> showt n
            Match _ _ f es ps Nothing        -> nest 2 $ vsep
                  [ text "match" <+> pBV es <+> text "of"
                  , pBV ps <+> text "->" <+> text f
                  ]
            Match _ _ f es ps (Just es2)      -> nest 2 $ vsep
                  [ text "match" <+> pBV es <+> text "of"
                  , pBV ps <+> text "->" <+> text f
                  , text "_" <+> text "->" <+> pBV es2
                  ]
            NativeVHDL _ s n []            -> parens (text "nativeVHDL" <+> dquotes (text n)) <> braces (pretty s)
            NativeVHDL _ s n args          -> parens (text "nativeVHDL" <+> dquotes (text n)) <> braces (pretty s) <> pBV args
            NativeVHDLComponent _ s n []   -> parens (text "nativeVHDLComponent" <+> dquotes (text n)) <> braces (pretty s)
            NativeVHDLComponent _ s n args -> parens (text "nativeVHDLComponent" <+> dquotes (text n)) <> braces (pretty s) <> pBV args

---

data Pat = PatVar      Annote !Size
         | PatWildCard Annote !Size
         | PatLit      Annote !Size !Int
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Pat

instance SizeAnnotated Pat where
      sizeOf = \ case
            PatVar      _ s       -> s
            PatWildCard _ s       -> s
            PatLit      _ s _     -> s

instance Annotated Pat where
      ann = \ case
            PatVar      a _       -> a
            PatWildCard a _       -> a
            PatLit      a _ _     -> a

instance Pretty Pat where
      pretty = \ case
            PatVar _ s        -> braces $ pretty s
            PatWildCard _ s   -> text "_" <> pretty s
            PatLit      _ s v -> text "LIT_" <> pretty v <> text "_" <> pretty s

---

data StartDefn = StartDefn Annote !Size !Size !Size !(GId, Sig) !(GId, Sig) -- input, output, res type, (loop, loop ty), (state0, state0 ty)
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric StartDefn

instance Annotated StartDefn where
      ann (StartDefn a _ _ _ _ _) = a

instance Pretty StartDefn where
      pretty (StartDefn _ tin tout tres (loop, _) (state0, _)) = vsep $
            [ text "Main.start" <+> text "::" <+> text "ReT" <+> pretty tin <+> pretty tout <+> text "I" <+> pretty tres
            , text "Main.start" <+> text "=" <+> nest 2 (text "unfold" <+> pretty loop <+> pretty state0)
            ]

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
      pretty (Defn _ n sig es) = vsep $
            [ text n <+> text "::" <+> pretty sig
            , text n <+> hsep (map (text . ("$" <>) . showt) [0 .. arity sig - 1]) <+> text "=" <+> nest 2 (pBV es)
            ]
            where arity :: Sig -> Int
                  arity (Sig _ args _) = length args

---

data Program = Program
      { start :: !StartDefn
      , defns :: ![Defn]
      }
      deriving (Generic, Eq, Ord, Show, Typeable, Data)
      deriving TextShow via FromGeneric Program

instance Pretty Program where
      pretty p = vsep $ intersperse (text "") $ pretty (start p) : map pretty (defns p)


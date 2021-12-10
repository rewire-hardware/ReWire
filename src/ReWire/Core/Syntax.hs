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
            Sig _ args res -> (brackets $ hsep $ punctuate comma $ map pretty args) <+> text "->" <+> pretty res

---

data Exp = Call                Annote !Sig  !GId           ![Exp] -- TODO(chathhorn): Sig instead of Size just because of case in ToCore.hs.
         | Con                 Annote !Size !Int !Size     ![Exp] -- Ints: type size, tag value, tag width (ceilLog2 of nctors)
         | LVar                Annote !Size !LId
         | Match               Annote !Size !GId !Exp !Pat !(Maybe Exp)
         | NativeVHDL          Annote !Size !Text          ![Exp]
         | NativeVHDLComponent Annote !Size !Text          ![Exp]
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Exp

-- Note:
-- Call an sz gid args = Match an sz (mkTuple an args) (mkTuplePat an $ map toPatVar args) gid Nothing

instance SizeAnnotated Exp where
      sizeOf = \ case
            Call _ (Sig _ _ s) _ _      -> s
            LVar _ s _                  -> s
            Con a s _ _ _               -> s
            Match _ s _ _ _ _           -> s
            NativeVHDL _ s _ _          -> s
            NativeVHDLComponent _ s _ _ -> s

instance Annotated Exp where
      ann = \ case
            Call a _ _ _                -> a
            LVar a _ _                  -> a
            Con a _ _ _ _               -> a
            Match a _ _ _ _ _           -> a
            NativeVHDL a _ _ _          -> a
            NativeVHDLComponent a _ _ _ -> a

instance Pretty Exp where
      pretty = \ case
            Call _ _ n args                                   -> pretty n <+> brackets (hsep $ punctuate comma $ map pretty args)

            Con _ _ 0 0 args                                  -> brackets $ hsep $ punctuate comma $ map pretty args
            Con _ _ v w args                                  -> brackets $ hsep $ punctuate comma $ (text "TAG_" <> pretty v <> text "_" <> pretty w) : map pretty args

            LVar _ _ n                                        -> text $ "$" <> showt n
            Match _ _ f e p Nothing                          -> nest 2 $ vsep
                  [ text "match" <+> pretty e <+> text "of"
                  , pretty p <+> text "->" <+> text f
                  ]
            Match _ _ f e p (Just e2)                        -> nest 2 $ vsep
                  [ text "match" <+> pretty e <+> text "of"
                  , pretty p <+> text "->" <+> text f
                  , text "_" <+> text "->" <+> pretty e2
                  ]
            NativeVHDL _ s n []                               -> parens (text "nativeVHDL" <+> dquotes (text n)) <> braces (pretty s)
            NativeVHDL _ s n args                             -> parens (text "nativeVHDL" <+> dquotes (text n)) <> braces (pretty s) <> brackets (hsep $ punctuate comma $ map pretty args)
            NativeVHDLComponent _ s n []                      -> parens (text "nativeVHDLComponent" <+> dquotes (text n)) <> braces (pretty s)
            NativeVHDLComponent _ s n args                    -> parens (text "nativeVHDLComponent" <+> dquotes (text n)) <> braces (pretty s) <> brackets (hsep $ punctuate comma $ map pretty args)

---

data Pat = PatCon Annote !Size !Int !Size ![Pat]
         | PatVar Annote !Size
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Pat

instance SizeAnnotated Pat where
      sizeOf = \ case
            PatCon _ s _ _ _ -> s
            PatVar _ s       -> s

instance Annotated Pat where
      ann = \ case
            PatCon a _ _ _ _ -> a
            PatVar a _       -> a

instance Pretty Pat where
      pretty = \ case
            PatCon _ _ 0 0 ps -> brackets $ hsep $ punctuate comma $ map pretty ps
            PatCon _ _ v w ps -> brackets $ hsep $ punctuate comma $ (text "TAG_" <> pretty v <> text "_" <> pretty w) : map pretty ps
            PatVar _ s        -> braces $ pretty s

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

data Defn = Defn { defnAnnote :: Annote,
                   defnName   :: !GId,
                   defnSig    :: !Sig, -- params given by the arity.
                   defnBody   :: !Exp }
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Defn

instance SizeAnnotated Defn where
      sizeOf (Defn _ _ (Sig _ _ s) _) = s

instance Annotated Defn where
      ann (Defn a _ _ _) = a

instance Pretty Defn where
      pretty (Defn _ n sig e) = vsep $
            [ text n <+> text "::" <+> pretty sig
            , text n <+> hsep (map (text . ("$" <>) . showt) [0 .. arity sig - 1]) <+> text "=" <+> nest 2 (pretty e)
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


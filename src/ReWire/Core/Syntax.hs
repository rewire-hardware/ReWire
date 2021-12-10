{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DerivingVia, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Syntax
  ( Ty (..)
  , Exp (..)
  , Pat (..)
  , StartDefn (..), Defn (..)
  , Program (..)
  , sizeOf
  , GId, LId
  , TypeAnnotated (..)
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

class TypeAnnotated a where
      typeOf :: a -> Ty

type Size = Int
type GId  = Text
type LId  = Int

---

data Ty = Ty Annote ![Size] !Size -- Function ty with sizes of arguments and size of result.
        deriving (Eq, Ord, Generic, Show, Typeable, Data)
        deriving TextShow via FromGeneric Ty

instance Annotated Ty where
      ann (Ty a _ _) = a

instance Pretty Ty where
      pretty = \ case
            Ty _ [] res   -> pretty res
            Ty _ args res -> (brackets $ hsep $ punctuate comma $ map pretty args) <+> text "->" <+> pretty res

-- | For arrow types, returns the size of the result.
sizeOf :: Ty -> Size
sizeOf (Ty _ _ s) = s

---

data Exp = Call       Annote !Ty   !GId            ![Exp]
         | Con        Annote !Size !Int !Size     ![Exp] -- Ints: type size, tag value, tag width (ceilLog2 of nctors)
         | LVar       Annote !Ty   !LId
         | Match      Annote !Ty   !Exp !Pat !GId !(Maybe Exp)
         | NativeVHDL Annote !Ty   !Text           ![Exp]
         | NativeVHDLComponent Annote !Ty  !Text ![Exp]
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Exp

-- TODO(chathhorn):
-- Call an sz gid args = Match an sz (Tuple args) (TuplePat $ map toVar args) gid [] Nothing
-- Match an sz e p gid lids e' = Match an sz (Tuple $ (map lidToVar lids) <> [e]) (TuplePat $ map toVar lids <> [p]) gid [] e'

instance TypeAnnotated Exp where
      typeOf = \ case
            Call _ t _ _       -> t
            LVar _ t _         -> t
            Con a s _ _ _      -> Ty a [] s
            Match _ t _ _ _ _  -> t
            NativeVHDL _ t _ _ -> t

instance Annotated Exp where
      ann = \ case
            Call a _ _ _       -> a
            LVar a _ _         -> a
            Con a _ _ _ _      -> a
            Match a _ _ _ _ _  -> a
            NativeVHDL a _ _ _ -> a

instance Pretty Exp where
      pretty = \ case
            Call _ (Ty _ [] _) n args                         -> pretty n <+> brackets (hsep $ punctuate comma $ map pretty args)
            Call _ _ n []                                     -> pretty n
            Call _ _ n args                                   -> pretty n <+> brackets (hsep $ punctuate comma $ map pretty args <> [text "..."])

            Con _ _ 0 0 args                                  -> brackets $ hsep $ punctuate comma $ map pretty args
            Con _ _ v w args                                  -> brackets $ hsep $ punctuate comma $ (text "TAG_" <> pretty v <> text "_" <> pretty w) : map pretty args

            LVar _ _ n                                        -> text $ "$" <> showt n
            Match _ _ e p e1 Nothing                          -> nest 2 $ vsep
                  [ text "match" <+> pretty e <+> text "of"
                  , pretty p <+> text "->" <+> text e1
                  ]
            Match _ _ e p e1 (Just e2)                        -> nest 2 $ vsep
                  [ text "match" <+> pretty e <+> text "of"
                  , pretty p <+> text "->" <+> text e1
                  , text "_" <+> text "->" <+> pretty e2
                  ]
            NativeVHDL _ t n []                               -> parens (text "nativeVHDL" <+> dquotes (text n)) <> braces (pretty t)
            NativeVHDL _ t n args                             -> parens (text "nativeVHDL" <+> dquotes (text n)) <> braces (pretty t) <> brackets (hsep $ punctuate comma $ map pretty args)

---

data Pat = PatCon Annote !Size !Int !Size ![Pat]
         | PatVar Annote !Ty
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Pat

instance TypeAnnotated Pat where
      typeOf = \ case
            PatCon a s _ _ _ -> Ty a [] s
            PatVar _ t       -> t

instance Annotated Pat where
      ann = \ case
            PatCon a _ _ _ _ -> a
            PatVar a _       -> a

instance Pretty Pat where
      pretty = \ case
            PatCon _ _ 0 0 ps -> brackets $ hsep $ punctuate comma $ map pretty ps
            PatCon _ _ v w ps -> brackets $ hsep $ punctuate comma $ (text "TAG_" <> pretty v <> text "_" <> pretty w) : map pretty ps
            PatVar _ t        -> braces $ pretty t

---

data StartDefn = StartDefn Annote !Size !Size !Size !(GId, Ty) !(GId, Ty) -- input, output, res type, (loop, loop ty), (state0, state0 ty)
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
                   defnTy     :: !Ty, -- params given by the arity.
                   defnBody   :: !Exp }
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Defn

instance TypeAnnotated Defn where
      typeOf (Defn _ _ t _) = t

instance Annotated Defn where
      ann (Defn a _ _ _) = a

instance Pretty Defn where
      pretty (Defn _ n ty e) = vsep $
            [ text n <+> text "::" <+> pretty ty
            , text n <+> hsep (map (text . ("$" <>) . showt) [0 .. arity ty - 1]) <+> text "=" <+> nest 2 (pretty e)
            ]
            where arity :: Ty -> Int
                  arity (Ty _ args _) = length args

---

data Program = Program
      { start :: !StartDefn
      , defns :: ![Defn]
      }
      deriving (Generic, Eq, Ord, Show, Typeable, Data)
      deriving TextShow via FromGeneric Program

instance Pretty Program where
      pretty p = vsep $ intersperse (text "") $ pretty (start p) : map pretty (defns p)


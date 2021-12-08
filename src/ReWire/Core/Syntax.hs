{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DerivingVia, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Syntax
  ( DataConId (..)
  , Ty (..)
  , Exp (..)
  , Pat (..)
  , StartDefn (..), Defn (..)
  , DataCon (..)
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

newtype DataConId = DataConId { deDataConId :: Text } deriving (Eq, Ord, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric DataConId

type GId  = Text
type LId  = Int

instance Pretty DataConId where
      pretty = pretty . deDataConId

---

data Ty = Ty Annote ![Int] !Int -- Function ty with sizes of arguments and size of result.
        deriving (Eq, Ord, Generic, Show, Typeable, Data)
        deriving TextShow via FromGeneric Ty

instance Annotated Ty where
      ann (Ty a _ _) = a

instance Pretty Ty where
      pretty = \ case
            Ty _ [] res   -> pretty res
            Ty _ args res -> (brackets $ hsep $ punctuate comma $ map pretty args) <+> text "->" <+> pretty res

-- For arrow types, returns the size of the result.
sizeOf :: Ty -> Int
sizeOf (Ty _ _ s) = s

---

data Exp = Call       Annote !Ty !GId            ![Exp]
         | Con        Annote !Ty !Int !DataConId ![Exp]
         | LVar       Annote !Ty !LId
         | Match      Annote !Ty !Exp !Pat !GId ![LId] !(Maybe Exp)
         | NativeVHDL Annote !Ty !Text           ![Exp]
         | NativeVHDLComponent Annote !Ty  !Text ![Exp]
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Exp

instance TypeAnnotated Exp where
      typeOf = \ case
            Call _ t _ _        -> t
            LVar _ t _          -> t
            Con _ t _ _ _       -> t
            Match _ t _ _ _ _ _ -> t
            NativeVHDL _ t _ _  -> t

instance Annotated Exp where
      ann = \ case
            Call a _ _ _        -> a
            LVar a _ _          -> a
            Con a _ _ _ _       -> a
            Match a _ _ _ _ _ _ -> a
            NativeVHDL a _ _ _  -> a

instance Pretty Exp where
      pretty = \ case
            Call _ t n []                                     -> pretty n <> braces (pretty t)
            Call _ t n args                                   -> pretty n <> braces (pretty t) <> brackets (hsep $ punctuate comma $ map pretty args)
            Con _ t _ (DataConId n) []                        -> pretty n <> braces (pretty t)
            Con _ t _ (DataConId n) args                      -> pretty n <> braces (pretty t) <> brackets (hsep $ punctuate comma $ map pretty args)
            LVar _ _ n                                        -> text $ "$" <> showt n
            Match _ t e p e1 as Nothing                       -> nest 2 $ vsep
                  [ text "match" <+> braces (pretty t) <+> pretty e <+> text "of"
                  , pretty p <+> text "->" <+> text e1 <+> hsep (map (pretty . LVar noAnn (Ty noAnn [] (-1))) as)
                  ]
            Match _ t e p e1 as (Just e2)                     -> nest 2 $ vsep
                  [ text "match"  <+> braces (pretty t) <+> pretty e <+> text "of"
                  , pretty p <+> text "->" <+> text e1 <+> hsep (map (pretty . LVar noAnn (Ty noAnn [] (-1))) as)
                  , text "_" <+> text "->" <+> pretty e2
                  ]
            NativeVHDL _ t n []                               -> parens (text "nativeVHDL" <+> dquotes (text n)) <> braces (pretty t)
            NativeVHDL _ t n args                             -> parens (text "nativeVHDL" <+> dquotes (text n)) <> braces (pretty t) <> brackets (hsep $ punctuate comma $ map pretty args)

---

data Pat = PatCon Annote !Ty !DataConId ![Pat]
         | PatVar Annote !Ty
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Pat

instance TypeAnnotated Pat where
      typeOf = \ case
            PatCon _ t _ _ -> t
            PatVar _ t     -> t

instance Annotated Pat where
      ann = \ case
            PatCon a _ _ _ -> a
            PatVar a _     -> a

mparen :: (Pretty a, Parenless a) => a -> Doc ann
mparen a = if parenless a then pretty a else parens $ pretty a
      where parenless :: Pat -> Bool
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

data StartDefn = StartDefn Annote !Ty !Ty !Ty !Exp -- input, output, res type, body
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric StartDefn

instance Annotated StartDefn where
      ann (StartDefn a _ _ _ _) = a

instance Pretty StartDefn where
      pretty (StartDefn _ tin tout tres e) = vsep $
            [ text "Main.start" <+> text "::" <+> text "ReT" <+> pretty tin <+> pretty tout <+> text "I" <+> pretty tres
            , text "Main.start" <+> text "=" <+> nest 2 (pretty e)
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

-- | annotation, id, ctor index (in the range [0, nctors)), nctors, type
data DataCon = DataCon Annote !DataConId !Int !Int !Ty
      deriving (Generic, Eq, Ord, Show, Typeable, Data)
      deriving TextShow via FromGeneric DataCon

instance TypeAnnotated DataCon where
      typeOf (DataCon _ _ _ _ t) = t

instance Annotated DataCon where
      ann (DataCon a _ _ _ _) = a

instance Pretty DataCon where
      pretty (DataCon _ n _ _ t) = text (deDataConId n) <+> text "::" <+> pretty t

---

data Program = Program
      { ctors :: ![DataCon]
      , start :: !StartDefn
      , defns :: ![Defn]
      }
      deriving (Generic, Eq, Ord, Show, Typeable, Data)
      deriving TextShow via FromGeneric Program

instance Pretty Program where
      pretty p = vsep $ intersperse (text "") $ map pretty (ctors p) <> [pretty (start p)] <> map pretty (defns p)


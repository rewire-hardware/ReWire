{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Syntax
  ( Sig (..), ExternSig (..)
  , Exp (..)
  , Pat (..)
  , Prim (..)
  , Defn (..)
  , Wiring (..)
  , Program (..)
  , Target (..)
  , Size, Index, Name, Value, GId, LId
  , SizeAnnotated (..)
  , bvTrue, bvFalse
  , isNil, nil
  , isNilPat, nilPat
  , cat, gather
  ) where

import ReWire.Annotation (Annote, Annotated (ann), noAnn)
import ReWire.BitVector (BV (..), width, showHex', zeros, ones, (==.))
import ReWire.Orphans ()
import ReWire.Pretty (text, Pretty (pretty), Doc, vsep, (<+>), nest, hsep, parens, punctuate, comma, squote, dquotes, braced, TextShow (showt), FromGeneric (..), colon)
import qualified ReWire.BitVector as BV

import Data.Data (Typeable, Data(..))
import Data.Hashable (Hashable)
import Data.List (intersperse, genericLength)
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

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

data Prim = Add | Sub
          | Mul | Div | Mod
          | Pow
          | LAnd | LOr
          | And | Or
          | XOr | XNor
          | LShift | RShift
          | RShiftArith
          | Eq | Gt | GtEq | Lt | LtEq
          | Replicate Natural
          | LNot | Not
          | RAnd | RNAnd
          | ROr | RNor | RXOr | RXNor
          | MSBit
          | Resize | Reverse
          | Id
      deriving (Eq, Ord, Generic, Show, Typeable, Data, Read)
      deriving TextShow via FromGeneric Prim

instance Hashable Prim

data Target = Global !GId
            | Extern !ExternSig !Name !Name
            | Prim !Prim
            | Const !BV
            | SetRef !Name
            | GetRef !Name
      deriving (Eq, Ord, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Target

instance Hashable Target

instance Pretty Target where
      pretty = \ case
            Global n     -> text n
            Extern _ n _ -> text "extern" <+> dquotes (text n)
            Const bv     -> ppBV [Lit noAnn bv]
            SetRef n     -> text "setRef" <+> dquotes (text n)
            GetRef n     -> text "getRef" <+> dquotes (text n)
            Prim p       -> text $ showt p

ppBVTy :: Integral n => n -> Doc an
ppBVTy n = text "W" <> pretty (fromIntegral n :: Int)

ppBV :: Pretty a => [a] -> Doc an
ppBV = \ case
      []  -> mempty
      [v] -> pretty v
      vs  -> braced $ pretty <$> vs
---

data ExternSig = ExternSig Annote ![(Text, Size)] !Text ![(Text, Size)] ![(Text, Size)]
        -- ^ Names and sizes of params, clock signal, inputs, and outputs, respectively.
        deriving (Eq, Ord, Generic, Show, Typeable, Data)
        deriving TextShow via FromGeneric ExternSig

instance Hashable ExternSig

instance Annotated ExternSig where
      ann (ExternSig a _ _ _ _) = a

instance SizeAnnotated ExternSig where
      sizeOf (ExternSig _ _ _ _ rs) = sum (snd <$> rs)

instance Pretty ExternSig where
      pretty (ExternSig _ _ _ args res) = hsep $ punctuate (text " ->") $ map (ppBVTy . snd) args <> [parens $ hsep $ punctuate comma $ map (ppBVTy . snd) res]

---

data Sig = Sig Annote ![Size] !Size
        deriving (Eq, Ord, Generic, Show, Typeable, Data)
        deriving TextShow via FromGeneric Sig

instance Hashable Sig

instance Annotated Sig where
      ann (Sig a _ _) = a

instance SizeAnnotated Sig where
      sizeOf (Sig _ _ s) = s

instance Pretty Sig where
      pretty (Sig _ args res) = hsep $ punctuate (text " ->") $ map ppBVTy $ args <> [res]

---

data Exp = Lit    Annote !BV
         | LVar   Annote !Size !LId
         | Concat Annote !Exp  !Exp
         | Call   Annote !Size !Target !Exp ![Pat] !Exp
         deriving (Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Exp

instance Hashable Exp

instance Eq Exp where
      (Lit    a bv)           == (Lit    a' bv')               = a == a' && bv ==. bv' -- Eq instance just for "==." instead of "==" here.
      (LVar   a sz lid)       == (LVar   a' sz' lid')          = a == a' && sz == sz' && lid == lid'
      (Concat a e1 e2)        == (Concat a' e1' e2')           = a == a' && e1 == e1' && e2 == e2'
      (Call   a sz t e1 p e2) == (Call   a' sz' t' e1' p' e2') = a == a' && sz == sz' && t == t' && e1 == e1' && p == p' && e2 == e2'
      _                       == _                             = False

instance SizeAnnotated Exp where
      sizeOf = \ case
            LVar _ s _       -> s
            Lit _ bv         -> fromIntegral $ width bv
            Concat _ e1 e2   -> sizeOf e1 + sizeOf e2
            Call _ s _ _ _ _ -> s

instance Annotated Exp where
      ann = \ case
            LVar a _ _       -> a
            Lit a _          -> a
            Concat a _ _     -> a
            Call a _ _ _ _ _ -> a

instance Pretty Exp where
      pretty = \ case
            Lit _ bv             -> pretty (width bv) <> squote <> text (showHex' bv)
            LVar _ sz n          -> pretty sz <> squote <> text ("$" <> showt n)
            Concat _ e1 e2       -> ppBV $ gather e1 <> gather e2
            Call _ sz f e ps els | isNil els -> nest 2 $ vsep
                  [ pretty sz <> squote <> text "case" <+> pretty e <+> text "of"
                  , braced (pretty <$> ps) <+> text "->" <+> pretty f
                  ]
            Call _ sz f e ps els -> nest 2 $ vsep
                  [ pretty sz <> squote <> text "case" <+> pretty e <+> text "of"
                  , braced (pretty <$> ps) <+> text "->" <+> pretty f
                  , text "_" <+> text "->" <+> pretty els
                  ]

cat :: [Exp] -> Exp
cat = (\ case
            []         -> nil
            es@(e : _) -> foldl1 (Concat $ ann e) es
      ) . filter (not . isNil)

gather :: Exp -> [Exp]
gather = filter (not . isNil) . \ case
      Concat _ e1 e2 -> gather e1 <> gather e2
      e              -> [e]

nil :: Exp
nil = Lit noAnn BV.nil

isNil :: Exp -> Bool
isNil e = sizeOf e <= 0

---

data Pat = PatVar      Annote !Size
         | PatWildCard Annote !Size
         | PatLit      Annote !BV
         deriving (Eq, Ord, Show, Typeable, Data, Generic)
         deriving TextShow via FromGeneric Pat

instance Hashable Pat

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
            PatVar _ sz       -> pretty sz <> squote <> text "@"
            PatWildCard _ sz  -> pretty sz <> squote <> text "_"
            PatLit      _ bv  -> pretty (width bv) <> squote <> text (showHex' bv)

nilPat :: Pat
nilPat = PatLit noAnn BV.nil

isNilPat :: Pat -> Bool
isNilPat p = sizeOf p == 0

---

data Wiring = Wiring
      { inputWires  :: ![(Name, Size)]
      , outputWires :: ![(Name, Size)]
      , stateWires  :: ![(Name, Size)]
      }
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Wiring

instance Hashable Wiring

instance Pretty Wiring where
      pretty w = vsep
            [ text "inputs" <+> ppWires (inputWires w)
            , text "outputs" <+> ppWires (outputWires w)
            , text "states" <+> ppWires (stateWires w)
            ]
            where ppWire :: (Name, Size) -> Doc a
                  ppWire (n, s) = text n <> colon <+> pretty s

                  ppWires :: [(Name, Size)] -> Doc a
                  ppWires = braced . (ppWire <$>)

---

data Defn = Defn
      { defnAnnote :: Annote
      , defnName   :: !GId
      , defnSig    :: !Sig -- params given by the arity.
      , defnBody   :: !Exp
      }
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Defn

instance Hashable Defn

instance SizeAnnotated Defn where
      sizeOf (Defn _ _ (Sig _ _ s) _) = s

instance Annotated Defn where
      ann (Defn a _ _ _) = a

instance Pretty Defn where
      pretty (Defn _ n sig e) = vsep
            [ text n <+> text "::" <+> pretty sig
            , text n <+> hsep (map (text . ("$" <>) . showt) [0 .. arity sig - 1]) <+> text "=" <+> nest 2 (pretty e)
            ]
            where arity :: Sig -> Int
                  arity (Sig _ args _) = genericLength args

---

data Program = Program
      { topLevel :: !Name
      , wiring   :: !Wiring
      , loop     :: !Defn
      , state0   :: !Defn
      , defns    :: ![Defn]
      }
      deriving (Generic, Eq, Ord, Show, Typeable, Data)
      deriving TextShow via FromGeneric Program

instance Hashable Program

instance Pretty Program where
      pretty (Program n w loop state0 defns) = vsep $ intersperse (text "") $
            [ text "device" <+> text n <> colon
            , pretty w
            , pretty loop
            , pretty state0
            ] <> map pretty defns

---


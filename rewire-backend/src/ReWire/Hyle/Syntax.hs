{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
-- | The Hyle IR: a pure, first-order, monomorphic, total language over
--   width-indexed bitvectors, plus a synchronous device construct (registers,
--   sequential-extern instances, parallel wire equations). The syntax and
--   semantics are specified in doc/hyle.md; this module is the AST and the
--   pretty printer for the concrete syntax (doc/hyle.md, section 10).
module ReWire.Hyle.Syntax
      ( Size, Index, Name, GId, Value
      , SizeAnnotated (..)
      , Op (..), opName, opResultSize
      , Exp (..)
      , Sig (..), Defn (..)
      , ExternKind (..), Extern (..), externResultSize
      , Register (..), Instance (..), Stmt (..), Device (..)
      , Program (..)
      , Blind (..)
      , nil, isNil, cat, gather
      , reservedWords
      ) where

import ReWire.Annotation (Annote, Annotated (ann), noAnn, annProv, Provenance (..), Span (..), Blind (..))
import ReWire.BitVector (BV (..), width, showHex')
import ReWire.Orphans ()
import ReWire.Pretty (text, Pretty (pretty), Doc, vsep, hsep, nest, parens, brackets, punctuate, dquotes, squote, align, (<+>), TextShow (showt), FromGeneric (..), comma, hcat)

import qualified ReWire.BitVector as BV

import Data.Char (isAlpha, isAlphaNum)
import Data.Data (Typeable, Data (..))
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.List (intersperse)
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import qualified Data.HashSet as Set
import qualified Data.Text    as T

type Value = Integer
type Size  = Word
type Index = Word
type GId   = Text
type Name  = Text

class SizeAnnotated a where
      sizeOf :: a -> Size

---

-- | Primitive operations (doc/hyle.md, section 3.3). Static parameters
--   (target widths, replication counts) are part of the operator.
data Op = Add | Sub | Mul | UDiv | UMod | Pow
        | And | Or | XOr | Not
        | Shl | LShr | AShr
        | Eq | Ne | ULt | ULe | UGt | UGe | SLt | SLe | SGt | SGe
        | RedAnd | RedOr | RedXOr
        | ZExt !Size | SExt !Size | Trunc !Size
        | Rep !Natural
      deriving (Eq, Ord, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Op

instance Hashable Op

-- | The operator name in the concrete syntax (static arguments printed
--   separately).
opName :: Op -> Text
opName = \ case
      Add     -> "add"
      Sub     -> "sub"
      Mul     -> "mul"
      UDiv    -> "udiv"
      UMod    -> "umod"
      Pow     -> "pow"
      And     -> "and"
      Or      -> "or"
      XOr     -> "xor"
      Not     -> "not"
      Shl     -> "shl"
      LShr    -> "lshr"
      AShr    -> "ashr"
      Eq      -> "eq"
      Ne      -> "ne"
      ULt     -> "ult"
      ULe     -> "ule"
      UGt     -> "ugt"
      UGe     -> "uge"
      SLt     -> "slt"
      SLe     -> "sle"
      SGt     -> "sgt"
      SGe     -> "sge"
      RedAnd  -> "redand"
      RedOr   -> "redor"
      RedXOr  -> "redxor"
      ZExt _  -> "zext"
      SExt _  -> "sext"
      Trunc _ -> "trunc"
      Rep _   -> "rep"

-- | The typing rule for each operator (doc/hyle.md, section 3.3): given the
--   operand widths, the result width, or Nothing if the operands are
--   ill-typed.
opResultSize :: Op -> [Size] -> Maybe Size
opResultSize op szs = case (op, szs) of
      (Add        , [a, b]) | a == b           -> pure a
      (Sub        , [a, b]) | a == b           -> pure a
      (Mul        , [a, b]) | a == b           -> pure a
      (UDiv       , [a, b]) | a == b           -> pure a
      (UMod       , [a, b]) | a == b           -> pure a
      (Pow        , [a, b]) | a == b           -> pure a
      (And        , [a, b]) | a == b           -> pure a
      (Or         , [a, b]) | a == b           -> pure a
      (XOr        , [a, b]) | a == b           -> pure a
      (Not        , [a])                       -> pure a
      (Shl        , [a, _])                    -> pure a
      (LShr       , [a, _])                    -> pure a
      (AShr       , [a, _])                    -> pure a
      (Eq         , [a, b]) | a == b           -> pure 1
      (Ne         , [a, b]) | a == b           -> pure 1
      (ULt        , [a, b]) | a == b           -> pure 1
      (ULe        , [a, b]) | a == b           -> pure 1
      (UGt        , [a, b]) | a == b           -> pure 1
      (UGe        , [a, b]) | a == b           -> pure 1
      (SLt        , [a, b]) | a == b           -> pure 1
      (SLe        , [a, b]) | a == b           -> pure 1
      (SGt        , [a, b]) | a == b           -> pure 1
      (SGe        , [a, b]) | a == b           -> pure 1
      (RedAnd     , [_])                       -> pure 1
      (RedOr      , [_])                       -> pure 1
      (RedXOr     , [_])                       -> pure 1
      (ZExt m     , [a]) | a <= m              -> pure m
      (SExt m     , [a]) | a >= 1, a <= m      -> pure m
      (Trunc m    , [a]) | m <= a              -> pure m
      (Rep k      , [a])                       -> pure $ fromIntegral k * a
      _                                        -> Nothing

---

data Exp = Lit   Annote !BV
         | Undef Annote !Size
         | Var   Annote !Size !Name
         | Cat   Annote !Exp !Exp
         | Slice Annote !Index !Size !Exp         -- ^ e[offset +: width], LSB-indexed.
         | Prim  Annote !Size !Op ![Exp]
         | Call  Annote !Size !GId ![Exp]
         | XCall Annote !Size !Name ![Natural] ![Exp] -- ^ Combinational extern call with generic arguments.
         | If    Annote !Size !Exp !Exp !Exp
         | Let   Annote !Size !Name !Exp !Exp
      deriving (Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Exp

instance Hashable Exp

instance Eq Exp where
      Lit   a bv          == Lit   a' bv'            = a == a' && bv BV.==. bv'
      Undef a sz          == Undef a' sz'            = a == a' && sz == sz'
      Var   a sz x        == Var   a' sz' x'         = a == a' && sz == sz' && x == x'
      Cat   a e1 e2       == Cat   a' e1' e2'        = a == a' && e1 == e1' && e2 == e2'
      Slice a i k e       == Slice a' i' k' e'       = a == a' && i == i' && k == k' && e == e'
      Prim  a sz op es    == Prim  a' sz' op' es'    = a == a' && sz == sz' && op == op' && es == es'
      Call  a sz g es     == Call  a' sz' g' es'     = a == a' && sz == sz' && g == g' && es == es'
      XCall a sz x cs es  == XCall a' sz' x' cs' es' = a == a' && sz == sz' && x == x' && cs == cs' && es == es'
      If    a sz c t e    == If    a' sz' c' t' e'   = a == a' && sz == sz' && c == c' && t == t' && e == e'
      Let   a sz x e1 e2  == Let   a' sz' x' e1' e2' = a == a' && sz == sz' && x == x' && e1 == e1' && e2 == e2'
      _                   == _                       = False

instance SizeAnnotated Exp where
      sizeOf = \ case
            Lit   _ bv        -> fromIntegral $ width bv
            Undef _ sz        -> sz
            Var   _ sz _      -> sz
            Cat   _ e1 e2     -> sizeOf e1 + sizeOf e2
            Slice _ _ k _     -> k
            Prim  _ sz _ _    -> sz
            Call  _ sz _ _    -> sz
            XCall _ sz _ _ _  -> sz
            If    _ sz _ _ _  -> sz
            Let   _ sz _ _ _  -> sz

instance Annotated Exp where
      ann = \ case
            Lit   a _         -> a
            Undef a _         -> a
            Var   a _ _       -> a
            Cat   a _ _       -> a
            Slice a _ _ _     -> a
            Prim  a _ _ _     -> a
            Call  a _ _ _     -> a
            XCall a _ _ _ _   -> a
            If    a _ _ _ _   -> a
            Let   a _ _ _ _   -> a

-- | The unique width-0 value.
nil :: Exp
nil = Lit noAnn BV.nil

isNil :: Exp -> Bool
isNil e = sizeOf e == 0

-- | Concatenate a list of expressions, dropping width-0 components.
cat :: [Exp] -> Exp
cat = (\ case
            []         -> nil
            es@(e : _) -> foldr1 (Cat $ ann e) es
      ) . filter (not . isNil)

-- | Flatten nested concatenations, dropping width-0 components.
gather :: Exp -> [Exp]
gather = filter (not . isNil) . \ case
      Cat _ e1 e2 -> gather e1 <> gather e2
      e           -> [e]

---

data Sig = Sig Annote ![Size] !Size
      deriving (Eq, Ord, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Sig

instance Hashable Sig

instance Annotated Sig where
      ann (Sig a _ _) = a

instance SizeAnnotated Sig where
      sizeOf (Sig _ _ s) = s

data Defn = Defn
      { defnAnnote   :: Annote
      , defnName     :: !GId
      , defnSig      :: !Sig
      , defnParams   :: ![Name] -- ^ Same length as the Sig's argument list.
      , defnBody     :: !Exp
      , defnNoInline :: !Bool           -- ^ Exempt from backend inlining (including --flatten); a real attribute, visible to Eq.
      , defnDoc      :: !(Blind [Text]) -- ^ Display-only doc lines ('--|'); invisible to Eq/Ord/Hashable.
      }
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Defn

instance Hashable Defn

instance SizeAnnotated Defn where
      sizeOf (Defn _ _ (Sig _ _ s) _ _ _ _) = s

instance Annotated Defn where
      ann = defnAnnote

---

-- | Sequential externs carry the names of their clock and reset ports (either
--   may be absent); combinational externs have neither.
data ExternKind = Comb | Seq !(Maybe Name) !(Maybe Name)
      deriving (Eq, Ord, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric ExternKind

instance Hashable ExternKind

data Extern = Extern
      { extAnnote   :: Annote
      , extName     :: !Name
      , extGenerics :: ![Name]
      , extKind     :: !ExternKind
      , extInputs   :: ![(Name, Size)]
      , extOutputs  :: ![(Name, Size)]
      , extModel    :: !(Maybe GId)   -- ^ Combinational externs only.
      }
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Extern

instance Hashable Extern

instance Annotated Extern where
      ann = extAnnote

-- | The width of a call to the extern: its outputs, concatenated.
externResultSize :: Extern -> Size
externResultSize = sum . map snd . extOutputs

instance SizeAnnotated Extern where
      sizeOf = externResultSize

---

data Register = Register Annote !Name !Size !BV
      deriving (Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Register

instance Hashable Register

instance Eq Register where
      Register a x sz bv == Register a' x' sz' bv' = a == a' && x == x' && sz == sz' && bv BV.==. bv'

instance Annotated Register where
      ann (Register a _ _ _) = a

instance SizeAnnotated Register where
      sizeOf (Register _ _ sz _) = sz

data Instance = Instance Annote !Name !Name ![Natural] -- ^ Instance name, extern name, generic arguments.
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Instance

instance Hashable Instance

instance Annotated Instance where
      ann (Instance a _ _ _) = a

data Stmt = SLet    Annote !Name !Exp
          | SOutput Annote !Name !Exp
          | SNext   Annote !Name !Exp
          | SInstIn Annote !Name !Name !Exp -- ^ Instance name, port name.
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Stmt

instance Hashable Stmt

instance Annotated Stmt where
      ann = \ case
            SLet    a _ _   -> a
            SOutput a _ _   -> a
            SNext   a _ _   -> a
            SInstIn a _ _ _ -> a

data Device = Device
      { devAnnote    :: Annote
      , devName      :: !Name
      , devInputs    :: ![(Name, Size)]
      , devOutputs   :: ![(Name, Size)]
      , devRegisters :: ![Register]
      , devInstances :: ![Instance]
      , devBody      :: ![Stmt]
      , devTags      :: !(Blind [(Text, Integer)]) -- ^ Display names for the '__resumption_tag' register's values; invisible to Eq/Ord/Hashable.
      }
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Device

instance Hashable Device

instance Annotated Device where
      ann = devAnnote

data Program = Program
      { progExterns :: ![Extern]
      , progDefns   :: ![Defn]
      , progDevice  :: !Device
      }
      deriving (Eq, Ord, Show, Typeable, Data, Generic)
      deriving TextShow via FromGeneric Program

instance Hashable Program

---
--- Pretty printing (the concrete syntax; see doc/hyle.md, section 10).
---

-- | Keywords and operator names: rejected as bare identifiers by the
--   parser, and printed quoted when used as identifiers.
reservedWords :: HashSet Text
reservedWords = Set.fromList $
      [ "let", "in", "if", "then", "else", "undef"
      , "extern", "generic", "clock", "reset", "input", "output", "model"
      , "device", "register", "init", "instance", "of", "next"
      ] <> map opName [ Add, Sub, Mul, UDiv, UMod, Pow, And, Or, XOr, Not
                      , Shl, LShr, AShr
                      , Eq, Ne, ULt, ULe, UGt, UGe, SLt, SLe, SGt, SGe
                      , RedAnd, RedOr, RedXOr, ZExt 0, SExt 0, Trunc 0, Rep 0
                      ]

-- | Soft keywords, added after the format was in the wild: the parser
--   still accepts them as bare identifiers (files written before they
--   existed must keep parsing, and their clause positions are
--   unambiguous), but the printer quotes them so new output can't be
--   misread.
softKeywords :: HashSet Text
softKeywords = Set.fromList ["noinline", "tag"]

-- | True for names that can print bare: an identifier-class name that
--   isn't a (soft) keyword. Everything else prints in double quotes.
bareName :: Name -> Bool
bareName x = case T.uncons x of
      Just (c, cs) -> (isAlpha c || c `elem` ("_$" :: String))
                   && T.all (\ c' -> isAlphaNum c' || c' `elem` ("_.$'" :: String)) cs
                   && not (x `Set.member` reservedWords)
                   && not (x `Set.member` softKeywords)
      Nothing      -> False

ppName :: Name -> Doc an
ppName x | bareName x = text x
         | otherwise  = dquotes $ text $ T.concatMap esc x
      where esc :: Char -> Text
            esc = \ case
                  '"'  -> "\\\""
                  '\\' -> "\\\\"
                  c    -> T.singleton c

ppTy :: Size -> Doc an
ppTy sz = brackets $ pretty sz

ppLit :: BV -> Doc an
ppLit bv | width bv == 0 = text "0'"
         | otherwise     = pretty (width bv) <> squote <> text (showHex' bv)

ppPort :: (Name, Size) -> Doc an
ppPort (x, sz) = ppName x <+> text ":" <+> ppTy sz

-- | Provenance comment: one '--@ file:l:c-l:c' line when the annotation
--   carries a source span (re-parsed to the same span by Hyle.Parse; rwc
--   only emits these in .rwc output under --locators).
ppSpan :: Annote -> [Doc an]
ppSpan a = case annProv a of
      FromSource (Span f (l1, c1) (l2, c2)) ->
            [ text $ "--@ " <> T.pack f <> ":" <> showt l1 <> ":" <> showt c1
                                        <> "-" <> showt l2 <> ":" <> showt c2 ]
      _ -> []

-- | Doc comments: one '--| <text>' line per doc entry (entries are split at
--   newlines so every printed line carries the marker and re-parses).
ppDocs :: Blind [Text] -> [Doc an]
ppDocs (Blind docs) = map (\ d -> text $ "--| " <> d) $ concatMap T.lines docs

-- | Expression at top (let/if) level.
ppExp :: Exp -> Doc an
ppExp = \ case
      Let _ _ x e1 e2  -> vsep [ text "let" <+> ppName x <+> text "=" <+> ppCat e1 <+> text "in", ppExp e2 ]
      If _ _ c t e     -> nest 6 $ vsep [ text "if" <+> ppCat c <+> text "then" <+> ppCat t, text "else" <+> ppExpUnderElse e ]
      e                -> ppCat e

-- | An if-chain prints flat (else if ...); other exps under "else" print at
--   top level.
ppExpUnderElse :: Exp -> Doc an
ppExpUnderElse = \ case
      If _ _ c t e -> text "if" <+> ppCat c <+> text "then" <+> ppCat t <+> text "else" <+> ppExpUnderElse e
      e            -> ppExp e

-- | Expression at concatenation level.
ppCat :: Exp -> Doc an
ppCat e = case gather e of
      []  -> ppLit BV.nil -- e is nil or a concatenation of nils
      [a] -> ppApp a
      es  -> hsep $ punctuate (text " #") $ map ppApp es

-- | Expression at application level.
ppApp :: Exp -> Doc an
ppApp = \ case
      Prim _ _ op es   -> hsep $ text (opName op) : ppOpArgs op <> map ppAtom es
      Call _ _ g es    -> hsep $ ppName g : map ppAtom es
      XCall _ _ x cs es -> hsep $ (ppName x <> ppGenerics cs) : map ppAtom es
      e                -> ppAtom e
      where ppOpArgs :: Op -> [Doc an]
            ppOpArgs = \ case
                  ZExt m  -> [pretty m]
                  SExt m  -> [pretty m]
                  Trunc m -> [pretty m]
                  Rep k   -> [pretty k]
                  _       -> []

ppGenerics :: [Natural] -> Doc an
ppGenerics = \ case
      [] -> mempty
      cs -> text "<" <> hcat (punctuate comma $ map pretty cs) <> text ">"

-- | Expression at atom level: parenthesize anything that isn't atomic.
ppAtom :: Exp -> Doc an
ppAtom = \ case
      Lit _ bv        -> ppLit bv
      Var _ _ x       -> ppName x
      Slice _ i k e   -> ppAtom e <> brackets (pretty i <+> text "+:" <+> pretty k)
      Undef _ sz      -> parens $ text "undef" <+> pretty sz
      Call _ _ g []   -> ppName g
      XCall _ _ x cs [] -> ppName x <> ppGenerics cs
      e               -> parens $ ppExp e

instance Pretty Exp where
      pretty = ppExp

instance Pretty Op where
      pretty = text . opName

instance Pretty Sig where
      pretty (Sig _ args res) = parens (hsep $ punctuate comma $ map ppTy args) <+> text "->" <+> ppTy res

instance Pretty Defn where
      pretty (Defn a n sig ps e ni docs) = vsep $
            ppSpan a
            <> ppDocs docs
            <> [ ppName n <+> text ":" <+> pretty sig
               , nest 6 $ vsep [ hsep ([ text "noinline" | ni ] <> (ppName n : map ppName ps)) <+> text "=", align $ ppExp e ]
               ]

instance Pretty ExternKind where
      pretty = \ case
            Comb      -> mempty
            Seq c r   -> vsep $ maybe [] (\ c' -> [text "clock" <+> ppName c']) c
                             <> maybe [] (\ r' -> [text "reset" <+> ppName r']) r

instance Pretty Extern where
      pretty (Extern _ n gs k ins outs m) = nest 6 $ vsep $
            [ text "extern" <+> ppName n ]
            <> (case gs of
                  [] -> []
                  _  -> [text "generic" <+> hsep (punctuate comma $ map ppName gs)])
            <> (case k of
                  Comb -> []
                  _    -> [pretty k])
            <> map (\ p -> text "input" <+> ppPort p) ins
            <> map (\ p -> text "output" <+> ppPort p) outs
            <> maybe [] (\ g -> [text "model" <+> ppName g]) m

instance Pretty Register where
      pretty (Register a x sz bv) = vsep $ ppSpan a
            <> [ text "register" <+> ppName x <+> text ":" <+> ppTy sz <+> text "init" <+> ppLit bv ]

instance Pretty Instance where
      pretty (Instance _ x ex cs) = text "instance" <+> ppName x <+> text "of" <+> ppName ex <> ppGenerics cs

instance Pretty Stmt where
      pretty = \ case
            SLet    _ x e   -> nest 6 $ text "let" <+> ppName x <+> text "=" <+> ppExp e
            SOutput _ x e   -> nest 6 $ ppName x <+> text ":=" <+> ppExp e
            SNext   _ x e   -> nest 6 $ text "next" <+> ppName x <+> text ":=" <+> ppExp e
            SInstIn _ x p e -> nest 6 $ ppName x <> text "." <> ppName p <+> text ":=" <+> ppExp e

instance Pretty Device where
      pretty (Device _ n ins outs regs insts body (Blind tags)) = nest 6 $ vsep $
            [ text "device" <+> ppName n ]
            <> map (\ p -> text "input" <+> ppPort p) ins
            <> map (\ p -> text "output" <+> ppPort p) outs
            <> map pretty regs
            <> map ppTag tags
            <> map pretty insts
            <> map pretty body
            where ppTag :: (Text, Integer) -> Doc an
                  ppTag (x, v) = text "tag" <+> ppName x <+> text "=" <+> pretty v

instance Pretty Program where
      pretty (Program exts ds dev) = vsep $ intersperse (text "") $
            map pretty exts <> map pretty ds <> [pretty dev]

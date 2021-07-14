{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DerivingVia, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.LoFIRRTL.Syntax where

-- Syntax for LoFIRRTL.

import safe Data.Data (Typeable, Data (..))
import safe Data.Text (Text)
import TextShow (TextShow (..))
import TextShow.Generic (FromGeneric (..))
import safe GHC.Generics (Generic (..))
import Numeric.Natural (Natural)
import safe Prettyprinter
      ( Doc, nest, parens, punctuate
      , vsep, (<+>), Pretty (..), space
      , angles, colon, comma, dot, hsep
      )
import ReWire.Pretty (text)

data Id = Id !Text
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Id

instance Pretty Id where
      pretty (Id x) = text x

data Circuit = Circuit !Id ![Module]
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Circuit

instance Pretty Circuit where
      pretty (Circuit x mods) = nest 2 (vsep ([text "circuit" <+> pretty x <+> colon] ++ map pretty mods))

data Module = ModuleDef !Id ![Port] ![Stmt]
            | ExtModule !Id ![Port]
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Module

instance Pretty Module where
      pretty = \ case
            ModuleDef x ps stmts -> nest 2 ( vsep (
                  [ text "module" <+> pretty x <+> colon ]
                  ++ map pretty ps
                  ++ [vsep $ map pretty stmts]))
            ExtModule x ps      -> nest 2 (vsep ([text "extmodule" <+> pretty x <+> colon] ++ map pretty ps))

data Port = Input !Id !Type
          | Output !Id !Type
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Port

instance Pretty Port where
      pretty = \ case
            Input x t  -> text "input" <+> pretty x <+> colon <+> pretty t
            Output x t -> text "output" <+> pretty x <+> colon <+> pretty t

data Type = UIntTy !Int
          | SIntTy !Int
          | ClockTy
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Type

instance Pretty Type where
      pretty = \ case
            UIntTy w -> text "UInt" <+> angles (pretty w)
            SIntTy w -> text "SIntTy" <+> angles (pretty w)
            ClockTy  -> text "Clock"

-- Read Under Write flag
data RUW = Old | New | Undefined
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric RUW

instance Pretty RUW where
      pretty = \ case
            Old       -> text "old"
            New       -> text "new"
            Undefined -> text "undefined"

data Stmt = Wire !Id !Type
          | Reg !Id !Type !Exp
          | RegReset !Id !Type !Exp !Exp !Exp
          -- id, type, depth, read latency, write latency, RUW, readers, writers, readwriters
          | Mem !Id !Type !Int !Int !Int !RUW ![Id] ![Id] ![Id]
          | Inst !Id !Id
          | Node !Id !Exp
          | Connect !Exp !Exp
          | Invalidate !Exp
          | Printf !Exp !Exp !Text ![Exp]
          | Skip
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Stmt

instance Pretty Stmt where
      pretty = \ case
            Wire x t                   -> text "wire" <+> pretty x <+> colon <+> pretty t
            Reg x t e                  -> text "reg" <+> pretty x <+> colon <+> pretty t <> comma <+> pretty e
            RegReset x t e1 e2 e3      -> text "reg" <+> pretty x <+> colon <+> pretty t <> comma <+> pretty e1
                                                     <+> text "with" <+> parens (text "reset" <+> text "=>" <+> args [pretty e2, pretty e3])
            Mem _ _ _ _ _ _ _ _ _      -> text "mem"
            Inst x y                   -> text "inst" <+> pretty x <+> text "of" <+> pretty y
            Node x e                   -> text "node" <+> pretty x <+> text "=" <+> pretty e
            Connect e1 e2              -> pretty e1 <+> text "<=" <+> pretty e2
            Invalidate e               -> pretty e <+> text "is" <+> text "invalid"
            Printf e1 e2 n es          -> text "printf" <+> args ([pretty e1, pretty e2, pretty n] ++ map pretty es)
            Skip                       -> text "skip"


data Exp = UInt !Natural !Natural
      | UIntBits !Natural !Text
      | SInt !Natural !Int
      | SIntBits !Natural !Text
      | Ref !Id
      | Field !Exp !Id -- only for instantiated modules?
      | Mux !Exp !Exp !Exp
      | Validif !Exp !Exp
      -- Primitive operations
      | Add !Exp !Exp
      | Sub !Exp !Exp
      | Mul !Exp !Exp
      | Div !Exp !Exp
      | Mod !Exp !Exp
      | Lt !Exp !Exp
      | Leq !Exp !Exp
      | Gt !Exp !Exp
      | Geq !Exp !Exp
      | Eq !Exp !Exp
      | Neq !Exp !Exp
      | Pad !Exp !Natural
      | AsUInt !Exp
      | AsSInt !Exp
      | AsClock !Exp
      -- Bit shift
      | Shl !Exp !Natural
      | Shr !Exp !Natural
      -- Dynamic shift
      | Dshl !Exp !Exp 
      | Dshr !Exp !Exp
      -- Convert to signed
      | Cvt !Exp
      | Neg !Exp
      | Not !Exp
      | And !Exp !Exp
      | Or !Exp !Exp
      | Xor !Exp !Exp
      -- Bitwise reduction
      | Andr !Exp
      | Orr !Exp
      | Xorr !Exp
      | Cat !Exp !Exp
      | Bits !Exp !Natural !Natural
      | Head !Exp !Natural
      | Tail !Exp !Natural
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Exp

args :: [Doc ann] -> Doc ann
args = parens . hsep . punctuate (comma <> space)

instance Pretty Exp where
      pretty = \ case
            UInt w n     -> text "UInt" <> angles (pretty w) <> args [pretty n]
            UIntBits w n -> text "UInt" <> angles (pretty w) <> args [pretty n]
            SInt w n     -> text "UInt" <> angles (pretty w) <> args [pretty n]
            SIntBits w n -> text "UInt" <> angles (pretty w) <> args [pretty n]
            Ref x        -> pretty x
            Field m x    -> pretty m <> dot <> pretty x
            Mux a b c    -> op "mux" [a, b, c]
            Validif a b  -> op "validif" [a, b]
            Add a b      -> op "add" [a, b]
            Sub a b      -> op "sub" [a, b]
            Mul a b      -> op "mul" [a, b]
            Div a b      -> op "div" [a, b]
            Mod a b      -> op "mod" [a, b]
            Lt a b       -> op "lt" [a, b]
            Leq a b      -> op "leq" [a, b]
            Gt a b       -> op "gt" [a, b]
            Geq a b      -> op "geq" [a, b]
            Eq a b       -> op "eq" [a, b]
            Neq a b      -> op "neq" [a, b]
            Pad a b      -> op' "pad" [pretty a, pretty b]
            AsUInt a     -> op "asUInt" [a]
            AsSInt a     -> op "asSInt" [a]
            AsClock a    -> op "asClock" [a]
            Shl a b      -> op' "shl" [pretty a, pretty b]
            Shr a b      -> op' "shr" [pretty a, pretty b]
            Dshl a b     -> op "dshl" [a, b]
            Dshr a b     -> op "dshr" [a, b]
            Cvt a        -> op "cvt" [a]
            Neg a        -> op "neg" [a]
            Not a        -> op "not" [a]
            And a b      -> op "and" [a, b]
            Or a b       -> op "or" [a, b]
            Xor a b      -> op "xor" [a, b]
            Andr a       -> op "andr" [a]
            Orr a        -> op "orr" [a]
            Xorr a       -> op "xorr" [a]
            Cat a b      -> op "cat" [a, b]
            Bits a b c   -> op' "bits" [pretty a, pretty b, pretty c]
            Head a b     -> op' "head" [pretty a, pretty b]
            Tail a b     -> op' "tail" [pretty a, pretty b]
            where op :: Pretty a => Text -> [a] -> Doc ann
                  op f xs = text f <+> args (map pretty xs)

                  op' :: Text -> [Doc ann] -> Doc ann
                  op' f xs = text f <+> args xs

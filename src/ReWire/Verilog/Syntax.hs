{-# LANGUAGE Safe, OverloadedStrings #-}
module ReWire.Verilog.Syntax where

import Prettyprinter (Pretty (..), parens, (<+>), vsep, hcat, hsep, semi, colon, punctuate, comma, nest, Doc, braces, brackets)
import ReWire.Pretty (empty, text)
import Data.Text (Text)
import Data.List (intersperse)

newtype Program = Program { pgmModules :: [Module] }
      deriving (Eq, Show)

instance Pretty Program where
      pretty (Program mods) = vsep (intersperse empty $ map pretty mods)

type Name = Text
type Size = Int

data Module = Module
      { modName    :: Name
      , modInputs  :: [Signal]
      , modOutputs :: [Signal] -- also inouts, but that seems dumb.
      , modSignals :: [Signal]
      , modStmt    :: Stmt
      }
      deriving (Eq, Show)

instance Pretty Module where
      pretty (Module n inps outps sigs stmt) = text "module" <+> text n
            <+> nest 2 (parens $ vsep $ punctuate comma $ map pretty inps <> map pretty outps)
             <> vsep [nest 2 (vsep ([semi] <> map ((<> semi) . pretty) sigs <> [pretty stmt])), text "endmodule"]

data Signal = Wire Size Name
            | Reg Size Name
      deriving (Eq, Show)

ppBVName :: Size -> Name -> Doc an
ppBVName sz n = brackets (pretty (sz - 1) <> colon <> text "0") <+> text n

instance Pretty Signal where
      pretty = \ case
            Wire sz n -> text "wire" <+> ppBVName sz n
            Reg sz n  -> text "reg"  <+> ppBVName sz n

data Stmt = Always [Sensitivity] Stmt
          | If Exp Stmt Stmt
          | Connect Name Exp
          | Block [Stmt]
      deriving (Eq, Show)

instance Pretty Stmt where
      pretty = \ case
            Always sens stmt -> text "always" <+> text "@" <+> parens (hsep $ punctuate (text "or") $ map pretty sens) <+> pretty stmt
            If c thn els     -> text "if" <+> parens (pretty c) <+> pretty thn <+> text "else" <+> pretty els
            Connect n v      -> text n <+> text "<=" <+> pretty v <> semi
            Block stmts      -> vsep [nest 2 $ vsep (text "begin" : map pretty stmts), text "end"]

data Sensitivity = Pos Name | Neg Name
      deriving (Eq, Show)

instance Pretty Sensitivity where
      pretty = \ case
            Pos n -> text "posedge" <+> text n
            Neg n -> text "negedge" <+> text n

data Exp = Sub Exp Exp
         | Mul Exp Exp
         | Mod Exp Exp
         | Div Exp Exp
         | Add Exp Exp
         | Pow Exp Exp
         | LAnd Exp Exp
         | LOr Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | LShift Exp Exp
         | RShift Exp Exp
         | LShiftArith Exp Exp
         | RShiftArith Exp Exp
         | Not Exp
         | Eq  Exp Exp
         | NEq Exp Exp
         | CEq  Exp Exp
         | CNEq Exp Exp
         | Lt  Exp Exp
         | Gt  Exp Exp
         | LtEq  Exp Exp
         | GtEq  Exp Exp
         | Concat [Exp]
         | Repl Int Exp
         | LitInt Size Int
         | LitBits Size [Bit]
         | Var Name
      deriving (Eq, Show)

ppBinOp :: Exp -> Text -> Exp -> Doc an
ppBinOp a op b = parens (pretty a) <+> text op <+> parens (pretty b)

ppUnOp :: Text -> Exp -> Doc an
ppUnOp op a = text op <> parens (pretty a)

instance Pretty Exp where
      pretty = \ case
            Sub a b         -> ppBinOp a "-"   b
            Mul a b         -> ppBinOp a "*"   b
            Mod a b         -> ppBinOp a "%"   b
            Div a b         -> ppBinOp a "/"   b
            Add a b         -> ppBinOp a "+"   b
            Pow a b         -> ppBinOp a "**"  b
            LAnd a b        -> ppBinOp a "&&"  b
            LOr a b         -> ppBinOp a "||"  b
            And a b         -> ppBinOp a "&"   b
            Or a b          -> ppBinOp a "|"   b
            LShift a b      -> ppBinOp a "<<"  b
            RShift a b      -> ppBinOp a ">>"  b
            LShiftArith a b -> ppBinOp a "<<<" b
            RShiftArith a b -> ppBinOp a ">>>" b
            Not a           -> ppUnOp    "!"   a
            Eq a b          -> ppBinOp a "=="  b
            NEq a b         -> ppBinOp a "!="  b
            CEq a b         -> ppBinOp a "===" b
            CNEq a b        -> ppBinOp a "!==" b
            Lt a b          -> ppBinOp a "<"   b
            Gt a b          -> ppBinOp a ">"   b
            LtEq a b        -> ppBinOp a "<="  b
            GtEq a b        -> ppBinOp a ">="  b
            Concat es       -> braces $ hsep $ punctuate comma $ map pretty es
            Repl i e        -> braces $ pretty i <> braces (pretty e)
            LitInt w v      -> pretty w <> text "'d" <> pretty v
            LitBits w v     -> pretty w <> text "'b" <> hcat (map pretty v)
            Var n           -> text n

data Bit = Zero | One | X | Z
      deriving (Eq, Show)

instance Pretty Bit where
      pretty = \ case
            Zero -> text "0"
            One  -> text "1"
            X    -> text "x"
            Z    -> text "z"

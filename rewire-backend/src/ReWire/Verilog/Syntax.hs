{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Verilog.Syntax where

import ReWire.Pretty (empty, text, squote, Pretty (..), parens, (<+>), vsep, hsep, semi, colon, punctuate, comma, nest, Doc, braces, brackets, hcat, line, softline)
import ReWire.BitVector (BV (..), showHex', width, ones, zeros)
import qualified ReWire.BitVector as BV

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intersperse)
import Numeric.Natural (Natural)

newtype Device = Device { pgmModules :: [Module] }
      deriving (Eq, Show)

instance Pretty Device where
      pretty (Device mods) = vsep (intersperse empty $ map pretty mods)

type Name  = Text
type Size  = Word
type Index = Int
type Value = Integer

data Module = Module
      { modName     :: Name
      , modComments :: ![Text] -- ^ Header comment lines, printed above the module keyword.
      , modPorts    :: [Port]
      , modSignals  :: [Signal]
      , modStmt     :: [Stmt]
      }
      deriving (Eq, Show)

instance Pretty Module where
      pretty (Module n cmts ps sigs stmt) = vsep $
            map ppComment cmts
            <> [ nest 2 ( vsep
                            ([ text "module" <+> text n <+> parens (vsep $ punctuate comma $ map pretty ps) <> semi ]
                            <> map ((<> semi) . pretty) sigs
                            <> map pretty stmt
                            )
                        )
               , text "endmodule"
               ]

ppComment :: Text -> Doc an
ppComment c = text "//" <+> text (sanitizeComment c)

-- | Newlines in comment text would push the remainder into code position.
sanitizeComment :: Text -> Text
sanitizeComment = T.map $ \ case
      '\n' -> ' '
      '\r' -> ' '
      c    -> c

data Port = Input  Signal -- can't be reg
          | InOut  Signal -- can't be reg
          | Output Signal
      deriving (Eq, Show)

instance Pretty Port where
      pretty = \ case
            Input s  -> text "input"  <+> pretty s
            InOut s  -> text "inout"  <+> pretty s
            Output s -> text "output" <+> pretty s

ppDims :: [Size] -> Doc an
ppDims = \ case
      [] -> mempty
      ds -> mempty <+> hcat (map ppDim ds)
      where ppDim :: Size -> Doc an
            ppDim sz = brackets (pretty (toInteger sz - 1) <> colon <> text "0")

data Signal = Wire  [Size] Name [Size]
            | Logic [Size] Name [Size]
            | Reg   [Size] Name [Size]
      deriving (Eq, Show)

instance Pretty Signal where
      pretty = \ case
            Wire  ds n ds' -> text "wire"  <> ppDims ds <+> text n <> ppDims ds'
            Logic ds n ds' -> text "logic" <> ppDims ds <+> text n <> ppDims ds'
            Reg   ds n ds' -> text "reg"   <> ppDims ds <+> text n <> ppDims ds'

sigName :: Signal -> Name
sigName = \ case
      Wire  _ n _ -> n
      Logic _ n _ -> n
      Reg   _ n _ -> n

data Stmt = Always [Sensitivity] Stmt
          | AlwaysComb Stmt
          | Initial Stmt
          | IfElse Exp Stmt Stmt
          | If Exp Stmt
          | Case Exp [(Exp, Stmt)] Stmt -- ^ Scrutinee, case items, default item.
          | Assign LVal Exp
          | SeqAssign LVal Exp
          | ParAssign LVal Exp
          | WireAssign Size Name Exp    -- ^ Net declaration assignment: wire [n-1:0] x = e;
          | Decl Signal                 -- ^ A signal declaration in statement position.
          | LocalParam Size Name BV
          | Comment Text
          | Block [Stmt]
          | Instantiate Name Name [(Name, Exp)] [(Name, Exp)] -- Exp for convenience, should be LVal?
          -- Procedural statements for testbenches.
          | Delay Natural           -- ^ #n;
          | Display Text [Exp]      -- ^ $display("fmt", args);
          | Finish                  -- ^ $finish;
      deriving (Eq, Show)

instance Pretty Stmt where
      pretty = \ case
            Always sens stmt         -> text "always" <+> text "@" <+> parens (hsep $ punctuate (text " or") $ map pretty sens) <+> pretty stmt
            AlwaysComb stmt          -> text "always_comb" <+> pretty stmt
            Initial stmt             -> text "initial" <+> pretty stmt
            IfElse c thn els         -> text "if" <+> parens (pretty c) <+> pretty thn <+> text "else" <+> pretty els
            If c thn                 -> text "if" <+> parens (pretty c) <+> pretty thn
            Case c items dflt        -> vsep
                  [ nest 2 $ vsep $ (text "case" <+> parens (pretty c))
                                  : map ppItem items <> [text "default:" <+> pretty dflt]
                  , text "endcase"
                  ]
            Assign lv v              -> text "assign" <+> pretty lv <+> text "="  <+> pretty v <> semi
            SeqAssign lv v           ->                   pretty lv <+> text "="  <+> pretty v <> semi
            ParAssign lv v           ->                   pretty lv <+> text "<=" <+> pretty v <> semi
            WireAssign sz n v        -> text "wire" <> ppDims [sz] <+> text n <+> text "=" <+> pretty v <> semi
            Decl sig                 -> pretty sig <> semi
            LocalParam sz n v        -> text "localparam" <> ppDims [sz] <+> text n <+> text "=" <+> pretty (LitBits v) <> semi
            Comment c                -> ppComment c
            Block stmts              -> vsep [nest 2 $ vsep (text "begin" : map pretty stmts), text "end"]
            Delay n                  -> text "#" <> pretty (toInteger n) <> semi
            Display fmt args         -> text "$display(\"" <> text fmt <> text "\"" <> hcat (map ((comma <+>) . pretty) args) <> text ")" <> semi
            Finish                   -> text "$finish" <> semi
            Instantiate m inst ps ss -> text m <+> (if null ps then mempty else text "#" <> params ps) <+> text inst <+> params ss <> semi
                  where param :: (Name, Exp) -> Doc an
                        param = \ case
                              ("", e) -> pretty e
                              (n, e)  -> text "." <> pretty n <> parens (pretty e)

                        params :: [(Name, Exp)] -> Doc an
                        params = parens . hsep . punctuate comma . map param

ppItem :: (Exp, Stmt) -> Doc an
ppItem (l, s) = pretty l <> colon <+> pretty s

data Sensitivity = Pos Name | Neg Name
      deriving (Eq, Show)

instance Pretty Sensitivity where
      pretty = \ case
            Pos n -> text "posedge" <+> text n
            Neg n -> text "negedge" <+> text n

data Exp = Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | Pow Exp Exp
         | LAnd Exp Exp
         | LOr Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | XOr Exp Exp
         | XNor Exp Exp
         | LShift Exp Exp
         | RShift Exp Exp
         | LShiftArith Exp Exp
         | RShiftArith Exp Exp
         | Not Exp
         | LNot Exp
         | RAnd Exp
         | RNAnd Exp
         | ROr Exp
         | RNor Exp
         | RXOr Exp
         | RXNor Exp
         | Eq  Exp Exp
         | NEq Exp Exp
         | CEq  Exp Exp
         | CNEq Exp Exp
         | Lt  Exp Exp
         | Gt  Exp Exp
         | LtEq  Exp Exp
         | GtEq  Exp Exp
         | Cond Exp Exp Exp
         | Concat [Exp]
         | Repl Exp Exp
         | WCast Size Exp
         | Signed Exp   -- ^ > $signed(e)
         | Unsigned Exp -- ^ > $unsigned(e)
         | LitBits BV
         | LVal LVal
      -- N.B.: deliberately no Ord, and Eq only for shape tests -- the bv
      -- package's BV Eq is width-blind (bitVec 4 0 == bitVec 8 0), so any
      -- instance derived through 'LitBits' is too. Never key a map on
      -- this type; use the emitters' width-exact 'expKey' instead.
      deriving (Eq, Show)

class Parenless a where
      parenless :: a -> Bool

instance Parenless Exp where
      parenless = \ case
            LVal    {} -> True
            LitBits {} -> True
            Repl    {} -> True
            WCast   {} -> True
            Concat  {} -> True
            Signed  {} -> True -- self-parenthesizing
            Unsigned {} -> True
            _          -> False

mparens :: (Pretty a, Parenless a) => a -> Doc an
mparens a | parenless a = pretty a
          | otherwise   = parens $ pretty a

ppBinOp :: Exp -> Text -> Exp -> Doc an
ppBinOp a op b = mparens a <+> text op <+> mparens b

ppUnOp :: Text -> Exp -> Doc an
ppUnOp op a = text op <> mparens a

instance Pretty Exp where
      pretty = \ case
            Add a b         -> ppBinOp a "+"   b
            Sub a b         -> ppBinOp a "-"   b
            Mul a b         -> ppBinOp a "*"   b
            Div a b         -> ppBinOp a "/"   b
            Mod a b         -> ppBinOp a "%"   b
            Pow a b         -> ppBinOp a "**"  b
            LAnd a b        -> ppBinOp a "&&"  b
            LOr a b         -> ppBinOp a "||"  b
            And a b         -> ppBinOp a "&"   b
            Or a b          -> ppBinOp a "|"   b
            XOr a b         -> ppBinOp a "^"   b
            XNor a b        -> ppBinOp a "~^"  b
            LShift a b      -> ppBinOp a "<<"  b
            RShift a b      -> ppBinOp a ">>"  b
            LShiftArith a b -> ppBinOp a "<<<" b
            -- | The left operand must be cast to signed for >>> to actually
            --   sign-extend (matching the interpreter's semantics; doc/hyle.md 5.2).
            RShiftArith a b -> text "$signed" <> parens (pretty a) <+> text ">>>" <+> mparens b
            LNot a          -> ppUnOp    "!"   a
            Not a           -> ppUnOp    "~"   a
            RAnd a          -> ppUnOp    "&"   a
            RNAnd a         -> ppUnOp    "~&"  a
            ROr a           -> ppUnOp    "|"   a
            RNor a          -> ppUnOp    "~|"  a
            RXOr a          -> ppUnOp    "^"   a
            RXNor a         -> ppUnOp    "~^"  a
            Eq a b          -> ppBinOp a "=="  b
            NEq a b         -> ppBinOp a "!="  b
            CEq a b         -> ppBinOp a "===" b
            CNEq a b        -> ppBinOp a "!==" b
            Lt a b          -> ppBinOp a "<"   b
            Gt a b          -> ppBinOp a ">"   b
            LtEq a b        -> ppBinOp a "<="  b
            GtEq a b        -> ppBinOp a ">="  b
            -- | A chained conditional lays out vertically, one arm per line
            --   (layout only: parenthesization is exactly the single-line
            --   form's).
            Cond e1 e2 e3@Cond {} -> mparens e1 <+> text "?" <+> mparens e2 <+> colon <> nest 2 (line <> mparens e3)
            Cond e1 e2 e3   -> mparens e1 <+> text "?" <+> mparens e2 <+> colon <+> mparens e3
            Signed e        -> text "$signed" <> parens (pretty e)
            Unsigned e      -> text "$unsigned" <> parens (pretty e)
            Concat [e]      -> pretty e
            Concat es       -> braces $ ppCommaList $ map pretty es
            Repl e1 e2      -> braces $ pretty e1 <> braces (pretty e2)
            WCast sz e      -> pretty sz <> text "'" <> parens (pretty e)
            LitBits bv | width bv == 0 -> text "0'h0"
            LitBits bv      -> pretty (width bv) <> squote <> text (showHex' bv)
            LVal x          -> pretty x

bTrue :: Exp
bTrue = LitBits $ ones 1

bFalse :: Exp
bFalse = LitBits $ zeros 1

data Bit = Zero | One | X | Z
      deriving (Eq, Show)

instance Pretty Bit where
      pretty = \ case
            Zero -> text "0"
            One  -> text "1"
            X    -> text "x"
            Z    -> text "z"

data LVal = Element Name Index
          | Range Name Index Index
          | Name Name
          | LVals [LVal]
      deriving (Eq, Show)

instance Pretty LVal where
      pretty = \ case
            Element x i -> pretty x <> brackets (pretty i)
            Range x i j -> pretty x <> brackets (pretty j <> colon <> pretty i)
            Name x      -> text x
            LVals lvs   -> braces $ ppCommaList $ map pretty lvs

-- | A comma-separated list; long lists (> 4 elements) may wrap at the page
--   width (each separator is a softline, giving line-filling behavior).
ppCommaList :: [Doc an] -> Doc an
ppCommaList ds | length ds > 4 = nest 2 $ hcat $ punctuate (comma <> softline) ds
               | otherwise     = hsep $ punctuate comma ds

cat :: [Exp] -> Exp
cat = (\ case
            [] -> nil
            es -> Concat es
      ) . filter (not . isNil)

nil :: Exp
nil = LitBits BV.nil

isNil :: Exp -> Bool
isNil = \ case
      LitBits bv -> width bv <= 0
      WCast sz _ -> sz <= 0
      _          -> False

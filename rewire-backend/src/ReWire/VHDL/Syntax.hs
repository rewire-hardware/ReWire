{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | Syntax for the subset of VHDL-2008 emitted by the VHDL backend
--   (ReWire.Hyle.ToVHDL). Everything is typed std_logic_vector; Verilog-style
--   expression semantics (unsigned arithmetic, width rules) are provided by
--   functions in an emitted rw_helpers package.
module ReWire.VHDL.Syntax where

import ReWire.BitVector (BV, width, nat)
import ReWire.VHDL.Helpers (helpersPackage)
import ReWire.Pretty (text, Pretty (..), parens, (<+>), vsep, hsep, semi, colon, punctuate, comma, nest, align, Doc, empty)

import Data.Char (isAsciiLower, isDigit)
import Data.List (intersperse)
import Data.Text (Text)
import Numeric (showHex)
import Numeric.Natural (Natural)

import qualified Data.HashSet as Set
import qualified Data.Text    as T

type Name  = Text
type Size  = Word
type Index = Int

newtype Device = Device { devUnits :: [Unit] }
      deriving (Eq, Show)

instance Pretty Device where
      pretty (Device units) = vsep $ intersperse empty $ ppHelpers : map pretty units
            where ppHelpers :: Doc an
                  ppHelpers = vsep $ map text $ T.lines helpersPackage

-- | An entity/architecture pair with its context clause (use clauses, e.g.,
--   ieee.std_logic_1164.all; library clauses are derived).
data Unit = Unit
      { unitName       :: !Name
      , unitComments   :: ![Text] -- ^ Header comment lines, printed above the entity.
      , unitPackages   :: ![Text]
      , unitPorts      :: ![Port]
      , unitComponents :: ![Component]
      , unitSignals    :: ![Signal]
      , unitStmts      :: ![Stmt]
      }
      deriving (Eq, Show)

instance Pretty Unit where
      pretty = ppUnit

data Direction = In | Out
      deriving (Eq, Show)

instance Pretty Direction where
      pretty = \ case
            In  -> text "in"
            Out -> text "out"

data Port = Port !Name !Direction !Size
      deriving (Eq, Show)

instance Pretty Port where
      pretty (Port n d sz) = ppName n <+> colon <+> pretty d <+> ppVecTy sz

-- | Component declaration: name, integer generic names, ports.
data Component = Component !Name ![Name] ![Port]
      deriving (Eq, Show)

instance Pretty Component where
      pretty (Component n gens ps) = vsep
            [ nest 6 $ vsep $ [ text "component" <+> ppName n <+> text "is" ]
                  <> [ text "generic" <+> parens (align $ hsep $ punctuate semi $ map ppGeneric gens) <> semi | not $ null gens ]
                  <> [ text "port" <+> parens (align $ vsep $ punctuate semi $ map pretty ps) <> semi | not $ null ps ]
            , text "end component" <> semi
            ]
            where ppGeneric :: Name -> Doc an
                  ppGeneric g = ppName g <+> colon <+> text "integer"

-- | Declarations in the architecture declarative part: signals with an
--   optional initial value, constants, and display-only comment lines.
data Signal = Signal !Name !Size !(Maybe BV)
            | Constant !Name !Size !BV
            | SigComment !Text
      deriving (Eq, Show)

instance Pretty Signal where
      pretty = \ case
            Signal n sz mbv  -> text "signal" <+> ppName n <+> colon <+> ppVecTy sz <> maybe mempty ((text " :=" <+>) . ppInit) mbv <> semi
            Constant n sz bv -> text "constant" <+> ppName n <+> colon <+> ppVecTy sz <+> text ":=" <+> ppLit bv <> semi
            SigComment c     -> ppComment c

ppComment :: Text -> Doc an
ppComment c = text "--" <+> text (sanitizeComment c)

-- | Newlines in comment text would push the remainder into code position.
sanitizeComment :: Text -> Text
sanitizeComment = T.map $ \ case
      '\n' -> ' '
      '\r' -> ' '
      c    -> c

data Stmt = Assign !LVal !Exp
          | SelAssign !Exp !LVal ![(BV, Exp)] !Exp                    -- ^ Selected signal assignment: scrutinee, target, choices, others.
          | Process ![Name] ![ProcVar] ![SeqStmt]                     -- ^ Sensitivity list (empty: none), variables, body.
          | Instantiate !Name !Name ![(Name, Integer)] ![(Name, Exp)] -- ^ Component, instance label, generic map, port map (empty name: positional).
          | Comment !Text
      deriving (Eq, Show)

-- | Process-local variable declarations.
data ProcVar = LineVar !Name -- ^ variable n : line;
      deriving (Eq, Show)

instance Pretty ProcVar where
      pretty = \ case
            LineVar n -> text "variable" <+> ppName n <+> colon <+> text "line" <> semi

instance Pretty Stmt where
      pretty = \ case
            Assign lv e               -> pretty lv <+> text "<=" <+> pretty e <> semi
            SelAssign c lv arms dflt  -> nest 6 ( vsep
                  $ (text "with" <+> pretty c <+> text "select" <+> pretty lv <+> text "<=")
                  : punctuate comma (map ppArm arms <> [pretty dflt <+> text "when others"]) ) <> semi
            Comment c                 -> ppComment c
            Process sens vars body    -> vsep
                  [ nest 6 $ vsep $ (text "process" <> (if null sens then mempty else mempty <+> parens (hsep $ punctuate comma $ map ppName sens))) : map pretty vars
                  , nest 6 $ vsep $ text "begin" : map pretty body
                  , text "end process" <> semi
                  ]
            Instantiate c inst gm pm  -> ppName inst <+> colon <+> ppName c
                  <> (if null gm then mempty else mempty <+> text "generic map" <+> parens (hsep $ punctuate comma $ map ppGenAssoc gm))
                  <+> text "port map" <+> parens (hsep $ punctuate comma $ map ppPortAssoc pm) <> semi
                  where ppGenAssoc :: (Name, Integer) -> Doc an
                        ppGenAssoc (n, v) = ppName n <+> text "=>" <+> pretty v

                        ppPortAssoc :: (Name, Exp) -> Doc an
                        ppPortAssoc = \ case
                              ("", e) -> pretty e
                              (n, e)  -> ppName n <+> text "=>" <+> pretty e

-- | A selected-assignment choice: the choices are literal bit-strings of
--   exactly the scrutinee's width (never constants or aggregates, for
--   portability).
ppArm :: (BV, Exp) -> Doc an
ppArm (v, e) = pretty e <+> text "when" <+> text "\"" <> text (ppBits v) <> text "\""

-- | Sequential statements (process bodies).
data SeqStmt = SIf ![(Cond, [SeqStmt])] ![SeqStmt] -- ^ if/elsif branches, else branch.
             | SAssign !LVal !Exp
             | SWait !Natural                      -- ^ wait for n ns;
             | SWriteLn ![Chunk]                   -- ^ write(l, ...); writeline(output, l);
             | SFinish                             -- ^ std.env.finish;
      deriving (Eq, Show)

-- | A piece of a line written by SWriteLn: a string literal or the
--   hex-string rendering of an expression.
data Chunk = ChunkLit !Text | ChunkHex !Exp
      deriving (Eq, Show)

instance Pretty Chunk where
      pretty = \ case
            ChunkLit t  -> text "string'(\"" <> text t <> text "\")"
            ChunkHex e  -> text "to_hstring" <> parens (pretty e)

instance Pretty SeqStmt where
      pretty = \ case
            SAssign lv e       -> pretty lv <+> text "<=" <+> pretty e <> semi
            SWait n            -> text "wait for" <+> pretty (toInteger n) <+> text "ns" <> semi
            SWriteLn chunks    -> text "write(l, " <> hsep (punctuate (mempty <+> text "&") $ map pretty chunks) <> text "); writeline(output, l)" <> semi
            SFinish            -> text "std.env.finish" <> semi
            SIf brs els        -> vsep $ zipWith ppBranch kws brs <> ppElse els <> [text "end if" <> semi]
                  where kws :: [Text]
                        kws = "if" : repeat "elsif"

                        ppBranch :: Text -> (Cond, [SeqStmt]) -> Doc an
                        ppBranch kw (c, ss) = nest 6 $ vsep $ (text kw <+> pretty c <+> text "then") : map pretty ss

                        ppElse :: [SeqStmt] -> [Doc an]
                        ppElse = \ case
                              [] -> []
                              ss -> [nest 6 $ vsep $ text "else" : map pretty ss]

-- | Conditions appearing in generated processes.
data Cond = CondEq !Name !BV  -- ^ name = "bits"
          | CondRising !Name  -- ^ rising_edge(name(0))
      deriving (Eq, Show)

instance Pretty Cond where
      pretty = \ case
            CondEq n bv   -> ppName n <+> text "=" <+> ppLit bv
            CondRising n  -> text "rising_edge" <> parens (ppName n <> parens (pretty (0 :: Int)))

data LVal = LVName !Name
          | LVRange !Name !Index !Index -- ^ name(j downto i): low index i, high index j.
          | LVElem !Name !Index
      deriving (Eq, Show)

instance Pretty LVal where
      pretty = \ case
            LVName n      -> ppName n
            LVRange n i j -> ppName n <> parens (pretty j <+> text "downto" <+> pretty i)
            LVElem n i    -> ppName n <> parens (pretty i <+> text "downto" <+> pretty i)

data Exp = Lit !BV
         | Var !Name
         | Slice !Name !Index !Index -- ^ name(j downto i): low index i, high index j.
         | Elem !Name !Index         -- ^ name(i downto i): a 1-bit vector.
         | Cat ![Exp]
         | FunCall !Name ![Exp]
         | Num !Natural
      -- N.B.: deliberately no Ord, and Eq only for shape tests -- the bv
      -- package's BV Eq is width-blind (bitVec 4 0 == bitVec 8 0), so any
      -- instance derived through 'Lit' is too. Never key a map on this
      -- type; use the emitter's width-exact 'expKey' instead.
      deriving (Eq, Show)

instance Pretty Exp where
      pretty = \ case
            Lit bv        -> ppLit bv
            Var n         -> ppName n
            Slice n i j   -> ppName n <> parens (pretty j <+> text "downto" <+> pretty i)
            Elem n i      -> ppName n <> parens (pretty i <+> text "downto" <+> pretty i)
            Cat []        -> ppLit mempty
            Cat [e]       -> pretty e
            Cat es        -> parens $ hsep $ punctuate (mempty <+> text "&") $ map pretty es
            FunCall f es  -> text f <> parens (hsep $ punctuate comma $ map pretty es) -- only rw_helpers functions
            Num n         -> pretty $ toInteger n

-- | A qualified bit-string literal: hex (std_logic_vector'(X"5a")) when the
--   width is a whole number of hex digits, binary (std_logic_vector'(B"0101"))
--   otherwise.
ppLit :: BV -> Doc an
ppLit bv | w > 0, w `mod` 4 == 0 = text "std_logic_vector'(X\"" <> text (ppHex bv) <> text "\")"
         | otherwise             = text "std_logic_vector'(B\"" <> text (ppBits bv) <> text "\")"
      where w :: Int
            w = width bv

-- | A literal in signal-initial position: like 'ppLit', except wide all-zero
--   vectors render as an aggregate.
ppInit :: BV -> Doc an
ppInit bv | width bv > 8, nat bv == 0 = text "(others => '0')"
          | otherwise                 = ppLit bv

-- | The value in binary, exactly the vector's width.
ppBits :: BV -> Text
ppBits bv = T.pack $ map (\ i -> if odd $ nat bv `div` (2 ^ i) then '1' else '0') $ reverse [0 .. toInteger (width bv) - 1]

-- | The value in hex, zero-padded to exactly width/4 digits.
ppHex :: BV -> Text
ppHex bv = T.justifyRight (width bv `div` 4) '0' $ T.pack $ showHex (nat bv) ""

ppVecTy :: Size -> Doc an
ppVecTy sz = text "std_logic_vector" <+> parens (pretty (toInteger sz - 1) <+> text "downto" <+> pretty (0 :: Int))

-- | Print a name as a VHDL identifier: a basic identifier when valid (and
--   unambiguous: all-lowercase, since basic identifiers are case-insensitive),
--   otherwise a VHDL-2008 extended identifier (\name\).
ppName :: Name -> Doc an
ppName = text . vhdlName

vhdlName :: Name -> Text
vhdlName n | basicOk   = n
           | otherwise = "\\" <> n <> "\\"
      where basicOk :: Bool
            basicOk = case T.unpack n of
                  c : cs -> isAsciiLower c
                         && all (\ c' -> isAsciiLower c' || isDigit c' || c' == '_') cs
                         && not ("__" `T.isInfixOf` n)
                         && not ("_" `T.isSuffixOf` n)
                         && not ("rw_" `T.isPrefixOf` n)
                         && not (n `Set.member` reservedWords)
                  _      -> False

reservedWords :: Set.HashSet Text
reservedWords = Set.fromList
      [ "abs", "access", "after", "alias", "all", "and", "architecture", "array", "assert"
      , "attribute", "begin", "block", "body", "buffer", "bus", "case", "component"
      , "configuration", "constant", "context", "default", "disconnect", "downto", "else"
      , "elsif", "end", "entity", "exit", "file", "for", "force", "function", "generate"
      , "generic", "group", "guarded", "if", "impure", "in", "inertial", "inout", "is"
      , "label", "library", "linkage", "literal", "loop", "map", "mod", "nand", "new"
      , "next", "nor", "not", "null", "of", "on", "open", "or", "others", "out", "package"
      , "parameter", "port", "postponed", "procedure", "process", "property", "protected"
      , "pure", "range", "record", "register", "reject", "release", "rem", "report"
      , "return", "rol", "ror", "select", "sequence", "severity", "shared", "signal"
      , "sla", "sll", "sra", "srl", "subtype", "then", "to", "transport", "type"
      , "unaffected", "units", "until", "use", "variable", "wait", "when", "while", "with"
      , "xnor", "xor"
      ]

ppUnit :: Unit -> Doc an
ppUnit (Unit n cmts pkgs ps comps sigs stmts) = vsep $
      map ppComment cmts
      <> [ ppContext pkgs
         , nest 6 $ vsep $ (text "entity" <+> ppName n <+> text "is")
               : [ text "port" <+> parens (align $ vsep $ punctuate semi $ map pretty ps) <> semi | not $ null ps ]
         , text "end entity" <> semi
         , empty
         , nest 6 $ vsep $ (text "architecture rtl of" <+> ppName n <+> text "is") : map pretty comps <> map pretty sigs
         , nest 6 $ vsep $ text "begin" : map pretty stmts
         , text "end architecture" <> semi
         ]

-- | Library clauses are derived from the package names (the work and std
--   libraries are implicit).
ppContext :: [Text] -> Doc an
ppContext pkgs = vsep $
      map (\ l -> text "library" <+> text l <> semi) libs
      <> map (\ p -> text "use" <+> text p <> semi) pkgs
      where libs :: [Text]
            libs = foldr (\ l ls -> if l `elem` ls then ls else l : ls) []
                 $ filter (`notElem` ["work", "std"]) $ map (T.takeWhile (/= '.')) pkgs

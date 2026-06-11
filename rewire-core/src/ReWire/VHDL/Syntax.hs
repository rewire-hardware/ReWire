{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | Syntax for the subset of VHDL-2008 emitted by the VHDL backend
--   (ReWire.Core.ToVHDL). Everything is typed std_logic_vector; Verilog-style
--   expression semantics (unsigned arithmetic, width rules) are provided by
--   functions in an emitted rw_helpers package.
module ReWire.VHDL.Syntax where

import ReWire.BitVector (BV, width, nat)
import ReWire.VHDL.Helpers (helpersPackage)
import ReWire.Pretty (text, Pretty (..), parens, (<+>), vsep, hsep, semi, colon, punctuate, comma, nest, Doc, empty)

import Data.Char (isAsciiLower, isDigit)
import Data.List (intersperse)
import Data.Text (Text)
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
      pretty (Component n gens ps) = vsep $
            [ text "component" <+> ppName n <+> text "is" ]
            <> (if null gens then []
                  else [ nest 6 $ text "generic" <+> parens (hsep $ punctuate semi $ map ppGeneric gens) <> semi ])
            <> (if null ps then []
                  else [ nest 6 $ text "port" <+> parens (vsep $ punctuate semi $ map pretty ps) <> semi ])
            <> [ text "end component" <> semi ]
            where ppGeneric :: Name -> Doc an
                  ppGeneric g = ppName g <+> colon <+> text "integer"

-- | Signal with an optional initial value.
data Signal = Signal !Name !Size !(Maybe BV)
      deriving (Eq, Show)

instance Pretty Signal where
      pretty (Signal n sz mbv) = text "signal" <+> ppName n <+> colon <+> ppVecTy sz <> maybe mempty ((text " :=" <+>) . ppLit) mbv <> semi

data Stmt = Assign !LVal !Exp
          | Process ![Name] ![ProcVar] ![SeqStmt]                     -- ^ Sensitivity list (empty: none), variables, body.
          | Instantiate !Name !Name ![(Name, Integer)] ![(Name, Exp)] -- ^ Component, instance label, generic map, port map (empty name: positional).
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
            Process sens vars body    -> vsep $
                  [ text "process" <> (if null sens then mempty else mempty <+> parens (hsep $ punctuate comma $ map ppName sens)) ]
                  <> (if null vars then [] else [nest 6 $ vsep $ map pretty vars])
                  <> [ text "begin"
                     , nest 6 $ vsep $ map pretty body
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

-- | A qualified bit-string literal: std_logic_vector'(B"0101").
ppLit :: BV -> Doc an
ppLit bv = text "std_logic_vector'(B\"" <> text bits <> text "\")"
      where bits :: Text
            bits = T.pack $ map (\ i -> if odd $ nat bv `div` (2 ^ i) then '1' else '0') $ reverse [0 .. toInteger (width bv) - 1]

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
ppUnit (Unit n pkgs ps comps sigs stmts) = vsep $
      [ ppContext pkgs
      , text "entity" <+> ppName n <+> text "is"
      ]
      <> (if null ps then []
            else [nest 6 $ text "port" <+> parens (vsep $ punctuate semi $ map pretty ps) <> semi])
      <> [ text "end entity" <> semi
         , empty
         , text "architecture rtl of" <+> ppName n <+> text "is"
         , nest 6 $ vsep $ map pretty comps <> map pretty sigs
         , text "begin"
         , nest 6 $ vsep $ map pretty stmts
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

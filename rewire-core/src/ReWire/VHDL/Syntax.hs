{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | Syntax for the subset of VHDL-2008 emitted by the VHDL backend
--   (ReWire.Core.ToVHDL). Everything is typed std_logic_vector; Verilog-style
--   expression semantics (unsigned arithmetic, width rules) are provided by
--   functions in an emitted rw_helpers package.
module ReWire.VHDL.Syntax where

import ReWire.BitVector (BV, width, nat)
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

data Device = Device
      { devPackages :: [Text] -- ^ use clauses (e.g., ieee.std_logic_1164.all).
      , devUnits    :: [Unit]
      }
      deriving (Eq, Show)

instance Pretty Device where
      pretty (Device pkgs units) = vsep $ intersperse empty $ helpersPackage : map (ppUnit pkgs) units

-- | An entity/architecture pair.
data Unit = Unit
      { unitName       :: !Name
      , unitPorts      :: ![Port]
      , unitComponents :: ![Component]
      , unitSignals    :: ![Signal]
      , unitStmts      :: ![Stmt]
      }
      deriving (Eq, Show)

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
          | Process ![Name] ![SeqStmt]                                -- ^ Sensitivity list, body.
          | Instantiate !Name !Name ![(Name, Integer)] ![(Name, Exp)] -- ^ Component, instance label, generic map, port map (empty name: positional).
      deriving (Eq, Show)

instance Pretty Stmt where
      pretty = \ case
            Assign lv e               -> pretty lv <+> text "<=" <+> pretty e <> semi
            Process sens body         -> vsep
                  [ text "process" <+> parens (hsep $ punctuate comma $ map ppName sens)
                  , text "begin"
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
      deriving (Eq, Show)

instance Pretty SeqStmt where
      pretty = \ case
            SAssign lv e       -> pretty lv <+> text "<=" <+> pretty e <> semi
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
ppName n | basicOk   = text n
         | otherwise = text "\\" <> text n <> text "\\"
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

ppUnit :: [Text] -> Unit -> Doc an
ppUnit pkgs (Unit n ps comps sigs stmts) = vsep $
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

ppContext :: [Text] -> Doc an
ppContext pkgs = vsep $
      [ text "library ieee" <> semi ]
      <> map (\ p -> text "use" <+> text p <> semi) (pkgs' <> ["work.rw_helpers.all"])
      where pkgs' :: [Text]
            pkgs' = foldr (\ p ps -> if p `elem` ps then ps else p : ps) ["ieee.std_logic_1164.all", "ieee.numeric_std.all"] pkgs

-- | The rw_helpers package implements Verilog expression semantics (unsigned
--   operations, width rules per the Verilog standard) over std_logic_vector.
--   The generated code only ever assigns through rw_resize, mirroring Verilog
--   assignment truncation/extension.
helpersPackage :: Doc an
helpersPackage = vsep $ map text
      [ "library ieee;"
      , "use ieee.std_logic_1164.all;"
      , "use ieee.numeric_std.all;"
      , ""
      , "package rw_helpers is"
      , "  function rw_resize (v : std_logic_vector; n : natural) return std_logic_vector;"
      , "  function rw_add (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_sub (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_mul (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_div (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_mod (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_pow (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_and (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_or (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_xor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_xnor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_not (a : std_logic_vector) return std_logic_vector;"
      , "  function rw_shiftl (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_shiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_ashiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_land (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_lor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_lnot (a : std_logic_vector) return std_logic_vector;"
      , "  function rw_rand (a : std_logic_vector) return std_logic_vector;"
      , "  function rw_rnand (a : std_logic_vector) return std_logic_vector;"
      , "  function rw_ror (a : std_logic_vector) return std_logic_vector;"
      , "  function rw_rnor (a : std_logic_vector) return std_logic_vector;"
      , "  function rw_rxor (a : std_logic_vector) return std_logic_vector;"
      , "  function rw_rxnor (a : std_logic_vector) return std_logic_vector;"
      , "  function rw_eq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_neq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_lt (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_gt (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_lteq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_gteq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_cond (c : std_logic_vector; a : std_logic_vector; b : std_logic_vector) return std_logic_vector;"
      , "  function rw_repl (n : natural; v : std_logic_vector) return std_logic_vector;"
      , "end package;"
      , ""
      , "package body rw_helpers is"
      , "  function rw_max (a : natural; b : natural) return natural is"
      , "  begin"
      , "    if a > b then return a; else return b; end if;"
      , "  end;"
      , "  function rw_b2v (b : boolean) return std_logic_vector is"
      , "  begin"
      , "    if b then return \"1\"; else return \"0\"; end if;"
      , "  end;"
      , "  function rw_resize (v : std_logic_vector; n : natural) return std_logic_vector is"
      , "  begin"
      , "    return std_logic_vector(resize(unsigned(v), n));"
      , "  end;"
      , "  function rw_add (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return std_logic_vector(resize(unsigned(a), n) + resize(unsigned(b), n));"
      , "  end;"
      , "  function rw_sub (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return std_logic_vector(resize(unsigned(a), n) - resize(unsigned(b), n));"
      , "  end;"
      , "  function rw_mul (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return std_logic_vector(resize(resize(unsigned(a), n) * resize(unsigned(b), n), n));"
      , "  end;"
      , "  function rw_div (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    if unsigned(b) = 0 then return std_logic_vector(to_unsigned(0, n) - 1); end if;"
      , "    return std_logic_vector(resize(resize(unsigned(a), n) / resize(unsigned(b), n), n));"
      , "  end;"
      , "  function rw_mod (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    if unsigned(b) = 0 then return std_logic_vector(to_unsigned(0, n) - 1); end if;"
      , "    return std_logic_vector(resize(resize(unsigned(a), n) mod resize(unsigned(b), n), n));"
      , "  end;"
      , "  function rw_pow (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := a'length;"
      , "    variable r : unsigned(n - 1 downto 0) := to_unsigned(1, n);"
      , "  begin"
      , "    for i in 1 to to_integer(unsigned(b)) loop"
      , "      r := resize(r * unsigned(a), n);"
      , "    end loop;"
      , "    return std_logic_vector(r);"
      , "  end;"
      , "  function rw_and (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return rw_resize(a, n) and rw_resize(b, n);"
      , "  end;"
      , "  function rw_or (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return rw_resize(a, n) or rw_resize(b, n);"
      , "  end;"
      , "  function rw_xor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return rw_resize(a, n) xor rw_resize(b, n);"
      , "  end;"
      , "  function rw_xnor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return rw_resize(a, n) xnor rw_resize(b, n);"
      , "  end;"
      , "  function rw_not (a : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    return not a;"
      , "  end;"
      , "  function rw_shiftl (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    if unsigned(b) >= a'length then return std_logic_vector(to_unsigned(0, a'length)); end if;"
      , "    return std_logic_vector(shift_left(unsigned(a), to_integer(unsigned(b))));"
      , "  end;"
      , "  function rw_shiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    if unsigned(b) >= a'length then return std_logic_vector(to_unsigned(0, a'length)); end if;"
      , "    return std_logic_vector(shift_right(unsigned(a), to_integer(unsigned(b))));"
      , "  end;"
      , "  function rw_ashiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    variable sh : natural;"
      , "  begin"
      , "    if unsigned(b) >= a'length then sh := a'length; else sh := to_integer(unsigned(b)); end if;"
      , "    return std_logic_vector(shift_right(signed(a), sh));"
      , "  end;"
      , "  function rw_land (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    return rw_b2v(unsigned(a) /= 0 and unsigned(b) /= 0);"
      , "  end;"
      , "  function rw_lor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    return rw_b2v(unsigned(a) /= 0 or unsigned(b) /= 0);"
      , "  end;"
      , "  function rw_lnot (a : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    return rw_b2v(unsigned(a) = 0);"
      , "  end;"
      , "  function rw_rand (a : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    return rw_b2v((and a) = '1');"
      , "  end;"
      , "  function rw_rnand (a : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    return rw_b2v((and a) /= '1');"
      , "  end;"
      , "  function rw_ror (a : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    return rw_b2v((or a) = '1');"
      , "  end;"
      , "  function rw_rnor (a : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    return rw_b2v((or a) /= '1');"
      , "  end;"
      , "  function rw_rxor (a : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    return rw_b2v((xor a) = '1');"
      , "  end;"
      , "  function rw_rxnor (a : std_logic_vector) return std_logic_vector is"
      , "  begin"
      , "    return rw_b2v((xor a) /= '1');"
      , "  end;"
      , "  function rw_eq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return rw_b2v(resize(unsigned(a), n) = resize(unsigned(b), n));"
      , "  end;"
      , "  function rw_neq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return rw_b2v(resize(unsigned(a), n) /= resize(unsigned(b), n));"
      , "  end;"
      , "  function rw_lt (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return rw_b2v(resize(unsigned(a), n) < resize(unsigned(b), n));"
      , "  end;"
      , "  function rw_gt (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return rw_b2v(resize(unsigned(a), n) > resize(unsigned(b), n));"
      , "  end;"
      , "  function rw_lteq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return rw_b2v(resize(unsigned(a), n) <= resize(unsigned(b), n));"
      , "  end;"
      , "  function rw_gteq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    return rw_b2v(resize(unsigned(a), n) >= resize(unsigned(b), n));"
      , "  end;"
      , "  function rw_cond (c : std_logic_vector; a : std_logic_vector; b : std_logic_vector) return std_logic_vector is"
      , "    constant n : natural := rw_max(a'length, b'length);"
      , "  begin"
      , "    if unsigned(c) /= 0 then return rw_resize(a, n); else return rw_resize(b, n); end if;"
      , "  end;"
      , "  function rw_repl (n : natural; v : std_logic_vector) return std_logic_vector is"
      , "    variable r : std_logic_vector(n * v'length - 1 downto 0);"
      , "  begin"
      , "    for i in 0 to n - 1 loop"
      , "      r((i + 1) * v'length - 1 downto i * v'length) := v;"
      , "    end loop;"
      , "    return r;"
      , "  end;"
      , "end package body;"
      ]

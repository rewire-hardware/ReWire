{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | The Eidos concrete syntax (.eir) pretty-printer, implementing
--   doc/eidos.md §9. 'ReWire.Eidos.Parse' is the other half of the
--   round-trip contract: @parse . pretty@ is the identity on programs
--   modulo annotations (annotations are semantically inert and never
--   printed), and @pretty . parse . pretty == pretty@ is a tested fixpoint.
--
--   Conventions (§9): term names, type variables, and labels print with
--   their uniques (@x#12@, @Main.loop#3@, @a#7@); type and data constructor
--   names print bare (@Main.CPUState@, @Vec@); primitives print by their
--   builtin name (@rwPrimBind@) — the @rwPrim@ prefix and absence of @#@
--   distinguishes them lexically from constructors and variables. Binders
--   print @(occ#uniq :: ty)@ (the case binder, whose type is the
--   scrutinee's, prints bare); 'Con', 'Prim', and integer-literal
--   occurrences print in the parenthesized-ascription form @(name :: ty)@;
--   type arguments print @\@tyatom@. The default case alternative, when
--   present, is first in the AST (the Core convention) and prints first,
--   as @_ -> e@.
--
--   Nothing in this module introduces a soft line break: layout is fixed
--   by 'vsep' alone, so signatures (and every type) always print on one
--   line — the @defn@ production's newline between the signature line and
--   the equation line is what terminates the signature's type.
--
--   M-level 'proc' declarations (§7.1) are reserved in the grammar but do
--   not exist in the AST yet; see the marker in 'ppProgram'.
module ReWire.Eidos.Pretty
      ( prettyProgram
      , ppProgram, ppDataDefn, ppDefn
      , ppExp, ppBind, ppAlt
      , ppTy, ppSig, ppKind
      , ppId, ppTyVar, ppBinder, ppName
      ) where

import ReWire.Builtins (builtinName)
import ReWire.Eidos.Syntax
import ReWire.Pretty (Doc, Pretty (pretty), text, int, vsep, hsep, nest, align, parens, brackets, dquotes, punctuate, comma, semi, space, (<+>), prettyPrint')

import Data.Char (isAlpha, isAlphaNum)
import Data.List (intersperse)
import Data.Text (Text)

import qualified Data.Text as T

-- | Render a whole program in the .eir concrete syntax.
prettyProgram :: Program -> Text
prettyProgram = prettyPrint' . ppProgram

---
--- Names.
---

-- | A unique-carrying name occurrence: @occ#uniq@. Occurrence text that
--   does not lex as an identifier (operator names, reserved words) prints
--   backtick-quoted: @`GHC.Classes.&&`#5@.
ppName :: Text -> Uniq -> Doc an
ppName occ u = ppOcc occ <> text "#" <> int u

-- | Like 'ppOcc', but the tuple, unit, and list constructor names print
--   bare (they have their own lexemes in the grammar).
ppOcc' :: Text -> Doc an
ppOcc' occ
      | occ `elem` (["[]", "[_]"] :: [Text]) = text occ
      | T.isPrefixOf "(" occ                 = text occ
      | otherwise                            = ppOcc occ

-- | Occurrence text, backtick-quoted unless it lexes as an identifier.
ppOcc :: Text -> Doc an
ppOcc occ
      | lexable   = text occ
      | otherwise = text "`" <> text occ <> text "`"
      where lexable :: Bool
            lexable = case T.uncons occ of
                  Just (c, _) -> (isAlpha c || c == '_' || c == '$')
                        && T.all (\ ch -> isAlphaNum ch || ch `elem` ("_.$'" :: String)) occ
                        && occ `notElem` reservedOccs
                  Nothing     -> False

            reservedOccs :: [Text]
            reservedOccs =
                  [ "let", "in", "rec", "join", "jump", "case", "of", "top", "data"
                  , "forall", "inline", "noinline", "from", "list", "vec"
                  , "proc", "entry", "block", "state", "put", "get", "pause", "goto", "halt", "undef"
                  , "Nat", "_"
                  ]

-- | A term name occurrence: @x#12@ (the type is read off the binder).
ppId :: Id -> Doc an
ppId x = ppName (idOcc x) (idUniq x)

-- | A type variable occurrence: @a#7@.
ppTyVar :: TyVar -> Doc an
ppTyVar a = ppName (tvOcc a) (tvUniq a)

-- | A term binder: @(occ#uniq :: ty)@. Local binders are monomorphic
--   (doc/eidos.md §3.2), so only the signature's type is printed.
ppBinder :: Id -> Doc an
ppBinder x = parens $ ppId x <+> text "::" <+> ppTy (sigTy $ idSig x)

-- | A type variable binder (in a @forall@): @(a#7 :: kind)@.
ppTyVarBinder :: TyVar -> Doc an
ppTyVarBinder a = parens $ ppTyVar a <+> text "::" <+> ppKind (tvKind a)

---
--- Kinds, types, signatures.
---

-- | Kind, at top (arrow) level: arrows are right-associative.
ppKind :: Kind -> Doc an
ppKind = \ case
      KFun k1 k2 -> ppKindAtom k1 <+> text "->" <+> ppKind k2
      k          -> ppKindAtom k

ppKindAtom :: Kind -> Doc an
ppKindAtom = \ case
      KStar -> text "*"
      KNat  -> text "Nat"
      k     -> parens $ ppKind k

-- | Type, at top (arrow) level: arrows are right-associative and bind
--   loosest; application binds tighter.
ppTy :: Ty -> Doc an
ppTy = \ case
      Arrow _ t u -> ppTyApp t <+> text "->" <+> ppTy u
      t           -> ppTyApp t

-- | Type, at application level (left-associative).
ppTyApp :: Ty -> Doc an
ppTyApp = \ case
      TyApp _ t u -> ppTyApp t <+> ppTyAtom u
      t           -> ppTyAtom t

-- | Type, at atom level: constructors, variables, and naturals print bare;
--   everything else parenthesizes.
ppTyAtom :: Ty -> Doc an
ppTyAtom = \ case
      TyCon _ c  -> text c
      TyVarT _ a -> ppTyVar a
      TyNat _ n  -> pretty n
      t          -> parens $ ppTy t

-- | A signature: @forall (a#1 :: kind) ... . ty@, or bare @ty@ when the
--   quantifier list is empty.
ppSig :: Sig -> Doc an
ppSig (Sig tvs t) = case tvs of
      [] -> ppTy t
      _  -> text "forall" <+> hsep (map ppTyVarBinder tvs) <> text "." <+> ppTy t

---
--- Expressions.
---

-- | Expression at top level: lambda, let, case, and jump live here; all
--   are parenthesized down at atom level.
ppExp :: Exp -> Doc an
ppExp = \ case
      Lam _ x e         -> ppLam x e
      Let _ b e         -> vsep [ text "let" <+> ppBind b <+> text "in", ppExp e ]
      Case _ t e x alts -> ppCase t e x alts
      Jump _ j es       -> text "jump" <+> ppId (jpId j) <+> parens (hsep $ punctuate comma $ map ppExp es)
      e                 -> ppApp e

-- | A lambda: nested 'Lam's collapse into one parameter telescope,
--   @\\ (x#1 :: ty) (y#2 :: ty) -> e@.
ppLam :: Id -> Exp -> Doc an
ppLam x e = text "\\" <+> hsep (map ppBinder $ x : xs) <+> text "->" <+> align (ppExp body)
      where (xs, body) = flattenLam e

            flattenLam :: Exp -> ([Id], Exp)
            flattenLam = \ case
                  Lam _ y b -> let (ys, b') = flattenLam b in (y : ys, b')
                  b         -> ([], b)

-- | @case e of x#u { alt; ... } :: ty@ — the case binder prints bare (its
--   type is the scrutinee's); the carried result type follows the braces.
ppCase :: Ty -> Exp -> Id -> [Alt] -> Doc an
ppCase t e x alts = vsep
      [ nest 6 $ vsep $ (text "case" <+> ppExp e <+> text "of" <+> ppId x <+> text "{")
                      : punctuate semi (map ppAlt alts)
      , text "}" <+> text "::" <+> ppTy t
      ]

-- | A case alternative. Default and literal alternatives bind no fields
--   (the grammar has no slot for them; the linter enforces emptiness).
ppAlt :: Alt -> Doc an
ppAlt (Alt _ c xs e) = case c of
      DefaultAlt -> text "_" <+> text "->" <+> align (ppExp e)
      DataAlt d  -> hsep (ppOcc' d : map ppBinder xs) <+> text "->" <+> align (ppExp e)
      LitAlt n   -> pretty n <+> text "->" <+> align (ppExp e)

-- | A local binding (the part between @let@ and @in@).
ppBind :: Bind -> Doc an
ppBind = \ case
      NonRec x e  -> ppEq x e
      Rec eqs     -> vsep [ nest 6 $ vsep $ text "rec {" : punctuate semi (map (uncurry ppEq) eqs), text "}" ]
      Join j xs e -> text "join" <+> ppId (jpId j)
                 <+> parens (hsep $ punctuate comma $ map ppBinder xs)
                 <+> text "=" <+> align (ppExp e)

-- | One equation of a (non-recursive or recursive) let: @x#u :: ty = e@.
ppEq :: Id -> Exp -> Doc an
ppEq x e = ppId x <+> text "::" <+> ppTy (sigTy $ idSig x) <+> text "=" <+> align (ppExp e)

-- | Expression at application level (left-associative); the head, if not
--   itself an application, prints at atom level.
ppApp :: Exp -> Doc an
ppApp = \ case
      App _ e a -> ppApp e <+> ppArg a
      e         -> ppAtom e

-- | An application argument: an atom, or @\@tyatom@ for a type argument.
ppArg :: Arg -> Doc an
ppArg = \ case
      EArg e -> ppAtom e
      TArg t -> text "@" <> ppTyAtom t

-- | Expression at atom level. 'Con', 'Prim', integer, list, and vector
--   literals are self-parenthesizing ascription forms; variables and
--   string literals print bare; everything else parenthesizes.
ppAtom :: Exp -> Doc an
ppAtom = \ case
      Var _ x        -> ppId x
      Con _ t c      -> parens $ ppOcc' c <+> text "::" <+> ppTy t
      Prim _ t p     -> parens $ text (builtinName p) <+> text "::" <+> ppTy t
      LitInt _ t n   -> parens $ pretty n <+> text "::" <+> ppTy t
      LitStr _ s     -> ppStrLit s
      LitList _ t es -> parens $ text "list" <+> ppExpList es <+> text "::" <+> ppTy t
      LitVec _ t es  -> parens $ text "vec" <+> ppExpList es <+> text "::" <+> ppTy t
      e              -> parens $ ppExp e

ppExpList :: [Exp] -> Doc an
ppExpList es = brackets $ hsep $ punctuate comma $ map ppExp es

ppStrLit :: Text -> Doc an
ppStrLit s = dquotes $ text $ T.concatMap esc s
      where esc :: Char -> Text
            esc = \ case
                  '"'  -> "\\\""
                  '\\' -> "\\\\"
                  '\n' -> "\\n"
                  '\t' -> "\\t"
                  '\r' -> "\\r"
                  c    -> T.singleton c

---
--- Definitions, datatypes, programs.
---

-- | A definition: the signature line, then the equation line (attributes,
--   name, parameter telescope) with the body on a continuation line.
ppDefn :: Defn -> Doc an
ppDefn (Defn _ x ps e attr orig) = vsep
      [ ppId x <+> text "::" <+> ppSig (idSig x)
      , nest 6 $ vsep [ hsep (ppAttrs attr orig <> (ppId x : map ppBinder ps)) <+> text "=", align $ ppExp e ]
      ]

ppAttrs :: Maybe DefnAttr -> Maybe SpecOrigin -> [Doc an]
ppAttrs attr orig = maybe [] (pure . ppAttr) attr <> maybe [] (pure . ppOrigin) orig
      where ppAttr :: DefnAttr -> Doc an
            ppAttr = \ case
                  Inline   -> text "inline"
                  NoInline -> text "noinline"

            ppOrigin :: SpecOrigin -> Doc an
            ppOrigin (SpecOrigin f ts) = text "from" <+> ppOcc f <+> parens (hsep $ punctuate comma $ map ppTy ts)

-- | @data T kind { C1 :: sig1; ... }@.
ppDataDefn :: DataDefn -> Doc an
ppDataDefn (DataDefn _ n k cs) = vsep
      [ nest 6 $ vsep $ (text "data" <+> ppOcc' n <+> ppKind k <+> text "{")
                      : punctuate semi (map ppDataCon cs)
      , text "}"
      ]

ppDataCon :: DataCon -> Doc an
ppDataCon (DataCon _ c sig) = ppOcc' c <+> text "::" <+> ppSig sig

---
--- Processes (the M level, doc/eidos.md §7.1, §9).
---

-- | @proc P : ty ~> ty clock? { state* entry block* }@.
ppProc :: Proc -> Doc an
ppProc (Proc _ n it ot clk cells entry blocks) = vsep
      [ nest 6 $ vsep $ (text "proc" <+> ppOcc' n <+> text ":" <+> ppTy it <+> text "~>" <+> ppTy ot <> maybe mempty ppClock clk <+> text "{")
                      : map ppCell cells
                      <> [ppEntry entry]
                      <> map (uncurry ppBlock) blocks
      , text "}"
      ]
      where ppClock :: Text -> Doc an
            ppClock c = space <> text "@" <+> text "clock" <+> ppOcc' c

ppCell :: Cell -> Doc an
ppCell (Cell _ s t e0) = text "state" <+> ppOcc' s <+> text ":" <+> ppTy t
      <+> text ":=" <+> maybe (text "undef") ppExp e0 <> semi

ppEntry :: Block -> Doc an
ppEntry b = vsep [ nest 6 $ vsep $ text "entry {" : ppBlockBody b, text "}" ]

ppBlock :: Id -> Block -> Doc an
ppBlock l b = vsep
      [ nest 6 $ vsep $ (text "block" <+> ppId l <+> parens (hsep $ punctuate comma $ map ppBinder $ blkParams b) <+> text "{")
                      : ppBlockBody b
      , text "}"
      ]

ppBlockBody :: Block -> [Doc an]
ppBlockBody b = map ppCmd (blkCmds b) <> [ppTerm $ blkTerm b]

ppCmd :: Cmd -> Doc an
ppCmd = \ case
      CmdBind _ x e -> ppId x <+> text "::" <+> ppTy (sigTy $ idSig x) <+> text "<-" <+> align (ppExp e) <> semi
      CmdGet _ x s  -> ppId x <+> text "::" <+> ppTy (sigTy $ idSig x) <+> text "<-" <+> text "get" <+> ppOcc' s <> semi
      CmdPut _ s a  -> text "put" <+> ppOcc' s <+> ppAtomE a <> semi

ppTerm :: Term -> Doc an
ppTerm = \ case
      Pause _ a l args -> text "pause" <+> ppAtomE a <+> text "->" <+> ppId l
            <+> parens (hsep $ punctuate comma $ map ppAtomE args)
      Goto _ l args    -> text "goto" <+> ppId l
            <+> parens (hsep $ punctuate comma $ map ppAtomE args)
      Halt _ a         -> text "halt" <+> ppAtomE a
      TCase _ a alts   -> vsep
            [ nest 6 $ vsep $ (text "case" <+> ppAtomE a <+> text "of" <+> text "{")
                            : punctuate semi (map ppTAlt alts)
            , text "}"
            ]

ppTAlt :: TAlt -> Doc an
ppTAlt (TAlt _ c xs t) = case c of
      DefaultAlt -> text "_" <+> text "->" <+> align (ppTerm t)
      DataAlt d  -> hsep (ppOcc' d : map ppBinder xs) <+> text "->" <+> align (ppTerm t)
      LitAlt n   -> pretty n <+> text "->" <+> align (ppTerm t)

-- | An expression in an atom position of the machine grammar: the
--   already-atom-shaped forms (variables, literals, constructor and
--   primitive occurrences, which self-parenthesize) print as themselves;
--   anything else parenthesizes (the parser's atom production admits a
--   parenthesized expression).
ppAtomE :: Exp -> Doc an
ppAtomE e = case e of
      Var {}     -> ppExp e
      LitStr {}  -> ppExp e
      LitInt {}  -> ppExp e
      LitList {} -> ppExp e
      LitVec {}  -> ppExp e
      Con {}     -> ppExp e
      Prim {}    -> ppExp e
      _          -> parens $ ppExp e

-- | A whole program: datatypes, definitions, processes, and the @top@
--   designation, separated by blank lines.
ppProgram :: Program -> Doc an
ppProgram (Program datas defns procs top) = vsep $ intersperse (text "") $
      map ppDataDefn datas
      <> map ppDefn defns
      <> map ppProc procs
      <> [ text "top" <+> ppId top ]

---
--- Pretty instances (canonical companion instances for the Eidos AST;
--- ReWire.Eidos.Syntax deliberately contains no printing code).
---

instance Pretty Kind where
      pretty = ppKind

instance Pretty TyVar where
      pretty = ppTyVar

instance Pretty Ty where
      pretty = ppTy

instance Pretty Sig where
      pretty = ppSig

instance Pretty Id where
      pretty = ppId

instance Pretty Exp where
      pretty = ppExp

instance Pretty Arg where
      pretty = ppArg

instance Pretty Bind where
      pretty = ppBind

instance Pretty Alt where
      pretty = ppAlt

instance Pretty Defn where
      pretty = ppDefn

instance Pretty DataCon where
      pretty = ppDataCon

instance Pretty DataDefn where
      pretty = ppDataDefn

instance Pretty Program where
      pretty = ppProgram

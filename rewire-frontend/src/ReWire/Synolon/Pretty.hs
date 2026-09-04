{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | The Synolon concrete syntax (@.syn@) pretty-printer (doc/synolon.md §3.4,
--   §9): the process declarations, and the program that embeds Eidos datatypes
--   and definitions (printed by 'ReWire.Eidos.Pretty', as are the
--   expressions inside blocks). 'ReWire.Synolon.Parse' is the other half of
--   the round-trip contract: @parse . pretty@ is the identity modulo
--   annotations, and @pretty . parse . pretty == pretty@ is a tested
--   fixpoint.
--
--   The 'Block' instance is normative beyond printing: the alpha-equal
--   block merge of 'ReWire.Synolon.Transform' keys blocks on this rendering
--   of their canonically renumbered form, so what it prints (parameter
--   binders, commands with their binders and right-hand sides, the
--   terminator with its target labels) decides which blocks merge, and so
--   the state count of every device.
module ReWire.Synolon.Pretty
      ( prettyProgram
      , ppProgram, ppProc, ppCell, ppBlock, ppCmd, ppTerm, ppTAlt, ppAtomE
      ) where

import ReWire.Eidos.Pretty (ppDataDefn, ppDefn, ppExp, ppTy, ppId, ppBinder, ppOcc')
import ReWire.Pretty (Doc, Pretty (pretty), text, vsep, hsep, nest, align, parens, punctuate, comma, semi, space, (<+>), prettyPrint')
import ReWire.Synolon.Syntax

import Data.List (intersperse)
import Data.Text (Text)

-- | Render a whole program in the .syn concrete syntax.
prettyProgram :: Program -> Text
prettyProgram = prettyPrint' . ppProgram


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


-- | A whole program: datatypes, definitions, and processes, separated by
--   blank lines.
ppProgram :: Program -> Doc an
ppProgram (Program datas defns procs) = vsep $ intersperse (text "") $
      map ppDataDefn datas
      <> map ppDefn defns
      <> map ppProc procs

---
--- Pretty instances (the Eidos types keep theirs in ReWire.Eidos.Pretty).
---

instance Pretty Block where
      pretty b = vsep $ parens (hsep $ punctuate comma $ map ppBinder $ blkParams b) : ppBlockBody b

instance Pretty Proc where
      pretty = ppProc

instance Pretty Program where
      pretty = ppProgram

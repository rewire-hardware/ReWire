{-# OPTIONS -fwarn-incomplete-patterns #-}

module ReWire.Core.PrettyPrintHaskell (ppHaskell) where

import ReWire.Core.Syntax
import Text.PrettyPrint
import Unbound.LocallyNameless hiding (empty)
import Control.Monad (liftM)

ppDataCon (RWCDataCon n ts) = do ts_p <- mapM ppTyAppR ts
                                 return (text n <+> hsep ts_p)

ppDataDecl (RWCData n b) = lunbind b (\(tvs,dcs) ->
                            do dcs_p <- mapM ppDataCon dcs
                               return (foldr ($+$) empty
                                             [text "data" <+> text n <+> hsep (map ppName tvs) <+> (if null dcs_p then empty else char '='),
                                              nest 4 (hsep (punctuate (char '|') dcs_p))]))

ppDataDecls dds = do dds_p <- mapM ppDataDecl dds
                     return (foldr ($+$) empty dds_p)

ppLiteral (RWCLitInteger n) = integer n
ppLiteral (RWCLitFloat x)   = double x
ppLiteral (RWCLitChar c)    = text (show c)

ppPat (RWCPatCon n ps)        = do ps_p <- mapM ppPat ps
                                   return (parens (text n <+> hsep ps_p))
ppPat (RWCPatVar (Embed t) n) = return (ppName n)
ppPat (RWCPatLiteral l)       = return (ppLiteral l)

ppAlt (RWCAlt b) = lunbind b (\(p,eb) ->
                    do p_p  <- ppPat p
                       eb_p <- ppExpr eb
                       return (parens p_p <+> text "->" <+> eb_p))

ppExpr (RWCApp t e1 e2) = do e1_p <- ppExpr e1
                             e2_p <- ppExpr e2
                             return (parens (hang e1_p 4 e2_p))
ppExpr (RWCLiteral t l) = return (ppLiteral l)
ppExpr (RWCCon t n)     = return (text n)
ppExpr (RWCVar t n)     = return (ppName n)
ppExpr (RWCLam t b)     = lunbind b (\(n,e) ->
                           do e_p <- ppExpr e
                              return (parens (char '\\' <+> ppName n <+> text "->" <+> e_p)))
ppExpr (RWCCase t e alts) = do e_p    <- ppExpr e
                               alts_p <- mapM ppAlt alts
                               return (parens $
                                         foldr ($+$) empty
                                              [text "case" <+> e_p <+> text "of",
                                               nest 4 (braces $ vcat $ punctuate (space <> text ";" <> space) alts_p)])

ppName :: Name a -> Doc
ppName = text . show

ppTyArrowL t@(RWCTyApp (RWCTyApp (RWCTyCon ("(->)")) t1) t2) = liftM parens (ppTy t)
ppTyArrowL t                                                 = ppTy t

ppTy (RWCTyApp (RWCTyApp (RWCTyCon ("(->)")) t1) t2) = do t1_p <- ppTyArrowL t1
                                                          t2_p <- ppTy t2
                                                          return (t1_p <+> text "->" <+> t2_p)
ppTy (RWCTyApp t1 t2) = do t1_p <- ppTy t1
                           t2_p <- ppTyAppR t2
                           return (t1_p <+> t2_p)
ppTy (RWCTyCon n)     = return (text n)
ppTy (RWCTyVar n)     = return (ppName n)

ppTyAppR t@(RWCTyApp _ _) = liftM parens (ppTy t)
ppTyAppR t                = ppTy t

commaSep []     = empty
commaSep [x]    = x
commaSep (x:xs) = x <> char ',' <> commaSep xs

ppDefn (RWCDefn n (Embed b)) = lunbind b (\(tvs,(ty,e)) ->
                                do ty_p <- ppTy ty
                                   e_p  <- ppExpr e
                                   return (foldr ($+$) empty
                                                 [ppName n <+> text "::" <+> ty_p,
                                                  ppName n <+> text "=",
                                                  nest 4 e_p]))

ppDefns defns_ = do defns   <- luntrec defns_
                    defns_p <- avoid (map defnName defns) $ mapM ppDefn defns
                    return (foldr ($+$) empty defns_p)
  where defnName (RWCDefn n _) = AnyName n

ppProg :: LFresh m => RWCProg -> m Doc
ppProg p = do dd_p <- ppDataDecls (dataDecls p)
              ds_p <- ppDefns (defns p)
              return (text "import Prelude ()" $+$ dd_p $+$ ds_p)

ppHaskell :: RWCProg -> Doc
ppHaskell = runLFreshM . ppProg

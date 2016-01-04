{-# OPTIONS -fwarn-incomplete-patterns #-}

--
-- NOTE: This is in Monad for historical reasons.
--

module ReWire.Core.PrettyPrintHaskell (ppHaskell) where

import ReWire.Scoping
import ReWire.Core.Syntax
import Text.PrettyPrint
--import Control.Monad (liftM)
import Control.Monad.Identity

ppDataCon :: Monad m => RWCDataCon -> m Doc
ppDataCon (RWCDataCon n ts) = do ts_p <- mapM ppTyAppR ts
                                 return (text (deDataConId n) <+> hsep ts_p)

-- FIXME: just ignoring the kind here
ppDataDecl :: Monad m => RWCData -> m Doc
ppDataDecl (RWCData n tvs _ dcs) =
                            do dcs_p <- mapM ppDataCon dcs
                               return (foldr ($+$) empty
                                             [text "data" <+> text (deTyConId n) <+> hsep (map ppId tvs) <+> (if null dcs_p then empty else char '='),
                                              nest 4 (hsep (punctuate (char '|') dcs_p))])

ppDataDecls :: Monad m => [RWCData] -> m Doc
ppDataDecls dds = do dds_p <- mapM ppDataDecl dds
                     return (foldr ($+$) empty dds_p)

ppId :: Id a -> Doc
ppId = text . deId

ppLiteral :: RWCLit -> Doc
ppLiteral (RWCLitInteger n) = integer n
ppLiteral (RWCLitFloat x)   = double x
ppLiteral (RWCLitChar c)    = text (show c)

ppPat :: Monad m => RWCPat -> m Doc
ppPat (RWCPatCon n ps)        = do ps_p <- mapM ppPat ps
                                   return (parens (text (deDataConId n) <+> hsep ps_p))
ppPat (RWCPatVar n _)         = return (ppId n)
ppPat (RWCPatLiteral l)       = return (ppLiteral l)

ppAlt :: Monad m => RWCAlt -> m Doc
ppAlt (RWCAlt p eb) =
                    do p_p  <- ppPat p
                       eb_p <- ppExpr eb
                       return (parens p_p <+> text "->" <+> eb_p)

ppExpr :: Monad m => RWCExp -> m Doc
ppExpr (RWCApp e1 e2) = do e1_p <- ppExpr e1
                           e2_p <- ppExpr e2
                           return (parens (hang e1_p 4 e2_p))
ppExpr (RWCLiteral l) = return (ppLiteral l)
ppExpr (RWCCon n _)     = return (text (deDataConId n))
ppExpr (RWCVar n _)     = return (ppId n)
ppExpr (RWCLam n _ e)   = do e_p <- ppExpr e
                             return (parens (char '\\' <+> ppId n <+> text "->" <+> e_p))
ppExpr (RWCCase e alts) = do e_p    <- ppExpr e
                             alts_p <- mapM ppAlt alts
                             return (parens $
                                      foldr ($+$) empty
                                        [text "case" <+> e_p <+> text "of",
                                         nest 4 (braces $ vcat $ punctuate (space <> text ";" <> space) alts_p)])
ppExpr (RWCNativeVHDL n e) = do e_p <- ppExpr e
                                return (parens (text "nativeVHDL" <+> doubleQuotes (text n) <+> parens e_p))

ppTyArrowL :: Monad m => RWCTy -> m Doc
ppTyArrowL t@(RWCTyApp (RWCTyApp (RWCTyCon (TyConId "->")) _) _) = liftM parens (ppTy t)
ppTyArrowL t                                                     = ppTy t

ppTy :: Monad m => RWCTy -> m Doc
ppTy (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "->")) t1) t2) = do t1_p <- ppTyArrowL t1
                                                                t2_p <- ppTy t2
                                                                return (t1_p <+> text "->" <+> t2_p)
ppTy (RWCTyApp t1 t2) = do t1_p <- ppTy t1
                           t2_p <- ppTyAppR t2
                           return (t1_p <+> t2_p)
ppTy (RWCTyCon n)     = return (text (deTyConId n))
ppTy (RWCTyVar n)     = return (ppId n)
ppTy (RWCTyComp t1 t2) = do t1_p <- ppTy t1
                            t2_p <- ppTyAppR t2
                            return (text "{- computation -}" <+> t1_p <+> t2_p)

ppTyAppR :: Monad m => RWCTy -> m Doc
ppTyAppR t@(RWCTyApp _ _) = liftM parens (ppTy t)
ppTyAppR t                = ppTy t

ppDefn :: Monad m => RWCDefn -> m Doc
ppDefn (RWCDefn n (_ :-> ty) b e) =
                                do ty_p <- ppTy ty
                                   e_p  <- ppExpr e
                                   return (foldr ($+$) empty
                                                 (   [ppId n <+> text "::" <+> ty_p]
                                                  ++ (if b then [text "{-# INLINE" <+> ppId n <+> text "#-}"] else [])
                                                  ++ [ppId n <+> text "=",
                                                      nest 4 e_p]))

ppDefns :: Monad m => [RWCDefn] -> m Doc
ppDefns defns = do defns_p <- mapM ppDefn defns
                   return (foldr ($+$) empty defns_p)

ppModule :: Monad m => RWCProgram -> m Doc
ppModule m = do dd_p      <- ppDataDecls (dataDecls m)
                ds_p      <- ppDefns (defns m)
                return (dd_p $+$ ds_p)

ppHaskell :: RWCProgram -> Doc
ppHaskell = runIdentity . ppModule

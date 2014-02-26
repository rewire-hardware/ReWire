{-# OPTIONS -fwarn-incomplete-patterns #-}

module ReWire.Core.PrettyPrint (pp) where

import ReWire.Core.Syntax
import Text.PrettyPrint
import Unbound.LocallyNameless hiding (empty)

ppDataCon (RWCDataCon n ts) = do ts_p <- mapM ppTy ts
                                 return (text n <+> char '<' <> commaSep ts_p <> char '>')

ppDataDecl (RWCData n b) = lunbind b (\(tvs,dcs) ->
                            do dcs_p     <- mapM ppDataCon dcs
                               return (foldr ($+$) empty
                                             [text "data" <+> text n,
                                              nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
                                              text "of",
                                              nest 4 (foldr ($+$) empty dcs_p),
                                              text "end"]))

ppDataDecls dds = do dds_p <- mapM ppDataDecl dds
                     return (foldr ($+$) empty dds_p)

ppLiteral (RWCLitInteger n) = integer n
ppLiteral (RWCLitFloat x)   = double x
ppLiteral (RWCLitChar c)    = text (show c)

ppPat (RWCPatCon n ps)        = do ps_p <- mapM ppPat ps
                                   return (parens (text n <+> hsep ps_p))
ppPat (RWCPatVar (Embed t) n) = do t_p <- ppTy t
                                   return (ppName n <> char '<' <> t_p <> char '>')
ppPat (RWCPatLiteral l)       = return (ppLiteral l)

ppAlt (RWCAlt b) = lunbind b (\(p,eb) ->
                    do p_p    <- ppPat p
                       eb_p   <- ppExpr eb
                       return (char '<' <> p_p <> char '>' <+> eb_p))

ppExpr (RWCApp t e1 e2)   = do t_p  <- ppTy t
                               e1_p <- ppExpr e1
                               e2_p <- ppExpr e2
                               return (parens (hang e1_p 4 e2_p) <> char '<' <> t_p <> char '>')
ppExpr (RWCLiteral t l) = do t_p <- ppTy t
                             return (ppLiteral l <> char '<' <> t_p <> char '>')
ppExpr (RWCCon t n)     = do t_p <- ppTy t
                             return (text n <> char '<' <> t_p <> char '>')
ppExpr (RWCVar t n)     = do t_p <- ppTy t
                             return (ppName n <> char '<' <> t_p <> char '>')
ppExpr (RWCLam t b)     = lunbind b (\(n,e) ->
                           do t_p   <- ppTy t
                              e_p   <- ppExpr e
                              return (braces (char '\\' <+> ppName n <+> text "->" <+> e_p) <> char '<' <> t_p <> char '>'))
ppExpr (RWCCase t e alts) = do t_p    <- ppTy t
                               e_p    <- ppExpr e
                               alts_p <- mapM ppAlt alts
                               return (foldr ($+$) empty
                                              [text "case" <+> e_p <+> text "of",
                                               nest 4 (foldr ($+$) empty alts_p),
                                               text "end",
                                               char '<' <> t_p <> char '>'])

ppName :: Name a -> Doc
ppName = text . show

ppTy (RWCTyApp t1 t2) = do t1_p <- ppTy t1
                           t2_p <- ppTy t2
                           return (parens (t1_p <+> t2_p))
ppTy (RWCTyCon n)     = return (text n)
ppTy (RWCTyVar n)     = return (ppName n)

commaSep []     = empty
commaSep [x]    = x
commaSep (x:xs) = x <> char ',' <> commaSep xs

ppDefn (RWCDefn n (Embed b)) = lunbind b (\(tvs,(ty,e)) ->
                                do let tvs_p    =  map ppName tvs
                                   ty_p         <- ppTy ty
                                   e_p          <- ppExpr e
                                   return (foldr ($+$) empty
                                                 [text "def" <+> ppName n,
                                                  nest 4 (char '<' <> commaSep tvs_p <> char '>'),
                                                  nest 4 (char '<' <> ty_p <> char '>'),
                                                  text "is",
                                                  nest 4 e_p,
                                                  text "end"]))

defnName (RWCDefn n _) = AnyName n

ppDefns defns_ = do defns   <- luntrec defns_
                    defns_p <- avoid (map defnName defns) $ mapM ppDefn defns
                    return (foldr ($+$) empty defns_p)

ppProg :: LFresh m => RWCProg -> m Doc
ppProg p = do dd_p <- ppDataDecls (dataDecls p)
              ds_p <- ppDefns (defns p)
              return (dd_p $+$ ds_p)

pp :: RWCProg -> Doc
pp = runLFreshM . ppProg

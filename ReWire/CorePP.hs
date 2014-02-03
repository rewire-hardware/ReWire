{-# OPTIONS -fwarn-incomplete-patterns #-}

module ReWire.CorePP where

import ReWire.Core
import ReWire.CoreParser hiding (parens,commaSep,braces,integer,float)
import Text.PrettyPrint
import Unbound.LocallyNameless hiding (empty)

data PPFlags = PPFlags { showTypesOnExpressions :: Bool }

defaultFlags = PPFlags { showTypesOnExpressions = True }

ppDataDecls _ = return empty
ppNewtypeDecls _ = return empty

ppLiteral (RWCLitInteger n) = integer n
ppLiteral (RWCLitFloat x)   = double x
ppLiteral (RWCLitChar c)    = text (show c)

ppPat (RWCPatCon (Embed t) n ps)  = do t_p  <- ppTy t
                                       ps_p <- mapM ppPat ps
                                       return (parens (text n <+> hsep ps_p) <> char '<' <> t_p <> char '>')
ppPat (RWCPatVar (Embed t) n)     = do t_p <- ppTy t
                                       return (ppName n <> char '<' <> t_p <> char '>')
ppPat (RWCPatLiteral (Embed t) l) = do t_p <- ppTy t
                                       return (ppLiteral l <> char '<' <> t_p <> char '>')

ppAlt (RWCAlt b) = do (p,(eg,eb)) <- unbind b
                      p_p         <- ppPat p
                      eg_p        <- ppExpr eg
                      eb_p        <- ppExpr eb
                      return (char '<' <> p_p <> char '|' <> eg_p <> char '>' <+> eb_p)

ppExpr (RWCApp t e1 e2) = do t_p  <- ppTy t
                             e1_p <- ppExpr e1
                             e2_p <- ppExpr e2
                             return (parens (e1_p <+> e2_p) <> char '<' <> t_p <> char '>')
ppExpr (RWCLiteral t l) = do t_p <- ppTy t
                             return (ppLiteral l <> char '<' <> t_p <> char '>')
ppExpr (RWCCon t n)     = do t_p <- ppTy t
                             return (text n <> char '<' <> t_p <> char '>')
ppExpr (RWCVar t n)     = do t_p <- ppTy t
                             return (ppName n <> char '<' <> t_p <> char '>')
ppExpr (RWCLam t b)     = do ((n,Embed t'),e) <- unbind b
                             t_p              <- ppTy t
                             t'_p             <- ppTy t'
                             e_p              <- ppExpr e
                             return (braces (char '\\' <+> ppName n <> char '<' <> t'_p <> char '>' <+> text "->" <+> e_p) <> char '<' <> t_p <> char '>')
ppExpr (RWCCase t e alts) = do t_p    <- ppTy t
                               e_p    <- ppExpr e
                               alts_p <- mapM ppAlt alts
                               return (foldr ($+$) empty
                                            [text "case" <+> e_p <+> text "of",
                                             nest 4 (foldr ($+$) empty alts_p),
                                             text "end" <> char '<' <> t_p <> char '>'])

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

ppConstraint (RWCConstraint n ts) = do ts_p <- mapM ppTy ts
                                       return (text n <+> char '<' <> commaSep ts_p <> char '>')

ppClassMethod (RWCClassMethod n (Embed b)) = do (tvs,(cs,t,mimpl)) <- unbind b
                                                cs_p               <- mapM ppConstraint cs
                                                t_p                <- ppTy t
                                                impl_p             <- case mimpl of
                                                                        Just impl -> do impl_p <- ppExpr impl
                                                                                        return (text "is" $+$ nest 4 impl_p)
                                                                        Nothing   -> return empty
                                                return (foldr ($+$) empty
                                                              [text "method" <+> ppName n,
                                                               nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
                                                               nest 4 (char '<' <> hsep cs_p <> char '>'),
                                                               nest 4 (char '<' <> t_p <> char '>'),
                                                               impl_p,
                                                               text "end"])

ppInstanceMethod (RWCInstanceMethod n b) = do (tvs,(constraints,t,e)) <- unbind b
                                              constraints_p           <- mapM ppConstraint constraints
                                              t_p                     <- ppTy t
                                              e_p                     <- ppExpr e
                                              return (foldr ($+$) empty
                                                             [text "def" <+> ppName n,
                                                              nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
                                                              nest 4 (char '<' <> hsep constraints_p <> char '>'),
                                                              nest 4 (char '<' <> t_p <> char '>'),
                                                              text "is",
                                                              nest 4 e_p,
                                                              text "end"])

ppInstance (RWCInstance b) = do (tvs,(constraints,t,ims)) <- unbind b
                                constraints_p             <- mapM ppConstraint constraints
                                t_p                       <- ppTy t
                                ims_p                     <- mapM ppInstanceMethod ims
                                return (foldr ($+$) empty
                                              [text "instance",
                                               nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
                                               nest 4 (char '<' <> hsep constraints_p <> char '>'),
                                               nest 4 (char '<' <> t_p <> char '>'),
                                               text "where",
                                               nest 4 (foldr ($+$) empty ims_p),
                                               text "end"])

ppDefn (RWCDefn n (Embed b)) = do (tvs,(constraints,ty,e)) <- unbind b
                                  constraints_p            <- mapM ppConstraint constraints
                                  ty_p                     <- ppTy ty
                                  e_p                      <- ppExpr e
                                  return (foldr ($+$) empty
                                                [text "def" <+> ppName n,
                                                 nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
                                                 nest 4 (char '<' <> hsep constraints_p <> char '>'),
                                                 nest 4 (char '<' <> ty_p <> char '>'),
                                                 text "is",
                                                 nest 4 e_p,
                                                 text "end"])
ppDefn (RWCClass n (Embed b) ms (Embed is)) = do (tvs,(constraints,ts)) <- unbind b
                                                 constraints_p          <- mapM ppConstraint constraints
                                                 ts_p                   <- mapM ppTy ts
                                                 ms_p                   <- mapM ppClassMethod ms
                                                 is_p                   <- mapM ppInstance is
                                                 return (foldr ($+$) empty
                                                               [text "class" <+> text n,
                                                                nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
                                                                nest 4 (char '<' <> hsep constraints_p <> char '>'),
                                                                nest 4 (char '<' <> hsep ts_p <> char '>'),
                                                                text "where",
                                                                nest 4 (foldr ($+$) empty ms_p),
                                                                nest 4 (foldr ($+$) empty is_p),
                                                                text "end"])

ppDefns defns_ = do defns   <- untrec defns_
                    defns_p <- mapM ppDefn defns
                    return (foldr ($+$) empty defns_p)

ppProg :: Fresh m => RWCProg -> m Doc
ppProg p = do dd_p <- ppDataDecls (dataDecls p)
              nt_p <- ppNewtypeDecls (newtypeDecls p)
              ds_p <- ppDefns (defns p)
              return (dd_p $+$ nt_p $+$ ds_p)
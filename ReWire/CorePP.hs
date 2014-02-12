{-# OPTIONS -fwarn-incomplete-patterns #-}

module ReWire.CorePP where

import ReWire.Core
import ReWire.CoreParser hiding (parens,commaSep,braces,integer,float)
import Text.Parsec (runParser,eof)
import Text.PrettyPrint
import Unbound.LocallyNameless hiding (empty)

data PPFlags = PPFlags { showTypesOnExpressions :: Bool }

defaultFlags = PPFlags { showTypesOnExpressions = True }

ppDataCon (RWCDataCon n ts) = do ts_p <- mapM ppTy ts
                                 return (text n <+> char '<' <> commaSep ts_p <> char '>')

ppDataDecl (RWCData n b) = do (tvs,dcs) <- unbind b
                              dcs_p     <- mapM ppDataCon dcs
                              return (foldr ($+$) empty
                                            [text "data" <+> text n,
                                             nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
                                             text "of",
                                             nest 4 (foldr ($+$) empty dcs_p),
                                             text "end"])

ppDataDecls dds = do dds_p <- mapM ppDataDecl dds
                     return (foldr ($+$) empty dds_p)

ppNewtypeCon (RWCNewtypeCon n t) = do t_p <- ppTy t
                                      return (text n <+> char '<' <> t_p <> char '>')

ppNewtypeDecl (RWCNewtype n b) = do (tvs,nc) <- unbind b
                                    nc_p     <- ppNewtypeCon nc
                                    return (foldr ($+$) empty
                                                  [text "newtype" <+> text n,
                                                   nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
                                                   text "of",
                                                   nest 4 nc_p,
                                                   text "end"])

ppNewtypeDecls nds = do nds_p <- mapM ppNewtypeDecl nds
                        return (foldr ($+$) empty nds_p)

ppLiteral (RWCLitInteger n) = integer n
ppLiteral (RWCLitFloat x)   = double x
ppLiteral (RWCLitChar c)    = text (show c)

ppPat (RWCPatCon n ps)  = do ps_p <- mapM ppPat ps
                             return (parens (text n <+> hsep ps_p))
ppPat (RWCPatVar n)     = return (ppName n)
ppPat (RWCPatLiteral l) = return (ppLiteral l)

ppAlt (RWCAlt b) = do (p,(eg,eb)) <- unbind b
                      p_p         <- ppPat p
                      eg_p        <- ppExpr eg
                      eb_p        <- ppExpr eb
                      return (char '<' <> p_p <> char '|' <> eg_p <> char '>' <+> eb_p)

ppExpr (RWCApp e1 e2)   = do e1_p <- ppExpr e1
                             e2_p <- ppExpr e2
                             return (parens (hang e1_p 4 e2_p))
ppExpr (RWCLiteral t l) = do t_p <- ppTy t
                             return (ppLiteral l <> char '<' <> t_p <> char '>')
ppExpr (RWCCon t n)     = do t_p <- ppTy t
                             return (text n <> char '<' <> t_p <> char '>')
ppExpr (RWCVar t n)     = do t_p <- ppTy t
                             return (ppName n <> char '<' <> t_p <> char '>')
ppExpr (RWCLam b)       = do (n,e) <- unbind b
                             e_p   <- ppExpr e
                             return (braces (char '\\' <+> ppName n <+> text "->" <+> e_p))
ppExpr (RWCCase e alts) = do e_p    <- ppExpr e
                             alts_p <- mapM ppAlt alts
                             return (foldr ($+$) empty
                                            [text "case" <+> e_p <+> text "of",
                                             nest 4 (foldr ($+$) empty alts_p),
                                             text "end"])

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
--                                                               nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
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
--                                                              nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
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
--                                               nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
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
--                                                 nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
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
--                                                                nest 4 (char '<' <> hsep (map ppName tvs) <> char '>'),
                                                                nest 4 (char '<' <> hsep constraints_p <> char '>'),
                                                                nest 4 (char '<' <> commaSep ts_p <> char '>'),
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

ppp :: FilePath -> IO ()
ppp n = do guts        <- readFile n
           let res     =  runParser (whiteSpace >> rwcProg >>= \ p -> whiteSpace >> eof >> return p) () n guts
           case res of
             Left err  -> print err
             Right ast -> print (runFreshM (ppProg ast))

module ReWire.Conversions where

--GHC Package Imports
import GHC
import DynFlags
import MonadUtils
import Bag
import SrcLoc
import HsExpr
import Var (Var(..), varUnique)
import Unique
import Name
import Type
import Outputable
import TyCon
import TcEvidence
import BasicTypes
import FastString
--End GHC Package Imports

--General Imports
import Numeric
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Writer
import Control.Monad.State
import Unbound.LocallyNameless
import qualified Data.Data as Dta
import Debug.Trace
--End General Imports

--ReWire Imports
import ReWire.Core
--End ReWire Imports


type TypesMap = ()

type RWDesugar = StateT Int (ReaderT TypesMap (ReaderT DynFlags Identity))

runRWDesugar :: RWDesugar a -> DynFlags -> a
runRWDesugar ds r = runIdentity $
                    (flip runReaderT) r $
                    (flip runReaderT) () $
                    evalStateT ds 0

dsTcBinds :: LHsBinds Id -> RWDesugar [RWCDefn]
dsTcBinds bag = let binds = map deLoc $ deBag bag
                  in liftM concat $ mapM dsHsBind binds
  where
    deLoc (L _ e) = e
    deBag b = bagToList b

dsTcBindsInner :: LHsBinds Id -> RWDesugar [(Unbound.LocallyNameless.Name RWCExp,RWCExp)]
dsTcBindsInner bag = let binds = map deLoc $ deBag bag
                  in mapM dsHsBind' binds
  where
    deLoc (L _ e) = e
    deBag b = bagToList b

--dsHsPats :: [Pat Id] -> RWDesugar (


type PatDS =  WriterT [RWCExp] RWDesugar

dsHsPats :: [Pat Id] -> RWDesugar ([RWCPat], [RWCExp])
dsHsPats pats = runWriterT (mapM dsHsPat pats)

dsHsPat_ :: Pat Id -> RWDesugar RWCPat
dsHsPat_ pat = do
                  (pat,_) <- runWriterT (dsHsPat pat)
                  return pat


dsHsPat :: Pat Id -> PatDS RWCPat --(GuardPrepends, RWCPat)
dsHsPat (VarPat var) = do
                         ty <- lift $ dsType $ varType var
                         vname <- lift $ ppVar var
                         return $ RWCPatVar (s2n vname)
dsHsPat (WildPat ty) = do
                        newname <- lift $ getLabel
                        ty' <- lift $ dsType ty
                        return $ RWCPatVar (s2n newname)

dsHsPat (LitPat lit) = error "dsHsPat LitPat unfinished!"
dsHsPat (TuplePat lpats _ ty) = do
                                  pats <- mapM (dsHsPat . deLoc) lpats
                                  return $ uncurry_rwpats pats
dsHsPat (NPat lit Nothing eqer) = do
                                    (ty,lit) <- lift $ dsOverLit lit
                                    lbl <- lift $ getLabel
                                    eqer' <- lift $ dsExpr eqer
                                    let eqer'' = RWCApp eqer' (RWCVar ty (s2n lbl))
                                    tell [RWCApp eqer'' (RWCLiteral ty lit)]
                                    return $ RWCPatVar (s2n lbl)
dsHsPat x = error $ "dsHsPat Encountered Incomplete defintion for: " ++ (Dta.showConstr $ Dta.toConstr x)

dsVars :: Type -> [Unbound.LocallyNameless.Name RWCTy] -> RWDesugar ([Unbound.LocallyNameless.Name RWCTy],Type)
dsVars ty vars = case splitForAllTys ty of
                            ([],_)      -> return (vars,ty)
                            (vars',ty')  -> do
                                             vars'' <- mapM ppVar vars'
                                             let bound_vars = map s2n vars''
                                             dsVars ty' $ vars ++ bound_vars


dsCons :: Type -> RWDesugar [RWCConstraint]
dsCons ty = return [] --error "desugar constraints not defined"

dsHsWrap :: HsWrapper -> HsExpr Id ->  RWDesugar RWCExp
dsHsWrap WpHole e = dsExpr e
dsHsWrap (WpCompose (WpEvApp es) (WpTyApp ty)) e = do
                                                      s1 <- ppShowSDoc $ ppr es
                                                      s2 <- ppShowSDoc $ ppr $ ty
                                                      trace ("DEBUG: " ++ s1 ++ "\nDEBUG: " ++ s2) $ dsExpr e
dsHsWrap (WpCompose w1 w2) e = dsHsWrap w2 e -- (dsHsWrap w2)
dsHsWrap (WpCast tc) e  = error "WpCast!"
dsHsWrap (WpEvLam ev) e = error "WpEvLam!"
dsHsWrap (WpEvApp es) e = error "WpEvApp!"
dsHsWrap (WpTyLam tv) e = error "WpTyLam!"
dsHsWrap (WpTyApp ty) e = error "WpTyApp!"
dsHsWrap (WpLet tcev) e = error "WpLet!"



dsHsBind :: HsBind Id -> RWDesugar [RWCDefn]

--dsHsBind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dicts
--                   , abs_exports = export, abs_ev_binds = ev_binds
--                   , abs_binds = binds }) = do
dsHsBind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dicts
                   , abs_exports = [export]
                   , abs_ev_binds = ev_binds, abs_binds = binds }) | ABE { abe_wrap = wrap, abe_poly = global
                   , abe_mono = local, abe_prags = prags } <- export = do
                                                                         flags <- getFlags
                                                                         [(bind_name,expr)] <- dsTcBindsInner binds --for now we just operate on one bind
                                                                         (def_vars,innerty) <- dsVars (varType global) []
                                                                         def_cons <- dsCons $ varType global --Get all constraints in this type
                                                                         def_ty <- dsType innerty --Get the converted type
                                                                         --s <- ppShow $ foralls
                                                                         --error $ show (s, s)
                                                                         return $ [RWCDefn (bind_name) (embed $ setbind
                                                                                                                          def_vars
                                                                                                                          (def_cons, def_ty, expr))]
                   --error "AbsBinds general, not defined yet."
--dsHsBind (PatSynBind{}) = error "dsHsBind panics"

dsHsBind' :: HsBind Id -> RWDesugar (Unbound.LocallyNameless.Name RWCExp ,RWCExp)
dsHsBind' (VarBind { var_id = var, var_rhs = expr, var_inline = inline_regardless }) = error "VarBind not implemented yet"

--Here we move all pattern matching to the right hand side of the function in a case statement
dsHsBind' (FunBind { fun_id = L _ fun, fun_matches = m@(MatchGroup matches match_type)
                  , fun_co_fn = co_fn, fun_tick = tick, fun_infix = inf }) = do
                                                                               fun_name <- ppShow fun
                                                                               ty <- dsType match_type
                                                                               rebound_case <- conv_matchgroup m
                                                                               return (s2n fun_name, rebound_case)
dsHsBind' (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty
                  , pat_ticks = (rhs_tick, var_ticks) }) = error "Patbind not defined yet."

conv_single_match :: Match Id -> RWDesugar (RWCExp,RWCExp) -- (guard, body)
conv_single_match (Match [] mbty grhs) = do
                                           rwcgexp <- conv_guards [] grhs >>= merge_guarded_rwcexp
                                           return rwcgexp


conv_match :: Match Id -> RWDesugar RWCAlt
conv_match (Match lpats mbty grhs) = do
                                       let pats = map deLoc lpats
                                       (pats',exps) <- dsHsPats pats
                                       let final_pat = case pats' of
                                            []   -> error "Can't make an alt with no Patterns.  dsHsBind' FunBind" --Build alt with no match?
                                            _    -> uncurry_rwpats pats'

                                       rwcgexp <- conv_guards exps grhs >>= merge_guarded_rwcexp

                                       return $ RWCAlt $ bind final_pat rwcgexp

merge_guarded_rwcexp :: [(RWCExp,RWCExp)] -> RWDesugar (RWCExp,RWCExp)
merge_guarded_rwcexp [exp] = return exp
merge_guarded_rwcexp _ = error "Multiple guarded equations not supported yet."

conv_guards :: [RWCExp] -> GRHSs Id -> RWDesugar [(RWCExp,RWCExp)] --(Guard,Body)
conv_guards exps (GRHSs {grhssGRHSs=lguards, grhssLocalBinds=wclause}) = do --single guard
                                                                     let guards = map deLoc lguards
                                                                     mapM (conv_grhs exps) guards

conv_grhs :: [RWCExp] -> GRHS Id -> RWDesugar (RWCExp,RWCExp) --Expressions to prepend -> (Guard,Body)
conv_grhs [] (GRHS [] lexpr) = do
                             let expr  = deLoc lexpr
                             let guard = true_expr -- Simply true
                             expr' <- dsLExpr lexpr
                             return (guard,expr')
conv_grhs [pexpr] (GRHS [] lexpr) = do
                             let expr  = deLoc lexpr
                             let guard = pexpr -- Simply true, FIXME  What goes here if the guard is true?
                             expr' <- dsLExpr lexpr
                             return (guard,expr')
conv_grhs (e:es) (GRHS [] lexpr) = do
                             let expr  = deLoc lexpr
                             let guard = foldl (\acc exp -> (RWCApp (RWCApp and_fun exp) acc)) e es
                             expr' <- dsLExpr lexpr
                             return (guard,expr')
    where
      --and_fun = RWCVar (RWCTyVar name "

conv_grhs _ _ = error "guards (and NPats) aren't supported yet"

conv_guard_stmt :: LStmt Id -> RWDesugar RWCExp
conv_guard_stmt stmt = error "conv_guard_stmt not defined"


matchTypes :: MatchGroup Id -> RWDesugar ([Type])
matchTypes (MatchGroup [] ty) = return ([])
matchTypes (MatchGroup matches ty) = do
                                       let (Match pats _ grhs) = deLoc $ head matches
                                           (GRHS _ lexpr) = deLoc $ head $ grhssGRHSs grhs
                                       --rhs_ty <- hsExpTy $ deLoc lexpr --extract the RHS type from the expression that resides on the RHS
                                       ltypes <- mapM (hsPatTy . deLoc) pats -- If we have stuff on the lhs, the rhs type differs
                                       return (ltypes)


conv_matchgroup :: MatchGroup Id -> RWDesugar RWCExp
conv_matchgroup m@(MatchGroup matches match_type) = do
                                                      let arity = matchGroupArity m
                                                      case arity of
                                                             0 -> do
                                                                    let [match] = matches
                                                                    gexp <- conv_single_match $ deLoc match
                                                                    let alt_match = RWCAlt $ bind true_pat gexp
                                                                        case_expr = RWCCase true_expr [alt_match]
                                                                    --trace (show "OH NO") $ error "Zero Arity functions not working yet ;-)"
                                                                    return case_expr
                                                             _ -> do
                                                                    (pat_tys) <- matchTypes m
                                                                    labels <- replicateM arity getLabel
                                                                    --Match up our labels we created to the LHS pattern types they correspond to
                                                                    expr_tys' <- mapM dsType pat_tys
                                                                    --end_ty' <- dsType end_ty
                                                                    alts <- mapM (conv_match . deLoc) matches
                                                                    let vtypes = zip labels expr_tys'
                                                                        lvars = map (\(lbl,ty) -> RWCVar ty (s2n lbl)) vtypes
                                                                        case_tuple = uncurry_rwexps lvars
                                                                        case_expr  = RWCCase case_tuple alts
                                                                        fun_tycon  = RWCTyCon "(->)"
                                                                        rebound_case = foldl (\acc (lbl,ty)-> RWCLam (bind (s2n lbl) acc)) case_expr vtypes
                                                                    --let label_pats =
                                                                    return rebound_case




uncurry_rwexps :: [RWCExp] -> RWCExp
uncurry_rwexps [] = error "Can't uncurry a list of no expressions.  dsHsBind' FunBind"
uncurry_rwexps [exp] = exp
uncurry_rwexps exps  = let commas = concat $ take (length exps - 1) $ repeat ","
                           tycon = "(" ++ commas ++ ")"
                                in foldl (\acc item -> RWCApp acc item) (RWCCon (RWCTyCon tycon) tycon) exps


--Combining argument patterns into a tuple enables us to match over things easily
uncurry_rwpats :: [RWCPat] -> RWCPat
uncurry_rwpats []    = error "Can't uncurry something with no arguments. dsHsBind' FunBind"
uncurry_rwpats [pat] = pat -- uncurrying one thing just gets you that thing again
uncurry_rwpats pats  = let commas = concat $ take (length pats - 1) $ repeat ","
                           tycon  = "(" ++ commas ++ ")"
                                in RWCPatCon tycon pats

hsPatTy :: Pat Id -> RWDesugar Type
hsPatTy (VarPat var) = return $ varType var
hsPatTy (TuplePat _ _ ty) = return ty
hsPatTy (WildPat ty) = return ty
hsPatTy (NPat lit _ _) = return $ ol_type lit

hsPatTy thing = error $ "hsPatTy encountered unimplemented: " ++ (show $ Dta.toConstr thing)

hsExpTy :: HsExpr Id -> RWDesugar Type
hsExpTy (HsVar var) = do
                        let n = (nameOccName . getName) var
                        n' <- ppShowSDoc $ ppr n
                        return $ trace ("DEBUG NAME: " ++ n') $ varType var
hsExpTy thing = error $ "hsExpTy encountered unimplemented: " ++ (show $ Dta.toConstr thing)


--This type can be a lot more general, but I'm fixing it here
--so I remember what it'll do.
--dsLExpr :: GenLocated SrcSpan (HsExpr a) -> b
dsLExpr :: GenLocated SrcSpan (HsExpr Id) -> RWDesugar RWCExp
dsLExpr (L loc e) = dsExpr e

dsExpr :: HsExpr Id -> RWDesugar RWCExp
dsExpr (HsPar e)               = dsLExpr e
dsExpr (ExprWithTySigOut e _)  = dsLExpr e
dsExpr (HsVar var)             = do
                                   vname <- ppVar var
                                   ty    <- dsType $ varType var
                                   return $ RWCVar ty (s2n vname)
dsExpr (HsIPVar _)             = error "HsIPVar panics in GHC."
dsExpr (HsLit lit)             = error "dsExpr: HsLit encountered."
  where
   {-
    hl (HsChar chr)         = RWCLitChar chr
    hl (HsCharPrim chr)     = RWCLitChar chr
    hl (HsString _)         = error "Strings Literals not supported."
    hl (HsStringPrim _)     = error "Strings Literals not supported."
    hl (HsInt int)          = RWCLitInteger int
    hl (HsIntPrim int)      = RWCLitInteger int
    hl (HsWordPrim int)     = RWCLitInteger int
    hl (HsInt64Prim int)    = RWCLitInteger int
    hl (HsWord64Prim int)   = RWCLitInteger int
    hl (HsInteger int t )   = RWCLitInteger int
    hl (HsRat float ty  )   = RWCLitFloat (fromRat $ fl_value $ float)
    hl (HsFloatPrim float)  = RWCLitFloat (fromRat $ fl_value $ float)
    hl (HsDoublePrim float) = RWCLitFloat (fromRat $ fl_value $ float)
    -}

    hl_ann (HsInteger int ty) = do
                                  ty' <- dsType ty
                                  return $ RWCLiteral ty' (RWCLitInteger int)
    hl_ann (HsRat float ty) = do
                               ty' <- dsType ty
                               return $ RWCLiteral ty' $ RWCLitFloat $ fromRat $ fl_value $ float
    hl_ann (HsChar chr) = do
                            let ty' = RWCTyCon "GHC.Prim.Char"
                            return $ RWCLiteral ty' $ RWCLitChar chr

dsExpr (HsOverLit lit)         = do
                                   (ty,lit') <- dsOverLit lit
                                   return $ RWCLiteral ty lit'
dsExpr w@(HsWrap co_fn e)        = dsHsWrap co_fn e
dsExpr (NegApp expr neg_expr)  = error "NegApp not implemented yet."
dsExpr (HsLam a_Match)         = conv_matchgroup a_Match
dsExpr (HsLamCase arg matches) = error "HSLamCase not implemented yet."
dsExpr (HsApp fun arg)         = do
                                   fun' <- dsLExpr fun
                                   arg' <- dsLExpr arg
                                   return $ RWCApp fun' arg'
--dsExpr (HsUnboundVar _)        = error "HsUnboundVar panics in GHC."
dsExpr (OpApp e1 op _ e2)      = do
                                 lexpr <- dsLExpr e1
                                 rexpr <- dsLExpr e2
                                 oper  <- dsLExpr op
                                 return (RWCApp (RWCApp oper lexpr) rexpr)
dsExpr (SectionL expr op)      = error "SectionL not implemented yet."
dsExpr (SectionR op expr)      = error "SectionR not implemented yet."
dsExpr (ExplicitTuple tup_args boxity) = do
                                           exprs <- mapM (dsExpr . deLoc . prjprs) tup_args
                                           return $ uncurry_rwexps exprs

  where
    prjprs (Present lhs) = lhs
    prjprs (Missing _)   = error "Missing explicit tuple arguments unsupported."
dsExpr (HsSCC cc expr@(L log _)) = error "HsSCC not implemented yet."
dsExpr (HsCoreAnn _ expr) = error "HsCoreAnn not implemented yet."
dsExpr (HsCase discrim matches) = error "HsCase not implemented yet."
dsExpr (HsLet binds body) = error "HsLet not implemented yet."
dsExpr (HsDo ListComp stmts res_ty) = error "HsDo ListComp not implemented yet."
dsExpr (HsDo PArrComp stmts _) = error "HsDo PArrComp not implemented yet."
dsExpr (HsDo DoExpr stmts _) = error "HsDo DoExpr not implemented yet."
  where
    prj (BindStmt _ _ _ _) = undefined
--dsExpr (HsDo GhciStmtCtxt stmts _) = error "HsDo GhciStmtCtxt not implemented yet."
dsExpr (HsDo MDoExpr stmts _) = error "HsDo MDoExpr not implemented yet."
dsExpr (HsDo MonadComp stmts _) = error "HsDo MonadComp not implemented yet."
dsExpr (HsDo ArrowExpr _ _) = error "HsDo ArrowExpr not implemented yet."
dsExpr (HsDo GhciStmt _ _) = error "HsDo GhciStmt not implemented.  Not in Ghci."
dsExpr (HsDo (PatGuard _) _ _) = error "HsDo PatGuard not implemented."
dsExpr (HsDo (ParStmtCtxt _) _ _) = error "HsDo ParStmtCtxt not implemented."
dsExpr (HsDo (TransStmtCtxt _) _ _) = error "HsDo TransStmtCtxt not implemented."
dsExpr (HsIf mb_fun guard_expr then_expr else_expr) = error "HsIf not implemented yet."
dsExpr (HsMultiIf res_ty alts) = error "HsMultiIf not implemented yet."
dsExpr (ExplicitList _ _) = error "ExplicitList not implemented yet."
dsExpr (ExplicitPArr ty []) = error "ExplicitPArr ty [] not implemented yet."
dsExpr (ExplicitPArr ty xs) = error "ExplicitPArr ty xs not implemented yet."
dsExpr (ArithSeq _ _) = error "ArithSeq not implemented yet."
dsExpr (PArrSeq expr (FromTo from to)) = error "PArrSeq fromTo not implemented yet."
dsExpr (PArrSeq expr (FromThenTo from thn to)) = error "PArrSeq FromThenTo not implemented yet."
dsExpr (PArrSeq _ _) = error "Panicking on an infinite parallel array."
dsExpr (RecordCon (L _ data_con_id) con_expr rbinds) = error "RecordCon not implemented."
dsExpr expr@(RecordUpd record_expr (HsRecFields {rec_flds = fields})
                       cons_to_upd in_inst_tys out_inst_tys) = error "Record Update not implemented."
--dsExpr (HsRnBracketOut _ _) = error "Panicking on HsRnBracketOut"
--dsExpr (HsTcBracketOut _ _) = error "Panicking on HsTcBracketOut, not in Ghci"
dsExpr (HsBracketOut _ _) = error "HsBracketOut not implemented."
dsExpr (HsSpliceE _) = error "Panicking on HsSpliceE."
dsExpr (HsProc pat cmd) = error "HsProc not implemented yet."
dsExpr (HsTick tickish e) = error "HsTickish not implemented yet."
dsExpr (HsBinTick ixT ixF e) = error "HsBinTick not implemented yet."
dsExpr (ExprWithTySig {}) = error "Panicking on ExprWithTySig"
dsExpr (HsBracket {}) = error "Panicking on HsBracket"
dsExpr (HsQuasiQuoteE {})  = error "Panicking on HsQuasiQuoteE"
dsExpr (HsArrApp      {})  = error "Panicking on HsArrApp"
dsExpr (HsArrForm     {})  = error "Panicking on HsArrForm"
dsExpr (HsTickPragma  {})  = error "Panicking on HsTickPragma"
dsExpr (EWildPat      {})  = error "Panicking on EWildPat"
dsExpr (EAsPat        {})  = error "Panicking on EAsPat"
dsExpr (EViewPat      {})  = error "Panicking on EViewPat"
dsExpr (ELazyPat      {})  = error "Panicking on ELazyPat"
dsExpr (HsType        {})  = error "Panicking on HsType"

anonLambdaWrap :: String -> RWCExp -> RWCTy -> RWCExp
anonLambdaWrap lbl exp ty = let bound = bind (s2n lbl) exp
                          in RWCLam bound

dsOverLit :: HsOverLit id -> RWDesugar (RWCTy, RWCLit)
dsOverLit lit = do
                                   ty <- dsType $ ol_type lit
                                   let lit' = case (ol_val lit) of
                                                    (HsIntegral int)   -> RWCLitInteger int
                                                    (HsFractional dbl) -> RWCLitFloat $ fromRat $ fl_value $ dbl
                                                    (HsIsString   str) -> let str' = unpackFS str
                                                                           in if (length str') == 1
                                                                              then RWCLitChar $ head str'
                                                                              else error "String literals are not supported."
                                   return (ty,lit')

--GHC Type to RWCTy
dsType :: Type -> RWDesugar RWCTy
dsType ty = case repSplitAppTy_maybe ty of
                      Nothing -> do
                                   conv
                      Just (left,right) -> do
                                              s <- ppShow ty
                                              left'  <- dsType left
                                              right' <- dsType right
                                              return $ RWCTyApp left' right'
      where
        conv = case getTyVar_maybe ty of
                      Just var -> do
                                    var' <- ppVar var
                                    --var' <- ppShowSDoc $ pprUnique $ Var.varUnique var
                                    return $ RWCTyVar $ s2n var'
                      Nothing  -> case splitFunTy_maybe ty of
                                         Just fun -> error "FunType encountered"
                                         Nothing  -> case splitTyConApp_maybe ty of
                                                              Just (tycon,tys) -> do
                                                                                   tycon' <- ppShow $ tyConName tycon
                                                                                   return (RWCTyCon tycon')
                                                              Nothing    -> case splitForAllTy_maybe ty of
                                                                                  Just (frall,ty') -> dsType ty'
                                                                                  Nothing    -> case isNumLitTy ty of
                                                                                                      Just i  -> error "NumLit Type encountered."
                                                                                                      Nothing -> case isStrLitTy ty of
                                                                                                                    Just str -> error "String Literal encountered"
                                                                                                                    Nothing  -> error "Given Type unsupported"


--Utility Functions
getFlags :: RWDesugar DynFlags
getFlags = lift $ lift ask

ppShow :: Outputable a => a -> RWDesugar String
ppShow op = do
              flags <- getFlags
              return $ showPpr flags op

ppShowSDoc :: SDoc -> RWDesugar String
ppShowSDoc sdoc = do
                    flags <- getFlags
                    return $ showSDoc flags sdoc

ppDebug :: SDoc -> RWDesugar String
ppDebug sdoc = do
                 flags <- getFlags
                 return $ showSDocDebug flags sdoc

ppVar :: Var -> RWDesugar String
ppVar var = ppShowSDoc $ pprOccName $ occName $ Var.varName var


getLabel :: RWDesugar String
getLabel = do
             counter <- get
             put (counter+1)
             return $ "dsX_" ++ (show counter)

deLoc (L _ e) = e

bool_type = RWCTyCon "GHC.Types.Bool"

true_pat   = RWCPatCon "GHC.Types.True" []
false_pat  = RWCPatCon "GHC.Types.False" []

true_expr   = RWCCon bool_type "GHC.Types.True"
false_expr  = RWCCon bool_type "GHC.Types.False"

x `farrow` y = RWCTyApp (RWCTyApp (RWCTyCon "(->)") x) y

bool_op_ty = bool_type `farrow` bool_type `farrow` bool_type
and_fun    = RWCVar bool_op_ty (s2n "(&&)")


dummy_ty    = RWCTyCon "DUMMYTYPE"

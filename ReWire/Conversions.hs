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
--End GHC Package Imports

--General Imports
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Unbound.LocallyNameless
import Debug.Trace
--End General Imports

--ReWire Imports
import ReWire.Core 
--End ReWire Imports



type RWDesugar a = StateT Int (ReaderT DynFlags Identity) a

runRWDesugar :: RWDesugar a -> DynFlags -> a
runRWDesugar ds r = runIdentity $
                    (flip runReaderT) r $
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

dsHsPat :: Pat Id -> RWDesugar RWCPat
dsHsPat (VarPat var) = do
                         ty <- dsType $ varType var
                         vname <- ppUniqueVar var
                         return $ RWCPatVar (embed ty) (s2n vname)
dsHsPat (LitPat lit) = error "dsHsPat LitPat unfinished!" 

dsVars :: Type -> [Unbound.LocallyNameless.Name RWCTy] -> RWDesugar ([Unbound.LocallyNameless.Name RWCTy],Type)
dsVars ty vars = case splitForAllTys ty of
                            ([],_)      -> return (vars,ty)
                            (vars',ty')  -> do
                                             vars'' <- mapM (ppShowSDoc . pprOccName . nameOccName . Var.varName) vars'
                                             let bound_vars = map s2n vars'' 
                                             dsVars ty' $ vars ++ bound_vars 


dsCons :: Type -> RWDesugar [RWCConstraint]
dsCons ty = return [] -- error "desugar constraints not defined"


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
                                                                         return $ [RWCDefn (bind_name) (embed $ bind 
                                                                                                                          def_vars 
                                                                                                                          (def_cons, def_ty, expr))] 
                   --error "AbsBinds general, not defined yet."
--dsHsBind (PatSynBind{}) = error "dsHsBind panics"

dsHsBind' :: HsBind Id -> RWDesugar (Unbound.LocallyNameless.Name RWCExp ,RWCExp)
dsHsBind' (VarBind { var_id = var, var_rhs = expr, var_inline = inline_regardless }) = error "VarBind not implemented yet"

--Here we move all pattern matching to the right hand side of the function in a case statement
dsHsBind' (FunBind { fun_id = L _ fun, fun_matches = m@(MatchGroup matches match_type) 
                  , fun_co_fn = co_fn, fun_tick = tick, fun_infix = inf }) = do 
                                                                               --Get the function name 
                                                                               fun_name <- ppShow fun
                                                                               --Get the function's type
                                                                               ty <- dsType match_type
                                                                               --Arity to determine if we need to rebuild anything on the RHS, zero means 
                                                                               --you're dealing with something like x = ...
                                                                               --Anything else is something like f x ... = ...
                                                                               let arity = matchGroupArity m
                                                                               case arity of
                                                                                     0 -> error "Zero Arity functions not working yet ;-)"
                                                                                     _ -> do
                                                                                             let (pat_tys,end_ty) = matchTypes m
                                                                                             labels <- replicateM arity getLabel
                                                                                             --Match up our labels we created to the LHS pattern types they correspond to
                                                                                             expr_tys' <- mapM dsType pat_tys
                                                                                             end_ty' <- dsType end_ty
                                                                                             alts <- mapM (conv_match . deLoc) matches
                                                                                             let vtypes = zip labels expr_tys'
                                                                                                 lvars = map (\(lbl,ty) -> RWCVar ty (s2n lbl)) vtypes 
                                                                                                 case_tuple = uncurry_rwexps lvars
                                                                                                 case_expr  = RWCCase end_ty' case_tuple alts
                                                                                                 fun_tycon  = RWCTyCon "(->)"
                                                                                                 rebound_case = foldl (\acc (lbl,ty)-> RWCLam (RWCTyApp (RWCTyApp fun_tycon (ty) ) (exp_ty acc))
                                                                                                                                               (bind (s2n lbl,embed ty) acc)) case_expr vtypes
                                                                                             --let label_pats = 
                                                                                             return (s2n fun_name,
                                                                                                     rebound_case) 
      where
        conv_match :: Match Id -> RWDesugar RWCAlt
        conv_match (Match lpats mbty grhs) = do
                                                let pats = map deLoc lpats
                                                pats' <- mapM dsHsPat pats
                                                let final_pat = case pats' of
                                                         []   -> error "Can't make an alt with no Patterns.  dsHsBind' FunBind" --Build alt with no match?
                                                         _    -> uncurry_rwpats pats'

                                                rwcgexp <- conv_guards grhs >>= merge_guarded_rwcexp

                                                return $ RWCAlt $ bind final_pat rwcgexp

        merge_guarded_rwcexp :: [(RWCExp,RWCExp)] -> RWDesugar (RWCExp,RWCExp)
        merge_guarded_rwcexp [exp] = return exp
        merge_guarded_rwcexp _ = error "Multiple guarded equations not supported yet."

        conv_guards :: GRHSs Id -> RWDesugar [(RWCExp,RWCExp)] --(Guard,Body)
        conv_guards (GRHSs {grhssGRHSs=lguards, grhssLocalBinds=wclause}) = do --single guard
                                                                              let guards = map deLoc lguards
                                                                              mapM conv_grhs guards
        conv_grhs :: GRHS Id -> RWDesugar (RWCExp,RWCExp) --(Guard,Body)
        conv_grhs (GRHS [] lexpr) = do
                                      let expr  = deLoc lexpr
                                      let guard = RWCCon (RWCTyCon "GHC.Types.Bool") "GHC.Types.Bool.True" -- Simply true, FIXME  What goes here if the guard is true?
                                      expr' <- dsLExpr lexpr
                                      return (guard,expr')

        conv_grhs _ = error "guards aren't supported yet"

        conv_guard_stmt :: LStmt Id -> RWDesugar RWCExp
        conv_guard_stmt stmt = error "conv_guard_stmt not defined"


        --Combining argument patterns into a tuple enables us to match over things easily
        uncurry_rwpats :: [RWCPat] -> RWCPat
        uncurry_rwpats []    = error "Can't uncurry something with no arguments. dsHsBind' FunBind"
        uncurry_rwpats [pat] = pat -- uncurrying one thing just gets you that thing again
        uncurry_rwpats pats  = let commas = concat $ take (length pats - 1) $ repeat ","
                                   tycon  = "(" ++ commas ++ ")"
                                   tys     = map pat_ty pats
                                   rwcty   = foldl (\acc item -> RWCTyApp acc item) (RWCTyCon tycon) tys
                                in RWCPatCon (embed rwcty) tycon pats

        pat_ty (RWCPatCon (Embed ty) _ _) = ty
        pat_ty (RWCPatLiteral (Embed ty) _) = ty
        pat_ty (RWCPatVar (Embed ty) _) = ty


        uncurry_rwexps :: [RWCExp] -> RWCExp
        uncurry_rwexps [] = error "Can't uncurry a list of no expressions.  dsHsBind' FunBind"
        uncurry_rwexps [exp] = exp
        uncurry_rwexps exps  = let commas = concat $ take (length exps - 1) $ repeat ","
                                   tycon = "(" ++ commas ++ ")"
                                   tys  = map exp_ty exps
                                   rwcty = foldl (\acc item -> RWCTyApp acc item) (RWCTyCon tycon) tys
                                in foldl (\acc item -> RWCApp (RWCTyApp (exp_ty acc) (exp_ty item)) acc item) (RWCCon (RWCTyCon tycon) tycon) exps

        exp_ty (RWCApp ty _ _) = ty
        exp_ty (RWCLam ty _) = ty
        exp_ty (RWCVar ty _) = ty
        exp_ty (RWCCon ty _) = ty
        exp_ty (RWCLiteral ty _) = ty
        exp_ty (RWCCase ty _ _) = ty


        matchTypes :: MatchGroup Id -> ([Type],Type)
        matchTypes (MatchGroup [] ty) = ([],ty)
        matchTypes (MatchGroup matches ty) = let (Match pats _ grhs) = deLoc $ head matches
                                                 ltypes           = map (hsPatTy . deLoc) pats -- If we have stuff on the lhs, the rhs type differs
                                                 (GRHS _ lexpr) = deLoc $ head $ grhssGRHSs grhs 
                                                 rhs_ty = hsExpTy $ deLoc lexpr --extract the RHS type from the expression that resides on the RHS
                                              in (ltypes,rhs_ty)


dsHsBind' (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty
                  , pat_ticks = (rhs_tick, var_ticks) }) = error "Patbind not defined yet."



hsPatTy :: Pat Id -> Type
hsPatTy (VarPat var) = varType var
hsPatTy _ = error "hsPatTy encountered unimplemented case."

hsExpTy :: HsExpr Id -> Type
hsExpTy (HsVar var) = varType var
hsExpTy _ = error "hsExpTy encountered unimplemented case."


--This type can be a lot more general, but I'm fixing it here
--so I remember what it'll do.
--dsLExpr :: GenLocated SrcSpan (HsExpr a) -> b
dsLExpr :: GenLocated SrcSpan (HsExpr Id) -> RWDesugar RWCExp 
dsLExpr (L loc e) = dsExpr e

dsExpr :: HsExpr Id -> RWDesugar RWCExp
dsExpr (HsPar e)               = dsLExpr e
dsExpr (ExprWithTySigOut e _)  = dsLExpr e
dsExpr (HsVar var)             = do
                                   vname <- ppUniqueVar var
                                   ty    <- dsType $ varType var
                                   return $ RWCVar ty (s2n vname)
dsExpr (HsIPVar _)             = error "HsIPVar panics in GHC."
dsExpr (HsLit lit)             = error "Lits not implemented yet."
dsExpr (HsOverLit lit)         = error "Lits not implemented yet."
dsExpr (HsWrap co_fn e)        = error "HsWrap not implemented yet."
dsExpr (NegApp expr neg_expr)  = error "NegApp not implemented yet."
dsExpr (HsLam a_Match)         = error "HSLam not implemented yet."
dsExpr (HsLamCase arg matches) = error "HSLamCase not implemented yet."
dsExpr (HsApp fun arg)         = error "HsApp not implemented yet"
--dsExpr (HsUnboundVar _)        = error "HsUnboundVar panics in GHC."
dsExpr (OpApp e1 op _ e2)      = error "OpApp not implemented yet."
dsExpr (SectionL expr op)      = error "SectionL not implemented yet."
dsExpr (SectionR op expr)      = error "SectionR not implemented yet."
dsExpr (ExplicitTuple tup_args boxity) = error "ExplicitTuple not implemented yet."
dsExpr (HsSCC cc expr@(L log _)) = error "HsSCC not implemented yet."
dsExpr (HsCoreAnn _ expr) = error "HsCoreAnn not implemented yet."
dsExpr (HsCase discrim matches) = error "HsCase not implemented yet."
dsExpr (HsLet binds body) = error "HsLet not implemented yet."
dsExpr (HsDo ListComp stmts res_ty) = error "HsDo ListComp not implemented yet."
dsExpr (HsDo PArrComp stmts _) = error "HsDo PArrComp not implemented yet."
dsExpr (HsDo DoExpr stmts _) = error "HsDo DoExpr not implemented yet."
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
anonLambdaWrap lbl exp ty = let bound = bind (s2n lbl,embed ty) exp
                          in RWCLam (extendType ty) bound
    where
      extendType :: RWCTy -> RWCTy
      extendType ty = RWCTyApp ty (eType exp)  


eType :: RWCExp -> RWCTy
eType (RWCApp t _ _)   = t
eType (RWCLam t _)     = t
eType (RWCVar t _)     = t
eType (RWCCon t _)     = t
eType (RWCLiteral t _) = t
eType (RWCCase t _ _)  = t


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
                                    var' <- ppShowSDoc $ pprOccName $ nameOccName $ Var.varName var
                                    --var' <- ppShowSDoc $ pprUnique $ Var.varUnique var
                                    return $ RWCTyVar $ s2n var' 
                      Nothing  -> case splitFunTy_maybe ty of
                                         Just fun -> error "FunType encountered" 
                                         Nothing  -> case splitTyConApp_maybe ty of
                                                              Just (tycon,tys) -> do
                                                                                   tycon' <- ppShow $ tyConName tycon 
                                                                                   return (RWCTyCon tycon') 
                                                              Nothing    -> case splitForAllTy_maybe ty of
                                                                                  Just frall -> error "Forall Type encountered" 
                                                                                  Nothing    -> case isNumLitTy ty of
                                                                                                      Just i  -> error "NumLit Type encountered."
                                                                                                      Nothing -> case isStrLitTy ty of
                                                                                                                    Just str -> error "String Literal encountered"
                                                                                                                    Nothing  -> error "Given Type unsupported"
                    

--Utility Functions
getFlags :: RWDesugar DynFlags
getFlags = ask

ppShow :: Outputable a => a -> RWDesugar String
ppShow op = do
              flags <- getFlags
              return $ showPpr flags op

ppShowSDoc :: SDoc -> RWDesugar String
ppShowSDoc sdoc = do
                    flags <- getFlags
                    return $ showSDoc flags sdoc

ppUniqueVar :: Var -> RWDesugar String
ppUniqueVar var = ppShowSDoc $ pprUnique $ varUnique var

getLabel :: RWDesugar String
getLabel = do
             counter <- get
             put (counter+1)
             return $ "dsX_" ++ (show counter)

deLoc (L _ e) = e

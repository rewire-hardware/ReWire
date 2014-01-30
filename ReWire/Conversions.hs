module ReWire.Conversions where

--GHC Package Imports
import GHC
import DynFlags
import MonadUtils
import Bag
import SrcLoc
import HsExpr
import Var (Var(..))
import Name
import Type
import Outputable
--End GHC Package Imports

--General Imports
import Control.Monad.Identity
import Control.Monad.Reader
import Unbound.LocallyNameless
--End General Imports

--ReWire Imports
import ReWire.Core 
--End ReWire Imports



type RWDesugar a = ReaderT DynFlags Identity a

runRWDesugar :: RWDesugar a -> DynFlags -> a
runRWDesugar ds r = runIdentity $
                    runReaderT ds r

dsTcBinds :: LHsBinds Id -> RWDesugar [RWCDefn]
dsTcBinds bag = let binds = map deLoc $ deBag bag 
                  in mapM dsHsBind binds 
  where
    deLoc (L _ e) = e
    deBag b = bagToList b


dsHsBind :: HsBind Id -> RWDesugar RWCDefn
dsHsBind (VarBind { var_id = var, var_rhs = expr, var_inline = inline_regardless }) = error "VarBind not implemented yet"
dsHsBind (FunBind { fun_id = L _ fun, fun_matches = matches
                  , fun_co_fn = co_fn, fun_tick = tick, fun_infix = inf }) = error "FunBind not defined yet."
dsHsBind (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty
                  , pat_ticks = (rhs_tick, var_ticks) }) = error "Patbind not defined yet."
dsHsBind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dicts
                   , abs_exports = [export]
                   , abs_ev_binds = ev_binds, abs_binds = binds }) | ABE { abe_wrap = wrap, abe_poly = global
                   , abe_mono = local, abe_prags = prags } <- export = error "AbsBinds Single Export, not defined yet."

dsHsBind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dicts
                   , abs_exports = exports, abs_ev_binds = ev_binds
                   , abs_binds = binds }) = error "AbsBinds general, not defined yet."
--dsHsBind (PatSynBind{}) = error "dsHsBind panics"




--This type can be a lot more general, but I'm fixing it here
--so I remember what it'll do.
--dsLExpr :: GenLocated SrcSpan (HsExpr a) -> b
dsLExpr :: GenLocated SrcSpan (HsExpr Id) -> RWDesugar RWCExp 
dsLExpr (L loc e) = dsExpr e

dsExpr :: HsExpr Id -> RWDesugar RWCExp
dsExpr (HsPar e)               = dsLExpr e
dsExpr (ExprWithTySigOut e _)  = dsLExpr e
dsExpr (HsVar var)             = error "HsVar not implemented yet."
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


--GHC Type to RWCTy
dsType :: Type -> RWDesugar RWCTy
dsType ty = case repSplitAppTy_maybe ty of 
                      Nothing -> undefined
                      Just (left,right) -> do
                                              left'  <- dsType left
                                              right' <- dsType right
                                              return $ RWCTyApp left' right'

--Utility Functions
getFlags :: RWDesugar DynFlags
getFlags = ask

ppShow :: Outputable a => a -> RWDesugar String
ppShow op = do
              flags <- getFlags
              return $ showPpr flags op

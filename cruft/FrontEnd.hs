module ReWire.FrontEnd where

import qualified ReWire.Conversions as Conv
import qualified ReWire.CorePP as PP
import Unbound.LocallyNameless
import Text.PrettyPrint
import System.Environment (getArgs)
import System.Exit

import GHC
import GHC.Paths ( libdir )
import DynFlags
import Outputable
import System.IO
import MonadUtils
import Bag
import SrcLoc
import StaticFlags
import Control.Monad (when)
import Data.IORef



main = do
    args  <- getArgs
    fname <- case args of
                    [] -> do
                            putStrLn "Please provide a filename."
                            exitFailure
                    xs -> return $ head xs
    ready <- readIORef v_opt_C_ready
    when (not ready) $ parseStaticFlags [noLoc "-dppr-debug"] >> return ()
    binds <- (defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
              runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                let dflags' = dflags {hscTarget = HscInterpreted,
                                      ghcLink = LinkInMemory,
                                      ghcMode = CompManager}
                setSessionDynFlags dflags'
                target <- guessTarget fname Nothing
                setTargets [target]
                load LoadAllTargets
                g <- getModuleGraph
                p <- parseModule $ head g
                tc <- typecheckModule p
                let pm = tm_parsed_module tc
                let src = pm_parsed_source pm
                let tc_src = tm_typechecked_source tc
                liftIO $ putStrLn "==="
                liftIO $ printForUser dflags' stdout neverQualify (ppr tc_src)
                liftIO $ putStrLn "==="
                let binds = Conv.runRWDesugar (Conv.dsTcBinds tc_src) dflags'
                return binds)

    let defs = runFreshM $ mapM PP.ppDefn binds
    print defs
    return ()

output dflags' s = printForC dflags' stdout (ppr s)


deGenLocate :: GenLocated l e -> e
deGenLocate (L _ e) = e

getBinds :: TypecheckedSource -> [HsBindLR Id Id]
getBinds t = map deGenLocate $ bagToList t

bindtype :: HsBindLR idL idR -> String
bindtype (FunBind _ _ _ _ _ _) = "Fun"
bindtype (PatBind _ _ _ _ _) = "Pat"
bindtype (VarBind _ _ _) = "Var"
bindtype (AbsBinds _ _ _ _ _) = "Abs"
--bindtype _ = "?"

{-
dsLHsBind :: (Origin, LHsBind Id) -> DsM (OrdList (Id,CoreExpr))
dsLHsBind (origin, L loc bind)
  = handleWarnings $ putSrcSpanDs loc $ dsHsBind bind
  where
    handleWarnings = if isGenerated origin
                     then discardWarningsDs
                     else id
dsHsBind :: HsBind Id -> DsM (OrdList (Id,CoreExpr))

dsHsBind (VarBind { var_id = var, var_rhs = expr, var_inline = inline_regardless })
  = do  { dflags <- getDynFlags
        ; core_expr <- dsLExpr expr

          -- Dictionary bindings are always VarBinds,
          -- so we only need do this here
        ; let var' | inline_regardless = var `setIdUnfolding` mkCompulsoryUnfolding core_expr
             | otherwise         = var

        ; return (unitOL (makeCorePair dflags var' False 0 core_expr)) }

dsHsBind (FunBind { fun_id = L _ fun, fun_matches = matches
                  , fun_co_fn = co_fn, fun_tick = tick
                  , fun_infix = inf })
 = do { dflags <- getDynFlags
        ; (args, body) <- matchWrapper (FunRhs (idName fun) inf) matches
        ; let body' = mkOptTickBox tick body
        ; rhs <- dsHsWrapper co_fn (mkLams args body')
        ; {- pprTrace "dsHsBind" (ppr fun <+> ppr (idInlinePragma fun)) $ -}
           return (unitOL (makeCorePair dflags fun False 0 rhs)) }

dsHsBind (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty
                  , pat_ticks = (rhs_tick, var_ticks) })
  = do  { body_expr <- dsGuarded grhss ty
        ; let body' = mkOptTickBox rhs_tick body_expr
        ; sel_binds <- mkSelectorBinds var_ticks pat body'
    -- We silently ignore inline pragmas; no makeCorePair
    -- Not so cool, but really doesn't matter
    ; return (toOL sel_binds) }

  -- A common case: one exported variable
  -- Non-recursive bindings come through this way
  -- So do self-recursive bindings, and recursive bindings
  -- that have been chopped up with type signatures
dsHsBind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dicts
                   , abs_exports = [export]
                   , abs_ev_binds = ev_binds, abs_binds = binds })
  | ABE { abe_wrap = wrap, abe_poly = global
        , abe_mono = local, abe_prags = prags } <- export
  = do  { dflags <- getDynFlags
        ; bind_prs    <- ds_lhs_binds binds
  ; let core_bind = Rec (fromOL bind_prs)
        ; ds_binds <- dsTcEvBinds ev_binds
        ; rhs <- dsHsWrapper wrap $  -- Usually the identity
          mkLams tyvars $ mkLams dicts $
                      mkCoreLets ds_binds $
                            Let core_bind $
                            Var local

  ; (spec_binds, rules) <- dsSpecs rhs prags

  ; let   global'   = addIdSpecialisations global rules
    main_bind = makeCorePair dflags global' (isDefaultMethod prags)
                                         (dictArity dicts) rhs

  ; return (main_bind `consOL` spec_binds) }

dsHsBind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dicts
                   , abs_exports = exports, abs_ev_binds = ev_binds
                   , abs_binds = binds })
         -- See Note [Desugaring AbsBinds]
  = do  { dflags <- getDynFlags
        ; bind_prs    <- ds_lhs_binds binds
        ; let core_bind = Rec [ makeCorePair dflags (add_inline lcl_id) False 0 rhs
                              | (lcl_id, rhs) <- fromOL bind_prs ]
          -- Monomorphic recursion possible, hence Rec

        locals       = map abe_mono exports
        tup_expr     = mkBigCoreVarTup locals
        tup_ty     = exprType tup_expr
        ; ds_binds <- dsTcEvBinds ev_binds
  ; let poly_tup_rhs = mkLams tyvars $ mkLams dicts $
                 mkCoreLets ds_binds $
           Let core_bind $
               tup_expr

  ; poly_tup_id <- newSysLocalDs (exprType poly_tup_rhs)

  ; let mk_bind (ABE { abe_wrap = wrap, abe_poly = global
                           , abe_mono = local, abe_prags = spec_prags })
          = do { tup_id  <- newSysLocalDs tup_ty
               ; rhs <- dsHsWrapper wrap $
                                 mkLams tyvars $ mkLams dicts $
                   mkTupleSelector locals local tup_id $
               mkVarApps (Var poly_tup_id) (tyvars ++ dicts)
                     ; let rhs_for_spec = Let (NonRec poly_tup_id poly_tup_rhs) rhs
         ; (spec_binds, rules) <- dsSpecs rhs_for_spec spec_prags
         ; let global' = (global `setInlinePragma` defaultInlinePragma)
                                             `addIdSpecialisations` rules
                           -- Kill the INLINE pragma because it applies to
                           -- the user written (local) function.  The global
                           -- Id is just the selector.  Hmm.
         ; return ((global', rhs) `consOL` spec_binds) }

        ; export_binds_s <- mapM mk_bind exports

  ; return ((poly_tup_id, poly_tup_rhs) `consOL`
        concatOL export_binds_s) }
  where
    inline_env :: IdEnv Id   -- Maps a monomorphic local Id to one with
                             -- the inline pragma from the source
                             -- The type checker put the inline pragma
                             -- on the *global* Id, so we need to transfer it
    inline_env = mkVarEnv [ (lcl_id, setInlinePragma lcl_id prag)
                          | ABE { abe_mono = lcl_id, abe_poly = gbl_id } <- exports
                          , let prag = idInlinePragma gbl_id ]

    add_inline :: Id -> Id    -- tran
    add_inline lcl_id = lookupVarEnv inline_env lcl_id `orElse` lcl_id

dsHsBind (PatSynBind{}) = panic "dsHsBind: PatSynBind"
-}

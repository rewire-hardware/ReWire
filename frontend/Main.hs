{-# LANGUAGE CPP #-}

module Main where

import GHC
import GHC.Paths ( libdir )
import DynFlags
import Outputable
import System.IO
import MonadUtils
import Bag
import SrcLoc 
 
main = 
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = dflags {hscTarget = HscInterpreted, 
                              ghcLink = LinkInMemory, 
                              ghcMode = CompManager}
        setSessionDynFlags dflags'  
        target <- guessTarget "tests/TestMain" Nothing
        setTargets [target]
        load LoadAllTargets
        g <- getModuleGraph
        p <- parseModule $ head g
        tc <- typecheckModule p
        let pm = tm_parsed_module tc
        let src = pm_parsed_source pm
        let tc_src = tm_typechecked_source tc
        liftIO $ printForC dflags' stdout (ppr tc_src)
        return (tc_src ,dflags')


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

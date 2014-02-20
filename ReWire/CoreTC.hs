{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.CoreTC where

import ReWire.Core
import ReWire.CorePP (ppp)
import ReWire.CoreParser
import Text.Parsec (runParser,eof)
import Control.Monad.State
import Control.Monad.Identity
import Unbound.LocallyNameless

-- Type checker for core.

type TySub = [(Name RWCTy,RWCTy)]

type TCM = FreshMT (StateT TySub Identity)

gathertvs :: RWCDefn -> TCM TySub
gathertvs (RWCDefn _ (Embed b)) = do (_,(_,e)) <- unbind b
                                     gathertvsExp e

gathertvsTy (RWCTyApp t1 t2) = liftM2 (++) (gathertvsTy t1) (gathertvsTy t2)
gathertvsTy (RWCTyCon _)     = return []
gathertvsTy (RWCTyVar v)     = let vn = name2String v
                               in  case vn of
                                     '?':_ -> return [(v,RWCTyVar v)]
                                     _     -> return []

gathertvsExp :: RWCExp -> TCM TySub
gathertvsExp (RWCApp t e1 e2)   = do tvs_t  <- gathertvsTy t 
                                     tvs_e1 <- gathertvsExp e1
                                     tvs_e2 <- gathertvsExp e2
                                     return (tvs_t++tvs_e1++tvs_e2)
gathertvsExp (RWCLam t b)       = do tvs_t  <- gathertvsTy t
                                     (_,e)  <- unbind b
                                     tvs_e  <- gathertvsExp e
                                     return (tvs_t++tvs_e)
gathertvsExp (RWCVar t _)       = gathertvsTy t
gathertvsExp (RWCCon t _)       = gathertvsTy t
gathertvsExp (RWCLiteral t _)   = gathertvsTy t
gathertvsExp (RWCCase t e alts) = do tvs_t    <- gathertvsTy t
                                     tvs_e    <- gathertvsExp e
                                     tvs_alts <- liftM concat (mapM gathertvsAlt alts)
                                     return (tvs_t++tvs_e++tvs_alts)

gathertvsAlt (RWCAlt b) = do (p,e) <- unbind b
                             tvs_p <- gathertvsPat p
                             tvs_e <- gathertvsExp e
                             return (tvs_p++tvs_e)

gathertvsPat (RWCPatVar (Embed t) _) = gathertvsTy t
gathertvsPat (RWCPatCon _ pats)      = liftM concat (mapM gathertvsPat pats)
gathertvsPat (RWCPatLiteral _)       = return []

initTySub :: RWCProg -> TCM ()
initTySub p = do ds  <- untrec (defns p)
                 sub <- liftM concat (mapM gathertvs ds)
                 put sub

typeOf (RWCApp t _ _)   = t
typeOf (RWCLam t _)     = t
typeOf (RWCVar t _)     = t
typeOf (RWCCon t _)     = t
typeOf (RWCLiteral t _) = t
typeOf (RWCCase t _ _)  = t

unify _ _ = return ()

freshav = undefined

askvarty = undefined
askconty = undefined

tcExp :: RWCExp -> TCM ()
tcExp (RWCApp t e1 e2) = do tcExp e1
                            tcExp e2
                            unify (typeOf e1)
                                  (RWCTyApp (RWCTyApp (RWCTyCon "(->)") (typeOf e2)) t)
tcExp (RWCLam t b)     = do (v,e) <- unbind b
                            t_    <- freshav
                            unify t 
                                  (RWCTyApp (RWCTyApp (RWCTyCon "(->)") (typeOf e)) t_)
tcExp (RWCVar t n)     = do mtn <- askvarty n
                            case mtn of
                              Nothing -> return ()
                              Just tn -> unify t n
tcExp (RWCCon t n)     = do tn <- askconty n
                            unify t n
--tcExp (RWCLiteral t n) = ...
--tcExp (RWCCase 
--tcExp e = return ()

tcDefn :: RWCDefn -> TCM ()
tcDefn (RWCDefn n (Embed b)) = do (tvs,(t,e)) <- unbind b
                                  tcExp e
                                  unify t (typeOf e)

tc :: RWCProg -> TCM ()
tc p = do initTySub p
          ds <- untrec (defns p)
          mapM_ tcDefn ds

ptc :: FilePath -> IO ()
ptc n = do guts        <- readFile n
           let res     =  runParser (whiteSpace >> prog >>= \ p -> whiteSpace >> eof >> return p) 0 n guts
           case res of
             Left err  -> print err
             Right ast -> print (runIdentity $ runStateT (runFreshMT $ tc ast) [])
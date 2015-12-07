{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.CaseInsertion where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad
import Control.Monad
import Unbound.LocallyNameless

--ciAlt = undefined

data Decon = WillDecon | WillMatch | WontDecon deriving Show

patDecon :: RWCPat -> RWCExp -> Name RWCExp -> RW Decon
patDecon (RWCPatCon i _) (RWCCon i

{-
doesDeconstr :: RWCExp -> Name RWCExp -> RW Bool
doesDeconstr (RWCApp t e1 _) x    = doesDeconstr e1 x -- FIXME: not too sure about this case. Is e2 really irrelevant?
doesDeconstr (RWCLam t b) x       = lunbind b (\(_,e) -> doesDeconstr e x)
doesDeconstr (RWCVar t n) x       = return False
doesDeconstr (RWCCon t i) x       = return False
doesDeconstr (RWCLiteral t l) x   = return False-}
--doesDeconstr (RWCCase t e alts) x =
{-
doCI :: Name RWCExp -> RWCTy -> RWCExp -> RW RWCExp
doCI x tx e = do dd <- doesDeconstr e x


ciExp :: RWCExp -> RW RWCExp
ciExp (RWCApp t e1 e2)   = liftM2 (RWCApp t) (ciExp e1) (ciExp e2)
ciExp (RWCLam t b)       = lunbind b (\(x,e) ->
                             do let tx =  arrowLeft t
                                e'     <- ciExp e
                                e''    <- doCI x tx e'
                                return (RWCLam t (bind x e'')))
ciExp (RWCVar t n)       = return (RWCVar t n)
ciExp (RWCCon t i)       = return (RWCCon t i)
ciExp (RWCLiteral t l)   = return (RWCLiteral t l)
ciExp (RWCCase t e alts) = do e' <- ciExp e
                              alts' <- mapM (ciAlt (typeOf e)) alts
                              return (RWCCase t e' alts')-}

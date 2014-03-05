{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Expand (cmdExpand) where

import Prelude hiding (sequence,mapM)
import ReWire.Core.Syntax
import Unbound.LocallyNameless
import Control.Monad hiding (sequence,mapM)
import Data.List (isInfixOf,find)
import Data.Traversable (sequence,mapM)
import Data.Maybe (catMaybes,isNothing,fromJust)
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad

import Debug.Trace (trace)

expandalt :: Name RWCExp -> RWCAlt -> RW RWCAlt
expandalt nexp (RWCAlt b) = lunbind b (\(p,eb) ->
                             do eb' <- expandexpr nexp eb
                                return (RWCAlt (bind p eb')))

expandexpr :: Name RWCExp -> RWCExp -> RW RWCExp
expandexpr nexp (RWCApp t e1 e2)         = liftM2 (RWCApp t) (expandexpr nexp e1) (expandexpr nexp e2)
expandexpr nexp (RWCLam t b)             = lunbind b (\(n,e) ->
                                            do e' <- expandexpr nexp e
                                               return (RWCLam t (bind n e')))
expandexpr nexp (RWCVar t n) | nexp == n = askvar t n
                             | otherwise = return (RWCVar t n)
expandexpr nexp e@(RWCCon {})            = return e
expandexpr nexp e@(RWCLiteral {})        = return e
expandexpr nexp (RWCCase t e alts)       = do e'    <- expandexpr nexp e
                                              alts' <- mapM (expandalt nexp) alts
                                              return (RWCCase t e' alts')

expanddefn :: Name RWCExp -> RWCDefn -> RW RWCDefn
expanddefn nexp (RWCDefn n (Embed b)) = lunbind b (\(tvs,(t,e)) ->
                                         do e' <- expandexpr nexp e
                                            return (RWCDefn n (Embed (setbind tvs (t,e')))))

go :: Name RWCExp -> RW [RWCDefn]
go n = do ds <- askDefns
          mapM (expanddefn n) ds

cmdExpand :: TransCommand
cmdExpand n p = let ds = runRW p (go (s2n n))
                    p' = p { defns = trec ds }
                in  (Just p',Nothing)

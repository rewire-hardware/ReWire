{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Expand where

import ReWire.Core.Syntax
import ReWire.Scoping
import ReWire.Core.Transformations.Monad
import ReWire.Core.Transformations.DeUniquify (deUniquify)
import ReWire.Core.Transformations.Uniquify (uniquify)
import ReWire.Core.Transformations.Types

expandAlt :: [Id RWCExp] -> RWCAlt -> RW RWCAlt
expandAlt ns (RWCAlt p e) = do e' <- expandExpr ns e
                               return (RWCAlt p e')

expandExpr :: [Id RWCExp] -> RWCExp -> RW RWCExp
expandExpr ns (RWCApp e1 e2)             = do e1' <- expandExpr ns e1
                                              e2' <- expandExpr ns e2
                                              return (RWCApp e1' e2')
expandExpr ns (RWCLam n t e)             = do e' <- expandExpr ns e
                                              return (RWCLam n t e')
expandExpr ns (RWCVar n t) | n `elem` ns = do me <- askVar t n
                                              case me of
                                                Just e  -> expandExpr ns e
                                                Nothing -> return (RWCVar n t)
                           | otherwise   = return (RWCVar n t)
expandExpr _ (RWCCon dci t)              = return (RWCCon dci t)
expandExpr _ (RWCLiteral l)              = return (RWCLiteral l)
expandExpr ns (RWCCase e alts)           = do e'    <- expandExpr ns e
                                              alts' <- mapM (expandAlt ns) alts
                                              return (RWCCase e' alts')
expandExpr _ (RWCNativeVHDL n e)         = return (RWCNativeVHDL n e)             -- FIXME(?!): special case here!

expandDefn :: [Id RWCExp] -> RWCDefn -> RW RWCDefn
expandDefn ns (RWCDefn n pt b e) = do e' <- expandExpr ns e
                                      return (RWCDefn n pt b e')

expandModule :: [Id RWCExp] -> RWCProgram -> RW RWCProgram
expandModule ns (RWCProgram dds defns) = do defns' <- mapM (expandDefn ns) defns
                                            return (RWCProgram dds defns')

expand :: [Id RWCExp] -> RWCProgram -> RWCProgram
expand ns m_ = deUniquify $ runRW ctr m (expandModule ns m)
    where (m,ctr) = uniquify 0 m_

cmdExpand :: TransCommand
cmdExpand s p = (Just (expand ns p),Nothing)
  where ns = map mkId (words s)

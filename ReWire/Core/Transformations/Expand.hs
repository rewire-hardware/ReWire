{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Expand where

import ReWire.Core.Syntax
import ReWire.Scoping
import ReWire.Core.Transformations.Monad
import ReWire.Core.Transformations.DeUniquify (deUniquify)
import ReWire.Core.Transformations.Uniquify (uniquify)
import ReWire.Core.Transformations.Types

expandExpr :: [Id RWCExp] -> RWCExp -> RW RWCExp
expandExpr ns (RWCApp an e1 e2)             = do e1' <- expandExpr ns e1
                                                 e2' <- expandExpr ns e2
                                                 return (RWCApp an e1' e2')
expandExpr ns (RWCLam an n t e)             = do e' <- expandExpr ns e
                                                 return (RWCLam an n t e')
expandExpr ns (RWCVar an n t) | n `elem` ns = do me <- askVar t n
                                                 case me of
                                                   Just e  -> expandExpr ns e
                                                   Nothing -> return (RWCVar an n t)
                              | otherwise   = return (RWCVar an n t)
expandExpr _ (RWCCon an dci t)              = return (RWCCon an dci t)
expandExpr _ (RWCLiteral an l)              = return (RWCLiteral an l)
expandExpr ns (RWCCase an e p e1 e2)        = do e'    <- expandExpr ns e
                                                 e1'   <- expandExpr ns e1
                                                 e2'   <- expandExpr ns e2
                                                 return (RWCCase an e' p e1' e2')
expandExpr _ (RWCNativeVHDL an n e)         = return (RWCNativeVHDL an n e) -- FIXME(?!): special case here!
expandExpr _ (RWCError an m t)              = return (RWCError an m t)

expandDefn :: [Id RWCExp] -> RWCDefn -> RW RWCDefn
expandDefn ns (RWCDefn an n pt b e) = do e' <- expandExpr ns e
                                         return (RWCDefn an n pt b e')

expandModule :: [Id RWCExp] -> RWCProgram -> RW RWCProgram
expandModule ns (RWCProgram dds defns) = do defns' <- mapM (expandDefn ns) defns
                                            return (RWCProgram dds defns')

expand :: [Id RWCExp] -> RWCProgram -> RWCProgram
expand ns m_ = deUniquify $ runRW ctr m (expandModule ns m)
    where (m,ctr) = uniquify 0 m_

cmdExpand :: TransCommand
cmdExpand s p = (Just (expand ns p),Nothing)
  where ns = map mkId (words s)

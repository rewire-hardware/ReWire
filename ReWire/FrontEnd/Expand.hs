module ReWire.FrontEnd.Expand (expand) where

import ReWire.FrontEnd.Monad
import ReWire.FrontEnd.Syntax
import ReWire.Scoping

expandExpr :: [Id RWMExp] -> RWMExp -> RW RWMExp
expandExpr ns (RWMApp an e1 e2)             = do e1' <- expandExpr ns e1
                                                 e2' <- expandExpr ns e2
                                                 return $ RWMApp an e1' e2'
expandExpr ns (RWMLam an n t e)             = do e' <- expandExpr ns e
                                                 return $ RWMLam an n t e'
expandExpr ns (RWMVar an n t) | n `elem` ns = do me <- askVar t n
                                                 case me of
                                                   Just e  -> expandExpr ns e
                                                   Nothing -> return $ RWMVar an n t
                              | otherwise   = return (RWMVar an n t)
expandExpr _ (RWMCon an dci t)              = return (RWMCon an dci t)
expandExpr _ (RWMLiteral an l)              = return (RWMLiteral an l)
expandExpr ns (RWMCase an e p e1 e2)        = do e'    <- expandExpr ns e
                                                 e1'   <- expandExpr ns e1
                                                 e2'   <- expandExpr ns e2
                                                 return $ RWMCase an e' p e1' e2'
expandExpr _ (RWMNativeVHDL an n e)         = return $ RWMNativeVHDL an n e -- FIXME(?!): special case here!
expandExpr _ (RWMError an m t)              = return $ RWMError an m t

expandDefn :: [Id RWMExp] -> RWMDefn -> RW RWMDefn
expandDefn ns (RWMDefn an n pt b e) = do e' <- expandExpr ns e
                                         return $ RWMDefn an n pt b e'

expand :: [Id RWMExp] -> RWMProgram -> RW RWMProgram
expand ns (RWMProgram dds defns) = do defns' <- mapM (expandDefn ns) defns
                                      return $ RWMProgram dds defns'

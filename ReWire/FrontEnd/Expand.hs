{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.Expand (expand) where

import ReWire.FrontEnd.Monad
import ReWire.FrontEnd.Syntax
import ReWire.Scoping

expandExpr :: [Id RWMExp] -> RWMExp -> RW RWMExp
expandExpr ns = \ case
      RWMApp an e1 e2       -> RWMApp an <$> expandExpr ns e1 <*> expandExpr ns e2
      RWMLam an n t e       -> RWMLam an n t <$> expandExpr ns e
      RWMVar an n t
            | n `elem` ns   -> do
                  me <- askVar t n
                  case me of
                        Just e  -> expandExpr ns e
                        Nothing -> return $ RWMVar an n t
            | otherwise     -> return $ RWMVar an n t
      RWMCon an dci t       -> return $ RWMCon an dci t
      RWMLiteral an l       -> return $ RWMLiteral an l
      RWMCase an e p e1 e2  -> RWMCase an <$> expandExpr ns e <*> return p <*> expandExpr ns e1 <*> expandExpr ns e2
      RWMNativeVHDL an n e  -> return $ RWMNativeVHDL an n e -- FIXME(?!): special case here!
      RWMError an m t       -> return $ RWMError an m t

expandDefn :: [Id RWMExp] -> RWMDefn -> RW RWMDefn
expandDefn ns (RWMDefn an n pt b e) = RWMDefn an n pt b <$> expandExpr ns e

expand :: [Id RWMExp] -> RWMProgram -> RW RWMProgram
expand ns (RWMProgram dds defns) = RWMProgram dds <$> mapM (expandDefn ns) defns

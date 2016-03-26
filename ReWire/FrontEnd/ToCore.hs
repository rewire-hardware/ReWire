{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.ToCore (toCore) where

import ReWire.Core.Syntax
import ReWire.FrontEnd.PrimBasis
import ReWire.FrontEnd.Rename
import ReWire.FrontEnd.Syntax
import ReWire.Scoping

import Data.Monoid ((<>))

import Control.Applicative((<$>), (<*>), Applicative(..))

toCore :: (Monad m, Functor m, Applicative m) => RWMProgram -> m RWCProgram
toCore p = toCore' $ p <> primBasis
      where toCore' :: (Monad m, Functor m, Applicative m) => RWMProgram -> m RWCProgram
            toCore' (RWMProgram datas funs) = RWCProgram <$> mapM transData datas <*> mapM transFun funs

transData :: Monad m => RWMData -> m RWCData
transData (RWMData an ty tys knd cons) = return $ RWCData an ty tys knd cons

transFun :: (Monad m, Applicative m) => RWMDefn -> m RWCDefn
transFun (RWMDefn an n ty inl e) = do
      n' <- transId n
      e' <- transExp e
      return $ RWCDefn an n' ty inl e'

transId :: Monad m => Id RWMExp -> m (Id RWCExp)
transId (Id x y) = return $ Id x y

transExp :: (Monad m, Functor m, Applicative m) => RWMExp -> m RWCExp
transExp = \ case
      RWMApp an e1 e2       -> RWCApp an <$> transExp e1 <*> transExp e2
      RWMLam an x t e       -> RWCLam an <$> transId x <*>  return t <*> transExp e
      RWMVar an x t         -> RWCVar an <$> transId x <*> return t
      RWMCon an d t         -> return $ RWCCon an d t
      RWMLiteral an l       -> return $ RWCLiteral an l
      RWMCase an e1 p e2 e3 -> RWCCase an <$> transExp e1 <*> transPat p <*> transExp e2 <*> transExp e3
      RWMNativeVHDL an s e  -> RWCNativeVHDL an s <$> transExp e
      RWMError an s t       -> return $ RWCError an s t


transPat :: (Monad m, Functor m, Applicative m) => RWMPat -> m RWCPat
transPat = \ case
      RWMPatCon an d ps  -> RWCPatCon an d <$> mapM transPat ps
      RWMPatLiteral an l -> return $ RWCPatLiteral an l
      RWMPatVar an x t   -> RWCPatVar an <$> transId x <*> return t

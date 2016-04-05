{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.ToCore (toCore) where

import ReWire.Core.Syntax hiding (typeOf)
import ReWire.FrontEnd.Syntax

import Unbound.Generics.LocallyNameless (name2String)

toCore :: Fresh m => RWMProgram -> m RWCProgram
toCore (RWMProgram datas funs) = do
      funs' <- untrec funs
      RWCProgram <$> mapM transData datas <*> mapM transFun funs'

transData :: Applicative m => RWMData -> m RWCData
transData (RWMData an t ts _ cs) = pure $ RWCData an t ts cs

transFun :: Fresh m => RWMDefn -> m RWCDefn
transFun (RWMDefn an n ty _ (Embed e)) = do
      (vs, e') <- unbind e
      RWCDefn an (name2String n) ty (length vs) <$> transExp e'

transExp :: Fresh m => RWMExp -> m RWCExp
transExp = \ case
      RWMApp an e1 e2       -> RWCApp an <$> transExp e1 <*> transExp e2
      -- TODO(chathhorn): distinguish locals from globals.
      RWMVar an t x         -> pure $ RWCGVar an t $ name2String x
      RWMCon an t d         -> pure $ RWCCon an t d
      -- TODO(chathhorn)
      --RWMCase an e1 e2 e3   -> RWCCase an <$> transExp e1 <*> transPat p <*> transExp e2 <*> transExp e3
      RWMNativeVHDL an s e  -> pure $ RWCNativeVHDL an (typeOf e) s
      RWMError an t s       -> pure $ RWCError an t s
      _                     -> error "ToCore: unsupported expression"

transPat :: Fresh m => RWMPat -> m RWCPat
transPat = \ case
      RWMPatCon an d ps  -> RWCPatCon an d <$> mapM transPat ps
      RWMPatVar an t _   -> RWCPatVar an <$> pure t

{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.ToCore (toCore) where

import ReWire.Core.Syntax hiding (typeOf)
import ReWire.FrontEnd.Syntax

import Unbound.Generics.LocallyNameless (name2String)

toCore :: Fresh m => RWMProgram -> m RWCProgram
toCore (RWMProgram datas funs) = do
      funs' <- untrec funs
      RWCProgram <$> mapM transData datas <*> mapM transDefn funs'

transData :: Applicative m => RWMData -> m RWCData
transData (RWMData an tid ts _ cs) = pure $ RWCData an tid (map name2String ts) (map transDataCon cs)

transDefn :: Fresh m => RWMDefn -> m RWCDefn
transDefn (RWMDefn an n (Embed (Poly t)) _ (Embed e)) = do
      (_, t')  <- unbind t
      (vs, e') <- unbind e
      RWCDefn an (name2String n) (transType t') (length vs) <$> transExp e'

transExp :: Fresh m => RWMExp -> m RWCExp
transExp = \ case
      RWMApp an e1 e2       -> RWCApp an <$> transExp e1 <*> transExp e2
      -- TODO(chathhorn): distinguish locals from globals.
      RWMVar an t x         -> pure $ RWCGVar an (transType t) $ name2String x
      RWMCon an t d         -> pure $ RWCCon an (transType t) d
      -- TODO(chathhorn)
      --RWMCase an e1 e2 e3   -> RWCCase an <$> transExp e1 <*> transPat p <*> transExp e2 <*> transExp e3
      RWMNativeVHDL an s e  -> pure $ RWCNativeVHDL an (transType $ typeOf e) s
      RWMError an t s       -> pure $ RWCError an (transType t) s
      _                     -> error "ToCore: unsupported expression"

transPat :: Fresh m => RWMPat -> m RWCPat
transPat = \ case
      RWMPatCon an d ps  -> RWCPatCon an d <$> mapM transPat ps
      RWMPatVar an t _   -> pure $ RWCPatVar an $ transType t

transType :: RWMTy -> RWCTy
transType = \ case
      RWMTyApp an t1 t2  -> RWCTyApp an (transType t1) $ transType t2
      RWMTyCon an c      -> RWCTyCon an c
      RWMTyVar an x      -> RWCTyVar an $ name2String x
      RWMTyComp an t1 t2 -> RWCTyComp an (transType t1) $ transType t2

transDataCon :: RWMDataCon -> RWCDataCon
transDataCon (RWMDataCon an c ts) = RWCDataCon an c $ map transType ts

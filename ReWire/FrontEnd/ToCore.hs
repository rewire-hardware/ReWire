{-# LANGUAGE LambdaCase, ViewPatterns #-}
module ReWire.FrontEnd.ToCore (toCore) where

import ReWire.Annotation
import ReWire.Core.Syntax hiding (typeOf, flattenApp)
import ReWire.FrontEnd.Syntax
import ReWire.Pretty

import Unbound.Generics.LocallyNameless (name2String, runFreshM)

toCore :: Fresh m => RWMProgram -> m RWCProgram
toCore (RWMProgram p) = do
      (ts, vs) <- untrec p
      let ts' = concatMap transData ts
      vs' <- mapM transDefn vs
      return $ RWCProgram ts' vs'

transData :: RWMData -> [RWCDataCon]
transData (RWMData _ _ _ cs) = runFreshM $ mapM transDataCon cs
      where transDataCon :: Fresh m => RWMDataCon -> m RWCDataCon
            transDataCon (RWMDataCon an c (Embed (Poly t))) = do
                  (_, t') <- unbind t
                  return $ RWCDataCon an (DataConId $ name2String c) $ transType t'

transDefn :: Fresh m => RWMDefn -> m RWCDefn
transDefn (RWMDefn an n (Embed (Poly t)) _ (Embed e)) = do
      (_, t')  <- unbind t
      (_, e') <- unbind e
      RWCDefn an (name2String n) (transType t') <$> transExp e'

transExp :: Fresh m => RWMExp -> m RWCExp
transExp = \ case
      RWMApp an e1 e2       -> RWCApp an <$> transExp e1 <*> transExp e2
      -- TODO(chathhorn): distinguish locals from globals.
      RWMVar an t x         -> pure $ RWCGVar an (transType t) $ name2String x
      RWMCon an t d         -> pure $ RWCCon an (transType t) $ DataConId $ name2String d
      RWMCase an e1 e2 (RWMError _ _ _)   -> do
            (p, e2') <- unbind e2
            RWCMatch an <$> transExp e1 <*> transPat p <*> pure (getVar e2') <*> pure Nothing
      RWMCase an e1 e2 e3   -> do
            (p, e2') <- unbind e2
            RWCMatch an <$> transExp e1 <*> transPat p <*> pure (getVar e2') <*> (Just <$> transExp e3)
      RWMNativeVHDL an s e  -> pure $ RWCNativeVHDL an (transType $ typeOf e) s
      RWMError an t _       -> pure $ RWCGVar an (transType t) "ERROR"
      e                     -> error $ "ToCore: unsupported expression: " ++ prettyPrint e
      where getVar :: RWMExp -> GId
            getVar (flattenApp -> (RWMVar _ _ v):_) = name2String v
            getVar e                                = error $ "ToCore: getVar: unsupported expression: " ++ prettyPrint e

transPat :: Fresh m => RWMPat -> m RWCPat
transPat = \ case
      RWMPatCon an (Embed d) ps -> RWCPatCon an (DataConId $ name2String d) <$> mapM transPat ps
      RWMPatVar an (Embed t) _  -> pure $ RWCPatVar an $ transType t

transType :: RWMTy -> RWCTy
transType = \ case
      RWMTyApp an t1 t2  -> RWCTyApp an (transType t1) $ transType t2
      RWMTyCon an c      -> RWCTyCon an $ TyConId $ name2String c
      RWMTyComp an t1 t2 -> RWCTyComp an (transType t1) $ transType t2
      _                  -> RWCTyCon noAnn $ TyConId "TODO(chathhorn): leaked polymorphism"


{-# LANGUAGE LambdaCase, ViewPatterns #-}
module ReWire.FrontEnd.ToCore (toCore) where

import ReWire.Core.Syntax hiding (typeOf, flattenApp)
import ReWire.FrontEnd.Syntax hiding (defnName)
import ReWire.Pretty

import Control.Monad.Reader (ReaderT (..), ask)
import Data.Map.Strict (Map)
import Unbound.Generics.LocallyNameless (runFreshMT)

import qualified Data.Map.Strict as Map

import Unbound.Generics.LocallyNameless (name2String)

toCore :: Monad m => RWMProgram -> m Program
toCore (RWMProgram p) = runFreshMT $ do
      (ts, vs) <- untrec p
      ts' <- concat <$> mapM transData ts
      vs' <- filter (elem '.' . defnName) <$> mapM transDefn vs
      return $ Program ts' vs'

transData :: Fresh m => RWMData -> m [DataCon]
transData (RWMData _ _ _ cs) = mapM transDataCon $ zip [0..] cs
      where transDataCon :: Fresh m => (Int, RWMDataCon) -> m DataCon
            transDataCon (n, RWMDataCon an c (Embed (Poly t))) = do
                  (_, t') <- unbind t
                  return $ DataCon an (DataConId $ name2String c) n $ transType t'

transDefn :: Fresh m => RWMDefn -> m Defn
transDefn (RWMDefn an n (Embed (Poly t)) _ (Embed e)) = do
      (_, t')  <- unbind t
      (xs, e') <- unbind e
      Defn an (transVar n) (transType t') <$> runReaderT (transExp e') (Map.fromList $ zip xs [0..])

transVar :: Name RWMExp -> GId
transVar n = case head $ name2String n of
      '$' -> prettyPrint n
      _   -> name2String n

transExp :: Fresh m => RWMExp -> ReaderT (Map (Name RWMExp) Int) m Exp
transExp = \ case
      RWMApp an e1 e2                     -> App an <$> transExp e1 <*> transExp e2
      RWMVar an t x                       -> (Map.lookup x <$> ask) >>= \ case
            Nothing -> if elem '.' (name2String x)
                  then pure $ GVar an (transType t) $ transVar x
                  else pure $ Prim an (transType t) $ transVar x
            Just i  -> pure $ LVar an (transType t) i
      RWMCon an t d                       -> pure $ Con an (transType t) $ DataConId $ name2String d
      RWMMatch an t e p f as (Just e2)    -> Match an (transType t) <$> transExp e <*> transPat p <*> (toGId <$> transExp f) <*> mapM ((toLId <$>) . transExp) as <*> (Just <$> transExp e2)
      RWMMatch an t e p f as Nothing      -> Match an (transType t) <$> transExp e <*> transPat p <*> (toGId <$> transExp f) <*> mapM ((toLId <$>) . transExp) as <*> pure Nothing
      RWMNativeVHDL an s (RWMError _ t _) -> pure $ NativeVHDL an (transType t) s
      RWMError an t _                     -> pure $ GVar an (transType t) "ERROR"
      e                                   -> error $ "ToCore: unsupported expression: " ++ prettyPrint e

toGId :: Exp -> GId
toGId (GVar _ _ x) = x
toGId e               = error $ "toGId: expected GVar, got: " ++ prettyPrint e

toLId :: Exp -> LId
toLId (LVar _ _ x) = x
toLId e               = error $ "toLId: expected LVar, got: " ++ prettyPrint e

transPat :: Fresh m => RWMMatchPat -> m Pat
transPat = \ case
      RWMMatchPatCon an d ps -> PatCon an (DataConId $ name2String d) <$> mapM transPat ps
      RWMMatchPatVar an t    -> pure $ PatVar an $ transType t

transType :: RWMTy -> Ty
transType = \ case
      RWMTyApp an t1 t2  -> TyApp an (transType t1) $ transType t2
      RWMTyCon an c      -> TyCon an $ TyConId $ name2String c
      RWMTyComp an t1 t2 -> TyComp an (transType t1) $ transType t2
      RWMTyVar an _ x    -> TyVar an $ name2String x
      t                  -> error $ "ToCore: unsupported type: " ++ prettyPrint t


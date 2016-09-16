{-# LANGUAGE LambdaCase, ViewPatterns #-}
{-# LANGUAGE Safe #-}
module ReWire.FrontEnd.ToCore (toCore) where

import ReWire.Pretty
import ReWire.FrontEnd.Unbound
      ( Name, Fresh, runFreshMT, Embed (..)
      , unbind, name2String
      )

import Control.Monad.Reader (ReaderT (..), ask)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict        as Map
import qualified ReWire.Core.Syntax     as C
import qualified ReWire.FrontEnd.Syntax as M

toCore :: Monad m => M.FreeProgram -> m C.Program
toCore (ts, vs) = runFreshMT $ do
      ts' <- concat <$> mapM transData ts
      vs' <- filter (notPrim . C.defnName) <$> mapM transDefn vs
      return $ C.Program ts' vs'

notPrim :: String -> Bool
notPrim x = isQual x || isLL x
      where isQual = elem '.'
            isLL = elem '$'

transData :: Fresh m => M.DataDefn -> m [C.DataCon]
transData (M.DataDefn _ _ _ cs) = mapM transDataCon $ zip [0..] cs
      where transDataCon :: Fresh m => (Int, M.DataCon) -> m C.DataCon
            transDataCon (n, M.DataCon an c (Embed (M.Poly t))) = do
                  (_, t') <- unbind t
                  return $ C.DataCon an (C.DataConId $ name2String c) n $ transType t'

transDefn :: Fresh m => M.Defn -> m C.Defn
transDefn (M.Defn an n (Embed (M.Poly t)) _ (Embed e)) = do
      (_, t')  <- unbind t
      (xs, e') <- unbind e
      C.Defn an (transVar n) (transType t') <$> runReaderT (transExp e') (Map.fromList $ zip xs [0..])

transVar :: Name M.Exp -> C.GId
transVar n = case head $ name2String n of
      '$' -> prettyPrint n
      _   -> name2String n

transExp :: Fresh m => M.Exp -> ReaderT (Map (Name M.Exp) Int) m C.Exp
transExp = \ case
      M.App an e1 e2                    -> C.App an <$> transExp e1 <*> transExp e2
      M.Var an t x                      -> (Map.lookup x <$> ask) >>= \ case
            Nothing -> if notPrim (name2String x)
                  then pure $ C.GVar an (transType t) $ transVar x
                  else pure $ C.Prim an (transType t) $ transVar x
            Just i  -> pure $ C.LVar an (transType t) i
      M.Con an t d                      -> pure $ C.Con an (transType t) $ C.DataConId $ name2String d
      M.Match an t e p f as (Just e2)   -> C.Match an (transType t) <$> transExp e <*> transPat p <*> (toGId <$> transExp f) <*> mapM ((toLId <$>) . transExp) as <*> (Just <$> transExp e2)
      M.Match an t e p f as Nothing     -> C.Match an (transType t) <$> transExp e <*> transPat p <*> (toGId <$> transExp f) <*> mapM ((toLId <$>) . transExp) as <*> pure Nothing
      M.NativeVHDL an s (M.Error _ t _) -> pure $ C.NativeVHDL an (transType t) s
      M.Error an t _                    -> pure $ C.GVar an (transType t) "ERROR"
      e                                 -> error $ "ToCore: unsupported expression: " ++ prettyPrint e

toGId :: C.Exp -> C.GId
toGId (C.GVar _ _ x) = x
toGId e               = error $ "toGId: expected GVar, got: " ++ prettyPrint e

toLId :: C.Exp -> C.LId
toLId (C.LVar _ _ x) = x
toLId e               = error $ "toLId: expected LVar, got: " ++ prettyPrint e

transPat :: Fresh m => M.MatchPat -> m C.Pat
transPat = \ case
      M.MatchPatCon an t d ps -> C.PatCon an (transType t) (C.DataConId $ name2String d) <$> mapM transPat ps
      M.MatchPatVar an t      -> pure $ C.PatVar an $ transType t

transType :: M.Ty -> C.Ty
transType = \ case
      M.TyApp an t1 t2  -> C.TyApp an (transType t1) $ transType t2
      M.TyCon an c      -> C.TyCon an $ C.TyConId $ name2String c
      M.TyComp an t1 t2 -> C.TyComp an (transType t1) $ transType t2
      M.TyVar an _ x    -> C.TyVar an $ name2String x
      t                 -> error $ "ToCore: unsupported type: " ++ prettyPrint t


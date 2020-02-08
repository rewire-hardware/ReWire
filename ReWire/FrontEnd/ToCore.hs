{-# LANGUAGE LambdaCase, FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.FrontEnd.ToCore (toCore) where

import ReWire.Annotation
import ReWire.Error
import ReWire.Pretty
import ReWire.FrontEnd.Unbound
      ( Name, Fresh, runFreshMT, Embed (..)
      , unbind, name2String
      )

import Control.Monad ((<=<))
import Control.Monad.Reader (ReaderT (..), asks)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict        as Map
import qualified ReWire.Core.Syntax     as C
import qualified ReWire.FrontEnd.Syntax as M

toCore :: MonadError AstError m => M.FreeProgram -> m C.Program
toCore (ts, vs) = runFreshMT $ do
      ts' <- concat <$> mapM transData ts
      vs' <- mapM transDefn $ filter (notPrim . M.defnName) vs
      return $ C.Program ts' vs'

notPrim :: Name a -> Bool
notPrim = elem '.' . name2String

transData :: (MonadError AstError m, Fresh m) => M.DataDefn -> m [C.DataCon]
transData (M.DataDefn _ _ _ cs) = mapM transDataCon $ zip [0..] cs
      where transDataCon :: (MonadError AstError m, Fresh m) => (Int, M.DataCon) -> m C.DataCon
            transDataCon (n, M.DataCon an c (Embed (M.Poly t))) = do
                  (_, t') <- unbind t
                  C.DataCon an (C.DataConId $ name2String c) n <$> transType t'

transDefn :: (MonadError AstError m, Fresh m) => M.Defn -> m C.Defn
transDefn (M.Defn an n (Embed (M.Poly t)) _ (Embed e)) = do
      (_, t')  <- unbind t
      (xs, e') <- unbind e
      C.Defn an (transVar n) <$> transType t' <*> runReaderT (transExp e') (Map.fromList $ zip xs [0..])

transVar :: Name M.Exp -> C.GId
transVar n = case head $ name2String n of
      '$' -> prettyPrint n
      _   -> name2String n

transExp :: (MonadError AstError m, Fresh m) => M.Exp -> ReaderT (Map (Name M.Exp) Int) m C.Exp
transExp = \ case
      M.App an e1 e2                    -> C.App an <$> transExp e1 <*> transExp e2
      M.Var an t x                      -> asks (Map.lookup x) >>= \ case
            Nothing -> if notPrim x
                  then C.GVar an <$> transType t <*> pure (transVar x)
                  else C.Prim an <$> transType t <*> pure (transVar x)
            Just i  -> C.LVar an <$> transType t <*> pure i
      M.Con an t d                      -> C.Con an <$> transType t <*> pure (C.DataConId (name2String d))
      M.Match an t e p f as (Just e2)   -> C.Match an <$> transType t <*> transExp e <*> transPat p <*> (toGId =<< transExp f) <*> mapM (toLId <=< transExp) as <*> (Just <$> transExp e2)
      M.Match an t e p f as Nothing     -> C.Match an <$> transType t <*> transExp e <*> transPat p <*> (toGId =<< transExp f) <*> mapM (toLId <=< transExp) as <*> pure Nothing
      M.NativeVHDL an s (M.Error _ t _) -> C.NativeVHDL an <$> transType t <*> pure s
      M.Error an t _                    -> C.GVar an <$> transType t <*> pure "ERROR"
      e                                 -> failAt (ann e) $ "ToCore: unsupported expression: " ++ prettyPrint e

toGId :: MonadError AstError m => C.Exp -> m C.GId
toGId (C.GVar _ _ x) = pure x
toGId e              = failAt (ann e) $ "toGId: expected GVar, got: " ++ prettyPrint e

toLId :: MonadError AstError m => C.Exp -> m C.LId
toLId (C.LVar _ _ x) = pure x
toLId e              = failAt (ann e) $ "toLId: expected LVar, got: " ++ prettyPrint e

transPat :: (MonadError AstError m, Fresh m) => M.MatchPat -> m C.Pat
transPat = \ case
      M.MatchPatCon an t d ps -> C.PatCon an <$> transType t <*> pure (C.DataConId $ name2String d) <*> mapM transPat ps
      M.MatchPatVar an t      -> C.PatVar an <$> transType t

transType :: MonadError AstError m => M.Ty -> m C.Ty
transType = \ case
      M.TyApp an t1 t2  -> C.TyApp an <$> transType t1 <*> transType t2
      M.TyCon an c      -> pure $ C.TyCon an $ C.TyConId $ name2String c
      M.TyVar an _ x    -> pure $ C.TyVar an $ name2String x
      t                 -> pure $ C.TyVar (ann t) "$$TyBlank$$" -- TODO(chathhorn)


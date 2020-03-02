{-# LANGUAGE LambdaCase, FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.ToCore (toCore) where

import ReWire.Annotation
import ReWire.Error
import ReWire.Pretty
import ReWire.Unbound (Name, Fresh, Embed (..) , unbind)

import Control.Monad ((<=<))
import Control.Monad.State (StateT (..), MonadState, get, put)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, lift)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)

import qualified Data.HashMap.Strict as Map
import qualified ReWire.Core.Syntax  as C
import qualified ReWire.Crust.Syntax as M

type TCM m = ReaderT [M.DataDefn] (ReaderT (HashMap (Name M.Exp) Int) m)
-- type TCM m = ReaderT [M.DataDefn] (ReaderT (HashMap (Name M.Exp) Int) (StateT (HashMap M.Ty Int) m))

toCore :: (Fresh m, MonadError AstError m) => M.FreeProgram -> m C.Program
toCore (ts, vs) = fst <$> flip runStateT mempty (do
      ts' <- concat <$> mapM (transData ts) ts
      vs' <- mapM (transDefn ts) $ filter (not . M.isPrim . M.defnName) vs
      pure $ C.Program ts' vs')

transData :: (MonadError AstError m, Fresh m, MonadState (HashMap M.Ty Int) m) => [M.DataDefn] -> M.DataDefn -> m [C.DataCon]
transData ts (M.DataDefn _ _ _ cs) = mapM transDataCon $ zip [0..] cs
      where transDataCon :: (MonadError AstError m, Fresh m, MonadState (HashMap M.Ty Int) m) => (Int, M.DataCon) -> m C.DataCon
            transDataCon (n, M.DataCon an c (Embed (M.Poly t))) = do
                  (_, t') <- unbind t
                  C.DataCon an (C.DataConId $ show c) n <$> runReaderT (transType t') ts

transDefn :: (MonadError AstError m, Fresh m, MonadState (HashMap M.Ty Int) m) => [M.DataDefn] -> M.Defn -> m C.Defn
transDefn ts (M.Defn an n (Embed (M.Poly t)) _ (Embed e)) = do
      (_, t')  <- unbind t
      (xs, e') <- unbind e
      C.Defn an (show n) <$> runReaderT (transType t') ts <*> runReaderT (runReaderT (transExp e') ts) (Map.fromList $ zip xs [0..])

transExp :: (MonadError AstError m, Fresh m, MonadState (HashMap M.Ty Int) m) => M.Exp -> TCM m C.Exp
transExp = \ case
      M.App an e1 e2                    -> C.App an <$> transExp e1 <*> transExp e2
      M.Var an t x                      -> lift (asks (Map.lookup x)) >>= \ case
            Nothing -> if not $ M.isPrim x
                  then C.GVar an <$> transType t <*> pure (show x)
                  else C.Prim an <$> transType t <*> pure (show x)
            Just i  -> C.LVar an <$> transType t <*> pure i
      M.Con an t d                      -> C.Con an <$> transType t <*> ctorWidth t d <*> pure (C.DataConId $ show d)
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

transPat :: (MonadError AstError m, Fresh m, MonadState (HashMap M.Ty Int) m) => M.MatchPat -> ReaderT [M.DataDefn] m C.Pat
transPat = \ case
      M.MatchPatCon an t d ps -> C.PatCon an <$> transType t <*> pure (C.DataConId $ show d) <*> mapM transPat ps
      M.MatchPatVar an t      -> C.PatVar an <$> transType t

transType :: (Fresh m, MonadError AstError m, MonadState (HashMap M.Ty Int) m) => M.Ty -> ReaderT [M.DataDefn] m C.Ty
transType t = case t of
      M.TyApp an t1 t2  -> C.TyApp an <$> sizeof an t <*> transType t1 <*> transType t2
      M.TyCon an c      -> C.TyCon an (C.TyConId $ show c) <$> sizeof an t
      M.TyVar an _ x    -> C.TyVar an (show x) <$> sizeof an t
      _                 -> pure $ C.TyVar (ann t) "$$TyBlank$$" 0 -- TODO(chathhorn)

matchTy :: MonadError AstError m => Annote -> M.Ty -> M.Ty -> m TySub
matchTy an (M.TyApp _ t1 t2) (M.TyApp _ t1' t2') = do
      s1 <- matchTy an t1 t1'
      s2 <- matchTy an t2 t2'
      merge an s1 s2
matchTy _ (M.TyVar _ _ v) t                    = pure [(show v, t)]
matchTy _ _ _ = pure []
-- matchTy _ (M.TyCon _ tci) (M.TyCon _ tci')
--       | tci == tci'                          = pure []
-- matchTy an t t'                              = failAt an
--       $ "ToCore: matchTy: can't match "
--       ++ prettyPrint t ++ " with " ++ prettyPrint t'

merge :: MonadError AstError m => Annote -> TySub -> TySub -> m TySub
merge an s'  = \ case
      []          -> pure s'
      (v, t) : s -> case lookup v s' of
            Nothing           -> ((v, t) :) <$> merge an s' s
            Just t' | t == t' -> merge an s' s
            Just t'           -> failAt an
                  $ "ToCore: merge: inconsistent assignment of tyvar " ++ v
                  ++ ": " ++ prettyPrint t ++ " vs. " ++ prettyPrint t'

type TySub = [(C.TyId, M.Ty)]

apply :: TySub -> M.Ty -> M.Ty
apply s (M.TyApp an t1 t2)  = M.TyApp an (apply s t1) $ apply s t2
apply _ t@M.TyCon {}        = t
apply s t@(M.TyVar _ _ i)     = fromMaybe t $ lookup (show i) s

ctorWidth :: (Fresh m, MonadError AstError m, MonadReader [M.DataDefn] m, MonadState (HashMap M.Ty Int) m) => M.Ty -> Name M.DataConId -> m Int
ctorWidth t d = do
      let t'            =  snd $ M.flattenArrow t
      cs                <- filter isMine <$> concatMap M.dataCons <$> ask
      case cs of
            [c] -> do
                  (targs, tres)     <- M.flattenArrow <$> projTy c
                  s                 <- matchTy (ann t') tres t'
                  sum <$> mapM (sizeof $ ann t) (map (apply s) targs)
            _ -> pure 0
      where isMine :: M.DataCon -> Bool
            isMine (M.DataCon _ d' _) = d == d'

            projTy :: Fresh m => M.DataCon -> m M.Ty
            projTy (M.DataCon _ _ (Embed (M.Poly t))) = snd <$> unbind t

-- TODO(chathhorn): shouldn't be necessary (should save this info before breaking out ctors)
getCtors :: MonadReader [M.DataDefn] m => Name M.TyConId -> m [Name M.DataConId]
getCtors n = asks (map projId <$> concatMap M.dataCons <$> filter isMine)
      where isMine :: M.DataDefn -> Bool
            isMine (M.DataDefn _ n' _ _) = n == n'

            projId :: M.DataCon -> Name M.DataConId
            projId (M.DataCon _ n _) = n

sizeof :: (Fresh m, MonadError AstError m, MonadReader [M.DataDefn] m, MonadState (HashMap M.Ty Int) m) => Annote -> M.Ty -> m Int
sizeof an t = do
      m <- get
      s <- case Map.lookup t m of
            Nothing -> case th of
                  M.TyApp {}     -> failAt an $ "ToCore: sizeof: Got TyApp after flattening (can't happen): " ++ prettyPrint t
                  M.TyCon _ c  -> do
                        ctors      <- getCtors c
                        ctorWidths <- mapM (ctorWidth t) ctors
                        pure $ ceilLog2 (length ctors) + maximum (0 : ctorWidths)
                  M.TyVar {}  -> pure 0
            Just s -> pure s
      put $ Map.insert t s m
      pure s
      where (th : _) = M.flattenTyApp t

ceilLog2 :: Int -> Int
ceilLog2 n | n < 1 = 0
ceilLog2 n = ceiling $ logBase 2 (fromIntegral n :: Double)

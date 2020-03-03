{-# LANGUAGE LambdaCase, FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.ToCore (toCore) where

import ReWire.Annotation
import ReWire.Error
import ReWire.Pretty
import ReWire.Unbound (Name, Fresh, runFreshM, Embed (..) , unbind)

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Control.Monad.State (StateT (..), MonadState, get, put)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, lift)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)

import qualified Data.HashMap.Strict as Map
import qualified ReWire.Core.Syntax  as C
import qualified ReWire.Crust.Syntax as M

type SizeMap = HashMap M.Ty Int
type ConMap = (HashMap (Name M.TyConId) [Name M.DataConId], HashMap (Name M.DataConId) M.Ty)
type TCM m = ReaderT ConMap (ReaderT (HashMap (Name M.Exp) Int) m)

toCore :: (Fresh m, MonadError AstError m) => M.FreeProgram -> m C.Program
toCore (ts, vs) = fst <$> flip runStateT mempty (do
      ts' <- concat <$> mapM (transData conMap) ts
      vs' <- mapM (transDefn conMap) $ filter (not . M.isPrim . M.defnName) vs
      pure $ C.Program ts' vs')
      where conMap :: ConMap
            conMap = ( Map.fromList $ map (M.dataName &&& map projId . M.dataCons) ts
                     , Map.fromList $ map (projId &&& projType) (concatMap M.dataCons ts)
                     )

            projId :: M.DataCon -> Name M.DataConId
            projId (M.DataCon _ n _) = n

            projType :: M.DataCon -> M.Ty
            projType (M.DataCon _ _ (Embed (M.Poly t))) = runFreshM (snd <$> unbind t)

-- getCtors n = asks (Map.lookup n)map projId <$> concatMap M.dataCons <$> filter isMine)
--       where isMine :: M.DataDefn -> Bool
--             isMine (M.DataDefn _ n' _ _) = n == n'
-- 
      -- where isMine :: M.DataCon -> Bool
      --       isMine (M.DataCon _ d' _) = d == d'


transData :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => ConMap -> M.DataDefn -> m [C.DataCon]
transData conMap (M.DataDefn _ _ _ cs) = mapM (transDataCon $ length cs) $ zip [0..] cs
      where transDataCon :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => Int -> (Int, M.DataCon) -> m C.DataCon
            transDataCon nctors (n, M.DataCon an c (Embed (M.Poly t))) = do
                  (_, t') <- unbind t
                  C.DataCon an (C.DataConId $ show c) n nctors <$> runReaderT (transType t') conMap

transDefn :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => ConMap -> M.Defn -> m C.Defn
transDefn conMap (M.Defn an n (Embed (M.Poly t)) _ (Embed e)) = do
      (_, t')  <- unbind t
      (xs, e') <- unbind e
      C.Defn an (show n) <$> runReaderT (transType t') conMap <*> runReaderT (runReaderT (transExp e') conMap) (Map.fromList $ zip xs [0..])

transExp :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => M.Exp -> TCM m C.Exp
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

transPat :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => M.MatchPat -> ReaderT ConMap m C.Pat
transPat = \ case
      M.MatchPatCon an t d ps -> C.PatCon an <$> transType t <*> pure (C.DataConId $ show d) <*> mapM transPat ps
      M.MatchPatVar an t      -> C.PatVar an <$> transType t

transType :: (Fresh m, MonadError AstError m, MonadState SizeMap m) => M.Ty -> ReaderT ConMap m C.Ty
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
apply s (M.TyApp an t1 t2) = M.TyApp an (apply s t1) $ apply s t2
apply s t@(M.TyVar _ _ i)  = fromMaybe t $ lookup (show i) s
apply _ t                  = t

ctorWidth :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState SizeMap m) => M.Ty -> Name M.DataConId -> m Int
ctorWidth t d = do
      let t'            =  snd $ M.flattenArrow t
      getCtorType d >>= \ case
            Just ct -> do
                  let (targs, tres) = M.flattenArrow ct
                  s                 <- matchTy (ann t') tres t'
                  sum <$> mapM (sizeof $ ann t) (map (apply s) targs)
            _ -> pure 0

-- TODO(chathhorn): shouldn't be necessary (should save this info before breaking out ctors)
getCtors :: MonadReader ConMap m => Name M.TyConId -> m [Name M.DataConId]
getCtors n = asks (fromMaybe [] . Map.lookup n . fst)

getCtorType :: MonadReader ConMap m => Name M.DataConId -> m (Maybe M.Ty)
getCtorType n = asks (Map.lookup n . snd)

sizeof :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState SizeMap m) => Annote -> M.Ty -> m Int
sizeof an t = do
      m <- get
      s <- case Map.lookup t m of
            Nothing -> case th of
                  M.TyApp {}     -> failAt an $ "ToCore: sizeof: Got TyApp after flattening (can't happen): " ++ prettyPrint t
                  M.TyCon _ c  -> do
                        ctors      <- getCtors c
                        ctorWidths <- mapM (ctorWidth t) ctors
                        pure $ ceilLog2 (length ctors) + maximum (0 : ctorWidths)
                  _  -> pure 0
            Just s -> pure s
      put $ Map.insert t s m
      pure s
      where (th : _) = M.flattenTyApp t

ceilLog2 :: Int -> Int
ceilLog2 n | n < 1 = 0
ceilLog2 n = ceiling $ logBase 2 (fromIntegral n :: Double)

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Crust.ToCore (toCore) where

import ReWire.Annotation
import ReWire.Error
import ReWire.Pretty
import ReWire.Unbound (Name, Fresh, runFreshM, Embed (..) , unbind, n2s)

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Control.Monad.State (StateT (..), MonadState, get, put)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, lift)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.List (findIndex)

import qualified Data.HashMap.Strict as Map
import qualified ReWire.Core.Syntax  as C
import qualified ReWire.Crust.Syntax as M

import TextShow (showt)

type SizeMap = HashMap M.Ty Int
type ConMap = (HashMap (Name M.TyConId) [Name M.DataConId], HashMap (Name M.DataConId) M.Ty)
type TCM m = ReaderT ConMap (ReaderT (HashMap (Name M.Exp) Int) m)

toCore :: (Fresh m, MonadError AstError m) => M.FreeProgram -> m C.Program
toCore (ts, _, vs) = fst <$> flip runStateT mempty (do
            vs' <- mapM (transDefn conMap) $ filter (not . M.isPrim . M.defnName) vs
            case partitionEithers vs' of
                  ([startDefn], defns) -> pure $ C.Program startDefn defns
                  _                    -> failAt noAnn "toCore: no Main.start."
      )
      where conMap :: ConMap
            conMap = ( Map.fromList $ map (M.dataName &&& map projId . M.dataCons) ts
                     , Map.fromList $ map (projId &&& projType) (concatMap M.dataCons ts)
                     )

            projId :: M.DataCon -> Name M.DataConId
            projId (M.DataCon _ n _) = n

            projType :: M.DataCon -> M.Ty
            projType (M.DataCon _ _ (Embed (M.Poly t))) = runFreshM (snd <$> unbind t)

transDefn :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => ConMap -> M.Defn -> m (Either C.StartDefn C.Defn)
transDefn conMap (M.Defn an n (Embed (M.Poly t)) _ (Embed e)) | n2s n == "Main.start" = do
      (_, t')  <- unbind t
      case t' of
            M.TyApp _ (M.TyApp _ (M.TyApp _ (M.TyApp _ (M.TyCon _ t_ret) t_in) t_out) (M.TyCon _ t_i)) t_res 
                        | n2s t_ret == "ReT" && n2s t_i == "I" -> do
                  (_, e') <- unbind e
                  Left <$> (C.StartDefn an <$> runReaderT (transType t_in) conMap
                                           <*> runReaderT (transType t_out) conMap
                                           <*> runReaderT (transType t_res) conMap
                                           <*> runReaderT (runReaderT (transExp e') conMap) mempty)
            _ -> failAt an $ "transDefn: Main.start has illegal type: " <> prettyPrint t'
transDefn conMap (M.Defn an n (Embed (M.Poly t)) _ (Embed e)) = do
      (_, t')  <- unbind t
      (xs, e') <- unbind e
      Right <$> (C.Defn an (showt n) <$> runReaderT (transType t') conMap <*> runReaderT (runReaderT (transExp e') conMap) (Map.fromList $ zip xs [0..]))

transExp :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => M.Exp -> TCM m C.Exp
transExp = \ case
      e@(M.App an _ _)                  -> case M.flattenApp e of
            (M.Var _ _ x : args)        -> C.Call an <$> transType (M.typeOf e) <*> pure (showt x) <*> mapM transExp args
            (M.Con an t d : args)        -> C.Con an <$> transType (M.typeOf e) <*> ctorId an (snd $ M.flattenArrow t) d <*> mapM transExp args
            (M.NativeVHDL _ s _ : args) -> C.NativeVHDL an <$> transType (M.typeOf e) <*> pure s <*> mapM transExp args
            _                                          -> failAt an "transExp: encountered ill-formed application."
      M.Var an t x                      -> lift (asks (Map.lookup x)) >>= \ case
            Nothing -> C.Call an <$> transType t <*> pure (showt x) <*> pure []
            Just i  -> C.LVar an <$> transType t <*> pure i
      M.Con an t d                      -> C.Con an <$> transType t <*> ctorId an (snd $ M.flattenArrow t) d <*> pure []
      M.Match an t e p f as (Just e2)   -> C.Match an <$> transType t <*> transExp e <*> transPat p <*> (toGId =<< transExp f) <*> mapM (toLId <=< transExp) as <*> (Just <$> transExp e2)
      M.Match an t e p f as Nothing     -> C.Match an <$> transType t <*> transExp e <*> transPat p <*> (toGId =<< transExp f) <*> mapM (toLId <=< transExp) as <*> pure Nothing
      M.NativeVHDL an s (M.Error _ t _) -> C.NativeVHDL an <$> transType t <*> pure s <*> pure []
      M.Error an t _                    -> C.Call an <$> transType t <*> pure "ERROR" <*> pure []
      e                                 -> failAt (ann e) $ "ToCore: unsupported expression: " <> prettyPrint e
      where toGId :: MonadError AstError m => C.Exp -> m C.GId
            toGId (C.Call _ _ x _) = pure x
            toGId e                = failAt (ann e) $ "toGId: expected Call, got: " <> prettyPrint e

            toLId :: MonadError AstError m => C.Exp -> m C.LId
            toLId (C.LVar _ _ x) = pure x
            toLId e              = failAt (ann e) $ "toLId: expected LVar, got: " <> prettyPrint e

transPat :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => M.MatchPat -> ReaderT ConMap m C.Pat
transPat = \ case
      M.MatchPatCon an t d ps -> C.PatCon an <$> transType t <*> ctorId an t d <*> mapM transPat ps
      M.MatchPatVar an t      -> C.PatVar an <$> transType t

transType :: (Fresh m, MonadError AstError m, MonadState SizeMap m) => M.Ty -> ReaderT ConMap m C.Ty
transType t = case t of
      M.TyApp an _ _ -> C.Ty an <$> mapM (sizeof an) (fst $ M.flattenArrow t) <*> sizeof an (snd $ M.flattenArrow t)
      M.TyCon an _   -> C.Ty an [] <$> sizeof an t
      _              -> pure $ C.Ty (ann t) [] 0

matchTy :: MonadError AstError m => Annote -> M.Ty -> M.Ty -> m TySub
matchTy an (M.TyApp _ t1 t2) (M.TyApp _ t1' t2') = do
      s1 <- matchTy an t1 t1'
      s2 <- matchTy an t2 t2'
      merge an s1 s2
matchTy _ (M.TyVar _ _ v) t                    = pure [(showt v, t)]
matchTy _ _ _ = pure []

merge :: MonadError AstError m => Annote -> TySub -> TySub -> m TySub
merge an s'  = \ case
      []          -> pure s'
      (v, t) : s -> case lookup v s' of
            Nothing           -> ((v, t) :) <$> merge an s' s
            Just t' | t == t' -> merge an s' s
            Just t'           -> failAt an
                  $ "ToCore: merge: inconsistent assignment of tyvar " <> v
                  <> ": " <> prettyPrint t <> " vs. " <> prettyPrint t'

type TySub = [(Text, M.Ty)]

apply :: TySub -> M.Ty -> M.Ty
apply s (M.TyApp an t1 t2) = M.TyApp an (apply s t1) $ apply s t2
apply s t@(M.TyVar _ _ i)  = fromMaybe t $ lookup (showt i) s
apply _ t                  = t

ctorWidth :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState SizeMap m) => M.Ty -> Name M.DataConId -> m Int
ctorWidth t d = do
      let t'            =  snd $ M.flattenArrow t
      getCtorType d >>= \ case
            Just ct -> do
                  let (targs, tres) = M.flattenArrow ct
                  s                <- matchTy (ann t') tres t'
                  sum <$> mapM (sizeof $ ann t) (map (apply s) targs)
            _ -> pure 0

-- TODO(chathhorn): shouldn't be necessary (should save this info before breaking out ctors)
getCtors :: MonadReader ConMap m => Name M.TyConId -> m [Name M.DataConId]
getCtors n = asks (fromMaybe [] . Map.lookup n . fst)

getCtorType :: MonadReader ConMap m => Name M.DataConId -> m (Maybe M.Ty)
getCtorType n = asks (Map.lookup n . snd)

ctorId :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState SizeMap m) => Annote -> M.Ty -> Name M.DataConId -> m C.DataConId
ctorId an t d = case th of
      M.TyCon _ c | n2s c == "ReT" -> pure $ C.DataConId "ReT" 0 0 -- TODO(chathhorn): ?
      M.TyCon _ c                  -> do
            ctors      <- getCtors c
            case findIndex ((== n2s d) . n2s) ctors of
                  Just idx -> pure $ C.DataConId (showt d) idx (ceilLog2 $ length ctors)
                  Nothing  -> failAt an $ "ToCore: ctorId: unknown ctor: " <> prettyPrint (n2s d) <> " of type " <> prettyPrint (n2s c)
      _                            -> failAt an $ "ToCore: ctorId: unexpected type: " <> prettyPrint t
      where (th : _) = M.flattenTyApp t

sizeof :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState SizeMap m) => Annote -> M.Ty -> m Int
sizeof an t = do
      m <- get
      s <- case Map.lookup t m of
            Nothing -> case th of
                  M.TyApp {}                   -> failAt an $ "ToCore: sizeof: Got TyApp after flattening (can't happen): " <> prettyPrint t
                  M.TyCon _ c | n2s c == "ReT" -> pure 1 -- TODO(chathhorn): ?
                  M.TyCon _ c                  -> do
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

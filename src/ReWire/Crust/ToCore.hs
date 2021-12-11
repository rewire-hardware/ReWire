{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Crust.ToCore (toCore) where

import ReWire.Annotation
import ReWire.Error
import ReWire.Pretty
import ReWire.Unbound (Name, Fresh, runFreshM, Embed (..) , unbind, n2s)

import Control.Arrow ((&&&))
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
                  case e' of
                        M.App _ (M.App _ (M.Var _ _ (n2s -> "unfold")) (M.Var _ loopTy loop)) (M.Var _ state0Ty state0) -> do
                              Left <$> (C.StartDefn an <$> runReaderT (sizeOf an t_in) conMap
                                                       <*> runReaderT (sizeOf an t_out) conMap
                                                       <*> runReaderT (sizeOf an t_res) conMap
                                                       <*> ((n2s loop, )   <$> runReaderT (transType loopTy) conMap)
                                                       <*> ((n2s state0, ) <$> runReaderT (transType state0Ty) conMap))
                        _ -> failAt an $ "transDefn: definition of Main.start must have form `Main.start = unfold n m' where n and m are global IDs; got " <> prettyPrint e'
            _ -> failAt an $ "transDefn: Main.start has illegal type: " <> prettyPrint t'
transDefn conMap (M.Defn an n (Embed (M.Poly t)) _ (Embed e)) = do
      (_, t')  <- unbind t
      (xs, e') <- unbind e
      Right <$> (C.Defn an (showt n) <$> runReaderT (transType t') conMap <*> runReaderT (runReaderT (transExp e') conMap) (Map.fromList $ zip xs [0..]))

transExp :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => M.Exp -> TCM m C.Exp
transExp = \ case
      e@(M.App an _ _)                  -> case M.flattenApp e of
            (M.Var _ _ x : args)        -> do
                  t'    <- sizeOf an $ M.typeOf e
                  args' <- mapM transExp args
                  pure $ C.Match an t' (showt x) (C.Slice an args') (map (C.PatVar an . C.sizeOf) args') Nothing
            (M.Con an t d : args)       -> do
                  (v, w) <- ctorId an (snd $ M.flattenArrow t) d
                  sz     <- sizeOf an $ M.typeOf e
                  szArgs <- sum <$> mapM (sizeOf an . M.typeOf) args
                  let tag = C.Lit an w v
                      pad = C.Lit an (sz - w - szArgs) 0
                  C.Slice an <$> (([tag, pad] <>) <$> mapM transExp args)
            (M.NativeVHDL _ s _ : args) -> C.NativeVHDL an <$> sizeOf an (M.typeOf e) <*> pure s <*> mapM transExp args
            _                                          -> failAt an "transExp: encountered ill-formed application."
      M.Var an t x                      -> lift (asks (Map.lookup x)) >>= \ case
            Nothing -> C.Match an <$> sizeOf an t <*> pure (showt x) <*> pure (C.Lit an 0 0) <*> pure [C.PatLit an 0 0] <*> pure Nothing
            Just i  -> C.LVar an  <$> sizeOf an t <*> pure i
      M.Con an t d                      -> do
            (v, w) <- ctorId an t d
            sz     <- sizeOf an t
            let tag = C.Lit an w v
                pad = C.Lit an (sz - w) 0
            pure $ C.Slice an [tag, pad]
      M.Match an t e ps f (Just e2)      -> C.Match an <$> sizeOf an t <*> (toGId =<< transExp f) <*> transExp e <*> transPat ps <*> (Just <$> transExp e2)
      M.Match an t e ps f Nothing        -> C.Match an <$> sizeOf an t <*> (toGId =<< transExp f) <*> transExp e <*> transPat ps <*> pure Nothing
      M.NativeVHDL an s (M.Error _ t _) -> C.NativeVHDL an <$> sizeOf an t <*> pure s <*> pure []
      M.Error an t _                    -> C.NativeVHDL an <$> sizeOf an t <*> pure "error" <*> pure []
      e                                 -> failAt (ann e) $ "ToCore: unsupported expression: " <> prettyPrint e
      where toGId :: MonadError AstError m => C.Exp -> m C.GId
            toGId (C.Match _ _ x _ _ _) = pure x
            toGId e                     = failAt (ann e) $ "toGId: expected Match, got: " <> prettyPrint e

transPat :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => M.MatchPat -> ReaderT ConMap m [C.Pat]
transPat = \ case
      M.MatchPatCon an t d ps -> do
            (v, w) <- ctorId an t d
            sz     <- sizeOf an t
            szArgs <- sum <$> mapM (sizeOf an . M.typeOf) ps
            let tag = C.PatLit an w v
                pad = C.PatWildCard an (sz - w - szArgs) -- or lit 0 bits?
            ([tag, pad] <>) <$> (concat <$> mapM transPat ps)
      M.MatchPatVar an t      -> pure <$> (C.PatVar an <$> sizeOf an t)
      M.MatchPatWildCard an t -> pure <$> (C.PatWildCard an <$> sizeOf an t)

transType :: (Fresh m, MonadError AstError m, MonadState SizeMap m) => M.Ty -> ReaderT ConMap m C.Sig
transType t = case t of
      M.TyApp an _ _ -> C.Sig an <$> mapM (sizeOf an) (fst $ M.flattenArrow t) <*> sizeOf an (snd $ M.flattenArrow t)
      M.TyCon an _   -> C.Sig an [] <$> sizeOf an t
      _              -> pure $ C.Sig (ann t) [] 0

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
                  sum <$> mapM (sizeOf $ ann t) (map (apply s) targs)
            _ -> pure 0

-- TODO(chathhorn): shouldn't be necessary (should save this info before breaking out ctors)
getCtors :: MonadReader ConMap m => Name M.TyConId -> m [Name M.DataConId]
getCtors n = asks (fromMaybe [] . Map.lookup n . fst)

getCtorType :: MonadReader ConMap m => Name M.DataConId -> m (Maybe M.Ty)
getCtorType n = asks (Map.lookup n . snd)

ctorId :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState SizeMap m) => Annote -> M.Ty -> Name M.DataConId -> m (Int, Int)
ctorId an t d = case th of
      -- M.TyCon _ c | n2s c == "ReT" -> pure $ C.DataConId "ReT" 0 0 -- TODO(chathhorn): ?
      M.TyCon _ c                  -> do
            ctors      <- getCtors c
            case findIndex ((== n2s d) . n2s) ctors of
                  Just idx -> pure (idx, ceilLog2 $ length ctors)
                  Nothing  -> failAt an $ "ToCore: ctorId: unknown ctor: " <> prettyPrint (n2s d) <> " of type " <> prettyPrint (n2s c)
      _                            -> failAt an $ "ToCore: ctorId: unexpected type: " <> prettyPrint t
      where (th : _) = M.flattenTyApp t

sizeOf :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState SizeMap m) => Annote -> M.Ty -> m Int
sizeOf an t = do
      m <- get
      s <- case Map.lookup t m of
            Nothing -> case th of
                  M.TyApp {}                   -> failAt an $ "ToCore: sizeOf: Got TyApp after flattening (can't happen): " <> prettyPrint t
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

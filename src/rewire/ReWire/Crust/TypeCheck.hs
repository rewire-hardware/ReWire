{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
--
-- This type checker is based loosely on Mark Jones's "Typing Haskell in
-- Haskell", though since we don't have type classes in core it is much
-- simpler.
--
module ReWire.Crust.TypeCheck (typeCheck, typeCheckDefn, untype, unify, unify', TySub) where

import ReWire.Annotation
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Fix (fixOn, fixOn')
import ReWire.Unbound (fresh, substs, Subst, n2s, s2n, unsafeUnbind)
import ReWire.Pretty
import ReWire.Crust.Syntax
import ReWire.SYB (runPureT, transform, runQ, query)

import Control.DeepSeq (deepseq, force)
import Control.Monad (zipWithM, foldM, mplus, when)
import Control.Monad.Catch (MonadCatch (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), local, asks)
import Control.Monad.State (evalStateT, gets, modify, MonadState)
import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (hash))
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Numeric.Natural (Natural)
import TextShow (TextShow (..))

import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set

-- import Debug.Trace (trace)
-- import Data.Text (unpack)

subst :: Subst b a => HashMap (Name b) b -> a -> a
subst = substs . Map.toList

-- Type checker for core.

type TySub = HashMap (Name Ty) Ty
type Concretes = HashMap (Name Exp, Ty) (Name Exp)
data TCEnv = TCEnv
      { as        :: !(HashMap (Name Exp) Poly)
      , cas       :: !(HashMap (Name DataConId) Poly)
      } deriving Show

instance Semigroup TCEnv where
      TCEnv a b <> TCEnv a' b' = TCEnv (a <> a') $ b <> b'

instance Monoid TCEnv where
      mempty = TCEnv mempty mempty

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll a = map snd . filter ((== a) . fst)

typeCheckDefn :: (Fresh m, MonadError AstError m) => [DataDefn] -> [Defn] -> Defn -> m Defn
typeCheckDefn ts vs d = runReaderT (withAssumps ts vs $ tcDefn "" d) mempty

typeCheck :: (Fresh m, MonadError AstError m, MonadCatch m) => Text -> FreeProgram -> m FreeProgram
typeCheck start (ts, syns, vs) = (ts, syns, ) <$> runReaderT tc mempty
      where conc :: Concretes -> Defn -> [Defn]
            conc cs (Defn an n _ b e) = mapMaybe conc' $ lookupAll n $ Map.keys cs
                  where conc' :: Ty -> Maybe Defn
                        conc' t = do
                              n' <- Map.lookup (n, t) cs
                              pure $ Defn an n' ([] |-> t) b e

            tc :: (MonadCatch m, Fresh m, MonadError AstError m, MonadReader TCEnv m) => m [Defn]
            tc = do
                  vs' <- withAssumps ts vs $ mapM (tcDefn start) vs
                  cs  <- concretes vs' -- TODO(chathhorn): don't think this needs to use fix.
                  fst . fst <$> fixOn (Map.keys . snd) "polymorphic function instantiation" 10 tc' ((vs', mempty), cs)

            tc' :: (MonadCatch m, Fresh m, MonadError AstError m, MonadReader TCEnv m) => (([Defn], Concretes), Concretes) -> m (([Defn], Concretes), Concretes)
            tc' ((acc, cs), cs') = do
                  let ep = concatMap (conc cs') polyDefs
                  ep' <- concretize (cs <> cs') <$> withAssumps ts (acc <> ep) (mapM (tcDefn start) ep)
                  ((concretize (cs <> cs') acc <> ep', cs <> cs'), ) <$> concretes ep'

            concretes :: Fresh m => [Defn] -> m Concretes
            concretes = foldM (\ m c -> Map.insert c <$> fresh (fst c) <*> pure m) mempty . uses

            polys :: Set (Name Exp)
            polys = foldr polys' mempty vs
                  where polys' :: Defn -> Set (Name Exp) -> Set (Name Exp)
                        polys' d | isPoly d  = Set.insert $ defnName d
                                 | otherwise = id

            polyDefs :: [Defn]
            polyDefs = foldr polys' mempty vs
                  where polys' :: Defn -> [Defn] -> [Defn]
                        polys' d | isPoly d  = (d :)
                                 | otherwise = id

            isPoly :: Defn -> Bool
            isPoly (Defn _ _ (Embed (Poly (unsafeUnbind -> (_, t)))) _ _) = not $ concrete t

            uses :: Data a => a -> Set (Name Exp, Ty)
            uses = runQ (query $ \ case
                  Var _ t n | concrete t, Set.member n polys -> Set.singleton (n, unAnn t)
                  _                                          -> mempty)

            concretize :: Data d => Concretes -> d -> d
            concretize cs = runIdentity . runPureT (transform $ \ case
                  v@(Var an t n) -> pure $ maybe v (Var an t) $ Map.lookup (n, unAnn t) cs
                  e              -> pure e)

freshv :: Fresh m => m Ty
freshv = TyVar (MsgAnnote "TypeCheck: freshv") kblank <$> fresh (s2n "?")

tsUnion :: TySub -> TySub -> TySub
tsUnion ts = foldr insert ts . Map.toList
      where insert :: (Name Ty, Ty) -> TySub -> TySub
            insert (v, t) ts' | Just t' <- Map.lookup v ts', Just s <- mgu t t' = Map.insert v (subst s t) ts' `tsUnion` s
                              | otherwise                                       = Map.insert v t ts'

mguNats :: (Natural, [Name Ty]) -> (Natural, [Name Ty]) -> Maybe TySub
mguNats ns      ns'       | ns == ns' = pure mempty
mguNats (n, []) (n', [])  | n /= n'   = Nothing
mguNats (n, vs) (n', vs') | n >= n'   = mguNats' (n - n', sort vs)  $ sort vs'
                          | otherwise = mguNats' (n' - n, sort vs') $ sort vs
      where mguNats' :: (Natural, [Name Ty]) -> [Name Ty] -> Maybe TySub
            mguNats' (0, us)  []                                 = pure $ mkSub0 us
            mguNats' (0, [])  vs                                 = pure $ mkSub0 vs
            mguNats' (0, [u]) [v]               | hash (show u) > hash (show v)
                                                                 = pure $ mkSubV u v
            mguNats' (0, [u]) [v]                                = pure $ mkSubV v u
            mguNats' (0, [u]) vs                | u `notElem` vs = pure $ Map.fromList [(u, foldr1 (plusTy noAnn) $ map (TyVar noAnn KNat) vs)]
            mguNats' (n, us)  [v]               | v `notElem` us = pure $ Map.fromList [(v, foldr (plusTy noAnn . TyVar noAnn KNat) (TyNat noAnn n) us)]

            mguNats' (n, []) vs@(v : _)         | dups <- fromIntegral $ length $ takeWhile (== v) vs
                                                                 = tsUnion (mkSubN v $ n `div` dups) <$> mguNats' (n `mod` dups, []) (drop (fromIntegral dups) vs)
            mguNats' (n, u : us) (v : vs)       | u == v         = mguNats' (n, us) vs


            mguNats' (n, us@(u : _)) vs@(v : _) | hash (show u) > hash (show v)
                                                , dups <- length $ takeWhile (== u) us
                                                                 = tsUnion (mkSubV u v) <$> mguNats' (n, take dups (repeat v) <> drop dups us) vs
            mguNats' (n, us@(u : _)) vs@(v : _) | dups <- length $ takeWhile (== v) vs
                                                                 = tsUnion (mkSubV v u) <$> mguNats' (n, us) (take dups (repeat u) <> drop dups vs)
            mguNats' _ _                                         = Nothing

            mkSubV :: Name Ty -> Name Ty -> TySub
            mkSubV u v = Map.fromList [(u, TyVar noAnn KNat v)]

            mkSubN :: Name Ty -> Natural -> TySub
            mkSubN u n = Map.fromList [(u, TyNat noAnn n)]

            mkSub0 :: [Name Ty] -> TySub
            mkSub0 = Map.fromList . map (, TyNat noAnn 0) . nubOrd

mgu :: Ty -> Ty -> Maybe TySub
mgu t                    t'                          | unAnn t == unAnn t'           = pure mempty
mgu (dstNats -> Just ns) (dstNats -> Just ns')                                       = mguNats ns ns'

mgu (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) ti ) to )
    (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) ti') to')                           = do
      s1 <- mgu iTy ti
      s2 <- tsUnion s1 <$> mgu (subst s1 ti) (subst s1 ti')
      s3 <- tsUnion s2 <$> mgu (subst s2 oTy) (subst s2 to)
      tsUnion s3 <$> mgu (subst s3 to) (subst s3 to')

mgu (TyApp _ tl tr)                 (TyApp _ tl' tr')                                = do
      let fwd = mgu tl tl' >>= \ s -> tsUnion s <$> mgu (subst s tr) (subst s tr')
          rev = mgu tr tr' >>= \ s -> tsUnion s <$> mgu (subst s tl) (subst s tl')
      fwd `mplus` rev

mgu (TyCon _ c1)                    (TyCon _ c2)     | n2s c1 == n2s c2              = pure mempty

mgu (TyVar _ _ u)                   tv@(TyVar _ _ v) | hash (show u) > hash (show v) = pure $ Map.fromList [(u, tv)]
mgu tu@TyVar {}                     (TyVar _ _ v)                                    = pure $ Map.fromList [(v, tu)]

mgu (TyVar _ _ u)                   t                | u `notElem` fv t              = pure $ Map.fromList [(u, t)]
mgu t                               (TyVar _ _ u)    | u `notElem` fv t              = pure $ Map.fromList [(u, t)]

mgu _                                _                                               = Nothing
--      trace ("     MGU: " <> unpack (prettyPrint t1)
--        <> "\n    with: " <> unpack (prettyPrint t2)) $
--      trace ("      t1: " <> show (unAnn t1)) $
--      trace ("      t2: " <> show (unAnn t2)) $ Nothing

dstNats :: Ty -> Maybe (Natural, [Name Ty])
dstNats t = foldM dstNat (0, []) $ plus' t
      where dstNat :: (Natural, [Name Ty]) -> Ty -> Maybe (Natural, [Name Ty])
            dstNat (n, ts) = \ case
                  TyNat _ n'  -> pure (n + n', ts)
                  TyVar _ _ v -> pure (n, ts <> [v])
                  _           -> Nothing

plus' :: Ty -> [Ty]
plus' t | Just (a, b) <- plus t = plus' a <> plus' b
        | otherwise             = [t]

unify' :: Ty -> Ty -> Maybe Ty
unify' t1 t2 = subst <$> mgu t1 t2 <*> pure t1

unify :: (MonadError AstError m, MonadState TySub m) => Annote -> Ty -> Ty -> m Ty
unify an t1 t2 = do
      t1' <- gets $ flip subst t1
      t2' <- gets $ flip subst t2
      -- trace ("Unifying: " <> unpack (prettyPrint t1')
      --   <> "\n    with: " <> unpack (prettyPrint t2')) $ pure ()
      -- trace ("     t1': " <> show (unAnn t1')) $ pure ()
      -- trace ("     t2': " <> show (unAnn t2')) $ pure ()
      case mgu t1' t2' of
            Just s -> do
                  modify $ tsUnion s
                  gets $ flip subst t1'
            _      -> failAt an $ "Types do not unify. Expected and got, respectively:\n"
                              <> prettyPrint t1' <> "\n"
                              <> prettyPrint t2'

inst :: Fresh m => Poly -> m Ty
inst (Poly pt) = snd <$> unbind pt

patAssumps :: Pat -> HashMap (Name Exp) Poly
patAssumps = flip patAssumps' mempty
      where patAssumps' :: Pat -> HashMap (Name Exp) Poly -> HashMap (Name Exp) Poly
            patAssumps' = \ case
                  PatCon _ _ _ ps      -> flip (foldr patAssumps') ps
                  PatVar _ (Embed t) n -> Map.insert n $ [] `poly` t
                  PatWildCard _ _      -> id

patHoles :: Fresh m => MatchPat -> m (HashMap (Name Exp) Poly)
patHoles = flip patHoles' $ pure mempty
      where patHoles' :: Fresh m => MatchPat -> m (HashMap (Name Exp) Poly) -> m (HashMap (Name Exp) Poly)
            patHoles' = \ case
                  MatchPatCon _ _ _ ps -> flip (foldr patHoles') ps
                  MatchPatVar _ t      -> (flip Map.insert ([] `poly` t) <$> fresh (s2n "PHOLE") <*>)
                  MatchPatWildCard _ _ -> id

tcPatCon :: (MonadReader TCEnv m, MonadState TySub m, Fresh m, MonadError AstError m) => Annote -> Ty -> (Ty -> pat -> m pat) -> Name DataConId -> [pat] -> m ([pat], Ty)
tcPatCon an t tc i ps = do
      cas     <- asks cas
      case Map.lookup i cas of
            Nothing  -> failAt an $ "Unknown constructor: " <> prettyPrint i
            Just pta -> do
                  ta               <- inst pta
                  let (targs, tres) = flattenArrow ta
                  when (length ps /= length targs) $ failAt an "Pattern is not applied to enough arguments"
                  (,) <$> zipWithM tc targs ps <*> unify an tres t

tcPat :: (Fresh m, MonadError AstError m, MonadReader TCEnv m, MonadState TySub m) => Ty -> Pat -> m Pat
tcPat t = \ case
      PatCon an _ (Embed i) ps -> do
            (ps', t') <- tcPatCon an t tcPat i ps
            pure $ PatCon an (Embed t') (Embed i) ps'
      PatVar an _ x            -> pure $ PatVar an (Embed t) x
      PatWildCard an _         -> pure $ PatWildCard an (Embed t)

tcMatchPat :: (Fresh m, MonadError AstError m, MonadReader TCEnv m, MonadState TySub m) => Ty -> MatchPat -> m MatchPat
tcMatchPat t = \ case
      MatchPatCon an _ i ps -> do
            (ps', t') <- tcPatCon an t tcMatchPat i ps
            pure $ MatchPatCon an t' i ps'
      MatchPatVar an _      -> pure $ MatchPatVar an t
      MatchPatWildCard an _ -> pure $ MatchPatWildCard an t

tcExp :: (Fresh m, MonadError AstError m, MonadReader TCEnv m, MonadState TySub m) => Exp -> m Exp
tcExp eye = case eye of
      App an _ e1 e2 | isFromList e1 -> do
            -- trace "tc: fromList" $ pure ()
            e2' <- tcExp e2
            case litListElems e2' of -- TODO: discards type annotation
                  Nothing -> failAt an "fromList: argument not a list literal."
                  -- Note: we instantiate LitVec here. TODO(chathhorn): move this to the inlining pass?
                  Just es -> tcExp $ LitVec an (TyBlank an) es
      App an _ e1 e2 -> do
            -- trace "tc: app" $ pure ()
            e1' <- tcExp e1
            e2' <- tcExp e2
            tvr <- freshv
            -- trace ("tc: app: e2':\n" <> show (unAnn e2')) $ pure ()
            t   <- unify an (typeOf e1') (typeOf e2' `arr` tvr)
            pure $ App an (arrowRight t) e1' e2'
      Lam an _ e -> do
            -- trace "tc: lam" $ pure ()
            (x, e') <- unbind e
            tvx     <- freshv
            e''     <- localAssumps (Map.insert x (poly [] tvx)) $ tcExp e'
            pure $ Lam an tvx $ bind x e''
      Var an _ v -> do
            -- trace "tc: var" $ pure ()
            as <- asks as
            case Map.lookup v as of
                  Nothing -> failAt an $ "Unknown variable: " <> showt v
                  Just pt -> do
                        t <- inst pt
                        pure $ Var an t v
      Con an _ i -> do
            -- trace "tc: con" $ pure ()
            cas <- asks cas
            case Map.lookup i cas of
                  Nothing -> failAt an $ "Unknown constructor: " <> prettyPrint i
                  Just pt -> do
                        t <- inst pt
                        pure $ Con an t i
      Case an _ e e1 e2 -> do
            -- trace "tc: case" $ pure ()
            e'       <- tcExp e
            (p, e1') <- unbind e1
            p'       <- tcPat (typeOf e') p
            let as   = patAssumps p'
            e1''     <- localAssumps (`Map.union` as) $ tcExp e1'
            case e2 of
                  Nothing -> pure $ Case an (typeOf e1'') e' (bind p' e1'') Nothing
                  Just e2 -> do
                        e2' <- tcExp e2
                        t <- unify an (typeOf e1'') $ typeOf e2'
                        pure $ Case an t e' (bind p' e1'') (Just e2')
      Match an _ e p f e2 -> do
            -- trace "tc: match" $ pure ()
            e'    <- tcExp e
            p'    <- tcMatchPat (typeOf e') p
            holes <- patHoles p'
            br    <- localAssumps (`Map.union` holes) $ tcExp $ mkApp' an f $ map fst $ Map.toList holes
            case e2 of
                  Nothing -> pure $ Match an (typeOf br) e' p' f Nothing
                  Just e2 -> do
                        e2' <- tcExp e2
                        t'' <- unify an (typeOf br) $ typeOf e2'
                        pure $ Match an t'' e' p' f (Just e2')
      Builtin an _ b -> do
            -- trace ("tc: builtin: " <> show b) $ pure ()
            as <- asks as
            case Map.lookup (s2n $ builtinName b) as of
                  Nothing -> failAt an $ "Unknown builtin: " <> builtinName b
                  Just pt -> do
                        t <- inst pt
                        pure $ Builtin an t b
      e@LitInt {} -> pure e
      e@LitStr {} -> pure e
      LitList an _ es -> do
            -- trace "tc: LitList" $ pure ()
            tv  <- freshv
            es' <- mapM tcExp es
            t' <- listTy an <$> foldM (unify an) tv (typeOf <$> es')
            pure $ LitList an t' es'
      LitVec an _ es -> do
            -- trace "tc: LitVec" $ pure ()
            tv  <- freshv
            es' <- mapM tcExp es
            t' <- vecTy an (TyNat an $ fromInteger $ toInteger $ length es) <$> foldM (unify an) tv (typeOf <$> es')
            pure $ LitVec an t' es'
      TypeAnn an pt e -> do
            -- trace "tc: TypeAnn" $ pure ()
            e' <- tcExp e
            ta <- inst pt
            _  <- unify an ta $ typeOf e'
            pure $ TypeAnn an pt e'

      where isFromList :: Exp -> Bool
            isFromList e = case unTyAnn e of
                  Builtin _ _ VecFromList -> True
                  _                       -> False

            litListElems :: Exp -> Maybe [Exp]
            litListElems e = case unTyAnn e of
                  LitList _ _ es -> pure es
                  _              -> Nothing

mkApp' :: Annote -> Exp -> [Name Exp] -> Exp
mkApp' an f holes = mkApp an f $ map (Var an $ TyBlank an) holes

tcDefn :: (Fresh m, MonadError AstError m, MonadReader TCEnv m) => Text -> Defn -> m Defn
tcDefn start d  = flip evalStateT mempty $ do
      -- trace ("tcDefn: " <> show (defnName d)) $ pure ()
      let Defn an n (Embed pt) b (Embed e) = force d
      t'       <- inst pt
      (vs, e') <- unbind e
      let (targs, _) = flattenArrow t'
      -- trace ("tcDefn body: " <> unpack (prettyPrint (unAnn $ untype e'))) $ pure ()
      e'' <- localAssumps (`Map.union` (Map.fromList $ zip vs $ map (poly []) targs)) $ tcExp e'
      -- trace ("e'':\n" <> show (unAnn e'')) $ pure ()

      startTy <- inst globalStartTy

      t'' <- if isStart $ defnName d then unify an startTy t' else pure t'
      _   <- unify an (iterate arrowRight t'' !! length vs) $ typeOf e''

      -- subs <- gets unAnn
      -- trace ("prefixed:\n" <> unpack (prettyPrint  (Map.toList subs))) $ pure ()
      modify tsFix
      -- subs <- gets unAnn
      -- trace ("fixed:\n" <> unpack (prettyPrint  (Map.toList subs))) $ pure ()
      e''' <- gets $ flip subst e''

      let d'  = Defn an n (Embed pt) b $ Embed $ bind vs e'''
      d' `deepseq` pure d'
      where isStart :: Name Exp -> Bool
            isStart = (== start) . n2s

-- | Calculates the transitive closure of the type var unifier set.
tsFix :: TySub -> TySub
tsFix = fixOn' norm subst'
      where subst' :: TySub -> TySub
            subst' s = Map.map (subst s) s

            norm :: TySub -> Int
            norm = hash . Map.map unAnn

withAssumps :: (MonadError AstError m, MonadReader TCEnv m) => [DataDefn] -> [Defn] -> m a -> m a
withAssumps ts vs = localAssumps (`Map.union` as) . localCAssumps (`Map.union` cas)
      where as  = foldr defnAssump mempty vs
            cas = foldr dataDeclAssumps mempty ts

            defnAssump :: Defn -> HashMap (Name Exp) Poly -> HashMap (Name Exp) Poly
            defnAssump (Defn _ n (Embed pt) _ _) = Map.insert n pt

            dataDeclAssumps :: DataDefn -> HashMap (Name DataConId) Poly -> HashMap (Name DataConId) Poly
            dataDeclAssumps (DataDefn _ _ _ cs) = flip (foldr dataConAssump) cs

            dataConAssump :: DataCon -> HashMap (Name DataConId) Poly -> HashMap (Name DataConId) Poly
            dataConAssump (DataCon _ i (Embed t)) = Map.insert i t

localAssumps :: (MonadReader TCEnv m, MonadError AstError m) => (HashMap (Name Exp) Poly -> HashMap (Name Exp) Poly) -> m a -> m a
localAssumps f = local (\ tce -> tce { as = f (as tce) })

localCAssumps :: (MonadReader TCEnv m, MonadError AstError m) => (HashMap (Name DataConId) Poly -> HashMap (Name DataConId) Poly) -> m a -> m a
localCAssumps f = local (\ tce -> tce { cas = f (cas tce) })

untype :: Data d => d -> d
untype = runIdentity . runPureT (transform $ \ (t :: Ty) -> pure $ TyBlank $ ann t)

iTy :: Ty
iTy = TyVar (MsgAnnote "ReacT input type.") KStar (s2n "?i")

oTy :: Ty
oTy = TyVar (MsgAnnote "ReacT output type.") KStar (s2n "?o")

globalReacT :: Ty -> Ty -> Ty
globalReacT s = TyApp an $ TyApp an (TyApp an (TyApp an (TyCon an $ s2n "ReacT") iTy) oTy) s
      where an :: Annote
            an = MsgAnnote "Expected ReacT type."

globalStartTy :: Poly
globalStartTy = poly [a] $ globalReacT (TyCon an $ s2n "Identity") $ TyVar an KStar a
      where an :: Annote
            an = MsgAnnote "Expected start function type."

            a :: Name Ty
            a = s2n "a"

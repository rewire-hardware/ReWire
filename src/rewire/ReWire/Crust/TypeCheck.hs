{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.TypeCheck (typeCheck, typeCheckDefn, untype, unify, unify', TySub) where

import ReWire.Annotation (Annote (MsgAnnote), unAnn, ann, noAnn)
import ReWire.Crust.Syntax (Exp (..), Ty (..), Kind (..), Poly (..), Pat (..), MatchPat (..), FreeProgram, Defn (..), DataDefn (..), Builtin (..), DataCon (..), DataConId, builtinName)
import ReWire.Crust.Types (poly, arrowRight, (|->), concrete, kblank, plusTy, tyAnn, setTyAnn, flattenArrow, plus, strTy, intTy, listTy, vecTy, arr)
import ReWire.Crust.Util (mkLam, flattenLam, mkApp)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Fix (fixOn, fixOn')
import ReWire.Pretty (showt, prettyPrint)
import ReWire.SYB (runT, transform, runQ, query)
import ReWire.Unbound (fresh, substs, Subst, n2s, s2n, unsafeUnbind, Fresh, Embed (Embed), Name, bind, unbind, fv)

import Control.Arrow (first)
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
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Numeric.Natural (Natural)

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

-- | Type-annotate the AST, but also eliminate uses of polymorphic
--   definitions by creating new definitions specialized to non-polymorphic types.
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
                  Var _ _ (Just t) n | concrete t, Set.member n polys -> Set.singleton (n, unAnn t)
                  _                                                   -> mempty)

            concretize :: Data d => Concretes -> d -> d
            concretize cs = runIdentity . runT (transform $ \ case
                  v@(Var an tan t n) -> pure $ maybe v (Var an tan t) $ (unAnn <$> t) >>= \ t' -> Map.lookup (n, t') cs
                  e                  -> pure e)

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
mguNats (n, us) (n', vs)  | n >= n'   = mguNats' (n - n', sort' us) $ sort' vs
                          | otherwise = mguNats' (n' - n, sort' vs) $ sort' us
      -- (0, [x1, x2]) [x2]
      where mguNats' :: (Natural, [Name Ty]) -> [Name Ty] -> Maybe TySub
            -- u1 + ... + un ~ 0
            mguNats' (0, us)  []                                 = pure $ mkSub0 us
            -- 0 ~ v1 + ... + vn
            mguNats' (0, [])  vs                                 = pure $ mkSub0 vs
            -- u ~ v
            mguNats' (0, [u]) [v]               | hash (show u) > hash (show v)
                                                                 = pure $ mkSubV u v
            mguNats' (0, [u]) [v]                                = pure $ mkSubV v u
            -- u ~ v1 + ... + vn
            mguNats' (0, [u]) vs                | u `notElem` vs = pure $ Map.fromList [(u, foldr1 (plusTy noAnn) $ map (TyVar noAnn KNat) vs)]
            -- n + u1 + ... + un ~ v
            mguNats' (n, us)  [v]               | v `notElem` us = pure $ Map.fromList [(v, foldr (plusTy noAnn . TyVar noAnn KNat) (TyNat noAnn n) us)]
            -- n ~ v1 + ... + vn
            mguNats' (n, []) vs@(v : _)         | dups <- fromIntegral $ length $ takeWhile (== v) vs
                                                                 = tsUnion (mkSubN v $ n `div` dups) <$> mguNats' (n `mod` dups, []) (drop (fromIntegral dups) vs)
            -- n + u1 + ... + un ~ v1 + ... + vn
            mguNats' (n, u : us) vs             | u `elem` vs    = mguNats' (n, us) $ dropFirst (== u) vs
            mguNats' (n, us) (v : vs)           | v `elem` us    = mguNats' (n, dropFirst (== v) us) vs

            mguNats' (n, us@(u : _)) vs@(v : _) | hash (show u) > hash (show v)
                                                , dups <- length $ takeWhile (== u) us
                                                                 = tsUnion (mkSubV u v) <$> mguNats' (n, replicate dups v <> drop dups us) vs
            mguNats' (n, us@(u : _)) vs@(v : _) | dups <- length $ takeWhile (== v) vs
                                                                 = tsUnion (mkSubV v u) <$> mguNats' (n, us) (replicate dups u <> drop dups vs)
            mguNats' _ _                                         = Nothing

            mkSubV :: Name Ty -> Name Ty -> TySub
            mkSubV u v = Map.fromList [(u, TyVar noAnn KNat v)]

            mkSubN :: Name Ty -> Natural -> TySub
            mkSubN u n = Map.fromList [(u, TyNat noAnn n)]

            mkSub0 :: [Name Ty] -> TySub
            mkSub0 = Map.fromList . map (, TyNat noAnn 0) . nubOrd

            sort' :: [Name Ty] -> [Name Ty]
            sort' = sortOn (hash . show)

            dropFirst :: (a -> Bool) -> [a] -> [a]
            dropFirst p = \ case
                  []                 -> []
                  a : as | p a       -> as
                         | otherwise -> a : dropFirst p as

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

mgu _                               _                                               = Nothing
      -- trace ("     MGU: " <> unpack (prettyPrint t1)
      --   <> "\n    with: " <> unpack (prettyPrint t2)) $
      -- trace ("      t1: " <> show (unAnn t1)) $
      -- trace ("      t2: " <> show (unAnn t2)) $ Nothing

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
      t1' <- gets $ flip tsFix t1
      t2' <- gets $ flip tsFix t2
      -- trace ("Unifying: " <> unpack (prettyPrint t1')
      --   <> "\n    with: " <> unpack (prettyPrint t2')) $ pure ()
      -- trace ("     t1': " <> show (unAnn t1')) $ pure ()
      -- trace ("     t2': " <> show (unAnn t2')) $ pure ()
      case mgu t1' t2' of
            Just s -> do
                  -- trace ("    result:\n" <> unpack (prettyPrint  (Map.toList s))) $ pure ()
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
                  PatCon _ _ _ _ ps             -> flip (foldr patAssumps') ps
                  PatVar _ _ (Embed (Just t)) n -> Map.insert n $ [] `poly` t
                  PatVar {}                     -> id
                  PatWildCard {}                -> id

patHoles :: Fresh m => MatchPat -> m (HashMap (Name Exp) Poly)
patHoles = flip patHoles' $ pure mempty
      where patHoles' :: Fresh m => MatchPat -> m (HashMap (Name Exp) Poly) -> m (HashMap (Name Exp) Poly)
            patHoles' = \ case
                  MatchPatCon _ _ _ _ ps   -> flip (foldr patHoles') ps
                  MatchPatVar _ _ (Just t) -> (flip Map.insert ([] `poly` t) <$> fresh (s2n "PHOLE") <*>)
                  MatchPatVar {}           -> id
                  MatchPatWildCard {}      -> id

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
      p | Just pt <- tyAnn p -> do
            ta <- inst pt
            t' <- unify (ann p) ta t
            setTyAnn (Just pt) <$> tcPat t' (setTyAnn Nothing p)
      PatCon an tan _ (Embed i) ps -> do
            (ps', t') <- tcPatCon an t tcPat i ps
            pure $ PatCon an tan (Embed $ Just t') (Embed i) ps'
      PatVar an tan _ x            -> pure $ PatVar an tan (Embed $ Just t) x
      PatWildCard an tan _         -> pure $ PatWildCard an tan (Embed $ Just t)

tcMatchPat :: (Fresh m, MonadError AstError m, MonadReader TCEnv m, MonadState TySub m) => Ty -> MatchPat -> m MatchPat
tcMatchPat t = \ case
      p | Just pt <- tyAnn p -> do
            ta <- inst pt
            t' <- unify (ann p) ta t
            setTyAnn (Just pt) <$> tcMatchPat t' (setTyAnn Nothing p)
      MatchPatCon an tan _ i ps -> do
            (ps', t') <- tcPatCon an t tcMatchPat i ps
            pure $ MatchPatCon an tan (Just t') i ps'
      MatchPatVar an tan _      -> pure $ MatchPatVar an tan $ Just t
      MatchPatWildCard an tan _ -> pure $ MatchPatWildCard an tan $ Just t

tcExp :: (Fresh m, MonadError AstError m, MonadReader TCEnv m, MonadState TySub m) => Exp -> m (Exp, Ty)
tcExp = \ case
      e | Just pt <- tyAnn e -> do
            -- trace "tc: type annotation" $ pure ()
            (e', t) <- first (setTyAnn $ Just pt) <$> tcExp (setTyAnn Nothing e)
            ta      <- inst pt
            t'      <- unify (ann e) ta t
            pure (e', t')
      App an tan _ e1 e2 | isFromList e1 -> do
            -- trace "tc: fromList" $ pure ()
            (e2', _) <- tcExp e2
            case litListElems e2' of
                  Nothing -> failAt an "fromList: argument not a list literal."
                  -- Note: we instantiate LitVec here. TODO(chathhorn): move this to the inlining pass?
                  Just es -> tcExp $ LitVec an tan Nothing es
      App an tan _ e1 e2 -> do
            -- trace "tc: app" $ pure ()
            (e1', t1) <- tcExp e1
            (e2', t2) <- tcExp e2
            tvr       <- freshv
            -- trace ("tc: app: e2':\n" <> show (unAnn e2')) $ pure ()
            t         <- unify an t1 (t2 `arr` tvr)
            pure (App an tan (Just $ arrowRight t) e1' e2', arrowRight t)
      Lam an tan _ e -> do
            -- trace "tc: lam" $ pure ()
            (x, e')  <- unbind e
            tvx      <- freshv
            (e'', t) <- localAssumps (Map.insert x (poly [] tvx)) $ tcExp e'
            pure (Lam an tan (Just tvx) $ bind x e'', tvx `arr` t)
      Var an tan _ v -> do
            -- trace "tc: var" $ pure ()
            as <- asks as
            case Map.lookup v as of
                  Nothing -> failAt an $ "Unknown variable: " <> showt v
                  Just pt -> do
                        t <- inst pt
                        pure (Var an tan (Just t) v, t)
      Con an tan _ i -> do
            -- trace "tc: con" $ pure ()
            cas <- asks cas
            case Map.lookup i cas of
                  Nothing -> failAt an $ "Unknown constructor: " <> prettyPrint i
                  Just pt -> do
                        t <- inst pt
                        pure (Con an tan (Just t) i, t)
      Case an tan _ e e1 e2 -> do
            -- trace "tc: case" $ pure ()
            (e', tp)   <- tcExp e
            (p, e1')   <- unbind e1
            p'         <- tcPat tp p
            let as     = patAssumps p'
            (e1'', t1) <- localAssumps (`Map.union` as) $ tcExp e1'
            case e2 of
                  Nothing -> pure (Case an tan (Just t1) e' (bind p' e1'') Nothing, t1)
                  Just e2 -> do
                        (e2', t2) <- tcExp e2
                        t         <- unify an t1 t2
                        pure (Case an tan (Just t) e' (bind p' e1'') (Just e2'), t)
      Match an tan _ e p f e2 -> do
            -- trace "tc: match" $ pure ()
            (e', tp) <- tcExp e
            p'       <- tcMatchPat tp p
            holes    <- patHoles p'
            (_, tb)  <- localAssumps (`Map.union` holes) $ tcExp $ mkApp' an f $ map fst $ Map.toList holes
            case e2 of
                  Nothing -> pure (Match an tan (Just tb) e' p' f Nothing, tb)
                  Just e2 -> do
                        (e2', t2) <- tcExp e2
                        t         <- unify an tb t2
                        pure (Match an tan (Just t) e' p' f (Just e2'), t)
      Builtin an tan _ b -> do
            -- trace ("tc: builtin: " <> show b) $ pure ()
            as <- asks as
            case Map.lookup (s2n $ builtinName b) as of
                  Nothing -> failAt an $ "Unknown builtin: " <> builtinName b
                  Just pt -> do
                        t <- inst pt
                        pure (Builtin an tan (Just t) b, t)
      e@LitInt {} -> pure (e, intTy $ ann e)
      e@LitStr {} -> pure (e, strTy $ ann e)
      LitList an tan _ es -> do
            -- trace "tc: LitList" $ pure ()
            tv  <- freshv
            es' <- mapM tcExp es
            t   <- listTy an <$> foldM (unify an) tv (snd <$> es')
            pure (LitList an tan (Just t) (fst <$> es'), t)
      LitVec an tan _ es -> do
            -- trace "tc: LitVec" $ pure ()
            tv  <- freshv
            es' <- mapM tcExp es
            t   <- vecTy an (TyNat an $ fromInteger $ toInteger $ length es) <$> foldM (unify an) tv (snd <$> es')
            pure (LitVec an tan (Just t) (fst <$> es'), t)

      where isFromList :: Exp -> Bool
            isFromList = \ case
                  Builtin _ _ _ VecFromList -> True
                  _                         -> False

            litListElems :: Exp -> Maybe [Exp]
            litListElems = \ case
                  LitList _ _ _ es -> pure es
                  _                -> Nothing

            mkApp' :: Annote -> Exp -> [Name Exp] -> Exp
            mkApp' an f holes = mkApp an f $ map (Var an Nothing Nothing) holes

tcDefn :: (Fresh m, MonadError AstError m, MonadReader TCEnv m) => Text -> Defn -> m Defn
tcDefn start d  = flip evalStateT mempty $ do
      -- trace ("tcDefn: " <> show (defnName d)) $ pure ()
      let Defn an n (Embed pt) b (Embed e) = force d
      t'       <- inst pt
      (vs, e') <- unbind e
      let (targs, _) = flattenArrow t'

      (fvs, fe) <- flattenLam e'
      -- trace ("tcDefn body: " <> unpack (prettyPrint (unAnn $ untype e'))) $ pure ()
      (fe', te'') <- localAssumps (`Map.union` (Map.fromList $ zip (vs <> fvs) $ map (poly []) targs))
                   $ tcExp fe

      let e'' = mkLam (ann e') (zip (drop (length vs) targs) fvs) fe'
      -- trace ("e'':\n" <> show (unAnn e'')) $ pure ()

      startTy <- inst globalStartTy

      t'' <- if isStart $ defnName d then unify an startTy t' else pure t'
      _   <- unify an (iterate arrowRight t'' !! length (vs <> fvs)) te''

      -- subs <- gets unAnn
      -- trace ("prefixed:\n" <> unpack (prettyPrint  (Map.toList subs))) $ pure ()
      modify tsFixAll
      -- subs <- gets unAnn
      -- trace ("fixed:\n" <> unpack (prettyPrint  (Map.toList subs))) $ pure ()
      e''' <- gets $ flip subst e''

      let d'  = Defn an n (Embed pt) b $ Embed $ bind vs e'''
      d' `deepseq` pure d'
      where isStart :: Name Exp -> Bool
            isStart = (== start) . n2s

-- | Calculates the transitive closure of the type var unifier set.
tsFixAll :: TySub -> TySub
tsFixAll = fixOn' norm subst'
      where subst' :: TySub -> TySub
            subst' s = Map.map (subst s) s

            norm :: TySub -> Int
            norm = hash . Map.map unAnn

tsFix :: TySub -> Ty -> Ty
tsFix ts = fixOn' norm (subst ts)
      where norm :: Ty -> Int
            norm = hash . unAnn

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
untype = runIdentity . runT (transform $ \ (_ :: Maybe Ty) -> pure Nothing)

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

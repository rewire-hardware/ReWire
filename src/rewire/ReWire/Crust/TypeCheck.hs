{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
--
-- This type checker is based loosely on Mark Jones's "Typing Haskell in
-- Haskell", though since we don't have type classes in core it is much
-- simpler.
--
module ReWire.Crust.TypeCheck (typeCheck, typeCheckDefn, untype, unify, unify') where

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
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (hash))
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

fixTySub :: TySub -> TySub
fixTySub = fixOn' norm subst'
      where subst' :: TySub -> TySub
            subst' s = Map.map (subst s) s

            norm :: TySub -> Int
            norm = hash . Map.map unAnn

mergeTySub :: TySub -> TySub -> TySub
mergeTySub ts = fixTySub . Map.unionWith merge ts
      where merge :: Ty -> Ty -> Ty
            merge (TyApp an t1 t2) (TyApp _ t1' t2') = TyApp an (merge t1 t1') (merge t2 t2')
            merge TyVar {}         t                 = t
            merge t                _                 = t

mgu :: Ty -> Ty -> Maybe TySub
mgu (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) ti ) to )
    (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) ti') to')                    = do
      s1 <- mgu iTy ti
      s2 <- mergeTySub s1 <$> mgu (subst s1 ti) (subst s1 ti')
      s3 <- mergeTySub s2 <$> mgu (subst s2 oTy) (subst s2 to)
      mergeTySub s3 <$> mgu (subst s3 to) (subst s3 to')

mgu (TyApp _ tl tr)                 (TyApp _ tl' tr')                         = do
      let fwd = mgu tl tl' >>= \ s -> mergeTySub s <$> mgu (subst s tr) (subst s tr')
          rev = mgu tr tr' >>= \ s -> mergeTySub s <$> mgu (subst s tl) (subst s tl')
      fwd `mplus` rev
mgu (TyCon _ c1)                    (TyCon _ c2) | n2s c1 == n2s c2           = pure mempty

mgu (TyNat _ n1)                    (TyNat _ n2) | n1 == n2                   = pure mempty

mgu (TyVar _ _ u)                   (plus -> Just (TyVar _ _ v, t)) | u == v  = mgu (TyNat (ann t) 0) t
mgu (plus -> Just (TyVar _ _ v, t)) (TyVar _ _ u)                   | u == v  = mgu (TyNat (ann t) 0) t

mgu (TyNat _ n)                     (plus -> Just (TyNat _ m, t))   | n >= m  = mgu (TyNat (ann t) $ n - m) t
mgu (plus -> Just (TyNat _ m, t))   (TyNat _ n)                     | n >= m  = mgu (TyNat (ann t) $ n - m) t

mgu (TyVar _ _ u)                   (TyVar _ _ v)          | u == v           = pure mempty

mgu (TyVar _ _ u)                   tv@(TyVar _ _ v)       | hash (show u) > hash (show v) = pure $ Map.fromList [(u, tv)]
mgu tu@TyVar {}                     (TyVar _ _ v)                                          = pure $ Map.fromList [(v, tu)]

mgu (TyVar _ _ u)                   t                      | u `notElem` fv t = pure $ Map.fromList [(u, t)]
mgu t                               (TyVar _ _ u)          | u `notElem` fv t = pure $ Map.fromList [(u, t)]
mgu _                               _                                         = Nothing
--      trace ("     MGU: " <> unpack (prettyPrint t1)
--        <> "\n    with: " <> unpack (prettyPrint t2)) $
--      trace ("      t1: " <> show (unAnn t1)) $
--      trace ("      t2: " <> show (unAnn t2)) $ Nothing

mapNat :: (Ty -> Ty) -> Ty -> Ty
mapNat norm = \ case
      TyApp a c n | isVec c || isProxy c -> TyApp a c $ norm n
      TyApp a t1 t2                      -> TyApp a (mapNat norm t1) $ mapNat norm t2
      t                                  -> t

normNat :: Ty -> Ty
normNat t = case evalNats $ plus' t of
      (0, ts@(_ : _)) -> foldr1 (plusTy $ ann t) ts
      (n, ts        ) -> foldr1 (plusTy $ ann t) $ TyNat (ann t) n : ts

normNat' :: Ty -> Ty
normNat' t = case evalNats $ plus' t of
      (0, ts@(_ : _)) -> foldr1 (plusTy $ ann t) $ reverse ts
      (n, ts        ) -> foldr1 (plusTy $ ann t) $ reverse $ TyNat (ann t) n : ts

evalNats :: [Ty] -> (Natural, [Ty])
evalNats = \ case
      TyNat _ n : (evalNats -> (n', ts')) -> (n' + n,     ts')
      t         : (evalNats -> (n', ts')) -> (n',     t : ts')
      _                                   -> (0,           [])

plus' :: Ty -> [Ty]
plus' t | Just (a, b) <- plus t = plus' a <> plus' b
        | otherwise             = [t]

unify' :: Ty -> Ty -> Maybe Ty
unify' t1 t2 = subst <$> mgu t1 t2 <*> pure t1

tySub :: (Data a, MonadState TySub m, Subst Ty a) => a -> m a
tySub a = gets (normNats . flip subst a)
      where normNats :: Data d => d -> d
            normNats = runIdentity . runPureT (transform $ \ (t :: Ty) -> pure $ mapNat normNat t)

unify :: (MonadError AstError m, MonadState TySub m) => Annote -> Ty -> Ty -> m ()
unify an t1 t2 = do
      t1' <- tySub t1
      t2' <- tySub t2
      let rev = mapNat normNat'
--      trace ("Unifying: " <> unpack (prettyPrint t1')
--        <> "\n    with: " <> unpack (prettyPrint t2')) $ pure ()
--      trace ("     t1': " <> show (unAnn t1')) $ pure ()
--      trace ("     t2': " <> show (unAnn t2')) $ pure ()
-- 
--      trace ("Unifying: " <> unpack (prettyPrint t1')
--        <> "\n    with: " <> unpack (prettyPrint $ rev t2')) $ pure ()
--      trace (" rev t2': " <> show (unAnn $ rev t2')) $ pure ()

      case mgu t1' t2' `mplus` mgu t1' (rev t2') of
            Just s' -> modify (mergeTySub s')
            _       -> failAt an $ "Types do not unify. Expected and got, respectively:\n"
                              <> prettyPrint t1' <> "\n"
                              <> prettyPrint t2'

isVec :: Ty -> Bool
isVec = \ case
      TyCon _ (n2s -> "Vec") -> True
      _                      -> False

isProxy :: Ty -> Bool
isProxy = \ case
      TyCon _ (n2s -> "Proxy") -> True
      _                        -> False

isPlus :: Ty -> Bool
isPlus = \ case
      TyCon _ (n2s -> "+") -> True
      _                    -> False

plus :: Ty -> Maybe (Ty, Ty)
plus = \ case
      TyApp _ (TyApp _ c t1) t2 | isPlus c -> pure (t1, t2)
      _                                    -> Nothing

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

tcPat :: (Fresh m, MonadError AstError m, MonadReader TCEnv m, MonadState TySub m) => Ty -> Pat -> m Pat
tcPat t = \ case
      PatCon an _ (Embed i) ps -> do
            cas     <- asks cas
            case Map.lookup i cas of
                  Nothing  -> failAt an $ "Unknown constructor: " <> prettyPrint i
                  Just pta -> do
                        ta               <- inst pta
                        let (targs, tres) = flattenArrow ta
                        if length ps /= length targs
                        then failAt an "Pattern is not applied to enough arguments"
                        else do
                              ps' <- zipWithM tcPat targs ps
                              unify an tres t
                              pure $ PatCon an (Embed t) (Embed i) ps'
      PatVar an _ x            -> pure $ PatVar an (Embed t) x
      PatWildCard an _         -> pure $ PatWildCard an (Embed t)

tcMatchPat :: (Fresh m, MonadError AstError m, MonadReader TCEnv m, MonadState TySub m) => Ty -> MatchPat -> m MatchPat
tcMatchPat t = \ case
      MatchPatCon an _ i ps -> do
            cas     <- asks cas
            case Map.lookup i cas of
                  Nothing  -> failAt an $ "Unknown constructor: " <> prettyPrint i
                  Just pta -> do
                        ta               <- inst pta
                        let (targs, tres) = flattenArrow ta
                        if length ps /= length targs
                        then failAt an "Pattern is not applied to enough arguments"
                        else do
                              ps' <- zipWithM tcMatchPat targs ps
                              unify an tres t
                              pure $ MatchPatCon an t i ps'
      MatchPatVar an _      -> pure $ MatchPatVar an t
      MatchPatWildCard an _ -> pure $ MatchPatWildCard an t

tcExp :: (Fresh m, MonadError AstError m, MonadReader TCEnv m, MonadState TySub m) => Exp -> m (Exp, Ty)
tcExp = \ case
      App an e1 e2 | isFromList e1 -> do
            (e2', _) <- tcExp e2
            case litListElems e2' of
                  Nothing -> failAt an $ "fromList: argument not a list literal."
                  -- Note: we instantiate LitVec here. TODO(chathhorn): move this to the inlining pass?
                  Just es -> tcExp $ LitVec an (TyBlank an) es
      App an e1 e2 -> do
            (e1', te1) <- tcExp e1
            (e2', te2) <- tcExp e2
            tvr        <- freshv
            unify an te1 (te2 `arr` tvr)
            pure (App an e1' e2', tvr)
      Lam an _ e -> do
            (x, e')   <- unbind e
            tvx       <- freshv
            tvr       <- freshv
            (e'', te) <- localAssumps (Map.insert x (poly [] tvx)) $ tcExp e'
            unify an tvr $ tvx `arr` te
            pure (Lam an tvx $ bind x e'', tvr)
      Var an _ v -> do
            as <- asks as
            case Map.lookup v as of
                  Nothing -> failAt an $ "Unknown variable: " <> showt v
                  Just pt -> do
                        t <- inst pt
                        pure (Var an t v, t)
      Con an _ i -> do
            cas <- asks cas
            case Map.lookup i cas of
                  Nothing -> failAt an $ "Unknown constructor: " <> prettyPrint i
                  Just pt -> do
                        t <- inst pt
                        pure (Con an t i, t)
      Case an _ e e1 e2 -> do
            (e', te)    <- tcExp e
            (p, e1')    <- unbind e1
            tv          <- freshv
            p'          <- tcPat te p
            let as      = patAssumps p'
            (e1'', te1) <- localAssumps (`Map.union` as) $ tcExp e1'
            unify an tv te1
            case e2 of
                  Nothing -> pure (Case an tv e' (bind p' e1'') Nothing, tv)
                  Just e2 -> do
                        (e2', te2)  <- tcExp e2
                        unify an tv te2
                        pure (Case an tv e' (bind p' e1'') (Just e2'), tv)
      Match an _ e p f e2 -> do
            (e', te) <- tcExp e
            tv       <- freshv
            p'       <- tcMatchPat te p
            holes    <- patHoles p'
            (_, te1) <- localAssumps (`Map.union` holes) $ tcExp $ mkApp' an f $ map fst $ Map.toList holes
            unify an tv te1
            case e2 of
                  Nothing -> pure (Match an tv e' p' f Nothing, tv)
                  Just e2 -> do
                        (e2', te2)  <- tcExp e2
                        unify an tv te2
                        pure (Match an tv e' p' f (Just e2'), tv)
      Builtin an _ b -> do
            as <- asks as
            case Map.lookup (s2n $ builtinName b) as of
                  Nothing -> failAt an $ "Unknown builtin: " <> builtinName b
                  Just pt -> do
                        t <- inst pt
                        pure (Builtin an t b, t)
      e@LitInt {} -> pure (e, typeOf e)
      e@LitStr {} -> pure (e, typeOf e)
      LitList an _ es -> do
            tv  <- freshv
            es' <- mapM tcExp es
            mapM_ (unify an tv) (snd <$> es')
            let t = listTy an tv
            pure (LitList an t (fst <$> es'), t)
      LitVec an _ es -> do
            tv  <- freshv
            es' <- mapM tcExp es
            mapM_ (unify an tv) (snd <$> es')
            let t = vecTy an (TyNat an $ fromInteger $ toInteger $ length es) tv
            pure (LitVec an t (fst <$> es'), t)
      TypeAnn an pt e -> do
            (e', te) <- tcExp e
            t        <- inst pt
            unify an t te
            pure (TypeAnn an pt e', te)

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
      let Defn an n (Embed pt) b (Embed e) = force d
      t'       <- inst pt
      (vs, e') <- unbind e
      let (targs, _) = flattenArrow t'
      (e'', te) <- localAssumps (`Map.union` (Map.fromList $ zip vs $ map (poly []) targs))
                 $ tcExp e'
      unify an (iterate arrowRight t' !! length vs) te
      when (isStart $ defnName d) $ do
            startTy <- inst globalStartTy
            unify an startTy t'
      d' <- Defn an n (Embed pt) b <$> Embed <$> bind vs <$> tySub e''
      d' `deepseq` pure d'

      where isStart :: Name Exp -> Bool
            isStart = (== start) . n2s

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

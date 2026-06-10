{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
-- {-# LANGUAGE Trustworthy #-}
module ReWire.Crust.TypeCheck (typeCheck, typeCheckDefn, untype, unify, unify', TySub) where

import ReWire.Annotation (Annote (MsgAnnote), ann)
import ReWire.Crust.Syntax (Exp (..), Ty (..), Kind (..), Poly (..), Pat (..), MatchPat (..), FreeProgram, Defn (..), DataDefn (..), Builtin (..), DataCon (..), DataConId, builtinName)
import ReWire.Crust.Types (mkArrowTy, poly, arrowRight, (|->), concrete, kblank, tyAnn, setTyAnn, flattenArrow, strTy, intTy, listTy, vecTy, arr, arrowLeft, dstPoly1, zeroP1, minusP1, pickVar, poly1Ty, prettyTy)
import ReWire.Crust.Util (transMPat, patVars)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Fix (fixOn)
import ReWire.Pretty (showt, prettyPrint, Pretty (..))
import ReWire.SYB (transform, query)
import ReWire.Unbound (fresh, substs, Subst, n2s, s2n, unsafeUnbind, Fresh, Embed (Embed), Name, bind, unbind, fv)

import Control.Arrow (first)
import Control.DeepSeq (deepseq, force)
import Control.Monad (zipWithM, foldM, mplus, when)
import Control.Monad.Reader (MonadReader, ReaderT (..), local, asks)
import Control.Monad.State (evalStateT, gets, modify, MonadState)
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Maybe (mapMaybe)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set

-- import ReWire.Pretty (hsep)
-- import Debug.Trace (trace)
-- import Data.Text (unpack)

subst :: Subst b a => HashMap (Name b) b -> a -> a
subst = substs . Map.toList

-- | Apply a substitution to a type, chasing bindings until a normal form:
--   equivalent to `subst` until a fix point, but each variable is resolved
--   with map lookups instead of repeated full passes over the term and the
--   substitution (`subst` is linear in the size of the substitution even
--   when the term is a single variable).
substTy :: TySub -> Ty -> Ty
substTy s = \ case
      TyApp an t1 t2  -> TyApp an (substTy s t1) $ substTy s t2
      t@(TyVar _ _ v) -> case Map.lookup v s of
            Just (TyVar _ _ v') | v' == v -> t -- Identity bindings occur; don't chase them.
            Just t'                       -> substTy s t'
            Nothing                       -> t
      t               -> t

-- | Close a substitution over itself, so a single `subst` pass suffices to
--   fully rewrite a term.
compress :: TySub -> TySub
compress s = Map.map (substTy s) s

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
typeCheckDefn ts vs d = runReaderT (withAssumps ts vs $ tcDefn (s2n "") d) mempty

-- | Type-annotate the AST, but also eliminate uses of polymorphic
--   definitions by creating new definitions specialized to non-polymorphic types.
typeCheck :: (Fresh m, MonadError AstError m) => Name Exp -> FreeProgram -> m FreeProgram
typeCheck start (ts, syns, vs) = (ts, syns, ) <$> runReaderT tc mempty
      where conc :: Concretes -> Defn -> [Defn]
            conc cs (Defn an n _ b e) = mapMaybe conc' $ lookupAll n $ Map.keys cs
                  where conc' :: Ty -> Maybe Defn
                        conc' t = do
                              n' <- Map.lookup (n, t) cs
                              pure $ Defn an n' ([] |-> t) b e

            tc :: (Fresh m, MonadError AstError m, MonadReader TCEnv m) => m [Defn]
            tc = do
                  vs' <- withAssumps ts vs $ mapM (tcDefn start) vs
                  cs  <- concretes vs' -- TODO(chathhorn): don't think this needs to use fix.
                  fst . fst <$> fixOn (Map.keys . snd) "polymorphic function instantiation" 10 tc' ((vs', mempty), cs)

            tc' :: (Fresh m, MonadError AstError m, MonadReader TCEnv m) => (([Defn], Concretes), Concretes) -> m (([Defn], Concretes), Concretes)
            tc' ((acc, cs), cs') = do
                  let ep = concatMap (conc cs') polyDefs
                  ep' <- concretize (cs <> cs') <$> withAssumps ts (acc <> ep) (mapM (tcDefn start) ep)
                  ((concretize (cs <> cs') acc <> ep', cs <> cs'), ) <$> concretes ep'

            concretes :: Fresh m => [Defn] -> m Concretes
            concretes = foldM (\ m c -> Map.insert c <$> fresh (fst c) <*> pure m) mempty . uses

            polys :: HashSet (Name Exp)
            polys = foldr polys' mempty vs
                  where polys' :: Defn -> HashSet (Name Exp) -> HashSet (Name Exp)
                        polys' d | isPoly d  = Set.insert $ defnName d
                                 | otherwise = id

            polyDefs :: [Defn]
            polyDefs = foldr polys' mempty vs
                  where polys' :: Defn -> [Defn] -> [Defn]
                        polys' d | isPoly d  = (d :)
                                 | otherwise = id

            isPoly :: Defn -> Bool
            isPoly (Defn _ _ (Embed (Poly (unsafeUnbind -> (_, t)))) _ _) = not $ concrete t

            -- Note: Eq and Hashable on Ty both ignore annotations, so the
            -- use types here don't need annotations stripped (with unAnn)
            -- for use as map keys.
            uses :: Data a => a -> HashSet (Name Exp, Ty)
            uses a = Set.fromList [(n, t) | Var _ _ (Just t) n <- query a, concrete t, Set.member n polys]

            concretize :: Data d => Concretes -> d -> d
            concretize cs = transform $ \ case
                  v@(Var an tan t n) -> maybe v (Var an tan t) $ t >>= \ t' -> Map.lookup (n, t') cs
                  e                  -> e

freshv :: Fresh m => m Ty
freshv = TyVar (MsgAnnote "TypeCheck: freshv") kblank <$> fresh (s2n "?")

tsUnion :: TySub -> TySub -> TySub
tsUnion ts = foldr insert ts . Map.toList
      where insert :: (Name Ty, Ty) -> TySub -> TySub
            insert (v, t) ts' | Just t' <- Map.lookup v ts', Just s <- mgu t t' = Map.insert v (subst s t) ts' `tsUnion` s
                              | otherwise                                       = Map.insert v t ts'

mgu :: Ty -> Ty -> Maybe TySub
-- Note: Eq on Ty ignores annotations (Eq Annote is trivially true), so
-- there's no need to strip them (with unAnn) before comparing.
mgu t                               t'               | t == t'                       = pure mempty

mgu (TyCon _ c1)                    (TyCon _ c2)     | n2s c1 == n2s c2              = pure mempty

mgu (TyVar _ _ u)                   tv@(TyVar _ _ v) | u > v                         = pure $ Map.fromList [(u, tv)]
mgu tu@TyVar {}                     (TyVar _ _ v)                                    = pure $ Map.fromList [(v, tu)]

-- TODO: the special-case unifying with ReacT/PuRe is a hack and doesn't cover all cases. Should be removed eventually.
mgu (TyVar _ _ u)                   t                | u `notElem` fv t, isReacT' t  = tsUnion (Map.fromList [(u, t)]) <$> mgu globalReacT' t
mgu (TyVar _ _ u)                   t                | u `notElem` fv t, isPuRe' t   = tsUnion (Map.fromList [(u, t)]) <$> mgu globalPuRe t
mgu (TyVar _ _ u)                   t                | u `notElem` fv t              = pure $ Map.fromList [(u, t)]
mgu t                               (TyVar _ _ u)    | u `notElem` fv t, isReacT' t  = tsUnion (Map.fromList [(u, t)]) <$> mgu globalReacT' t
mgu t                               (TyVar _ _ u)    | u `notElem` fv t, isPuRe' t   = tsUnion (Map.fromList [(u, t)]) <$> mgu globalPuRe t
mgu t                               (TyVar _ _ u)    | u `notElem` fv t              = pure $ Map.fromList [(u, t)]

mgu (dstPoly1 -> Just p1)           (dstPoly1 -> Just p2)                            = case p1 `minusP1` p2 of
                  p' | p' == zeroP1         -> pure mempty
                  (pickVar -> Just (x, p')) -> pure $ Map.singleton x $ poly1Ty p'
                  _                         -> Nothing

-- TODO: one ReacT assumption.
mgu (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) ti ) to )
    (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) ti') to')                           = do
      s1 <- mgu iTy ti
      s2 <- tsUnion s1 <$> mgu (subst s1 ti) (subst s1 ti')
      s3 <- tsUnion s2 <$> mgu (subst s2 oTy) (subst s2 to)
      tsUnion s3 <$> mgu (subst s3 to) (subst s3 to')

-- TODO: one PuRe assumption.
mgu (TyApp _ (TyApp _ (TyCon _ (n2s -> "PuRe")) ts ) to )
    (TyApp _ (TyApp _ (TyCon _ (n2s -> "PuRe")) ts') to')                            = do
      s1 <- mgu sPTy ts
      s2 <- tsUnion s1 <$> mgu (subst s1 ts) (subst s1 ts')
      s3 <- tsUnion s2 <$> mgu (subst s2 oPTy) (subst s2 to)
      tsUnion s3 <$> mgu (subst s3 to) (subst s3 to')

mgu (TyApp _ tl tr)                 (TyApp _ tl' tr')                                = do
      let fwd = mgu tl tl' >>= \ s -> tsUnion s <$> mgu (subst s tr) (subst s tr')
          rev = mgu tr tr' >>= \ s -> tsUnion s <$> mgu (subst s tl) (subst s tl')
      fwd `mplus` rev

mgu _                               _                                                = Nothing
--       trace ("     MGU: " <> unpack (prettyPrint t1)
--         <> "\n    with: " <> unpack (prettyPrint t2)) $ Nothing

unify' :: Ty -> Ty -> Maybe Ty
unify' t1 t2 = flip subst t1 <$> mgu t1 t2

unify :: (MonadError AstError m, MonadState TySub m) => Annote -> Ty -> Ty -> m Ty
unify an t1 t2 = do
      t1' <- gets $ flip substTy t1
      t2' <- gets $ flip substTy t2
      -- trace ("... Unifying: " <> unpack (prettyPrint t1')
      --    <> "\n...     with: " <> unpack (prettyPrint t2')) $ pure ()
      case mgu t1' t2' of
            Just s -> do
                  -- trace ("    result:\n" <> unpack (prettyPrint  (Map.toList s))) $ pure ()
                  modify $ tsUnion s
                  gets $ flip substTy t1'
            _      -> failAt an $ "Types do not unify. Expected and got, respectively:\n"
                              <> prettyTy t1' <> "\n"
                              <> prettyTy t2'

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

tcPatCon :: (MonadReader TCEnv m, MonadState TySub m, Fresh m, MonadError AstError m, Pretty pat) => Annote -> Ty -> (Ty -> pat -> m pat) -> Name DataConId -> [pat] -> m ([pat], Ty)
tcPatCon an t tc i ps = do
      -- trace ("     tcPatCon: " <> show i <> show (hsep (map pretty ps)) <> " :: " <> unpack (prettyPrint t)) $ pure ()
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
            -- trace ("   tcPat: " <> show (pretty p) <> " :: " <> unpack (prettyPrint pt)) $ pure ()
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
            -- trace ("   tcPat: " <> show (pretty p) <> " :: " <> unpack (prettyPrint pt)) $ pure ()
            ta <- inst pt
            t' <- unify (ann p) ta t
            setTyAnn (Just pt) <$> tcMatchPat t' (setTyAnn Nothing p)
      MatchPatCon an tan _ i ps -> do
            (ps', t') <- tcPatCon an t tcMatchPat i ps
            pure $ MatchPatCon an tan (Just t') i ps'
      MatchPatVar an tan _      -> pure $ MatchPatVar an tan $ Just t
      MatchPatWildCard an tan _ -> pure $ MatchPatWildCard an tan $ Just t

tcExp :: (Fresh m, MonadError AstError m, MonadReader TCEnv m, MonadState TySub m) => Ty -> Exp -> m (Exp, Ty)
tcExp tt = \ case
      e | Just pt <- tyAnn e -> do
            -- trace ("tcExp: type annotation: " <> show (pretty pt)) $ pure ()
            ta       <- inst pt
            t        <- unify (ann e) ta tt
            (e', t') <- first (setTyAnn $ Just pt) <$> tcExp t (setTyAnn Nothing e)
            pure (e', t')
      App an tan _ e1 e2 | isFromList e1 -> do
            -- trace "tc: fromList" $ pure ()
            tve      <- freshv
            (e2', _) <- tcExp tve e2
            case litListElems e2' of
                  Nothing -> failAt an "fromList: argument not a list literal."
                  -- Note: we instantiate LitVec here. TODO(chathhorn): move this to the inlining pass?
                  Just es -> tcExp tt $ LitVec an tan Nothing es
      App an tan _ e1 e2 -> do
            -- trace "tcExp: app (1): tc e2" $ pure ()
            tvx       <- freshv
            (e2', t2) <- tcExp tvx e2
            -- trace "tc: app (2): tc e1" $ pure ()
            (e1', t1) <- tcExp (t2 `arr` tt) e1
            -- trace ("tc: app: e2':\n" <> show (unAnn e2')) $ pure ()
            pure (App an tan (Just $ arrowRight t1) e1' e2', arrowRight t1)
      Lam an tan _ e -> do
            -- trace "tcExp: lam" $ pure ()
            tvx          <- freshv
            tvr          <- freshv
            tt'          <- unify an tt $ tvx `arr` tvr
            let tvx'     = arrowLeft tt'
                tvr'     = arrowRight tt'
            (x, e')      <- unbind e
            (e'', tvr'') <- localAssumps (Map.insert x (poly [] tvx'))
                        $ tcExp tvr' e'
            pure (Lam an tan (Just tvx') $ bind x e'', tvx' `arr` tvr'')
      Var an tan _ v -> do
            -- trace ("tcExp: var: " <> show v) $ pure ()
            as <- asks as
            case Map.lookup v as of
                  Nothing -> failAt an $ "Unknown variable: " <> showt v
                  Just pt -> do
                        t <- inst pt
                        t' <- unify an tt t
                        pure (Var an tan (Just t') v, t')
      Con an tan _ i -> do
            -- trace ("tcExp: con " ++ show (pretty i)) $ pure ()
            cas <- asks cas
            case Map.lookup i cas of
                  Nothing -> failAt an $ "Unknown constructor: " <> prettyPrint i
                  Just pt -> do
                        t <- inst pt
                        t' <- unify an tt t
                        pure (Con an tan (Just t') i, t')
      Case an tan _ disc e els -> do
            -- trace ("  tcCase: " <> show (pretty disc) <> " :: " <>
            --                unpack (prettyPrint tan)) $ pure ()
            -- trace "tcExp: case" $ pure ()
            tve         <- freshv
            (disc', tp) <- tcExp tve disc
            (p, e')     <- unbind e
            p'          <- tcPat tp p
            let as     = patAssumps p'
            -- trace "tc: case: tc then" $ pure ()
            (e'', t1) <- localAssumps (`Map.union` as) $ tcExp tt e'
            case els of
                  Nothing -> pure (Case an tan (Just t1) disc' (bind p' e'') Nothing, t1)
                  Just els -> do
                        -- trace "tc: case: tc else" $ pure ()
                        (els', t2) <- tcExp t1 els
                        pure (Case an tan (Just t2) disc' (bind p' e'') (Just els'), t2)
      Match an tan _ disc p e els -> do
            -- trace "tcExp: match" $ pure ()
            tve         <- freshv
            (disc', tp) <- tcExp tve disc
            p'          <- tcMatchPat tp p
            tps         <- map fst . patVars <$> transMPat p'
            (e', flattenArrow -> (targs, tres))
                        <- tcExp (mkArrowTy tps tt) e
            let tm       = mkArrowTy (drop (length tps) targs) tres
            case els of
                  Nothing  -> pure (Match an tan (Just tm) disc' p' e' Nothing, tm)
                  Just els -> do
                        (els', t2) <- tcExp tm els
                        pure (Match an tan (Just t2) disc' p' e' (Just els'), t2)
      Builtin an tan _ b -> do
            -- trace ("tcExp: builtin: " <> show b) $ pure ()
            as <- asks as
            case Map.lookup (s2n $ builtinName b) as of
                  Nothing -> failAt an $ "Unknown builtin: " <> builtinName b
                  Just pt -> do
                        t <- inst pt
                        t' <- unify an tt t
                        pure (Builtin an tan (Just t') b, t')
      e@LitInt {} -> (e, ) <$> unify (ann e) tt (intTy $ ann e)
      e@LitStr {} -> (e, ) <$> unify (ann e) tt (strTy $ ann e)
      LitList an tan _ es -> do
            -- trace "tcExp: LitList" $ pure ()
            te  <- freshv
            _ <- unify an tt $ listTy an te
            (es', te') <- tcElems te es
            -- trace "tc: LitList: unify2" $ pure ()
            tt' <- unify an tt $ listTy an te'
            pure (LitList an tan (Just tt') es', tt')
      LitVec an tan _ es -> do
            -- trace "tcExp: LitVec" $ pure ()
            te  <- freshv
            (es', te') <- tcElems te es
            tt'   <- unify an tt $ vecTy an (TyNat an $ fromInteger $ toInteger $ length es') te'
            pure (LitVec an tan (Just tt') es', tt')

      where isFromList :: Exp -> Bool
            isFromList = \ case
                  Builtin _ _ _ VecFromList -> True
                  _                         -> False

            litListElems :: Exp -> Maybe [Exp]
            litListElems = \ case
                  LitList _ _ _ es -> pure es
                  _                -> Nothing

            -- Note: accumulates the checked elements in reverse to avoid
            -- quadratic list appends on long (e.g., bit-vector) literals.
            tcElems :: (Fresh m, MonadError AstError m, MonadReader TCEnv m, MonadState TySub m) => Ty -> [Exp] -> m ([Exp], Ty)
            tcElems te = (first reverse <$>) . foldM tcElem ([], te)

            tcElem :: (Fresh m, MonadError AstError m, MonadReader TCEnv m, MonadState TySub m) => ([Exp], Ty) -> Exp -> m ([Exp], Ty)
            tcElem (els, tel) el = do
                  -- trace "tc: tcElem" $ pure ()
                  (el', tel') <- tcExp tel el
                  pure (el' : els, tel')


tcDefn :: (Fresh m, MonadError AstError m, MonadReader TCEnv m) => Name Exp -> Defn -> m Defn
tcDefn start d  = flip evalStateT mempty $ do
      -- trace ("tcDefn: " <> show (defnName d)) $ pure ()
      let Defn an n (Embed pt) b (Embed e) = force d
      t           <- if
            | isStart $ defnName d -> do
                  startTy <- inst globalStartTy
                  inst pt >>= unify an startTy
            | isPureStart $ defnName d -> do
                  pureStartTy <- inst globalPureStartTy
                  inst pt >>= unify an pureStartTy
            | otherwise -> inst pt
      (vs, body)  <- unbind e
      let (targs, _) = flattenArrow t
          tbody      = iterate arrowRight t !! length vs

      (body', _) <- localAssumps (`Map.union` (Map.fromList $ zip vs $ map (poly []) targs))
                         $ tcExp tbody body

      body'' <- gets $ \ s -> transform (substTy $ compress s) body'

      let d'  = Defn an n (Embed pt) b $ Embed $ bind vs body''
      d' `deepseq` pure d'
      where isStart :: Name Exp -> Bool
            isStart = (== start)

            isPureStart :: Name Exp -> Bool
            isPureStart = (== "$Pure.start") . n2s

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
untype = transform $ \ (_ :: Maybe Ty) -> Nothing

iTy :: Ty
iTy = TyVar (MsgAnnote "ReacT input type.") KStar (s2n "?i")

oTy :: Ty
oTy = TyVar (MsgAnnote "ReacT output type.") KStar (s2n "?o")

sPTy :: Ty
sPTy = TyVar (MsgAnnote "PuRe s type.") KStar (s2n "?ps")

oPTy :: Ty
oPTy = TyVar (MsgAnnote "PuRe o type.") KStar (s2n "?po")

isReacT' :: Ty -> Bool
isReacT' = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "ReacT")) _) _ -> True
      _                                                -> False

globalReacT' :: Ty
globalReacT' = TyApp an (TyApp an (TyCon an $ s2n "ReacT") iTy) oTy
      where an :: Annote
            an = MsgAnnote "Global ReacT type."

globalReacT :: Ty -> Ty -> Ty
globalReacT = TyApp an . TyApp an globalReacT'
      where an :: Annote
            an = MsgAnnote "Global ReacT type."

isPuRe' :: Ty -> Bool
isPuRe' = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "PuRe")) _) _ -> True
      _                                               -> False

globalPuRe :: Ty
globalPuRe = TyApp an (TyApp an (TyCon an $ s2n "PuRe") sPTy) oPTy
      where an :: Annote
            an = MsgAnnote "Global PuRe type."

globalStartTy :: Poly
globalStartTy = poly [a] $ globalReacT (TyCon an $ s2n "Identity") $ TyVar an KStar a
      where an :: Annote
            an = MsgAnnote "Expected start function type."

            a :: Name Ty
            a = s2n "a"

globalPureStartTy :: Poly
globalPureStartTy = poly [] globalPuRe

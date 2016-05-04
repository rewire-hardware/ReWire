{-# LANGUAGE FlexibleContexts, LambdaCase, TupleSections #-}
--
-- This type checker is based loosely on Mark Jones's "Typing Haskell in
-- Haskell", though since we don't have type classes in core it is much
-- simpler.
--

module ReWire.FrontEnd.TypeCheck (typeCheck) where

import ReWire.Annotation
import ReWire.Error
import ReWire.FrontEnd.Syntax
import ReWire.Pretty

import Control.DeepSeq (deepseq, force)
import Control.Monad.Reader (ReaderT (..), local, ask)
import Control.Monad.State (StateT (..), get, put, modify)
import Control.Monad (zipWithM)
import Data.List (foldl')
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map

import Unbound.Generics.LocallyNameless
      ( fresh, substs, aeq, Subst
      , name2String, string2Name
      )

subst :: Subst b a => Map (Name b) b -> a -> a
subst ss = substs (Map.assocs ss)

-- Type checker for core.

type TySub = Map (Name RWMTy) RWMTy
data TCEnv = TCEnv
      { as  :: Map (Name RWMExp) Poly
      , cas :: Map (Name DataConId) Poly
      , arr :: Name TyConId
      } deriving Show

type Assump  = (Name RWMExp, Poly)
type CAssump = (Name DataConId, Poly)

type TCM m = ReaderT TCEnv (StateT TySub m)

typeCheck :: (Fresh m, SyntaxError m) => RWMProgram -> m RWMProgram
typeCheck (RWMProgram p) = do
      (ts, vs) <- untrec p
      (ts', vs') <- fst <$> runStateT (runReaderT (tc (ts, vs)) (TCEnv mempty mempty (getArrow ts))) mempty
      return $ RWMProgram $ trec (ts', vs')

localAssumps :: SyntaxError m => (Map (Name RWMExp) Poly -> Map (Name RWMExp) Poly) -> TCM m a -> TCM m a
localAssumps f = local (\ tce -> tce { as = f (as tce) })

localCAssumps :: SyntaxError m => (Map (Name DataConId) Poly -> Map (Name DataConId) Poly) -> TCM m a -> TCM m a
localCAssumps f = local (\ tce -> tce { cas = f (cas tce) })

freshv :: (Fresh m, SyntaxError m) => TCM m RWMTy
freshv = do
      n <- fresh $ string2Name "?"
      modify $ Map.insert n $ RWMTyVar noAnn kblank n
      return $ RWMTyVar noAnn kblank n

defnAssump :: RWMDefn -> Assump
defnAssump (RWMDefn _ n (Embed pt) _ _) = (n, pt)

dataDeclAssumps :: RWMData -> [CAssump]
dataDeclAssumps (RWMData _ _ _ cs) = map dataConAssump cs

dataConAssump :: RWMDataCon -> CAssump
dataConAssump (RWMDataCon _ i (Embed t)) = (i, t)

(@@) :: TySub -> TySub -> TySub
s1 @@ s2 = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

isFlex :: Name a -> Bool
isFlex = (=='?') . head . name2String

varBind :: SyntaxError m => Annote -> Name RWMTy -> RWMTy -> TCM m TySub
varBind an u t | t `aeq` RWMTyVar noAnn kblank u = return mempty
               | u `elem` fv t                   = failAt an $ "Occurs check fails: " ++ show u ++ ", " ++ prettyPrint t
               | otherwise                       = return (Map.singleton u t)

mgu :: SyntaxError m => Annote -> RWMTy -> RWMTy -> TCM m TySub
mgu an (RWMTyApp _ tl tr) (RWMTyApp _ tl' tr')                                      = do
      s1 <- mgu an tl tl'
      s2 <- mgu an (subst s1 tr) $ subst s1 tr'
      return $ s2 @@ s1
mgu an (RWMTyVar _ _ u)   t                  | isFlex u                             = varBind an u t
mgu an t                  (RWMTyVar _ _ u)   | isFlex u                             = varBind an u t
mgu _  (RWMTyCon _ c1)    (RWMTyCon _ c2)    | (name2String c1) == (name2String c2) = return mempty
mgu _  (RWMTyVar _ _ v)   (RWMTyVar _ _ u)   | not (isFlex v) && v==u               = return mempty
mgu an (RWMTyComp _ m t)  (RWMTyComp _ m' t')                                       = do
      s1 <- mgu an m m'
      s2 <- mgu an (subst s1 t) (subst s1 t')
      return $ s2 @@ s1
mgu an t1 t2 = failAt an $ "Types do not unify: " ++ prettyPrint t1 ++ ", " ++ prettyPrint t2

unify :: SyntaxError m => Annote -> RWMTy -> RWMTy -> TCM m ()
unify an t1 t2 = do
      s <- get
      u <- mgu an (subst s t1) $ subst s t2
      modify (u@@)

inst :: (Fresh m, SyntaxError m) => Poly -> TCM m RWMTy
inst (Poly pt) = do
      (tvs, t) <- unbind pt
      sub      <- Map.fromList <$> mapM (\ tv -> (tv,) <$> freshv) tvs
      return $ subst sub t

patAssumps :: RWMPat -> [Assump]
patAssumps = \ case
      RWMPatCon _ _ ps        -> concatMap patAssumps ps
      RWMPatVar _ (Embed t) n -> [(n, [] `poly` t)]

patHoles :: Fresh m => RWMMatchPat -> m [Assump]
patHoles = \ case
      RWMMatchPatCon _ _ ps -> do
            holes <- mapM patHoles ps
            return $ concat holes
      RWMMatchPatVar _ t    -> do
            x <- fresh $ string2Name "PHOLE"
            return [(x, [] `poly` t)]

tcPat :: (Fresh m, SyntaxError m) => RWMTy -> RWMPat -> TCM m RWMPat
tcPat t = \ case
      RWMPatVar an _ x  -> return $ RWMPatVar an (Embed t) x
      RWMPatCon an (Embed i) ps -> do
            cas     <- cas <$> ask
            case Map.lookup i cas of
                  Nothing  -> failAt an $ "Unknown constructor: " ++ prettyPrint i
                  Just pta -> do
                        ta               <- inst pta
                        let (targs, tres) = flattenArrow ta
                        if length ps /= length targs
                        then failAt an "Pattern is not applied to enough arguments"
                        else do
                              ps' <- zipWithM tcPat targs ps
                              unify an t tres
                              return $ RWMPatCon an (Embed i) ps'

tcMatchPat :: (Fresh m, SyntaxError m) => RWMTy -> RWMMatchPat -> TCM m RWMMatchPat
tcMatchPat t = \ case
      RWMMatchPatVar an _ -> return $ RWMMatchPatVar an t
      RWMMatchPatCon an i ps -> do
            cas     <- cas <$> ask
            case Map.lookup i cas of
                  Nothing  -> failAt an $ "Unknown constructor: " ++ prettyPrint i
                  Just pta -> do
                        ta               <- inst pta
                        let (targs, tres) = flattenArrow ta
                        if length ps /= length targs
                        then failAt an "Pattern is not applied to enough arguments"
                        else do
                              ps' <- zipWithM tcMatchPat targs ps
                              unify an t tres
                              return $ RWMMatchPatCon an i ps'

tcExp :: (Fresh m, SyntaxError m) => RWMExp -> TCM m (RWMExp, RWMTy)
tcExp = \ case
      e_@(RWMApp an _ _)   -> do
            let (ef:es) =  flattenApp e_
            (ef', tf)   <- tcExp ef
            ress        <- mapM tcExp es
            let   es'   =  map fst ress
                  tes   =  map snd ress
            tv          <- freshv
            arr         <- arr <$> ask
            let tf'     =  foldr (mkArrow arr) tv tes
            unify an tf tf'
            return (foldl (RWMApp an) ef' es', tv)
      RWMLam an _ e      -> do
            (x, e')   <- unbind e
            tvx       <- freshv
            tvr       <- freshv
            (e'', te) <- localAssumps (Map.insert x ([] `poly` tvx)) $ tcExp e'
            arr       <- arr <$> ask
            unify an tvr $ mkArrow arr tvx te
            return (RWMLam an tvx $ bind x e'', tvr)
      RWMVar an _ v -> do
            as      <- as <$> ask
            case Map.lookup v as of
                  Nothing -> failAt an $ "Unknown variable: " ++ show v
                  Just pt -> do
                        t <- inst pt
                        return (RWMVar an t v, t)
      RWMCon an _ i        -> do
            cas     <- cas <$> ask
            case Map.lookup i cas of
                  Nothing -> failAt an $ "Unknown constructor: " ++ prettyPrint i
                  Just pt -> do
                        t <- inst pt
                        return (RWMCon an t i, t)
      RWMCase an _ e e1 e2   -> do
            (p, e1')    <- unbind e1
            (e', te)    <- tcExp e
            tv          <- freshv
            p'          <- tcPat te p
            let as      = Map.fromList $ patAssumps p'
            (e1'', te1) <- localAssumps (as `Map.union`) $ tcExp e1'
            unify an tv te1
            case e2 of
                  Nothing -> return (RWMCase an tv e' (bind p' e1'') Nothing, tv)
                  Just e2 -> do
                        (e2', te2)  <- tcExp e2
                        unify an tv te2
                        return (RWMCase an tv e' (bind p' e1'') (Just e2'), tv)
      RWMMatch an _ e p f as e2 -> do
            (e', te) <- tcExp e
            tv       <- freshv
            p'       <- tcMatchPat te p
            holes    <- Map.fromList <$> patHoles p'
            (_, te1) <- localAssumps (holes `Map.union`) $ tcExp $ mkApp an f as $ map fst $ Map.toList holes
            unify an tv te1
            case e2 of
                  Nothing -> return (RWMMatch an tv e' p' f as Nothing, tv)
                  Just e2 -> do
                        (e2', te2)  <- tcExp e2
                        unify an tv te2
                        return (RWMMatch an tv e' p' f as (Just e2'), tv)
      RWMNativeVHDL an n e -> do
            (e', te) <- tcExp e
            return (RWMNativeVHDL an n e', te)
      RWMError an _ m      -> do
            tv <- freshv
            return (RWMError an tv m, tv)

mkApp :: Annote -> RWMExp -> [RWMExp] -> [Name RWMExp] -> RWMExp
mkApp an f as holes = foldl' (\ e x -> RWMApp an e x) f
      $ as ++ map (RWMVar an tblank) holes

tcDefn :: (Fresh m, SyntaxError m) => RWMDefn -> TCM m RWMDefn
tcDefn d  = do
      put mempty
      let RWMDefn an n (Embed (Poly pt)) b (Embed e) = force d
      (tvs, t)  <- unbind pt
      (vs, e')  <- unbind e
      let (targs, _) = flattenArrow t
      (e'', te) <- localAssumps (Map.union $ Map.fromList $ zip vs $ map (poly []) targs)
            $ tcExp e'
      let te' = iterate arrowRight t !! length vs
      unify an te' te
      s         <- get
      put mempty
      let d' = RWMDefn noAnn n (tvs |-> t) b $ Embed $ bind vs $ subst s e''
      d' `deepseq` return d'

tc :: (Fresh m, SyntaxError m) => ([RWMData], [RWMDefn]) -> TCM m ([RWMData], [RWMDefn])
tc (ts, vs) = do
      let as   =  Map.fromList $ map defnAssump vs
          cas  =  Map.fromList $ concatMap dataDeclAssumps ts
      vs'      <- localAssumps (as `Map.union`) $ localCAssumps (cas `Map.union`) $ mapM tcDefn vs
      return (ts, vs')

flattenArrow :: RWMTy -> ([RWMTy], RWMTy)
flattenArrow (RWMTyApp _ (RWMTyApp _ (RWMTyCon _ c) t1) t2)
      | name2String c == "->" = (t1:ts, t)
      where (ts, t) = flattenArrow t2
flattenArrow t                = ([], t)


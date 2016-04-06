{-# LANGUAGE FlexibleContexts, LambdaCase #-}
--
-- This type checker is based loosely on Mark Jones's "Typing Haskell in
-- Haskell", though since we don't have type classes in core it is much
-- simpler.
--

module ReWire.FrontEnd.TypeCheck (typecheck) where

import ReWire.Annotation
import ReWire.Error
import ReWire.FrontEnd.Syntax
import ReWire.Pretty

import Control.Monad (zipWithM)
import Control.DeepSeq (deepseq, force)
import Control.Monad.Reader (ReaderT (..), local, ask)
import Control.Monad.State (StateT (..), get, put, modify)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map

import Unbound.Generics.LocallyNameless
      ( fresh, substs, aeq, Subst
      , name2String, string2Name)

subst :: Subst b a => Map (Name b) b -> a -> a
subst ss = substs (Map.assocs ss)

-- Type checker for core.

type TySub = Map (Name RWMTy) RWMTy
data TCEnv = TCEnv
      { as  :: Map (Name RWMExp) Poly
      , cas :: Map DataConId Poly
      } deriving Show

type Assump  = (Name RWMExp, Poly)
type CAssump = (DataConId, Poly)

type TCM m = ReaderT TCEnv (StateT TySub m)

typecheck :: (Fresh m, SyntaxError m) => RWMProgram -> m RWMProgram
typecheck p = fmap fst $ runStateT (runReaderT (tc p) (TCEnv mempty mempty)) mempty

localAssumps :: SyntaxError m => (Map (Name RWMExp) Poly -> Map (Name RWMExp) Poly) -> TCM m a -> TCM m a
localAssumps f = local (\ tce -> tce { as = f (as tce) })

askAssumps :: SyntaxError m => TCM m (Map (Name RWMExp) Poly)
askAssumps = ask >>= return . as

localCAssumps :: SyntaxError m => (Map DataConId Poly -> Map DataConId Poly) -> TCM m a -> TCM m a
localCAssumps f = local (\ tce -> tce { cas = f (cas tce) })

askCAssumps :: SyntaxError m => TCM m (Map DataConId Poly)
askCAssumps = ask >>= return . cas

freshv :: (Fresh m, SyntaxError m) => TCM m RWMTy
freshv = do
      n <- fresh $ string2Name "?"
      modify $ Map.insert n $ RWMTyVar noAnn n
      return $ RWMTyVar noAnn n

defnAssump :: RWMDefn -> Assump
defnAssump (RWMDefn _ n (Embed pt) _ _) = (n, pt)

dataConAssump :: [Name RWMTy] -> RWMTy -> RWMDataCon -> CAssump
dataConAssump tvs rt (RWMDataCon _ i ts) = (i, tvs `poly` foldr mkArrow rt ts)

dataDeclAssumps :: RWMData -> [CAssump]
dataDeclAssumps (RWMData _ i tvs _ dcs) = map (dataConAssump tvs rt) dcs
      where rt = foldl (RWMTyApp noAnn) (RWMTyCon noAnn i) (map (RWMTyVar noAnn) tvs)

(@@) :: TySub -> TySub -> TySub
s1 @@ s2 = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

isFlex :: Name a -> Bool
isFlex = (=='?') . head . name2String

varBind :: SyntaxError m => Annote -> Name RWMTy -> RWMTy -> TCM m TySub
varBind an u t | t `aeq` RWMTyVar noAnn u = return mempty
               | u `elem` fv t            = failAt an $ "Occurs check fails: " ++ show u ++ ", " ++ prettyPrint t
               | otherwise                = return (Map.singleton u t)

mgu :: SyntaxError m => Annote -> RWMTy -> RWMTy -> TCM m TySub
mgu an (RWMTyApp _ tl tr) (RWMTyApp _ tl' tr')                     = do
      s1 <- mgu an tl tl'
      s2 <- mgu an (subst s1 tr) $ subst s1 tr'
      return $ s2 @@ s1
mgu an (RWMTyVar _ u)     t               | isFlex u               = varBind an u t
mgu an t                  (RWMTyVar _ u)  | isFlex u               = varBind an u t
mgu _  (RWMTyCon _ c1)    (RWMTyCon _ c2) | c1 == c2               = return mempty
mgu _  (RWMTyVar _ v)     (RWMTyVar _ u)  | not (isFlex v) && v==u = return mempty
mgu an (RWMTyComp _ m t)  (RWMTyComp _ m' t')                      = do
      s1 <- mgu an m m'
      s2 <- mgu an (subst s1 t) (subst s1 t')
      return $ s2 @@ s1
mgu an t1 t2                                                       = failAt an $ "Types do not unify: " ++ prettyPrint t1 ++ ", " ++ prettyPrint t2

unify :: SyntaxError m => Annote -> RWMTy -> RWMTy -> TCM m ()
unify an t1 t2 = do
      s <- get
      u <- mgu an (subst s t1) $ subst s t2
      modify (u@@)

inst :: (Fresh m, SyntaxError m) => Poly -> TCM m RWMTy
inst (Poly pt) = do
      (tvs, t) <- unbind pt
      sub      <- Map.fromList <$> mapM (\ tv -> freshv >>= \ v -> return (tv, v)) tvs
      return $ subst sub t

patassumps :: RWMPat -> [Assump]
patassumps = \ case
      RWMPatCon _ _ ps -> concatMap patassumps ps
      RWMPatVar _ t n  -> [(n, [] `poly` t)]

tcPat :: (Fresh m, SyntaxError m) => RWMTy -> RWMPat -> TCM m RWMPat
tcPat t = \ case
      RWMPatVar an _ x  -> return $ RWMPatVar an t x
      RWMPatCon an i ps -> do
            cas     <- askCAssumps
            case Map.lookup i cas of
                  Nothing -> failAt an $ "Unknown constructor: " ++ prettyPrint i
                  Just pta  -> do
                        ta               <- inst pta
                        let (targs, tres) = flattenArrow ta
                        if length ps /= length targs
                        then failAt an "Pattern is not applied to enough arguments"
                        else do
                              ps' <- zipWithM tcPat targs ps
                              unify an t tres
                              return $ RWMPatCon an i ps'

tcExp :: (Fresh m, SyntaxError m) => RWMExp -> TCM m (RWMExp, RWMTy)
tcExp = \ case
      e_@(RWMApp an _ _)   -> do
            let (ef:es) =  flattenApp e_
            (ef', tf)   <- tcExp ef
            ress        <- mapM tcExp es
            let   es'   =  map fst ress
                  tes   =  map snd ress
            tv          <- freshv
            unify an tf (foldr mkArrow tv tes)
            return (foldl (RWMApp an) ef' es', tv)
      RWMLam an _ e      -> do
            (x, e')   <- unbind e
            tvx       <- freshv
            tvr       <- freshv
            (e'', te) <- localAssumps (Map.insert x ([] `poly` tvx)) $ tcExp e'
            unify an tvr $ tvx `mkArrow` te
            return (RWMLam an tvx $ bind x e'', tvr)
      RWMVar an _ v        -> do
            as      <- askAssumps
            case Map.lookup v as of
                  Nothing -> failAt an $ "Unknown variable: " ++ show v
                  Just pt -> do
                        t <- inst pt
                        return (RWMVar an t v, t)
      RWMCon an _ i        -> do
            cas     <- askCAssumps
            case Map.lookup i cas of
                  Nothing -> failAt an $ "Unknown constructor: " ++ prettyPrint i
                  Just pt -> do
                        t <- inst pt
                        return (RWMCon an t i, t)
      RWMCase an e e1 e2   -> do
            (p, e1')    <- unbind e1
            (e', te)    <- tcExp e
            tv          <- freshv
            p'          <- tcPat te p
            let as      = Map.fromList $ patassumps p'
            (e1'', te1) <- localAssumps (as `Map.union`) $ tcExp e1'
            unify an tv te1
            (e2', te2)  <- tcExp e2
            unify an tv te2
            return (RWMCase an e' (bind p' e1'') e2', tv)
      RWMNativeVHDL an n e -> do
            (e', te) <- tcExp e
            return (RWMNativeVHDL an n e', te)
      RWMError an _ m      -> do
            tv <- freshv
            return (RWMError an tv m, tv)

tcDefn :: (Fresh m, SyntaxError m) => RWMDefn -> TCM m RWMDefn
tcDefn d  = do
      put mempty
      let RWMDefn an n (Embed (Poly pt)) b (Embed e) = force d
      (tvs, t)  <- unbind pt
      (vs, e')  <- unbind e
      (e'', te) <- tcExp e'
      unify an t te
      s         <- get
      put mempty
      let d' = RWMDefn noAnn n (tvs |-> t) b $ Embed $ bind vs $ subst s e''
      d' `deepseq` return d'

tc :: (Fresh m, SyntaxError m) => RWMProgram -> TCM m RWMProgram
tc p = do
      ds  <- untrec $ defns p
      let as  =  Map.fromList $ map defnAssump ds
          cas =  Map.fromList $ concatMap dataDeclAssumps $ dataDecls p
      ds'     <- localAssumps (as `Map.union`) $ localCAssumps (cas `Map.union`) $ mapM tcDefn ds
      return p { defns = trec ds' }

flattenArrow :: RWMTy -> ([RWMTy], RWMTy)
flattenArrow (RWMTyApp _ (RWMTyApp _ (RWMTyCon _ (TyConId "->")) t1) t2) = (t1:ts, t)
      where (ts, t) = flattenArrow t2
flattenArrow t                                                           = ([], t)


{-# LANGUAGE FlexibleContexts, LambdaCase #-}
--
-- This type checker is based loosely on Mark Jones's "Typing Haskell in
-- Haskell", though since we don't have type classes in core it is much
-- simpler.
--

module ReWire.FrontEnd.TypeCheck (typecheck) where

import ReWire.Annotation
import ReWire.Error
import ReWire.FrontEnd.PrimBasis
import ReWire.FrontEnd.Syntax
import ReWire.Pretty
import ReWire.Scoping

import Control.Monad (zipWithM)
import Control.DeepSeq (deepseq, force)
import Control.Monad.Reader (ReaderT (..), local, ask)
import Control.Monad.State (StateT (..), get, put)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map

-- Type checker for core.

type TySub = Map (Id RWCTy) RWCTy
data TCEnv = TCEnv
      { as  :: Map (Id RWMExp) (Poly RWCTy)
      , cas :: Map DataConId (Poly RWCTy)
      } deriving Show
data TCState = TCState
      { tySub :: TySub
      , ctr   :: Int
      } deriving Show

type Assump  = (Id RWMExp, Poly RWCTy)
type CAssump = (DataConId, Poly RWCTy)

type TCM m = ReaderT TCEnv (StateT TCState m)

typecheck :: (SyntaxError m, Functor m) => RWMProgram -> m RWMProgram
typecheck m = fmap fst $ runStateT (runReaderT (tc [primBasis] m) (TCEnv mempty mempty)) $ TCState mempty 0

localAssumps :: SyntaxError m => (Map (Id RWMExp) (Poly RWCTy) -> Map (Id RWMExp) (Poly RWCTy)) -> TCM m a -> TCM m a
localAssumps f = local (\ tce -> tce { as = f (as tce) })

askAssumps :: SyntaxError m => TCM m (Map (Id RWMExp) (Poly RWCTy))
askAssumps = ask >>= \ tce -> return (as tce)

localCAssumps :: SyntaxError m => (Map DataConId (Poly RWCTy) -> Map DataConId (Poly RWCTy)) -> TCM m a -> TCM m a
localCAssumps f = local (\ tce -> tce { cas = f (cas tce) })

askCAssumps :: SyntaxError m => TCM m (Map DataConId (Poly RWCTy))
askCAssumps = ask >>= \ tce -> return (cas tce)

getTySub :: SyntaxError m => TCM m TySub
getTySub = get >>= return . tySub

updTySub :: SyntaxError m => (TySub -> TySub) -> TCM m ()
updTySub f = get >>= \ s -> put (s { tySub = f (tySub s) })

putTySub :: SyntaxError m => TySub -> TCM m ()
putTySub sub = get >>= \ s -> put (s { tySub = sub })

getCtr :: SyntaxError m => TCM m Int
getCtr = get >>= return . ctr

putCtr :: SyntaxError m => Int -> TCM m ()
putCtr c = get >>= \ s -> put (s { ctr = c })

freshv :: SyntaxError m => TCM m (Id RWCTy)
freshv = do
      ctr   <- getCtr
      putCtr (ctr+1)
      let n =  mkId $ "?" ++ show ctr
      updTySub (Map.insert n (RWCTyVar noAnn n))
      return n

defnAssump :: RWMDefn -> Assump
defnAssump (RWMDefn _ n pt _ _) = (n, pt)

dataConAssump :: [Id RWCTy] -> RWCTy -> RWCDataCon -> CAssump
dataConAssump tvs rt (RWCDataCon _ i ts) = (i, tvs :-> foldr mkArrow rt ts)

dataDeclAssumps :: RWMData -> [CAssump]
dataDeclAssumps (RWMData _ i tvs _ dcs) = let rt = foldl (RWCTyApp noAnn) (RWCTyCon noAnn i) (map (RWCTyVar noAnn) tvs)
                                          in  map (dataConAssump tvs rt) dcs

(@@) :: TySub -> TySub -> TySub
s1 @@ s2 = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

isFlex :: Id a -> Bool
isFlex = (=='?') . head . deId

varBind :: SyntaxError m => Annote -> Id RWCTy -> RWCTy -> TCM m TySub
varBind an u t | t `aeq` RWCTyVar noAnn u = return mempty
               | u `elem` fv t            = failAt an $ "Occurs check fails: " ++ prettyPrint u ++ ", " ++ prettyPrint t
               | otherwise                = return (Map.singleton u t)

mgu :: SyntaxError m => Annote -> RWCTy -> RWCTy -> TCM m TySub
mgu an (RWCTyApp _ tl tr) (RWCTyApp _ tl' tr')               = do
      s1 <- mgu an tl tl'
      s2 <- mgu an (subst s1 tr) (subst s1 tr')
      return (s2@@s1)
mgu an (RWCTyVar _ u) t | isFlex u                           = varBind an u t
mgu an t (RWCTyVar _ u) | isFlex u                           = varBind an u t
mgu _ (RWCTyCon _ c1) (RWCTyCon _ c2) | c1 == c2             = return mempty
mgu _ (RWCTyVar _ v) (RWCTyVar _ u) | not (isFlex v) && v==u = return mempty
mgu an (RWCTyComp _ m t) (RWCTyComp _ m' t')                 = do
      s1 <- mgu an m m'
      s2 <- mgu an (subst s1 t) (subst s1 t')
      return (s2@@s1)
mgu an t1 t2                                                 = failAt an $ "Types do not unify: " ++ prettyPrint t1 ++ ", " ++ prettyPrint t2

unify :: SyntaxError m => Annote -> RWCTy -> RWCTy -> TCM m ()
unify an t1 t2 = do
      s <- getTySub
      u <- mgu an (subst s t1) (subst s t2)
      updTySub (u@@)

inst :: SyntaxError m => Poly RWCTy -> TCM m RWCTy
inst (tvs :-> t) = do
      sub <- Map.fromList <$> mapM (\ tv -> freshv >>= \ v -> return (tv, RWCTyVar noAnn v)) tvs
      return (subst sub t)

patassumps :: RWMPat -> [Assump]
patassumps (RWMPatCon _ _ ps)  = concatMap patassumps ps
patassumps (RWMPatLiteral _ _) = []
patassumps (RWMPatVar _ n t)   = [(n, [] :-> t)]

tcPat :: SyntaxError m => RWCTy -> RWMPat -> TCM m RWMPat
tcPat t p@(RWMPatLiteral an l) = do
      case l of
            RWCLitInteger _ -> unify an t (RWCTyCon noAnn (TyConId "Integer"))
            RWCLitFloat _   -> unify an t (RWCTyCon noAnn (TyConId "Float"))
            RWCLitChar _    -> unify an t (RWCTyCon noAnn (TyConId "Char"))
      return p
tcPat t (RWMPatVar an x _)  = return (RWMPatVar an x t)
tcPat t (RWMPatCon an i ps) = do
      cas     <- askCAssumps
      let mpt =  Map.lookup i cas
      case mpt of
            Nothing -> failAt an $ "Unknown constructor: " ++ prettyPrint i
            Just pta  -> do
                  ta               <- inst pta
                  let (targs, tres) =  flattenArrow ta
                  if length ps /= length targs
                  then failAt an "Pattern is not applied to enough arguments"
                  else do
                        ps' <- zipWithM tcPat targs ps
                        unify an t tres
                        return (RWMPatCon an i ps')
tcExp :: SyntaxError m => RWMExp -> TCM m (RWMExp, RWCTy)
tcExp = \ case
      e_@(RWMApp an _ _)   -> do
            let (ef:es) =  flattenApp e_
            (ef', tf)   <- tcExp ef
            ress        <- mapM tcExp es
            let   es'   =  map fst ress
                  tes   =  map snd ress
            tv          <- freshv
            unify an tf (foldr mkArrow (RWCTyVar noAnn tv) tes)
            return (foldl (RWMApp an) ef' es', RWCTyVar noAnn tv)
      RWMLam an x _ e      -> do
            tvx      <- freshv
            tvr      <- freshv
            (e', te) <- localAssumps (Map.insert x ([] :-> RWCTyVar noAnn tvx)) (tcExp e)
            unify an (RWCTyVar noAnn tvr) (RWCTyVar noAnn tvx `mkArrow` te)
            return (RWMLam an x (RWCTyVar noAnn tvx) e', RWCTyVar noAnn tvr)
      RWMVar an v _        -> do
            as      <- askAssumps
            case Map.lookup v as of
                  Nothing -> failAt an $ "Unknown variable: " ++ prettyPrint v
                  Just pt -> do
                        t <- inst pt
                        return (RWMVar an v t, t)
      RWMCon an i _        -> do
            cas     <- askCAssumps
            case Map.lookup i cas of
                  Nothing -> failAt an $ "Unknown constructor: " ++ prettyPrint i
                  Just pt -> do
                        t <- inst pt
                        return (RWMCon an i t, t)
      e@(RWMLiteral _ l)   -> return $ case l of
            RWCLitInteger _ -> (e, RWCTyCon noAnn $ TyConId "Integer")
            RWCLitFloat _   -> (e, RWCTyCon noAnn $ TyConId "Float")
            RWCLitChar _    -> (e, RWCTyCon noAnn $ TyConId "Char")
      RWMCase an e p e1 e2 -> do
            (e', te)   <- tcExp e
            tv         <- freshv
            p'         <- tcPat te p
            let as = Map.fromList (patassumps p')
            (e1', te1) <- localAssumps (as `Map.union`) (tcExp e1)
            unify an (RWCTyVar noAnn tv) te1
            (e2', te2) <- tcExp e2
            unify an (RWCTyVar noAnn tv) te2
            return (RWMCase an e' p' e1' e2', RWCTyVar noAnn tv)

      RWMNativeVHDL an n e -> do
            (e', te) <- tcExp e
            return (RWMNativeVHDL an n e', te)
      RWMError an m _      -> do
            tv <- freshv
            return (RWMError an m (RWCTyVar noAnn tv), RWCTyVar noAnn tv)

tcDefn :: SyntaxError m => RWMDefn -> TCM m RWMDefn
tcDefn d  = do
      putTySub mempty
      let RWMDefn an n (tvs :-> t) b e = force d
      (e', te) <- tcExp e
      unify an t te
      s        <- getTySub
      putTySub mempty
      let d' = RWMDefn noAnn n (tvs :-> t) b (subst s e')
      d' `deepseq` return d'

tc :: SyntaxError m => [RWMProgram] -> RWMProgram -> TCM m RWMProgram
tc ms m = do
      let as  =  Map.fromList $ concatMap (map defnAssump . defns) $ m:ms
          cas =  Map.fromList $ concatMap (concatMap dataDeclAssumps . dataDecls) $ m:ms
      ds'     <- localAssumps (as `Map.union`) $ localCAssumps (cas `Map.union`) $ mapM tcDefn (defns m)
      return m { defns = ds' }

flattenArrow :: RWCTy -> ([RWCTy], RWCTy)
flattenArrow (RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) t1) t2) = (t1:ts, t)
      where (ts, t) = flattenArrow t2
flattenArrow t                                                           = ([], t)


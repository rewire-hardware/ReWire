{-# LANGUAGE FlexibleContexts, LambdaCase, TupleSections #-}
{-# LANGUAGE Safe #-}
--
-- This type checker is based loosely on Mark Jones's "Typing Haskell in
-- Haskell", though since we don't have type classes in core it is much
-- simpler.
--
module ReWire.FrontEnd.TypeCheck (typeCheck) where

import ReWire.Annotation
import ReWire.Error
import ReWire.FrontEnd.Syntax
import ReWire.FrontEnd.Unbound
      ( fresh, substs, aeq, Subst, runFreshMT
      , name2String, string2Name
      )
import ReWire.Pretty

import Control.DeepSeq (deepseq, force)
import Control.Monad.Reader (ReaderT (..), local, ask)
import Control.Monad.State (evalStateT, StateT (..), get, put, modify)
import Control.Monad (zipWithM)
import Data.List (foldl')
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map

subst :: Subst b a => Map (Name b) b -> a -> a
subst ss = substs (Map.assocs ss)

-- Type checker for core.

type TySub = Map (Name Ty) Ty
data TCEnv = TCEnv
      { as  :: Map (Name Exp) Poly
      , cas :: Map (Name DataConId) Poly
      , arr :: Name TyConId
      } deriving Show

type Assump  = (Name Exp, Poly)
type CAssump = (Name DataConId, Poly)

type TCM m = ReaderT TCEnv (StateT TySub m)

typeCheck :: SyntaxError m => FreeProgram -> m FreeProgram
typeCheck (ts, vs) = runFreshMT $
      evalStateT (runReaderT (tc (ts, vs)) (TCEnv mempty mempty (getArrow ts))) mempty

localAssumps :: SyntaxError m => (Map (Name Exp) Poly -> Map (Name Exp) Poly) -> TCM m a -> TCM m a
localAssumps f = local (\ tce -> tce { as = f (as tce) })

localCAssumps :: SyntaxError m => (Map (Name DataConId) Poly -> Map (Name DataConId) Poly) -> TCM m a -> TCM m a
localCAssumps f = local (\ tce -> tce { cas = f (cas tce) })

freshv :: (Fresh m, SyntaxError m) => TCM m Ty
freshv = do
      n <- fresh $ string2Name "?"
      modify $ Map.insert n $ TyVar noAnn kblank n
      return $ TyVar noAnn kblank n

defnAssump :: Defn -> Assump
defnAssump (Defn _ n (Embed pt) _ _) = (n, pt)

dataDeclAssumps :: DataDefn -> [CAssump]
dataDeclAssumps (DataDefn _ _ _ cs) = map dataConAssump cs

dataConAssump :: DataCon -> CAssump
dataConAssump (DataCon _ i (Embed t)) = (i, t)

(@@) :: TySub -> TySub -> TySub
s1 @@ s2 = Map.mapWithKey (\ _ t -> subst s1 t) s2 `Map.union` s1

isFlex :: Name a -> Bool
isFlex = (=='?') . head . name2String

varBind :: SyntaxError m => Annote -> Name Ty -> Ty -> TCM m TySub
varBind an u t | t `aeq` TyVar noAnn kblank u = return mempty
               | u `elem` fv t                = failAt an $ "Occurs check fails: " ++ show u ++ ", " ++ prettyPrint t
               | otherwise                    = return (Map.singleton u t)

mgu :: SyntaxError m => Annote -> Ty -> Ty -> TCM m TySub
mgu an (TyApp _ tl tr) (TyApp _ tl' tr')                                      = do
      s1 <- mgu an tl tl'
      s2 <- mgu an (subst s1 tr) $ subst s1 tr'
      return $ s2 @@ s1
mgu an (TyVar _ _ u)   t                | isFlex u                             = varBind an u t
mgu an t                  (TyVar _ _ u) | isFlex u                             = varBind an u t
mgu _  (TyCon _ c1)    (TyCon _ c2)     | (name2String c1) == (name2String c2) = return mempty
mgu _  (TyVar _ _ v)   (TyVar _ _ u)    | not (isFlex v) && v==u               = return mempty
mgu an (TyComp _ m t)  (TyComp _ m' t')                                       = do
      s1 <- mgu an m m'
      s2 <- mgu an (subst s1 t) (subst s1 t')
      return $ s2 @@ s1
mgu an t1 t2 = failAt an $ "Types do not unify: " ++ prettyPrint t1 ++ ", " ++ prettyPrint t2

unify :: SyntaxError m => Annote -> Ty -> Ty -> TCM m ()
unify an t1 t2 = do
      s <- get
      u <- mgu an (subst s t1) $ subst s t2
      modify (u@@)

inst :: (Fresh m, SyntaxError m) => Poly -> TCM m Ty
inst (Poly pt) = do
      (tvs, t) <- unbind pt
      sub      <- Map.fromList <$> mapM (\ tv -> (tv,) <$> freshv) tvs
      return $ subst sub t

patAssumps :: Pat -> [Assump]
patAssumps = \ case
      PatCon _ _ _ ps      -> concatMap patAssumps ps
      PatVar _ (Embed t) n -> [(n, [] `poly` t)]

patHoles :: Fresh m => MatchPat -> m [Assump]
patHoles = \ case
      MatchPatCon _ _ _ ps -> do
            holes <- mapM patHoles ps
            return $ concat holes
      MatchPatVar _ t    -> do
            x <- fresh $ string2Name "PHOLE"
            return [(x, [] `poly` t)]

tcPat :: (Fresh m, SyntaxError m) => Ty -> Pat -> TCM m Pat
tcPat t = \ case
      PatVar an _ x  -> return $ PatVar an (Embed t) x
      PatCon an _ (Embed i) ps -> do
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
                              return $ PatCon an (Embed t) (Embed i) ps'

tcMatchPat :: (Fresh m, SyntaxError m) => Ty -> MatchPat -> TCM m MatchPat
tcMatchPat t = \ case
      MatchPatVar an _ -> return $ MatchPatVar an t
      MatchPatCon an _ i ps -> do
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
                              return $ MatchPatCon an t i ps'

tcExp :: (Fresh m, SyntaxError m) => Exp -> TCM m (Exp, Ty)
tcExp = \ case
      e_@(App an _ _)        -> do
            let (ef:es) =  flattenApp e_
            (ef', tf)   <- tcExp ef
            ress        <- mapM tcExp es
            let   es'   =  map fst ress
                  tes   =  map snd ress
            tv          <- freshv
            arr         <- arr <$> ask
            let tf'     =  foldr (mkArrow arr) tv tes
            unify an tf tf'
            return (foldl (App an) ef' es', tv)
      Lam an _ e             -> do
            (x, e')   <- unbind e
            tvx       <- freshv
            tvr       <- freshv
            (e'', te) <- localAssumps (Map.insert x ([] `poly` tvx)) $ tcExp e'
            arr       <- arr <$> ask
            unify an tvr $ mkArrow arr tvx te
            return (Lam an tvx $ bind x e'', tvr)
      Var an _ v             -> do
            as <- as <$> ask
            case Map.lookup v as of
                  Nothing -> failAt an $ "Unknown variable: " ++ show v
                  Just pt -> do
                        t <- inst pt
                        return (Var an t v, t)
      Con an _ i      -> do
            cas <- cas <$> ask
            case Map.lookup i cas of
                  Nothing -> failAt an $ "Unknown constructor: " ++ prettyPrint i
                  Just pt -> do
                        t <- inst pt
                        return (Con an t i, t)
      Case an _ e e1 e2      -> do
            (p, e1')    <- unbind e1
            (e', te)    <- tcExp e
            tv          <- freshv
            p'          <- tcPat te p
            let as      = Map.fromList $ patAssumps p'
            (e1'', te1) <- localAssumps (as `Map.union`) $ tcExp e1'
            unify an tv te1
            case e2 of
                  Nothing -> return (Case an tv e' (bind p' e1'') Nothing, tv)
                  Just e2 -> do
                        (e2', te2)  <- tcExp e2
                        unify an tv te2
                        return (Case an tv e' (bind p' e1'') (Just e2'), tv)
      Match an _ e p f as e2 -> do
            (e', te) <- tcExp e
            tv       <- freshv
            p'       <- tcMatchPat te p
            holes    <- Map.fromList <$> patHoles p'
            (_, te1) <- localAssumps (holes `Map.union`) $ tcExp $ mkApp an f as $ map fst $ Map.toList holes
            unify an tv te1
            case e2 of
                  Nothing -> return (Match an tv e' p' f as Nothing, tv)
                  Just e2 -> do
                        (e2', te2)  <- tcExp e2
                        unify an tv te2
                        return (Match an tv e' p' f as (Just e2'), tv)
      NativeVHDL an n e      -> do
            (e', te) <- tcExp e
            return (NativeVHDL an n e', te)
      Error an _ m           -> do
            tv <- freshv
            return (Error an tv m, tv)

mkApp :: Annote -> Exp -> [Exp] -> [Name Exp] -> Exp
mkApp an f as holes = foldl' (\ e x -> App an e x) f
      $ as ++ map (Var an tblank) holes

tcDefn :: (Fresh m, SyntaxError m) => Defn -> TCM m Defn
tcDefn d  = do
      put mempty
      let Defn an n (Embed (Poly pt)) b (Embed e) = force d
      (tvs, t) <- unbind pt
      (vs, e') <- unbind e
      let (targs, _) = flattenArrow t
      (e'', te) <- localAssumps (Map.union $ Map.fromList $ zip vs $ map (poly []) targs)
            $ tcExp e'
      let te' = iterate arrowRight t !! length vs
      unify an te' te
      s <- get
      put mempty
      let d' = Defn noAnn n (tvs |-> t) b $ Embed $ bind vs $ subst s e''
      d' `deepseq` return d'

tc :: (Fresh m, SyntaxError m) => FreeProgram -> TCM m FreeProgram
tc (ts, vs) = do
      let as   =  Map.fromList $ map defnAssump vs
          cas  =  Map.fromList $ concatMap dataDeclAssumps ts
      vs'      <- localAssumps (as `Map.union`) $ localCAssumps (cas `Map.union`) $ mapM tcDefn vs
      return (ts, vs')

flattenArrow :: Ty -> ([Ty], Ty)
flattenArrow (TyApp _ (TyApp _ (TyCon _ c) t1) t2)
      | name2String c == "->" = (t1:ts, t)
      where (ts, t) = flattenArrow t2
flattenArrow t                = ([], t)


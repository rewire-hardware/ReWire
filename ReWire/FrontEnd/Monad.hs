{-# LANGUAGE FlexibleContexts, LambdaCase #-}
--
-- This is a handy monad class/transformer with lots of morphisms for stuff
-- you often want to do in ReWire transformations.
--
module ReWire.FrontEnd.Monad
      ( RW, RWT
      , TyConInfo(..), DataConInfo(..)
      , askVar
      , runRW
      , fsubstE, fsubstsE
      ) where

import ReWire.FrontEnd.Syntax
import ReWire.Scoping

import Control.Monad ((>=>))
import Control.Monad.Identity (Identity (..))
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type RWT m = AssumeT (Id RWMExp) VarInfo
                        (AssumeT TyConId TyConInfo
                              (AssumeT DataConId DataConInfo (ScopeT m)))

data VarInfo = GlobalVar RWMDefn | LocalVar RWCTy deriving Show
newtype TyConInfo = TyConInfo RWMData deriving Show
data DataConInfo = DataConInfo TyConId RWCDataCon deriving Show

type RW = RWT Identity

queryG :: MonadAssume (Id RWMExp) VarInfo m => Id RWMExp -> m (Maybe RWMDefn)
queryG = query >=> return . \ case
      Just (GlobalVar d) -> Just d
      _                  -> Nothing

mkInitialVarMap :: [RWMDefn] -> Map (Id RWMExp) VarInfo
mkInitialVarMap ds = foldr (\ d@(RWMDefn _ n _ _ _) -> Map.insert n (GlobalVar d)) Map.empty ds

mkInitialTyConMap :: [RWMData] -> Map TyConId TyConInfo
mkInitialTyConMap = foldr (\ d@(RWMData _ n _ _ _) -> Map.insert n (TyConInfo d)) Map.empty

mkInitialDataConMap :: [RWMData] -> Map DataConId DataConInfo
mkInitialDataConMap = foldr addDD Map.empty
      where addDD (RWMData _ dn _ _ dcs) m = foldr (\ d@(RWCDataCon _ cn _) -> Map.insert cn (DataConInfo dn d)) m dcs

mkInitialVarSet :: [RWMDefn] -> Set IdAny
mkInitialVarSet ds = foldr (\ (RWMDefn _ n _ _ _) -> Set.insert (IdAny n)) Set.empty ds

runRWT :: Monad m => RWMProgram -> RWT m a -> m a
runRWT m phi = runScopeTWith varset
             $ runAssumeTWith dmap
             $ runAssumeTWith tmap
             $ runAssumeTWith varmap phi
      where varmap = mkInitialVarMap (defns m)
            tmap   = mkInitialTyConMap (dataDecls m)
            dmap   = mkInitialDataConMap (dataDecls m)
            varset = mkInitialVarSet (defns m)

runRW :: RWMProgram -> RW a -> a
runRW m = runIdentity . runRWT m

askVar :: MonadAssume (Id RWMExp) VarInfo m => RWCTy -> Id RWMExp -> m (Maybe RWMExp)
askVar t = queryG >=> \ case
      Just (RWMDefn _ _ (_ :-> t') _ e) -> do
            sub <- matchty Map.empty t' t
            return $ Just $ subst sub e
      _                                 -> return Nothing


fsubstE :: Monad m => Id RWMExp -> RWMExp -> RWMExp -> RWT m RWMExp
fsubstE n e = fsubstsE [(n, e)]

fsubstsE :: Monad m => [(Id RWMExp, RWMExp)] -> RWMExp -> RWT m RWMExp
fsubstsE s = \ case
      RWMApp an e1 e2      -> RWMApp an <$> fsubstsE s e1 <*> fsubstsE s e2
      RWMLam an n t eb     -> RWMLam an n t <$> fsubstsE s eb
      RWMVar an n t        -> case lookup n s of
            Just e  -> freshenE e
            Nothing -> return $ RWMVar an n t
      RWMCon an dci t      -> return $ RWMCon an dci t
      RWMLiteral an l      -> return $ RWMLiteral an l
      RWMCase an e p e1 e2 -> RWMCase an <$> fsubstsE s e <*> return p <*> fsubstsE s e1 <*> fsubstsE s e2
      RWMNativeVHDL an n e -> RWMNativeVHDL an n <$> fsubstsE s e
      RWMError an m t      -> return $ RWMError an m t

-- *** TODO TODO TODO ***
freshenE :: Monad m => RWMExp -> RWT m RWMExp
freshenE = return
-- freshenE e = do ctr <- getCtr
--                 let (e', ctr') = uniquifyE ctr e
--                 putCtr ctr'
--                 return e'


-- FIXME: begin stuff that should maybe be moved to a separate module
mergesubs :: Monad m => Map (Id RWCTy) RWCTy -> Map (Id RWCTy) RWCTy -> m (Map (Id RWCTy) RWCTy)
mergesubs sub sub' = Map.foldrWithKey f (return sub') sub
      where f n t m = do
                  s <- m
                  case Map.lookup n s of
                        Just t' -> if t `aeq` t'
                              then return s
                              else fail "mergesubs failed"
                        Nothing -> Map.insert n t <$> m

matchty :: Monad m => Map (Id RWCTy) RWCTy -> RWCTy -> RWCTy -> m (Map (Id RWCTy) RWCTy)
matchty sub (RWCTyVar _ n) t                         = case Map.lookup n sub of
      Nothing -> return $ Map.insert n t sub
      Just t' -> if t `aeq` t'
            then return sub
            else fail "matchty failed (variable inconsistency)"
matchty sub (RWCTyCon _ i1) (RWCTyCon _ i2) | i1 == i2 = return sub
matchty sub (RWCTyApp _ t1 t2) (RWCTyApp _ t1' t2')    = do
      sub1 <- matchty sub t1 t1'
      sub2 <- matchty sub t2 t2'
      mergesubs sub1 sub2
matchty sub (RWCTyComp _ t1 t2) (RWCTyComp _ t1' t2')  = do
      sub1 <- matchty sub t1 t1'
      sub2 <- matchty sub t2 t2'
      mergesubs sub1 sub2
matchty _ t1 t2                                        = fail $ "matchty failed (constructor head): " ++ show t1 ++ ", " ++ show t2

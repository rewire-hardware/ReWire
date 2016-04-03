{-# LANGUAGE FlexibleInstances,UndecidableInstances,MultiParamTypeClasses,
             GeneralizedNewtypeDeriving,FlexibleContexts #-}
--
-- This is a handy monad class/transformer with lots of morphisms for stuff
-- you often want to do in ReWire transformations.
--
module ReWire.Core.Monad
  ( RW,RWT
  , TyConInfo(..),DataConInfo(..)
  , runRW
  , queryG,queryT,queryD
  , matchty
  ) where

import ReWire.Core.Syntax
-- import ReWire.FrontEnd.Uniquify (uniquifyE)
import ReWire.Scoping

import Control.Monad.Reader
import Control.Monad.Identity
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

type RWT m = AssumeT (Id RWCExp) VarInfo
                        (AssumeT TyConId TyConInfo
                              (AssumeT DataConId DataConInfo m))

data VarInfo = GlobalVar RWCDefn | LocalVar RWCTy deriving Show
newtype TyConInfo = TyConInfo RWCData deriving Show
data DataConInfo = DataConInfo TyConId RWCDataCon deriving Show

type RW = RWT Identity

queryG :: Monad m => Id RWCExp -> RWT m (Maybe RWCDefn)
queryG x = do
      mvi <- query x
      case mvi of
            Just (GlobalVar d) -> return (Just d)
            _                  -> return Nothing

queryT :: MonadAssume TyConId TyConInfo m => TyConId -> m (Maybe TyConInfo)
queryT t = query t

queryD :: Monad m => DataConId -> RWT m (Maybe DataConInfo)
queryD d = query d

mkInitialVarMap :: [RWCDefn] -> Map (Id RWCExp) VarInfo
mkInitialVarMap ds = foldr (\ d@(RWCDefn _ n _ _ _) -> Map.insert n (GlobalVar d)) Map.empty ds

mkInitialTyConMap :: [RWCData] -> Map TyConId TyConInfo
mkInitialTyConMap = foldr (\ d@(RWCData _ n _ _) -> Map.insert n (TyConInfo d)) Map.empty

mkInitialDataConMap :: [RWCData] -> Map DataConId DataConInfo
mkInitialDataConMap = foldr addDD Map.empty
  where addDD (RWCData _ dn _ dcs) m = foldr (\ d@(RWCDataCon _ cn _) -> Map.insert cn (DataConInfo dn d)) m dcs

mkInitialVarSet :: [RWCDefn] -> Set IdAny
mkInitialVarSet ds = foldr (\ (RWCDefn _ n _ _ _) -> Set.insert (IdAny n)) Set.empty ds

runRWT :: Monad m => RWCProgram -> RWT m a -> m a
runRWT m phi = runAssumeTWith dmap $
                   runAssumeTWith tmap $
                     runAssumeTWith varmap phi
  where varmap = mkInitialVarMap (defns m)
        tmap   = mkInitialTyConMap (dataDecls m)
        dmap   = mkInitialDataConMap (dataDecls m)
        varset = mkInitialVarSet (defns m)

runRW :: RWCProgram -> RW a -> a
runRW m = runIdentity . runRWT m

-- FIXME: begin stuff that should maybe be moved to a separate module
mergesubs :: Monad m => Map (Id RWCTy) RWCTy -> Map (Id RWCTy) RWCTy -> m (Map (Id RWCTy) RWCTy)
mergesubs sub sub' = Map.foldrWithKey f (return sub') sub
   where f n t m = do s <- m
                      case Map.lookup n s of
                        Just t' -> if t `aeq` t' then return s
                                                 else fail "mergesubs failed"
                        Nothing -> liftM (Map.insert n t) m

matchty :: Monad m => Map (Id RWCTy) RWCTy -> RWCTy -> RWCTy -> m (Map (Id RWCTy) RWCTy)
matchty sub (RWCTyVar _ n) t                         = case Map.lookup n sub of
                                                       Nothing -> return (Map.insert n t sub)
                                                       Just t' -> if t `aeq` t' then return sub
                                                                                else fail "matchty failed (variable inconsistency)"
matchty sub (RWCTyCon _ i1) (RWCTyCon _ i2) | i1 == i2 = return sub
matchty sub (RWCTyApp _ t1 t2) (RWCTyApp _ t1' t2')    = do sub1 <- matchty sub t1 t1'
                                                            sub2 <- matchty sub t2 t2'
                                                            mergesubs sub1 sub2
matchty sub (RWCTyComp _ t1 t2) (RWCTyComp _ t1' t2')  = do sub1 <- matchty sub t1 t1'
                                                            sub2 <- matchty sub t2 t2'
                                                            mergesubs sub1 sub2
matchty _ t1 t2                                        = fail $ "matchty failed (constructor head): " ++ show t1 ++ ", " ++ show t2

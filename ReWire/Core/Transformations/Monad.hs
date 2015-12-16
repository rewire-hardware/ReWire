{-# LANGUAGE FlexibleInstances,UndecidableInstances,MultiParamTypeClasses,
             GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

--
-- This is a handy monad class/transformer with lots of morphisms for stuff
-- you often want to do in ReWire transformations.
--
module ReWire.Core.Transformations.Monad where

import Control.Applicative
import Control.Monad.Reader
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Identity
import ReWire.Core.Syntax
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Map (Map)
import Data.List (find)
import Data.Maybe (fromJust)
import ReWire.Scoping
import qualified Data.Set as Set
import Data.Set (Set)
import ReWire.Core.Transformations.Uniquify (uniquify,uniquifyE)

newtype RWT m a = RWT { deRWT :: AssumeT (Id RWCExp) VarInfo
                                  (AssumeT TyConId TyConInfo
                                   (AssumeT DataConId DataConInfo (ScopeT (StateT Int m)))) a }
                  deriving (Functor,Applicative,Monad)

data VarInfo = GlobalVar RWCDefn | LocalVar RWCTy deriving Show
newtype TyConInfo = TyConInfo RWCData deriving Show
data DataConInfo = DataConInfo TyConId RWCDataCon deriving Show

data RWTEnv = RWTEnv { envDataDecls :: [RWCData] }

type RW = RWT Identity

{-
assumingG :: Monad m => Id RWCExp -> RWCDefn -> RWT m a -> RWT m a
assumingG x d m = RWT $ assuming x (GlobalVar d) (deRWT m)

assumingL :: Monad m => Id RWCExp -> RWCTy -> RWT m a -> RWT m a
assumingL x t m = RWT $ assuming x (LocalVar t) (deRWT m)

assumingT :: Monad m => TyConId -> TyConInfo -> RWT m a -> RWT m a
assumingT i inf m = RWT $ AssumeT $ ReaderT $ \ rho -> assuming i inf (runReaderT (deAssumeT (deRWT m)) rho)

assumingD :: Monad m => DataConId -> DataConInfo -> RWT m a -> RWT m a
assumingD i inf m = RWT $
                     AssumeT $ ReaderT $ \ rho0 ->
                      AssumeT $ ReaderT $ \ rho1 ->
                        assuming i inf (runReaderT (deAssumeT
                                        (runReaderT (deAssumeT
                                                     (deRWT m))
                                         rho0))
                                        rho1)

forgettingV,forgettingG,forgettingL :: Monad m => Id RWCExp -> RWT m a -> RWT m a
forgettingV x m = RWT $ forgetting x (deRWT m)
forgettingG = forgettingV
forgettingL = forgettingV

forgettingT :: Monad m => TyConId -> RWT m a -> RWT m a
forgettingT x m = RWT $ AssumeT $ ReaderT $ \ rho -> forgetting x (runReaderT (deAssumeT (deRWT m)) rho)

forgettingD :: Monad m => DataConId -> RWT m a -> RWT m a
forgettingD x m = RWT $
                   AssumeT $ ReaderT $ \rho0 ->
                    AssumeT $ ReaderT $ \rho1 ->
                      forgetting x (runReaderT (deAssumeT
                                    (runReaderT (deAssumeT
                                                 (deRWT m))
                                     rho0))
                                    rho1)

queryV :: Monad m => Id RWCExp -> RWT m (Maybe VarInfo)
queryV x = RWT $ query x

queryP :: Monad m => Id RWCExp -> RWT m (Maybe RWCPrim)
queryP x = RWT $
            do mvi <- query x
               case mvi of
                 Just (PrimVar p) -> return (Just p)
                 _                -> return Nothing
-}

queryG :: Monad m => Id RWCExp -> RWT m (Maybe RWCDefn)
queryG x = RWT $
            do mvi <- query x
               case mvi of
                 Just (GlobalVar d) -> return (Just d)
                 _                  -> return Nothing

{-
queryL :: Monad m => Id RWCExp -> RWT m (Maybe RWCTy)
queryL x = RWT $
            do mvi <- query x
               case mvi of
                 Just (LocalVar t)  -> return (Just t)
                 _                  -> return Nothing

-}

queryT :: Monad m => TyConId -> RWT m (Maybe TyConInfo)
queryT t = RWT $ lift $ query t

queryD :: Monad m => DataConId -> RWT m (Maybe DataConInfo)
queryD d = RWT $ lift $ lift $ query d

{-
getAssumptionsV :: Monad m => RWT m (Map (Id RWCExp) VarInfo)
getAssumptionsV = RWT getAssumptions

getAssumptionsG :: Monad m => RWT m (Map (Id RWCExp) RWCDefn)
getAssumptionsG = RWT $
                   do
                    m <- getAssumptions
                    let deG (GlobalVar x) = Just x
                        deG _             = Nothing
                    return (Map.mapMaybe deG m)

getAssumptionsL :: Monad m => RWT m (Map (Id RWCExp) RWCTy)
getAssumptionsL = RWT $
                   do
                    m <- getAssumptions
                    let deL (LocalVar x) = Just x
                        deL _            = Nothing
                    return (Map.mapMaybe deL m)

getAssumptionsT :: Monad m => RWT m (Map TyConId TyConInfo)
getAssumptionsT = RWT $ lift getAssumptions

getAssumptionsD :: Monad m => RWT m (Map DataConId DataConInfo)
getAssumptionsD = RWT $ lift $ lift getAssumptions
-}

mkInitialVarMap :: [RWCDefn] -> Map (Id RWCExp) VarInfo
mkInitialVarMap ds = foldr (\ d@(RWCDefn n _ _ _) -> Map.insert n (GlobalVar d)) Map.empty ds

mkInitialTyConMap :: [RWCData] -> Map TyConId TyConInfo
mkInitialTyConMap = foldr (\ d@(RWCData n _ _) -> Map.insert n (TyConInfo d)) Map.empty

mkInitialDataConMap :: [RWCData] -> Map DataConId DataConInfo
mkInitialDataConMap = foldr addDD Map.empty
  where addDD (RWCData dn _ dcs) m = foldr (\ d@(RWCDataCon cn _) -> Map.insert cn (DataConInfo dn d)) m dcs

mkInitialVarSet :: [RWCDefn] -> Set IdAny
mkInitialVarSet ds = foldr (\ d@(RWCDefn n _ _ _) -> Set.insert (IdAny n)) Set.empty ds

runRWT :: Monad m => Int -> RWCModule -> RWT m a -> m a
runRWT ctr m phi = liftM fst $
                     runStateT (runScopeTWith varset $
                                 runAssumeTWith dmap $
                                  runAssumeTWith tmap $
                                   runAssumeTWith varmap $
                                    deRWT phi)
                               ctr
  where varmap      = mkInitialVarMap (defns m)
        tmap        = mkInitialTyConMap (dataDecls m)
        dmap        = mkInitialDataConMap (dataDecls m)
        varset      = mkInitialVarSet (defns m)

runRW :: Int -> RWCModule -> RW a -> a
runRW ctr m = runIdentity . runRWT ctr m

getCtr :: Monad m => RWT m Int
getCtr = RWT get

putCtr :: Monad m => Int -> RWT m ()
putCtr = RWT . put

fsubstE n e = fsubstsE [(n,e)]

fsubstsE :: Monad m => [(Id RWCExp,RWCExp)] -> RWCExp -> RWT m RWCExp
fsubstsE s (RWCApp e1 e2)      = do e1' <- fsubstsE s e1
                                    e2' <- fsubstsE s e2
                                    return (RWCApp e1' e2')
fsubstsE s (RWCLam n t eb)     = do eb' <- fsubstsE s eb
                                    return (RWCLam n t eb')
fsubstsE s (RWCVar n t)        = case lookup n s of
                                   Just e  -> freshenE e
                                   Nothing -> return (RWCVar n t)
fsubstsE s (RWCCon dci t)      = return (RWCCon dci t)
fsubstsE s (RWCLiteral l)      = return (RWCLiteral l)
fsubstsE s (RWCCase esc alts)  = do esc'  <- fsubstsE s esc
                                    alts' <- mapM fsubstsE_Alt alts
                                    return (RWCCase esc' alts')
  where fsubstsE_Alt (RWCAlt p eb) = do eb' <- fsubstsE s eb
                                        return (RWCAlt p eb')
fsubstsE s (RWCNativeVHDL n e) = liftM (RWCNativeVHDL n) (fsubstsE s e)

freshenE :: Monad m => RWCExp -> RWT m RWCExp
freshenE e = do ctr <- getCtr
                let (e',ctr') = uniquifyE ctr e
                putCtr ctr'
                return e'

askVar :: Monad m => RWCTy -> Id RWCExp -> RWT m (Maybe RWCExp)
askVar t n = do md <- queryG n
                case md of
                  Just (RWCDefn _ (tvs :-> t') _ e) -> do sub <- matchty Map.empty t' t
                                                          e'  <- freshenE (subst sub e)
                                                          return (Just e')
                  _                                 -> return Nothing

{-
askDefn :: MonadReWire m => Name RWCExp -> m (Maybe RWCDefn)
askDefn n = do defns <- askDefns
               return $ find (\(RWCDefn n' _) -> n==n') defns

askConDataDecl :: MonadReWire m => Identifier -> m (Maybe RWCData)
askConDataDecl i = do dds <- askDataDecls
                      liftM msum $ mapM checkDD dds
  where checkDD d@(RWCData _ b) = lunbind b $ \ (_,dcs) ->
                                    if any (\(RWCDataCon i' _) -> i==i') dcs
                                       then return (Just d)
                                       else return Nothing
-}

-- FIXME: begin stuff that should maybe be moved to a separate module
mergesubs :: Monad m => Map (Id RWCTy) RWCTy -> Map (Id RWCTy) RWCTy -> m (Map (Id RWCTy) RWCTy)
mergesubs sub sub' = Map.foldrWithKey f (return sub') sub
   where f n t m = do s <- m
                      case Map.lookup n s of
                        Just t' -> if t `aeq` t' then return s
                                                 else fail "mergesubs failed"
                        Nothing -> liftM (Map.insert n t) m

matchty :: Monad m => Map (Id RWCTy) RWCTy -> RWCTy -> RWCTy -> m (Map (Id RWCTy) RWCTy)
matchty sub (RWCTyVar n) t                         = case Map.lookup n sub of
                                                       Nothing -> return (Map.insert n t sub)
                                                       Just t' -> if t `aeq` t' then return sub
                                                                                else fail "matchty failed (variable inconsistency)"
matchty sub (RWCTyCon i1) (RWCTyCon i2) | i1 == i2 = return sub
matchty sub (RWCTyApp t1 t2) (RWCTyApp t1' t2')    = do sub1 <- matchty sub t1 t1'
                                                        sub2 <- matchty sub t2 t2'
                                                        mergesubs sub1 sub2
matchty sub (RWCTyComp t1 t2) (RWCTyComp t1' t2')  = do sub1 <- matchty sub t1 t1'
                                                        sub2 <- matchty sub t2 t2'
                                                        mergesubs sub1 sub2
matchty _ t1 t2                                    = fail $ "matchty failed (constructor head): " ++ show t1 ++ ", " ++ show t2

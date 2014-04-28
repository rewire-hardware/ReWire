{-# LANGUAGE FlexibleInstances,UndecidableInstances,MultiParamTypeClasses, 
             GeneralizedNewtypeDeriving #-}

--
-- This is a handy monad class/transformer with lots of morphisms for stuff
-- you often want to do in ReWire transformations.
--
module ReWire.Core.Transformations.Monad where

import Control.Monad.Reader
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Identity
import ReWire.Core.Syntax
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Map (Map)
import Control.Monad.Error
import Data.List (find)
import Data.Maybe (fromJust)
import ReWire.Scoping

newtype RWT m a = RWT { deRWT :: AssumeT (Id RWCExp) VarInfo
                                 (AssumeT TyConId TyConInfo
                                  (AssumeT DataConId DataConInfo m)) a }
                  deriving Monad

instance MonadTrans RWT where
  lift = RWT . lift . lift . lift

data VarInfo = GlobalVar RWCDefn | LocalVar RWCTy deriving Show
newtype TyConInfo = TyConInfo RWCData deriving Show
data DataConInfo = DataConInfo TyConId RWCDataCon deriving Show

data RWTEnv = RWTEnv { envDataDecls :: [RWCData] }

type RW = RWT Identity

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
queryV x = RWT (query x)

queryG :: Monad m => Id RWCExp -> RWT m (Maybe RWCDefn)
queryG x = RWT $
            do mvi <- query x
               case mvi of
                 Nothing            -> return Nothing
                 Just (LocalVar _)  -> return Nothing
                 Just (GlobalVar d) -> return (Just d)

queryL :: Monad m => Id RWCExp -> RWT m (Maybe RWCTy)
queryL x = RWT $
            do mvi <- query x
               case mvi of
                 Nothing            -> return Nothing
                 Just (LocalVar t)  -> return (Just t)
                 Just (GlobalVar d) -> return Nothing

queryT :: Monad m => TyConId -> RWT m (Maybe TyConInfo)
queryT t = RWT $ lift $ query t

queryD :: Monad m => DataConId -> RWT m (Maybe DataConInfo)
queryD d = RWT $ lift $ lift $ query d

getAssumptionsV :: Monad m => RWT m (Map (Id RWCExp) VarInfo)
getAssumptionsV = RWT getAssumptions

getAssumptionsG :: Monad m => RWT m (Map (Id RWCExp) RWCDefn)
getAssumptionsG = RWT $ do
                   m <- getAssumptions
                   let deG (GlobalVar x) = Just x
                       deG _             = Nothing
                   return (Map.mapMaybe deG m)

getAssumptionsL :: Monad m => RWT m (Map (Id RWCExp) RWCTy)
getAssumptionsL = RWT $ do
                   m <- getAssumptions
                   let deL (LocalVar x) = Just x
                       deL _            = Nothing
                   return (Map.mapMaybe deL m)

getAssumptionsT :: Monad m => RWT m (Map TyConId TyConInfo)
getAssumptionsT = RWT (lift getAssumptions)

getAssumptionsD :: Monad m => RWT m (Map DataConId DataConInfo)
getAssumptionsD = RWT $ lift $ lift getAssumptions

inLambdas :: Monad m => RWCExp -> ([(Id RWCExp,RWCTy)] -> RWCExp -> RWT m a) -> RWT m a
inLambdas (RWCLam x t e) k = inLambdas e (\ xts e' -> assumingL x t (k ((x,t):xts) e'))
inLambdas e k              = k [] e

inPattern :: Monad m => RWCPat -> ([(Id RWCExp,RWCTy)] -> RWT m a) -> RWT m a
inPattern (RWCPatCon _ ps) k  = inPatterns ps k
  where inPatterns (p:ps) k = inPattern p (\ xts -> inPatterns ps (\ xts' -> k (xts++xts')))
        inPatterns [] k     = k []
inPattern (RWCPatLiteral _) k = k []
inPattern (RWCPatVar x t) k   = assumingL x t (k [(x,t)])

mkInitialVarMap :: [RWCDefn] -> Map (Id RWCExp) VarInfo
mkInitialVarMap = foldr (\ d@(RWCDefn n _ _) -> Map.insert n (GlobalVar d)) Map.empty

mkInitialTyConMap :: [RWCData] -> Map TyConId TyConInfo
mkInitialTyConMap = foldr (\ d@(RWCData n _ _) -> Map.insert n (TyConInfo d)) Map.empty

mkInitialDataConMap :: [RWCData] -> Map DataConId DataConInfo
mkInitialDataConMap = foldr addDD Map.empty
  where addDD (RWCData dn _ dcs) m = foldr (\ d@(RWCDataCon cn _) -> Map.insert cn (DataConInfo dn d)) m dcs

runRWT :: Monad m => RWCProg -> RWT m a -> m a
runRWT p (RWT phi) = runAssumeTWith dmap $
                      runAssumeTWith tmap $
                       runAssumeTWith varmap $
                        phi
  where varmap = mkInitialVarMap (defns p)
        tmap   = mkInitialTyConMap (dataDecls p)
        dmap   = mkInitialDataConMap (dataDecls p)

runRW :: RWCProg -> RW a -> a
runRW p = runIdentity . runRWT p

instance MonadState s m => MonadState s (RWT m) where
  get = lift get
  put = lift . put

-- Gott im Himmel!
instance MonadError e m => MonadError e (RWT m) where
  throwError = lift . throwError
  catchError = liftCatch catchError
    where peel m = runReaderT (deAssumeT m)
          unpeel = AssumeT . ReaderT
          liftCatch f m h =
            RWT $ unpeel $ \ rho0 ->
                   unpeel $ \ rho1 ->
                    unpeel $ \ rho2 ->
                     f (peel (peel (peel (deRWT m) rho0) rho1) rho2)
                       (\ e -> peel (peel (peel (deRWT (h e)) rho0) rho1) rho2)

{-
askvar :: MonadReWire m => RWCTy -> Name RWCExp -> m RWCExp
askvar t n = do ds <- askDefns
                case find (\ (RWCDefn n' _) -> n == n') ds of
                  Just (RWCDefn _ (Embed b)) -> lunbind b (\(tvs,(t',e)) ->
                                                 do sub <- matchty [] t' t
                                                    return (substs sub e))
                  _                          -> return (RWCVar t n)

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

-- FIXME: begin stuff that should maybe be moved to a separate module
mergesubs :: Monad m => [(Name RWCTy,RWCTy)] -> [(Name RWCTy,RWCTy)] -> m [(Name RWCTy,RWCTy)]
mergesubs ((n,t):sub) sub' = case lookup n sub' of
                               Just t' -> if t `aeq` t' then mergesubs sub sub'
                                                        else fail "mergesubs failed"
                               Nothing -> do sub'' <- mergesubs sub sub'
                                             return ((n,t):sub'')
mergesubs [] sub'          = return sub'

matchty :: Monad m => [(Name RWCTy,RWCTy)] -> RWCTy -> RWCTy -> m [(Name RWCTy,RWCTy)]
matchty sub (RWCTyVar n) t                         = case lookup n sub of
                                                       Nothing -> return ((n,t):sub)
                                                       Just t' -> if t `aeq` t' then return sub
                                                                                else fail "matchty failed (variable inconsistency)"
matchty sub (RWCTyCon i1) (RWCTyCon i2) | i1 == i2 = return sub
matchty sub (RWCTyApp t1 t2) (RWCTyApp t1' t2')    = do sub1 <- matchty [] t1 t1'
                                                        sub2 <- matchty [] t2 t2'
                                                        mergesubs sub1 sub2
matchty _ t1 t2                                    = fail $ "matchty failed (constructor head): " ++ show t1 ++ ", " ++ show t2
-- FIXME: end stuff that should maybe be moved to a separate module-}
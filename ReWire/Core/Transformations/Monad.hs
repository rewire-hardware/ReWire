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
import Debug.Trace (trace)
import qualified Data.Set as Set
import Data.Set (Set)

type RWT m = AssumeT (Id RWCExp) VarInfo
              (AssumeT TyConId TyConInfo
               (AssumeT DataConId DataConInfo (ScopeT m)))

data VarInfo = GlobalVar RWCDefn | LocalVar RWCTy deriving Show
newtype TyConInfo = TyConInfo RWCData deriving Show
data DataConInfo = DataConInfo TyConId RWCDataCon deriving Show

data RWTEnv = RWTEnv { envDataDecls :: [RWCData] }

type RW = RWT Identity

assumingG :: Monad m => Id RWCExp -> RWCDefn -> RWT m a -> RWT m a
assumingG x d m = assuming x (GlobalVar d) m

assumingL :: Monad m => Id RWCExp -> RWCTy -> RWT m a -> RWT m a
assumingL x t m = assuming x (LocalVar t) m

assumingT :: Monad m => TyConId -> TyConInfo -> RWT m a -> RWT m a
assumingT i inf m = AssumeT $ ReaderT $ \ rho -> assuming i inf (runReaderT (deAssumeT m) rho)

assumingD :: Monad m => DataConId -> DataConInfo -> RWT m a -> RWT m a
assumingD i inf m = AssumeT $ ReaderT $ \ rho0 ->
                     AssumeT $ ReaderT $ \ rho1 ->
                       assuming i inf (runReaderT (deAssumeT
                                       (runReaderT (deAssumeT
                                                    m)
                                        rho0))
                                       rho1)

forgettingV,forgettingG,forgettingL :: Monad m => Id RWCExp -> RWT m a -> RWT m a
forgettingV x m = forgetting x m
forgettingG = forgettingV
forgettingL = forgettingV

forgettingT :: Monad m => TyConId -> RWT m a -> RWT m a
forgettingT x m = AssumeT $ ReaderT $ \ rho -> forgetting x (runReaderT (deAssumeT m) rho)

forgettingD :: Monad m => DataConId -> RWT m a -> RWT m a
forgettingD x m = AssumeT $ ReaderT $ \rho0 ->
                   AssumeT $ ReaderT $ \rho1 ->
                     forgetting x (runReaderT (deAssumeT
                                   (runReaderT (deAssumeT
                                                m)
                                    rho0))
                                   rho1)

queryV :: Monad m => Id RWCExp -> RWT m (Maybe VarInfo)
queryV x = query x

queryG :: Monad m => Id RWCExp -> RWT m (Maybe RWCDefn)
queryG x = do mvi <- query x
              case mvi of
                Nothing            -> return Nothing
                Just (LocalVar _)  -> return Nothing
                Just (GlobalVar d) -> return (Just d)

queryL :: Monad m => Id RWCExp -> RWT m (Maybe RWCTy)
queryL x = do mvi <- query x
              case mvi of
                Nothing            -> return Nothing
                Just (LocalVar t)  -> return (Just t)
                Just (GlobalVar d) -> return Nothing

queryT :: Monad m => TyConId -> RWT m (Maybe TyConInfo)
queryT t = lift $ query t

queryD :: Monad m => DataConId -> RWT m (Maybe DataConInfo)
queryD d = lift $ lift $ query d

getAssumptionsV :: Monad m => RWT m (Map (Id RWCExp) VarInfo)
getAssumptionsV = getAssumptions

getAssumptionsG :: Monad m => RWT m (Map (Id RWCExp) RWCDefn)
getAssumptionsG = do
                   m <- getAssumptions
                   let deG (GlobalVar x) = Just x
                       deG _             = Nothing
                   return (Map.mapMaybe deG m)

getAssumptionsL :: Monad m => RWT m (Map (Id RWCExp) RWCTy)
getAssumptionsL = do
                   m <- getAssumptions
                   let deL (LocalVar x) = Just x
                       deL _            = Nothing
                   return (Map.mapMaybe deL m)

getAssumptionsT :: Monad m => RWT m (Map TyConId TyConInfo)
getAssumptionsT = lift getAssumptions

getAssumptionsD :: Monad m => RWT m (Map DataConId DataConInfo)
getAssumptionsD = lift $ lift getAssumptions

inLambdas :: Monad m => RWCExp -> ([(Id RWCExp,RWCTy)] -> RWCExp -> RWT m a) -> RWT m a
inLambdas (RWCLam x_ t e_) k = refreshingVar x_ e_ $ \ x e ->
                                 inLambdas e (\ xts e' -> assumingL x t (k ((x,t):xts) e'))
inLambdas e k                = k [] e

refreshingPat :: Monad m => RWCPat -> RWCExp -> (RWCPat -> RWCExp -> RWT m a) -> RWT m a
refreshingPat (RWCPatCon i ps) e k  = refreshingPats ps e (\ ps' e' -> k (RWCPatCon i ps') e')
  where refreshingPats (p:ps) e k = refreshingPat p e (\ p' e' -> refreshingPats ps e' (\ ps' e'' -> k (p':ps') e''))
        refreshingPats [] e k     = k [] e
refreshingPat (RWCPatLiteral l) e k = k (RWCPatLiteral l) e
refreshingPat (RWCPatVar x_ t) e_ k = refreshingVar x_ e_ $ \ x e ->
                                       k (RWCPatVar x t) e

inAlt :: Monad m => RWCAlt -> (RWCPat -> RWCExp -> RWT m a) -> RWT m a
inAlt (RWCAlt p_ e_) = refreshingPat p_ e_

--inPattern :: Monad m => RWCPat -> ([(Id RWCExp,RWCTy)] -> RWT m a) -> RWT m a
--inPattern (RWCPatCon _ ps) k  = inPatterns ps k
--  where inPatterns (p:ps) k = inPattern p (\ xts -> inPatterns ps (\ xts' -> k (xts++xts')))
--        inPatterns [] k     = k []
--inPattern (RWCPatLiteral _) k = k []
--inPattern (RWCPatVar x t) k   = assumingL x t (k [(x,t)])

mkInitialVarMap :: [RWCDefn] -> Map (Id RWCExp) VarInfo
mkInitialVarMap = foldr (\ d@(RWCDefn n _ _) -> Map.insert n (GlobalVar d)) Map.empty

mkInitialTyConMap :: [RWCData] -> Map TyConId TyConInfo
mkInitialTyConMap = foldr (\ d@(RWCData n _ _) -> Map.insert n (TyConInfo d)) Map.empty

mkInitialDataConMap :: [RWCData] -> Map DataConId DataConInfo
mkInitialDataConMap = foldr addDD Map.empty
  where addDD (RWCData dn _ dcs) m = foldr (\ d@(RWCDataCon cn _) -> Map.insert cn (DataConInfo dn d)) m dcs

mkInitialVarSet :: [RWCDefn] -> Set IdAny
mkInitialVarSet = foldr (\ d@(RWCDefn n _ _) -> Set.insert (IdAny n)) Set.empty

runRWT :: Monad m => RWCProg -> RWT m a -> m a
runRWT p phi = runScopeTWith varset $
                runAssumeTWith dmap $
                 runAssumeTWith tmap $
                  runAssumeTWith varmap $
                   phi
  where varmap = mkInitialVarMap (defns p)
        tmap   = mkInitialTyConMap (dataDecls p)
        dmap   = mkInitialDataConMap (dataDecls p)
        varset = mkInitialVarSet (defns p)

runRW :: RWCProg -> RW a -> a
runRW p = runIdentity . runRWT p

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
-}

-- FIXME: begin stuff that should maybe be moved to a separate module
mergesubs :: Monad m => Map (Id RWCTy) RWCTy -> Map (Id RWCTy) RWCTy -> m (Map (Id RWCTy) RWCTy)
mergesubs sub sub' = Map.foldrWithKey f (return sub') sub
   where f n t m = do s <- m
                      case Map.lookup n s of
                        Just t' -> if t `aeq` t' then return s
                                                 else fail "mergesubs failed"
                        Nothing -> liftM (Map.insert n t) m
                        
--mergesubs ((n,t):sub) sub' = case lookup n sub' of
--                               Just t' -> if t `aeq` t' then mergesubs sub sub'
--                                                        else fail "mergesubs failed"
--                               Nothing -> do sub'' <- mergesubs sub sub'
--                                             return ((n,t):sub'')
--mergesubs [] sub'          = return sub'

matchty :: Monad m => Map (Id RWCTy) RWCTy -> RWCTy -> RWCTy -> m (Map (Id RWCTy) RWCTy)
matchty sub (RWCTyVar n) t                         = case Map.lookup n sub of
                                                       Nothing -> return (Map.insert n t sub)
                                                       Just t' -> if t `aeq` t' then return sub
                                                                                else fail "matchty failed (variable inconsistency)"
matchty sub (RWCTyCon i1) (RWCTyCon i2) | i1 == i2 = return sub
matchty sub (RWCTyApp t1 t2) (RWCTyApp t1' t2')    = do sub1 <- matchty sub t1 t1'
                                                        sub2 <- matchty sub t2 t2'
                                                        mergesubs sub1 sub2
matchty _ t1 t2                                    = fail $ "matchty failed (constructor head): " ++ show t1 ++ ", " ++ show t2

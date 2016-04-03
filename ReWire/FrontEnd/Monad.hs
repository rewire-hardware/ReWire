{-# LANGUAGE FlexibleContexts, LambdaCase #-}
--
-- This is a handy monad class/transformer with lots of morphisms for stuff
-- you often want to do in ReWire transformations.
--
module ReWire.FrontEnd.Monad
      ( RW, RWT
      , VarInfo (..)
      , askVar
      , runRWT
      , fsubstE, fsubstsE
      ) where

import ReWire.FrontEnd.Syntax
import ReWire.Scoping
import ReWire.SYB hiding (query)

import Control.Monad ((>=>))
import Control.Monad.Identity (Identity (..))
import Data.Map (Map)

import qualified Data.Map.Strict as Map

type RWT m = AssumeT (Id RWMExp) VarInfo m

data VarInfo = GlobalVar RWMDefn | LocalVar RWCTy deriving Show
newtype TyConInfo = TyConInfo RWMData deriving Show
data DataConInfo = DataConInfo TyConId RWCDataCon deriving Show

type RW = RWT Identity

queryG :: MonadAssume (Id RWMExp) VarInfo m => Id RWMExp -> m (Maybe RWMDefn)
queryG = query >=> return . \ case
      Just (GlobalVar d) -> Just d
      _                  -> Nothing

mkInitialVarMap :: [RWMDefn] -> Map (Id RWMExp) VarInfo
mkInitialVarMap = runQ
       $ (\ d@(RWMDefn _ x _ _ _ _) -> Map.insert x (GlobalVar d) mempty)
      ||? \ case
            RWMLam _ x t _          -> Map.insert x (LocalVar t) mempty
            _                       -> mempty
      ||? \ case
            RWMPatVar _ x t         -> Map.insert x (LocalVar t) mempty
            _                       -> mempty
      ||? QEmpty

runRWT :: Monad m => RWMProgram -> (RWMProgram -> RWT m RWMProgram) -> m RWMProgram
runRWT m phi = runAssumeTWith varmap
             $ phi m
      where varmap = mkInitialVarMap $ defns m

askVar :: MonadAssume (Id RWMExp) VarInfo m => RWCTy -> Id RWMExp -> m (Maybe RWMExp)
askVar t = queryG >=> return . \ case
      Just (RWMDefn _ _ (_ :-> t') _ _ e) -> Just $ subst (matchty mempty t' t) e
      _                                   -> Nothing

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
mergesubs :: Map (Id RWCTy) RWCTy -> Map (Id RWCTy) RWCTy -> Map (Id RWCTy) RWCTy
mergesubs sub sub' = Map.foldrWithKey f sub' sub
      where f n t m = case Map.lookup n m of
                  Just t' -> if t `aeq` t'
                        then m
                        else error "mergesubs failed"
                  Nothing -> Map.insert n t m

matchty :: Map (Id RWCTy) RWCTy -> RWCTy -> RWCTy -> Map (Id RWCTy) RWCTy
matchty sub (RWCTyVar _ n) t                           = case Map.lookup n sub of
      Nothing -> Map.insert n t sub
      Just t' -> if t `aeq` t'
            then sub
            else error "matchty failed (variable inconsistency)"
matchty sub (RWCTyCon _ i1) (RWCTyCon _ i2) | i1 == i2 = sub
matchty sub (RWCTyApp _ t1 t2) (RWCTyApp _ t1' t2')    = mergesubs (matchty sub t1 t1') (matchty sub t2 t2')
matchty sub (RWCTyComp _ t1 t2) (RWCTyComp _ t1' t2')  = mergesubs (matchty sub t1 t1') (matchty sub t2 t2')
matchty _ t1 t2                                        = error $ "matchty failed (constructor head): " ++ show t1 ++ ", " ++ show t2

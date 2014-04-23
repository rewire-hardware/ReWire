{-# LANGUAGE MultiParamTypeClasses,GeneralizedNewtypeDeriving,FlexibleInstances #-}

module ReWire.Core.Syntax where

import Data.Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

newtype Id a = Id { deId :: String } deriving (Eq,Ord,Show,Read)
type ConId = String

data RWCTy = RWCTyApp RWCTy RWCTy
           | RWCTyCon ConId
           | RWCTyVar (Id RWCTy)
           deriving Show

data RWCExp = RWCApp RWCExp RWCExp
            | RWCLam (Id RWCExp) RWCTy RWCExp
            | RWCVar (Id RWCExp) [RWCTy]
            | RWCCon ConId [RWCTy]
            | RWCLiteral RWCLit
            | RWCCase RWCExp [RWCAlt]
            deriving Show

data RWCLit = RWCLitInteger Integer
            | RWCLitFloat Double
            | RWCLitChar Char
            deriving (Eq,Show)

data RWCAlt = RWCAlt RWCPat RWCExp
              deriving Show

data RWCPat = RWCPatCon ConId [RWCPat]
            | RWCPatLiteral RWCLit
            | RWCPatVar (Id RWCExp) RWCTy
            deriving Show

data RWCDefn = RWCDefn { defnName   :: Id RWCExp,
                         defnPolyTy :: Poly RWCTy,
                         defnBody   :: RWCExp }
               deriving Show

data RWCData = RWCData { dataName   :: ConId,
                         dataTyVars :: [Id RWCTy],
                         dataCons   :: [RWCDataCon] }
               deriving Show

data RWCDataCon = RWCDataCon ConId [RWCTy]
                  deriving Show

data RWCProg = RWCProg { dataDecls    :: [RWCData],
                         defns        :: [RWCDefn] }
                       deriving Show

flattenArrow :: RWCTy -> ([RWCTy],RWCTy)
flattenArrow (RWCTyApp (RWCTyApp (RWCTyCon "(->)") t1) t2) = let (ts,t) = flattenArrow t2 in (t1:ts,t)
flattenArrow t                                             = ([],t)
  
flattenTyApp :: RWCTy -> [RWCTy]
flattenTyApp (RWCTyApp t1 t2) = flattenTyApp t1 ++ [t2]
flattenTyApp t                = [t]

flattenApp :: RWCExp -> [RWCExp]
flattenApp (RWCApp e e') = flattenApp e++[e']
flattenApp e             = [e]                                                     

mkArrow :: RWCTy -> RWCTy -> RWCTy
mkArrow t1 t2 = RWCTyApp (RWCTyApp (RWCTyCon "(->)") t1) t2

arrowLeft :: RWCTy -> RWCTy
arrowLeft (RWCTyApp (RWCTyApp (RWCTyCon "(->)") t1) t2) = t1

tyTVs :: RWCTy -> [Id RWCTy]
tyTVs (RWCTyApp t1 t2) = tyTVs t1 ++ tyTVs t2
tyTVs (RWCTyCon _)     = []
tyTVs (RWCTyVar v)     = [v]

class Subst t t' where
  substs :: Map (Id t') t' -> t -> t

newtype AlphaM a = AlphaM { deAlphaM :: StateT (Map String String) Maybe a }
  deriving (Functor,Monad,MonadPlus)

equating :: Id t -> Id t -> AlphaM a -> AlphaM a
equating (Id x) (Id y) m = do let l = min x y
                                  r = max x y
                              vm <- AlphaM get
                              case Map.lookup l vm of
                                Just z | z==r      -> return ()
                                       | otherwise -> mzero
                                Nothing            -> AlphaM (modify (Map.insert l r))
                              m

equatings :: [Id t] -> [Id t] -> AlphaM a -> AlphaM a
equatings l1 l2 m | length l1 /= length l2 = mzero
                  | otherwise              = eqs' l1 l2
   where eqs' (x:xs) (y:ys) = equating x y (eqs' xs ys)
         eqs' [] []         = m

class Alpha t where
  aeq' :: t -> t -> AlphaM ()

instance Alpha (Id t) where
  aeq' (Id x) (Id y) = do let l = min x y
                              r = max x y
                          vm <- AlphaM get
                          case Map.lookup l vm of
                            Nothing | x == y    -> return ()
                                    | otherwise -> mzero
                            Just z  | z == r    -> return ()
                                    | otherwise -> mzero

aeq :: Alpha a => a -> a -> Bool
aeq x y = case runStateT (deAlphaM (aeq' x y)) Map.empty of
            Just _  -> True
            Nothing -> False

infix 4 `aeq`

class FV t t' where
  fv :: t -> [Id t']
  
data Poly t = [Id t] :-> t deriving Show

instance Alpha t => Alpha (Poly t) where
  aeq' (vs :-> x) (us :-> y) = equatings vs us (aeq' x y)

instance FV t t => FV (Poly t) t where
  fv (vs :-> x) = [v | v <- fv x, not (v `elem` vs)]

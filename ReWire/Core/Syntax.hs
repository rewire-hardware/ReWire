{-# LANGUAGE MultiParamTypeClasses,GeneralizedNewtypeDeriving,FlexibleInstances #-}

module ReWire.Core.Syntax where

import Data.Set hiding (map)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.DeepSeq

newtype Id a = Id { deId :: String } deriving (Eq,Ord,Show,Read,NFData)

type ConId = String

data RWCTy = RWCTyApp RWCTy RWCTy
           | RWCTyCon ConId
           | RWCTyVar (Id RWCTy)
           deriving Show

instance NFData RWCTy where
  rnf (RWCTyApp t1 t2) = t1 `deepseq` t2 `deepseq` ()
  rnf (RWCTyCon i)     = i `deepseq` ()
  rnf (RWCTyVar x)     = x `deepseq` ()

data RWCExp = RWCApp RWCExp RWCExp
            | RWCLam (Id RWCExp) RWCTy RWCExp
            | RWCVar (Id RWCExp) RWCTy
            | RWCCon ConId RWCTy
            | RWCLiteral RWCLit
            | RWCCase RWCExp [RWCAlt]
            deriving Show

instance NFData RWCExp where
  rnf (RWCApp e1 e2)   = e1 `deepseq` e2 `deepseq` ()
  rnf (RWCLam x t e)   = x `deepseq` t `deepseq` e `deepseq` ()
  rnf (RWCVar x t)     = x `deepseq` t `deepseq` ()
  rnf (RWCCon i t)     = i `deepseq` t `deepseq` ()
  rnf (RWCLiteral l)   = l `deepseq` ()
  rnf (RWCCase e alts) = e `deepseq` alts `deepseq` ()

data RWCLit = RWCLitInteger Integer
            | RWCLitFloat Double
            | RWCLitChar Char
            deriving (Eq,Show)

instance NFData RWCLit where
  rnf (RWCLitInteger i) = i `deepseq` ()
  rnf (RWCLitFloat d)   = d `deepseq` ()
  rnf (RWCLitChar c)    = c `deepseq` ()

data RWCAlt = RWCAlt RWCPat RWCExp
              deriving Show

instance NFData RWCAlt where
  rnf (RWCAlt p e) = p `deepseq` e `deepseq` ()

data RWCPat = RWCPatCon ConId [RWCPat]
            | RWCPatLiteral RWCLit
            | RWCPatVar (Id RWCExp) RWCTy
            deriving Show

instance NFData RWCPat where
  rnf (RWCPatCon i ps)  = i `deepseq` ps `deepseq` ()
  rnf (RWCPatLiteral l) = l `deepseq` ()
  rnf (RWCPatVar x t)   = x `deepseq` t `deepseq` ()

data RWCDefn = RWCDefn { defnName   :: Id RWCExp,
                         defnPolyTy :: Poly RWCTy,
                         defnBody   :: RWCExp }
               deriving Show

instance NFData RWCDefn where
  rnf (RWCDefn n pt e) = n `deepseq` pt `deepseq` e `deepseq` ()

data RWCData = RWCData { dataName   :: ConId,
                         dataTyVars :: [Id RWCTy],
                         dataCons   :: [RWCDataCon] }
               deriving Show

instance NFData RWCData where
  rnf (RWCData i tvs dcs) = i `deepseq` tvs `deepseq` dcs `deepseq` ()

data RWCDataCon = RWCDataCon ConId [RWCTy]
                  deriving Show

instance NFData RWCDataCon where
  rnf (RWCDataCon i ts) = i `deepseq` ts `deepseq` ()

data RWCProg = RWCProg { dataDecls    :: [RWCData],
                         defns        :: [RWCDefn] }
                       deriving Show

instance NFData RWCProg where
  rnf (RWCProg dds defs) = dds `deepseq` defs `deepseq` ()

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
  subst :: Map (Id t') t' -> t -> t

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

instance NFData t => NFData (Poly t) where
  rnf (vs :-> x) = vs `deepseq` x `deepseq` ()

instance Alpha t => Alpha (Poly t) where
  aeq' (vs :-> x) (us :-> y) = equatings vs us (aeq' x y)

instance FV t t => FV (Poly t) t where
  fv (vs :-> x) = [v | v <- fv x, not (v `elem` vs)]

instance Alpha RWCTy where
  aeq' (RWCTyApp t1 t2) (RWCTyApp t1' t2')   = aeq' t1 t1' >> aeq' t2 t2'
  aeq' (RWCTyCon i) (RWCTyCon j) | i == j    = return ()
                                 | otherwise = mzero
  aeq' (RWCTyVar x) (RWCTyVar y)             = aeq' x y
  aeq' _ _                                   = mzero

instance FV RWCTy RWCTy where
  fv (RWCTyApp t1 t2) = fv t1 ++ fv t2
  fv (RWCTyVar x)     = [x]
  fv (RWCTyCon _)     = []

instance Subst RWCTy RWCTy where
  subst m (RWCTyApp t1 t2) = RWCTyApp (subst m t1) (subst m t2)
  subst m (RWCTyVar x)     = case Map.lookup x m of
                               Just t  -> t
                               Nothing -> RWCTyVar x
  subst m (RWCTyCon i)     = RWCTyCon i

instance Subst a b => Subst [a] b where
  subst m = map (subst m)

instance Subst RWCExp RWCTy where
  subst m (RWCApp e1 e2) = RWCApp (subst m e1) (subst m e2)
  subst m (RWCLam x t e) = RWCLam x (subst m t) (subst m e)
  subst m (RWCVar x t)   = RWCVar x (subst m t)
  subst m (RWCCon i t)   = RWCCon i (subst m t)
  subst m (RWCLiteral l) = RWCLiteral l
  subst m (RWCCase e as) = RWCCase (subst m e) (subst m as)

instance Subst RWCAlt RWCTy where
  subst m (RWCAlt p e) = RWCAlt (subst m p) (subst m e)

instance Subst RWCPat RWCTy where
  subst m (RWCPatCon i ps)  = RWCPatCon i (subst m ps)
  subst m (RWCPatLiteral l) = RWCPatLiteral l
  subst m (RWCPatVar x t)   = RWCPatVar x (subst m t)

--instance (NFData k,NFData v) => NFData (Map k v) where
--  rnf = rnf . Map.toList
{-# LANGUAGE GeneralizedNewtypeDeriving,MultiParamTypeClasses,
             FlexibleInstances,TupleSections,FunctionalDependencies,
             FlexibleContexts,ScopedTypeVariables #-}

module ReWire.Scoping where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Map.Strict (Map,insert,delete)
import qualified Data.Map.Strict as Map
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Control.DeepSeq
import Data.Either (rights)
--import Data.Maybe (fromJust,catMaybes,isNothing,isJust)
--import Unbound.LocallyNameless hiding (fv,subst,substs,Subst,Alpha,aeq)
--import qualified Unbound.LocallyNameless as U
--import Test.QuickCheck
--import Test.QuickCheck.Gen
--import Data.List (nub)
--import Debug.Trace (traceShow)

class (Ord v,Monad m) => MonadAssume v t m | m -> v, m -> t where
  assuming       :: v -> t -> m a -> m a
  forgetting     :: v -> m a -> m a
  query          :: v -> m (Maybe t)
  getAssumptions :: m (Map v t)

newtype AssumeT v t m a = AssumeT { deAssumeT :: ReaderT (Map v t) m a }
                           deriving (Monad,MonadTrans,MonadPlus)

instance (Ord v,NFData v,NFData t,Monad m) => MonadAssume v t (AssumeT v t m) where
  assuming n t m = AssumeT $ local (insert n t) (ask >>= \ s -> s `deepseq` deAssumeT m)
  forgetting n m = AssumeT $ local (delete n) (ask >>= \ s -> s `deepseq` deAssumeT m)
  query n        = AssumeT $ do { m <- ask ; return (Map.lookup n m) }
  getAssumptions = AssumeT ask

type Assume e t = AssumeT e t Identity

runAssumeTWith :: (Ord v,Foldable f) => f (v,t) -> AssumeT v t m a -> m a
runAssumeTWith as m = runReaderT (deAssumeT m) (Foldable.foldr (uncurry insert) Map.empty as)

runAssumeT :: Ord v => AssumeT v t m a -> m a
runAssumeT = runAssumeTWith []

runAssumeWith :: (Ord v,Foldable f) => f (v,t) -> Assume v t a -> a
runAssumeWith as = runIdentity . runAssumeTWith as

runAssume :: Ord v => Assume v t a -> a
runAssume = runAssumeWith []

newtype Id a = Id { deId :: String } deriving (Eq,Ord,Show,Read,NFData)

type Sort = String

class Sorted t where
  sort :: t -> Sort

instance Sorted t => Sorted (Id t) where
  sort _ = sort (undefined::t)

instance Sorted Int where
  sort _ = "Int"

class Subst t t' where
  fv     :: t -> [Id t']
  subst' :: t -> SubstM t' t

instance Subst t t' => Subst [t] t' where
  fv = concatMap fv  
  subst' = mapM subst'

subst :: Subst t t' => (Map (Id t') t') -> t -> t
subst s t = let as = Map.toList $ fmap Right s
            in  runAssumeWith as (subst' t)

type SubstM t' = Assume (Id t') (Either (Id t') t')
--            [(Id t',t')] -> t -> t

refresh :: (NFData t,Subst t t) => Id t -> [Id t] -> (Id t -> SubstM t a) -> SubstM t a
refresh x fvs_ k = do as      <- getAssumptions
                      let es  =  Map.elems as 
                          fvs =  fvs_ ++ concatMap fv (rights es)
                          ys  =  x : map (Id . (++"'") . deId) ys
                          x'  =  head (filter (not . (`elem` fvs)) ys)
                      if x==x'
                        then forgetting x (k x)
                        else assuming x (Left x') (k x')

refreshs :: (NFData t,Subst t t) => [Id t] -> [Id t] -> ([Id t] -> SubstM t a) -> SubstM t a
refreshs xs av k  = ref' (breaks xs) k
   where breaks (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (breaks xs)
         breaks []     = []  
         ref' ((x,av'):xs) k = refresh x (av++av') $ \ x' ->
                                  ref' xs (\ xs' -> k (x':xs'))
         ref' [] k           = k []

type IdSort = String
type AlphaM = Assume (Either (Sort,String) (Sort,String)) String

class Alpha t where
  aeq' :: t -> t -> AlphaM Bool

instance Alpha t => Alpha [t] where
  aeq' l1 l2 | length l1 /= length l2 = return False
             | otherwise              = liftM and (zipWithM aeq' l1 l2)

equating :: Sorted a => Id a -> Id a -> AlphaM b -> AlphaM b
equating x y m = assuming (Left (sort x,deId x)) (deId y) $
                  assuming (Right (sort y,deId y)) (deId x) $
                   m

aeq :: Alpha t => t -> t -> Bool
aeq x y = runAssume (aeq' x y)

infix 4 `aeq`

equatings :: Sorted a => [Id a] -> [Id a] -> AlphaM Bool -> AlphaM Bool
equatings xs ys m | length xs /= length ys = return False
                  | otherwise              = foldr (uncurry equating) m (zip xs ys)

varsaeq :: Sorted a => Id a -> Id a -> AlphaM Bool
varsaeq x y = do mx <- query (Left (sort x,deId x))
                 my <- query (Right (sort y,deId y))
                 case (mx,my) of
                   (Just x',Just y') -> return (x'==deId y && y'==deId x)
                   (Just x',Nothing) -> return False
                   (Nothing,Just y') -> return False
                   (Nothing,Nothing) -> return (x==y)

instance (NFData v,NFData t) => NFData (Map v t) where
  rnf = rnf . Map.toList






{-
--
-- Begin grungy example/test case. (Uses QuickCheck to verify that the
-- results agree with unbound...)
--
data Lam = Lam [Id Lam] Lam | Var (Id Lam) | App Lam Lam deriving Eq

instance Show Lam where
  show (Lam x l)   = "(\\ " ++ concatMap ((" "++) . deId) x ++ " . " ++ show l ++ ")"
  show (Var x)     = deId x
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

instance Subst Lam Lam where
  fv (Var x)     = [x]
  fv (Lam x e)   = [y | y <- fv e, not (y `elem` x)]
  fv (App e1 e2) = fv e1 ++ fv e2
  mkvar = Var
  substs xes e = runAssumeWith xes (doSubst e)
    where doSubst (Var x)     = do ml <- query x
                                   case ml of
                                     Just l  -> return l
                                     Nothing -> return (Var x)
          doSubst (App e1 e2) = do e1' <- doSubst e1
                                   e2' <- doSubst e2
                                   return (App e1' e2')
          doSubst (Lam x e)   = refreshs x (fv e) $ \ x' -> do
                                   e' <- doSubst e
                                   return (Lam x' e')

instance Alpha Lam where
  aeq l1 l2 = runAssume (aeq' l1 l2)
    where aeq' (Var x) (Var y)           = varsaeq x y 
          aeq' (Lam xs e) (Lam ys e')    = equatings xs ys $ aeq' e e'
          aeq' (App e1 e2) (App e1' e2') = liftM2 (&&) (aeq' e1 e1') (aeq' e2 e2')
          aeq' _ _                       = return False

data LamU = LamU (Bind [Name LamU] LamU) | VarU (Name LamU) | AppU LamU LamU deriving Show

$(derive [''LamU])

instance U.Alpha LamU
  
instance U.Subst LamU LamU where
  isvar (VarU n) = Just (SubstName n)
  isvar _        = Nothing

lam2lamu :: Lam -> LamU
lam2lamu (Var x)     = VarU (s2n (deId x))
lam2lamu (App e1 e2) = AppU (lam2lamu e1) (lam2lamu e2)
lam2lamu (Lam x e)   = LamU (bind (map (s2n . deId) x) (lam2lamu e))

checkeqv :: [(Id Lam,Lam)] -> Lam -> Bool
checkeqv xes e' = let eMine   = lam2lamu $ substs xes e'
                      xes_u   = map (\ (x,e) -> (s2n (deId x),lam2lamu e)) xes
                      e'_u    = lam2lamu e'
                      eTheirs = U.substs xes_u e'_u
                  in  eMine `U.aeq` eTheirs
                      
--test2 :: Lam -> Lam -> Bool
test2 l1 l2 = l1 `aeq` l2 && l1 /= l2 ==> label (show l1 ++ " --- " ++ show l2) $ (lam2lamu l1 `U.aeq` lam2lamu l2) == (l1 `aeq` l2)

newtype GenSu = GenSu [(Id Lam,Lam)] deriving Show

instance Arbitrary GenSu where
  arbitrary = liftM GenSu $ resize 3 $ listOf1 $ liftM2 (,) genvar arbitrary
  
genvar :: Gen (Id Lam)
genvar = liftM Id $ elements [v ++ suf | v <- vars, suf <- sufs]
  where vars = ["x","y","z"]
        sufs = [""]--,"'","''","'''"]

instance Arbitrary Lam where
  arbitrary = frequency [(7,liftM2 Lam (liftM nub $ resize 5 $ listOf1 $ genvar) arbitrary), -- have to nub here because unbound treats leftmost duplicate binders in a list as being tighter (which I think is probably not right)
                         (7,liftM Var genvar),
                         (6,liftM2 App arbitrary arbitrary)]

qc = \ (GenSu xes) -> checkeqv xes
--
-- End grungy example/test case.
--
-}

{-# LANGUAGE GeneralizedNewtypeDeriving,MultiParamTypeClasses,FlexibleInstances,TupleSections,FunctionalDependencies,TemplateHaskell,FlexibleContexts,UndecidableInstances #-}

module AssumeT where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Map.Strict (Map,insert,delete)
import qualified Data.Map.Strict as Map
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Maybe (fromJust,catMaybes,isNothing)
import Unbound.LocallyNameless hiding (fv,subst)
import qualified Unbound.LocallyNameless as U
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.List (nub)

class (Ord v,Monad m) => MonadAssume v t m | m -> v, m -> t where
  assume     :: v -> t -> m a -> m a
  forget     :: v -> m a -> m a
  query      :: v -> m (Maybe t)
  getAssumps :: m (Map v t)

instance (Ord v,Monad m) => MonadAssume v t (AssumeT v t m) where
  assume v t m = AssumeT $ local (insert v t) (deAssumeT m)
  forget v m   = AssumeT $ local (delete v) (deAssumeT m)
  query v      = AssumeT $ do { m <- ask ; return (Map.lookup v m) }
  getAssumps   = AssumeT ask

assumptions :: (Foldable f,MonadAssume v t m) => f (v,t) -> m a -> m a
assumptions as m = Foldable.foldr (uncurry assume) m as

forgets :: (Foldable f,MonadAssume v t m) => f v -> m a -> m a
forgets as m = Foldable.foldr forget m as

unsafeQuery :: MonadAssume v t m => v -> m t
unsafeQuery = liftM fromJust . query

addscope v = assume v ()
inscope v  = do mt <- query v
                case mt of
                  Just _  -> return True
                  Nothing -> return False

newtype AssumeT v t m a = AssumeT { deAssumeT :: ReaderT (Map v t) m a }
                           deriving (Monad,MonadTrans,MonadPlus)
type Assume v t = AssumeT v t Identity
type ScopeT v = AssumeT v ()
type Scope v = Assume v ()

runAssumeTWith :: (Foldable f,Ord v) => f (v,t) -> AssumeT v t m a -> m a
runAssumeTWith as m = runReaderT (deAssumeT m) (Foldable.foldr (uncurry insert) Map.empty as)

runAssumeT :: Ord v => AssumeT v t m a -> m a
runAssumeT = runAssumeTWith []

runAssumeWith :: (Foldable f,Ord v) => f (v,t) -> Assume v t a -> a
runAssumeWith as = runIdentity . runAssumeTWith as

runAssume :: Ord v => Assume v t a -> a
runAssume = runAssumeWith []

runScopeTWith :: (Foldable f,Ord v) => f v -> ScopeT v m a -> m a
runScopeTWith as m = runReaderT (deAssumeT m) (Foldable.foldr (uncurry insert . (,())) Map.empty as)

runScopeT :: Ord v => ScopeT v m a -> m a
runScopeT = runScopeTWith []

runScopeWith :: (Foldable f,Ord v) => f v -> Scope v a -> a
runScopeWith as = runIdentity . runScopeTWith as

runScope :: Ord v => Scope v  a -> a
runScope = runScopeWith []

data Lam = Lam [String] Lam | Var String | App Lam Lam deriving Show

--instance Show Lam where
--  show (Lam x l)   = "(\\ " ++ x ++ " . " ++ show l ++ ")"
--  show (Var x)     = x
--  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

fv :: Lam -> [String]
fv (Var x)     = [x]
fv (Lam x e)   = [y | y <- fv e, not (y `elem` x)]
fv (App e1 e2) = fv e1 ++ fv e2

type Id x = String

-- refresh :: FV e e => Id e -> [Id e] -> (Id e -> Assume (Id e) e a) -> Assume (Id e) e a
refresh :: String -> [String] -> (String -> Assume String Lam a) -> Assume String Lam a
refresh x fvs_ k = do as      <- getAssumps
                      let es  =  Map.elems as 
                          fvs =  fvs_ ++ concatMap fv es -- ++ Map.keys as
                          ys  =  x : map (++"'") ys
                          x'  =  head (filter (not . (`elem` fvs)) ys)
                      if x==x'
                        then forget x (k x)
                        else assume x (Var x') (k x')

refreshs :: [String] -> [String] -> ([String] -> Assume String Lam a) -> Assume String Lam a
refreshs xs av k  = ref' (breaks xs) k
   where breaks (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (breaks xs)
         breaks []     = []  
         ref' ((x,av'):xs) k = refresh x (av++av') $ \ x' ->
                                  ref' xs (\ xs' -> k (x':xs'))
         ref' [] k           = k []

subst :: Lam -> Assume String Lam Lam
subst (Var x)     = do ml <- query x
                       case ml of
                         Just l  -> return l
                         Nothing -> return (Var x)
subst (App e1 e2) = do e1' <- subst e1
                       e2' <- subst e2
                       return (App e1' e2')
subst (Lam x e)   = refreshs x (fv e) $ \ x' -> do
                       e' <- subst e
                       return (Lam x' e')

doSubst :: [(String,Lam)] -> Lam -> Lam
doSubst xes e' = runAssumeWith xes (subst e')

data LamU = LamU (Bind [Name LamU] LamU) | VarU (Name LamU) | AppU LamU LamU deriving Show

$(derive [''LamU])

instance Alpha LamU
  
instance Subst LamU LamU where
  isvar (VarU n) = Just (SubstName n)
  isvar _        = Nothing

lam2lamu :: Lam -> LamU
lam2lamu (Var x)     = VarU (s2n x)
lam2lamu (App e1 e2) = AppU (lam2lamu e1) (lam2lamu e2)
lam2lamu (Lam x e)   = LamU (bind (map s2n x) (lam2lamu e))

checkeqv :: [(String,Lam)] -> Lam -> Bool
checkeqv xes e' = let eMine   = lam2lamu $ doSubst xes e'
                      xes_u   = map (\ (x,e) -> (s2n x,lam2lamu e)) xes
                      e'_u    = lam2lamu e'
                      eTheirs = U.substs xes_u e'_u
                  in  eMine `aeq` eTheirs

newtype GenSu = GenSu [(String,Lam)] deriving Show

instance Arbitrary GenSu where
  arbitrary = liftM GenSu $ resize 30 $ listOf1 $ liftM2 (,) genvar arbitrary
  
genvar :: Gen String
genvar = elements [v ++ suf | v <- vars, suf <- sufs]
  where vars = ["x","y","z"]
        sufs = ["","'","''","'''"]

instance Arbitrary Lam where
  arbitrary = frequency [(7,liftM2 Lam (liftM nub $ resize 5 $ listOf1 $ genvar) arbitrary),
                         (7,liftM Var genvar),
                         (6,liftM2 App arbitrary arbitrary)]

qc = \ (GenSu xes) -> checkeqv xes

grux :: [a] -> [(a,[a])]
grux (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (grux xs)
grux []     = []
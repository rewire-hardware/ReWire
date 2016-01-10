{-# LANGUAGE GeneralizedNewtypeDeriving,MultiParamTypeClasses,
             FlexibleInstances,TupleSections,FunctionalDependencies,
             FlexibleContexts,ScopedTypeVariables,GADTs,StandaloneDeriving,
             UndecidableInstances,DeriveDataTypeable
  #-}

module ReWire.Scoping where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.Data (Typeable,Data)
import Data.Map.Strict (Map,insert,delete)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
--import Data.Foldable (Foldable)
--import qualified Data.Foldable as Foldable
import Control.DeepSeq
import Data.Either (rights)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromJust,isJust)
{-
import Unbound.LocallyNameless hiding (fv,subst,substs,Subst,Alpha,aeq,aeq')
import qualified Unbound.LocallyNameless as U
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.List (nub)
-}

--
-- A monad for assumptions.
--
class (Ord v,Monad m) => MonadAssume v t m | m -> v, m -> t where
  assuming       :: v -> t -> m a -> m a
  forgetting     :: v -> m a -> m a
  query          :: v -> m (Maybe t)
  getAssumptions :: m (Map v t)

newtype AssumeT v t m a = AssumeT { deAssumeT :: ReaderT (Map v t) m a }
                           deriving (Functor,Applicative,Alternative,Monad,
                                     MonadTrans,MonadPlus,MonadScope)

deriving instance MonadState s m => MonadState s (AssumeT v t m)
deriving instance MonadError e m => MonadError e (AssumeT v t m)

instance MonadReader r m => MonadReader r (AssumeT v t m) where
  ask       = lift ask
  local f m = AssumeT $ ReaderT $ \ rhoA -> local f (runReaderT (deAssumeT m) rhoA)

instance (Ord v,Monad m) => MonadAssume v t (AssumeT v t m) where
  assuming n t m = AssumeT $ local (insert n t) (deAssumeT m)
  forgetting n m = AssumeT $ local (delete n) (deAssumeT m)
  query n        = AssumeT $ do { m <- ask ; return (Map.lookup n m) }
  getAssumptions = AssumeT ask

type Assume e t = AssumeT e t Identity

runAssumeTWith :: Ord v => Map v t -> AssumeT v t m a -> m a
runAssumeTWith as m = runReaderT (deAssumeT m) as

runAssumeT :: Ord v => AssumeT v t m a -> m a
runAssumeT = runAssumeTWith Map.empty

runAssumeWith :: Ord v => Map v t -> Assume v t a -> a
runAssumeWith as = runIdentity . runAssumeTWith as

runAssume :: Ord v => Assume v t a -> a
runAssume = runAssumeWith Map.empty

---
--- Identifiers.
---

-- NB: The "Id" constructor should be hidden
data Id a = Id {-# UNPACK #-} !ByteString {-# UNPACK #-} !ByteString
            deriving (Eq,Ord,Typeable,Data)

instance Show (Id a) where
  show (Id _ x) = BS.unpack x

mkId :: forall a . IdSort a => String -> Id a
mkId x = Id (idSort (undefined::a)) (BS.pack x)

deId :: Id a -> String
deId (Id _ i) = BS.unpack i

instance NFData (Id a) where
  rnf (Id s i) = s `seq` i `seq` ()

data IdAny where
  IdAny :: forall a . !(Id a) -> IdAny

instance Eq IdAny where
  IdAny (Id _ i1) == IdAny (Id _ i2) = i1==i2

instance Ord IdAny where
  compare (IdAny (Id _ i1)) (IdAny (Id _ i2)) = compare i1 i2

class IdSort t where
  idSort :: t -> ByteString

instance IdSort t => IdSort (Id t) where
  idSort _ = idSort (undefined::t)

any2Id :: forall a . IdSort a => IdAny -> Maybe (Id a)
any2Id (IdAny (Id s i)) | s == idSort (undefined :: a) = Just (Id s i)
                        | otherwise                    = Nothing

sortOf :: IdAny -> ByteString
sortOf (IdAny (Id s _)) = s

---
--- A monad for generating locally fresh names.
---
newtype ScopeT m a = ScopeT { deScopeT :: ReaderT (Set IdAny) m a }
   deriving (Functor,Applicative,Alternative,Monad,MonadTrans,MonadPlus)

deriving instance MonadState s m => MonadState s (ScopeT m)
deriving instance MonadError e m => MonadError e (ScopeT m)

instance MonadReader r m => MonadReader r (ScopeT m) where
  ask       = lift ask
  local f m = ScopeT $ ReaderT $ \ rhoS -> local f (runReaderT (deScopeT m) rhoS)

class Monad m => MonadScope m where
  getInScope    :: m (Set IdAny)
  addingToScope :: Id a -> m b -> m b

instance Monad m => MonadScope (ScopeT m) where
  getInScope        = ScopeT ask
  addingToScope x m = ScopeT $ local (Set.insert (IdAny x)) (deScopeT m)

instance MonadScope m => MonadScope (ReaderT r m) where
  getInScope        = lift getInScope
  addingToScope x m = ReaderT $ \ rho -> addingToScope x (runReaderT m rho)

getInScopeSorted :: forall m t . (MonadScope m,IdSort t) => m (Set (Id t))
getInScopeSorted = do s <- getInScope
                      let s' :: Set (Id t)
                          s' = Set.mapMonotonic fromJust $
                                 Set.filter isJust $
                                   Set.mapMonotonic any2Id s
                      return s'

refreshingVar :: forall m t t' a . (Subst t t',MonadScope m) => Id t' -> t -> (Id t' -> t -> m a) -> m a
refreshingVar x e k = do insc <- getInScopeSorted :: m (Set (Id t'))
                         if x `Set.member` insc || x `elem` fv e
                            then let ov =  occv e
                                     ys =  x : map (mkId . (++"'") . deId) ys
                                     x' =  head (filter (\ y -> not (y `Set.member` insc || y `elem` ov)) ys)
                                 in if x /= x'
                                      then addingToScope x' (k x' (replace (Map.singleton x x') e))
                                      else k x e
                            else k x e

runScopeTWith :: Set IdAny -> ScopeT m a -> m a
runScopeTWith s m = runReaderT (deScopeT m) s

---
---
---

type SubstM t' = Assume (Id t') (Either (Id t') t')

class IdSort t' => Subst t t' where
  fv     :: t -> [Id t']
  bv     :: t -> [Id t']
  occv   :: t -> [Id t']
  occv e = fv e ++ bv e
  subst' :: t -> SubstM t' t

instance Subst t t' => Subst [t] t' where
  fv = concatMap fv
  bv = concatMap bv
  subst' = mapM subst'

subst :: Subst t t' => Map (Id t') t' -> t -> t
subst s t = runAssumeWith (fmap Right s) (subst' t)

replace :: Subst t t' => Map (Id t') (Id t') -> t -> t
replace s t = runAssumeWith (fmap Left s) (subst' t)

refresh ::
  Subst t t =>
  Id t -> [Id t] -> (Id t -> SubstM t a) -> SubstM t a
refresh x av_ k = do as      <- getAssumptions
                     let es =  Map.elems as
                         av =  av_ ++ concatMap fv (rights es)
                         ys =  x : map (mkId . (++"'") . deId) ys
                         x' =  head (filter (not . (`elem` av)) ys)
                     if x==x'
                       then forgetting x (k x)
                       else assuming x (Left x') (k x')

refreshs ::
   Subst t t =>
   [Id t] -> [Id t] -> ([Id t] -> SubstM t a) -> SubstM t a
refreshs xs av k  = ref' (breaks xs) k
   where breaks (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (breaks xs)
         breaks []     = []
         ref' ((x,av'):xs) k = refresh x (av++av') $ \ x' ->
                                  ref' xs (\ xs' -> k (x':xs'))
         ref' [] k           = k []

type AlphaM = Assume (Either IdAny IdAny) IdAny

class Alpha t where
  aeq' :: t -> t -> AlphaM Bool

instance Alpha t => Alpha [t] where
  aeq' l1 l2 | length l1 /= length l2 = return False
             | otherwise              = liftM and (zipWithM aeq' l1 l2)

equating :: IdSort a => Id a -> Id a -> AlphaM b -> AlphaM b
equating x y m = assuming (Left (IdAny x)) (IdAny y) $
                  assuming (Right (IdAny y)) (IdAny x) $
                   m

aeq :: Alpha t => t -> t -> Bool
aeq x y = runAssume (aeq' x y)

infix 4 `aeq`

equatings :: IdSort a => [Id a] -> [Id a] -> AlphaM b -> AlphaM b -> AlphaM b
equatings xs ys mfail m | length xs /= length ys = mfail
                        | otherwise              = foldr (uncurry equating) m (zip xs ys)

varsaeq :: IdSort a => Id a -> Id a -> AlphaM Bool
varsaeq x y = do mx <- query (Left (IdAny x))
                 my <- query (Right (IdAny y))
                 case (mx,my) of
                   (Just x',Just y') -> return (x'==IdAny y && y'==IdAny x)
                   (Just _,Nothing)  -> return False
                   (Nothing,Just _)  -> return False
                   (Nothing,Nothing) -> return (x==y)






{-
--
-- Begin grungy example/test case. (Uses QuickCheck to verify that the
-- results agree with unbound...)
--
data Lam = Lam [Id Lam] Lam | Var (Id Lam) | App Lam Lam deriving Eq

instance IdSort Lam where
  idSort _ = BS.pack ""

instance Show Lam where
  show (Lam x l)   = "(\\ " ++ concatMap ((" "++) . deId) x ++ " . " ++ show l ++ ")"
  show (Var x)     = deId x
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

instance Subst Lam Lam where
  fv (Var x)     = [x]
  fv (Lam x e)   = [y | y <- fv e, not (y `elem` x)]
  fv (App e1 e2) = fv e1 ++ fv e2
  subst' = doSubst
    where doSubst (Var x)     = do ml <- query x
                                   case ml of
                                     Just (Left y)  -> return (Var y)
                                     Just (Right l) -> return l
                                     Nothing        -> return (Var x)
          doSubst (App e1 e2) = do e1' <- doSubst e1
                                   e2' <- doSubst e2
                                   return (App e1' e2')
          doSubst (Lam x e)   = refreshs x (fv e) $ \ x' -> do
                                   e' <- doSubst e
                                   return (Lam x' e')

instance Alpha Lam where
  aeq' (Var x) (Var y)           = varsaeq x y
  aeq' (Lam xs e) (Lam ys e')    = equatings xs ys (return False) (aeq' e e')
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
checkeqv xes e' = let eMine   = lam2lamu $ subst (Map.fromList xes) e'
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
genvar = liftM mkId $ elements [v ++ suf | v <- vars, suf <- sufs]
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

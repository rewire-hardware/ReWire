{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances,
             TupleSections, FunctionalDependencies, FlexibleContexts, ScopedTypeVariables,
             GADTs, StandaloneDeriving, UndecidableInstances, OverlappingInstances,
             DeriveDataTypeable #-}

module ReWire.Scoping
  ( Id(..),IdAny(..),IdSort(..),mkId,any2Id
  , Alpha(..),AlphaM
  , MonadAssume(..),AssumeT,runAssume,runAssumeT,runAssumeTWith
  , ScopeT,runScopeTWith
  , Subst(..),subst
  , aeq,varsaeq
  , equating,equatings
  , replace
  , refresh,refreshs
  , deId
  ) where

import ReWire.Pretty

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import Data.Data (Typeable,Data)
import Data.Either (rights)
import Data.Map.Strict (Map,insert,delete)
import Data.Set (Set)
import Text.PrettyPrint (text)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as Map
import qualified Data.Set              as Set

--
-- A monad for assumptions.
--
class MonadAssume v t m | t -> v, m v -> t where
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

instance (Ord v, Monad m) => MonadAssume v t (AssumeT v t m) where
  assuming n t m = AssumeT $ local (insert n t) (deAssumeT m)
  forgetting n m = AssumeT $ local (delete n) (deAssumeT m)
  query n        = AssumeT $ do { m <- ask ; return (Map.lookup n m) }
  getAssumptions = AssumeT ask

instance (Ord v', Monad m, MonadAssume v t m) => MonadAssume v t (AssumeT v' t' m) where
  assuming n t m = AssumeT $ ReaderT $ \x -> assuming n t (runAssumeTWith x m)
  forgetting n m = AssumeT $ ReaderT $ \x -> forgetting n (runAssumeTWith x m)
  query n        = lift $ query n
  getAssumptions = lift $ getAssumptions

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

instance Pretty (Id a) where
  pretty = text . deId

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

runScopeTWith :: Set IdAny -> ScopeT m a -> m a
runScopeTWith s m = runReaderT (deScopeT m) s

---
---
---

type SubstM t' = Assume (Id t') (Either (Id t') t')
type Assumps t = SubstM t (Map (Id t) (Either (Id t) t))

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
refresh x av_ k = do as     <- getAssumptions :: Assumps t
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

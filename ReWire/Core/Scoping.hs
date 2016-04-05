{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances,
             TupleSections, FunctionalDependencies, FlexibleContexts, ScopedTypeVariables,
             GADTs, StandaloneDeriving, UndecidableInstances, DeriveDataTypeable #-}

module ReWire.Core.Scoping (MonadAssume(..),AssumeT,runAssume,runAssumeT,runAssumeTWith) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Trans (lift,MonadIO,MonadTrans)
import Control.Monad.Catch (MonadCatch,MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Reader (MonadReader(..),ReaderT(..))
import Control.Monad.State (MonadState(..),StateT(..))
import Data.Map.Strict (Map,insert,delete)

import qualified Data.Map.Strict       as Map

--
-- A monad for assumptions.
--
class (Ord v, Monad m) => MonadAssume v t m | m v -> t, m t -> v where
  assuming       :: v -> t -> m a -> m a
  forgetting     :: v -> m a -> m a
  query          :: v -> m (Maybe t)
  getAssumptions :: m (Map v t)

newtype AssumeT v t m a = AssumeT { deAssumeT :: ReaderT (Map v t) m a }
                           deriving (Functor,Applicative,Alternative,Monad,
                                     MonadTrans,MonadPlus)

deriving instance MonadState s m => MonadState s (AssumeT v t m)
deriving instance MonadError e m => MonadError e (AssumeT v t m)
deriving instance MonadCatch m   => MonadCatch (AssumeT v t m)
deriving instance MonadThrow m   => MonadThrow (AssumeT v t m)
deriving instance MonadIO m      => MonadIO (AssumeT v t m)

instance MonadReader r m => MonadReader r (AssumeT v t m) where
  ask       = lift ask
  local f m = AssumeT $ ReaderT $ \ rhoA -> local f (runReaderT (deAssumeT m) rhoA)

instance {-# OVERLAPPING #-} (Ord v, Monad m) => MonadAssume v t (AssumeT v t m) where
  assuming n t m = AssumeT $ local (insert n t) (deAssumeT m)
  forgetting n m = AssumeT $ local (delete n) (deAssumeT m)
  query n        = AssumeT $ do { m <- ask ; return (Map.lookup n m) }
  getAssumptions = AssumeT ask

instance {-# OVERLAPPING #-} (Ord v', Monad m, MonadAssume v t m) => MonadAssume v t (AssumeT v' t' m) where
  assuming n t m = AssumeT $ ReaderT $ \x -> assuming n t (runAssumeTWith x m)
  forgetting n m = AssumeT $ ReaderT $ \x -> forgetting n (runAssumeTWith x m)
  query n        = lift $ query n
  getAssumptions = lift $ getAssumptions

instance {-# OVERLAPPING #-} (Monad m, MonadAssume v t m) => MonadAssume v t (StateT s m) where
  assuming n t m = StateT $ \s -> do
      (a, s') <- runStateT m s
      a'      <- assuming n t (return a)
      return (a', s')
  forgetting n m = StateT $ \s -> do
      (a, s') <- runStateT m s
      a'      <- forgetting n (return a)
      return (a', s')
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


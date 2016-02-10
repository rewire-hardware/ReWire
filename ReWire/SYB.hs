{-# LANGUAGE RankNTypes, GADTs, LambdaCase #-}
module ReWire.SYB
      ( Transform (TId), Query (QEmpty)
      , (||>)
      , (||?), (|?)
      , runT, runPureT
      , runQ, runPureQ
      ) where

import Control.Exception (PatternMatchFail (..))
import Control.Monad.Catch (MonadCatch (..))
import Control.Monad (liftM, (>=>), MonadPlus (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Data (Data, Typeable, gmapM, gmapQr, cast)
import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Data.Monoid (Monoid (..))

everywhere :: (Monad m, Data a) => (forall d. Data d => d -> m d) -> a -> m a
everywhere f = gmapM (everywhere f) >=> f

everywhereQ :: (MonadPlus m, Data a) => (forall d. Data d => d -> m b) -> a -> m b
everywhereQ f n = f n `mplus` gmapQr mplus mzero (everywhereQ f) n

generalizeA :: (Monad m, Typeable a) => (a -> m b) -> forall d. Typeable d => d -> MaybeT m b
generalizeA f x = case f <$> cast x of
      Nothing -> mzero
      Just x' -> lift x'

generalize :: (Monad m, Typeable a) => (a -> m a) -> forall d. Typeable d => d -> MaybeT m d
generalize f = generalizeA f >=> tr
      where tr :: (Monad m, Typeable a, Typeable b) => a -> m b
            tr = return . fromJust . cast

-- | This is just a list of type
-- > [exists d. Data d => d -> MaybeT m d]
data Transform m where
      TCons :: (forall d. Data d => d -> MaybeT m d) -> Transform m -> Transform m
      TId   :: Transform m

type T m = forall d. Data d => d -> MaybeT m d

instance Monoid (Transform m) where
      mempty                 = TId
      mappend (TCons f fs) g = TCons f $ mappend fs g
      mappend TId g          = g

foldT :: Monad m => (T m -> T m -> T m) -> Transform m -> T m
foldT op (TCons f fs) = f `op` foldT op fs
foldT _ TId           = lift . return

(||>) :: (Monad m, Typeable d) => (d -> m d) -> Transform m -> Transform m
f ||> fs = generalize f `TCons` fs
infixr 1 ||>

runT :: (Functor m, MonadCatch m, Data d) => Transform m -> d -> m d
runT t = everywhere $ liftM fromJust . runMaybeT . foldT (\ f g x -> f x `mplusE` g x) t

runPureT :: (Functor m, Monad m, Data d) => Transform m -> d -> m d
runPureT t = everywhere $ liftM fromJust . runMaybeT . foldT (\ f g x -> f x `mplus` g x) t

-- | This is just a list of type
-- > [exists d. Data d => d -> MaybeT m a]
data Query m a where
      QCons  :: (forall d. Data d => d -> MaybeT m a) -> Query m a -> Query m a
      QEmpty :: Query m a

type Q m a = forall d. Data d => d -> MaybeT m a

instance Monoid (Query m a) where
      mempty                 = QEmpty
      mappend (QCons f fs) g = QCons f $ mappend fs g
      mappend QEmpty g       = g

(|?) :: (Monad m, Typeable d) => (d -> a) -> Query m a -> Query m a
f |? fs = QCons (generalizeA $ return . f) fs
infixr 1 |?

(||?) :: (Monad m, Typeable d) => (d -> m a) -> Query m a -> Query m a
f ||? fs = QCons (generalizeA f) fs
infixr 1 ||?

foldQ :: MonadPlus m => (Q m a -> Q m a -> Q m a) -> Query m a -> Q m a
foldQ op (QCons f fs) = f `op` foldQ op fs
foldQ _ QEmpty        = lift . const mzero

runQ :: (Functor m, MonadPlus m, MonadCatch m, Data d) => Query m a -> d -> m a
runQ q = everywhereQ $ liftM fromJust . runMaybeT . foldQ (\ f g x -> f x `mplusE` g x) q

runPureQ :: (Functor m, MonadPlus m, Data d) => Query m a -> d -> m a
runPureQ q = everywhereQ $ liftM fromJust . runMaybeT . foldQ (\ f g x -> f x `mplus` g x) q

-- | mplus + match fail treated as mzero.
mplusE :: (MonadCatch m, MonadPlus m) => m a -> m a -> m a
mplusE a b = catch a (\ (PatternMatchFail _) -> mzero) `mplus` b

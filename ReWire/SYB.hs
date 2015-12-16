{-# LANGUAGE Rank2Types, GADTs #-}
module ReWire.SYB
      ( Transform
      , Query
      , runT
      , runQ
      , match
      , match'
      , query
      , query'
      ) where

import Control.Applicative ((<*>))
import Control.Exception (PatternMatchFail(..))
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad ((>=>), msum, MonadPlus(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Data (Data, Typeable, gmapM, gmapQr, cast)
import Data.Functor ((<$>))
import Data.Functor.Identity
import Data.Maybe (fromJust)
import Data.Monoid (Monoid(..))

everywhere :: (Monad m, Data a) => (forall d. Data d => (d -> m d)) -> a -> m a
everywhere f = gmapM (everywhere f) >=> f

everywhereQ :: (MonadPlus m, Data a) => (forall d. Data d => d -> m b) -> a -> m b
everywhereQ f n = f n `mplus` gmapQr mplus mzero (everywhereQ f) n

generalizeA :: (Monad m, Typeable a) => (a -> m b) -> forall a. Typeable a => a -> MaybeT m b
generalizeA f x = case f <$> cast x of
      Nothing -> mzero
      Just x' -> lift x'

generalize :: (Monad m, Typeable a) => (a -> m a) -> forall a. Typeable a => a -> MaybeT m a
generalize f = generalizeA f >=> tr

data Transform m where
      TEmpty     :: Transform m
      Transform  :: (Functor m, MonadCatch m) => (forall d. Data d => d -> MaybeT m d) -> Transform m
      Transform' :: (Functor m, Monad m)      => (forall d. Data d => d -> MaybeT m d) -> Transform m

instance Monoid (Transform m) where
      mempty                                = TEmpty
      mappend TEmpty         a              = a
      mappend a              TEmpty         = a
      mappend (Transform f)  (Transform g)  = Transform  (f <+> g)
      mappend (Transform f)  (Transform' g) = Transform  (f <+> g)
      mappend (Transform' f) (Transform g)  = Transform  (f <+> g)
      mappend (Transform' f) (Transform' g) = Transform' (f |+| g)

data Query m a where
      QEmpty  :: Query m a
      Query   :: (Functor m, MonadPlus m, MonadCatch m) => (forall d. Data d => d -> MaybeT m a) -> Query m a
      Query'  :: (Functor m, MonadPlus m)               => (forall d. Data d => d -> MaybeT m a) -> Query m a

instance MonadPlus m => Monoid (Query m a) where
      mempty                        = QEmpty
      mappend QEmpty     a          = a
      mappend a          QEmpty     = a
      mappend (Query f)  (Query g)  = Query  (f <++> g)
      mappend (Query f)  (Query' g) = Query  (f <++> g)
      mappend (Query' f) (Query g)  = Query  (f <++> g)
      mappend (Query' f) (Query' g) = Query' (f |++| g)

match :: (Functor m, MonadCatch m, Data a) => (a -> m a) -> Transform m
match f = Transform $ generalize f

-- | Pure version.
match' :: (Functor m, Monad m, Data a) => (a -> m a) -> Transform m
match' f = Transform' $ generalize f

query :: (Functor m, MonadPlus m, MonadCatch m, Data a) => (a -> b) -> Query m b
query f = Query $ generalizeA (return . f)

-- | Pure version.
query' :: (Functor m, MonadPlus m, Data a) => (a -> m b) -> Query m b
query' f = Query' $ generalizeA f

runT :: (Monad m, Data d) => Transform m -> d -> m d
runT TEmpty         = return
runT (Transform  f) = everywhere $ \n -> fromJust <$> runMaybeT (f n `mplusE` tr n)
runT (Transform' f) = everywhere $ \n -> fromJust <$> runMaybeT (f n `mplus`  tr n)

runQ :: (MonadPlus m, Data d) => Query m a -> d -> m a
runQ QEmpty     = const mzero
runQ (Query  f) = everywhereQ $ \n -> fromJust <$> runMaybeT (f n `mplusE` lift mzero)
runQ (Query' f) = everywhereQ $ \n -> fromJust <$> runMaybeT (f n `mplus`  lift mzero)

(<+>) :: (MonadPlus m, MonadCatch m)
      => (forall d. Data d => d -> m d)
      -> (forall d. Data d => d -> m d)
      ->  forall d. Data d => d -> m d
f <+> g = \x -> f (fromJust $ cast x) `mplusE` g (fromJust $ cast x)

(<++>) :: (MonadPlus m, MonadCatch m)
      => (forall d. Data d => d -> m a)
      -> (forall d. Data d => d -> m a)
      ->  forall d. Data d => d -> m a
f <++> g = \x -> f x `mplusE` g x

(|+|) :: MonadPlus m
      => (forall d. Data d => d -> m d)
      -> (forall d. Data d => d -> m d)
      ->  forall d. Data d => d -> m d
f |+| g = \x -> f (fromJust $ cast x) `mplus` g (fromJust $ cast x)

(|++|) :: MonadPlus m
      => (forall d. Data d => d -> m a)
      -> (forall d. Data d => d -> m a)
      ->  forall d. Data d => d -> m a
f |++| g = \x -> f x `mplus` g x

-- | mplus + match fail treated as mzero.
mplusE :: (MonadCatch m, MonadPlus m) => m a -> m a -> m a
mplusE a b = catch a (\(PatternMatchFail _) -> mzero) `mplus` b

tr :: (Monad m, Typeable a, Typeable b) => a -> m b
tr = return . fromJust . cast

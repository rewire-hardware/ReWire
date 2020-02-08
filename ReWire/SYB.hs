{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE Safe #-}
module ReWire.SYB
      ( Transform (TId)
      , transform, (||>), runT, runPureT
      , Query (QEmpty)
      , query, (||?), runQ
      ) where

import Control.Exception (PatternMatchFail (..))
import Control.Monad.Catch (MonadCatch (..))
import Control.Monad ((>=>), MonadPlus (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Data (Data, Typeable, gmapM, gmapQr, cast)
import Data.Maybe (fromJust)

everywhere :: (Monad m, Data a) => (forall d. Data d => d -> m d) -> a -> m a
everywhere f = gmapM (everywhere f) >=> f

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

instance Semigroup (Transform m) where
      (TCons f fs) <> g = f `TCons` (fs <> g)
      TId          <> g = g

instance Monoid (Transform m) where
      mempty                 = TId

foldT :: Monad m => (T m -> T m -> T m) -> Transform m -> T m
foldT op (TCons f fs) = f `op` foldT op fs
foldT _ TId           = lift . return

(||>) :: (Monad m, Typeable d) => (d -> m d) -> Transform m -> Transform m
f ||> fs = generalize f `TCons` fs
infixr 1 ||>

transform :: (Monad m, Typeable d) => (d -> m d) -> Transform m
transform = (||> TId)

runPureT :: (Monad m, Data d) => Transform m -> d -> m d
runPureT t = everywhere $ fmap fromJust . runMaybeT . foldT (\ f g x -> f x `mplus` g x) t

runT :: (MonadCatch m, Data d) => Transform m -> d -> m d
runT t = everywhere $ fmap fromJust . runMaybeT . foldT (\ f g x -> f x `mplusE` g x) t

-- | mplus + match fail treated as mzero.
mplusE :: (MonadCatch m, MonadPlus m) => m a -> m a -> m a
mplusE a b = catch a (\ (PatternMatchFail _) -> mzero) `mplus` b

-- | This is just a list of type
-- > [exists d. Data d => d -> a]
data Query a where
      QCons  :: (forall d. Data d => d -> a) -> Query a -> Query a
      QEmpty :: Query a

type Q a = forall d. Data d => d -> a

instance Semigroup (Query a) where
      (QCons f fs) <> g = f `QCons` (fs <> g)
      QEmpty       <> g = g

instance Monoid (Query a) where
      mempty                 = QEmpty

generalizeQ :: (Typeable a, Monoid b) => (a -> b) -> forall d. Typeable d => d -> b
generalizeQ f x = maybe mempty f $ cast x

(||?) :: (Typeable d, Monoid a) => (d -> a) -> Query a -> Query a
f ||? fs = generalizeQ f `QCons` fs
infixr 1 ||?

query :: (Typeable d, Monoid a) => (d -> a) -> Query a
query = (||? QEmpty)

foldQ :: Monoid a => (Q a -> Q a -> Q a) -> Query a -> Q a
foldQ op (QCons f fs) = f `op` foldQ op fs
foldQ _ QEmpty        = const mempty

everywhereQ :: (Data a, Monoid b) => (forall d. Data d => d -> b) -> a -> b
everywhereQ f n = f n <> gmapQr (<>) mempty (everywhereQ f) n

-- | Returns the mappend sum of the result of all matches. No impure version,
--   so pattern match failures aren't caught.
runQ :: (Data d, Monoid a) => Query a -> d -> a
runQ q = everywhereQ $ foldQ (\ f g x -> f x <> g x) q

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}
module ReWire.SYB
      ( Transform (TId)
      , transform, (||>), runT
      , Query (QEmpty)
      , query, (||?), runQ
      , everywhere, gmapT
      ) where

import Control.Monad ((>=>))
import Data.Data (Data, Typeable, gmapQr, cast, gmapT)

import Data.Data.Lens (biplate, uniplate)
import Control.Lens.Plated (transformMOnOf)

everywhere :: Data a => (forall d. Data d => d -> d) -> a -> a
everywhere f = f . gmapT (everywhere f)

everywhereQ :: (Data a, Monoid b) => (forall d. Data d => d -> b) -> a -> b
everywhereQ f n = f n <> gmapQr (<>) mempty (everywhereQ f) n

data Transform m d where
      TCons :: Data d => (d -> m d) -> !(Transform m d) -> Transform m d
      TId   :: Transform m d

instance Semigroup (Transform m d) where
      (TCons f fs) <> g = f `TCons` (fs <> g)
      TId          <> g = g

instance Monoid (Transform m d) where
      mempty                 = TId

foldT :: Monad m => ((d -> m d) -> (d -> m d) -> (d -> m d)) -> Transform m d -> d -> m d
foldT op = \ case
      TCons f fs -> f `op` foldT op fs
      TId        -> pure

(||>) :: (Monad m, Data d, Data a) => (a -> m a) -> Transform m d -> Transform m d
f ||> fs = transformMOnOf biplate uniplate f `TCons` fs
infixr 1 ||>

transform :: (Monad m, Data a, Data d) => (a -> m a) -> Transform m d
transform = (||> TId)

runT :: (Monad m, Data d) => Transform m d -> d -> m d
runT = foldT (>=>)

-- | This is just a list of type
-- > [forall d. Data d => d -> a]
data Query a where
      QCons  :: !(forall d. Data d => d -> a) -> !(Query a) -> Query a
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
foldQ op = \ case
      QCons f fs -> f `op` foldQ op fs
      QEmpty     -> const mempty

-- | Returns the mappend sum of the result of all matches.
runQ :: (Data d, Monoid a) => Query a -> d -> a
runQ q = everywhereQ $ foldQ (\ f g x -> f x <> g x) q

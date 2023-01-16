{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}
module ReWire.SYB
      ( Transform (TId)
      , transform, (||>), runT
      , query
      , gmapT
      ) where

import Control.Monad ((>=>))
import Data.Data (Data, gmapT)

import Data.Data.Lens (biplate, uniplate)
import Control.Lens.Plated (transformMOnOf, universeOnOf)

data Transform m d where
      TCons :: Data d => (d -> m d) -> !(Transform m d) -> Transform m d
      TId   :: Transform m d

instance Semigroup (Transform m d) where
      (TCons f fs) <> g = f `TCons` (fs <> g)
      TId          <> g = g

instance Monoid (Transform m d) where
      mempty            = TId

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

query :: (Data a, Data b) => a -> [b]
query = universeOnOf biplate uniplate


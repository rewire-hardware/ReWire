{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ReWire.SYB
      ( Tr (TId, TM, T), transformTr
      , transform, transformM
      , query, queryWith, gmapT
      ) where

import Control.Lens.Plated (transformOnOf, transformMOnOf, universeOnOf)
import Control.Monad ((>=>))
import Data.Data (Data, Typeable, gmapT, gmapQ, cast)
import Data.Data.Lens (biplate, uniplate)

data Tr m a = TId
            | TM (a -> m a)
            | T  (a -> a)

instance Monad m => Semigroup (Tr m a) where
      TId  <> d    = d
      d    <> TId  = d
      TM f <> TM g = TM (f >=> g)
      TM f <> T g  = TM f <> TM (pure . g)
      T f  <> TM g = TM (pure . f) <> TM g
      T f  <> T g  = T (f . g)

instance Monad m => Monoid (Tr m a) where
      mempty = TId

transformTr :: (Data a, Data b, Monad m) => Tr m a -> b -> m b
transformTr = \ case
      TId  -> pure
      TM f -> transformM f
      T f  -> pure . transform f

transform :: (Data a, Data b) => (a -> a) -> b -> b
transform = transformOnOf biplate uniplate

transformM :: (Monad m, Data a, Data b) => (a -> m a) -> b -> m b
transformM = transformMOnOf biplate uniplate

query :: (Data a, Data b) => a -> [b]
query = universeOnOf biplate uniplate

-- | A query in a single generic sweep: collects @f b@ at every node of type
--   @b@. Unlike 'query' at an expression type, it never re-extracts a node's
--   children per level, which is quadratic on deeply nested binders.
queryWith :: forall a b r. (Data a, Typeable b) => (b -> [r]) -> a -> [r]
queryWith f = go
      where go :: forall d. Data d => d -> [r]
            go x = maybe [] f (cast x) <> concat (gmapQ go x)

{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReWire.FrontEnd.Unbound
      ( module Unbound.Generics.LocallyNameless.Name
      , module Unbound.Generics.LocallyNameless.Internal.Fold
      , Alpha (..), Fresh (..), FreshMT (..), Embed (..)
      , TRec (..), Bind (..)
      , Subst (..), SubstName (..)
      , runFreshM, runFreshMT, fv, fvAny, aeq
      , trec, untrec, bind, unbind, unembed
      ) where

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind
import safe Unbound.Generics.LocallyNameless.Name
import safe Unbound.Generics.LocallyNameless.Internal.Fold

import Control.Monad.Fail (MonadFail (..))

-- TODO(chathhorn): Orphan instance
instance MonadFail m => MonadFail (FreshMT m) where
      fail = undefined -- TODO(chathhorn)

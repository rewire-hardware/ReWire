{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReWire.Unbound
      ( module Unbound.Generics.LocallyNameless.Internal.Fold
      , Alpha (..), Fresh (..), FreshMT (..), Embed (..)
      , TRec (..), Bind (..)
      , Subst (..), SubstName (..)
      , runFreshM, runFreshMT, fv, fvAny, aeq
      , trec, untrec, bind, unbind, unembed
      , UB.Name (UB.Bn), AnyName (..), isFreeName
      , n2s, s2n
      ) where

import safe Data.Data (Data (..))
import Unbound.Generics.LocallyNameless hiding (s2n)
import Unbound.Generics.LocallyNameless.Bind
import safe qualified Unbound.Generics.LocallyNameless.Name as UB
import safe Unbound.Generics.LocallyNameless.Internal.Fold

import safe Data.Text (pack, unpack, Text)

n2s :: Name a -> Text
{-# INLINE n2s #-}
n2s = pack . name2String

s2n :: Text -> Name a
{-# INLINE s2n #-}
s2n = UB.s2n . unpack

deriving instance Data a => Data (Embed a)
deriving instance Data a => Data (Name a)
deriving instance (Data a, Data b) => Data (Bind a b)


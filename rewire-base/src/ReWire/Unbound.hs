{-# LANGUAGE Trustworthy #-}
module ReWire.Unbound
      ( UB.Alpha (..), UB.Fresh (..), UB.FreshMT (..), UB.Embed (..)
      , UB.TRec (..), UB.Bind (..)
      , UB.Subst (..), UB.SubstName (..)
      , UB.runFreshM, UB.runFreshMT, UB.aeq
      , UB.trec, UB.untrec, UB.bind, UB.unbind, UB.unembed
      , UB.Name (..), UB.AnyName (..), UB.isFreeName
      , UB.unsafeUnbind
      , n2s, s2n, bn2s, fv, fvAny, makeName, UB.name2Integer
      , freshVar
      ) where

import qualified Unbound.Generics.LocallyNameless                    as UB
import qualified Unbound.Generics.LocallyNameless.Bind               as UB
import safe qualified Unbound.Generics.LocallyNameless.Name          as UB
import safe qualified Unbound.Generics.LocallyNameless.Internal.Fold as UB
import qualified Unbound.Generics.LocallyNameless.Unsafe             as UB

import safe Data.Data (Typeable)
import safe Data.Text (pack, unpack, Text)

n2s :: UB.Name a -> Text
{-# INLINE n2s #-}
n2s = pack . UB.name2String

s2n :: Text -> UB.Name a
{-# INLINE s2n #-}
s2n = UB.s2n . unpack

makeName :: Text -> Integer -> UB.Name a
makeName = UB.makeName . unpack

-- | Version of name2String that returns an empty string instead of error for
--   bound variables.
bn2s :: UB.Name a -> Text
bn2s n | UB.isFreeName n = n2s n
       | otherwise       = mempty

fv :: (UB.Alpha a, Typeable b) => a -> [UB.Name b]
fv = UB.toListOf UB.fv

fvAny :: UB.Alpha a => a -> [UB.AnyName]
fvAny = UB.toListOf UB.fvAny

freshVar :: UB.Fresh m => Text -> m (UB.Name a)
freshVar = UB.fresh . s2n

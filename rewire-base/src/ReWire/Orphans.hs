{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReWire.Orphans where

import safe ReWire.Pretty (Doc, TextShow (showb, showbPrec), genericShowbPrec)
import safe ReWire.BitVector (BV (nat), width, showHex)

import safe Data.HashMap.Strict (HashMap)
import safe Data.HashSet (HashSet)
import safe Data.Hashable (Hashable (hashWithSalt, hash))
import safe Data.Map.Strict.Internal (Map (..))
import safe Data.Set.Internal (Set (..))
import safe GHC.Generics (Generic)

import qualified Data.Yaml                as YAML
import safe qualified Data.HashMap.Strict as Map
import safe qualified Data.HashSet        as Set

instance TextShow (Doc ann) where
      showb = showb . show

instance Hashable BV where
      hashWithSalt s bv = hashWithSalt s (width bv, nat bv)
      hash bv = hash (width bv, nat bv)

instance TextShow BV where
      showb = showb . showHex

instance YAML.ToJSON BV where
      toJSON = YAML.String . showHex

instance TextShow a => TextShow (HashSet a) where
      showb = showb . Set.toList

instance (TextShow a, TextShow b) => TextShow (HashMap a b) where
      showb = showb . Map.toList

deriving instance Generic a => Generic (Set a)
deriving instance (Generic a, Generic b) => Generic (Map a b)

instance (Generic a, TextShow a) => TextShow (Set a) where
      showbPrec = genericShowbPrec
instance (Generic a, Generic b, TextShow a, TextShow b) => TextShow (Map a b) where
      showbPrec = genericShowbPrec

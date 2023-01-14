{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReWire.Orphans where

import safe ReWire.Unbound (Bind (B), Name (..), Embed (Embed), AnyName (AnyName), TRec (TRec), Alpha (..))
import safe ReWire.Pretty (Pretty, pretty, Doc, TextShow (showt, showb, showbPrec), fromString, genericShowbPrec)
import safe ReWire.BitVector (BV (nat), width, showHex)

import safe Control.DeepSeq (NFData (rnf), deepseq)
import safe Data.Data (Data)
import safe Data.Hashable (Hashable (hashWithSalt, hash))
import safe Data.Text (Text)
import safe Numeric.Natural (Natural)

import qualified Data.Yaml as YAML

instance (Eq a, Eq b) => Eq (Bind a b) where
      (B a b) == (B a' b') = a == a' && b == b'

instance Alpha Text where
      aeq' _ctx i j = i == j
      fvAny' _ctx _nfn = pure
      close _ctx _b i = i
      open _ctx _b i = i
      isPat _ = mempty
      isTerm _ = mempty
      nthPatFind _ = mempty
      namePatFind _ = mempty
      swaps' _ctx _p i = i
      freshen' _ctx i = return (i, mempty)
      lfreshen' _ctx i cont = cont i mempty
      acompare' _ctx = compare

instance Alpha Natural where
      aeq' _ctx i j = i == j
      fvAny' _ctx _nfn = pure
      close _ctx _b i = i
      open _ctx _b i = i
      isPat _ = mempty
      isTerm _ = mempty
      nthPatFind _ = mempty
      namePatFind _ = mempty
      swaps' _ctx _p i = i
      freshen' _ctx i = return (i, mempty)
      lfreshen' _ctx i cont = cont i mempty
      acompare' _ctx = compare

instance NFData a => NFData (TRec a) where
      rnf (TRec r) = r `deepseq` ()

instance Pretty AnyName where
      pretty (AnyName n) = pretty n

instance Pretty (Name a) where
      pretty n = pretty $ showt n

instance (TextShow a, TextShow b) => TextShow (Bind a b) where
      showbPrec = genericShowbPrec

instance TextShow (Name a) where
      showb n = fromString $ show n

instance TextShow a => TextShow (Embed a) where
      showbPrec = genericShowbPrec

instance Pretty a => Pretty (Embed a) where
      pretty (Embed a) = pretty a

instance Hashable e => Hashable (Embed e)
instance Hashable e => Hashable (Name e)
instance (Hashable a, Hashable b) => Hashable (Bind a b)

instance TextShow (Doc ann) where
      showb = showb . show

instance Hashable BV where
      hashWithSalt s bv = hashWithSalt s (width bv, nat bv)
      hash bv = hash (width bv, nat bv)

instance TextShow BV where
      showb = showb . showHex

deriving instance Data a => Data (Embed a)
deriving instance Data a => Data (Name a)
deriving instance (Data a, Data b) => Data (Bind a b)

instance YAML.ToJSON BV where
      toJSON = YAML.String . showHex

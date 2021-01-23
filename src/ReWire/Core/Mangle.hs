{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Mangle (mangle) where

import Encoding (zEncodeString)   -- this is from the ghc package
import Data.Text (Text, pack, unpack)

-- TODO: text version of this?
mangle :: Text -> Text
mangle = pack . zEncodeString . unpack

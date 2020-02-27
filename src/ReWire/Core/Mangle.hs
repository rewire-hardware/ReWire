{-# LANGUAGE Trustworthy #-}
module ReWire.Core.Mangle (mangle) where

import Encoding (zEncodeString)   -- this is from the ghc package

mangle :: String -> String
mangle = zEncodeString

{-# LANGUAGE Trustworthy #-}
module ReWire.BitVector
      ( BV.BV (..), BV.width, BV.ones, BV.zeros, BV.nil
      , (BV.@@), BV.bitVec, (BV.>>.), (BV.<<.), (BV.==.), BV.ashr
      , BV.replicate, BV.lsb1, (BV.@.), BV.concat
      , showHex, showHex'
      ) where

import Data.Text (Text, pack)
import qualified Data.BitVector as BV

showHex :: BV.BV -> Text
showHex = pack . BV.showHex

-- | Like `showHex`, but prefix with `h` instead of `0x`.
showHex' :: BV.BV -> Text
showHex' = pack . ("h" <>) . drop 2 . BV.showHex

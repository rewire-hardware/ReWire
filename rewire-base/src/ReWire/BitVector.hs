{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.BitVector
      ( BV.BV (..), BV.width, BV.ones, BV.zeros, BV.nil
      , (BV.@@), BV.bitVec, (BV.>>.), (BV.<<.), (BV.==.), BV.ashr
      , BV.replicate, BV.lsb1, (BV.@.), BV.concat
      , showHex, showHex', nbits, szBitRep
      ) where

import Data.Text (Text, pack)
import Numeric.Natural (Natural)

import qualified Data.BitVector as BV
import qualified Numeric        as Num

-- | Show the bitvec value in hex, with "0x" prefix, but no leading zeros.
showHex :: BV.BV -> Text
showHex = toHex "0x"

-- | Like `showHex`, but prefix with `h` instead of `0x`.
showHex' :: BV.BV -> Text
showHex' = toHex "h"

toHex :: Text -> BV.BV -> Text
toHex pre = (pre <>) . pack . flip Num.showHex "" . BV.nat

-- | Number of bits needed to encode `n` different values.
nbits :: Natural -> Natural
nbits 0 = 0
nbits n = ceiling $ logBase 2 (fromIntegral n :: Double)

-- | Number of bits in the binary representation of `n` (with no leading
--   zeros).
szBitRep :: Natural -> Natural
szBitRep n = nbits $ n + 1

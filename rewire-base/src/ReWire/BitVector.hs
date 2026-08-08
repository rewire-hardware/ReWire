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

-- | Number of bits needed to encode `n` different values: exact integer
--   ceil-log2 via clog2(n) = 1 + clog2(ceil(n/2)) (floating-point logBase
--   mis-rounds near powers of two once n exceeds 2^29 or so).
nbits :: Natural -> Natural
nbits n | n <= 1    = 0
        | otherwise = 1 + nbits ((n + 1) `div` 2)

-- | Number of bits in the binary representation of `n` (with no leading
--   zeros).
szBitRep :: Natural -> Natural
szBitRep n = nbits $ n + 1

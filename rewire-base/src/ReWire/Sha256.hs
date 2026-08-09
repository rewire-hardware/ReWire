{-# LANGUAGE Safe #-}
-- | Pure SHA-256 (FIPS 180-4), used to bind validator responses to the
--   exact artifact bytes they were computed from. Self-contained so rwc
--   carries no cryptography dependency for the sake of one digest.
module ReWire.Sha256 (hashHex) where

import Data.Bits (complement, rotateR, shiftL, shiftR, xor, (.&.))
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Numeric (showHex)

import qualified Data.ByteString as BS
import qualified Data.Sequence   as Seq
import qualified Data.Text       as T

-- | The lowercase hex SHA-256 digest of a byte string.
hashHex :: BS.ByteString -> Text
hashHex bs = T.concat $ map wordHex [a, b, c, d, e, f, g, h]
      where (a, b, c, d, e, f, g, h) = foldl' block h0 $ chunks $ padded bs

            wordHex :: Word32 -> Text
            wordHex w = T.justifyRight 8 '0' $ T.pack $ showHex w ""

type St = (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32)

h0 :: St
h0 = (0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19)

ks :: Seq.Seq Word32
ks = Seq.fromList
      [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
      , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
      , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
      , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
      , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
      , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
      , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
      , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
      ]

padded :: BS.ByteString -> BS.ByteString
padded bs = bs <> BS.singleton 0x80 <> BS.replicate nzero 0 <> lenBytes
      where nzero    = (55 - BS.length bs) `mod` 64
            bitLen   = 8 * fromIntegral (BS.length bs) :: Word64
            lenBytes = BS.pack [fromIntegral $ bitLen `shiftR` s | s <- [56, 48 .. 0]]

chunks :: BS.ByteString -> [BS.ByteString]
chunks bs | BS.null bs = []
          | otherwise  = BS.take 64 bs : chunks (BS.drop 64 bs)

block :: St -> BS.ByteString -> St
block (a0, b0, c0, d0, e0, f0, g0, h') blk =
      case foldl' round' (a0, b0, c0, d0, e0, f0, g0, h') [0 .. 63] of
            (a, b, c, d, e, f, g, h) ->
                  (a0 + a, b0 + b, c0 + c, d0 + d, e0 + e, f0 + f, g0 + g, h' + h)
      where ws :: Seq.Seq Word32
            ws = extend $ Seq.fromList [ word32At (4 * i) | i <- [0 .. 15] ]

            extend :: Seq.Seq Word32 -> Seq.Seq Word32
            extend s | Seq.length s >= 64 = s
                     | otherwise          = extend $ s Seq.|> w
                  where i  = Seq.length s
                        w  = Seq.index s (i - 16) + s0 + Seq.index s (i - 7) + s1
                        s0 = rotateR (Seq.index s (i - 15)) 7  `xor` rotateR (Seq.index s (i - 15)) 18 `xor` shiftR (Seq.index s (i - 15)) 3
                        s1 = rotateR (Seq.index s (i - 2)) 17  `xor` rotateR (Seq.index s (i - 2)) 19  `xor` shiftR (Seq.index s (i - 2)) 10

            word32At :: Int -> Word32
            word32At i = shiftL (byte i) 24 + shiftL (byte $ i + 1) 16 + shiftL (byte $ i + 2) 8 + byte (i + 3)
                  where byte :: Int -> Word32
                        byte = fromIntegral . BS.index blk

            round' :: St -> Int -> St
            round' (a, b, c, d, e, f, g, h) i = (t1 + t2, a, b, c, d + t1, e, f, g)
                  where s1  = rotateR e 6 `xor` rotateR e 11 `xor` rotateR e 25
                        ch  = (e .&. f) `xor` (complement e .&. g)
                        t1  = h + s1 + ch + Seq.index ks i + Seq.index ws i
                        s0  = rotateR a 2 `xor` rotateR a 13 `xor` rotateR a 22
                        mj  = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
                        t2  = s0 + mj

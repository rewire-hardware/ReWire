module Globals256 where

import Data.Bits
import Data.Word
import Data.Char

data Oct a = Oct a a a a
                 a a a a deriving Show

data Hex a = Hex a a a a
                 a a a a
                 a a a a
                 a a a a deriving Show


-------------------------------------------
--- Padding the input (hacky)
-------------------------------------------
pad :: String -> [Hex Word32]
pad t = hex $ (quad $ map char2word8 t ++ zeroes l) ++ [hi,lo]
   where
     l  = length t * 8
     hi = hiword (fromIntegral l)
     lo = loword (fromIntegral l)

---
--- Hack to convert Char's to Word8's
---
char2word8 :: Char -> Word8
char2word8 = fromIntegral . ord

four :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
four w1 w2 w3 w4 = 0x1000000 * fromIntegral w1 + 0x10000 * fromIntegral w2 + 0x100 * fromIntegral w3 + fromIntegral w4

---
--- l is the length in bits of the input string
---
calc_k l =
  if r <= -1
    then fromIntegral r + 512
    else fromIntegral r
 where
  r = toInteger 448 - toInteger l `mod` toInteger 512 - 1

zeroes :: Integral a => a -> [Word8]
zeroes l = 0x80 : take n (repeat 0x00)
  where n = calc_k l `div` 8

quad :: [Word8] -> [Word32]
quad []                = []
quad (w1:w2:w3:w4:w8s) = four w1 w2 w3 w4 : quad w8s
quad _                 = error "shouldn't happen"
     
hiword :: Word64 -> Word32
hiword l = fromIntegral $ shiftR l 32

loword :: Word64 -> Word32
loword l = fromIntegral (l - (0x100000000 * (fromIntegral (hiword l))))

hex []                                                   = []
hex (w0:w1:w2:w3:w4:w5:w6:w7:w8:w9:wa:wb:wc:wd:we:wf:ws) = h : hex ws
  where
    h = Hex w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 wa wb wc wd we wf
hex _                                                    = error "shouldn't happen"

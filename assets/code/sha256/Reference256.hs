module Reference256 where

import Data.Bits
import Data.Word
import Data.Char
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption.Reactive
import Counter
import Globals256

-------------------------------------------------------------------------------------------
--- The following constitutes the reference semantics for SHA256. It is a straightforward
--- adaptation into monadic style of the algorithm given in the NIST document. See, in
--- particular, the "sha256" function below.
-------------------------------------------------------------------------------------------

--------------------------------------------
--- The standard functions
--------------------------------------------

ch :: Word32 -> Word32 -> Word32 -> Word32
ch x y z = (x .&. y) `xor` (complement x .&. z)

maj :: Word32 -> Word32 -> Word32 -> Word32
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

bigsigma0 :: Word32 -> Word32
bigsigma0 x = (rotateR x 2) `xor` (rotateR x 13) `xor` (rotateR x 22)

bigsigma1 :: Word32 -> Word32
bigsigma1 x = (rotateR x 6) `xor` (rotateR x 11) `xor` (rotateR x 25)

sigma0 :: Word32 -> Word32
sigma0 x = (rotateR x 7) `xor` (rotateR x 18) `xor` (shiftR x 3)

sigma1 :: Word32 -> Word32
sigma1 x = (rotateR x 17) `xor` (rotateR x 19) `xor` (shiftR x 10)

-------------------------------------------
--- The hashing algorithm
-------------------------------------------

type M = StateT (Oct Word32)
            (StateT (Hex Word32)
                (StateT (Oct Word32)
                    (StateT Ctr Identity)))

getDigest :: M (Oct Word32)
getDigest = lift (lift get)
putDigest :: Oct Word32 -> M ()
putDigest = lift . lift . put

getBlock :: M (Hex Word32)
getBlock = lift get
putBlock :: Hex Word32 -> M ()
putBlock = lift . put

getIntDig :: M (Oct Word32)
getIntDig = get
putIntDig :: Oct Word32 -> M ()
putIntDig = put

getCtr :: M Ctr
getCtr = lift (lift (lift get))
putCtr :: Ctr -> M ()
putCtr = lift . lift . lift . put

sha256 :: [Hex Word32] -> M (Oct Word32)
sha256 hws = do
  putDigest initialSHA256State
  mainloop hws
  getDigest

mainloop :: [Hex Word32] -> M ()
mainloop []             = return ()
mainloop (hw32 : hw32s) = do
                             hi_1 <- getDigest
                             putIntDig hi_1
                             putBlock hw32
                             putCtr C0
                             innerloop
                             mainloop hw32s

innerloop :: M ()
innerloop = do
               ctr <- getCtr
               s <- sched
               compress (seed ctr) s
               putCtr (incCtr ctr)
               case ctr of
                    C63 -> intermediate
                    _   -> innerloop
        
intermediate :: M ()
intermediate = do
  Oct h1 h2 h3 h4 h5 h6 h7 h8 <- getDigest
  Oct a b c d e f g h         <- getIntDig
  putDigest (Oct (a+h1) (b+h2) (c+h3) (d+h4) (e+h5) (f+h6) (g+h7) (h+h8))

-------------------------------------------
--- SHA-256 scheduler algorithm
-------------------------------------------

sched :: M Word32
sched = do
           s <- getBlock
           case s of
            (Hex w00 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) -> do
                                                          putBlock (updateSched s)
                                                          return w00

updateSched :: Hex Word32 -> Hex Word32
updateSched (Hex w00 w01 w02 w03 w04 w05 w06 w07 w08 w09 w10 w11 w12 w13 w14 w15) =
            (Hex w01 w02 w03 w04 w05 w06 w07 w08 w09 w10 w11 w12 w13 w14 w15 w16)
  where
    w16 = sigma1 w14 + w09 + sigma0 w01 + w00

-------------------------------------------
--- SHA-256 compression algorithm
-------------------------------------------

compress :: Word32 -> Word32 -> M ()
compress k w = do
                  dig' <- getIntDig
                  putIntDig (step256 k w dig')
    where step256 :: Word32 -> Word32 -> Oct Word32 -> Oct Word32
          step256 k w (Oct a b c d e f g h) = Oct a' b' c' d' e' f' g' h'
            where
              t1 = h + bigsigma1 e + ch e f g + k + w
              t2 = bigsigma0 a + maj a b c
              h' = g
              g' = f
              f' = e
              e' = d + t1
              d' = c
              c' = b
              b' = a
              a' = t1 + t2

initialSHA256State :: Oct Word32
initialSHA256State = Oct 0x6a09e667 0xbb67ae85 0x3c6ef372 0xa54ff53a
                         0x510e527f 0x9b05688c 0x1f83d9ab 0x5be0cd19



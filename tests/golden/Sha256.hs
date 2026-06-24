{-# LANGUAGE DataKinds #-}

import Prelude hiding ((+), (^), (==))
import ReWire hiding (Bit)
import ReWire.Bits
import ReWire.Finite (toFinite)
import ReWire.Vectors (index)

type W32 = W 32

data Oct a = Oct a a a a
                 a a a a -- deriving Show

data Hex a = Hex a a a a
                 a a a a
                 a a a a
                 a a a a -- deriving Show

--------------------------------------------
--- The standard functions
--------------------------------------------

ch :: W32 -> W32 -> W32 -> W32
ch x y z = (x .&. y) ^ (bnot x .&. z)

maj :: W32 -> W32 -> W32 -> W32
maj x y z = (x .&. y) ^ (x .&. z) ^ (y .&. z)

bigsigma0 :: W32 -> W32
bigsigma0 x = (rotR (lit 2) x) ^ (rotR (lit 13) x) ^ (rotR (lit 22) x)

bigsigma1 :: W32 -> W32
bigsigma1 x = (rotR (lit 6) x) ^ (rotR (lit 11) x) ^ (rotR (lit 25) x)

sigma0 :: W32 -> W32
sigma0 x = (rotR (lit 7) x) ^ (rotR (lit 18) x) ^ (x >>. lit 3)

sigma1 :: W32 -> W32
sigma1 x = (rotR (lit 17) x) ^ (rotR (lit 19) x) ^ (x >>. lit 10)

-------------------------------------------
--- The hashing algorithm
-------------------------------------------

intermediate :: StateT (Oct W32) (StateT (Hex W32) (StateT (Oct W32) (StateT Ctr Identity))) ()
{-# INLINE intermediate #-}
intermediate = do
  Oct h1 h2 h3 h4 h5 h6 h7 h8 <- lift (lift get)
  Oct a b c d e f g h         <- get
  lift (lift (put (Oct (a + h1) (b + h2) (c + h3) (d + h4) (e + h5) (f + h6) (g + h7) (h + h8))))

-------------------------------------------
--- SHA-256 scheduler algorithm
-------------------------------------------

sched :: StateT (Oct W32) (StateT (Hex W32) (StateT (Oct W32) (StateT Ctr Identity))) W32
{-# INLINE sched #-}
sched = lift (get >>= \ s ->
              case s of
                (Hex w00 a b c d e f g h i j k l m n o) -> put (updateSched s) >>= \ _ -> return w00)

updateSched :: Hex W32 -> Hex W32
updateSched (Hex w00 w01 w02 w03 w04 w05 w06 w07 w08 w09 w10 w11 w12 w13 w14 w15) =
            (Hex w01 w02 w03 w04 w05 w06 w07 w08 w09 w10 w11 w12 w13 w14 w15 w16)
  where
    w16 :: W32
    w16 = ((sigma1 w14) + w09) + ((sigma0 w01) + w00)

-------------------------------------------
--- SHA-256 compression algorithm
-------------------------------------------

compress :: W32 -> W32 -> StateT (Oct W32) (StateT (Hex W32) (StateT (Oct W32) (StateT Ctr Identity))) ()
{-# INLINE compress #-}
compress k w = do s <- get
                  put (step256 k w s)

step256 :: W32 -> W32 -> Oct W32 -> Oct W32
step256 k w (Oct a b c d e f g h) = Oct a' b' c' d' e' f' g' h'
            where
              t1,t2,h',g',f',e',d',c',b',a' :: W32
              t1 = h + (((bigsigma1 e) + (ch e f g)) + (k + w))
              t2 = (bigsigma0 a) + (maj a b c)
              h' = g
              g' = f
              f' = e
              e' = d + t1
              d' = c
              c' = b
              b' = a
              a' = t1 + t2

initialSHA256State :: Oct W32
initialSHA256State = Oct (lit 0x6a09e667) (lit 0xbb67ae85) (lit 0x3c6ef372) (lit 0xa54ff53a)
                         (lit 0x510e527f) (lit 0x9b05688c) (lit 0x1f83d9ab) (lit 0x5be0cd19)

-------------------------------------------
--- Rapid prototype of SHA256 in Hardware
-------------------------------------------

data Inp = Init (Hex W32) | Load (Hex W32) | DigestQ
data Out = DigestR (Oct W32) | Nix | Hashing

-------------------------------------------------------------------------------------------
--- SHA256 in ReWire. Note that the code from the reference semantics is
--- "cut and pasted" directly into the definition of dev below. In particular,
--- the highlighted lines are copied and lifted into ReacT. Calling this device
--- with the correctly formatted I-signals is an "unrolling" of the reference
--- semantics which can, I believe, be captured equationally.
-------------------------------------------------------------------------------------------

main = undefined

start :: ReacT Inp Out Identity ()
start = extrude
         (extrude
           (extrude
            (extrude
                 devsha256'
                 (Oct (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0)))
             (Hex (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0)))
         (Oct (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0) (lit 0x0))) (lit 0)


devsha256' :: ReacT Inp Out (StateT (Oct W32) (StateT (Hex W32) (StateT (Oct W32) (StateT Ctr Identity)))) ()
devsha256' = signal Nix >>= \ i -> dev i

dev :: Inp -> ReacT Inp Out (StateT (Oct W32) (StateT (Hex W32) (StateT (Oct W32) (StateT Ctr Identity)))) ()
dev (Init hw32) = do
                     lift (do
                              lift (lift (put initialSHA256State))
                              lift (lift (lift (put (lit 0))))
                              hi_1 <- lift (lift get)
                              put hi_1
                              lift (put hw32))
                     signal Nix
                     loop
dev (Load hw32) = do
                     lift (do
                              hi_1 <- lift (lift get)
                              put hi_1
                              lift (lift (lift (put (lit 0))))
                              lift (put hw32))
                     signal Nix
                     loop
dev DigestQ     = do
                     h_n <- lift (lift (lift get))
                     i <- signal (DigestR h_n)
                     dev i

genhash' :: StateT (Oct W32) (StateT (Hex W32) (StateT (Oct W32) (StateT Ctr Identity))) Ctr
{-# INLINE genhash' #-}
genhash' = do
             ctr <- (lift (lift (lift get)))
             sched >>= compress (seed ctr)
             lift (lift (lift (put (incCtr ctr))))
             return ctr

loop :: ReacT Inp Out (StateT (Oct W32) (StateT (Hex W32) (StateT (Oct W32) (StateT Ctr Identity)))) ()
loop   = do
            ctr <- lift genhash'
            i <- signal Hashing
            if ctr == lit 63
                  then lift intermediate >>= \ _ -> dev i
                  else loop


---
--- Inlining Counter.hs
---

type Ctr = W 6

incCtr :: Ctr -> Ctr
incCtr c = c + lit 1

seed :: Ctr -> W32
seed c = kTable `index` toFinite c

kTable :: Vec 64 (W 32)
kTable = fromList
      [ lit 0x428a2f98, lit 0x71374491, lit 0xb5c0fbcf, lit 0xe9b5dba5
      , lit 0x3956c25b, lit 0x59f111f1, lit 0x923f82a4, lit 0xab1c5ed5
      , lit 0xd807aa98, lit 0x12835b01, lit 0x243185be, lit 0x550c7dc3
      , lit 0x72be5d74, lit 0x80deb1fe, lit 0x9bdc06a7, lit 0xc19bf174
      , lit 0xe49b69c1, lit 0xefbe4786, lit 0x0fc19dc6, lit 0x240ca1cc
      , lit 0x2de92c6f, lit 0x4a7484aa, lit 0x5cb0a9dc, lit 0x76f988da
      , lit 0x983e5152, lit 0xa831c66d, lit 0xb00327c8, lit 0xbf597fc7
      , lit 0xc6e00bf3, lit 0xd5a79147, lit 0x06ca6351, lit 0x14292967
      , lit 0x27b70a85, lit 0x2e1b2138, lit 0x4d2c6dfc, lit 0x53380d13
      , lit 0x650a7354, lit 0x766a0abb, lit 0x81c2c92e, lit 0x92722c85
      , lit 0xa2bfe8a1, lit 0xa81a664b, lit 0xc24b8b70, lit 0xc76c51a3
      , lit 0xd192e819, lit 0xd6990624, lit 0xf40e3585, lit 0x106aa070
      , lit 0x19a4c116, lit 0x1e376c08, lit 0x2748774c, lit 0x34b0bcb5
      , lit 0x391c0cb3, lit 0x4ed8aa4a, lit 0x5b9cca4f, lit 0x682e6ff3
      , lit 0x748f82ee, lit 0x78a5636f, lit 0x84c87814, lit 0x8cc70208
      , lit 0x90befffa, lit 0xa4506ceb, lit 0xbef9a3f7, lit 0xc67178f2 ]

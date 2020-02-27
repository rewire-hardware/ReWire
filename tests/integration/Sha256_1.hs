{-
This is the first example from the paper "Model-driven Design & Synthesis of
the SHA-256 Cryptographic Hash Function in ReWire", RSP 2016. I have inlined
the MetaprogrammingRW and ReWirePrelude files because it works better with
cabal test.
-}

import ReWire
import ReWire.Bits hiding (W32 (..), andW32, xorW32, plusW32, notW32)

plusW32 :: W32 -> W32 -> W32
plusW32 = nativeVhdl "plusW32" plusW32

andW32 :: W32 -> W32 -> W32
andW32 = nativeVhdl "andW32" andW32

xorW32 :: W32 -> W32 -> W32
xorW32 = nativeVhdl "xorW32" xorW32

notW32 :: W32 -> W32
notW32 = nativeVhdl "notW32" notW32

data Oct a = Oct a a a a
                 a a a a -- deriving Show

data Hex a = Hex a a a a
                 a a a a
                 a a a a
                 a a a a -- deriving Show

data W32 = W32 Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit

--------------------------------------------
--- The standard functions
--------------------------------------------

ch :: W32 -> W32 -> W32 -> W32
ch x y z = (x `andW32` y) `xorW32` (notW32 x `andW32` z)

maj :: W32 -> W32 -> W32 -> W32
maj x y z = (x `andW32` y) `xorW32` (x `andW32` z) `xorW32` (y `andW32` z)

bigsigma0 :: W32 -> W32
bigsigma0 x = (rotateR2 x) `xorW32` (rotateR13 x) `xorW32` (rotateR22 x)

bigsigma1 :: W32 -> W32
bigsigma1 x = (rotateR6 x) `xorW32` (rotateR11 x) `xorW32` (rotateR25 x)

sigma0 :: W32 -> W32
sigma0 x = (rotateR7 x) `xorW32` (rotateR18 x) `xorW32` (shiftR3 x)

sigma1 :: W32 -> W32
sigma1 x = (rotateR17 x) `xorW32` (rotateR19 x) `xorW32` (shiftR10 x)

-------------------------------------------
--- The hashing algorithm
-------------------------------------------

intermediate :: StT (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I))) ()
{-# INLINE intermediate #-}
intermediate = do
  Oct h1 h2 h3 h4 h5 h6 h7 h8 <- lift (lift get)
  Oct a b c d e f g h         <- get
  lift (lift (put (Oct (plusW32 a h1) (plusW32 b h2) (plusW32 c h3) (plusW32 d h4) (plusW32 e h5) (plusW32 f h6) (plusW32 g h7) (plusW32 h h8))))

-------------------------------------------
--- SHA-256 scheduler algorithm
-------------------------------------------

sched :: StT (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I))) W32
{-# INLINE sched #-}
sched = lift (get >>= \ s ->
              case s of
                (Hex w00 a b c d e f g h i j k l m n o) -> put (updateSched s) >>= \ blah -> return w00)

updateSched :: Hex W32 -> Hex W32
updateSched (Hex w00 w01 w02 w03 w04 w05 w06 w07 w08 w09 w10 w11 w12 w13 w14 w15) =
            (Hex w01 w02 w03 w04 w05 w06 w07 w08 w09 w10 w11 w12 w13 w14 w15 w16)
  where
    w16 :: W32
    w16 = plusW32 (plusW32 (sigma1 w14) w09) (plusW32 (sigma0 w01) w00)

-------------------------------------------
--- SHA-256 compression algorithm
-------------------------------------------

compress :: W32 -> W32 -> StT (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I))) ()
{-# INLINE compress #-}
compress k w = do s <- get
                  put (step256 k w s)
                  
step256 :: W32 -> W32 -> Oct W32 -> Oct W32
step256 k w (Oct a b c d e f g h) = Oct a' b' c' d' e' f' g' h'
            where
              t1,t2,h',g',f',e',d',c',b',a' :: W32
              t1 = plusW32 h (plusW32 (plusW32 (bigsigma1 e) (ch e f g)) (plusW32 k w))
              t2 = plusW32 (bigsigma0 a) (maj a b c)
              h' = g
              g' = f
              f' = e
              e' = plusW32 d t1
              d' = c
              c' = b
              b' = a
              a' = plusW32 t1 t2

initialSHA256State :: Oct W32
initialSHA256State = Oct w6a09e667 wbb67ae85 w3c6ef372 wa54ff53a
                         w510e527f w9b05688c w1f83d9ab w5be0cd19

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
--- semantics which can, I believe, be captured equationally. Compare the two
--- "go" functions in TestingSHA256.hs.
-------------------------------------------------------------------------------------------

main = undefined

start :: ReT Inp Out I ()
start = extrude
         (extrude
           (extrude
            (extrude
                 devsha256'
                 (Oct w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000))
             (Hex w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000))
         (Oct w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000)) C0


devsha256' :: ReT Inp Out (StT (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I)))) ()
devsha256' = signal Nix >>= \ i -> dev i

dev :: Inp -> ReT Inp Out (StT (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I)))) ()
dev (Init hw32) = do
                     lift (do
                              lift (lift (put initialSHA256State))
                              lift (lift (lift (put C0)))
                              hi_1 <- lift (lift get)
                              put hi_1
                              lift (put hw32))
                     signal Nix
                     loop 
dev (Load hw32) = do
                     lift (do
                              hi_1 <- lift (lift get)
                              put hi_1
                              lift (lift (lift (put C0)))
                              lift (put hw32))
                     signal Nix
                     loop
dev DigestQ     = do
                     h_n <- lift (lift (lift get))
                     i <- signal (DigestR h_n)
                     dev i

genhash' :: StT (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I))) Ctr
{-# INLINE genhash' #-}
genhash' = do
             ctr <- (lift (lift (lift get)))
             sched >>= compress (seed ctr)
             lift (lift (lift (put (incCtr ctr))))
             return ctr

loop :: ReT Inp Out (StT (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I)))) ()
loop   = do
            ctr <- lift genhash'
            i <- signal Hashing
            case ctr of
                 C63 -> lift intermediate >>= \ dummy -> dev i
                 _   -> loop


---
--- Inlining Counter.hs
---

data Ctr = C0  | C1  | C2  | C3  | C4  | C5  | C6  | C7  |
           C8  | C9  | C10 | C11 | C12 | C13 | C14 | C15 |
           C16 | C17 | C18 | C19 | C20 | C21 | C22 | C23 |
           C24 | C25 | C26 | C27 | C28 | C29 | C30 | C31 |
           C32 | C33 | C34 | C35 | C36 | C37 | C38 | C39 |
           C40 | C41 | C42 | C43 | C44 | C45 | C46 | C47 |
           C48 | C49 | C50 | C51 | C52 | C53 | C54 | C55 |
           C56 | C57 | C58 | C59 | C60 | C61 | C62 | C63 

incCtr :: Ctr -> Ctr
incCtr = nativeVhdl "incCtr" incCtr

seed :: Ctr -> W32
seed C0  = w428a2f98
seed C1  = w71374491
seed C2  = wb5c0fbcf
seed C3  = we9b5dba5
seed C4  = w3956c25b
seed C5  = w59f111f1
seed C6  = w923f82a4
seed C7  = wab1c5ed5
seed C8  = wd807aa98
seed C9  = w12835b01
seed C10 = w243185be
seed C11 = w550c7dc3
seed C12 = w72be5d74
seed C13 = w80deb1fe
seed C14 = w9bdc06a7
seed C15 = wc19bf174
seed C16 = we49b69c1
seed C17 = wefbe4786
seed C18 = w0fc19dc6
seed C19 = w240ca1cc
seed C20 = w2de92c6f
seed C21 = w4a7484aa
seed C22 = w5cb0a9dc
seed C23 = w76f988da
seed C24 = w983e5152
seed C25 = wa831c66d
seed C26 = wb00327c8
seed C27 = wbf597fc7
seed C28 = wc6e00bf3
seed C29 = wd5a79147
seed C30 = w06ca6351
seed C31 = w14292967
seed C32 = w27b70a85
seed C33 = w2e1b2138
seed C34 = w4d2c6dfc
seed C35 = w53380d13
seed C36 = w650a7354
seed C37 = w766a0abb
seed C38 = w81c2c92e
seed C39 = w92722c85
seed C40 = wa2bfe8a1
seed C41 = wa81a664b
seed C42 = wc24b8b70
seed C43 = wc76c51a3
seed C44 = wd192e819
seed C45 = wd6990624
seed C46 = wf40e3585
seed C47 = w106aa070
seed C48 = w19a4c116
seed C49 = w1e376c08
seed C50 = w2748774c
seed C51 = w34b0bcb5
seed C52 = w391c0cb3
seed C53 = w4ed8aa4a
seed C54 = w5b9cca4f
seed C55 = w682e6ff3
seed C56 = w748f82ee
seed C57 = w78a5636f
seed C58 = w84c87814
seed C59 = w8cc70208
seed C60 = w90befffa
seed C61 = wa4506ceb
seed C62 = wbef9a3f7
seed C63 = wc67178f2

rotateR2,rotateR6,rotateR7,rotateR11,rotateR13,rotateR17,rotateR18,rotateR19,rotateR22,rotateR25 :: W32 -> W32
rotateR2 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b30 b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13
                  b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29)

rotateR6 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           =  (W32 b26 b27 b28 b29 b30 b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 
                   b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25)

rotateR7 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           =  (W32 b25 b26 b27 b28 b29 b30 b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 
                   b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24)

rotateR11 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b0 b1 b2 b3 b4 b5 
                  b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20)

rotateR13 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b0 b1 b2 b3 
                  b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18)

rotateR17 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31  
                  b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14)

rotateR18 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30
                  b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13)

rotateR19 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29
                  b30 b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)

rotateR22 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26
                  b27 b28 b29 b30 b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9)

rotateR25 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23
                  b24 b25 b26 b27 b28 b29 b30 b31 b0 b1 b2 b3 b4 b5 b6)

shiftR3,shiftR10 :: W32 -> W32
shiftR3 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
             b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 C C C b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12
                  b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28)

shiftR10 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
            = (W32 C C C C C C C C C C b0 b1 b2 b3 b4 b5
                   b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21)

w00000000 :: W32
w00000000 = W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C

{-
initialstateconsts = [ "6a09e667", "bb67ae85", "3c6ef372", "a54ff53a",
                         "510e527f", "9b05688c", "1f83d9ab", "5be0cd19" ]-}
                         
w6a09e667 :: W32
w6a09e667 = W32 C S S C S C S C C C C C S C C S S S S C C S S C C S S C C S S S

wbb67ae85 :: W32
wbb67ae85 = W32 S C S S S C S S C S S C C S S S S C S C S S S C S C C C C S C S

w3c6ef372 :: W32
w3c6ef372 = W32 C C S S S S C C C S S C S S S C S S S S C C S S C S S S C C S C

wa54ff53a :: W32
wa54ff53a = W32 S C S C C S C S C S C C S S S S S S S S C S C S C C S S S C S C

w510e527f :: W32
w510e527f = W32 C S C S C C C S C C C C S S S C C S C S C C S C C S S S S S S S

w9b05688c :: W32
w9b05688c = W32 S C C S S C S S C C C C C S C S C S S C S C C C S C C C S S C C

w1f83d9ab :: W32
w1f83d9ab = W32 C C C S S S S S S C C C C C S S S S C S S C C S S C S C S C S S

w5be0cd19 :: W32
w5be0cd19 = W32 C S C S S C S S S S S C C C C C S S C C S S C S C C C S S C C S

-- constants from genhash
{-
constantlist = [
           "428a2f98",
           "71374491",
           "b5c0fbcf",
           "e9b5dba5",
           "3956c25b",
           "59f111f1",
           "923f82a4",
           "ab1c5ed5",
           "d807aa98",
           "12835b01",
           "243185be",
           "550c7dc3",
           "72be5d74",
           "80deb1fe",
           "9bdc06a7",
           "c19bf174",
           "e49b69c1",
           "efbe4786",
           "0fc19dc6",
           "240ca1cc",
           "2de92c6f",
           "4a7484aa",
           "5cb0a9dc",
           "76f988da",
           "983e5152",
           "a831c66d",
           "b00327c8",
           "bf597fc7",
           "c6e00bf3",
           "d5a79147",
           "06ca6351",
           "14292967",
           "27b70a85",
           "2e1b2138",
           "4d2c6dfc",
           "53380d13",
           "650a7354",
           "766a0abb",
           "81c2c92e",
           "92722c85",
           "a2bfe8a1",
           "a81a664b",
           "c24b8b70",
           "c76c51a3",
           "d192e819",
           "d6990624",
           "f40e3585",
           "106aa070",
           "19a4c116",
           "1e376c08",
           "2748774c",
           "34b0bcb5",
           "391c0cb3",
           "4ed8aa4a",
           "5b9cca4f",
           "682e6ff3",
           "748f82ee",
           "78a5636f",
           "84c87814",
           "8cc70208",
           "90befffa",
           "a4506ceb",
           "bef9a3f7",
           "c67178f2"
           ]-}

w428a2f98 :: W32
w428a2f98 = W32 C S C C C C S C S C C C S C S C C C S C S S S S S C C S S C C C

w71374491 :: W32
w71374491 = W32 C S S S C C C S C C S S C S S S C S C C C S C C S C C S C C C S

wb5c0fbcf :: W32
wb5c0fbcf = W32 S C S S C S C S S S C C C C C C S S S S S C S S S S C C S S S S

we9b5dba5 :: W32
we9b5dba5 = W32 S S S C S C C S S C S S C S C S S S C S S C S S S C S C C S C S

w3956c25b :: W32
w3956c25b = W32 C C S S S C C S C S C S C S S C S S C C C C S C C S C S S C S S

w59f111f1 :: W32
w59f111f1 = W32 C S C S S C C S S S S S C C C S C C C S C C C S S S S S C C C S

w923f82a4 :: W32
w923f82a4 = W32 S C C S C C S C C C S S S S S S S C C C C C S C S C S C C S C C

wab1c5ed5 :: W32
wab1c5ed5 = W32 S C S C S C S S C C C S S S C C C S C S S S S C S S C S C S C S

wd807aa98 :: W32
wd807aa98 = W32 S S C S S C C C C C C C C S S S S C S C S C S C S C C S S C C C

w12835b01 :: W32
w12835b01 = W32 C C C S C C S C S C C C C C S S C S C S S C S S C C C C C C C S

w243185be :: W32
w243185be = W32 C C S C C S C C C C S S C C C S S C C C C S C S S C S S S S S C

w550c7dc3 :: W32
w550c7dc3 = W32 C S C S C S C S C C C C S S C C C S S S S S C S S S C C C C S S

w72be5d74 :: W32
w72be5d74 = W32 C S S S C C S C S C S S S S S C C S C S S S C S C S S S C S C C

w80deb1fe :: W32
w80deb1fe = W32 S C C C C C C C S S C S S S S C S C S S C C C S S S S S S S S C

w9bdc06a7 :: W32
w9bdc06a7 = W32 S C C S S C S S S S C S S S C C C C C C C S S C S C S C C S S S

wc19bf174 :: W32
wc19bf174 = W32 S S C C C C C S S C C S S C S S S S S S C C C S C S S S C S C C

we49b69c1 :: W32
we49b69c1 = W32 S S S C C S C C S C C S S C S S C S S C S C C S S S C C C C C S

wefbe4786 :: W32
wefbe4786 = W32 S S S C S S S S S C S S S S S C C S C C C S S S S C C C C S S C

w0fc19dc6 :: W32
w0fc19dc6 = W32 C C C C S S S S S S C C C C C S S C C S S S C S S S C C C S S C

w240ca1cc :: W32
w240ca1cc = W32 C C S C C S C C C C C C S S C C S C S C C C C S S S C C S S C C

w2de92c6f :: W32
w2de92c6f = W32 C C S C S S C S S S S C S C C S C C S C S S C C C S S C S S S S

w4a7484aa :: W32
w4a7484aa = W32 C S C C S C S C C S S S C S C C S C C C C S C C S C S C S C S C

w5cb0a9dc :: W32
w5cb0a9dc = W32 C S C S S S C C S C S S C C C C S C S C S C C S S S C S S S C C

w76f988da :: W32
w76f988da = W32 C S S S C S S C S S S S S C C S S C C C S C C C S S C S S C S C

w983e5152 :: W32
w983e5152 = W32 S C C S S C C C C C S S S S S C C S C S C C C S C S C S C C S C

wa831c66d :: W32
wa831c66d = W32 S C S C S C C C C C S S C C C S S S C C C S S C C S S C S S C S

wb00327c8 :: W32
wb00327c8 = W32 S C S S C C C C C C C C C C S S C C S C C S S S S S C C S C C C

wbf597fc7 :: W32
wbf597fc7 = W32 S C S S S S S S C S C S S C C S C S S S S S S S S S C C C S S S

wc6e00bf3 :: W32
wc6e00bf3 = W32 S S C C C S S C S S S C C C C C C C C C S C S S S S S S C C S S

wd5a79147 :: W32
wd5a79147 = W32 S S C S C S C S S C S C C S S S S C C S C C C S C S C C C S S S

w06ca6351 :: W32
w06ca6351 = W32 C C C C C S S C S S C C S C S C C S S C C C S S C S C S C C C S

w14292967 :: W32
w14292967 = W32 C C C S C S C C C C S C S C C S C C S C S C C S C S S C C S S S

w27b70a85 :: W32
w27b70a85 = W32 C C S C C S S S S C S S C S S S C C C C S C S C S C C C C S C S

w2e1b2138 :: W32
w2e1b2138 = W32 C C S C S S S C C C C S S C S S C C S C C C C S C C S S S C C C

w4d2c6dfc :: W32
w4d2c6dfc = W32 C S C C S S C S C C S C S S C C C S S C S S C S S S S S S S C C

w53380d13 :: W32
w53380d13 = W32 C S C S C C S S C C S S S C C C C C C C S S C S C C C S C C S S

w650a7354 :: W32
w650a7354 = W32 C S S C C S C S C C C C S C S C C S S S C C S S C S C S C S C C

w766a0abb :: W32
w766a0abb = W32 C S S S C S S C C S S C S C S C C C C C S C S C S C S S S C S S

w81c2c92e :: W32
w81c2c92e = W32 S C C C C C C S S S C C C C S C S S C C S C C S C C S C S S S C

w92722c85 :: W32
w92722c85 = W32 S C C S C C S C C S S S C C S C C C S C S S C C S C C C C S C S

wa2bfe8a1 :: W32
wa2bfe8a1 = W32 S C S C C C S C S C S S S S S S S S S C S C C C S C S C C C C S

wa81a664b :: W32
wa81a664b = W32 S C S C S C C C C C C S S C S C C S S C C S S C C S C C S C S S

wc24b8b70 :: W32
wc24b8b70 = W32 S S C C C C S C C S C C S C S S S C C C S C S S C S S S C C C C

wc76c51a3 :: W32
wc76c51a3 = W32 S S C C C S S S C S S C S S C C C S C S C C C S S C S C C C S S

wd192e819 :: W32
wd192e819 = W32 S S C S C C C S S C C S C C S C S S S C S C C C C C C S S C C S

wd6990624 :: W32
wd6990624 = W32 S S C S C S S C S C C S S C C S C C C C C S S C C C S C C S C C

wf40e3585 :: W32
wf40e3585 = W32 S S S S C S C C C C C C S S S C C C S S C S C S S C C C C S C S

w106aa070 :: W32
w106aa070 = W32 C C C S C C C C C S S C S C S C S C S C C C C C C S S S C C C C

w19a4c116 :: W32
w19a4c116 = W32 C C C S S C C S S C S C C S C C S S C C C C C S C C C S C S S C

w1e376c08 :: W32
w1e376c08 = W32 C C C S S S S C C C S S C S S S C S S C S S C C C C C C S C C C

w2748774c :: W32
w2748774c = W32 C C S C C S S S C S C C S C C C C S S S C S S S C S C C S S C C

w34b0bcb5 :: W32
w34b0bcb5 = W32 C C S S C S C C S C S S C C C C S C S S S S C C S C S S C S C S

w391c0cb3 :: W32
w391c0cb3 = W32 C C S S S C C S C C C S S S C C C C C C S S C C S C S S C C S S

w4ed8aa4a :: W32
w4ed8aa4a = W32 C S C C S S S C S S C S S C C C S C S C S C S C C S C C S C S C

w5b9cca4f :: W32
w5b9cca4f = W32 C S C S S C S S S C C S S S C C S S C C S C S C C S C C S S S S

w682e6ff3 :: W32
w682e6ff3 = W32 C S S C S C C C C C S C S S S C C S S C S S S S S S S S C C S S

w748f82ee :: W32
w748f82ee = W32 C S S S C S C C S C C C S S S S S C C C C C S C S S S C S S S C

w78a5636f :: W32
w78a5636f = W32 C S S S S C C C S C S C C S C S C S S C C C S S C S S C S S S S

w84c87814 :: W32
w84c87814 = W32 S C C C C S C C S S C C S C C C C S S S S C C C C C C S C S C C

w8cc70208 :: W32
w8cc70208 = W32 S C C C S S C C S S C C C S S S C C C C C C S C C C C C S C C C

w90befffa :: W32
w90befffa = W32 S C C S C C C C S C S S S S S C S S S S S S S S S S S S S C S C

wa4506ceb :: W32
wa4506ceb = W32 S C S C C S C C C S C S C C C C C S S C S S C C S S S C S C S S

wbef9a3f7 :: W32
wbef9a3f7 = W32 S C S S S S S C S S S S S C C S S C S C C C S S S S S S C S S S

wc67178f2 :: W32
wc67178f2 = W32 S S C C C S S C C S S S C C C S C S S S S C C C S S S S C C S C


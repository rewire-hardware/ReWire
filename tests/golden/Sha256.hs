{-# LANGUAGE DataKinds #-}

import Prelude hiding ((+), (^))
import ReWire hiding (Bit)
import ReWire.Bits

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
                (Hex w00 a b c d e f g h i j k l m n o) -> put (updateSched s) >>= \ blah -> return w00)

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
--- semantics which can, I believe, be captured equationally.
-------------------------------------------------------------------------------------------

main = undefined

start :: ReacT Inp Out Identity ()
start = extrude
         (extrude
           (extrude
            (extrude
                 devsha256'
                 (Oct w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000))
             (Hex w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000))
         (Oct w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000)) C0


devsha256' :: ReacT Inp Out (StateT (Oct W32) (StateT (Hex W32) (StateT (Oct W32) (StateT Ctr Identity)))) ()
devsha256' = signal Nix >>= \ i -> dev i

dev :: Inp -> ReacT Inp Out (StateT (Oct W32) (StateT (Hex W32) (StateT (Oct W32) (StateT Ctr Identity)))) ()
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
incCtr c = case c of
      C0 -> C1
      C1 -> C2
      C2 -> C3
      C3 -> C4
      C4 -> C5
      C5 -> C6
      C6 -> C7
      C7 -> C8
      C8 -> C9
      C9 -> C10
      C10 -> C11
      C11 -> C12
      C12 -> C13
      C13 -> C14
      C14 -> C15
      C15 -> C16
      C16 -> C17
      C17 -> C18
      C18 -> C19
      C19 -> C20
      C20 -> C21
      C21 -> C22
      C22 -> C23
      C23 -> C24
      C24 -> C25
      C25 -> C26
      C26 -> C27
      C27 -> C28
      C28 -> C29
      C29 -> C30
      C30 -> C31
      C31 -> C32
      C32 -> C33
      C33 -> C34
      C34 -> C35
      C35 -> C36
      C36 -> C37
      C37 -> C38
      C38 -> C39
      C39 -> C40
      C40 -> C41
      C41 -> C42
      C42 -> C43
      C43 -> C44
      C44 -> C45
      C45 -> C46
      C46 -> C47
      C47 -> C48
      C48 -> C49
      C49 -> C50
      C50 -> C51
      C51 -> C52
      C52 -> C53
      C53 -> C54
      C54 -> C55
      C55 -> C56
      C56 -> C57
      C57 -> C58
      C58 -> C59
      C59 -> C60
      C60 -> C61
      C61 -> C62
      C62 -> C63
      C63 -> C0

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

w00000000 :: W32
w00000000 = lit 0x0

-- Initial constants

w6a09e667 :: W32
w6a09e667 = lit 0x6a09e667

wbb67ae85 :: W32
wbb67ae85 = lit 0xbb67ae85

w3c6ef372 :: W32
w3c6ef372 = lit 0x3c6ef372

wa54ff53a :: W32
wa54ff53a = lit 0xa54ff53a

w510e527f :: W32
w510e527f = lit 0x510e527f

w9b05688c :: W32
w9b05688c = lit 0x9b05688c

w1f83d9ab :: W32
w1f83d9ab = lit 0x1f83d9ab

w5be0cd19 :: W32
w5be0cd19 = lit 0x5be0cd19

-- Constants from genhash

w428a2f98 :: W32
w428a2f98 = lit 0x428a2f98

w71374491 :: W32
w71374491 = lit 0x71374491

wb5c0fbcf :: W32
wb5c0fbcf = lit 0xb5c0fbcf

we9b5dba5 :: W32
we9b5dba5 = lit 0xe9b5dba5

w3956c25b :: W32
w3956c25b = lit 0x3956c25b

w59f111f1 :: W32
w59f111f1 = lit 0x59f111f1

w923f82a4 :: W32
w923f82a4 = lit 0x923f82a4

wab1c5ed5 :: W32
wab1c5ed5 = lit 0xab1c5ed5

wd807aa98 :: W32
wd807aa98 = lit 0xd807aa98

w12835b01 :: W32
w12835b01 = lit 0x12835b01

w243185be :: W32
w243185be = lit 0x243185be

w550c7dc3 :: W32
w550c7dc3 = lit 0x550c7dc3

w72be5d74 :: W32
w72be5d74 = lit 0x72be5d74

w80deb1fe :: W32
w80deb1fe = lit 0x80deb1fe

w9bdc06a7 :: W32
w9bdc06a7 = lit 0x9bdc06a7

wc19bf174 :: W32
wc19bf174 = lit 0xc19bf174

we49b69c1 :: W32
we49b69c1 = lit 0xe49b69c1

wefbe4786 :: W32
wefbe4786 = lit 0xefbe4786

w0fc19dc6 :: W32
w0fc19dc6 = lit 0x0fc19dc6

w240ca1cc :: W32
w240ca1cc = lit 0x240ca1cc

w2de92c6f :: W32
w2de92c6f = lit 0x2de92c6f

w4a7484aa :: W32
w4a7484aa = lit 0x4a7484aa

w5cb0a9dc :: W32
w5cb0a9dc = lit 0x5cb0a9dc

w76f988da :: W32
w76f988da = lit 0x76f988da

w983e5152 :: W32
w983e5152 = lit 0x983e5152

wa831c66d :: W32
wa831c66d = lit 0xa831c66d

wb00327c8 :: W32
wb00327c8 = lit 0xb00327c8

wbf597fc7 :: W32
wbf597fc7 = lit 0xbf597fc7

wc6e00bf3 :: W32
wc6e00bf3 = lit 0xc6e00bf3

wd5a79147 :: W32
wd5a79147 = lit 0xd5a79147

w06ca6351 :: W32
w06ca6351 = lit 0x06ca6351

w14292967 :: W32
w14292967 = lit 0x14292967

w27b70a85 :: W32
w27b70a85 = lit 0x27b70a85

w2e1b2138 :: W32
w2e1b2138 = lit 0x2e1b2138

w4d2c6dfc :: W32
w4d2c6dfc = lit 0x4d2c6dfc

w53380d13 :: W32
w53380d13 = lit 0x53380d13

w650a7354 :: W32
w650a7354 = lit 0x650a7354

w766a0abb :: W32
w766a0abb = lit 0x766a0abb

w81c2c92e :: W32
w81c2c92e = lit 0x81c2c92e

w92722c85 :: W32
w92722c85 = lit 0x92722c85

wa2bfe8a1 :: W32
wa2bfe8a1 = lit 0xa2bfe8a1

wa81a664b :: W32
wa81a664b = lit 0xa81a664b

wc24b8b70 :: W32
wc24b8b70 = lit 0xc24b8b70

wc76c51a3 :: W32
wc76c51a3 = lit 0xc76c51a3

wd192e819 :: W32
wd192e819 = lit 0xd192e819

wd6990624 :: W32
wd6990624 = lit 0xd6990624

wf40e3585 :: W32
wf40e3585 = lit 0xf40e3585

w106aa070 :: W32
w106aa070 = lit 0x106aa070

w19a4c116 :: W32
w19a4c116 = lit 0x19a4c116

w1e376c08 :: W32
w1e376c08 = lit 0x1e376c08

w2748774c :: W32
w2748774c = lit 0x2748774c

w34b0bcb5 :: W32
w34b0bcb5 = lit 0x34b0bcb5

w391c0cb3 :: W32
w391c0cb3 = lit 0x391c0cb3

w4ed8aa4a :: W32
w4ed8aa4a = lit 0x4ed8aa4a

w5b9cca4f :: W32
w5b9cca4f = lit 0x5b9cca4f

w682e6ff3 :: W32
w682e6ff3 = lit 0x682e6ff3

w748f82ee :: W32
w748f82ee = lit 0x748f82ee

w78a5636f :: W32
w78a5636f = lit 0x78a5636f

w84c87814 :: W32
w84c87814 = lit 0x84c87814

w8cc70208 :: W32
w8cc70208 = lit 0x8cc70208

w90befffa :: W32
w90befffa = lit 0x90befffa

wa4506ceb :: W32
wa4506ceb = lit 0xa4506ceb

wbef9a3f7 :: W32
wbef9a3f7 = lit 0xbef9a3f7

wc67178f2 :: W32
wc67178f2 = lit 0xc67178f2


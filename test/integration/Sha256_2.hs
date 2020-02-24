{-
This is the second example from the paper "Model-driven Design & Synthesis of
the SHA-256 Cryptographic Hash Function in ReWire", RSP 2016. This is the
"marshaling" example. I have inlined the MetaprogrammingRW and ReWirePrelude
files because it works better with cabal test.
-}
--import MetaprogrammingRW
--import RWPrelude

{-
---------------------------------------------
--- ReWire Fig Leaf
---------------------------------------------

import Data.Bits
import Data.Word
import Data.Char
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption.Reactive

type ReT = ReacT
type StT = StateT
type I   = Identity

extrude :: Monad m => ReT i o (StT s m) a -> s -> ReT i o m a
extrude = undefined

nativeVhdl :: String -> a -> a
nativeVhdl = flip const

---------------------------------------------
--- End of Fig Leaf
---------------------------------------------
-}

w32Plus :: W32 -> W32 -> W32
{-# INLINE w32Plus #-}
w32Plus = nativeVhdl "w32Plus" w32Plus
w32Xor :: W32 -> W32 -> W32
{-# INLINE w32Xor #-}
w32Xor  = nativeVhdl "w32Xor" w32Xor
w32And :: W32 -> W32 -> W32
{-# INLINE w32And #-}
w32And  = nativeVhdl "w32And" w32And
w32Not :: W32 -> W32
{-# INLINE w32Not #-}
w32Not = nativeVhdl "w32Not" w32Not

data Oct a = Oct a a a a
                 a a a a deriving Show

data Hex a = Hex a a a a
                 a a a a
                 a a a a
                 a a a a deriving Show

--------------------------------------------
--- The standard functions
--------------------------------------------

ch :: W32 -> W32 -> W32 -> W32
ch x y z = (x `w32And` y) `w32Xor` (w32Not x `w32And` z)

maj :: W32 -> W32 -> W32 -> W32
maj x y z = (x `w32And` y) `w32Xor` (x `w32And` z) `w32Xor` (y `w32And` z)

bigsigma0 :: W32 -> W32
bigsigma0 x = (rotateR2 x) `w32Xor` (rotateR13 x) `w32Xor` (rotateR22 x)

bigsigma1 :: W32 -> W32
bigsigma1 x = (rotateR6 x) `w32Xor` (rotateR11 x) `w32Xor` (rotateR25 x)

sigma0 :: W32 -> W32
sigma0 x = (rotateR7 x) `w32Xor` (rotateR18 x) `w32Xor` (shiftR3 x)

sigma1 :: W32 -> W32
sigma1 x = (rotateR17 x) `w32Xor` (rotateR19 x) `w32Xor` (shiftR10 x)

-------------------------------------------
--- The hashing algorithm
-------------------------------------------

intermediate :: StT (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I))) ()
{-# INLINE intermediate #-}
intermediate = do
  Oct h1 h2 h3 h4 h5 h6 h7 h8 <- lift (lift get)
  Oct a b c d e f g h         <- get
  lift (lift (put (Oct (w32Plus a h1) (w32Plus b h2) (w32Plus c h3) (w32Plus d h4) (w32Plus e h5) (w32Plus f h6) (w32Plus g h7) (w32Plus h h8))))

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
    w16 = w32Plus (w32Plus (sigma1 w14) w09) (w32Plus (sigma0 w01) w00)

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
              t1 = w32Plus h (w32Plus (w32Plus (bigsigma1 e) (ch e f g)) (w32Plus k w))
              t2 = w32Plus (bigsigma0 a) (maj a b c)
              h' = g
              g' = f
              f' = e
              e' = w32Plus d t1
              d' = c
              c' = b
              b' = a
              a' = w32Plus t1 t2

initialSHA256State :: Oct W32
initialSHA256State = Oct w6a09e667 wbb67ae85 w3c6ef372 wa54ff53a
                         w510e527f w9b05688c w1f83d9ab w5be0cd19

-------------------------------------------
--- Rapid prototype of SHA256 in Hardware
-------------------------------------------

load0 :: a -> a -> Hex a -> Hex a
{-# INLINE load0 #-}
load0 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex w1 w2 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf

load1 :: a -> a -> Hex a -> Hex a
{-# INLINE load1 #-}
load1 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 w1 w2 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf

load2 :: a -> a -> Hex a -> Hex a
{-# INLINE load2 #-}
load2 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 w1 w2 x6 x7 x8 x9 xa xb xc xd xe xf

load3 :: a -> a -> Hex a -> Hex a
{-# INLINE load3 #-}
load3 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 x4 x5 w1 w2 x8 x9 xa xb xc xd xe xf

load4 :: a -> a -> Hex a -> Hex a
{-# INLINE load4 #-}
load4 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 x4 x5 x6 x7 w1 w2 xa xb xc xd xe xf

load5 :: a -> a -> Hex a -> Hex a
{-# INLINE load5 #-}
load5 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 w1 w2 xc xd xe xf
    
load6 :: a -> a -> Hex a -> Hex a
{-# INLINE load6 #-}
load6 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb w1 w2 xe xf

load7 :: a -> a -> Hex a -> Hex a
{-# INLINE load7 #-}
load7 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd w1 w2

data Inp = Init W32 W32  |
           Load0 W32 W32 | 
           Load1 W32 W32 | 
           Load2 W32 W32 | 
           Load3 W32 W32 | 
           Load4 W32 W32 | 
           Load5 W32 W32 | 
           Load6 W32 W32 | 
           Load7 W32 W32 |
           DigestQ0      |
           DigestQ1      |
           DigestQ2      |
           DigestQ3

data Out = DigestR W32 W32 | Nix

digest0, digest1, digest2, digest3 :: Oct W32 -> Out
digest0 (Oct x0 x1 x2 x3 x4 x5 x6 x7) = DigestR x0 x1
digest1 (Oct x0 x1 x2 x3 x4 x5 x6 x7) = DigestR x2 x3
digest2 (Oct x0 x1 x2 x3 x4 x5 x6 x7) = DigestR x4 x5
digest3 (Oct x0 x1 x2 x3 x4 x5 x6 x7) = DigestR x6 x7

-------------------------------------------------------------------------------------------
--- SHA256 in ReWire. Note that the code from the reference semantics is
--- "cut and pasted" directly into the definition of dev below. In particular,
--- the highlighted lines are copied and lifted into ReacT. Calling this device
--- with the correctly formatted I-signals is an "unrolling" of the reference
--- semantics which can, I believe, be captured equationally. Compare the two
--- "go" functions in TestingSHA256.hs.
-------------------------------------------------------------------------------------------

start :: ReT Inp Out I ()
start = extrude
         (extrude
           (extrude
            (extrude
                 devsha256'
                 (Oct w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000))
             (Hex w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000))
         (Oct w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000)) C0

devsha256'
  :: ReT
       Inp
       Out
       (StT
          (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I))))
       ()
devsha256' = signal Nix >>= \ d -> dev d

dev
  :: Inp
     -> ReT
          Inp
          Out
          (StT
             (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I))))
          ()
dev (Init w1 w2) = do
                      lift  (do
                                lift (lift (put initialSHA256State))
                                hw <- lift get
                                lift (put (load0 w1 w2 hw)))
                      i <- signal Nix
                      dev i
dev (Load0 w1 w2) = do
                       lift  (do
                                 hw <- lift get
                                 lift (put (load0 w1 w2 hw)))
                       i <- signal Nix
                       dev i
dev (Load1 w1 w2) = do
                       lift  (do
                                 hw <- lift get
                                 lift (put (load1 w1 w2 hw)))
                       i <- signal Nix
                       dev i
dev (Load2 w1 w2) = do
                       lift  (do
                                 hw <- lift get
                                 lift (put (load2 w1 w2 hw)))
                       i <- signal Nix
                       dev i
dev (Load3 w1 w2) = do
                       lift  (do
                                 hw <- lift get
                                 lift (put (load3 w1 w2 hw)))
                       i <- signal Nix
                       dev i
dev (Load4 w1 w2) = do
                       lift  (do
                                 hw <- lift get
                                 lift (put (load4 w1 w2 hw)))
                       i <- signal Nix
                       dev i
dev (Load5 w1 w2) = do
                       lift  (do
                                 hw <- lift get
                                 lift (put (load5 w1 w2 hw)))
                       i <- signal Nix
                       dev i
dev (Load6 w1 w2) = do
                       lift  (do
                                 hw <- lift get
                                 lift (put (load6 w1 w2 hw)))
                       i <- signal Nix
                       dev i
dev (Load7 w1 w2) = do
                       lift  (do
                                 lift (lift (lift (put C0)))
                                 hi_1 <- lift (lift get)
                                 put hi_1
                                 hw <- lift get
                                 lift (put (load7 w1 w2 hw)))
                       signal Nix
                       loop
dev DigestQ0      = do
                       h_n <- lift (lift (lift get))
                       i <- signal (digest0 h_n)
                       dev i
dev DigestQ1      = do
                       h_n <- lift (lift (lift get))
                       i <- signal (digest1 h_n)
                       dev i
dev DigestQ2      = do
                       h_n <- lift (lift (lift get))
                       i <- signal (digest2 h_n)
                       dev i
dev DigestQ3      = do
                       h_n <- lift (lift (lift get))
                       i <- signal (digest3 h_n)
                       dev i

genhash'
  :: StT
       (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I))) Ctr
genhash' = do
             ctr <- (lift (lift (lift get)))
             sched >>= compress (seed ctr)
             lift (lift (lift (put (incCtr ctr))))
             return ctr

loop
  :: ReT
       Inp
       Out
       (StT
          (Oct W32) (StT (Hex W32) (StT (Oct W32) (StT Ctr I))))
       ()
loop   = do
            ctr <- lift genhash'
            i <- signal Nix
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
incCtr C0  = C1
incCtr C1  = C2
incCtr C2  = C3
incCtr C3  = C4
incCtr C4  = C5
incCtr C5  = C6
incCtr C6  = C7
incCtr C7  = C8
incCtr C8  = C9
incCtr C9  = C10
incCtr C10 = C11
incCtr C11 = C12
incCtr C12 = C13
incCtr C13 = C14
incCtr C14 = C15
incCtr C15 = C16
incCtr C16 = C17
incCtr C17 = C18
incCtr C18 = C19
incCtr C19 = C20
incCtr C20 = C21
incCtr C21 = C22
incCtr C22 = C23
incCtr C23 = C24
incCtr C24 = C25
incCtr C25 = C26
incCtr C26 = C27
incCtr C27 = C28
incCtr C28 = C29
incCtr C29 = C30
incCtr C30 = C31
incCtr C31 = C32
incCtr C32 = C33
incCtr C33 = C34
incCtr C34 = C35
incCtr C35 = C36
incCtr C36 = C37
incCtr C37 = C38
incCtr C38 = C39
incCtr C39 = C40
incCtr C40 = C41
incCtr C41 = C42
incCtr C42 = C43
incCtr C43 = C44
incCtr C44 = C45
incCtr C45 = C46
incCtr C46 = C47
incCtr C47 = C48
incCtr C48 = C49
incCtr C49 = C50
incCtr C50 = C51
incCtr C51 = C52
incCtr C52 = C53
incCtr C53 = C54
incCtr C54 = C55
incCtr C55 = C56
incCtr C56 = C57
incCtr C57 = C58
incCtr C58 = C59
incCtr C59 = C60
incCtr C60 = C61
incCtr C61 = C62
incCtr C62 = C63
incCtr C63 = C0 

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

data Bit = Zero | One
data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit
data W32 = W32 Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit

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
           = (W32 Zero Zero Zero b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12
                  b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28)

shiftR10 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
            = (W32 Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero b0 b1 b2 b3 b4 b5
                   b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21)


w00000000 :: W32
w00000000 = W32 Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero

{-
initialstateconsts = [ "6a09e667", "bb67ae85", "3c6ef372", "a54ff53a",
                         "510e527f", "9b05688c", "1f83d9ab", "5be0cd19" ]-}
                         
w6a09e667 :: W32
w6a09e667 = W32 Zero One One Zero One Zero One Zero Zero Zero Zero Zero One Zero Zero One One One One Zero Zero One One Zero Zero One One Zero Zero One One One

wbb67ae85 :: W32
wbb67ae85 = W32 One Zero One One One Zero One One Zero One One Zero Zero One One One One Zero One Zero One One One Zero One Zero Zero Zero Zero One Zero One

w3c6ef372 :: W32
w3c6ef372 = W32 Zero Zero One One One One Zero Zero Zero One One Zero One One One Zero One One One One Zero Zero One One Zero One One One Zero Zero One Zero

wa54ff53a :: W32
wa54ff53a = W32 One Zero One Zero Zero One Zero One Zero One Zero Zero One One One One One One One One Zero One Zero One Zero Zero One One One Zero One Zero

w510e527f :: W32
w510e527f = W32 Zero One Zero One Zero Zero Zero One Zero Zero Zero Zero One One One Zero Zero One Zero One Zero Zero One Zero Zero One One One One One One One

w9b05688c :: W32
w9b05688c = W32 One Zero Zero One One Zero One One Zero Zero Zero Zero Zero One Zero One Zero One One Zero One Zero Zero Zero One Zero Zero Zero One One Zero Zero

w1f83d9ab :: W32
w1f83d9ab = W32 Zero Zero Zero One One One One One One Zero Zero Zero Zero Zero One One One One Zero One One Zero Zero One One Zero One Zero One Zero One One

w5be0cd19 :: W32
w5be0cd19 = W32 Zero One Zero One One Zero One One One One One Zero Zero Zero Zero Zero One One Zero Zero One One Zero One Zero Zero Zero One One Zero Zero One

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
w428a2f98 = W32 Zero One Zero Zero Zero Zero One Zero One Zero Zero Zero One Zero One Zero Zero Zero One Zero One One One One One Zero Zero One One Zero Zero Zero

w71374491 :: W32
w71374491 = W32 Zero One One One Zero Zero Zero One Zero Zero One One Zero One One One Zero One Zero Zero Zero One Zero Zero One Zero Zero One Zero Zero Zero One

wb5c0fbcf :: W32
wb5c0fbcf = W32 One Zero One One Zero One Zero One One One Zero Zero Zero Zero Zero Zero One One One One One Zero One One One One Zero Zero One One One One

we9b5dba5 :: W32
we9b5dba5 = W32 One One One Zero One Zero Zero One One Zero One One Zero One Zero One One One Zero One One Zero One One One Zero One Zero Zero One Zero One

w3956c25b :: W32
w3956c25b = W32 Zero Zero One One One Zero Zero One Zero One Zero One Zero One One Zero One One Zero Zero Zero Zero One Zero Zero One Zero One One Zero One One

w59f111f1 :: W32
w59f111f1 = W32 Zero One Zero One One Zero Zero One One One One One Zero Zero Zero One Zero Zero Zero One Zero Zero Zero One One One One One Zero Zero Zero One

w923f82a4 :: W32
w923f82a4 = W32 One Zero Zero One Zero Zero One Zero Zero Zero One One One One One One One Zero Zero Zero Zero Zero One Zero One Zero One Zero Zero One Zero Zero

wab1c5ed5 :: W32
wab1c5ed5 = W32 One Zero One Zero One Zero One One Zero Zero Zero One One One Zero Zero Zero One Zero One One One One Zero One One Zero One Zero One Zero One

wd807aa98 :: W32
wd807aa98 = W32 One One Zero One One Zero Zero Zero Zero Zero Zero Zero Zero One One One One Zero One Zero One Zero One Zero One Zero Zero One One Zero Zero Zero

w12835b01 :: W32
w12835b01 = W32 Zero Zero Zero One Zero Zero One Zero One Zero Zero Zero Zero Zero One One Zero One Zero One One Zero One One Zero Zero Zero Zero Zero Zero Zero One

w243185be :: W32
w243185be = W32 Zero Zero One Zero Zero One Zero Zero Zero Zero One One Zero Zero Zero One One Zero Zero Zero Zero One Zero One One Zero One One One One One Zero

w550c7dc3 :: W32
w550c7dc3 = W32 Zero One Zero One Zero One Zero One Zero Zero Zero Zero One One Zero Zero Zero One One One One One Zero One One One Zero Zero Zero Zero One One

w72be5d74 :: W32
w72be5d74 = W32 Zero One One One Zero Zero One Zero One Zero One One One One One Zero Zero One Zero One One One Zero One Zero One One One Zero One Zero Zero

w80deb1fe :: W32
w80deb1fe = W32 One Zero Zero Zero Zero Zero Zero Zero One One Zero One One One One Zero One Zero One One Zero Zero Zero One One One One One One One One Zero

w9bdc06a7 :: W32
w9bdc06a7 = W32 One Zero Zero One One Zero One One One One Zero One One One Zero Zero Zero Zero Zero Zero Zero One One Zero One Zero One Zero Zero One One One

wc19bf174 :: W32
wc19bf174 = W32 One One Zero Zero Zero Zero Zero One One Zero Zero One One Zero One One One One One One Zero Zero Zero One Zero One One One Zero One Zero Zero

we49b69c1 :: W32
we49b69c1 = W32 One One One Zero Zero One Zero Zero One Zero Zero One One Zero One One Zero One One Zero One Zero Zero One One One Zero Zero Zero Zero Zero One

wefbe4786 :: W32
wefbe4786 = W32 One One One Zero One One One One One Zero One One One One One Zero Zero One Zero Zero Zero One One One One Zero Zero Zero Zero One One Zero

w0fc19dc6 :: W32
w0fc19dc6 = W32 Zero Zero Zero Zero One One One One One One Zero Zero Zero Zero Zero One One Zero Zero One One One Zero One One One Zero Zero Zero One One Zero

w240ca1cc :: W32
w240ca1cc = W32 Zero Zero One Zero Zero One Zero Zero Zero Zero Zero Zero One One Zero Zero One Zero One Zero Zero Zero Zero One One One Zero Zero One One Zero Zero

w2de92c6f :: W32
w2de92c6f = W32 Zero Zero One Zero One One Zero One One One One Zero One Zero Zero One Zero Zero One Zero One One Zero Zero Zero One One Zero One One One One

w4a7484aa :: W32
w4a7484aa = W32 Zero One Zero Zero One Zero One Zero Zero One One One Zero One Zero Zero One Zero Zero Zero Zero One Zero Zero One Zero One Zero One Zero One Zero

w5cb0a9dc :: W32
w5cb0a9dc = W32 Zero One Zero One One One Zero Zero One Zero One One Zero Zero Zero Zero One Zero One Zero One Zero Zero One One One Zero One One One Zero Zero

w76f988da :: W32
w76f988da = W32 Zero One One One Zero One One Zero One One One One One Zero Zero One One Zero Zero Zero One Zero Zero Zero One One Zero One One Zero One Zero

w983e5152 :: W32
w983e5152 = W32 One Zero Zero One One Zero Zero Zero Zero Zero One One One One One Zero Zero One Zero One Zero Zero Zero One Zero One Zero One Zero Zero One Zero

wa831c66d :: W32
wa831c66d = W32 One Zero One Zero One Zero Zero Zero Zero Zero One One Zero Zero Zero One One One Zero Zero Zero One One Zero Zero One One Zero One One Zero One

wb00327c8 :: W32
wb00327c8 = W32 One Zero One One Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero One One Zero Zero One Zero Zero One One One One One Zero Zero One Zero Zero Zero

wbf597fc7 :: W32
wbf597fc7 = W32 One Zero One One One One One One Zero One Zero One One Zero Zero One Zero One One One One One One One One One Zero Zero Zero One One One

wc6e00bf3 :: W32
wc6e00bf3 = W32 One One Zero Zero Zero One One Zero One One One Zero Zero Zero Zero Zero Zero Zero Zero Zero One Zero One One One One One One Zero Zero One One

wd5a79147 :: W32
wd5a79147 = W32 One One Zero One Zero One Zero One One Zero One Zero Zero One One One One Zero Zero One Zero Zero Zero One Zero One Zero Zero Zero One One One

w06ca6351 :: W32
w06ca6351 = W32 Zero Zero Zero Zero Zero One One Zero One One Zero Zero One Zero One Zero Zero One One Zero Zero Zero One One Zero One Zero One Zero Zero Zero One

w14292967 :: W32
w14292967 = W32 Zero Zero Zero One Zero One Zero Zero Zero Zero One Zero One Zero Zero One Zero Zero One Zero One Zero Zero One Zero One One Zero Zero One One One

w27b70a85 :: W32
w27b70a85 = W32 Zero Zero One Zero Zero One One One One Zero One One Zero One One One Zero Zero Zero Zero One Zero One Zero One Zero Zero Zero Zero One Zero One

w2e1b2138 :: W32
w2e1b2138 = W32 Zero Zero One Zero One One One Zero Zero Zero Zero One One Zero One One Zero Zero One Zero Zero Zero Zero One Zero Zero One One One Zero Zero Zero

w4d2c6dfc :: W32
w4d2c6dfc = W32 Zero One Zero Zero One One Zero One Zero Zero One Zero One One Zero Zero Zero One One Zero One One Zero One One One One One One One Zero Zero

w53380d13 :: W32
w53380d13 = W32 Zero One Zero One Zero Zero One One Zero Zero One One One Zero Zero Zero Zero Zero Zero Zero One One Zero One Zero Zero Zero One Zero Zero One One

w650a7354 :: W32
w650a7354 = W32 Zero One One Zero Zero One Zero One Zero Zero Zero Zero One Zero One Zero Zero One One One Zero Zero One One Zero One Zero One Zero One Zero Zero

w766a0abb :: W32
w766a0abb = W32 Zero One One One Zero One One Zero Zero One One Zero One Zero One Zero Zero Zero Zero Zero One Zero One Zero One Zero One One One Zero One One

w81c2c92e :: W32
w81c2c92e = W32 One Zero Zero Zero Zero Zero Zero One One One Zero Zero Zero Zero One Zero One One Zero Zero One Zero Zero One Zero Zero One Zero One One One Zero

w92722c85 :: W32
w92722c85 = W32 One Zero Zero One Zero Zero One Zero Zero One One One Zero Zero One Zero Zero Zero One Zero One One Zero Zero One Zero Zero Zero Zero One Zero One

wa2bfe8a1 :: W32
wa2bfe8a1 = W32 One Zero One Zero Zero Zero One Zero One Zero One One One One One One One One One Zero One Zero Zero Zero One Zero One Zero Zero Zero Zero One

wa81a664b :: W32
wa81a664b = W32 One Zero One Zero One Zero Zero Zero Zero Zero Zero One One Zero One Zero Zero One One Zero Zero One One Zero Zero One Zero Zero One Zero One One

wc24b8b70 :: W32
wc24b8b70 = W32 One One Zero Zero Zero Zero One Zero Zero One Zero Zero One Zero One One One Zero Zero Zero One Zero One One Zero One One One Zero Zero Zero Zero

wc76c51a3 :: W32
wc76c51a3 = W32 One One Zero Zero Zero One One One Zero One One Zero One One Zero Zero Zero One Zero One Zero Zero Zero One One Zero One Zero Zero Zero One One

wd192e819 :: W32
wd192e819 = W32 One One Zero One Zero Zero Zero One One Zero Zero One Zero Zero One Zero One One One Zero One Zero Zero Zero Zero Zero Zero One One Zero Zero One

wd6990624 :: W32
wd6990624 = W32 One One Zero One Zero One One Zero One Zero Zero One One Zero Zero One Zero Zero Zero Zero Zero One One Zero Zero Zero One Zero Zero One Zero Zero

wf40e3585 :: W32
wf40e3585 = W32 One One One One Zero One Zero Zero Zero Zero Zero Zero One One One Zero Zero Zero One One Zero One Zero One One Zero Zero Zero Zero One Zero One

w106aa070 :: W32
w106aa070 = W32 Zero Zero Zero One Zero Zero Zero Zero Zero One One Zero One Zero One Zero One Zero One Zero Zero Zero Zero Zero Zero One One One Zero Zero Zero Zero

w19a4c116 :: W32
w19a4c116 = W32 Zero Zero Zero One One Zero Zero One One Zero One Zero Zero One Zero Zero One One Zero Zero Zero Zero Zero One Zero Zero Zero One Zero One One Zero

w1e376c08 :: W32
w1e376c08 = W32 Zero Zero Zero One One One One Zero Zero Zero One One Zero One One One Zero One One Zero One One Zero Zero Zero Zero Zero Zero One Zero Zero Zero

w2748774c :: W32
w2748774c = W32 Zero Zero One Zero Zero One One One Zero One Zero Zero One Zero Zero Zero Zero One One One Zero One One One Zero One Zero Zero One One Zero Zero

w34b0bcb5 :: W32
w34b0bcb5 = W32 Zero Zero One One Zero One Zero Zero One Zero One One Zero Zero Zero Zero One Zero One One One One Zero Zero One Zero One One Zero One Zero One

w391c0cb3 :: W32
w391c0cb3 = W32 Zero Zero One One One Zero Zero One Zero Zero Zero One One One Zero Zero Zero Zero Zero Zero One One Zero Zero One Zero One One Zero Zero One One

w4ed8aa4a :: W32
w4ed8aa4a = W32 Zero One Zero Zero One One One Zero One One Zero One One Zero Zero Zero One Zero One Zero One Zero One Zero Zero One Zero Zero One Zero One Zero

w5b9cca4f :: W32
w5b9cca4f = W32 Zero One Zero One One Zero One One One Zero Zero One One One Zero Zero One One Zero Zero One Zero One Zero Zero One Zero Zero One One One One

w682e6ff3 :: W32
w682e6ff3 = W32 Zero One One Zero One Zero Zero Zero Zero Zero One Zero One One One Zero Zero One One Zero One One One One One One One One Zero Zero One One

w748f82ee :: W32
w748f82ee = W32 Zero One One One Zero One Zero Zero One Zero Zero Zero One One One One One Zero Zero Zero Zero Zero One Zero One One One Zero One One One Zero

w78a5636f :: W32
w78a5636f = W32 Zero One One One One Zero Zero Zero One Zero One Zero Zero One Zero One Zero One One Zero Zero Zero One One Zero One One Zero One One One One

w84c87814 :: W32
w84c87814 = W32 One Zero Zero Zero Zero One Zero Zero One One Zero Zero One Zero Zero Zero Zero One One One One Zero Zero Zero Zero Zero Zero One Zero One Zero Zero

w8cc70208 :: W32
w8cc70208 = W32 One Zero Zero Zero One One Zero Zero One One Zero Zero Zero One One One Zero Zero Zero Zero Zero Zero One Zero Zero Zero Zero Zero One Zero Zero Zero

w90befffa :: W32
w90befffa = W32 One Zero Zero One Zero Zero Zero Zero One Zero One One One One One Zero One One One One One One One One One One One One One Zero One Zero

wa4506ceb :: W32
wa4506ceb = W32 One Zero One Zero Zero One Zero Zero Zero One Zero One Zero Zero Zero Zero Zero One One Zero One One Zero Zero One One One Zero One Zero One One

wbef9a3f7 :: W32
wbef9a3f7 = W32 One Zero One One One One One Zero One One One One One Zero Zero One One Zero One Zero Zero Zero One One One One One One Zero One One One

wc67178f2 :: W32
wc67178f2 = W32 One One Zero Zero Zero One One Zero Zero One One One Zero Zero Zero One Zero One One One One Zero Zero Zero One One One One Zero Zero One Zero


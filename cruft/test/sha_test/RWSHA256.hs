import MetaprogrammingRW
import RWPrelude
---------------------------------------------
--- ReWire Fig Leaf
---------------------------------------------
{-
import Data.Bits
import Data.Word
import Data.Char
import Control.Monad.I
import Control.Monad.State
import Control.Monad.Resumption.Reactive

type ReT = ReacT
type StT = StateT
type I = I

extrude :: Monad m => ReT i o (StT s m) a -> s -> ReT i o m (a,s)
extrude = undefined

nativeVHDL :: String -> a -> a
nativeVHDL = flip const
-}
---------------------------------------------
--- End of Fig Leaf
---------------------------------------------

w32Plus,w32Xor,w32And :: W32 -> W32 -> W32
w32Plus = nativeVhdl "w32Plus" w32Plus
w32Xor  = nativeVhdl "w32Xor" w32Xor
w32And  = nativeVhdl "w32And" w32And

w32Not :: W32 -> W32
w32Not = nativeVhdl "w32Not" w32Not

data Oct a = Oct a a a a
                 a a a a

data Hex a = Hex a a a a
                 a a a a
                 a a a a
                 a a a a

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

data Inp = Init (Hex W32) | Cont (Hex W32) | DigestQ
data Out = DigestR (Oct W32) | Nix

start :: ReT Inp Out I ((((), Oct W32), Hex W32), Oct W32)
start = begin

begin :: ReT Inp Out I ((((), Oct W32), Hex W32), Oct W32)
begin = extrude
         (extrude
             (extrude devsha256 (Oct w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000))
             (Hex w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000))
         (Oct w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000 w00000000)

devsha256 :: ReT Inp Out (StT (Oct W32) (StT (Hex W32) (StT (Oct W32) I))) ()
devsha256 = signal Nix >>= \ i -> dev i

dev :: Inp -> ReT Inp Out (StT (Oct W32) (StT (Hex W32) (StT (Oct W32) I))) ()
dev (Init hw32) = do
                    lift (do
                            lift (lift (put initialSHA256State))
                            hi_1 <- lift (lift get)
                            put hi_1
                            lift (put hw32)
                            genhash
                            intermediate)
                    i <- signal Nix
                    dev i
dev (Cont hw32) = do
                    lift (do
                            hi_1 <- lift (lift get)
                            put hi_1
                            lift (put hw32)
                            genhash
                            intermediate)
                    i <- signal Nix
                    dev i
dev DigestQ     = do
                    h_n <- lift (lift (lift get))
                    i   <- signal (DigestR h_n)
                    dev i

intermediate  ::
     StT
       (Oct W32)
       (StT (Hex W32) (StT (Oct W32) I))
       ()
{-# INLINE intermediate #-}
-- FIXME? irrefutable
intermediate = do
  Oct h1 h2 h3 h4 h5 h6 h7 h8 <- lift (lift get)
  Oct a b c d e f g h         <- get
  lift (lift (put (Oct (w32Plus a h1) (w32Plus b h2) (w32Plus c h3) (w32Plus d h4) (w32Plus e h5) (w32Plus f h6) (w32Plus g h7) (w32Plus h h8))))


genhash
  :: StT
       (Oct W32)
       (StT (Hex W32) (StT (Oct W32) I))
       ()
{-# INLINE genhash #-}
genhash = do
  sched >>= compress w428a2f98
  sched >>= compress w71374491
  sched >>= compress wb5c0fbcf
  sched >>= compress we9b5dba5
  sched >>= compress w3956c25b
  sched >>= compress w59f111f1
  sched >>= compress w923f82a4
  sched >>= compress wab1c5ed5
  sched >>= compress wd807aa98
  sched >>= compress w12835b01
  sched >>= compress w243185be
  sched >>= compress w550c7dc3
  sched >>= compress w72be5d74
  sched >>= compress w80deb1fe
  sched >>= compress w9bdc06a7
  sched >>= compress wc19bf174
  sched >>= compress we49b69c1
  sched >>= compress wefbe4786
  sched >>= compress w0fc19dc6
  sched >>= compress w240ca1cc
  sched >>= compress w2de92c6f
  sched >>= compress w4a7484aa
  sched >>= compress w5cb0a9dc
  sched >>= compress w76f988da
  sched >>= compress w983e5152
  sched >>= compress wa831c66d
  sched >>= compress wb00327c8
  sched >>= compress wbf597fc7
  sched >>= compress wc6e00bf3
  sched >>= compress wd5a79147
  sched >>= compress w06ca6351
  sched >>= compress w14292967
  sched >>= compress w27b70a85
  sched >>= compress w2e1b2138
  sched >>= compress w4d2c6dfc
  sched >>= compress w53380d13
  sched >>= compress w650a7354
  sched >>= compress w766a0abb
  sched >>= compress w81c2c92e
  sched >>= compress w92722c85
  sched >>= compress wa2bfe8a1
  sched >>= compress wa81a664b
  sched >>= compress wc24b8b70
  sched >>= compress wc76c51a3
  sched >>= compress wd192e819
  sched >>= compress wd6990624
  sched >>= compress wf40e3585
  sched >>= compress w106aa070
  sched >>= compress w19a4c116
  sched >>= compress w1e376c08
  sched >>= compress w2748774c
  sched >>= compress w34b0bcb5
  sched >>= compress w391c0cb3
  sched >>= compress w4ed8aa4a
  sched >>= compress w5b9cca4f
  sched >>= compress w682e6ff3
  sched >>= compress w748f82ee
  sched >>= compress w78a5636f
  sched >>= compress w84c87814
  sched >>= compress w8cc70208
  sched >>= compress w90befffa
  sched >>= compress wa4506ceb
  sched >>= compress wbef9a3f7
  sched >>= compress wc67178f2

sched
  :: StT
       (Oct W32)
       (StT (Hex W32) (StT (Oct W32) I))
       W32
{-# INLINE sched #-}
-- FIXME? Irrefutable pattern; @ pattern
sched = lift (get >>= \ s@(Hex w00 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) ->
              put (updateSched s) >>= \ blah ->
              return w00)

updateSched :: Hex W32 -> Hex W32
updateSched (Hex w00 w01 w02 w03 w04 w05 w06 w07 w08 w09 w10 w11 w12 w13 w14 w15) =
            (Hex w01 w02 w03 w04 w05 w06 w07 w08 w09 w10 w11 w12 w13 w14 w15 w16)
  where
    w16 :: W32
    w16 = w32Plus (w32Plus (sigma1 w14) w09) (w32Plus (sigma0 w01) w00)

compress :: W32 -> W32 -> StT
                            (Oct W32)
                            (StT (Hex W32) (StT (Oct W32) I))
                            ()
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


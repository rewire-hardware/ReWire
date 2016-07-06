module SecondImplementation where

import Data.Bits
import Data.Word
import Data.Char
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption.Reactive
import Counter
import Globals256
import Reference256

-------------------------------------------
--- Rapid prototype of SHA256 in Hardware
-------------------------------------------

load0 :: a -> a -> Hex a -> Hex a
load0 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex w1 w2 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf

load1 :: a -> a -> Hex a -> Hex a
load1 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 w1 w2 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf

load2 :: a -> a -> Hex a -> Hex a
load2 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 w1 w2 x6 x7 x8 x9 xa xb xc xd xe xf

load3 :: a -> a -> Hex a -> Hex a
load3 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 x4 x5 w1 w2 x8 x9 xa xb xc xd xe xf

load4 :: a -> a -> Hex a -> Hex a
load4 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 x4 x5 x6 x7 w1 w2 xa xb xc xd xe xf

load5 :: a -> a -> Hex a -> Hex a
load5 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 w1 w2 xc xd xe xf
    
load6 :: a -> a -> Hex a -> Hex a
load6 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb w1 w2 xe xf

load7 :: a -> a -> Hex a -> Hex a
load7 w1 w2 (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
  = Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd w1 w2

data Inp = Init Word32 Word32  |
           Load0 Word32 Word32 | 
           Load1 Word32 Word32 | 
           Load2 Word32 Word32 | 
           Load3 Word32 Word32 | 
           Load4 Word32 Word32 | 
           Load5 Word32 Word32 | 
           Load6 Word32 Word32 | 
           Load7 Word32 Word32 |
           DigestQ0            |
           DigestQ1            |
           DigestQ2            |
           DigestQ3
              deriving Show
data Out = DigestR Word32 Word32 | Nix deriving Show

type Dev i o   = ReacT i o M

digest0, digest1, digest2, digest3 :: Oct Word32 -> Out
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

devsha256' :: Dev Inp Out ()
devsha256' = signal Nix >>= dev

dev :: Inp -> Dev Inp Out ()
dev (Init w1 w2) = do
                      lift (do
                               putDigest initialSHA256State
                               hw <- getBlock
                               putBlock (load0 w1 w2 hw))
                      i <- signal Nix
                      dev i
dev (Load0 w1 w2) = do
                       lift (do
                                hw <- getBlock
                                putBlock (load0 w1 w2 hw))
                       i <- signal Nix
                       dev i
dev (Load1 w1 w2) = do
                       lift (do
                                hw <- getBlock
                                putBlock (load1 w1 w2 hw))
                       i <- signal Nix
                       dev i
dev (Load2 w1 w2) = do
                       lift (do
                                hw <- getBlock
                                putBlock (load2 w1 w2 hw))
                       i <- signal Nix
                       dev i
dev (Load3 w1 w2) = do
                       lift (do
                                hw <- getBlock
                                putBlock (load3 w1 w2 hw))
                       i <- signal Nix
                       dev i
dev (Load4 w1 w2) = do
                       lift (do
                                hw <- getBlock
                                putBlock (load4 w1 w2 hw))
                       i <- signal Nix
                       dev i
dev (Load5 w1 w2) = do
                       lift (do
                                hw <- getBlock
                                putBlock (load5 w1 w2 hw))
                       i <- signal Nix
                       dev i
dev (Load6 w1 w2) = do
                       lift (do
                                hw <- getBlock
                                putBlock (load6 w1 w2 hw))
                       i <- signal Nix
                       dev i
dev (Load7 w1 w2) = do
                       lift (do
                                putCtr C0
                                hi_1 <- getDigest
                                putIntDig hi_1
                                hw <- getBlock
                                putBlock (load7 w1 w2 hw))
                       signal Nix
                       loop
dev DigestQ0      = do
                       h_n <- lift getDigest
                       i <- signal (digest0 h_n)
                       dev i
dev DigestQ1      = do
                       h_n <- lift getDigest
                       i <- signal (digest1 h_n)
                       dev i
dev DigestQ2      = do
                       h_n <- lift getDigest
                       i <- signal (digest2 h_n)
                       dev i
dev DigestQ3      = do
                       h_n <- lift getDigest
                       i <- signal (digest3 h_n)
                       dev i

genhash' :: M Ctr
genhash' = do
             ctr <- (lift (lift (lift get)))
             sched >>= compress (seed ctr)
             lift (lift (lift (put (incCtr ctr))))
             return ctr

loop :: Dev Inp Out ()
loop   = do
            ctr <- lift genhash'
            i <- signal Nix
            case ctr of
                 C63 -> lift intermediate >> dev i
                 _   -> loop

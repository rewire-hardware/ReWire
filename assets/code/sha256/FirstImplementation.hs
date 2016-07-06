module FirstImplementation where

import Data.Bits
import Data.Word
import Data.Char
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption.Reactive
import Counter
import Globals256
import Reference256 hiding (innerloop)

-------------------------------------------
--- Rapid prototype of SHA256 in Hardware
-------------------------------------------

data Inp = Init (Hex Word32) | Load (Hex Word32) | DigestQ deriving Show
data Out = DigestR (Oct Word32) | Nix deriving Show

type Dev i o   = ReacT i o M

-------------------------------------------------------------------------------------------
--- SHA256 in ReWire. Note that the code from the reference semantics is
--- "cut and pasted" directly into the definition of dev below. In particular,
--- the highlighted lines are copied and lifted into ReacT. Calling this device
--- with the correctly formatted I-signals is an "unrolling" of the reference
--- semantics which can, I believe, be captured equationally. Compare the two
--- "go" functions in TestingSHA256.hs.
-------------------------------------------------------------------------------------------

devsha256 :: Dev Inp Out ()
devsha256 = signal Nix >>= dev

dev :: Inp -> Dev Inp Out ()
dev (Init hw32) = do
                     lift (do
                              putDigest initialSHA256State
                              hi_1 <- getDigest
                              putIntDig hi_1
                              putBlock hw32
                              putCtr C0)
                     signal Nix
                     innerloop 
dev (Load hw32) = do
                     lift (do
                              hi_1 <- getDigest
                              putIntDig hi_1
                              putBlock hw32
                              putCtr C0)
                     signal Nix
                     innerloop
dev DigestQ     = do
                     h_n <- lift getDigest 
                     i <- signal (DigestR h_n)
                     dev i

innerloop :: Dev Inp Out ()
innerloop   = do
                 ctr <- lift (do
                                 c <- getCtr
                                 s <- sched
                                 compress (seed c) s
                                 putCtr (incCtr c)
                                 return c)
                 i <- signal Nix
                 case ctr of
                      C63 -> lift intermediate >> dev i
                      _   -> innerloop

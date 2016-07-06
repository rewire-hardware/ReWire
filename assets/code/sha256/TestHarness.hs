module TestHarness where

import Data.Bits
import Data.Word
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption.Reactive
import Globals256
import FirstImplementation
import SecondImplementation
import Reference256 

--
-- These three examples are directly from the NIST document defining SHA256
--

--
-- example 1
--
msg1    = "abc"

padded1 :: [Hex Word32]
padded1 = [Hex 0x61626380 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000
               0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000018]

hashed1 = Oct 0xba7816bf 0x8f01cfea 0x414140de 0x5dae2223 0xb00361a3 0x96177a9c 0xb410ff61 0xf20015ad

--
-- example 2
--

msg2    = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"

padded2 :: [Hex Word32]
padded2 = [Hex 0x61626364 0x62636465 0x63646566 0x64656667 0x65666768 0x66676869 0x6768696a 0x68696a6b
               0x696a6b6c 0x6a6b6c6d 0x6b6c6d6e 0x6c6d6e6f 0x6d6e6f70 0x6e6f7071 0x80000000 0x00000000,
           Hex 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000
               0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x000001c0]

hashed2 = Oct 0x248d6a61 0xd20638b8 0xe5c02693 0x0c3e6039 0xa33ce459 0x64ff2167 0xf6ecedd4 0x19db06c1

--
-- example 3
--

msg3 = take 1000000 (repeat 'a')

hashed3 = Oct 0xcdc76e5c 0x9914fb92 0x81a1c7e2 0x84d73e67 0xf1809a48 0xa497200e 0x046d39cc 0xc7112cd0




runM :: StateT b (StateT b1 (StateT b2 Identity)) a -> a
runM phi =  fst $
            fst $
            fst $
            runIdentity $
            runStateT (runStateT (runStateT phi undefined) undefined) undefined

runM'
  :: StateT
       b (StateT (Hex Word32) (StateT b1 (StateT b2 Identity))) a
     -> a
runM' phi =  fst $
             fst $
             fst $
             fst $
             runIdentity $
             runStateT (runStateT (runStateT (runStateT phi undefined1) undefined2) undefined3) undefined4
     where
       undefined1 = error "one"
       undefined2 :: Hex Word32
       undefined2 = Hex 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 -- error "two"
       undefined3 = error "three"
       undefined4 = error "four"

simdev :: Monad m => ReacT t b m t1 -> [t] -> m b
simdev (ReacT dev) []     = do Right (o,_) <- dev
                               return o 
simdev (ReacT dev) (i:is) = do
                               Right (o,k) <- dev 
                               simdev (k i) is 
          
simdev' :: Monad m => ReacT t b m t1 -> [b] -> [t] -> m [b]
simdev' (ReacT dev) os []     = do
                                   Right (o,_) <- dev
                                   return (os ++ [o]) 
simdev' (ReacT dev) os (i:is) = do
                                   Right (o,k) <- dev 
                                   simdev' (k i) (os ++ [o]) is 


----------------------------------------------
--- Testing functions for the Reference Semantics for SHA256
----------------------------------------------
refsha256 :: String -> Oct Word32
refsha256 = runM' . sha256 . pad

-----------------------------------
-- First Implementation
-----------------------------------

godev256 :: String -> [FirstImplementation.Out]
godev256 = runM' . simdev' devsha256 [] . format' . pad

godev256' :: String -> FirstImplementation.Out
godev256' = runM' . simdev devsha256 . format' . pad


format' :: [Hex Word32] -> [FirstImplementation.Inp]
format' (hw32 : hw32s) = (FirstImplementation.Init hw32 : padding) ++ doit hw32s ++ [DigestQ]
    where doit []           = []
          doit (hw32:hw32s) = (Load hw32 : padding) ++ doit hw32s
          padding = take 64 $ repeat DigestQ -- these are ignored


-----------------------------------
-- Second Implementation
-----------------------------------
godev256'' = runM' . simdev' devsha256' [] . format'' . pad

format'' :: [Hex Word32] -> [SecondImplementation.Inp]
format'' (hw32 : hw32s) = initialize hw32 ++ padding ++ doit hw32s ++ [DigestQ0,DigestQ1,DigestQ2,DigestQ3]
    where doit []           = []
          doit (hw32:hw32s) = continue hw32 ++ padding ++ doit hw32s
          padding = take 64 $ repeat DigestQ3 -- these are ignored

initialize (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
    = [SecondImplementation.Init x0 x1, Load1 x2 x3, Load2 x4 x5, Load3 x6 x7,
       Load4 x8 x9, Load5 xa xb, Load6 xc xd, Load7 xe xf]

continue (Hex x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf)
    = [Load0 x0 x1, Load1 x2 x3, Load2 x4 x5, Load3 x6 x7,
       Load4 x8 x9, Load5 xa xb, Load6 xc xd, Load7 xe xf]



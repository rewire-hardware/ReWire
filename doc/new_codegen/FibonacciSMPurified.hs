{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module FibonacciSMPurified where

---------------------------------------------
--- Start: ReWire Fig Leaf
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

unfold :: (b -> i -> Either a (o,b)) -> Either a (o,b) -> ReT i o I a
unfold f (Left a)      = ReacT $ return (Left a)
unfold f (Right (o,b)) = ReacT $ return (Right (o, unfold f . f b))

extrude :: Monad m => ReT i o (StT s m) a -> s -> ReT i o m (a,s)
extrude = undefined

nativeVhdl :: String -> a -> a
nativeVhdl = undefined

---------------------------------------------
--- End: ReWire Fig Leaf
---------------------------------------------
-- State-monad version of Fibonacci

--
-- The compiler doesn't yet support a "prelude" so we will have to define a
-- few things ourselves!
--
data Bit        = Zero | One
data W8         = W8 Bit Bit Bit Bit Bit Bit Bit Bit

plusW8 :: W8 -> W8 -> W8
{-# INLINE plusW8 #-}
plusW8 = nativeVhdl "plusW8" plusW8

zeroW8 :: W8
zeroW8 = W8 Zero Zero Zero Zero Zero Zero Zero Zero

oneW8 :: W8
oneW8 = W8 Zero Zero Zero Zero Zero Zero Zero One

--
-- End stuff that will eventually be in the prelude.
--

start_pure :: Either () (W8,R)
start_pure = loop_pure zeroW8 oneW8

getN_pure :: W8 -> W8 -> (W8,(W8,W8))
getN_pure s1 s2 = (s1,(s1,s2))

putN_pure :: W8 -> W8 -> W8 -> ((),(W8,W8))
putN_pure x s1 s2 = ((),(x,s2))

getM_pure :: W8 -> W8 -> (W8,(W8,W8))
getM_pure s1 s2 = (s2,(s1,s2))

putM_pure :: W8 -> W8 -> W8 -> ((),(W8,W8))
putM_pure x s1 s2 = ((),(s1,x))

upd_pure :: Bit -> W8 -> W8 -> ((),(W8,W8))
upd_pure One  s1 s2 = ((),(s1,s2))
upd_pure Zero s1 s2 = let (n,(s1_,s2_)) = getN_pure s1 s2
                          (m,(s1__,s2__)) = getM_pure s1_ s2_
                          (_,(s1____,s2____)) = putN_pure m s1__ s2__
                      in putM_pure (plusW8 n m) s1____ s2____

loop_pure :: W8 -> W8 -> Either () (W8,R)
loop_pure s1 s2 = let (n,(s1,s2)) = getN_pure s1 s2
                  in  (Right (n,R_k s1 s2))

k_pure :: Bit -> W8 -> W8 -> Either () (W8,R)
k_pure b s1 s2 = let (_,(s1,s2)) = upd_pure b s1 s2
                 in  loop_pure s1 s2

data R = R_k W8 W8

dispatch :: R -> Bit -> Either () (W8,R)
dispatch (R_k n m) i = k_pure i n m

start :: ReT Bit W8 I ()
start = unfold dispatch start_pure

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module OneState where

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
    
---------------------------------------------
--- End: ReWire Fig Leaf
---------------------------------------------

data Bit :: * where
    Zero :: Bit

data W8 :: * where
    W8 :: Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> W8

lambda6 :: W8 -> StT W8 I ()
lambda6 x = put x

lambda5 :: Bit -> ReT Bit W8 (StT W8 I) ()
lambda5 d = incr1

lambda4 :: W8 -> ReT Bit W8 (StT W8 I) ()
lambda4 x =  signal x >>= lambda5

lambda3 :: () -> ReT Bit W8 (StT W8 I) ()
lambda3 d = lift get >>= lambda4 

grunt2 :: StT W8 I ()
grunt2 = get >>= lambda6

incr1 :: ReT Bit W8 (StT W8 I) ()
incr1 = lift grunt2 >>= lambda3

start :: ReT Bit W8 I ((() , W8))
start =
    ((extrude incr1)
         ((((((((W8 Zero) Zero) Zero) Zero)
                 Zero)
                Zero)
               Zero)
              Zero))

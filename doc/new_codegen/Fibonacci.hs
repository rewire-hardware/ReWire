{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Fibonacci where

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
--module Fibonacci where

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

nativeVhdl = undefined
    
---------------------------------------------
--- End: ReWire Fig Leaf
---------------------------------------------

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

start :: ReT Bit W8 I ()
start = begin

begin :: ReT Bit W8 I ()
begin = loop zeroW8 oneW8

loop :: W8 -> W8 -> ReT Bit W8 I ()
loop n m = do b <- signal n
              case b of
                  One  -> loop n m
                  Zero -> loop m (plusW8 n m)

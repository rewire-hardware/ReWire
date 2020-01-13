{-
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
-}

data Bit = Zero | One
data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit

{-
grunt :: StT W8 I ()
grunt = do
  x <- get
  put x
-}

incr :: ReT Bit W8 (StT W8 I) ()
incr = do
--  lift grunt
  x <- lift get
  signal x
  incr

start :: ReT Bit W8 I ((),W8)
start = extrude incr (W8 Zero Zero Zero Zero Zero Zero Zero Zero)

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Zero where

---------------------------------------------
--- Start: ReWire Fig Leaf
---------------------------------------------

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption.Reactive

type ReT = ReacT
type StT = StateT
type I   = Identity

---------------------------------------------
--- End: ReWire Fig Leaf
---------------------------------------------

data Bit :: * where
     Zero :: Bit

lambda1 :: Bit -> ReT Bit Bit I ()
lambda1 d = start

start :: ReT Bit Bit I ()
start = signal Zero >>= lambda1

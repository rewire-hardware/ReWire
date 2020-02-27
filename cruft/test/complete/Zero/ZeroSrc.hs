{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Zero where

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

---------------------------------------------
--- End: ReWire Fig Leaf
---------------------------------------------

data Bit = Zero | One

start :: ReT Bit Bit I ()
start = do
          signal Zero
          start 

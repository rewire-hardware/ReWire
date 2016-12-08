{-
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
type I = Identity

unfold :: (b -> i -> Either a (o,b)) -> Either a (o,b) -> ReT i o I a
unfold f (Left a)      = ReacT $ return (Left a)
unfold f (Right (o,b)) = ReacT $ return (Right (o, unfold f . f b))
    
---------------------------------------------
--- End: ReWire Fig Leaf
---------------------------------------------
-}

data Bit = Zero | One

start :: ReT Bit Bit I ()
start = do
          signal Zero
          start 

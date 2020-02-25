{-
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module UpCounter where

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

--data Bit = Zero | One
--data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit

unfold  = undefined
extrude = undefined
nativeVhdl = undefined
---------------------------------------------
--- End: ReWire Fig Leaf
---------------------------------------------
-}

data Bit = Zero | One

data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit

plusOne :: W8 -> W8
{-# INLINE plusOne #-}
plusOne = nativeVhdl "prim_plusOne" undefined

rotl :: W8 -> W8
{-# INLINE rotl #-}
rotl = nativeVhdl "prim_rotl" undefined

tick :: ReT Bit W8 (StT W8 I) Bit
{-# INLINE tick #-}
tick = lift get >>= \ x -> signal x

main :: ReT Bit W8 (StT W8 I) ()
main = do
      b <- tick
      case b of
            One -> lift get >>= \n -> lift (put (plusOne n))
            Zero -> lift get >>= \n -> lift (put (rotl n))
      main

start :: ReT Bit W8 I ()
start = extrude main (W8 Zero Zero Zero Zero Zero Zero Zero Zero)

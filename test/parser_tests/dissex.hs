module Dissex where

data Bit = Zero | One
data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit
data Tuple2 a b = Tuple2 a b
data Unit = Unit

undefined :: a
undefined = undefined

plus :: W8 -> W8 -> W8
{-# INLINE plus #-}
plus = nativeVhdl "plus" undefined

sig :: ReT Bit W8 (StT W8 (StT W8 I)) ()
sig = do
      r0 <- lift get
      i <- signal r0
      case i of
            Zero -> sig
            One  -> incr

incr :: ReT Bit W8 (StT W8 (StT W8 I)) ()
incr = do
      r0 <- lift get
      r1 <- lift (lift get)
      lift (put r1)
      lift (lift (put (plus r0 r1)))
      sig

start :: ReT Bit W8 I (((),W8),W8)
start = extrude (extrude sig (W8 Zero Zero Zero Zero Zero Zero Zero Zero)) (W8 Zero Zero Zero Zero Zero Zero Zero One)

module ToAGs where

data Bit = Zero | One

undefined :: a
undefined = undefined

bitAnd :: Bit -> Bit -> Bit
{-# INLINE bitAnd #-}
bitAnd = nativeVhdl "prim_bitAnd" undefined

bitNot :: Bit -> Bit
{-# INLINE bitNot #-}
bitNot = nativeVhdl "prim_bitNot" undefined

go :: ReT Bit Bit (StT Bit (StT Bit I)) Bit
go = do
      x <- signal Zero
      y <- case x of { One  -> signal Zero ; Zero -> lift get }
      z <- signal (bitAnd (case y of { One -> x ; Zero -> bitAnd x x }) y)
      lift (lift (put y))
      go

start :: ReT Bit Bit I ((Bit,Bit),Bit)
start = extrude (extrude go Zero) One

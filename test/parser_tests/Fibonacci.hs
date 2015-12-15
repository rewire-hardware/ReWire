-- File: Fibonacci.rw

--
-- The compiler doesn't yet support a "prelude" so we will have to define a
-- few things ourselves!
--
data Bit        = Zero | One
data W8         = W8 Bit Bit Bit Bit Bit Bit Bit Bit
data Unit       = Unit
data Tuple2 a b = Tuple2 a b

plusW8 :: W8 -> W8 -> W8
{-# INLINE plusW8 #-}
plusW8 x y = nativeVhdl "plusW8" plusW8 x y

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

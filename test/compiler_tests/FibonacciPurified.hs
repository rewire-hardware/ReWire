data Bit = Zero | One deriving Show
data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit

plusW8 :: W8 -> W8 -> W8
{-# INLINE plusW8 #-}
plusW8 = nativeVhdl "plusW8" plusW8

zeroW8 :: W8
zeroW8 = W8 Zero Zero Zero Zero Zero Zero Zero Zero

oneW8 :: W8
oneW8 = W8 Zero Zero Zero Zero Zero Zero Zero One

data R = R_k W8 W8

start_pure :: Either () (W8,R)
start_pure = begin_pure

begin_pure :: Either () (W8,R)
begin_pure = loop_pure zeroW8 oneW8

loop_pure :: W8 -> W8 -> Either () (W8,R)
loop_pure n m = (Right (n,R_k n m))

k_pure :: W8 -> W8 -> Bit -> Either () (W8,R)
k_pure n m b = case b of
                 One  -> loop_pure n m
                 Zero -> loop_pure m (plusW8 n m)

dispatch :: R -> Bit -> Either () (W8,R)
dispatch (R_k n m) i = k_pure n m i

start :: ReT Bit W8 I ()
start = unfold dispatch start_pure

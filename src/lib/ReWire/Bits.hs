module ReWire.Bits where

data Bit = Zero | One
data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit

bitAnd :: Bit -> Bit -> Bit
bitAnd One One = One
bitAnd _   _   = Zero

bitNot :: Bit -> Bit
bitNot Zero = One
bitNot One  = Zero

plusOne :: W8 -> W8
plusOne (W8 a    b    c    d    e    f    g    Zero)= W8 a    b    c    d    e    f    g    One
plusOne (W8 a    b    c    d    e    f    Zero One) = W8 a    b    c    d    e    f    One  Zero
plusOne (W8 a    b    c    d    e    Zero One  One) = W8 a    b    c    d    e    One  Zero Zero
plusOne (W8 a    b    c    d    Zero One  One  One) = W8 a    b    c    d    One  Zero Zero Zero
plusOne (W8 a    b    c    Zero One  One  One  One) = W8 a    b    c    One  Zero Zero Zero Zero
plusOne (W8 a    b    Zero One  One  One  One  One) = W8 a    b    One  Zero Zero Zero Zero Zero
plusOne (W8 a    Zero One  One  One  One  One  One) = W8 a    One  Zero Zero Zero Zero Zero Zero
plusOne (W8 Zero One  One  One  One  One  One  One) = W8 One  Zero Zero Zero Zero Zero Zero Zero
plusOne (W8 One  One  One  One  One  One  One  One) = W8 Zero Zero Zero Zero Zero Zero Zero Zero

rotl :: W8 -> W8
rotl (W8 a b c d e f g h) = W8 b c d e f g h a


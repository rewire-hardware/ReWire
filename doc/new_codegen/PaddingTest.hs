data Bit = Zero | One
data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit
data W4 = W4 Bit Bit Bit Bit

w4_0 :: W4
w4_0 = W4 Zero Zero Zero Zero

w4_1 :: W4
w4_1 = W4 Zero Zero Zero One

w8_0 :: W8
w8_0 = W8 Zero Zero Zero Zero Zero Zero Zero Zero

w8_1 :: W8
w8_1 = W8 Zero Zero Zero Zero Zero Zero Zero One

start :: (Either W8 W4,Either W8 W4,Either W4 W8,Either W4 W8)
start = (Left w8_1,Right w4_1,Left w4_0,Right w8_0)

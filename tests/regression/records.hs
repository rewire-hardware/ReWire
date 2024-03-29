import ReWire
import ReWire.Bits

data S = S1 { f1, f2 :: Bit, f3 :: (Bit, Bit) } | S2 { f4 :: (Bit, Bit, Bit) }

x :: S -> Bit
x d = f1 d

y :: S -> Bit
y d = f2 d

z :: S -> (Bit, Bit)
z d = f3 d

w :: S -> (Bit, Bit, Bit)
w d = f4 d

foo :: S -> S
foo (S1 { f1 = a, f2 = b, f3 = c }) = S1 { f1 = b, f2 = a, f3 = c }
foo (S2 { f4 = a }) = S2 { f4 = a }

bar :: S -> S
bar s@(S1 {}) = s { f1 = zero, f2 = one, f3 = (zero, one) }
bar s@(S2 {}) = s { f4 = (zero, one, zero) }

start :: ReacT Bit Bit Identity ()
start = do
  signal $ x $ foo $ bar (S1 zero zero (zero, zero))
  start

main = undefined

module Boilerplate where

import Control.Monad.State hiding (when)
import Control.Monad.Identity hiding (when)

-- begin primitive boilerplate
data Bit = Zero | One deriving Show
notBit One  = Zero
notBit Zero = One
eqBit One  One  = One
eqBit Zero Zero = One
eqBit _    _    = Zero
andBit One b  = b
andBit Zero _ = Zero
orBit Zero b = b
orBit One _  = One
xorBit One b  = notBit b
xorBit Zero b = b
plusCBit Zero Zero Zero = (Zero,Zero)
plusCBit Zero Zero  One = (Zero, One)
plusCBit Zero  One Zero = (Zero, One)
plusCBit Zero  One  One = ( One,Zero)
plusCBit  One Zero Zero = (Zero, One)
plusCBit  One Zero  One = ( One,Zero)
plusCBit  One  One Zero = ( One,Zero)
plusCBit  One  One  One = ( One, One)
minusCBit  Zero Zero Zero = (Zero,Zero)
minusCBit  Zero Zero  One = ( One, One)
minusCBit  Zero  One Zero = ( One, One)
minusCBit  Zero  One  One = ( One,Zero)
minusCBit   One Zero Zero = (Zero, One)
minusCBit   One Zero  One = (Zero,Zero)
minusCBit   One  One Zero = (Zero,Zero)
minusCBit   One  One  One = ( One,Zero)

data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit deriving Show
zeroW8 = W8 Zero Zero Zero Zero Zero Zero Zero Zero
oneW8  = W8 Zero Zero Zero Zero Zero Zero Zero  One
notW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 (notBit b0) (notBit b1) (notBit b2) (notBit b3) (notBit b4) (notBit b5) (notBit b6) (notBit b7)
andW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (andBit b0 c0) (andBit b1 c1) (andBit b2 c2) (andBit b3 c3) (andBit b4 c4) (andBit b5 c5) (andBit b6 c6) (andBit b7 c7)


xorW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (xorBit b0 c0) (xorBit b1 c1) (xorBit b2 c2) (xorBit b3 c3) (xorBit b4 c4) (xorBit b5 c5) (xorBit b6 c6) (xorBit b7 c7)
eqW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = andBit (andBit (andBit (eqBit b0 c0) (eqBit b1 c1)) (andBit (eqBit b2 c2) (eqBit b3 c3))) (andBit (andBit (eqBit b4 c4) (eqBit b5 c5)) (andBit (eqBit b6 c6) (eqBit b7 c7)))
rolW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 b1 b2 b3 b4 b5 b6 b7 b0
rorW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 b7 b0 b1 b2 b3 b4 b5 b6

plusCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) ci = (co0,W8 d0 d1 d2 d3 d4 d5 d6 d7)
 where (co0,d0) = plusCBit b0 c0 co1
       (co1,d1) = plusCBit b1 c1 co2
       (co2,d2) = plusCBit b2 c2 co3
       (co3,d3) = plusCBit b3 c3 co4
       (co4,d4) = plusCBit b4 c4 co5
       (co5,d5) = plusCBit b5 c5 co6
       (co6,d6) = plusCBit b6 c6 co7
       (co7,d7) = plusCBit b7 c7 ci

data W32  = W32 Bit Bit Bit Bit Bit Bit Bit Bit 
                Bit Bit Bit Bit Bit Bit Bit Bit 
                Bit Bit Bit Bit Bit Bit Bit Bit 
                Bit Bit Bit Bit Bit Bit Bit Bit 
              deriving Show

--
-- 2's complement or unsigned arithmetic. I haven't thought about this bullshit since
-- 1989. Does it make a difference with addition here? It doesn't seem too.
-- But it does bear making sure that I'm not building nonsense.
--
plusW32  (W32  b0  b1  b2  b3  b4  b5  b6  b7  b8  b9 b10 b11 b12 b13 b14 b15 
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31) 
         (W32  c0  c1  c2  c3  c4  c5  c6  c7  c8  c9 c10 c11 c12 c13 c14 c15 
              c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31) 
         ci 
         = (W32  d0  d1  d2  d3  d4  d5  d6  d7  d8  d9 d10 d11 d12 d13 d14 d15 
                d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31) 
 where 
       (co0,d0)   = plusCBit b0  c0  co1
       (co1,d1)   = plusCBit b1  c1  co2
       (co2,d2)   = plusCBit b2  c2  co3
       (co3,d3)   = plusCBit b3  c3  co4
       (co4,d4)   = plusCBit b4  c4  co5
       (co5,d5)   = plusCBit b5  c5  co6
       (co6,d6)   = plusCBit b6  c6  co7
       (co7,d7)   = plusCBit b7  c7  co8
       (co8,d8)   = plusCBit b8  c8  co9
       (co9,d9)   = plusCBit b9  c9  co10
       (co10,d10) = plusCBit b10 c10 co11
       (co11,d11) = plusCBit b11 c11 co12
       (co12,d12) = plusCBit b12 c12 co13
       (co13,d13) = plusCBit b13 c13 co14
       (co14,d14) = plusCBit b14 c14 co15
       (co15,d15) = plusCBit b15 c15 co16
       (co16,d16) = plusCBit b16 c16 co17
       (co17,d17) = plusCBit b17 c17 co18
       (co18,d18) = plusCBit b18 c18 co19
       (co19,d19) = plusCBit b19 c19 co20
       (co20,d20) = plusCBit b20 c20 co21
       (co21,d21) = plusCBit b21 c21 co22
       (co22,d22) = plusCBit b22 c22 co23
       (co23,d23) = plusCBit b23 c23 co24
       (co24,d24) = plusCBit b24 c24 co25
       (co25,d25) = plusCBit b25 c25 co26
       (co26,d26) = plusCBit b26 c26 co27
       (co27,d27) = plusCBit b27 c27 co28
       (co28,d28) = plusCBit b28 c28 co29
       (co29,d29) = plusCBit b29 c29 co30
       (co30,d30) = plusCBit b30 c30 co31
       (co31,d31) = plusCBit b31 c31 ci

plusCW32 (W32  b0  b1  b2  b3  b4  b5  b6  b7  b8  b9 b10 b11 b12 b13 b14 b15 
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31) 
         (W32  c0  c1  c2  c3  c4  c5  c6  c7  c8  c9 c10 c11 c12 c13 c14 c15 
              c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31) 
         ci 
         = (co0, W32  d0  d1  d2  d3  d4  d5  d6  d7  d8  d9 d10 d11 d12 d13 d14 d15 
                     d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31) 
 where 
       (co0,d0)   = plusCBit b0  c0  co1
       (co1,d1)   = plusCBit b1  c1  co2
       (co2,d2)   = plusCBit b2  c2  co3
       (co3,d3)   = plusCBit b3  c3  co4
       (co4,d4)   = plusCBit b4  c4  co5
       (co5,d5)   = plusCBit b5  c5  co6
       (co6,d6)   = plusCBit b6  c6  co7
       (co7,d7)   = plusCBit b7  c7  co8
       (co8,d8)   = plusCBit b8  c8  co9
       (co9,d9)   = plusCBit b9  c9  co10
       (co10,d10) = plusCBit b10 c10 co11
       (co11,d11) = plusCBit b11 c11 co12
       (co12,d12) = plusCBit b12 c12 co13
       (co13,d13) = plusCBit b13 c13 co14
       (co14,d14) = plusCBit b14 c14 co15
       (co15,d15) = plusCBit b15 c15 co16
       (co16,d16) = plusCBit b16 c16 co17
       (co17,d17) = plusCBit b17 c17 co18
       (co18,d18) = plusCBit b18 c18 co19
       (co19,d19) = plusCBit b19 c19 co20
       (co20,d20) = plusCBit b20 c20 co21
       (co21,d21) = plusCBit b21 c21 co22
       (co22,d22) = plusCBit b22 c22 co23
       (co23,d23) = plusCBit b23 c23 co24
       (co24,d24) = plusCBit b24 c24 co25
       (co25,d25) = plusCBit b25 c25 co26
       (co26,d26) = plusCBit b26 c26 co27
       (co27,d27) = plusCBit b27 c27 co28
       (co28,d28) = plusCBit b28 c28 co29
       (co29,d29) = plusCBit b29 c29 co30
       (co30,d30) = plusCBit b30 c30 co31
       (co31,d31) = plusCBit b31 c31 ci

orW32 (W32  b0  b1  b2  b3  b4  b5  b6  b7  b8  b9 b10 b11 b12 b13 b14 b15 
           b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31) 
      (W32  c0  c1  c2  c3  c4  c5  c6  c7  c8  c9 c10 c11 c12 c13 c14 c15 
           c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31) 
         = (W32  d0  d1  d2  d3  d4  d5  d6  d7  d8  d9 d10 d11 d12 d13 d14 d15 
                d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31) 
      where d0  = orBit b0  c0
            d1  = orBit b1  c1
            d2  = orBit b2  c2
            d3  = orBit b3  c3
            d4  = orBit b4  c4
            d5  = orBit b5  c5
            d6  = orBit b6  c6
            d7  = orBit b7  c7
            d8  = orBit b8  c8
            d9  = orBit b9  c9
            d10 = orBit b10 c10
            d11 = orBit b11 c11
            d12 = orBit b12 c12
            d13 = orBit b13 c13
            d14 = orBit b14 c14
            d15 = orBit b15 c15
            d16 = orBit b16 c16
            d17 = orBit b17 c17
            d18 = orBit b18 c18
            d19 = orBit b19 c19
            d20 = orBit b20 c20
            d21 = orBit b21 c21
            d22 = orBit b22 c22
            d23 = orBit b23 c23
            d24 = orBit b24 c24
            d25 = orBit b25 c25
            d26 = orBit b26 c26
            d27 = orBit b27 c27
            d28 = orBit b28 c28
            d29 = orBit b29 c29
            d30 = orBit b30 c30
            d31 = orBit b31 c31

         
plusW8 a b = plusCW8 a b Zero
negW8 w = snd $ plusW8 (notW8 w) oneW8
minusCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) ci = (co0,W8 d0 d1 d2 d3 d4 d5 d6 d7)
 where (co0,d0) = minusCBit b0 c0 co1
       (co1,d1) = minusCBit b1 c1 co2
       (co2,d2) = minusCBit b2 c2 co3
       (co3,d3) = minusCBit b3 c3 co4
       (co4,d4) = minusCBit b4 c4 co5
       (co5,d5) = minusCBit b5 c5 co6
       (co6,d6) = minusCBit b6 c6 co7
       (co7,d7) = minusCBit b7 c7 ci
shlCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) ci = (b0,W8 b1 b2 b3 b4 b5 b6 b7 ci)
shrCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) ci = (b7,W8 ci b0 b1 b2 b3 b4 b5 b6)
msbW8 (W8 b _ _ _ _ _ _ _) = b
lsbW8 (W8 _ _ _ _ _ _ _ b) = b

-- end primitive boilerplate

-- Kleisli composition
(<>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
f <> g = \ a -> f a >>= g




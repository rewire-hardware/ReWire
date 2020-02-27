module ReWire.Bits where

import ReWire

-- | "Cleared," "set."
data Bit = C | S
data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit
data W32 = W32 W8 W8 W8 W8

isSet :: Bit -> Bool
isSet S = True
isSet _ = False

toBit :: Bool -> Bit
toBit True = S
toBit _    = C

andb :: Bit -> Bit -> Bit
andb S S = S
andb _ _ = C

orb :: Bit -> Bit -> Bit
orb C C = C
orb _ _ = S

xorb :: Bit -> Bit -> Bit
xorb C C = C
xorb S S = C
xorb _ _ = S

notb :: Bit -> Bit
notb C = S
notb S = C

eqb :: Bit -> Bit -> Bool
eqb C C = True
eqb S S = True
eqb _ _ = False

zeroW8 :: W8
zeroW8 = W8 C C C C C C C C

isZero :: W8 -> Bool
isZero = eqW8 zeroW8

isZeroW8 :: W8 -> Bool
isZeroW8 = isZero

oneW8 :: W8
oneW8 = W8 C C C C C C C S

onesW8 :: W8
onesW8 = W8 S S S S S S S S

-- | Arithmetic mod 2^8 on nats. Increment.
incW8 :: W8 -> W8
incW8 (W8 a b c d e f g C) = W8 a b c d e f g S
incW8 (W8 a b c d e f C S) = W8 a b c d e f S C
incW8 (W8 a b c d e C S S) = W8 a b c d e S C C
incW8 (W8 a b c d C S S S) = W8 a b c d S C C C
incW8 (W8 a b c C S S S S) = W8 a b c S C C C C
incW8 (W8 a b C S S S S S) = W8 a b S C C C C C
incW8 (W8 a C S S S S S S) = W8 a S C C C C C C
incW8 (W8 C S S S S S S S) = W8 S C C C C C C C
incW8 (W8 S S S S S S S S) = W8 C C C C C C C C

-- | Decrement.
decW8 :: W8 -> W8
decW8 (W8 a b c d e f g S) = W8 a b c d e f g C
decW8 (W8 a b c d e f S C) = W8 a b c d e f C S
decW8 (W8 a b c d e S C C) = W8 a b c d e C S S
decW8 (W8 a b c d S C C C) = W8 a b c d C S S S
decW8 (W8 a b c S C C C C) = W8 a b c C S S S S
decW8 (W8 a b S C C C C C) = W8 a b C S S S S S
decW8 (W8 a S C C C C C C) = W8 a C S S S S S S
decW8 (W8 S C C C C C C C) = W8 C S S S S S S S
decW8 (W8 C C C C C C C C) = W8 S S S S S S S S

-- bitwiseW8 :: (Bit -> Bit) -> W8 -> W8
-- bitwiseW8 f (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 (f b0) (f b1) (f b2) (f b3) (f b4) (f b5) (f b6) (f b7)
--
-- bitwiseW8' :: (Bit -> Bit -> Bit) -> W8 -> W8 -> W8
-- bitwiseW8' f (W8 b0  b1  b2  b3  b4  b5  b6  b7)
--              (W8 b0' b1' b2' b3' b4' b5' b6' b7') = W8 (f b0 b0') (f b1 b1') (f b2 b2') (f b3 b3') (f b4 b4') (f b5 b5') (f b6 b6') (f b7 b7')

notW8 :: W8 -> W8
notW8 (W8 a b c d e f g h) = W8 (notb a) (notb b) (notb c) (notb d) (notb e) (notb f) (notb g) (notb h)
-- notW8 = bitwiseW8 notb

-- | Rotate left.
rolW8 :: W8 -> W8
rolW8 (W8 a b c d e f g h) = W8 b c d e f g h a

-- | Rotate right.
rorW8 :: W8 -> W8
rorW8 (W8 a b c d e f g h) = W8 h a b c d e f g

-- | Bitwise and.
andW8 :: W8 -> W8 -> W8
andW8 = nativeVhdl "andW8" andW8
-- andW8 = bitwiseW8' andb

-- | Bitwise or.
orW8 :: W8 -> W8 -> W8
orW8 = nativeVhdl "orW8" orW8
-- orW8 = bitwiseW8' orb

-- | Bitwise xor.
xorW8 :: W8 -> W8 -> W8
xorW8 = nativeVhdl "xorW8" xorW8
-- xorW8 = bitwiseW8' xorb

plusW8 :: W8 -> W8 -> W8
plusW8 a b | isZero a = b
plusW8 a b | isZero b = a
plusW8 a b = plusW8 (decW8 a) (incW8 b)

eqW8 :: W8 -> W8 -> Bool
eqW8 (W8 a b c d e f g h) (W8 a' b' c' d' e' f' g' h')
      = a `eqb` a' && b `eqb` b'
     && c `eqb` c' && d `eqb` d'
     && e `eqb` e' && f `eqb` f'
     && g `eqb` g' && h `eqb` h'

-- {-# INLINE bitwiseW32 #-}
-- bitwiseW32 :: (Bit -> Bit) -> W32 -> W32
-- bitwiseW32 f (W32 b0 b1 b2 b3) = W32 (bitwiseW8 f b0) (bitwiseW8 f b1) (bitwiseW8 f b2) (bitwiseW8 f b3)
--
-- {-# INLINE bitwiseW32' #-}
-- bitwiseW32' :: (Bit -> Bit -> Bit) -> W32 -> W32 -> W32
-- bitwiseW32' f (W32 b0  b1  b2  b3)
--               (W32 b0' b1' b2' b3') = W32 (bitwiseW8' f b0 b0') (bitwiseW8' f b1 b1') (bitwiseW8' f b2 b2') (bitwiseW8' f b3 b3')

zeroW32 :: W32
zeroW32 = W32 zeroW8 zeroW8 zeroW8 zeroW8

oneW32 :: W32
oneW32 = W32 zeroW8 zeroW8 zeroW8 oneW8

notW32 :: W32 -> W32
notW32 (W32 a b c d) = W32 (notW8 a) (notW8 b) (notW8 c) (notW8 d)

andW32 :: W32 -> W32 -> W32
andW32 = nativeVhdl "andW32" andW32
-- andW32 = bitwiseW32' andb

orW32 :: W32 -> W32 -> W32
orW32 = nativeVhdl "orW32" orW32
-- orW32 = bitwiseW32' orb

xorW32 :: W32 -> W32 -> W32
xorW32 = nativeVhdl "xorW32" xorW32
-- xorW32 = bitwiseW32' xorb

plusW32 :: W32 -> W32 -> W32
plusW32 = nativeVhdl "plusW32" plusW32

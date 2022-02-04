module ReWire.Bits where

import ReWire

-- | "Cleared," "set."
--data Bit = C | S
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

-- | addb x y ci = (x + y + ci, carry-out)
addb :: Bit -> Bit -> Bit -> (Bit, Bit)
addb x y ci = ((x `xorb` y) `xorb` ci, ((x `xorb` y) `andb` ci) `orb` (x `andb` y))

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

notW8 :: W8 -> W8
notW8 (W8 a b c d e f g h) = W8 (notb a) (notb b) (notb c) (notb d) (notb e) (notb f) (notb g) (notb h)

-- | Rotate left.
rolW8 :: W8 -> W8
rolW8 (W8 a b c d e f g h) = W8 b c d e f g h a

-- | Rotate right.
rorW8 :: W8 -> W8
rorW8 (W8 a b c d e f g h) = W8 h a b c d e f g

-- | Bitwise and.
andW8 :: W8 -> W8 -> W8
andW8 (W8 a  b  c  d  e  f  g  h )
      (W8 a' b' c' d' e' f' g' h') = W8 (andb a a') (andb b b') (andb c c') (andb d d') (andb e e') (andb f f') (andb g g') (andb h h')

-- | Bitwise or.
orW8 :: W8 -> W8 -> W8
orW8 (W8 a  b  c  d  e  f  g  h )
     (W8 a' b' c' d' e' f' g' h') = W8 (orb a a') (orb b b') (orb c c') (orb d d') (orb e e') (orb f f') (orb g g') (orb h h')

-- | Bitwise xor.
xorW8 :: W8 -> W8 -> W8
xorW8 (W8 a  b  c  d  e  f  g  h )
      (W8 a' b' c' d' e' f' g' h') = W8 (xorb a a') (xorb b b') (xorb c c') (xorb d d') (xorb e e') (xorb f f') (xorb g g') (xorb h h')

plusW8 :: W8 -> W8 -> W8
plusW8 x y = fst (plusW8' x y C)

-- | Ripple-carry adder.
plusW8' :: W8 -> W8 -> Bit -> (W8, Bit)
plusW8' (W8 a  b  c  d  e  f  g  h ) (W8 a' b' c' d' e' f' g' h') ci =
      let (r0, co0) = addb h h' ci   in
          (r1, co1) = addb g g' co0 in
          (r2, co2) = addb f f' co1 in
          (r3, co3) = addb e e' co2 in
          (r4, co4) = addb d d' co3 in
          (r5, co5) = addb c c' co4 in
          (r6, co6) = addb b b' co5 in
          (r7, co7) = addb a a' co6 in
      (W8 r7 r6 r5 r4 r3 r2 r1 r0, co7)

eqW8 :: W8 -> W8 -> Bool
eqW8 (W8 a b c d e f g h) (W8 a' b' c' d' e' f' g' h')
      = a `eqb` a' && b `eqb` b'
     && c `eqb` c' && d `eqb` d'
     && e `eqb` e' && f `eqb` f'
     && g `eqb` g' && h `eqb` h'

zeroW32 :: W32
zeroW32 = W32 zeroW8 zeroW8 zeroW8 zeroW8

oneW32 :: W32
oneW32 = W32 zeroW8 zeroW8 zeroW8 oneW8

notW32 :: W32 -> W32
notW32 (W32 a b c d) = W32 (notW8 a) (notW8 b) (notW8 c) (notW8 d)

andW32 :: W32 -> W32 -> W32
andW32 (W32 a  b  c  d )
       (W32 a' b' c' d') = W32 (andW8 a a') (andW8 b b') (andW8 c c') (andW8 d d')

orW32 :: W32 -> W32 -> W32
orW32 (W32 a  b  c  d )
      (W32 a' b' c' d') = W32 (orb a a') (orb b b') (orb c c') (orb d d')

xorW32 :: W32 -> W32 -> W32
xorW32 (W32 a  b  c  d )
       (W32 a' b' c' d') = W32 (xorb a a') (xorb b b') (xorb c c') (xorb d d')

plusW32 :: W32 -> W32 -> W32
plusW32 (W32 a b c d) (W32 a' b' c' d') =
      let (r0, co0) = plusW8' d d' C   in
          (r1, co1) = plusW8' c c' co0 in
          (r2, co2) = plusW8' b b' co1 in
          (r3, _)   = plusW8' a a' co2 in
      W32 r3 r2 r1 r0

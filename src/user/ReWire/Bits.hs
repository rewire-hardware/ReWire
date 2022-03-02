module ReWire.Bits where

import ReWire

data W2  = W2  Bit Bit
data W3  = W3  Bit Bit Bit
data W4  = W4  Bit Bit Bit Bit
data W5  = W5  Bit Bit Bit Bit Bit
data W6  = W6  Bit Bit Bit Bit Bit Bit
data W7  = W7  Bit Bit Bit Bit Bit Bit Bit

data W8  = W8  Bit Bit Bit Bit Bit Bit Bit Bit

data W9  = W9  Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W10 = W10 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W11 = W11 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W12 = W12 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W13 = W13 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W14 = W14 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W15 = W15 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit

data W16 = W16 W8 W8
data W17 = W17 Bit W16

-- data W18 = W18 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W19 = W19 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W20 = W20 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W21 = W21 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W22 = W22 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W23 = W23 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W24 = W24 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W25 = W25 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W26 = W26 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W27 = W27 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W28 = W28 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W29 = W29 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W30 = W30 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W31 = W31 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit

data W32 = W32 W16 W16
data W33 = W33 Bit W16 W16

data W34 = W34 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W35 = W35 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W36 = W36 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W37 = W37 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W38 = W38 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W39 = W39 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W40 = W40 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W41 = W41 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W42 = W42 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W43 = W43 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W44 = W44 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W45 = W45 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W46 = W46 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W47 = W47 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W48 = W48 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W49 = W49 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W50 = W50 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W51 = W51 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W52 = W52 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W53 = W53 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W54 = W54 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W55 = W55 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W56 = W56 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W57 = W57 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W58 = W58 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W59 = W59 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W60 = W60 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W61 = W61 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W62 = W62 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
data W63 = W63 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit

data W64 = W64 W32 W32
data W65 = W65 Bit W32 W32
data W66 = W66 Bit Bit W32 W32
data W67 = W67 Bit Bit Bit W32 W32

data W128 = W128 W64 W64
data W129 = W129 Bit W64 W64
data W130 = W130 Bit Bit W64 W64
data W131 = W131 Bit Bit Bit W64 W64
data W132 = W132 Bit Bit Bit Bit W64 W64
data W133 = W133 Bit Bit Bit Bit Bit W64 W64
data W134 = W134 Bit Bit Bit Bit Bit Bit W64 W64

data W256 = W256 W64 W64 W64 W64
data W257 = W257 Bit W256
data W258 = W258 W2 W256
data W259 = W259 W3 W256
data W260 = W260 W4 W256

data W274 = W274 W7 W8 W259
data W275 = W275 W16 W259

-- Bit operations

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

-- W8 operations

zeroW8 :: W8
zeroW8 = W8 C C C C C C C C

oneW8 :: W8
oneW8 = W8 C C C C C C C S

onesW8 :: W8
onesW8 = W8 S S S S S S S S

isZeroW8 :: W8 -> Bool
isZeroW8 = eqW8 zeroW8

-- | Arithmetic mod 2^8 on nats. Increment.
incW8 :: W8 -> W8
incW8 = plusW8 oneW8

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
      let (r0, co0) = addb h h' ci
          (r1, co1) = addb g g' co0
          (r2, co2) = addb f f' co1
          (r3, co3) = addb e e' co2
          (r4, co4) = addb d d' co3
          (r5, co5) = addb c c' co4
          (r6, co6) = addb b b' co5
          (r7, co7) = addb a a' co6
      in (W8 r7 r6 r5 r4 r3 r2 r1 r0, co7)

eqW8 :: W8 -> W8 -> Bool
eqW8 (W8 a b c d e f g h) (W8 a' b' c' d' e' f' g' h')
      = a `eqb` a' && b `eqb` b'
     && c `eqb` c' && d `eqb` d'
     && e `eqb` e' && f `eqb` f'
     && g `eqb` g' && h `eqb` h'

-- W16 operations

zeroW16 :: W16
zeroW16 = W16 zeroW8 zeroW8

oneW16 :: W16
oneW16 = W16 zeroW8 oneW8

onesW16 :: W16
onesW16 = W16 onesW8 onesW8

isZeroW16 :: W16 -> Bool
isZeroW16 = eqW16 zeroW16

-- | Increment.
incW16 :: W16 -> W16
incW16 = plusW16 oneW16

notW16 :: W16 -> W16
notW16 (W16 w1 w0) = W16 (notW8 w1) (notW8 w0)

-- | Bitwise and.
andW16 :: W16 -> W16 -> W16
andW16 (W16 w1 w0) (W16 w1' w0') = W16 (andW8 w1 w1') (andW8 w0 w0')

-- | Bitwise or.
orW16 :: W16 -> W16 -> W16
orW16 (W16 w1 w0) (W16 w1' w0') = W16 (orW8 w1 w1') (orW8 w0 w0')

-- | Bitwise xor.
xorW16 :: W16 -> W16 -> W16
xorW16 (W16 w1 w0) (W16 w1' w0') = W16 (xorW8 w1 w1') (xorW8 w0 w0')

plusW16 :: W16 -> W16 -> W16
plusW16 x y = fst (plusW16' x y C)

-- | Ripple-carry adder.
plusW16' :: W16 -> W16 -> Bit -> (W16, Bit)
plusW16' (W16 w1 w0) (W16 w1' w0') ci =
      let (r0, co0) = plusW8' w0 w0' ci
          (r1, co1) = plusW8' w1 w1' co0
      in (W16 r1 r0, co1)

eqW16 :: W16 -> W16 -> Bool
eqW16 (W16 w1 w0) (W16 w1' w0') = w1 `eqW8` w1' && w0 `eqW8` w0'
-- |
-- | W18 operations
-- |

data W18 = W18 Bit Bit W16

zeroW18 :: W18
zeroW18 = W18 C C zeroW16

oneW18 :: W18
oneW18 = W18 C C oneW16

onesW18 :: W18
onesW18 = W18 S S onesW16

eqW18 :: W18 -> W18 -> Bool
eqW18 (W18 b1 b0 w0) (W18 b1' b0' w0') = eqb b1 b1' && eqb b0 b0' && w0 `eqW16` w0'

isZeroW18 :: W18 -> Bool
isZeroW18 = eqW18 zeroW18

notW18 :: W18 -> W18
notW18 (W18 b1 b0 w0) = W18 (notb b1) (notb b0) (notW16 w0)

-- | Bitwise and.
andW18 :: W18 -> W18 -> W18
andW18 (W18 b1 b0 w0) (W18 b1' b0' w0') = W18 (andb b1 b1') (andb b0 b0') (andW16 w0 w0')

-- | Bitwise or.
orW18 :: W18 -> W18 -> W18
orW18 (W18 b1 b0 w0) (W18 b1' b0' w0') = W18 (orb b1 b1') (orb b0 b0') (orW16 w0 w0')

-- | Bitwise xor.
xorW18 :: W18 -> W18 -> W18
xorW18 (W18 b1 b0 w0) (W18 b1' b0' w0') = W18 (xorb b1 b1') (xorb b0 b0') (xorW16 w0 w0')

plusW18 :: W18 -> W18 -> W18
plusW18 x y = fst (plusW18' x y C)

-- | Ripple-carry adder.
plusW18' :: W18 -> W18 -> Bit -> (W18, Bit)
plusW18' (W18 b1 b0 w0) (W18 b1' b0' w0') ci =
      let (r0, co0) = plusW16' w0 w0' ci
          (r1, co1) = addb b0 b0' co0
          (r2, co2) = addb b1 b1' co1
      in (W18 r2 r1 r0, co2)

-- | Arithmetic mod 2^8 on nats. Increment.
incW18 :: W18 -> W18
incW18 = plusW18 oneW18

-- W32 operations

zeroW32 :: W32
zeroW32 = W32 zeroW16 zeroW16

oneW32 :: W32
oneW32 = W32 zeroW16 oneW16

onesW32 :: W32
onesW32 = W32 onesW16 onesW16

isZeroW32 :: W32 -> Bool
isZeroW32 = eqW32 zeroW32

-- | Increment.
incW32 :: W32 -> W32
incW32 = plusW32 oneW32

notW32 :: W32 -> W32
notW32 (W32 w1 w0) = W32 (notW16 w1) (notW16 w0)

-- | Bitwise and.
andW32 :: W32 -> W32 -> W32
andW32 (W32 w1 w0) (W32 w1' w0') = W32 (andW16 w1 w1') (andW16 w0 w0')

-- | Bitwise or.
orW32 :: W32 -> W32 -> W32
orW32 (W32 w1 w0) (W32 w1' w0') = W32 (orW16 w1 w1') (orW16 w0 w0')

-- | Bitwise xor.
xorW32 :: W32 -> W32 -> W32
xorW32 (W32 w1 w0) (W32 w1' w0') = W32 (xorW16 w1 w1') (xorW16 w0 w0')

plusW32 :: W32 -> W32 -> W32
plusW32 x y = fst (plusW32' x y C)

-- | Ripple-carry adder.
plusW32' :: W32 -> W32 -> Bit -> (W32, Bit)
plusW32' (W32 w1 w0) (W32 w1' w0') ci =
      let (r0, co0) = plusW16' w0 w0' ci
          (r1, co1) = plusW16' w1 w1' co0
      in (W32 r1 r0, co1)

eqW32 :: W32 -> W32 -> Bool
eqW32 (W32 w1 w0) (W32 w1' w0') = w1 `eqW16` w1' && w0 `eqW16` w0'

-- W64 operations

zeroW64 :: W64
zeroW64 = W64 zeroW32 zeroW32

oneW64 :: W64
oneW64 = W64 zeroW32 oneW32

onesW64 :: W64
onesW64 = W64 onesW32 onesW32

isZeroW64 :: W64 -> Bool
isZeroW64 = eqW64 zeroW64

-- | Increment.
incW64 :: W64 -> W64
incW64 = plusW64 oneW64

notW64 :: W64 -> W64
notW64 (W64 w1 w0) = W64 (notW32 w1) (notW32 w0)

-- | Bitwise and.
andW64 :: W64 -> W64 -> W64
andW64 (W64 w1 w0) (W64 w1' w0') = W64 (andW32 w1 w1') (andW32 w0 w0')

-- | Bitwise or.
orW64 :: W64 -> W64 -> W64
orW64 (W64 w1 w0) (W64 w1' w0') = W64 (orW32 w1 w1') (orW32 w0 w0')

-- | Bitwise xor.
xorW64 :: W64 -> W64 -> W64
xorW64 (W64 w1 w0) (W64 w1' w0') = W64 (xorW32 w1 w1') (xorW32 w0 w0')

plusW64 :: W64 -> W64 -> W64
plusW64 x y = fst (plusW64' x y C)

-- | Ripple-carry adder.
plusW64' :: W64 -> W64 -> Bit -> (W64, Bit)
plusW64' (W64 w1 w0) (W64 w1' w0') ci =
      let (r0, co0) = plusW32' w0 w0' ci
          (r1, co1) = plusW32' w1 w1' co0
      in (W64 r1 r0, co1)

eqW64 :: W64 -> W64 -> Bool
eqW64 (W64 w1 w0) (W64 w1' w0') = w1 `eqW32` w1' && w0 `eqW32` w0'

-- |
-- | W3 operations
-- |

zeroW3 :: W3
zeroW3  = W3 C C C

oneW3  :: W3
oneW3   = W3 C C S

onesW3 :: W3
onesW3  = W3 S S S 

eqW3 :: W3 -> W3 -> Bool
eqW3 (W3 a b c) (W3 a' b' c')
      = a `eqb` a' && b `eqb` b' && c `eqb` c'

isZeroW3 :: W3 -> Bool
isZeroW3 = eqW3 zeroW3

incW3 :: W3 -> W3
incW3 = plusW3 oneW3

notW3 :: W3 -> W3
notW3 (W3 a b c) = W3 (notb a) (notb b) (notb c)

-- | Rotate left.
rolW3 :: W3 -> W3
rolW3 (W3 a b c) = W3 b c a

-- | Rotate right.
rorW3 :: W3 -> W3
rorW3 (W3 a b c) = W3 c a b

-- | Bitwise and.
andW3 :: W3 -> W3 -> W3
andW3 (W3 a  b  c)
      (W3 a' b' c') = W3 (andb a a') (andb b b') (andb c c')

-- | Bitwise or.
orW3 :: W3 -> W3 -> W3
orW3 (W3 a  b  c)
     (W3 a' b' c') = W3 (orb a a') (orb b b') (orb c c')

-- | Bitwise xor.
xorW3 :: W3 -> W3 -> W3
xorW3 (W3 a  b  c)
      (W3 a' b' c') = W3 (xorb a a') (xorb b b') (xorb c c')

plusW3 :: W3 -> W3 -> W3
plusW3 x y = fst (plusW3' x y C)

-- | Ripple-carry adder.
plusW3' :: W3 -> W3 -> Bit -> (W3, Bit)
plusW3' (W3 a  b  c) (W3 a' b' c') ci =
      let (r0, co0) = addb c c' ci
          (r1, co1) = addb b b' co0
          (r2, co2) = addb a a' co1
      in (W3 r2 r1 r0, co2)

-- |
-- | W7 operations
-- |

zeroW7, oneW7, onesW7 :: W7
zeroW7 = W7 C C C C C C C
oneW7  = W7 C C C C C C S
onesW7 = W7 S S S S S S S

eqW7 :: W7 -> W7 -> Bool
eqW7 (W7 a6 a5 a4 a3 a2 a1 a0) (W7 b6 b5 b4 b3 b2 b1 b0) = eqb a6 b6 && eqb a5 b5 && eqb a4 b4 &&
                                                           eqb a3 b3 && eqb a2 b2 && eqb a1 b1 && eqb a0 b0

isZeroW7 :: W7 -> Bool
isZeroW7 = eqW7 zeroW7

notW7 :: W7 -> W7
notW7 (W7 a06 a05 a04 a03 a02 a01 a00) = W7 (notb a06) (notb a05) (notb a04) (notb a03) (notb a02) (notb a01) (notb a00)

andW7 , orW7 , xorW7 :: W7 -> W7 -> W7
andW7 (W7 a06 a05 a04 a03 a02 a01 a00) 
      (W7 b06 b05 b04 b03 b02 b01 b00) =
         W7 (a06 `andb` b06) (a05 `andb` b05) (a04 `andb` b04)
            (a03 `andb` b03) (a02 `andb` b02) (a01 `andb` b01) (a00 `andb` b00)

orW7 (W7 a06 a05 a04 a03 a02 a01 a00) 
     (W7 b06 b05 b04 b03 b02 b01 b00) =
          W7  (a06 `orb` b06) (a05 `orb` b05) (a04 `orb` b04)
              (a03 `orb` b03) (a02 `orb` b02) (a01 `orb` b01) (a00 `orb` b00)

xorW7 (W7 a06 a05 a04 a03 a02 a01 a00) 
      (W7 b06 b05 b04 b03 b02 b01 b00) =
         W7 (a06 `xorb` b06) (a05 `xorb` b05) (a04 `xorb` b04)
            (a03 `xorb` b03) (a02 `xorb` b02) (a01 `xorb` b01) (a00 `xorb` b00)

plusW7 :: W7 -> W7 -> W7
plusW7 x y = fst (plusW7' x y C)

-- | Ripple-carry adder.
plusW7' :: W7 -> W7 -> Bit -> (W7, Bit)
plusW7' (W7 a6 a5 a4 a3 a2 a1 a0) (W7 b6 b5 b4 b3 b2 b1 b0) c = (W7 r6 r5 r4 r3 r2 r1 r0 , c')
    where
      (r0, co0) = addb a0 b0 c
      (r1, co1) = addb a1 b1 co0
      (r2, co2) = addb a2 b2 co1
      (r3, co3) = addb a3 b3 co2
      (r4, co4) = addb a4 b4 co3
      (r5, co5) = addb a5 b5 co4
      (r6, c') = addb a6 b6 co5

-- | Increment.
incW7 :: W7 -> W7
incW7 = plusW7 oneW7

-- |
-- | W17 operations
-- |

zeroW17 :: W17
zeroW17 = W17 C zeroW16

oneW17 :: W17
oneW17 = W17 C oneW16

onesW17 :: W17
onesW17 = W17 S onesW16

eqW17 :: W17 -> W17 -> Bool
eqW17 (W17 b w0) (W17 b' w0') = eqb b b' && w0 `eqW16` w0'

isZeroW17 :: W17 -> Bool
isZeroW17 = eqW17 zeroW17

notW17 :: W17 -> W17
notW17 (W17 b w0) = W17 (notb b) (notW16 w0)

-- | Bitwise and.
andW17 :: W17 -> W17 -> W17
andW17 (W17 b w0) (W17 b' w0') = W17 (andb b b') (andW16 w0 w0')

-- | Bitwise or.
orW17 :: W17 -> W17 -> W17
orW17 (W17 b w0) (W17 b' w0') = W17 (orb b b') (orW16 w0 w0')

-- | Bitwise xor.
xorW17 :: W17 -> W17 -> W17
xorW17 (W17 b w0) (W17 b' w0') = W17 (xorb b b') (xorW16 w0 w0')

plusW17 :: W17 -> W17 -> W17
plusW17 x y = fst (plusW17' x y C)

-- | Ripple-carry adder.
plusW17' :: W17 -> W17 -> Bit -> (W17, Bit)
plusW17' (W17 b w0) (W17 b' w0') ci =
      let (r0, co0) = plusW16' w0 w0' ci
          (r, co1)  = addb b b' co0
      in (W17 r r0, co1)

-- | Increment.
incW17 :: W17 -> W17
incW17 = plusW17 oneW17

-- |
-- | W33 operations
-- |

zeroW33 :: W33
zeroW33 = W33 C zeroW16 zeroW16

oneW33 :: W33
oneW33 = W33 C zeroW16 oneW16

onesW33 :: W33
onesW33 = W33 S onesW16 onesW16

eqW33 :: W33 -> W33 -> Bool
eqW33 (W33 b w1 w0) (W33 b' w1' w0') = eqb b b' && w1 `eqW16` w1' && w0 `eqW16` w0'

isZeroW33 :: W33 -> Bool
isZeroW33 = eqW33 zeroW33

notW33 :: W33 -> W33
notW33 (W33 b w1 w0) = W33 (notb b) (notW16 w1) (notW16 w0)

-- | Bitwise and.
andW33 :: W33 -> W33 -> W33
andW33 (W33 b w1 w0) (W33 b' w1' w0') = W33 (andb b b') (andW16 w1 w1') (andW16 w0 w0')

-- | Bitwise or.
orW33 :: W33 -> W33 -> W33
orW33 (W33 b w1 w0) (W33 b' w1' w0') = W33 (orb b b') (orW16 w1 w1') (orW16 w0 w0')

-- | Bitwise xor.
xorW33 :: W33 -> W33 -> W33
xorW33 (W33 b w1 w0) (W33 b' w1' w0') = W33 (xorb b b') (xorW16 w1 w1') (xorW16 w0 w0')

plusW33 :: W33 -> W33 -> W33
plusW33 x y = fst (plusW33' x y C)

-- | Ripple-carry adder.
plusW33' :: W33 -> W33 -> Bit -> (W33, Bit)
plusW33' (W33 b w1 w0) (W33 b' w1' w0') ci =
      let (r0, co0) = plusW16' w0 w0' ci
          (r1, co1) = plusW16' w1 w1' co0
          (r, co2)  = addb b b' co1
      in (W33 r r1 r0, co2)

-- | Increment.
incW33 :: W33 -> W33
incW33 = plusW33 oneW33

-- |
-- | W128
-- |

zeroW128 :: W128
zeroW128  = W128 zeroW64 zeroW64

-- |
-- | W129
-- |

zeroW129 :: W129
zeroW129  = W129 C zeroW64 zeroW64

-- |
-- | W131
-- |

zeroW131 :: W131
zeroW131  = W131 C C C zeroW64 zeroW64

-- |
-- | W256
-- |

zeroW256, oneW256, onesW256 :: W256
zeroW256 = W256 zeroW64 zeroW64 zeroW64 zeroW64
oneW256  = W256 zeroW64 zeroW64 zeroW64 oneW64
onesW256 = W256 onesW64 onesW64 onesW64 onesW64 

eqW256 :: W256 -> W256 -> Bool
eqW256 (W256 w3 w2 w1 w0) (W256 w3' w2' w1' w0') = w3 `eqW64` w3' && w2 `eqW64` w2' && w1 `eqW64` w1' && w0 `eqW64` w0'

isZeroW256 :: W256 -> Bool
isZeroW256 = eqW256 zeroW256

notW256 :: W256 -> W256
notW256 (W256 w3 w2 w1 w0) = W256 (notW64 w3) (notW64 w2) (notW64 w1) (notW64 w0)

-- | Bitwise and.
andW256 :: W256 -> W256 -> W256
andW256 (W256 w3 w2 w1 w0) (W256 w3' w2' w1' w0') = W256 (andW64 w3 w3') (andW64 w2 w2') (andW64 w1 w1') (andW64 w0 w0')

-- | Bitwise or.
orW256 :: W256 -> W256 -> W256
orW256 (W256 w3 w2 w1 w0) (W256 w3' w2' w1' w0') = W256 (orW64 w3 w3') (orW64 w2 w2') (orW64 w1 w1') (orW64 w0 w0')

-- | Bitwise xor.
xorW256 :: W256 -> W256 -> W256
xorW256 (W256 w3 w2 w1 w0) (W256 w3' w2' w1' w0') = W256 (xorW64 w3 w3') (xorW64 w2 w2') (xorW64 w1 w1') (xorW64 w0 w0')

plusW256 :: W256 -> W256 -> W256
plusW256 x y = fst (plusW256' x y C)

-- | Ripple-carry adder.
plusW256' :: W256 -> W256 -> Bit -> (W256, Bit)
plusW256' (W256 w3 w2 w1 w0) (W256 w3' w2' w1' w0') ci =
      let (r0, co0) = plusW64' w0 w0' ci
          (r1, co1) = plusW64' w1 w1' co0
          (r2, co2) = plusW64' w2 w2' co1
          (r3, co3) = plusW64' w3 w3' co2
      in (W256 r3 r2 r1 r0 , co3)

-- | Increment.
incW256 :: W256 -> W256
incW256 = plusW256 oneW256


-- |
-- | W257
-- |

zeroW257, oneW257, onesW257 :: W257
zeroW257 = W257 C zeroW256
oneW257  = W257 C oneW256
onesW257 = W257 S onesW256

eqW257 :: W257 -> W257 -> Bool
eqW257 (W257 w1 w0) (W257 w1' w0') = w1 `eqb` w1' && w0 `eqW256` w0'

isZeroW257 :: W257 -> Bool
isZeroW257 = eqW257 zeroW257


notW257 :: W257 -> W257
notW257 (W257 w1 w0) = W257 (notb w1) (notW256 w0)

-- | Bitwise and.
andW257 :: W257 -> W257 -> W257
andW257 (W257 w1 w0) (W257 w1' w0') = W257 (andb w1 w1') (andW256 w0 w0')


-- | Bitwise or.
orW257 :: W257 -> W257 -> W257
orW257 (W257 w1 w0) (W257 w1' w0') = W257 (orb w1 w1') (orW256 w0 w0')

-- | Bitwise xor.
xorW257 :: W257 -> W257 -> W257
xorW257 (W257 w1 w0) (W257 w1' w0') = W257 (xorb w1 w1') (xorW256 w0 w0')

plusW257 :: W257 -> W257 -> W257
plusW257 x y = fst (plusW257' x y C)

-- | Ripple-carry adder.
plusW257' :: W257 -> W257 -> Bit -> (W257, Bit)
plusW257' (W257 w1 w0) (W257 w1' w0') ci =
      let (r0, co0) = plusW256' w0 w0' ci
          (r1, co1) = addb w1 w1' co0
      in (W257 r1 r0 , co1)

-- | Arithmetic mod 2^8 on nats. Increment.
incW257 :: W257 -> W257
incW257 = plusW257 oneW257

-- |
-- | W259
-- |

zeroW259, oneW259, onesW259 :: W259
zeroW259 = W259 zeroW3 zeroW256
oneW259  = W259 zeroW3 zeroW256
onesW259 = W259 onesW3 onesW256

eqW259 :: W259 -> W259 -> Bool
eqW259 (W259 w1 w0) (W259 w1' w0') = w1 `eqW3` w1' && w0 `eqW256` w0'

isZeroW259 :: W259 -> Bool
isZeroW259 = eqW259 zeroW259

notW259 :: W259 -> W259
notW259 (W259 w1 w0) = W259 (notW3 w1) (notW256 w0)

-- | Bitwise and.
andW259 :: W259 -> W259 -> W259
andW259 (W259 w1 w0) (W259 w1' w0') = W259 (andW3 w1 w1') (andW256 w0 w0')

-- | Bitwise or.
orW259 :: W259 -> W259 -> W259
orW259 (W259 w1 w0) (W259 w1' w0') = W259 (orW3 w1 w1') (orW256 w0 w0')

-- | Bitwise xor.
xorW259 :: W259 -> W259 -> W259
xorW259 (W259 w1 w0) (W259 w1' w0') = W259 (xorW3 w1 w1') (xorW256 w0 w0')

plusW259 :: W259 -> W259 -> W259
plusW259 x y = fst (plusW259' x y C)

-- | Ripple-carry adder.
plusW259' :: W259 -> W259 -> Bit -> (W259, Bit)
plusW259' (W259 w1 w0) (W259 w1' w0') ci =
      let (r0, co0) = plusW256' w0 w0' ci
          (r1, co1) = plusW3' w1 w1' co0
      in (W259 r1 r0 , co1)

-- | Increment.
incW259 :: W259 -> W259
incW259 = plusW259 oneW259

-- |
-- | W274
-- |

zeroW274, oneW274, onesW274 :: W274
zeroW274 = W274 zeroW7 zeroW8 zeroW259
oneW274  = W274 zeroW7 zeroW8 oneW259
onesW274 = W274 onesW7 onesW8 onesW259


eqW274 :: W274 -> W274 -> Bool
eqW274 (W274 w2 w1 w0) (W274 w2' w1' w0') = w2 `eqW7` w2' && w1 `eqW8` w1' && w0 `eqW259` w0'

isZeroW274 :: W274 -> Bool
isZeroW274 = eqW274 zeroW274

notW274 :: W274 -> W274
notW274 (W274 w2 w1 w0) = W274 (notW7 w2) (notW8 w1) (notW259 w0)

-- | Bitwise and.
andW274 :: W274 -> W274 -> W274
andW274 (W274 w2 w1 w0) (W274 w2' w1' w0') = W274 (andW7 w2 w2') (andW8 w1 w1') (andW259 w0 w0')

-- | Bitwise or.
orW274 :: W274 -> W274 -> W274
orW274 (W274 w2 w1 w0) (W274 w2' w1' w0') = W274 (orW7 w2 w2') (orW8 w1 w1') (orW259 w0 w0')

-- | Bitwise xor.
xorW274 :: W274 -> W274 -> W274
xorW274 (W274 w2 w1 w0) (W274 w2' w1' w0') = W274 (xorW7 w2 w2') (xorW8 w1 w1') (xorW259 w0 w0')

plusW274 :: W274 -> W274 -> W274
plusW274 x y = fst (plusW274' x y C)

-- | Ripple-carry adder.
plusW274' :: W274 -> W274 -> Bit -> (W274, Bit)
plusW274' (W274 w2 w1 w0) (W274 w2' w1' w0') ci =
      let (r0, co0) = plusW259' w0 w0' ci
          (r1, co1) = plusW8' w1 w1' co0
          (r2, co2) = plusW7' w2 w2' co1
      in (W274 r2 r1 r0 , co2)

-- | Increment.
incW274 :: W274 -> W274
incW274 = plusW274 oneW274


-- |
-- | W275
-- |

zeroW275, oneW275, onesW275 :: W275
zeroW275 = W275 zeroW16 zeroW259
oneW275  = W275 zeroW16 zeroW259
onesW275 = W275 onesW16 onesW259

eqW275 :: W275 -> W275 -> Bool
eqW275 (W275 w1 w0) (W275 w1' w0') = w1 `eqW16` w1' && w0 `eqW259` w0'

isZeroW275 :: W275 -> Bool
isZeroW275 = eqW275 zeroW275

notW275 :: W275 -> W275
notW275 (W275 w1 w0) = W275 (notW16 w1) (notW259 w0)

-- | Bitwise and.
andW275 :: W275 -> W275 -> W275
andW275 (W275 w1 w0) (W275 w1' w0') = W275 (andW16 w1 w1') (andW259 w0 w0')

-- | Bitwise or.
orW275 :: W275 -> W275 -> W275
orW275 (W275 w1 w0) (W275 w1' w0') = W275 (orW16 w1 w1') (orW259 w0 w0')

-- | Bitwise xor.
xorW275 :: W275 -> W275 -> W275
xorW275 (W275 w1 w0) (W275 w1' w0') = W275 (xorW16 w1 w1') (xorW259 w0 w0')

plusW275 :: W275 -> W275 -> W275
plusW275 x y = fst (plusW275' x y C)

-- | Ripple-carry adder.
plusW275' :: W275 -> W275 -> Bit -> (W275, Bit)
plusW275' (W275 w1 w0) (W275 w1' w0') ci =
      let (r0, co0) = plusW259' w0 w0' ci
          (r1, co1) = plusW16' w1 w1' co0
      in (W275 r1 r0 , co1)

-- | Increment.
incW275 :: W275 -> W275
incW275 = plusW275 oneW275


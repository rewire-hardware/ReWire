module Salsa20 where

--
-- NB: This should parse, but it should not typecheck.
--

--module Salsa20Cipher where
--import Control.Monad.Resumption.Reactive
--import Prelude (undefined,Num)
--import Control.Monad.Identity

--import Data.Bits

--Type shimming
--type Id = Identity


--End type shimming
data Unit = Unit
data Bit = Low | High
data W8  = W8  Bit Bit Bit Bit Bit Bit Bit Bit
data W16 = W16 Bit Bit Bit Bit Bit Bit Bit Bit
                    Bit Bit Bit Bit Bit Bit Bit Bit
data W32 = W32 Bit Bit Bit Bit Bit Bit Bit Bit
                   Bit Bit Bit Bit Bit Bit Bit Bit
                   Bit Bit Bit Bit Bit Bit Bit Bit
                   Bit Bit Bit Bit Bit Bit Bit Bit

data Bytes16 = Bytes16 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8


data Bytes8  = Bytes8 W8 W8 W8 W8 W8 W8 W8 W8
data Bytes64 = Bytes64 W8 W8 W8 W8 W8 W8 W8 W8
                       W8 W8 W8 W8 W8 W8 W8 W8
                       W8 W8 W8 W8 W8 W8 W8 W8
                       W8 W8 W8 W8 W8 W8 W8 W8
                       W8 W8 W8 W8 W8 W8 W8 W8
                       W8 W8 W8 W8 W8 W8 W8 W8
                       W8 W8 W8 W8 W8 W8 W8 W8
                       W8 W8 W8 W8 W8 W8 W8 W8

data Words16 a = Words16 a a a a a a a a a a a a a a a a

data Tuple2 a b = Tuple2 a b
data Tuple3 a b c = Tuple3 a b c
data Tuple4 a b c d = Tuple4 a b c d
data Tuple5 a b c d e = Tuple5 a b c d e
data ParTen = ParTen (Tuple2 (Words16 W32) (Words16 W32)) (Tuple2 (Words16 W32) (Words16 W32)) (Tuple2 (Words16 W32) (Words16 W32)) (Tuple2 (Words16 W32) (Words16 W32)) (Tuple2 (Words16 W32) (Words16 W32)) (Tuple2 (Words16 W32) (Words16 W32)) (Tuple2 (Words16 W32) (Words16 W32)) (Tuple2 (Words16 W32) (Words16 W32)) (Tuple2 (Words16 W32) (Words16 W32)) (Tuple2 (Words16 W32) (Words16 W32))

undefined :: a
undefined = undefined

add8 :: W8 -> W8 -> W8
add8 = nativeVhdl "prim_plus" undefined
xor32 :: W32 -> W32 -> W32
xor32 = nativeVhdl "prim_xor" undefined
add32 :: W32 -> W32 -> W32
add32 = nativeVhdl "prim_plus" undefined
xor512 :: Bytes64 -> Bytes64 -> Bytes64
xor512 = nativeVhdl "prim_xor" undefined

--Constant Values for initialization
--(101,120,112,97)
sigma0 :: Tuple4 W8 W8 W8 W8
sigma0 = Tuple4 (W8 Low High High Low Low High Low High) (W8 Low High High High High Low Low Low) (W8 Low High High High Low Low Low Low) (W8 Low High High Low Low Low Low High)

--(110,100,32,51)
sigma1 :: Tuple4 W8 W8 W8 W8
sigma1 = Tuple4 (W8 Low High High Low High High High Low) (W8 Low High High Low Low High Low Low) (W8 Low Low High Low Low Low Low Low) (W8 Low Low High High Low Low High High)

--(50,45,98,121)
sigma2 :: Tuple4 W8 W8 W8 W8
sigma2 = Tuple4 (W8 Low Low High High Low Low High Low) (W8 Low Low High Low High High Low High) (W8 Low High High Low Low Low High Low) (W8 Low High High High High Low Low High)

--(116,101,32,107)
sigma3 :: Tuple4 W8 W8 W8 W8
sigma3 = Tuple4 (W8 Low High High High Low High Low Low) (W8 Low High High Low Low High Low High) (W8 Low Low High Low Low Low Low Low) (W8 Low High High Low High Low High High)

--(101,120,112,97)
tau0 :: Tuple4 W8 W8 W8 W8
tau0 = Tuple4 (W8 Low High High Low Low High Low High) (W8 Low High High High High Low Low Low) (W8 Low High High High Low Low Low Low) (W8 Low High High Low Low Low Low High)

--(110,100,32,49)
tau1 :: Tuple4 W8 W8 W8 W8
tau1 = Tuple4 (W8 Low High High Low High High High Low) (W8 Low High High Low Low High Low Low) (W8 Low Low High Low Low Low Low Low) (W8 Low Low High High Low Low Low High)

--(54,45,98,121)
tau2 :: Tuple4 W8 W8 W8 W8
tau2 = Tuple4 (W8 Low Low High High Low High High Low) (W8 Low Low High Low High High Low High) (W8 Low High High Low Low Low High Low) (W8 Low High High High High Low Low High)

--(116,101,32,107)
tau3 :: Tuple4 W8 W8 W8 W8
tau3 = Tuple4 (W8 Low High High High Low High Low Low) (W8 Low High High Low Low High Low High) (W8 Low Low High Low Low Low Low Low) (W8 Low High High Low High Low High High)

--Primitives

--Machine Messages
--data Rsp = Init Key Key Nonce | Encrypt Message Offset | Complete
--data Req = IReady | EReady | ReadyEmit Ciphertext | Busy

--The salsa ReactT
rot7 :: W32 -> W32
rot7 (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) =
      ((((((((((((((((((((((((((((((((W32 b24) b23) b22) b21) b20) b19) b18) b17) b16) b15) b14) b13) b12) b11) b10) b9) b8) b7) b6) b5) b4) b3) b2) b1) b0) b31) b30) b29) b28) b27) b26) b25)

rot9 :: W32 -> W32
rot9 (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) =
      ((((((((((((((((((((((((((((((((W32 b22) b21) b20) b19) b18) b17) b16) b15) b14) b13) b12) b11) b10) b9) b8) b7) b6) b5) b4) b3) b2) b1) b0) b31) b30) b29) b28) b27) b26) b25) b24) b23)

rot13 :: W32 -> W32
rot13 (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) =
      ((((((((((((((((((((((((((((((((W32 b18) b17) b16) b15) b14) b13) b12) b11) b10) b9) b8) b7) b6) b5) b4) b3) b2) b1) b0) b31) b30) b29) b28) b27) b26) b25) b24) b23) b22) b21) b20) b19)

rot18 :: W32 -> W32
rot18 (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) =
      ((((((((((((((((((((((((((((((((W32 b13) b12) b11) b10) b9) b8) b7) b6) b5) b4) b3) b2) b1) b0) b31) b30) b29) b28) b27) b26) b25) b24) b23) b22) b21) b20) b19) b18) b17) b16) b15) b14)

littleendian :: (W8, W8, W8, W8) -> W32
littleendian ( (W8 b7  b6  b5  b4  b3  b2  b1  b0)
             , (W8 b15 b14 b13 b12 b11 b10 b9  b8)
             , (W8 b23 b22 b21 b20 b19 b18 b17 b16)
             , (W8 b31 b30 b29 b28 b27 b26 b25 b24)
             ) = (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)

littleendianp :: W32 -> (W8, W8, W8, W8)
littleendianp (W32 a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 a0) =
      ((((Tuple4
      ((((((((W8  a7) --first
                 a6)
                a5)
               a4)
              a3)
             a2)
            a1)
           a0))

       ((((((((W8  a15) -- second
                  a14)
                 a13)
                a12)
               a11)
              a10)
             a9)
            a8))

        ((((((((W8  a23) -- third
                   a22)
                  a21)
                 a20)
                a19)
               a18)
              a17)
             a16))

         ((((((((W8  a31) --fourth
                    a30)
                   a29)
                  a28)
                 a27)
                a26)
               a25)
              a24))

expwords :: Words16 W32 -> Bytes64
expwords (Words16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) =
      let   x0  = littleendianp a0 in
      let   x1  = littleendianp a1 in
      let   x2  = littleendianp a2 in
      let   x3  = littleendianp a3 in
      let   x4  = littleendianp a4 in
      let   x5  = littleendianp a5 in
      let   x6  = littleendianp a6 in
      let   x7  = littleendianp a7 in
      let   x8  = littleendianp a8 in
      let   x9  = littleendianp a9 in
      let   x10 = littleendianp a10 in
      let   x11 = littleendianp a11 in
      let   x12 = littleendianp a12 in
      let   x13 = littleendianp a13 in
      let   x14 = littleendianp a14 in
      let   x15 = littleendianp a15 in
      case Words16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 of {
            Words16
            (Tuple4 b0 b1 b2 b3)
            (Tuple4 b4 b5 b6 b7)
            (Tuple4 b8 b9 b10 b11)
            (Tuple4 b12 b13 b14 b15)
            (Tuple4 b16 b17 b18 b19)
            (Tuple4 b20 b21 b22 b23)
            (Tuple4 b24 b25 b26 b27)
            (Tuple4 b28 b29 b30 b31)
            (Tuple4 b32 b33 b34 b35)
            (Tuple4 b36 b37 b38 b39)
            (Tuple4 b40 b41 b42 b43)
            (Tuple4 b44 b45 b46 b47)
            (Tuple4 b48 b49 b50 b51)
            (Tuple4 b52 b53 b54 b55)
            (Tuple4 b56 b57 b58 b59)
            (Tuple4 b60 b61 b62 b63) -> Bytes64 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b32 b33 b34 b35 b36 b37 b38 b39 b40 b41 b42 b43 b44 b45 b46 b47 b48 b49 b50 b51 b52 b53 b54 b55 b56 b57 b58 b59 b60 b61 b62 b63
      }


impwords :: Bytes64 -> (Words16 W32)
impwords (Bytes64 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16
                  a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31
                  a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46
                  a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63) =
      let b0 = littleendian (Tuple4 a0 a1 a2 a3) in
      let b1 = littleendian (Tuple4 a4 a5 a6 a7) in
      let b2 = littleendian (Tuple4 a8 a9 a10 a11) in
      let b3 = littleendian (Tuple4 a12 a13 a14 a15) in
      let b4 = littleendian (Tuple4 a16 a17 a18 a19) in
      let b5 = littleendian (Tuple4 a20 a21 a22 a23) in
      let b6 = littleendian (Tuple4 a24 a25 a26 a27) in
      let b7 = littleendian (Tuple4 a28 a29 a30 a31) in
      let b8 = littleendian (Tuple4 a32 a33 a34 a35) in
      let b9 = littleendian (Tuple4 a36 a37 a38 a39) in
      let b10 = littleendian (Tuple4 a40 a41 a42 a43) in
      let b11 = littleendian (Tuple4 a44 a45 a46 a47) in
      let b12 = littleendian (Tuple4 a48 a49 a50 a51) in
      let b13 = littleendian (Tuple4 a52 a53 a54 a55) in
      let b14 = littleendian (Tuple4 a56 a57 a58 a59) in
      let b15 = littleendian (Tuple4 a60 a61 a62 a63) in
      Words16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15

quarterRound :: (W32, W32, W32, W32) -> (W32, W32, W32, W32)
quarterRound (y0,y1,y2,y3) = let z1 = xor32 y1 (rot7 (add32 y0 y3))  in
                             let z2 = xor32 y2 (rot9 (add32 z1 y0))  in
                             let z3 = xor32 y3 (rot13 (add32 z2 z1)) in
                             let z0 = xor32 y0 (rot18 (add32 z3 z2)) in
                             (z0,z1,z2,z3)

rowRound :: Words16 W32 -> Words16 W32
rowRound (Words16 y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) =
      let x0 = quarterRound(y0,y1,y2,y3) in
      let x1 = quarterRound(y5,y6,y7,y4) in
      let x2 = quarterRound(y10,y11,y8,y9) in
      let x3 = quarterRound(y15,y12,y13,y14) in
      case (x0, x1, x2, x3) of {
            ( (z0,  z1,  z2,  z3)
            , (z5,  z6,  z7,  z4)
            , (z10, z11, z8,  z9)
            , (z15, z12, z13, z14)
            ) -> Words16 z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15
      }

columnRound :: Words16 W32 -> Words16 W32
columnRound (Words16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) =
      let z0 = quarterRound(x0,x4,x8,x12) in
      let z1 = quarterRound(x5,x9,x13,x1) in
      let z2 = quarterRound(x10,x14,x2,x6) in
      let z3 = quarterRound(x15,x3,x7,x11) in
      case (z0, z1, z2, z3) of {
            ( (y0,  y4,  y8,  y12)
            , (y5,  y9,  y13, y1)
            , (y10, y14, y2,  y6)
            , (y15, y3,  y7,  y11)
            ) -> (Words16 y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15)
      }

doubleRound :: Words16 W32 -> Words16 W32
doubleRound x = rowRound (columnRound x)

salsaHashp :: Words16 W32 -> Words16 W32
salsaHashp (Words16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) =
      case doubleRound(doubleRound(doubleRound(doubleRound(doubleRound(doubleRound(doubleRound(doubleRound(doubleRound(doubleRound x))))))))) of
            (Words16 z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15) -> Words16
                                                                                    (add32 z0 x0)
                                                                                    (add32 z1 x1)
                                                                                    (add32 z2 x2)
                                                                                    (add32 z3 x3)
                                                                                    (add32 z4 x4)
                                                                                    (add32 z5 x5)
                                                                                    (add32 z6 x6)
                                                                                    (add32 z7 x7)
                                                                                    (add32 z8 x8)
                                                                                    (add32 z9 x9)
                                                                                    (add32 z10 x10)
                                                                                    (add32 z11 x11)
                                                                                    (add32 z12 x12)
                                                                                    (add32 z13 x13)
                                                                                    (add32 z14 x14)
                                                                                    (add32 z15 x15)

salsaHash :: Bytes64 -> Bytes64
salsaHash x = expwords (salsaHashp (impwords x))

--Build a 256-bit frame
--Includes the sigma paddings defined in the Salsa20 spec by Berstein
--This function should be PE'd up in the hand-cranked PE process
buildSalsa256 :: Bytes16 -> Bytes16 -> Bytes8 -> Bytes8 -> Bytes64
buildSalsa256 (Bytes16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
              (Bytes16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
              (Bytes8  n0 n1 n2 n3 n4 n5 n6 n7)
              (Bytes8 n8 n9 n10 n11 n12 n13 n14 n15) = case (sigma0, sigma1, sigma2, sigma3) of {
                        ( (s0,s1,s2,s3)
                        , (s4,s5,s6,s7)
                        , (s8,s9,s10,s11)
                        , (s12,s13,s14,s15)
                        ) -> salsaHash (Bytes64 s0  s1  s2  s3  a0  a1  a2  a3  a4 a5 a6 a7  a8 a9 a10 a11
                                    a12 a13 a14 a15 s4  s5  s6  s7  n0 n1 n2 n3  n4  n5  n6 n7
                                    n8  n9  n10 n11 n12 n13 n14 n15 s8 s9 s10 s11 b0  b1  b2 b3
                                    b4 b5 b6  b7 b8  b9  b10 b11 b12 b13 b14 b15 s12 s13 s14 s15)
              }


buildFrame :: Bytes16 -> Bytes16 -> Bytes8 -> Bytes8 -> Bytes64
buildFrame (Bytes16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
           (Bytes16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
           (Bytes8  n0 n1 n2 n3 n4 n5 n6 n7)
           (Bytes8 n8 n9 n10 n11 n12 n13 n14 n15) = case (sigma0, sigma1, sigma2, sigma3) of {
                  ( (s0,s1,s2,s3)
                  , (s4,s5,s6,s7)
                  , (s8,s9,s10,s11)
                  , (s12,s13,s14,s15)
                  ) -> Bytes64 s0  s1  s2  s3  a0  a1  a2  a3  a4 a5 a6 a7  a8 a9 a10 a11
                              a12 a13 a14 a15 s4  s5  s6  s7  n0 n1 n2 n3  n4  n5  n6 n7
                              n8  n9  n10 n11 n12 n13 n14 n15 s8 s9 s10 s11 b0  b1  b2 b3
                              b4 b5 b6  b7 b8  b9  b10 b11 b12 b13 b14 b15 s12 s13 s14 s15
           }

key1 :: Bytes16
key1 = Bytes16 (W8 High Low Low Low Low Low Low Low) (W8 Low High Low Low Low Low Low Low) (W8 High High Low Low Low Low Low Low) (W8 Low Low High Low Low Low Low Low) (W8 High Low High Low Low Low Low Low) (W8 Low High High Low Low Low Low Low) (W8 High High High Low Low Low Low Low) (W8 Low Low Low High Low Low Low Low) (W8 High Low Low High Low Low Low Low) (W8 Low High Low High Low Low Low Low) (W8 High High Low High Low Low Low Low) (W8 Low Low High High Low Low Low Low) (W8 High Low High High Low Low Low Low) (W8 Low High High High Low Low Low Low) (W8 High High High High Low Low Low Low) (W8 Low Low Low Low High Low Low Low)

key2 :: Bytes16
key2 = Bytes16 (W8 High Low Low Low High Low Low Low) (W8 Low High Low Low High Low Low Low) (W8 High High Low Low High Low Low Low) (W8 Low Low High Low High Low Low Low) (W8 High Low High Low High Low Low Low) (W8 Low High High Low High Low Low Low) (W8 High High High Low High Low Low Low) (W8 Low Low Low High High Low Low Low) (W8 High Low Low High High Low Low Low) (W8 Low High Low High High Low Low Low) (W8 High High Low High High Low Low Low) (W8 Low Low High High High Low Low Low) (W8 High Low High High High Low Low Low) (W8 Low High High High High Low Low Low) (W8 High High High High High Low Low Low) (W8 Low Low Low Low Low High Low Low)

zerothoutput :: Bytes64
zerothoutput = Bytes64 (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low) (W8 Low Low Low Low Low Low Low Low)

zerothi :: Words16 W32
zerothi = Words16 (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low) (W32 Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low Low)

pipeline :: ReT i z I a -> ReT z o I a -> ReT i o I a
pipeline left right = refold snd pipe (parI left right)

snd :: (a, b) -> b
snd (_, b) = b

pipe :: (a, b) -> c -> (c, a)
pipe (a, _) newinp = (newinp, a)

dri :: (Words16 W32, Words16 W32) -> ReT (Words16 W32, Words16 W32) (Words16 W32, Words16 W32) I ()
dri (hashed, pass) = signal ((doubleRound hashed), pass) >>= dri

ddev :: ReT (Words16 W32, Words16 W32) (Words16 W32, Words16 W32) I ()
ddev = signal (zerothi, zerothi) >>= dri

par10 :: ( (Words16 W32, Words16 W32)
         , ( (Words16 W32, Words16 W32)
           , ( (Words16 W32, Words16 W32)
             , ( (Words16 W32, Words16 W32)
               , ( (Words16 W32, Words16 W32)
                 , ( (Words16 W32, Words16 W32)
                   , ( (Words16 W32, Words16 W32)
                     , ( (Words16 W32, Words16 W32)
                       , ( (Words16 W32, Words16 W32)
                         , ( Words16 W32, Words16 W32)))))))))) -> ParTen
par10 (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) = ParTen a b c d e f g h i j

dePar10 :: ParTen -> (Tuple2 ((Tuple2 (Words16 W32) (Words16 W32)))
                      (Tuple2 ((Tuple2 (Words16 W32) (Words16 W32)))
                       (Tuple2 ((Tuple2 (Words16 W32) (Words16 W32)))
                        (Tuple2 ((Tuple2 (Words16 W32) (Words16 W32)))
                         (Tuple2 ((Tuple2 (Words16 W32) (Words16 W32)))
                          (Tuple2 ((Tuple2 (Words16 W32) (Words16 W32)))
                           (Tuple2 ((Tuple2 (Words16 W32) (Words16 W32)))
                            (Tuple2 ((Tuple2 (Words16 W32) (Words16 W32)))
                             (Tuple2 ((Tuple2 (Words16 W32) (Words16 W32))) ((Tuple2 (Words16 W32) (Words16 W32))))))))))))
dePar10 (ParTen a b c d e f g h i j) = (a, (b, (c, (d, (e, (f, (g, (h, (i, j)))))))))

ddevP :: ParTen -> (Tuple2 Bytes8 Bytes8) -> ParTen
ddevP (ParTen a b c d e f g h i nope) (Tuple2 b0 b1) =
      let x = buildFrame key1 key2 b0 b1 in
      let y = impwords x in
      let z = Tuple2 y y
      in (ParTen z a b c d e f g h i)

pipeout :: (Words16 W32, Words16 W32) -> Bytes64
pipeout ((Words16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15), (Words16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)) =
      expwords (Words16 (add32 a0 b0)
                        (add32 a1 b1)
                        (add32 a2 b2)
                        (add32 a3 b3)
                        (add32 a4 b4)
                        (add32 a5 b5)
                        (add32 a6 b6)
                        (add32 a7 b7)
                        (add32 a8 b8)
                        (add32 a9 b9)
                        (add32 a10 b10)
                        (add32 a11 b11)
                        (add32 a12 b12)
                        (add32 a13 b13)
                        (add32 a14 b14)
                        (add32 a15 b15))


ddevOutput :: ParTen -> Bytes64
ddevOutput (ParTen a b c d e f g h i this) = pipeout this

--salsa20 :: ReT (Tuple2 Bytes8 Bytes8) Bytes64 I ()
salsa20 :: ReT (Words16 W32, Words16 W32) (Words16 W32, Words16 W32) I ()
salsa20 = refold (\out -> pipeout out) (\out -> \inp -> case (old, inp) of { _ -> undefined } )
          (pipeline (pipeline (pipeline (pipeline ddev ddev)
                                        (pipeline ddev ddev))
                              (pipeline (pipeline ddev ddev)
                                        (pipeline ddev ddev)))
                    (pipeline ddev ddev))


{-
salsa20 :: ReT (Tuple2 Bytes8 Bytes8) Bytes64 I ()
is
  refold
         (
            \out -> ddevOutput (par10 out)
         )

         (
            \old -> \inp -> dePar10 (ddevP (par10 old) inp)
         )
         (parI ddev
          (parI ddev
          (parI ddev
          (parI ddev
          (parI ddev
          (parI ddev
          (parI ddev
          (parI ddev
           (parI ddev ddev)
          ))))))))
end

start :: ReT (Tuple2 Bytes8 Bytes8) Bytes64 I ()
is
  salsa20
end
-}

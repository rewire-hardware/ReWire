module Salsa20Cipher where
import Control.Monad.Resumption.Reactive 
--import Prelude (undefined,Num)
import Control.Monad.Identity

import Data.Bits

--Type shimming
type Id = Identity

--add function should primitive in VHDL
add8 :: W8 -> W8 -> W8
add8 = undefined

add32 :: W32 -> W32 -> W32
add32 = undefined

xor32 :: W32 -> W32 -> W32
xor32 = undefined
--End type shimming

data Bit = High | Low deriving Show
data W8      = W8  Bit Bit Bit Bit Bit Bit Bit Bit deriving Show
data W16     = W16 Bit Bit Bit Bit Bit Bit Bit Bit
                   Bit Bit Bit Bit Bit Bit Bit Bit 
data W32     = W32 Bit Bit Bit Bit Bit Bit Bit Bit
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
--Making my life a little easier for testing
{-
instance Num W8 where
   fromInteger n = W8 (bbit 0) (bbit 1) (bbit 2) (bbit 3) (bbit 4) (bbit 5) (bbit 6) (bbit 7)
                  
    where
      bool2bit True  = High
      bool2bit False = Low

      bbit = bool2bit . (testBit n)
-}   


--Constant Values for initialization

--(101,120,112,97)
sigma0 = (W8 High Low High Low Low High High Low,W8 Low Low Low High High High High Low,W8 Low Low Low Low High High High Low,W8 High Low Low Low Low High High Low)
--(110,100,32,51)
sigma1 = (W8 Low High High High Low High High Low,W8 Low Low High Low Low High High Low,W8 Low Low Low Low Low High Low Low,W8 High High Low Low High High Low Low) 
--(50,45,98,121)
sigma2 = (W8 Low High Low Low High High Low Low,W8 High Low High High Low High Low Low,W8 Low High Low Low Low High High Low,W8 High Low Low High High High High Low)
--(116,101,32,107)
sigma3 = (W8 Low Low High Low High High High Low,W8 High Low High Low Low High High Low,W8 Low Low Low Low Low High Low Low,W8 High High Low High Low High High Low)

--(101,120,112,97)
tau0 = (W8 High Low High Low Low High High Low,W8 Low Low Low High High High High Low,W8 Low Low Low Low High High High Low,W8 High Low Low Low Low High High Low)
--(110,100,32,49)
tau1 = (W8 Low High High High Low High High Low,W8 Low Low High Low Low High High Low,W8 Low Low Low Low Low High Low Low,W8 High Low Low Low High High Low Low)
--(54,45,98,121)
tau2 = (W8 Low High High Low High High Low Low,W8 High Low High High Low High Low Low,W8 Low High Low Low Low High High Low,W8 High Low Low High High High High Low)
--(116,101,32,107)
tau3 = (W8 Low Low High Low High High High Low,W8 High Low High Low Low High High Low,W8 Low Low Low Low Low High Low Low,W8 High High Low High Low High High Low)

--Primitives
type Message = Bytes64
type Key = Bytes16
type Nonce = Bytes8
type Count = Bytes8
type Offset = Count
type Ciphertext = Bytes64

--Machine Messages
data Rsp = Init Key Key Nonce | Encrypt Message Offset | Complete
data Req = IReady | EReady | ReadyEmit Ciphertext | Busy

--The salsa ReactT
type ReSalsa = ReacT Req Rsp Id

rot7  (W32 b0  b1  b2  b3  b4  b5  b6  b7  
           b8  b9  b10 b11 b12 b13 b14 b15 
           b16 b17 b18 b19 b20 b21 b22 b23 
           b24 b25 b26 b27 b28 b29 b30 b31) = W32 b7 b8 b9 b10 b11 b12 b13 b14 
                                                  b15 b16 b17 b18 b19 b20 b21 b22 
                                                  b23 b24 b25 b26 b27 b28 b29 b30 
                                                  b31 b0 b1 b2 b3 b4 b5 b6
rot9  (W32 b0  b1  b2  b3  b4  b5  b6  b7  
           b8  b9  b10 b11 b12 b13 b14 b15 
           b16 b17 b18 b19 b20 b21 b22 b23 
           b24 b25 b26 b27 b28 b29 b30 b31) = W32 b9  b10 b11 b12 b13 b14 b15 b16
                                                  b17 b18 b19 b20 b21 b22 b23 b24 
                                                  b25 b26 b27 b28 b29 b30 b31 b0
                                                  b1  b2  b3  b4  b5  b6  b7  b8
rot13  (W32 b0  b1  b2  b3  b4  b5  b6  b7  
            b8  b9  b10 b11 b12 b13 b14 b15 
            b16 b17 b18 b19 b20 b21 b22 b23 
            b24 b25 b26 b27 b28 b29 b30 b31) = W32 b13 b14 b15 b16 b17 b18 b19 b20
                                                   b21 b22 b23 b24 b25 b26 b27 b28
                                                   b29 b30 b31 b0  b1  b2  b3  b4
                                                   b5  b6  b7  b8  b9  b10 b11 b12
rot18  (W32 b0  b1  b2  b3  b4  b5  b6  b7  
            b8  b9  b10 b11 b12 b13 b14 b15 
            b16 b17 b18 b19 b20 b21 b22 b23 
            b24 b25 b26 b27 b28 b29 b30 b31) = W32 b18 b19 b20 b21 b22 b23 b24 b25
                                                   b26 b27 b28 b29 b30 b31 b0  b1
                                                   b2  b3  b4  b5  b6  b7  b8  b9 
                                                   b10 b11 b12 b13 b14 b15 b16 b17
expwords :: (Words16 W32) -> Bytes64
expwords (Words16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) = let 
                                                                                (b0,b1,b2,b3)       = littleendian' a0
                                                                                (b4,b5,b6,b7)       = littleendian' a1
                                                                                (b8,b9,b10,b11)     = littleendian' a2
                                                                                (b12,b13,b14,b15)   = littleendian' a3
                                                                                (b16,b17,b18,b19)   = littleendian' a4
                                                                                (b20,b21,b22,b23)   = littleendian' a5
                                                                                (b24,b25,b26,b27)   = littleendian' a6
                                                                                (b28,b29,b30,b31)   = littleendian' a7
                                                                                (b32,b33,b34,b35)   = littleendian' a8
                                                                                (b36,b37,b38,b39)   = littleendian' a9
                                                                                (b40,b41,b42,b43)   = littleendian' a10
                                                                                (b44,b45,b46,b47)   = littleendian' a11
                                                                                (b48,b49,b50,b51)   = littleendian' a12
                                                                                (b52,b53,b54,b55)   = littleendian' a13
                                                                                (b56,b57,b58,b59)   = littleendian' a14
                                                                                (b60,b61,b62,b63)   = littleendian' a15
                                                                      in Bytes64 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b32 b33 b34 b35 b36 b37 b38 b39 b40 b41 b42 b43 b44 b45 b46 b47 b48 b49 b50 b51 b52 b53 b54 b55 b56 b57 b58 b59 b60 b61 b62 b63

impwords :: Bytes64 -> (Words16 W32)
impwords (Bytes64 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 
                  a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 
                  a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 
                  a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63) = let 
                                                                                                      b0 = littleendian (a0,a1,a2,a3)       
                                                                                                      b1 = littleendian (a4,a5,a6,a7)       
                                                                                                      b2 = littleendian (a8,a9,a10,a11)     
                                                                                                      b3 = littleendian (a12,a13,a14,a15)   
                                                                                                      b4 = littleendian (a16,a17,a18,a19)   
                                                                                                      b5 = littleendian (a20,a21,a22,a23)   
                                                                                                      b6 = littleendian (a24,a25,a26,a27)   
                                                                                                      b7 = littleendian (a28,a29,a30,a31)   
                                                                                                      b8 = littleendian (a32,a33,a34,a35)   
                                                                                                      b9 = littleendian (a36,a37,a38,a39)   
                                                                                                      b10 = littleendian (a40,a41,a42,a43)   
                                                                                                      b11 = littleendian (a44,a45,a46,a47)   
                                                                                                      b12 = littleendian (a48,a49,a50,a51)   
                                                                                                      b13 = littleendian (a52,a53,a54,a55)   
                                                                                                      b14 = littleendian (a56,a57,a58,a59)   
                                                                                                      b15 = littleendian (a60,a61,a62,a63)   
                                                                                          in Words16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15

w8to16le :: W8 -> W8 -> W16
w8to16le (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 b8 b9 b10 b11 b12 b13 b14 b15) = W16 b8 b9 b10 b11 b12 b13 b14 b15 b0 b1 b2 b3 b4 b5 b6 b7

w16tow32le :: W16 -> W16 -> W32
w16tow32le (W16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) (W16 c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15) = W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
                                                                                                                                        c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15
--Converting 4-bytes in ascending order to a 32-bit little endian word 
littleendian :: (W8,W8,W8,W8) -> W32
littleendian (b0,b1,b2,b3) = w16tow32le (w8to16le b3 b2) (w8to16le b1 b0)

--The numbers are reversed.  The "bit vector" is "big endian" or the leftmost bit is the most significant bit.
littleendian' :: W32 -> (W8,W8,W8,W8)
littleendian' (W32 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31) =
              (W8 a24 a25 a26 a27 a28 a29 a30 a31,W8 a16 a17 a18 a19 a20 a21 a22 a23,W8 a8 a9 a10 a11 a12 a13 a14 a15, W8 a0 a1 a2 a3 a4 a5 a6 a7)


quarterRound :: (W32,W32,W32,W32) -> (W32,W32,W32,W32)
quarterRound (y0,y1,y2,y3) = let z1 = xor32 y1 (rot7 (add32 y0 y3))
                                 z2 = xor32 y2 (rot9 (add32 z1 y0))
                                 z3 = xor32 y3 (rot13 (add32 z2 z1))
                                 z0 = xor32 y0 (rot18 (add32 z3 z2))
                              in (z0,z1,z2,z3)

rowRound :: Words16 W32 -> Words16 W32
rowRound (Words16 y0 y1 y2 y3
          y4 y5 y6 y7
          y8 y9 y10 y11
          y12 y13 y14 y15) = let (z0,z1,z2,z3)     = quarterRound(y0,y1,y2,y3)
                                 (z5,z6,z7,z4)     = quarterRound(y5,y6,y7,y4)
                                 (z10,z11,z8,z9)   = quarterRound(y10,y11,y8,y9)
                                 (z15,z12,z13,z14) = quarterRound(y15,y12,y13,y14)
                            in (Words16 z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15)

columnRound :: Words16 W32 -> Words16 W32
columnRound (Words16 x0 x1 x2 x3
             x4 x5 x6 x7
             x8 x9 x10 x11
             x12 x13 x14 x15) = let (y0,y4,y8,y12)  = quarterRound(x0,x4,x8,x12)
                                    (y5,y9,y13,y1)  = quarterRound(x5,x9,x13,x1)
                                    (y10,y14,y2,y6) = quarterRound(x10,x14,x2,x6)
                                    (y15,y3,y7,y11) = quarterRound(x15,x3,x7,x11)
                              in (Words16 y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15)


doubleRound :: Words16 W32 -> Words16 W32
doubleRound x = rowRound(columnRound(x))


salsaHash' :: Words16 W32 -> Words16 W32
salsaHash' x@(Words16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) =
                  let (Words16 z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15) = doubleRound(doubleRound(doubleRound(doubleRound(doubleRound(doubleRound(doubleRound(doubleRound(doubleRound(doubleRound(x))))))))))
               in Words16
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
salsaHash x = expwords (salsaHash' (impwords x))

--Build a 256-bit frame
--Includes the sigma paddings defined in the Salsa20 spec by Berstein
--This function should be PE'd up in the hand-cranked PE process
buildSalsa256 :: Bytes16 -> Bytes16 -> Bytes8 -> Bytes8 -> Bytes64
buildSalsa256 (Bytes16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) 
              (Bytes16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) 
              (Bytes8  n0 n1 n2 n3 n4 n5 n6 n7) (Bytes8 n8 n9 n10 n11 n12 n13 n14 n15) = let (s0,s1,s2,s3)     = sigma0
                                                                                             (s4,s5,s6,s7)     = sigma1
                                                                                             (s8,s9,s10,s11)   = sigma2
                                                                                             (s12,s13,s14,s15) = sigma3
                                                                                 in 
    salsaHash $
      Bytes64 s0  s1  s2  s3  a0  a1  a2  a3  a4 a5 a6 a7  a8 a9 a10 a11 
              a12 a13 a14 a15 s4  s5  s6  s7  n0 n1 n2 n3  n4  n5  n6 n7
              n8  n9  n10 n11 n12 n13 n14 n15 s8 s9 s10 s11 b0  b1  b2 b3 
              b4 b5 b6  b7 b8  b9  b10 b11 b12 b13 b14 b15 s12 s13 s14 s15 
{-
--These are the device functions
salsaInit :: ReSalsa ()
salsaInit = signal IReady >>= (\rsp -> case rsp of
                                             Init k0 k1 n -> salsaEncrypt k0 k1 n
                                             _            -> salsaInit) -- Errors reset the state of the encryptor 

salsaEncrypt :: Key -> Key -> Nonce -> ReSalsa ()
salsaEncrypt k0 k1 n = signal EReady >>= (\rsp -> case rsp of 
                                                        Complete     -> salsaInit
                                                        Encrypt m o  -> return (encrypt256 m k0 k1 n o) >>= \e -> salsaEmitEncrypt k0 k1 n e
                                                        _            -> salsaInit) --Errors reset the state of the encryptor 

salsaEmitEncrypt :: Key -> Key -> Nonce -> Ciphertext -> ReSalsa ()
salsaEmitEncrypt k0 k1 n ct = signal (ReadyEmit ct) >>= (\rsp -> case rsp of 
                                                                        Complete     -> salsaInit
                                                                        Encrypt m o  -> return (encrypt256 m k0 k1 n o) >>= \e -> salsaEmitEncrypt k0 k1 n e
                                                                        _            -> salsaInit) --Errors reset the state of the encryptor 
--End device functions

xor64 :: Bytes64 -> Bytes64 -> Bytes64
xor64 (Bytes64 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63) 
      (Bytes64 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b32 b33 b34 b35 b36 b37 b38 b39 b40 b41 b42 b43 b44 b45 b46 b47 b48 b49 b50 b51 b52 b53 b54 b55 b56 b57 b58 b59 b60 b61 b62 b63) = 
        Bytes64(a0 `xor` b0) (a1 `xor` b1) (a2 `xor` b2) (a3 `xor` b3) (a4 `xor` b4) (a5 `xor` b5) (a6 `xor` b6) (a7 `xor` b7) (a8 `xor` b8) (a9 `xor` b9) (a10 `xor` b10) (a11 `xor` b11) (a12 `xor` b12) (a13 `xor` b13) (a14 `xor` b14) (a15 `xor` b15) (a16 `xor` b16) (a17 `xor` b17) (a18 `xor` b18) (a19 `xor` b19) (a20 `xor` b20) (a21 `xor` b21) (a22 `xor` b22) (a23 `xor` b23) (a24 `xor` b24) (a25 `xor` b25) (a26 `xor` b26) (a27 `xor` b27) (a28 `xor` b28) (a29 `xor` b29) (a30 `xor` b30) (a31 `xor` b31) (a32 `xor` b32) (a33 `xor` b33) (a34 `xor` b34) (a35 `xor` b35) (a36 `xor` b36) (a37 `xor` b37) (a38 `xor` b38) (a39 `xor` b39) (a40 `xor` b40) (a41 `xor` b41) (a42 `xor` b42) (a43 `xor` b43) (a44 `xor` b44) (a45 `xor` b45) (a46 `xor` b46) (a47 `xor` b47) (a48 `xor` b48) (a49 `xor` b49) (a50 `xor` b50) (a51 `xor` b51) (a52 `xor` b52) (a53 `xor` b53) (a54 `xor` b54) (a55 `xor` b55) (a56 `xor` b56) (a57 `xor` b57) (a58 `xor` b58) (a59 `xor` b59) (a60 `xor` b60) (a61 `xor` b61) (a62 `xor` b62) (a63 `xor` b63)

salsaRot  :: StateT (Words16 W32) I ()
salsaRot = salsa >> salsa >> salsa >> salsa >> salsa >> salsa >> salsa >> salsa >> salsa >> salsa

salsa20 :: Bytes64 -> Bytes64
salsa20 x = let x'         = impwords x 
                (_,salsad) = deId $ deST salsaRot x'
             in expwords (addWords x' salsad)
              where
                 addWords (Words16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) 
                          (Words16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) = 
                            Words16 (a0+b0) #$ a1+b1 #$ a2+b2 #$ a3+b3 #$ a4+b4 #$ a5+b5 #$ a6+b6 #$ a7+b7 #$ a8+b8 #$ a9+b9 #$ a10+b10 #$ a11+b11 #$ a12+b12 #$ a13+b13 #$ a14+b14 #$ a15+b15

--Build a 256-bit frame
--Includes the sigma paddings defined in the Salsa20 spec by Berstein
buildSalsa256 :: Bytes16 -> Bytes16 -> Bytes16 -> Bytes64
buildSalsa256 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15) (n0,n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15) = salsa20 $
      Bytes64 101 120 112 97  a0  a1  a2  a3  a4 a5 a6 a7  a8 a9 a10 a11 
              a12 a13 a14 a15 110 100 32  51  n0 n1 n2 n3  n4  n5  n6 n7
              n8  n9  n10 n11 n12 n13 n14 n15 50 45 98 121 b0  b1  b2 b3 
              b4 b5 b6  b7 b8  b9  b10 b11 b12 b13 b14 b15 116 101 32 107

encrypt256 :: Message -> Key -> Key -> Nonce -> Count -> Ciphertext
encrypt256 m k0 k1 n c = let frame = buildSalsa256 k0 k1 n c 
                          in frame `xor64` m


-}

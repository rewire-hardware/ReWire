module RWPrelude where


data Bit = Zero | One
data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit
data W32 = W32 Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit

rotateR2,rotateR6,rotateR7,rotateR11,rotateR13,rotateR17,rotateR18,rotateR19,rotateR22,rotateR25 :: W32 -> W32
rotateR2 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b30 b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13
                  b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29)

rotateR6 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           =  (W32 b26 b27 b28 b29 b30 b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 
                   b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25)

rotateR7 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           =  (W32 b25 b26 b27 b28 b29 b30 b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 
                   b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24)

rotateR11 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b0 b1 b2 b3 b4 b5 
                  b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20)

rotateR13 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b0 b1 b2 b3 
                  b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18)

rotateR17 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31  
                  b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14)

rotateR18 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30
                  b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13)

rotateR19 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29
                  b30 b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)

rotateR22 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26
                  b27 b28 b29 b30 b31 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9)

rotateR25 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
               b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23
                  b24 b25 b26 b27 b28 b29 b30 b31 b0 b1 b2 b3 b4 b5 b6)

shiftR3,shiftR10 :: W32 -> W32
shiftR3 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
             b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
           = (W32 Zero Zero Zero b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12
                  b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28)

shiftR10 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
            = (W32 Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero b0 b1 b2 b3 b4 b5
                   b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21)

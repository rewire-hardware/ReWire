module MetaprogrammingRW where

import RWPrelude
{-
import Data.Bits
import Data.Word

genW32 :: String -> W32
genW32 w32 = W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 ca cb cc cd ce cf
  where [(b0,b1,b2,b3),(b4,b5,b6,b7),(b8,b9,ba,bb),(bc,bd,be,bf),(c0,c1,c2,c3),(c4,c5,c6,c7),(c8,c9,ca,cb),(cc,cd,ce,cf)] = map trans w32

trans :: Char -> (Bit, Bit, Bit, Bit)
trans '0' = (Zero,Zero,Zero,Zero)
trans '1' = (Zero,Zero,Zero,One)
trans '2' = (Zero,Zero,One,Zero)
trans '3' = (Zero,Zero,One,One)
trans '4' = (Zero,One,Zero,Zero)
trans '5' = (Zero,One,Zero,One)
trans '6' = (Zero,One,One,Zero)
trans '7' = (Zero,One,One,One)
trans '8' = (One,Zero,Zero,Zero)
trans '9' = (One,Zero,Zero,One)
trans 'a' = (One,Zero,One,Zero)
trans 'b' = (One,Zero,One,One)
trans 'c' = (One,One,Zero,Zero)
trans 'd' = (One,One,Zero,One)
trans 'e' = (One,One,One,Zero)
trans 'f' = (One,One,One,One)

grunt s32 = print (x32 ++ " = " ++ show w32)
  where w32 = genW32 s32
        x32 = "w" ++ s32

b :: Bit -> Word32
b Zero = 0
b One  = 1

-- used this to check that the grunt encoding works. It seems to, tested it on five cases or so.
val :: W32 -> Word32
val (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 ca cb cc cd ce cf)
  = b cf + (b ce * 2 ^ 1) + (b cd * 2 ^ 2) + (b cc * 2 ^ 3) + (b cb * 2 ^ 4) +
    (b ca * 2 ^ 5) + (b c9 * 2 ^ 6) + (b c8 * 2 ^ 7) + (b c7 * 2 ^ 8) + (b c6 * 2 ^ 9) +
    (b c5 * 2 ^ 10) + (b c4 * 2 ^ 11) + (b c3 * 2 ^ 12) + (b c2 * 2 ^ 13) + (b c1 * 2 ^ 14) +
    (b c0 * 2 ^ 15) +
    (b bf * 2 ^ 16) + (b be * 2 ^ 17) + (b bd * 2 ^ 18) + (b bc * 2 ^ 19) + (b bb * 2 ^ 20) +
    (b ba * 2 ^ 21) + (b b9 * 2 ^ 22) + (b b8 * 2 ^ 23) + (b b7 * 2 ^ 24) + (b b6 * 2 ^ 25) +
    (b b5 * 2 ^ 26) + (b b4 * 2 ^ 27) + (b b3 * 2 ^ 28) + (b b2 * 2 ^ 29) + (b b1 * 2 ^ 30) +
    (b b0 * 2 ^ 31)
-}

w00000000 :: W32
w00000000 = W32 Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero

{-
initialstateconsts = [ "6a09e667", "bb67ae85", "3c6ef372", "a54ff53a",
                         "510e527f", "9b05688c", "1f83d9ab", "5be0cd19" ]-}
                         
w6a09e667 :: W32
w6a09e667 = W32 Zero One One Zero One Zero One Zero Zero Zero Zero Zero One Zero Zero One One One One Zero Zero One One Zero Zero One One Zero Zero One One One

wbb67ae85 :: W32
wbb67ae85 = W32 One Zero One One One Zero One One Zero One One Zero Zero One One One One Zero One Zero One One One Zero One Zero Zero Zero Zero One Zero One

w3c6ef372 :: W32
w3c6ef372 = W32 Zero Zero One One One One Zero Zero Zero One One Zero One One One Zero One One One One Zero Zero One One Zero One One One Zero Zero One Zero

wa54ff53a :: W32
wa54ff53a = W32 One Zero One Zero Zero One Zero One Zero One Zero Zero One One One One One One One One Zero One Zero One Zero Zero One One One Zero One Zero

w510e527f :: W32
w510e527f = W32 Zero One Zero One Zero Zero Zero One Zero Zero Zero Zero One One One Zero Zero One Zero One Zero Zero One Zero Zero One One One One One One One

w9b05688c :: W32
w9b05688c = W32 One Zero Zero One One Zero One One Zero Zero Zero Zero Zero One Zero One Zero One One Zero One Zero Zero Zero One Zero Zero Zero One One Zero Zero

w1f83d9ab :: W32
w1f83d9ab = W32 Zero Zero Zero One One One One One One Zero Zero Zero Zero Zero One One One One Zero One One Zero Zero One One Zero One Zero One Zero One One

w5be0cd19 :: W32
w5be0cd19 = W32 Zero One Zero One One Zero One One One One One Zero Zero Zero Zero Zero One One Zero Zero One One Zero One Zero Zero Zero One One Zero Zero One

-- constants from genhash
{-
constantlist = [
           "428a2f98",
           "71374491",
           "b5c0fbcf",
           "e9b5dba5",
           "3956c25b",
           "59f111f1",
           "923f82a4",
           "ab1c5ed5",
           "d807aa98",
           "12835b01",
           "243185be",
           "550c7dc3",
           "72be5d74",
           "80deb1fe",
           "9bdc06a7",
           "c19bf174",
           "e49b69c1",
           "efbe4786",
           "0fc19dc6",
           "240ca1cc",
           "2de92c6f",
           "4a7484aa",
           "5cb0a9dc",
           "76f988da",
           "983e5152",
           "a831c66d",
           "b00327c8",
           "bf597fc7",
           "c6e00bf3",
           "d5a79147",
           "06ca6351",
           "14292967",
           "27b70a85",
           "2e1b2138",
           "4d2c6dfc",
           "53380d13",
           "650a7354",
           "766a0abb",
           "81c2c92e",
           "92722c85",
           "a2bfe8a1",
           "a81a664b",
           "c24b8b70",
           "c76c51a3",
           "d192e819",
           "d6990624",
           "f40e3585",
           "106aa070",
           "19a4c116",
           "1e376c08",
           "2748774c",
           "34b0bcb5",
           "391c0cb3",
           "4ed8aa4a",
           "5b9cca4f",
           "682e6ff3",
           "748f82ee",
           "78a5636f",
           "84c87814",
           "8cc70208",
           "90befffa",
           "a4506ceb",
           "bef9a3f7",
           "c67178f2"
           ]-}

w428a2f98 :: W32
w428a2f98 = W32 Zero One Zero Zero Zero Zero One Zero One Zero Zero Zero One Zero One Zero Zero Zero One Zero One One One One One Zero Zero One One Zero Zero Zero

w71374491 :: W32
w71374491 = W32 Zero One One One Zero Zero Zero One Zero Zero One One Zero One One One Zero One Zero Zero Zero One Zero Zero One Zero Zero One Zero Zero Zero One

wb5c0fbcf :: W32
wb5c0fbcf = W32 One Zero One One Zero One Zero One One One Zero Zero Zero Zero Zero Zero One One One One One Zero One One One One Zero Zero One One One One

we9b5dba5 :: W32
we9b5dba5 = W32 One One One Zero One Zero Zero One One Zero One One Zero One Zero One One One Zero One One Zero One One One Zero One Zero Zero One Zero One

w3956c25b :: W32
w3956c25b = W32 Zero Zero One One One Zero Zero One Zero One Zero One Zero One One Zero One One Zero Zero Zero Zero One Zero Zero One Zero One One Zero One One

w59f111f1 :: W32
w59f111f1 = W32 Zero One Zero One One Zero Zero One One One One One Zero Zero Zero One Zero Zero Zero One Zero Zero Zero One One One One One Zero Zero Zero One

w923f82a4 :: W32
w923f82a4 = W32 One Zero Zero One Zero Zero One Zero Zero Zero One One One One One One One Zero Zero Zero Zero Zero One Zero One Zero One Zero Zero One Zero Zero

wab1c5ed5 :: W32
wab1c5ed5 = W32 One Zero One Zero One Zero One One Zero Zero Zero One One One Zero Zero Zero One Zero One One One One Zero One One Zero One Zero One Zero One

wd807aa98 :: W32
wd807aa98 = W32 One One Zero One One Zero Zero Zero Zero Zero Zero Zero Zero One One One One Zero One Zero One Zero One Zero One Zero Zero One One Zero Zero Zero

w12835b01 :: W32
w12835b01 = W32 Zero Zero Zero One Zero Zero One Zero One Zero Zero Zero Zero Zero One One Zero One Zero One One Zero One One Zero Zero Zero Zero Zero Zero Zero One

w243185be :: W32
w243185be = W32 Zero Zero One Zero Zero One Zero Zero Zero Zero One One Zero Zero Zero One One Zero Zero Zero Zero One Zero One One Zero One One One One One Zero

w550c7dc3 :: W32
w550c7dc3 = W32 Zero One Zero One Zero One Zero One Zero Zero Zero Zero One One Zero Zero Zero One One One One One Zero One One One Zero Zero Zero Zero One One

w72be5d74 :: W32
w72be5d74 = W32 Zero One One One Zero Zero One Zero One Zero One One One One One Zero Zero One Zero One One One Zero One Zero One One One Zero One Zero Zero

w80deb1fe :: W32
w80deb1fe = W32 One Zero Zero Zero Zero Zero Zero Zero One One Zero One One One One Zero One Zero One One Zero Zero Zero One One One One One One One One Zero

w9bdc06a7 :: W32
w9bdc06a7 = W32 One Zero Zero One One Zero One One One One Zero One One One Zero Zero Zero Zero Zero Zero Zero One One Zero One Zero One Zero Zero One One One

wc19bf174 :: W32
wc19bf174 = W32 One One Zero Zero Zero Zero Zero One One Zero Zero One One Zero One One One One One One Zero Zero Zero One Zero One One One Zero One Zero Zero

we49b69c1 :: W32
we49b69c1 = W32 One One One Zero Zero One Zero Zero One Zero Zero One One Zero One One Zero One One Zero One Zero Zero One One One Zero Zero Zero Zero Zero One

wefbe4786 :: W32
wefbe4786 = W32 One One One Zero One One One One One Zero One One One One One Zero Zero One Zero Zero Zero One One One One Zero Zero Zero Zero One One Zero

w0fc19dc6 :: W32
w0fc19dc6 = W32 Zero Zero Zero Zero One One One One One One Zero Zero Zero Zero Zero One One Zero Zero One One One Zero One One One Zero Zero Zero One One Zero

w240ca1cc :: W32
w240ca1cc = W32 Zero Zero One Zero Zero One Zero Zero Zero Zero Zero Zero One One Zero Zero One Zero One Zero Zero Zero Zero One One One Zero Zero One One Zero Zero

w2de92c6f :: W32
w2de92c6f = W32 Zero Zero One Zero One One Zero One One One One Zero One Zero Zero One Zero Zero One Zero One One Zero Zero Zero One One Zero One One One One

w4a7484aa :: W32
w4a7484aa = W32 Zero One Zero Zero One Zero One Zero Zero One One One Zero One Zero Zero One Zero Zero Zero Zero One Zero Zero One Zero One Zero One Zero One Zero

w5cb0a9dc :: W32
w5cb0a9dc = W32 Zero One Zero One One One Zero Zero One Zero One One Zero Zero Zero Zero One Zero One Zero One Zero Zero One One One Zero One One One Zero Zero

w76f988da :: W32
w76f988da = W32 Zero One One One Zero One One Zero One One One One One Zero Zero One One Zero Zero Zero One Zero Zero Zero One One Zero One One Zero One Zero

w983e5152 :: W32
w983e5152 = W32 One Zero Zero One One Zero Zero Zero Zero Zero One One One One One Zero Zero One Zero One Zero Zero Zero One Zero One Zero One Zero Zero One Zero

wa831c66d :: W32
wa831c66d = W32 One Zero One Zero One Zero Zero Zero Zero Zero One One Zero Zero Zero One One One Zero Zero Zero One One Zero Zero One One Zero One One Zero One

wb00327c8 :: W32
wb00327c8 = W32 One Zero One One Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero One One Zero Zero One Zero Zero One One One One One Zero Zero One Zero Zero Zero

wbf597fc7 :: W32
wbf597fc7 = W32 One Zero One One One One One One Zero One Zero One One Zero Zero One Zero One One One One One One One One One Zero Zero Zero One One One

wc6e00bf3 :: W32
wc6e00bf3 = W32 One One Zero Zero Zero One One Zero One One One Zero Zero Zero Zero Zero Zero Zero Zero Zero One Zero One One One One One One Zero Zero One One

wd5a79147 :: W32
wd5a79147 = W32 One One Zero One Zero One Zero One One Zero One Zero Zero One One One One Zero Zero One Zero Zero Zero One Zero One Zero Zero Zero One One One

w06ca6351 :: W32
w06ca6351 = W32 Zero Zero Zero Zero Zero One One Zero One One Zero Zero One Zero One Zero Zero One One Zero Zero Zero One One Zero One Zero One Zero Zero Zero One

w14292967 :: W32
w14292967 = W32 Zero Zero Zero One Zero One Zero Zero Zero Zero One Zero One Zero Zero One Zero Zero One Zero One Zero Zero One Zero One One Zero Zero One One One

w27b70a85 :: W32
w27b70a85 = W32 Zero Zero One Zero Zero One One One One Zero One One Zero One One One Zero Zero Zero Zero One Zero One Zero One Zero Zero Zero Zero One Zero One

w2e1b2138 :: W32
w2e1b2138 = W32 Zero Zero One Zero One One One Zero Zero Zero Zero One One Zero One One Zero Zero One Zero Zero Zero Zero One Zero Zero One One One Zero Zero Zero

w4d2c6dfc :: W32
w4d2c6dfc = W32 Zero One Zero Zero One One Zero One Zero Zero One Zero One One Zero Zero Zero One One Zero One One Zero One One One One One One One Zero Zero

w53380d13 :: W32
w53380d13 = W32 Zero One Zero One Zero Zero One One Zero Zero One One One Zero Zero Zero Zero Zero Zero Zero One One Zero One Zero Zero Zero One Zero Zero One One

w650a7354 :: W32
w650a7354 = W32 Zero One One Zero Zero One Zero One Zero Zero Zero Zero One Zero One Zero Zero One One One Zero Zero One One Zero One Zero One Zero One Zero Zero

w766a0abb :: W32
w766a0abb = W32 Zero One One One Zero One One Zero Zero One One Zero One Zero One Zero Zero Zero Zero Zero One Zero One Zero One Zero One One One Zero One One

w81c2c92e :: W32
w81c2c92e = W32 One Zero Zero Zero Zero Zero Zero One One One Zero Zero Zero Zero One Zero One One Zero Zero One Zero Zero One Zero Zero One Zero One One One Zero

w92722c85 :: W32
w92722c85 = W32 One Zero Zero One Zero Zero One Zero Zero One One One Zero Zero One Zero Zero Zero One Zero One One Zero Zero One Zero Zero Zero Zero One Zero One

wa2bfe8a1 :: W32
wa2bfe8a1 = W32 One Zero One Zero Zero Zero One Zero One Zero One One One One One One One One One Zero One Zero Zero Zero One Zero One Zero Zero Zero Zero One

wa81a664b :: W32
wa81a664b = W32 One Zero One Zero One Zero Zero Zero Zero Zero Zero One One Zero One Zero Zero One One Zero Zero One One Zero Zero One Zero Zero One Zero One One

wc24b8b70 :: W32
wc24b8b70 = W32 One One Zero Zero Zero Zero One Zero Zero One Zero Zero One Zero One One One Zero Zero Zero One Zero One One Zero One One One Zero Zero Zero Zero

wc76c51a3 :: W32
wc76c51a3 = W32 One One Zero Zero Zero One One One Zero One One Zero One One Zero Zero Zero One Zero One Zero Zero Zero One One Zero One Zero Zero Zero One One

wd192e819 :: W32
wd192e819 = W32 One One Zero One Zero Zero Zero One One Zero Zero One Zero Zero One Zero One One One Zero One Zero Zero Zero Zero Zero Zero One One Zero Zero One

wd6990624 :: W32
wd6990624 = W32 One One Zero One Zero One One Zero One Zero Zero One One Zero Zero One Zero Zero Zero Zero Zero One One Zero Zero Zero One Zero Zero One Zero Zero

wf40e3585 :: W32
wf40e3585 = W32 One One One One Zero One Zero Zero Zero Zero Zero Zero One One One Zero Zero Zero One One Zero One Zero One One Zero Zero Zero Zero One Zero One

w106aa070 :: W32
w106aa070 = W32 Zero Zero Zero One Zero Zero Zero Zero Zero One One Zero One Zero One Zero One Zero One Zero Zero Zero Zero Zero Zero One One One Zero Zero Zero Zero

w19a4c116 :: W32
w19a4c116 = W32 Zero Zero Zero One One Zero Zero One One Zero One Zero Zero One Zero Zero One One Zero Zero Zero Zero Zero One Zero Zero Zero One Zero One One Zero

w1e376c08 :: W32
w1e376c08 = W32 Zero Zero Zero One One One One Zero Zero Zero One One Zero One One One Zero One One Zero One One Zero Zero Zero Zero Zero Zero One Zero Zero Zero

w2748774c :: W32
w2748774c = W32 Zero Zero One Zero Zero One One One Zero One Zero Zero One Zero Zero Zero Zero One One One Zero One One One Zero One Zero Zero One One Zero Zero

w34b0bcb5 :: W32
w34b0bcb5 = W32 Zero Zero One One Zero One Zero Zero One Zero One One Zero Zero Zero Zero One Zero One One One One Zero Zero One Zero One One Zero One Zero One

w391c0cb3 :: W32
w391c0cb3 = W32 Zero Zero One One One Zero Zero One Zero Zero Zero One One One Zero Zero Zero Zero Zero Zero One One Zero Zero One Zero One One Zero Zero One One

w4ed8aa4a :: W32
w4ed8aa4a = W32 Zero One Zero Zero One One One Zero One One Zero One One Zero Zero Zero One Zero One Zero One Zero One Zero Zero One Zero Zero One Zero One Zero

w5b9cca4f :: W32
w5b9cca4f = W32 Zero One Zero One One Zero One One One Zero Zero One One One Zero Zero One One Zero Zero One Zero One Zero Zero One Zero Zero One One One One

w682e6ff3 :: W32
w682e6ff3 = W32 Zero One One Zero One Zero Zero Zero Zero Zero One Zero One One One Zero Zero One One Zero One One One One One One One One Zero Zero One One

w748f82ee :: W32
w748f82ee = W32 Zero One One One Zero One Zero Zero One Zero Zero Zero One One One One One Zero Zero Zero Zero Zero One Zero One One One Zero One One One Zero

w78a5636f :: W32
w78a5636f = W32 Zero One One One One Zero Zero Zero One Zero One Zero Zero One Zero One Zero One One Zero Zero Zero One One Zero One One Zero One One One One

w84c87814 :: W32
w84c87814 = W32 One Zero Zero Zero Zero One Zero Zero One One Zero Zero One Zero Zero Zero Zero One One One One Zero Zero Zero Zero Zero Zero One Zero One Zero Zero

w8cc70208 :: W32
w8cc70208 = W32 One Zero Zero Zero One One Zero Zero One One Zero Zero Zero One One One Zero Zero Zero Zero Zero Zero One Zero Zero Zero Zero Zero One Zero Zero Zero

w90befffa :: W32
w90befffa = W32 One Zero Zero One Zero Zero Zero Zero One Zero One One One One One Zero One One One One One One One One One One One One One Zero One Zero

wa4506ceb :: W32
wa4506ceb = W32 One Zero One Zero Zero One Zero Zero Zero One Zero One Zero Zero Zero Zero Zero One One Zero One One Zero Zero One One One Zero One Zero One One

wbef9a3f7 :: W32
wbef9a3f7 = W32 One Zero One One One One One Zero One One One One One Zero Zero One One Zero One Zero Zero Zero One One One One One One Zero One One One

wc67178f2 :: W32
wc67178f2 = W32 One One Zero Zero Zero One One Zero Zero One One One Zero Zero Zero One Zero One One One One Zero Zero Zero One One One One Zero Zero One Zero


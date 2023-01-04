{-# LANGUAGE ConstrainedClassMethods #-}

module ReWire.BitWord where 

import Prelude hiding ((+),(*),(-),(||),(&&))
import qualified Prelude
import Data.Bits
import qualified Data.Vector.Sized as V

type Vec n a = V.Vector n a
type Bit = Bool

zero :: Bit
zero = False

one :: Bit
one = True


toInt :: Bit -> Int
toInt True = 1
toInt False = 0

notBit :: Bit -> Bit
notBit = not

-- AND
(>&&<) :: Bit -> Bit -> Bit
True >&&< True = True
_ >&&< _ = False

-- OR
(>||<) :: Bit -> Bit -> Bit
False >||< False = False
_ >||< _ = True

-- XOR
(>^<) :: Bit -> Bit -> Bit
False >^< True = True
True >^< False = True
_ >^< _ = False

-- Eq
(>==<) :: Bit -> Bit -> Bit
False >==< False = True
True >==< True = True
_ >==< _ = False

-- NAND
(>~&<) :: Bit -> Bit -> Bit
True >~&< True = False
_ >~&< _ = True

-- NOR
(>~|<) :: Bit -> Bit -> Bit
False >~|< False = True
_ >~|< _ = False

-- XNOR
(>~^<) :: Bit -> Bit -> Bit
False >~^< True = False
True >~^< False = False
_ >~^< _ = True


-- a b c ~> (a+b+c,carry_out)
rca :: Bit -> Bit -> Bit -> (Bit,Bit)
rca False False False = (False,False)
rca False False True = (True,False)
rca False True False = (True,False)
rca False True True = (False,True)
rca True False False = (True,False)
rca True False True = (False,True)
rca True True False = (False,True)
rca True True True = (True,True)



int2bin :: Int -> [Bit]
int2bin i | i==0      = []
          | otherwise = b : int2bin (i `div` 2)
              where b = case i `mod` 2 of
                      0 -> False
                      1 -> True
                      _ -> Prelude.error "can't happen"

-- assumes inputs are same length
carryadd' :: [Bool] -> [Bool] -> Bool -> ([Bool],Bool)
carryadd' [] _ c = ([],c)
carryadd' (_:_) [] c = ([],c)
carryadd' (a:as) (b:bs) c = 
  (ab:res, c'')
  where
   (res, c') = carryadd' as bs c
   (ab , c'') = rca a b c'

msBit' :: [Bool] -> Bool
msBit' (b:_) = b

lsBit' :: [Bool] -> Bool
lsBit' = last

bitwiseXor' :: [Bool] -> [Bool] -> [Bool]
bitwiseXor' = zipWith xor

bitwiseAnd':: [Bool] -> [Bool] -> [Bool]
bitwiseAnd' = zipWith (Prelude.&&)

bitwiseOr' :: [Bool] -> [Bool] -> [Bool]
bitwiseOr' = zipWith (Prelude.||)

bitwiseNot' :: [Bool] -> [Bool]
bitwiseNot' = map not

bitwiseXNor' :: [Bool] -> [Bool] -> [Bool]
bitwiseXNor' = zipWith (>~^<)

rAnd' :: [Bool] -> Bool
rAnd' = foldr (>&&<) True

rOr' :: [Bool] -> Bool
rOr' = foldr (>||<) False

rNand' :: [Bool] -> Bool
rNand' = not . foldr (>&&<) True

rNor' :: [Bool] -> Bool
rNor' = not . foldr (>||<) False

rXor' :: [Bool] -> Bool
rXor' = foldr (>^<) False

rXnor' :: [Bool] -> Bool
rXnor' = not . foldr (>^<) False

-- Produces bits in little endian form.
int2bits' :: Integer -> [Bool]
int2bits' i | i Prelude.== 0      = []
          | otherwise = b : int2bits' (i `div` 2)
              where b = case i `mod` 2 of
                      0 -> False
                      1 -> True
                      _ -> error "impossible remainder"

lit' :: Integer -> [Bool]
lit' = reverse . int2bits'

pad' :: Int -> [Bool] -> [Bool]
pad' n v | n Prelude.== 0 = v
         | n Prelude.> 0 = False : pad' (n Prelude.- 1) v
         | otherwise = error "negative padding"

-- w is bigendian
padTrunc' :: Int -> [Bool] -> [Bool]
padTrunc' d w 
      | l == d    = w
      | l < d     = pad' (d Prelude.- l) w
      | otherwise = reverse . take d . reverse $ w
         where
           l = length w

-- takes little endian bits 
toIntLE' :: [Bool] -> Int
toIntLE' [] = 0
toIntLE' (b:bs) = toInt b Prelude.+ 2 Prelude.* toIntLE' bs

toInt' :: [Bool] -> Int
toInt' = toIntLE' . reverse

fromBool :: Bool -> Integer
fromBool = toInteger . fromEnum

-- should check that this works on big-endian
toInteger' :: Vec n Bool -> Integer
toInteger' = foldr (\ x s -> fromBool x Prelude.+ 2 Prelude.* s) 0

-- w is bigendian
resize' :: Int -> [Bool] -> [Bool]
resize' d w | l Prelude.== d = w
            | l Prelude.< d  = pad' (d Prelude.- l) w
            | otherwise = reverse . take d . reverse $ w
       where
         l = length w
    
false' :: [Bool] -> Bool
false' [] = True
false' (False:bs) = false' bs
false' (True:_) = False

toBit' :: [Bool] -> Bool
toBit' = not . false'

zero' :: [Bool]
zero' = [False]

one' :: [Bool]
one' = [True]

negate' :: [Bool] -> [Bool]
negate' w = plus' (bitwiseNot' w) (resize' (length w) one')

plus' :: [Bool] -> [Bool] -> [Bool]
plus' as bs = fst $ carryadd' as bs False

minus' :: [Bool] -> [Bool] -> [Bool]
minus' as bs = plus' as (negate' bs)

nudgeL' :: ([Bool] , Bool) -> (Bool , [Bool])
nudgeL' (b:bs,q) = (b,bs ++ [q])

nudgeR' :: (Bool,[Bool]) -> ([Bool],Bool)
nudgeR' (q,w) = (q : init w , last w)

rNudge' :: ([Bool], [Bool]) -> ([Bool], [Bool], Bool)
rNudge' (a,q) = (a' , q' , lsb)
     where
       (a',la)  = nudgeR' (msBit' a , a)
       (q',lsb) = nudgeR' (la , q)

boothround :: ([Bool], [Bool], Bool, [Bool]) -> ([Bool], [Bool], Bool, [Bool])
boothround (a,q,q_1,m) = let
    q_0 = lsBit' q in
    case (q_0,q_1) of
      (False,False) -> (a',q',q_1',m)
        where
          (a',q',q_1') = rNudge' (a,q)
        
      (True,False) -> (a'',q',q_1',m)           -- A - M
        where
          a'            = minus' a m
          (a'',q',q_1') = rNudge' (a',q)

      (False,True) -> (a'', q',q_1',m)     -- A + M
        where
          a'            = plus' a m
          (a'',q',q_1') = rNudge' (a',q)

      (True,True) -> (a',q',q_1',m)
        where
          (a',q',q_1')  = rNudge' (a,q)


-- assume all are same length, for example W8
booth' :: ([Bool], [Bool]) -> ([Bool], [Bool])
booth' (w1,w2) = proj $ rounds (resize' (length w1) zero' , w1,False,w2)
      where proj (x,y,_,_) = (x,y)
            rounds z = iterate boothround z !! length w1

times' :: [Bool] -> [Bool] -> [Bool]
times' as bs = snd $ booth' (as,bs)

shiftL1' :: [Bool] -> [Bool]
shiftL1' w = snd $ nudgeL' (w,False)

shiftR1' :: [Bool] -> [Bool]
shiftR1' w = fst $ nudgeR' (False,w)

arithShiftR1' :: [Bool] -> [Bool]
arithShiftR1' w = 
   if msBit' w 
   then fst $ nudgeR' (True,w)
   else fst $ nudgeR' (False,w)

decr' :: [Bool] -> [Bool]
decr' n = minus' n (resize' (length n) one')

iter' :: [Bool] -> ([Bool] -> [Bool]) -> [Bool] -> [Bool]
iter' n f w =
  if false' n
  then w
  else iter' (decr' n) f (f w)

shiftL' :: [Bool] -> [Bool] -> [Bool]
shiftL' w n = iter' n shiftL1' w

shiftR' :: [Bool] -> [Bool] -> [Bool]
shiftR' w n = iter' n shiftR1' w
  
arithShiftR' :: [Bool] -> [Bool] -> [Bool]
arithShiftR' w n = iter' n arithShiftR1' w

-- For (Unsigned!) Falseomparison:
-- we assume that the input words are the same length
-- so we can use (lexicographic) ordering as defined on lists
-- so we need a function pad the shorter word
padMax' :: [Bool] -> [Bool] -> ([Bool],[Bool])
padMax' v w = 
  case compare (length v) (length w) of
       EQ -> (v,w)
       LT -> (pad' (length w Prelude.- length v) v , w)
       GT -> (v, pad' (length v Prelude.- length w) w)

power' :: [Bool] -> [Bool] -> [Bool]
power' w n = iter' n (times' w) (resize' (length w) one')


-- assumed positive inputs, n = dividend, d = divisor
-- b is s.t. d << b <= n , d << b+1 > n
-- returns (quotient,remainder) 
nonrestoringDivide' :: [Bool] -> [Bool] -> [Bool] -> ([Bool],[Bool])
nonrestoringDivide' n d b =
   let rq = False : n
       b' = plus' (False : b) (resize' (1 Prelude.+ length b) one')
       shiftd = shiftL' (resize' (length rq) d) b
       rq' = iter' b' (loopbody shiftd) rq in
        (resize' (toInt' b') rq'
        ,shiftR' rq' b')
    where 
      loopbody :: [Bool] -> [Bool] -> [Bool]
      loopbody shiftd rq = 
        if shiftd Prelude.> rq
          then shiftL' rq one'
          else bitwiseOr' (resize' (length rq) one') $ shiftL' (minus' rq shiftd) one'

-- assumes n and d are the same length and n >= d
divCounter' :: [Bool] -> [Bool] -> Int
divCounter' n d = 
  if n Prelude.< d 
    then -1 
    else 1 Prelude.+ divCounter' n (shiftL' d one')

-- assumes the same length inputs
divide' :: [Bool] -> [Bool] -> [Bool]
divide' n d = resize' (length n) $ int2bits' $ toInteger $ toInt' n `div` toInt' d

{- PREFERRED DIVIDE IMPLEMENTATION (long division); but broken for now
divide' n d = resize' (length n) $ fst $ nonrestoringDivide' n d b
  where 
  b = lit' $ toInteger $ divCounter' (False:n) (False:d)  -- start positive unsigned
-}

-- assumes the same length inputs
mod' :: [Bool] -> [Bool] -> [Bool]
mod' n d = resize' (length n) $ snd $ nonrestoringDivide' n d b
  where
  b = lit' $ toInteger $ divCounter' (False:n) (False:d)

---------------------------
-- Showing BitWords
---------------------------

fours :: a -> [a] -> [(a,a,a,a)]
fours _ []                       = []
fours d (x1 : x2 : x3 : x4 : xs) = (x4 , x3 , x2 , x1) : fours d xs
fours d [x1,x2,x3]      = [(d , x3 , x2 , x1)]
fours d [x1,x2]           = [(d , d , x2 , x1)]
fours d [x1]               = [(d , d , d , x1)]

toHex :: (Bool , Bool , Bool , Bool) -> Char
toHex (False , False , False , False) = '0'
toHex (False , False , False , True) = '1'
toHex (False , False , True , False) = '2'
toHex (False , False , True , True) = '3'
toHex (False , True , False , False) = '4'
toHex (False , True , False , True) = '5'
toHex (False , True , True , False) = '6'
toHex (False , True , True , True) = '7'
toHex (True , False , False , False) = '8'
toHex (True , False , False , True) = '9'
toHex (True , False , True , False) = 'a'
toHex (True , False , True , True) = 'b'
toHex (True , True , False , False) = 'c'
toHex (True , True , False , True) = 'd'
toHex (True , True , True , False) = 'e'
toHex (True , True , True , True) = 'f'

toBin :: Bool -> Char
toBin False = '0'
toBin True = '1'

hexify :: [Bool] -> String
hexify bits = "0x" ++ map toHex (reverse (fours False (reverse bits)))

binify :: [Bool] -> String
binify bits = "0b" ++ map toBin (reverse bits)

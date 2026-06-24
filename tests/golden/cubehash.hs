{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import Prelude hiding ((+) , (-) , (^) , round)
import ReWire

import ReWire.Bits ((^) , rotL , (+) , lit)
import ReWire.Finite (toFinite , fromFinite , finite)
import ReWire.Vectors (update,index,(!=),map,generate)

r , b , h :: W 32
r = lit 8
b = lit 1
h = lit 256

type State = Vec 32 (W 32)

initialize :: W 32 -> W 32 -> W 32 -> State
initialize x y z = update (update (update zero (finite 0) x) (finite 1) y) (finite 2) z
  where
    zero :: State
    zero = generate $ \ i -> lit 0

s0 :: State
s0 = initialize (lit 32) b r

explode5 :: W 5 -> (Bit , Bit , Bit , Bit , Bit)
explode5 w5 = (w5 `index` (finite 0) , w5 `index` (finite 1) , w5 `index` (finite 2) , w5 `index` (finite 3) , w5 `index` (finite 4))

add :: State -> State
add s = generate (addix s)

accesses :: State -> Finite 32 -> (Bit , W 32 , W 32)
accesses s f32 = (h , s `index` i0 , s `index` i1)
  where
     w5 :: W 5
     w5 = fromFinite f32
     h , j , k , l , m :: Bit
     (h , j , k , l , m) = explode5 w5
     i0 , i1 :: Finite 32
     i0 = toFinite (fromList [False , j , k , l , m] :: W 5)
     i1 = toFinite (fromList [True  , j , k , l , m] :: W 5)

addix :: State -> Finite 32 -> W 32
addix s f32 | h         = v0 + v1
            | otherwise = v0
   where
     v0 , v1 :: W 32
     h :: Bit
     (h , v0 , v1) = accesses s f32

----------------------------
-- rotate operations
----------------------------

rotate :: W 32 -> State -> State
rotate rc s = generate $ rot rc s

rot :: W 32 -> State -> Finite 32 -> W 32
rot rc s f32 | h         = w0
             | otherwise = rotL rc w0
   where
     h :: Bit
     w0 :: W 32
     (h , w0) = rotaccess s f32

-- | The rotate step modifies only the x[0jklm] (h = False) words, rotating each
--   left by rc; the x[1jklm] words are left unchanged. So read the word at f32
--   itself and rotate it when its high bit is clear.
rotaccess :: State -> Finite 32 -> (Bit , W 32)
rotaccess s f32 = (h , s `index` f32)
   where
     h :: Bit
     (h , _ , _ , _ , _) = explode5 (fromFinite f32)

----------------------------
-- swap operations
----------------------------

-- | swap 1

swap1 :: State -> State
swap1 s = generate (swapix1 s)

swapix1 :: State -> Finite 32 -> W 32
swapix1 s f32 | not b1 && b2     = s `index` ffx
              | not b1 && not b2 = s `index` ftx
              | otherwise        = s `index` f32
    where
     b1 , b2 , k , l , m :: Bit
     (b1 , b2 , k , l , m) = explode5 (fromFinite f32)
     ffx , ftx :: Finite 32
     ffx = toFinite (fromList [False , False , k , l , m] :: W 5)
     ftx = toFinite (fromList [False , True  , k , l , m] :: W 5)

-- | swap 2

swap2 :: State -> State
swap2 s = generate (swapix2 s)

swapix2 :: State -> Finite 32 -> W 32
swapix2 s f32 | b1 && not b2 = s `index` ttx
              | b1 && b2     = s `index` tfx
              | otherwise    = s `index` f32
    where
     b1 , j , k , b2 , m :: Bit
     (b1 , j , k , b2 , m) = explode5 (fromFinite f32)
     ttx , tfx :: Finite 32
     ttx = toFinite (fromList [True , j , k , True  , m] :: W 5)
     tfx = toFinite (fromList [True , j , k , False , m] :: W 5)

-- | swap 3

swap3 :: State -> State
swap3 s = generate (swapix3 s)

swapix3 :: State -> Finite 32 -> W 32
swapix3 s f32 | not b1 && not b2 = s `index` ftx
              | not b1 && b2     = s `index` ffx
              | otherwise        = s `index` f32
    where
     b1 , j , b2 , l , m :: Bit
     (b1 , j , b2 , l , m) = explode5 (fromFinite f32)
     ftx , ffx :: Finite 32
     ftx = toFinite (fromList [False , j , True  , l , m] :: W 5)
     ffx = toFinite (fromList [False , j , False , l , m] :: W 5)

-- | swap 4

swap4 :: State -> State
swap4 s = generate (swapix4 s)

swapix4 :: State -> Finite 32 -> W 32
swapix4 s f32 | b1 && not b2 = s `index` ttx
              | b1 && b2     = s `index` tfx
              | otherwise    = s `index` f32
    where
     b1 , j , k , l , b2 :: Bit
     (b1 , j , k , l , b2) = explode5 (fromFinite f32)
     ttx , tfx :: Finite 32
     ttx = toFinite (fromList [True , j , k , l , True] :: W 5)
     tfx = toFinite (fromList [True , j , k , l , False] :: W 5)

----------------------------
-- xor operation
----------------------------

xor :: State -> State
xor s = generate (xorix s)

xorix :: State -> Finite 32 -> W 32
xorix s f32 = if not h
                then
                     s `index` i0 ^ s `index` i1
                else
                     s `index` i1
  where
     h , j , k , l , m :: Bit
     (h , j , k , l , m) = explode5 (fromFinite f32)
     i0 , i1 :: Finite 32
     i0 = toFinite (fromList [False , j , k , l , m] :: W 5)
     i1 = toFinite (fromList [True  , j , k , l , m] :: W 5)

----------------------------
-- Single Round
----------------------------

-- | One CubeHash round: the ten steps below are listed in application order
--   (add first, swap4 last). Since (.) composes right-to-left, the first step
--   applied is the rightmost.
round :: State -> State
round = swap4 . xor . swap3 . rotate (lit 11) . add . swap2 . xor . swap1 . rotate (lit 7) . add

data Inp = Go | NoGo
type Out = State

nopipeline :: Inp -> ReacT Inp Out (StateT State Identity) Inp
nopipeline Go   = do
      lift (modify round)
      x <- lift get
      signal x >>= nopipeline
nopipeline NoGo = do
      x <- lift get
      signal x >>= nopipeline

start :: ReacT Inp Out Identity Inp
start = extrude (nopipeline NoGo) s0

main = undefined

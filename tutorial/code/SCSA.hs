{-# LANGUAGE DataKinds #-}

import Prelude hiding ((^))
import ReWire
import ReWire.Bits

type W8 = W 8

f :: W8 -> W8 -> W8 -> (W8, W8)
f a b c = ( ((a .&. b) .|. (a .&. c) .|. (b .&. c) ) <<. lit 1 , (a ^ b) ^ c )

-- |
-- | Example 2. Storing CSA
-- |

scsa :: (W8, W8, W8) -> ReacT (W8, W8, W8) (W8, W8) (StateT  (W8, W8) Identity) ()
scsa abc = save abc >>= \ cs -> signal cs >>= scsa
  where
    save :: (W8 , W8 , W8) -> ReacT (W8, W8, W8) (W8, W8) (StateT  (W8, W8) Identity) (W8 , W8)
    save (a , b , c) = lift (put (f a b c) >> get)

start :: ReacT (W8 , W8 , W8) (W8 , W8) Identity ()
start = extrude (scsa (lit 0, lit 0, lit 0)) (lit 0, lit 0)
  

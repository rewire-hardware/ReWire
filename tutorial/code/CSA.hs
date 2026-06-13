{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^))
import ReWire
import ReWire.Bits

-- | ReWire compiler will complain if this is imported
-- import ReWire.Interactive

f :: W 8 -> W 8 -> W 8 -> (W 8, W 8)
f a b c = ( ((a .&. b) .|. (a .&. c) .|. (b .&. c) ) <<. lit 1 , (a ^ b) ^ c )

-- Constants for a running example.
_40 , _25 , _20 , _41 , _0 :: W 8
_40 = lit 40
_25 = lit 25
_20 = lit 20
_41 = lit 41
_0  = lit 0

_1 :: W 8
_1 = lit 1

_1' :: W 9
_1' = lit 1

-- |
-- | Example 1. CSA
-- |
-- | The only thing this does is take its inputs i, computes csa on them, and
-- | output the results every clock cycle.

csa :: (W 8, W 8, W 8) -> ReacT (W 8, W 8, W 8) (W 8, W 8) Identity ()
csa (a, b, c) = do
                   abc' <- signal (f a b c)
                   csa abc'

start :: ReacT (W 8, W 8, W 8) (W 8, W 8) Identity ()
start = csa (_0, _0, _0)

-- -- | ReWire compiler will complain if this is here (i.e., comment it before compiling):
-- inputs :: [(W 8 , W 8 , W 8)]
-- inputs = (_40 , _25 , _20)
--        : (_41 , _25 , _20)
--        : (_40 , _25 , _20)  : []

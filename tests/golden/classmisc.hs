{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
-- Class-feature corners that work and must keep working: a multi-parameter
-- class, DeriveAnyClass over an all-defaults class, a NOINLINE instance
-- method, a constraint-bearing GADT field eliminated by
-- case-of-known-constructor, and a kind-annotated marker class.
import ReWire
import ReWire.Bits (lit, (^), (.&.), (==))
import ReWire.Monad (iter, Dev)
import Data.Kind (Type)
import Prelude hiding ((^), (==))

class Convert a b where
      convert :: a -> b
      backfit :: b -> a

instance Convert Bool (W 8) where
      convert b = if b then lit 1 else lit 0
      backfit w = w == lit 1

class Tweak a where
      tweak :: a -> a
      tweak x = x
      untweak :: a -> a
      untweak x = x

data Cmd = Go | Stop
      deriving Tweak

class Mask a where
      mask   :: a -> a
      unmask :: a -> a

instance Mask (W 8) where
      {-# NOINLINE mask #-}
      mask w   = w .&. lit 0x3c
      unmask w = w

class Marked (a :: Type)
instance Marked (W 8)

withMark :: Marked a => a -> a
withMark x = x

data Box a where
      Box :: Mask a => a -> Box a

-- INLINE so the case sees the Box construction site: a captured
-- dictionary must be scrutinized where its constructor is visible.
{-# INLINE unbox #-}
unbox :: Box (W 8) -> W 8
unbox b = case b of
      Box w -> mask w

cmdMask :: Cmd -> W 8 -> W 8
cmdMask c w = case c of
      Go   -> mask w
      Stop -> unmask w

step :: W 8 -> W 8
step w = convert (backfit (unbox (Box (withMark w))) :: Bool)
       ^ cmdMask (tweak (if (backfit w :: Bool) then Go else Stop)) w

start :: Dev (W 8) (W 8)
start = iter step (lit 0)

main :: IO ()
main = undefined

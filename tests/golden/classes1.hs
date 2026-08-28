{-# LANGUAGE DataKinds #-}
-- Multi-method (data-dictionary) classes: a superclass constraint, a
-- default method, and a constraint-polymorphic helper dispatched at two
-- instance types (Bool and W 8) in one device.
import ReWire
import ReWire.Bits (lit, (^), (.&.), (.|.), (@.))
import ReWire.Monad (iter, Dev)
import Prelude hiding ((^))

class Gate a where
      invert :: a -> a
      keep   :: a -> a
      keep x = invert (invert x)

class Gate a => Logic a where
      conj :: a -> a -> a
      disj :: a -> a -> a

instance Gate Bool where
      invert = Prelude.not

instance Gate (W 8) where
      invert w = w ^ lit 0xff
      keep w   = w

instance Logic Bool where
      conj = (&&)
      disj = (||)

instance Logic (W 8) where
      conj = (.&.)
      disj = (.|.)

norm :: Logic a => a -> a -> a
norm x y = conj (invert x) (disj x (keep y))

step :: W 8 -> W 8
step w = if norm (w @. 0) (invert (w @. 7)) then norm w (invert w) else keep w

start :: Dev (W 8) (W 8)
start = iter step (lit 0)

main :: IO ()
main = undefined

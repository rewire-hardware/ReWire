-- EXPECT-ERROR: Types do not unify
-- Solving Vec (n + m) ~ Vec 4 under n := 8 would need m := -4, which no
-- type-level natural can satisfy.
{-# LANGUAGE DataKinds #-}
import Prelude hiding (take)
import ReWire
import ReWire.Bits
import ReWire.Vectors (take)
import ReWire.Monad (iter, Dev)

grow :: W 4 -> W 8
grow v = take v

start :: Dev (W 4) (W 8)
start = iter grow (lit 0)

main = undefined

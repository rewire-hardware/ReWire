-- EXPECT-ERROR: INLINE definition expansion not terminating
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)
import ReWire.Bits (lit)
{-# INLINE foo #-}
foo :: W 8 -> W 8
foo x = bar x
{-# INLINE bar #-}
bar :: W 8 -> W 8
bar x = foo x
start :: Dev (W 8) (W 8)
start = iter foo (lit 0)
main = undefined

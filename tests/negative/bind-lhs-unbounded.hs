-- EXPECT-ERROR: unbounded resumption stack
-- Recursion THROUGH the left-hand side of a bind: each iteration of f
-- suspends a fresh continuation ("the rest of f after the recursive
-- call"), so the machine would need an unbounded resumption stack.
-- Contrast tests/golden/subfsm.hs, where the recursion inside the
-- bind-LHS callee is in tail position and compiles.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits

f :: ReacT Bool (W 8) Identity ()
f = do
      _ <- f
      _ <- signal (lit 0)
      pure ()

start :: ReacT Bool (W 8) Identity ()
start = f

main :: IO ()
main = undefined

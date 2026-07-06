-- Sequential machine composition (the plan's Q3 relaxation): a reactive
-- sub-machine on the left-hand side of a bind runs to completion --
-- pausing internally as often as it likes -- and its result is consumed
-- by the continuation. procify splices the sub-machine's block graph
-- once per continuation; its internal tail recursion closes through the
-- (definition, continuation) memo.
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

-- Count while the input holds True; return the final count when it
-- drops. Recursive AND called from bind-LHS position: compilable
-- because the recursion is in tail position within the sub-machine.
counter :: ReacT Bool (W 8) (StateT (W 8) Identity) (W 8)
counter = do
      s <- lift get
      i <- signal s
      lift (put (s + lit 1))
      if i then counter else pure s

loop :: ReacT Bool (W 8) (StateT (W 8) Identity) ()
loop = do
      n <- counter          -- run the sub-FSM to completion
      _ <- signal (n + n)   -- one cycle reporting its doubled result
      lift (put (lit 0))    -- reset the count
      loop

start :: ReacT Bool (W 8) Identity ()
start = extrude loop (lit 0)

main :: IO ()
main = undefined

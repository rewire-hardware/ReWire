-- EXPECT-ERROR: might pause
-- EXPECT-ERROR: NOINLINE
-- The left-hand side of the bind in start is a NOINLINE (pausing) device:
-- NOINLINE is the user's opt-out from procify's per-continuation block
-- splicing, so the call is rejected. (Without the pragma this program
-- compiles by splicing -- see tests/golden/subfsm.hs.)
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

{-# NOINLINE dev #-}
dev :: ReacT Bool (W 8) (StateT (W 8) Identity) ()
dev = do
      s <- lift get
      i <- signal s
      lift (put (s + lit 1))
      if i then dev else pure ()

loop :: Bool -> ReacT Bool (W 8) Identity ()
loop _ = signal (lit 0) >>= loop

start :: ReacT Bool (W 8) Identity ()
start = extrude dev (lit 0) >>= \ _ -> signal (lit 1) >>= loop

main :: IO ()
main = undefined

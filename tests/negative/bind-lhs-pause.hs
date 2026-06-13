-- EXPECT-ERROR: might pause
-- The left-hand side of the bind in start is a (recursive, pausing) device:
-- purify's Done-only treatment of bind would miscompile it, so it must be
-- rejected. (Loops must be in tail position.)
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

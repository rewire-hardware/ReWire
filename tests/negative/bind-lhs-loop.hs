-- EXPECT-ERROR: might pause
-- EXPECT-ERROR: tail position
-- A recursive (inlinable) reactive loop on the left-hand side of a bind: its
-- result is consumed by the continuation, so the loop is not in tail position.
-- normalizeBind inlines reaction calls on bind left-hand sides, but leaves
-- recursive ones alone (inlining `loop` here would never terminate); the
-- residual call must be rejected cleanly by purify rather than looping in
-- "Bind LHS definition expansion". Cf. bind-lhs-pause.hs, which exercises the
-- same restriction with a NOINLINE (non-inlined) device.
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

loop :: ReacT Bool (W 8) (StateT (W 8) Identity) ()
loop = do
      s <- lift get
      i <- signal s
      lift (put (s + lit 1))
      if i then loop else pure ()

start :: ReacT Bool (W 8) Identity ()
start = extrude (loop >>= \ _ -> signal (lit 0) >>= \ _ -> pure ()) (lit 0)

main :: IO ()
main = undefined

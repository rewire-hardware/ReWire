-- FLAGS: --no-halt
-- EXPECT-ERROR: can halt
-- A device that pauses twice and then returns: the return becomes a
-- halt state, and post-halt outputs are unspecified -- --no-halt
-- rejects it statically. (Without the flag this program compiles.)
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits

start :: ReacT Bool (W 8) Identity ()
start = do
      _ <- signal (lit 1)
      _ <- signal (lit 2)
      pure ()

main :: IO ()
main = undefined

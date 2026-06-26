-- EXPECT-ERROR: unsupported syntax in do-block
{-# LANGUAGE RecursiveDo #-}
import ReWire
import ReWire.Bits
start :: ReacT Bit Bit Identity ()
start = do
  rec { signal zero }
  start
main = undefined

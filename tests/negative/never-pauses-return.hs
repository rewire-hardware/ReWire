-- EXPECT-ERROR: never pauses (no machine to generate)
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits
start :: ReacT Bit (W 8) Identity ()
start = return ()
main = undefined

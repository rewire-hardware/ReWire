-- EXPECT-ERROR: Empty dispatch
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits
start :: ReacT Bit (W 8) Identity ()
start = return ()
main = undefined

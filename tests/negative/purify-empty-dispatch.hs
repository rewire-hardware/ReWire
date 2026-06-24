-- EXPECT-ERROR: empty dispatch
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits
start :: ReacT Bit (W 8) Identity ()
start = return ()
main = undefined

-- EXPECT-ERROR: The top definition must have type ReacT i o Identity
-- start must have a ReacT type.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits

start :: W 8
start = lit 0

main = undefined

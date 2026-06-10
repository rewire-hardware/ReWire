-- EXPECT-ERROR: Types do not unify
-- start must have a ReacT type.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits

start :: W 8
start = lit 0

main = undefined

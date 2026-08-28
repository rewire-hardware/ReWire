{-# LANGUAGE DataKinds #-}
-- A class (data-dict and newtype-dict) declared in one module, orphan
-- instances in a second, uses in Main: the bridge treats the whole home
-- module graph uniformly.
import ReWire
import ReWire.Bits (lit, (@.))
import ReWire.Monad (iter, Dev)

import ClassMods.Gate
import ClassMods.Impl

step :: W 8 -> W 8
step w = if w @. 0 then par (inv w) (flick w) else inv (par w (flick w))

start :: Dev (W 8) (W 8)
start = iter step (lit 0)

main :: IO ()
main = undefined

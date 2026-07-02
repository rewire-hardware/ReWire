-- EXPECT-ERROR: No instance for ‘KnownNat
-- The size of the literal is unconstrained: nothing determines n.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits
import ReWire.Monad (iter, Dev)

discard :: W n -> Bool
discard x = True

bad :: Bool -> Bool
bad y = discard (lit 0)

start :: Dev Bool Bool
start = iter bad False

main = undefined

-- EXPECT-ERROR: Couldn't match type ‘Bool’
-- A signature-less defn used at an incompatible type: rejected by GHC's
-- typechecker in the front end.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits
import ReWire.Monad (iter, Dev)

f x = True

bad :: W 8 -> W 8
bad y = f y

start :: Dev (W 8) (W 8)
start = iter bad (lit 0)

main = undefined

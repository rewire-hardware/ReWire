-- EXPECT-ERROR: Couldn't match type ‘Bool’
-- An unsigned defn (assumed forall a. a) used at an incompatible type:
-- caught when specialization re-typechecks the body at the use's type.
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

-- EXPECT-ERROR: Types do not unify
-- Same as illtyped-use, but the ill-typed defn is unreachable from start:
-- still caught, since specialization runs before purging.
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits
import ReWire.Monad (iter, Dev)

f x = True

deadBad :: W 8 -> W 8
deadBad y = f y

ok :: W 8 -> W 8
ok y = y

start :: Dev (W 8) (W 8)
start = iter ok (lit 0)

main = undefined

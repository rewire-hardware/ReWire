module Prim where

-- ReWire primitives.

-- Primitive types:
-- data (->) a b
-- data ReT i o m a
-- data StT s m a
-- data I a

-- Also tuples:
-- data () = ()
-- data (a, b) = (a, b)
-- ...

return :: a -> m a
return = primError "Prim: return"

(>>=) :: m a -> (a -> m b) -> m b
(>>=) = primError "Prim: >>="

get :: StT s m s
get = primError "Prim: get"

put :: s -> StT s m ()
put = primError "Prim: put"

signal :: o -> ReT i o m i
signal = primError "Prim: signal"

lift :: m a -> t m a
lift = primError "Prim: lift"

extrude :: ReT i o (StT s m) a -> s -> ReT i o m (a, s)
extrude = primError "Prim: extrude"

-- TODO(chathhorn): this is not the real type of unfold. r and a need
-- to be fixed types:
-- data A -- = (generated ctors)
-- data R -- = (generated ctors)
-- unfold :: (R -> i -> Either A (o, R)) -> Either A (o, R) -> ReT i o I a
unfold :: (r -> i -> Either a (o, r)) -> Either a (o, r) -> ReT i o I a
unfold = primError "Prim: unfold"

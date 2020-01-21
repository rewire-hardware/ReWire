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

unfold :: (b -> i -> Either a (o, b)) -> Either a (o, b) -> ReT i o I a
unfold = primError "Prim: unfold"

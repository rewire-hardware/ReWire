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

data R_ -- Ctors generated during program build.
data A_ -- Ctors generated during program build.

return :: a -> m a
return = error "Prim: return"

(>>=) :: m a -> (a -> m b) -> m b
(>>=) = error "Prim: >>="

get :: StT s m s
get = error "Prim: get"

put :: s -> StT s m ()
put = error "Prim: put"

signal :: o -> ReT i o m i
signal = error "Prim: signal"

lift :: m a -> t m a
lift = error "Prim: lift"

extrude :: ReT i o (StT s m) a -> s -> ReT i o m a
extrude = error "Prim: extrude"

unfold :: ((R_, s) -> i -> Either (A_, s) (o, (R_, s))) -> Either (A_, s) (o, (R_, s)) -> ReT i o I a
unfold = error "Prim: unfold"

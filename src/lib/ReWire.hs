module ReWire
      ( get, put, modify, signal, lift
      , extrude, unfold
      , error, return, (>>=)
      ) where

import Prelude (Either) -- TODO(chathhorn): annoying exception
import qualified Prelude                           as GHC
import qualified Control.Monad.Identity            as GHC
import qualified Control.Monad.Resumption.Reactive as GHC
import qualified Control.Monad.State               as GHC

type I = GHC.Identity
type ReT = GHC.ReacT
type StT = GHC.StateT

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

data PuRe s o = Done (A_, s) | Pause (o, (R_, s))

error = GHC.error -- TODO(chathhorn): can't type in RW.

return :: GHC.Monad m => a -> m a
return = GHC.return

(>>=) :: GHC.Monad m => m a -> (a -> m b) -> m b
(>>=) = (GHC.>>=)

get :: GHC.Monad m => StT s m s
get = GHC.get

put :: GHC.Monad m => s -> StT s m ()
put = GHC.put

{-# INLINE modify #-}
modify :: GHC.Monad m => (s -> s) -> StT s m ()
modify f = get GHC.>>= (put GHC.. f)

signal :: GHC.Monad m => o -> ReT i o m i
signal = GHC.signal

lift :: (GHC.MonadTrans t, GHC.Monad m) => m a -> t m a
lift = GHC.lift

extrude :: ReT i o (StT s m) a -> s -> ReT i o m a
extrude = GHC.error "Prim: extrude"

unfold :: ((R_, s) -> i -> PuRe s o) -> PuRe s o -> ReT i o I a
unfold = GHC.error "Prim: unfold"


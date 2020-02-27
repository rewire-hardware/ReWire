module ReWire where

-- Imports in this file are ignored by rwc.
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

-- TODO(chathhorn): can't give a type to these in RW (string lits are only allowed
-- as arguments to error and are never given a type).
error = GHC.error
nativeVhdl _ f = f

rwReturn :: GHC.Monad m => a -> m a
rwReturn = GHC.return

rwBind :: GHC.Monad m => m a -> (a -> m b) -> m b
rwBind = (GHC.>>=)

get :: GHC.Monad m => StT s m s
get = GHC.get

put :: GHC.Monad m => s -> StT s m ()
put = GHC.put

-- | Non-inline definitions in this file are ignored by rwc.
{-# INLINE modify #-}
modify :: GHC.Monad m => (s -> s) -> StT s m ()
modify f = get `rwBind` (\ x -> put (f x))

signal :: GHC.Monad m => o -> ReT i o m i
signal = GHC.signal

lift :: (GHC.MonadTrans t, GHC.Monad m) => m a -> t m a
lift = GHC.lift

extrude :: ReT i o (StT s m) a -> s -> ReT i o m a
extrude = GHC.error "Prim: extrude"

unfold :: ((R_, s) -> i -> PuRe s o) -> PuRe s o -> ReT i o I a
unfold = GHC.error "Prim: unfold"


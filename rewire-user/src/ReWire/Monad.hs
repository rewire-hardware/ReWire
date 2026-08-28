-- | This module defines operations related to monads in ReWire, including 
-- functions for defining looping monadic actions, and simplifying a tower
-- of monad transformers down to the simple ReacT action required by rwc.
-- It provides type synonyms for distinguishing looping devices and stateful devices
-- that run indefinitely.
module ReWire.Monad 
      ( iter, iterSt
      , extrudeDev, extrudeStateDev
      , Dev, StateDev
      ) where

import ReWire

iter :: (i -> o) -> i -> ReacT i o Identity ()
iter f i = signal (f i) >>= iter f

iterSt :: (i -> s -> (o, s)) -> i -> ReacT i o (StateT s Identity) ()
iterSt f i = do
      (o, s) <- lift $ get >>= return . f i
      lift $ put s
      signal o >>= iterSt f

-- | Should be used to represent a looping device that operates indefinitely.
-- Formally, distinguishes a Re_+ device from an Re_INF device for embedding.
type Dev i o = ReacT i o Identity ()

-- | Similar to `Dev`, but for stateful computations.
type StateDev i o m = ReacT i o m ()

-- | Extrude a stateful device to reduce the monad transformer tower.
{-# INLINE extrudeStateDev #-}
extrudeStateDev :: Monad m => StateDev i o (StateT s m) -> s -> StateDev i o m
extrudeStateDev = extrude

-- | Extrude a stateful device to remove the final StateT monad transformer.
{-# INLINE extrudeDev #-}
extrudeDev :: StateDev i o (StateT s Identity) -> s -> Dev i o
extrudeDev = extrude

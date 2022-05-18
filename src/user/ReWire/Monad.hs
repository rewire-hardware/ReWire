module ReWire.Monad where

import ReWire

iter :: (i -> o) -> i -> ReT i o I ()
iter f i = signal (f i) >>= iter f

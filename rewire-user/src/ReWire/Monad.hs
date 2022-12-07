module ReWire.Monad (iter, iterSt) where

import ReWire

iter :: (i -> o) -> i -> ReacT i o Identity ()
iter f i = signal (f i) >>= iter f

iterSt :: (i -> s -> (o, s)) -> i -> ReacT i o (StateT s Identity) ()
iterSt f i = do
      (o, s) <- lift $ get >>= return . f i
      lift $ put s
      signal o >>= iterSt f

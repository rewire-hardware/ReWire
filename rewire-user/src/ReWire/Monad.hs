module ReWire.Monad (iter, iterSt) where

import ReWire

iter :: (i -> o) -> i -> ReT i o I ()
iter f i = signal (f i) >>= iter f

iterSt :: (i -> s -> (o, s)) -> i -> ReT i o (StT s I) ()
iterSt f i = do
      (o, s) <- lift $ get >>= return . f i
      lift $ put s
      signal o >>= iterSt f

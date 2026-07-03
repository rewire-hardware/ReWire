-- Helpers polymorphic over the monad, with monadic operators at a bare type
-- variable: 'whenB' (no pragma) must be monomorphized by the typechecker's
-- specializer; 'unlessB' (INLINE) instead dissolves in inlineAnnotated;
-- 'thenB' exercises (>>) at a type variable. The helpers are instantiated at
-- two concrete monads (the inner StateT and the full ReacT stack). The state
-- bit toggles when the input is low and latches high otherwise.
import ReWire

whenB :: Monad m => Bit -> m () -> m ()
whenB b m = case b of
      True  -> m
      False -> return ()

{-# INLINE unlessB #-}
unlessB :: Monad m => Bit -> m () -> m ()
unlessB b m = case b of
      True  -> return ()
      False -> m

thenB :: Monad m => m () -> m () -> m ()
thenB a b = a >> b

dev :: ReacT Bit Bit (StateT Bit Identity) ()
dev = do
      s <- lift get
      lift (whenB s (put False))
      lift (unlessB s (put True))
      b <- signal s
      whenB b (lift (thenB (put True) (put True)))
      dev

start :: ReacT Bit Bit Identity ()
start = extrude dev False

main :: IO ()
main = undefined

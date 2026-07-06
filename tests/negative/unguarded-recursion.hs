-- A reactive loop whose recursion is not guarded by signal on every path
-- (here: both whens skip when the state is False, so the loop calls itself
-- without pausing). The register-initial evaluation would diverge on it;
-- the compiler must reject it instead of hanging.
-- EXPECT-ERROR: a cycle of gotos crosses no pause (is recursion guarded by signal?)
import ReWire

{-# INLINE when #-}
when :: Monad m => Bit -> m () -> m ()
when b m = case b of
      True  -> m
      False -> return ()

{-# INLINE tick #-}
tick :: ReacT Bit Bit (StateT Bit Identity) ()
tick = lift get >>= \ o -> signal o >>= \ i -> lift (put i)

loop :: ReacT Bit Bit (StateT Bit Identity) ()
loop = do
      b <- lift get
      when b tick
      when b tick
      loop

start :: ReacT Bit Bit Identity ()
start = extrude loop False

main :: IO ()
main = undefined

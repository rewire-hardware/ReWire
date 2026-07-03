-- A monad-polymorphic helper instantiated at a monad off the reactive stack
-- is only caught late, after specialization (the front end sees no monadic
-- operator at the bad monad -- cf. tests/negative/unsupported-monad.hs, where
-- the operator itself is at the unsupported monad and is rejected early).
-- EXPECT-ERROR: unsupported builtin use: rwPrimReturn (a monadic operator at a monad other than the ReacT/StateT/Identity stack?)
import ReWire

data M a = M a

instance Functor M where
      fmap f (M a) = M (f a)

instance Applicative M where
      pure = M
      M f <*> M a = M (f a)

instance Monad M where
      M a >>= k = k a

whenB :: Monad m => Bit -> m () -> m ()
whenB b m = case b of
      True  -> m
      False -> return ()

f :: Bit -> M Bit
f b = case whenB b (M ()) of
      M () -> M b

start :: ReacT Bit Bit Identity ()
start = case f True of
      M b -> signal b >>= \ _ -> start

main :: IO ()
main = undefined

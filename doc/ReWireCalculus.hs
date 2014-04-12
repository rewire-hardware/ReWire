import Control.Monad
import Control.Monad.Trans

----
----
----

newtype ReactT i o m a = ReactT { deReactT :: m (Either a (o,i -> ReactT i o m a)) }

instance Monad m => Monad (ReactT i o m) where
  return = ReactT . return . Left
  ReactT phi >>= f = ReactT $ do
    res <- phi
    case res of
      Left x      -> deReactT (f x)
      Right (o,k) -> return (Right (o,\ i -> k i >>= f))

instance MonadTrans (ReactT i o) where
  lift m = ReactT $ m >>= return . Left

instance Functor m => Functor (ReactT i o m) where
  fmap f (ReactT phi) = ReactT $
    fmap
      (\ res ->
        case res of
          Left x      -> Left (f x)
          Right (o,k) -> Right (o,\ i -> fmap f (k i)))
      phi

-- prim
signal :: Monad m => o -> ReactT i o m i
signal o = ReactT (return (Right (o,return)))

----
---- State monad transformer.
----

newtype StateT s m a = StateT { deStateT :: s -> m (a,s) }

instance Monad m => Monad (StateT s m) where
  return x = StateT (\ s -> return (x,s))
  m >>= f  = StateT (\ s -> do (x,s') <- deStateT m s
                               deStateT (f x) s')

instance MonadTrans (StateT s) where
  lift m = StateT (\ s -> m >>= \ x -> return (x,s))

instance Functor m => Functor (StateT s m) where
  fmap f m =
   StateT $ \ s -> 
    fmap
      (\ (x,s') -> (f x,s'))
      (deStateT m s)

-- prim
get :: Monad m => StateT s m s
get = StateT (\ s -> return (s,s))

-- prim
put :: Monad m => s -> StateT s m ()
put s = StateT (\ _ -> return ((),s))

-- prim
extrudeStateT :: Monad m => ReactT i o (StateT s m) a -> s -> ReactT i o m (a,s)
extrudeStateT (ReactT phi) s = ReactT $ do (res,s') <- deStateT phi s
                                           case res of
                                             Left x      -> return (Left (x,s'))
                                             Right (o,k) -> return (Right (o,\ i -> extrudeStateT (k i) s'))

---
--- Error monad transformer.
---

newtype ErrorT e m a = ErrorT { deErrorT :: m (Either e a) }

instance Monad m => Monad (ErrorT e m) where
  return = ErrorT . return . Right
  m >>= f = ErrorT $ do res <- deErrorT m
                        case res of
                          Left e  -> return (Left e)
                          Right x -> deErrorT (f x)

-- prim
extrudeErrorT :: Monad m => ReactT i o (ErrorT e m) a -> ReactT i o m (Either e a)
extrudeErrorT (ReactT phi) = ReactT $ do res <- deErrorT phi
                                         case res of
                                           Left e              -> return (Left (Left e))
                                           Right (Left x)      -> return (Left (Right x))
                                           Right (Right (o,k)) -> return (Right (o,extrudeErrorT . k))

-- prim
throwError :: Monad m => e -> ErrorT e m a
throwError = ErrorT . return . Left

-- prim (I'm not sure how hard this is to synthesize)
catchError :: Monad m => ErrorT e m a -> (e -> ErrorT e m a) -> ErrorT e m a
catchError (ErrorT phi) h = ErrorT $ do res <- phi
                                        case res of
                                          Left e  -> deErrorT $ h e
                                          Right x -> return (Right x)

---
--- Identity monad (not a transformer, but we could have that too).
---

newtype Identity a = Identity { deIdentity :: a }

instance Monad Identity where
  return = Identity
  Identity x >>= f = f x

instance Functor Identity where
  fmap f = Identity . f . deIdentity

---
--- A memory monad transformer.
---
--- Synthesizing this to infer a RAM may be tricky but it's probably doable.
--- I expect this would just be mapped to a VHDL array. There is nothing in
--- the structure here to enforce a maximum number of write or reads per
--- clock cycle. If there is more than one read/write per clock cycle,
--- presumably XST will either fail with a message like "sorry, I can tell
--- you wanted a RAM here but you can't have it", or (hopefully) just create
--- an area-guzzling register-based implementation (which may actually be
--- what we want sometimes, for small "memories" like register files [and
--- caches?]). More important question: how often does XST *fail* to
--- recognize that something can actually be put in RAM? (And is this failure
--- predictable/explainable at the ReWire level?)
---
--- Compiling this will be a "version 1.5" sorta project.
---
newtype MemT addr word m a = MemT { deMemT :: StateT (addr -> word) m a }

instance Monad m => Monad (MemT addr word m) where
  return         = MemT . return
  MemT phi >>= f = MemT $ phi >>= deMemT . f

-- prim
getMem :: Monad m => addr -> MemT addr word m word
getMem a = MemT $ get >>= \ m -> return (m a)

-- prim
-- the Eq constraint is kind of superfluous in the calculus (this'll be a
-- primitive and the addr/word types will have to be "bitty" anyway).
putMem :: Eq addr => Monad m => addr -> word -> MemT addr word m ()
putMem a w = MemT $ get >>= \ m -> put (\ a' -> if a'==a then w else m a')

---
--- Other stuff from MTL that might be useful and implementable:
---    WriterT, maybe ContT(?!)
---
--- Unlikely to be implementable and/or useful:
---    List (though some other form of nondeterminism may be possible),
---    ReaderT/RWS (this tends to require non-tail recursion to be useful).
---

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

signal :: Monad m => o -> ReactT i o m i
signal o = ReactT (return (Right (o,return)))

----
----
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

get :: Monad m => StateT s m s
get = StateT (\ s -> return (s,s))

put :: Monad m => s -> StateT s m ()
put s = StateT (\ _ -> return ((),s))

extrudeStateT :: Monad m => ReactT i o (StateT s m) a -> s -> ReactT i o m (a,s)
extrudeStateT (ReactT phi) s = ReactT $ do (res,s') <- deStateT phi s
                                           case res of
                                             Left x      -> return (Left (x,s'))
                                             Right (o,k) -> return (Right (o,\ i -> extrudeStateT (k i) s'))

---
--- just for giggles...
---

newtype ErrorT e m a = ErrorT { deErrorT :: m (Either e a) }

instance Monad m => Monad (ErrorT e m) where
  return = ErrorT . return . Right
  m >>= f = ErrorT $ do res <- deErrorT m
                        case res of
                          Left e  -> return (Left e)
                          Right x -> deErrorT (f x)

extrudeErrorT :: Monad m => ReactT i o (ErrorT e m) a -> ReactT i o m (Either e a)
extrudeErrorT (ReactT phi) = ReactT $ do res <- deErrorT phi
                                         case res of
                                           Left e              -> return (Left (Left e))
                                           Right (Left x)      -> return (Left (Right x))
                                           Right (Right (o,k)) -> return (Right (o,extrudeErrorT . k))

---
---
---

newtype Identity a = Identity { deIdentity :: a }

instance Monad Identity where
  return = Identity
  Identity x >>= f = f x

instance Functor Identity where
  fmap f = Identity . f . deIdentity

---
--- Other stuff from MTL that might be useful and implementable:
---    WriterT, maybe ContT(?!)
---
--- Unlikely to be implementable:
---    List, ReaderT ("ask" is fine but "local" is a problem), RWS
---

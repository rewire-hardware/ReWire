module MonadWrangling where

import Prelude hiding (Monad, return, (>>=), Maybe, Just, Nothing)

--------------------------------------
-- This file contains from-scratch versions of several important monads and transformers.
--------------------------------------

class Monad m where
   return :: a -> m a
   (>>=)  :: m a -> (a -> m b) -> m b

--------------------------------------
-- Identity
--------------------------------------

newtype Identity a = Id a
-- Punning the type and data constructors is a
-- long and cherished Haskell community tradition. 8<
  
instance Monad Identity where
  return       = Id
  (Id x) >>= f = f x

--------------------------------------
-- Maybe monad, from scratch
--------------------------------------

data Maybe a = Nothing | Just a

instance Monad Maybe where
  return         = Just
  (Just a) >>= f = f a
  Nothing >>= f  = Nothing

--------------------------------------
-- State monad, from scratch
--------------------------------------

newtype State s a = St (s -> (a , s))
runState :: State s a -> s -> (a , s)
runState (St x) = x

instance Monad (State s) where
  return a     = St (\ s -> (a , s))
  (St x) >>= f = St (\ s -> let (a , s') = x s in runState (f a) s') 

-- "non-proper" morphisms
getState :: State s s
getState = St (\ s -> (s , s))

putState :: s -> State s ()
putState s = St (\ _ -> (() , s))

--------------------------------------
-- Monad transformers, from scratch
--------------------------------------

data StateT s m a = StateT (s -> m (a , s))

runStateT :: StateT s m a -> s -> m (a , s)
runStateT (StateT x) = x

instance Monad m => Monad (StateT s m) where
  return a = StateT (\ s -> return (a , s))
                 --         ^^^^^^ from Monad m. Looks recursive, but isn't.
  (StateT x) >>= f = StateT (\ s0 -> x s0 >>= \ (a , s1) -> runStateT (f a) s1)
                 --                       ^^^ from Monad m. Overloading, yuck.

-- "non-proper" morphisms, now generalized over any Monad m.
get :: Monad m => StateT s m s
get = StateT (\ s -> return (s , s))

put :: Monad m => s -> StateT s m ()
put s = StateT (\ _ -> return (() , s))

-- ``lift'' can be used to re-define m-operations at (t m).
class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (StateT s) where
  lift x = StateT (\ s -> x >>= \ a -> return (a , s))

--
-- Unenforced, yet intended, behavior
--    lift (return_m a) = return_{t m} a
--    lift (x >>=_m f)  = lift x >>=_{t m} (lift . f)
--
-- The "monad laws" are also unenforced but intended
--    return v >>= f = f v    (aka, left unit)
--    x >>= return = x        (aka, right unit)
--    >>= is associative.
-- 

--------------------------------------
-- Reactive Resumption over State, from scratch
--------------------------------------

data Re i s o a = Re (State s (Either a (o , i -> Re i s o a)))
runRe :: Re i s o a -> State s (Either a (o , i -> Re i s o a))
runRe (Re x) = x

(>>>=) :: Re i s o a -> (a -> Re i s o b) -> Re i s o b
(Re x) >>>= f = Re (St (\ s -> case runState x s of
                                     (Left a , s')       -> runState (runRe (f a)) s'
                                     (Right (o , k), s') -> (Right (o , \ i -> k i >>>= f) , s')))

instance Monad (Re i s o) where
  return a = Re (return (Left a))
  (>>=) = (>>>=)

liftRe :: State s a -> Re i s o a
liftRe x = Re (x >>= (return . Left))

-- "non-proper" morphisms
getRe :: Re i s o s
getRe = liftRe getState

putRe :: s -> Re i s o ()
putRe s = liftRe (putState s)

signalRe :: o -> Re i s o i
signalRe o = Re (return (Right (o , return)))
     --          ^^^^^^ State          ^^^^^^ Re

--------------------------------------
-- Reactive Resumptions as a monad transformer
--------------------------------------

data ReacT i o m a = ReacT (m (Either a (o , i -> ReacT i o m a)))

runReacT :: ReacT i o m a -> m (Either a (o , i -> ReacT i o m a))
runReacT (ReacT x) = x

instance Monad m => Monad (ReacT i o m) where
  return a = ReacT (return (Left a))
  (ReacT x) >>= f = ReacT (x >>= \ r ->
                           case r of
                                Left a        -> runReacT (f a)
                                Right (o , k) -> return (Right (o , \ i -> k i >>= f)))

instance MonadTrans (ReacT i o) where
  lift x = ReacT (x >>= (return . Left))

-- "non-proper" morphisms

signal :: Monad m => o -> ReacT i o m i
signal o = ReacT (return (Right (o , return)))
      --          ^^^^^^ m           ^^^^^^ ReacT
           
step :: Monad m => m o -> ReacT i o m i
step x = lift x >>= signal

-- |
-- | Fact: Re i s o a ~= ReacT i o (StateT s Identity) a
-- |   You will see this ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- |   all over the place in ReWire. Now you know.


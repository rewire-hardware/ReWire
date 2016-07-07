---
layout: page
title: "Language Reference"
category: doc
date: 2016-05-03 10:51:52
order: 4
---

The purpose of this section is to provide an overview of the ReWire language as it relates to vanilla Haskell. Readers unfamiliar with Haskell may wish to consult an introductory text such as [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/book.html) by Graham Hutton or [Learn You a Haskell for Great Good!](http://learnyouahaskell.com) by Miran Lipovača before diving into ReWire.

ReWire is a _subset_ of Haskell: all ReWire programs are Haskell programs, but not all Haskell programs are ReWire programs. The main difference between ReWire and Haskell is that ReWire places limits on the use of higher order functions, polymorphism, and recursion. Higher order functions and polymorphism are not allowed except where they can be eliminated via inlining. Recursion is only allowed for functions and computations in a certain class of monads, and such computations must be guarded and tail recursive.

# Reactive Resumption Monads

The fundamental abstraction in ReWire is a class of monads called _reactive resumption monads_. Reactive resumption monads allow us to give a functional semantics to clocked, reactive processes like hardware circuits.

While reactive resumption monads are treated as a primitive in ReWire, their semantics can be defined in terms of Haskell. We will start with a simple case, then generalize to a monad transformer. The type of the _reactive resumption monad_ `Re` is defined in Haskell as follows.

```` haskell
data Re i o a = Done a
              | Pause o (i -> Re i o a)
````

Think of the type `Re i o a` as representing a computation that is exchanging input and output signals (of types `i` and `o` respectively) with an external environment, and will return a value of type `a` if and when it terminates. Formally, a computation in `Re i o a` is either in a `Done` state, representing a finished computation that has returned a value of type `a`, or in a `Pause` state, yielding an output of type `o` and a function (i.e., a continuation) of type `i -> Re i o a` that is waiting for the next input from the environment. The type `Re i o` is a monad as follows:

```` haskell
instance Monad (Re i o) where
  return          = Done
  Done v >>= f    = f v
  Pause o k >>= f = Pause o (k >=> f)
````

where `>=>` is the left-to-right Kleisli composition operator.

We can generalize `Re` to a monad transformer `ReT`, which allows us to mix other kinds of effects with reactivity.

```` haskell
newtype ReT i o m a =
        ReT { deReT :: m (Either a (o, i -> ReT i o m a)) }

instance Monad m => Monad (ReT i o m) where
  return = ReT . return . Left
  ReT m >>= f = ReT $ do
    r <- m
    case r of
      Left v      -> deReT (f v)
      Right (o,k) -> return (Right (o,k >=> f))

instance MonadTrans (ReT i o) where
  lift m = ReT (m >>= return . Left)
````

One particularly useful operation in `ReT`, which we will actually take as a primitive in ReWire, is called `signal`.

```` haskell
signal :: Monad m => o -> ReT i o m i
signal o = ReT (return (Right (o,return)))
````

Think of `signal o` as meaning "yield the output `o` to the environment, wait for a new input `i`, then return `i`".

# Layered State Monads

ReWire also contains built-in support for _layered state monads_, which enable us to describe circuits with mutable state. Formally, a layered state monad is any monad composed from one or more applications of the state monad transformer `StT` to the base identity monad `I`. While `StT` and `I` are primitives in ReWire, they can be defined in Haskell as follows:

```` haskell
newtype StT s m a = StT { deStT :: s -> m (a,s) }

instance Monad m => Monad (StT s m) where
  return x    = StT $ \ s -> return (x,s)
  StT m >>= f = StT $ \ s -> m s >>= \ (x,s) -> deStT (f x) s

instance MonadTrans (StT s) where
  lift m = StT $ \ s -> m >>= \ x -> return (x,s)

newtype I a = I { deI :: I a -> a }

instance Monad I where
  return    = I
  I x >>= f = f x
````

These are, in fact, equivalent to `StateT` and `Identity` in Haskell's standard libraries.

The monads supported by ReWire are limited to monads composed of 

# Restrictions

The ReWire language is essentially a subset of Haskell 98. The major restrictions are as follows.

* __Type classes__ are not (yet) implemented.
* The __`type` keyword__ is not (yet) implemented.
* __Polymorphic__ and __higher-order__ functions are only allowed if they can be __eliminated via inlining__.
  - For example, the polymorphic function `fst :: (a,b) -> a` can always be inlined, as can the higher-order (and polymorphic) function `(.) :: (b -> c) -> (a -> b) -> (a -> c)`.
* __`Data` types__ are allowed, including parametric data types like `Maybe`, but only if they are __first-order__ (i.e., do not contain any function- or monad-typed fields) and __non-recursive__.
* __Recursive function definitions__ are allowed, but only in a reactive resumption monad. Such definitions must be _tail recursive_ and _guarded_.
  - FIXME: Insert some examples/nonexamples.

# Interfacing with VHDL

* Data types -> bit vectors
* `nativeVhdl`
* `extrude`
* `start`
* `signal`

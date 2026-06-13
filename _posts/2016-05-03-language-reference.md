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

We can generalize `Re` to a monad transformer `ReacT`, which allows us to mix other kinds of effects with reactivity. (`ReacT` is the type you actually import from the `ReWire` standard library; it was called `ReT` in older versions of ReWire.)

```` haskell
newtype ReacT i o m a =
        ReacT { deReacT :: m (Either a (o, i -> ReacT i o m a)) }

instance Monad m => Monad (ReacT i o m) where
  return = ReacT . return . Left
  ReacT m >>= f = ReacT $ do
    r <- m
    case r of
      Left v      -> deReacT (f v)
      Right (o,k) -> return (Right (o,k >=> f))

instance MonadTrans (ReacT i o) where
  lift m = ReacT (m >>= return . Left)
````

One particularly useful operation in `ReacT`, which we will actually take as a primitive in ReWire, is called `signal`.

```` haskell
signal :: Monad m => o -> ReacT i o m i
signal o = ReacT (return (Right (o,return)))
````

Think of `signal o` as meaning "yield the output `o` to the environment, wait for a new input `i`, then return `i`".

# Layered State Monads

ReWire also contains built-in support for _layered state monads_, which enable us to describe circuits with mutable state. Formally, a layered state monad is any monad composed from zero or more applications of the state monad transformer `StateT` to the base identity monad `Identity`. ReWire uses the same `StateT` and `Identity` as Haskell's standard libraries; for reference, they can be defined as follows:

```` haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Monad m => Monad (StateT s m) where
  return x       = StateT $ \ s -> return (x,s)
  StateT m >>= f = StateT $ \ s -> m s >>= \ (x,s) -> runStateT (f x) s

instance MonadTrans (StateT s) where
  lift m = StateT $ \ s -> m >>= \ x -> return (x,s)

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return            = Identity
  Identity x >>= f  = f x
````

A ReWire device thus has the type `ReacT i o (StateT s1 (StateT s2 (... Identity))) ()`, with one `StateT` layer per register bank; with no state at all it is just `ReacT i o Identity ()`, for which the `ReWire` standard library provides the synonym `Dev i o`. The `extrude` operation lowers a `StateT` layer by supplying its initial value, turning a stateful device into one ready for synthesis:

```` haskell
extrude :: Monad m => ReacT i o (StateT s m) a -> s -> ReacT i o m a
````

# Restrictions

The ReWire language is essentially a subset of Haskell. The major restrictions are as follows.

* __Type classes__ are not implemented: programs may not declare their own `class`es or `instance`s.
* __Polymorphic__ and __higher-order__ functions are only allowed if they can be __eliminated via inlining__.
  - For example, the polymorphic function `fst :: (a,b) -> a` can always be inlined, as can the higher-order (and polymorphic) function `(.) :: (b -> c) -> (a -> b) -> (a -> c)`.
* __`data` types__ are allowed, including parametric data types like `Maybe`, but only if they are __first-order__ (i.e., do not contain any function- or monad-typed fields) and __non-recursive__.
* __`type` synonyms__ are supported (including type-level naturals, e.g. `type W n = Vec n Bool`).
* __Recursive function definitions__ are allowed, but only in a reactive resumption monad. Such definitions must be _tail recursive_ and _guarded_ (by a `signal`).
  - For instance, `loop n = signal n >>= \ i -> loop (n + i)` is fine, but a non-reactive recursion like `count n = count (n + 1)` is rejected, as is a recursive `data` type.

A misuse of any of these is reported as a compile-time error rather than producing incorrect hardware.

# Interfacing with HDL

* __Bit-level types.__ `import ReWire` and `import ReWire.Bits` provide `Bit` (a single bit) and the fixed-width word type `W n`, an `n`-bit word indexed by a type-level natural, together with arithmetic, comparison, bitwise, shift, slicing, and reduction operations. More generally, any first-order `data` type is laid out automatically as a bit vector. The `Vec n a` type (from `ReWire.Vectors`) gives fixed-length vectors.
* __Externs.__ When you need to drop down to a hand-written HDL module (a primitive the compiler doesn't generate, an IP block, a clocked component), use `extern` (or `externWithSig` for full control over port names, parameters, and clock/reset):

  ```` haskell
  extern        :: String -> a -> a
  externWithSig :: [(String, Integer)]  -- module parameters
                -> String               -- clock signal name (or "")
                -> String               -- reset signal name (or "")
                -> [(String, Integer)]  -- input ports (name, width)
                -> [(String, Integer)]  -- output ports (name, width)
                -> String               -- module name
                -> a                    -- Haskell model (for interpreting/Cryptol)
                -> String               -- instance name
                -> a
  ````

  The next-to-last argument is a Haskell definition used as the extern's *model* when interpreting or generating Cryptol; you supply the matching HDL module to the synthesis/simulation toolchain yourself. (`extern` replaces the old `nativeVhdl` primitive.)
* __`start`.__ Every program designates a top-level device named `start` (override with `--start`), whose type fixes the widths of the generated module's input and output ports.
* __`signal`.__ `signal o` yields output `o` for one clock cycle and returns the input sampled on the next cycle&mdash;the basic unit of clocked I/O.

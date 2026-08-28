# Chapter 1: Introduction

These are the tutorial notes for the ReWire language.

## Prerequisites

### Haskell

There's no way of learning ReWire without knowing basic Haskell. Here are some good sources:
- **Programming in Haskell** by Graham Hutton.
This is an excellent, step-by-step introduction to Haskell. Graham also has a lot of online resources (slides, videos, etc.) to go along with the book.
- **Learn You a Haskell for Great Good!** by Miran Lipovaca.
Highly amusing and informative; available [here](https://learnyouahaskell.github.io/).
- **A Gentle Introduction to Haskell** by Hudak, Peterson, and Fasel. 
Available at [http://www.haskell.org/tutorial/](http://www.haskell.org/tutorial/).
- **Real World Haskell** by Bryan O'Sullivan.
Also available online (I believe). 
-  Google.

### Monads in Haskell

You have to be comfortable with the basics of "monad wrangling". You don't need to understand them in any great depth, but understanding the following ought to do:
1. The `Identity` monad;
1. the state monad; and 
1. the `Maybe` monad.

Understanding the basic usage of the `StateT` monad transformer is important. It's a shame that they are known as "transformer" instead of "constructor", because all a monad transformer is is a way to construct monads in a canonical fashion. 

Monads are a concept from Category Theory. I love Category Theory, really I do. But I'd **strongly** recommend avoiding categorical treatments of monads if this is your first time with this material. Rather, check out Graham Hutton or Miran Lipovaca's texts as they're both excellent.

#### Reactive Resumption Monads

These are a particular family of monads that can be used to precisely describe synchronous concurrency (e.g., like clocked computations in hardware). They sound scary, but they're not. Check out the following papers of mine for the basics if you want. I suspect a lot of readers will just look at its usage in ReWire and get them well enough.
1. [Essence of Multitasking](https://bibbase.org/network/publication/harrison-theessenceofmultitasking-2006) and
1. [Cheap (But Functional) Threads](https://bibbase.org/network/publication/harrison-procter-cheapbutfunctionalthreads-2015). 

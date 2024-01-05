# ReWire

ReWire is an experimental compiler for a subset of
[Haskell](http://haskell.org/) to Verilog and VHDL, suitable for synthesis and
implementation on FPGAs. ReWire enables a semantics-directed style of
synchronous hardware development, based on reactive resumption monads. See the
[online documentation](http://rewire-hardware.github.io/ReWire/) for more information.

## Installing

Make sure Haskell Stack is installed, e.g.:
```
$ sudo apt install haskell-stack
```

To clone this repo, build, and install the `rwc` executable to `~/.local/bin`:
```
$ git clone https://github.com/mu-chaco/ReWire
$ cd ReWire
$ stack install
```

## Usage

See `rwc --help` for a list of supported options and the `tests` directory for
some examples.

## Changelog

### 2.5

* Support for a vector library with lengths encoded using type-level natural
  numbers (compatible with GHC using the `GHC.TypeLits.Normalise` typechecker
  plugin). E.g.,
  ```hs
  {-# LANGUAGE DataKinds #-}
  import ReWire

  type W n = Vec n Bool

  a :: W 2
  a = fromList [False, True]

  c :: W 4
  c = a ++ (fromList [False] :: W 1)
  ```
  See `rewire-user/src/{ReWire.hs, ReWire/Bits.hs, RWC/Primitives.hs}` for
  supported operations.

* Improved the ability to test ReWire programs with GHC by giving many more RWC
  primitives GHC-compatible implementations. See
  `rewire-user/src/RWC/Primitives/hs` for a list of RWC primitives and their
  GHC-compatible implementations.

## Acknowledgments

Distribution Statement ‘A’ (Approved for Public Release, Distribution Unlimited).
This work is supported in part by DARPA. The views, opinions, and/or findings expressed 
are those of the author(s) and should not be interpreted as representing the official 
views or policies of the Department of Defense or the U.S. Government.

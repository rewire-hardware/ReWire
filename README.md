# ReWire

ReWire is an experimental compiler for a subset of [Haskell](http://haskell.org/) to VHDL, suitable for synthesis and implementation on FPGAs. ReWire enables a semantics-directed style of synchronous hardware development, based on reactive resumption monads.

## Simple Example: Fibonacci Sequence

Also known as the "Hello, World" of functional programming. The example consists of two parts: **Fibonacci.rw** is the ReWire code, and **prims.vhd** contains a few supporting functions writte in VHDL.

```haskell
-- File: Fibonacci.rw

--
-- The compiler doesn't yet support a "prelude" so we will have to define a
-- few things ourselves!
--
data Bit is Zero | One end
data W8 is W8 Bit Bit Bit Bit Bit Bit Bit Bit end
data Unit = Unit end

vhdl plusW8 :: W8 -> W8 -> W8

zeroW8 :: W8
is
  W8 Zero Zero Zero Zero Zero Zero Zero Zero
end

oneW8 :: W8
is
  W8 Zero Zero Zero Zero Zero Zero Zero One
end
--
-- End stuff that will eventually be in the prelude.
--

start :: <ReT () W8 I><()>
is
  loop zeroW8 oneW8
end

loop :: W8 -> W8 -> <ReT () W8 I><()>
is
  \ n -> \ m -> bind x <- signal n
             in loop m (plusW8 n m)
end
```

### Simple Processor

## Installation

### Requirements

ReWire is developed against the latest version of the [Haskell Platform](https://www.haskell.org/platform/). The generated VHDL has only been tested with the Xilinx ISE toolchain, but since it makes no use (yet) of Xilinx-specific primitives it should be reasonably portable to other VHDL implementations.

### Building

```
$ git clone git@github.com:mu-chaco/ReWire.git
$ cd ReWire
$ cabal configure
$ cabal install
```

## Usage

The main executable file for the compiler is **rwc** (short for ReWire Compiler).

### Compiling to VHDL

### Synthesis

## Caveats

## Further Reading

1. Adam Procter, William L. Harrison, Ian Graves, Michela Becchi, and Gerard Allwein. Semantics Driven Hardware Design, Implementation, and Verification with ReWire. <em>In Proceedings of the 16th ACM SIGPLAN/SIGBED Conference on Languages, Compilers and Tools for Embedded Systems (LCTES'15)</em>. ACM, New York, NY, USA, 10 pages. http://doi.acm.org/10.1145/2670529.2754970

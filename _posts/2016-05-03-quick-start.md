---
layout: page
title: "Quick Start"
category: doc
date: 2016-05-03 10:51:43
order: 3
---


In this section we will walk through the process of compiling, simulating, and running a ReWire program. We will be working with every functional programmer's favorite example: the [Fibonacci sequence](http://en.wikipedia.org/wiki/Fibonacci_number). Specifically, we will use ReWire to build a circuit that displays one element of the Fibonacci sequence per clock tick, in order, in 8-bit binary, on a bank of LEDs (labeled "fib[0:7]" in the diagram). To make things interesting, we will add an input (labeled "pause" in the diagram) that causes the circuit to hold its current value when high.

```
     ______________________
     |                    |
  ---| pause              |
     |           fib[0:7] |-/-
  ---|>                   |
     |____________________|
```

# Source Code

Two pieces: Fibonacci.hs and prims.vhd.


```
     _____________                ______________
    |             \              |              \
    | Fibonacci.hs | --> rwc --> | Fibonacci.vhd |---.
    |______________|             |_______________|   |     VHDL simulator
                                                     +---> Circuit synthesis
                                  ______________     |     ...
                                 |              \    |
                                 |   prims.vhd   |---'
                                 |_______________|
```

## Fibonacci.hs

```haskell
module Fibonacci where

--
-- The compiler doesn't yet support a "prelude" so we will have to define a
-- few things ourselves!
--
data Bit        = Zero | One
data W8         = W8 Bit Bit Bit Bit Bit Bit Bit Bit

plusW8 :: W8 -> W8 -> W8
{-# INLINE plusW8 #-}
plusW8 = nativeVhdl "plusW8" plusW8

zeroW8 :: W8
zeroW8 = W8 Zero Zero Zero Zero Zero Zero Zero Zero

oneW8 :: W8
oneW8 = W8 Zero Zero Zero Zero Zero Zero Zero One

--
-- End stuff that will eventually be in the prelude.
--

start :: ReT Bit W8 I ()
start = begin

begin :: ReT Bit W8 I ()
begin = loop zeroW8 oneW8

loop :: W8 -> W8 -> ReT Bit W8 I ()
loop n m = do b <- signal n
              case b of
                  One  -> loop n m
                  Zero -> loop m (plusW8 n m)
```

For now let's not worry too much about the particulars, but...

## prims.vhd

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package prims is
  pure function plusW8 (x : std_logic_vector; y : std_logic_vector) return std_logic_vector;
end prims;

package body prims is
  pure function plusW8 (x : std_logic_vector; y : std_logic_vector) return std_logic_vector is
  begin
	return (std_logic_vector(unsigned(x)+unsigned(y)));
  end plusW8;
end prims;
```

# Running the Compiler

    $ rwc Fibonacci.hs -o Fibonacci.vhd

# Setting up the ISE Project

* Create
* Add Files

# Simulating the Circuit

# Implementing the Circuit on the Board

* Map pins
* Synthesize/implement
* Generate bitstream
* Download

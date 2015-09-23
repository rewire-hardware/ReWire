# ReWire

ReWire is an experimental compiler for a subset of [Haskell](http://haskell.org/) to VHDL, suitable for synthesis and implementation on FPGAs. ReWire enables a semantics-directed style of synchronous hardware development, based on reactive resumption monads.

## Simple Example: Fibonacci Sequence

The example program produces the elements of the Fibonacci sequence on its output (encoded as 8-bit integers, so things will overflow pretty quickly!). The circuit has a one-bit input that pauses the circuit's operation on low. Our example consists of two parts: **Fibonacci.rw** is the ReWire code, and **prims.vhd** contains a few supporting functions writte in VHDL.

### Fibonacci.rw
```haskell
--
-- The compiler doesn't yet support a "prelude" so we will have to define a
-- few things ourselves!
--
data Bit is Zero | One end
data W8 is W8 Bit Bit Bit Bit Bit Bit Bit Bit end
data Unit is Unit end

vhdl plusW8 :: W8 -> W8 -> W8 is plusW8

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

start :: <ReT Bit W8 I><()>
is
  begin
end

begin :: <ReT Bit W8 I><()>
is
  loop zeroW8 oneW8
end

loop :: W8 -> W8 -> <ReT Bit W8 I><()>
is
  \ n -> \ m -> bind b <- signal n
             in case b of
                { One  -> loop n m
                ; Zero -> loop m (plusW8 n m)
                }
end
```

### prims.vhd
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

Invoking rwc starts an interactive loop that can be used for various compiler-debugging/testing purposes. For our purposes, the only interesting commands are:

* **tovhdl**: Generates VHDL for the loaded program to rewire.cmd.out.
* **:q** : Exits the interactive loop.

```
$ cd ReWire/examples/Fibonacci
$ rwc Fibonacci.rw
> tovhdl
> :q
$ mv rewire.cmd.out Fibonacci.vhd
```

(We will soon remove the necessity to use the interactive loop here.)

We now have two VHDL files: **Fibonacci.vhd** is the main VHDL module for our program, and **prims.vhd** contains VHDL-defined primitives.

### Synthesis/Implementation

1. Create a new Xilinx ISE project.
2. Add Fibonacci.vhd and prims.vhd to the project.
3. Simulate/synthesize/implement as you normally would.

For the Fibonacci example, the top-level VHDL entity will have inputs and outputs as follows:

```vhdl
entity rewire is
  Port ( clk : in std_logic ;
         input : in std_logic_vector (0 to 0);
         output : out std_logic_vector (0 to 7));
end rewire;
```

The one-bit input and the eight-bit output on the VHDL side correspond respectively to the Bit-typed input and the W8-typed output on the ReWire side.

## Caveats

Syntax

Checks for guardedness/higher-order/polymorphism are a bit wonky

Make sure your circuit never terminates (we could implement something that gives meaning to a "return-ing" circuit though)

Generated VHDL may throw a lot of warnings about unused this or that, but it's okay. :)

## Further Reading

1. Adam Procter, William L. Harrison, Ian Graves, Michela Becchi, and Gerard Allwein. Semantics Driven Hardware Design, Implementation, and Verification with ReWire. In *Proceedings of the 16th ACM SIGPLAN/SIGBED Conference on Languages, Compilers and Tools for Embedded Systems (LCTES'15)*. ACM, New York, NY, USA, 10 pages. http://doi.acm.org/10.1145/2670529.2754970
2. FPT'15
3. FPT'13
4. ARC
5. Adam's dissertation
6. Ian's dissertation

---
layout: page
title: "Quick Start"
category: doc
date: 2016-05-03 10:51:43
order: 3
---


In this section we will walk through the process of compiling, running, and
simulating a ReWire program. The purpose of this tutorial is not to help you
understand the ReWire language, just how to use the ReWire compiler. (For a
gentler, example-driven introduction to the *language*, see the
`tutorial/rewire-by-example` tutorial in the source tree.)

We will be working with every functional programmer's favorite example: the
[Fibonacci sequence](http://en.wikipedia.org/wiki/Fibonacci_number). The
Fibonacci sequence is defined as 0, 1, 1, 2, 3, 5, 8, 13, ... . In other words,
the first two elements of the sequence are 0 and 1, and the <i>n</i>th element
of the sequence (for <i>n</i> > 1) is obtained by adding together the two
elements that precede it. We will use ReWire to build a circuit that displays
one element of the Fibonacci sequence per clock tick, in order, in 8-bit binary
(say, on a bank of LEDs).

The following block diagram illustrates the inputs and outputs of the circuit.
The one-bit `pause` input will cause the circuit to pause operation (i.e., hold
its current value) when high. The 8-bit output `fib` carries the current
element of the sequence.

```
     ______________________
     |                    |
  ---| pause              |
     |             fib[7:0]|-/-
  ---|>                   |
     |____________________|
```

# Source Code

Unlike the very first versions of ReWire, you no longer need to hand-roll bit
and word types or hand-write VHDL primitives: `import ReWire` and
`import ReWire.Bits` bring in a small standard library, including the
fixed-width word type `W n` (an `n`-bit word) and arithmetic over it.

Every ReWire program must have a symbol named `start` whose type is of the form
`ReacT i o Identity ()` (also written `Dev i o`), where `i` and `o` are the
types of the circuit's input and output respectively. (The clock and reset
inputs are always implicitly present.) Here our circuit takes a one-bit input
and produces an eight-bit word, so `start :: ReacT Bit (W 8) Identity ()`.

## `Fibonacci.hs`

```haskell
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

start :: ReacT Bit (W 8) Identity ()
start = fibgen (lit 0) (lit 1)

fibgen :: W 8 -> W 8 -> ReacT Bit (W 8) Identity ()
fibgen n m = do b <- signal n
                if b then fibgen n m else fibgen m (n + m)
```

The `fibgen` function, given two words `n` and `m`, puts `n` on the output port
using `signal` and reads a new value `b` off the input port. If `b` is high
(`pause` pressed) it holds, recursing on the same `n` and `m`; otherwise it
advances, recursing on `m` and `n + m`. (`Prelude`'s `(+)` is hidden because
`ReWire.Bits` gives `(+)` a fixed-width-word meaning.)

Because *every* ReWire program is also an ordinary Haskell program, you can load
`Fibonacci.hs` into GHCi and test your definitions there before compiling them
to hardware.

# Step 1: Compiling to Hardware

Getting our ReWire program to run on an FPGA requires two basic steps. First, we
use the `rwc` compiler to translate the program into Verilog (or VHDL):

```
     _____________                _____________
    |             \              |             \
    | Fibonacci.hs | --> rwc --> | Fibonacci.sv |
    |______________|             |______________|
```

Second, we hand the resulting HDL file off to a standard synthesis toolchain
(AMD/Xilinx Vivado, Intel/Altera Quartus, an open-source flow, etc.).

The command

```
$ rwc Fibonacci.hs -o Fibonacci.sv
```

compiles `Fibonacci.hs` and writes Verilog to `Fibonacci.sv`. (Use `--vhdl -o
Fibonacci.vhdl` for VHDL instead, or `--cryptol` for a Cryptol model.) The
top-level module is named `top_level` and has the following interface:

```systemverilog
module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
```

The compiler always produces a top-level module (named `top_level` by default)
with a `clk` and `rst` port plus an input and output whose widths (here 1 and 8
bits) are determined by the type of `start`. You can rename the ports with
`--top`, `--clock`, `--reset`, `--inputs`, and `--outputs`; for example,

```
$ rwc --inputs=pause --outputs=fib Fibonacci.hs -o Fibonacci.sv
```

names the data ports `pause` and `fib`. The implicit reset can be tuned
(`--invert-reset`, `--sync-reset`, `--no-reset`) or omitted entirely along with
the clock (`--no-clock`, for a purely combinational circuit). Run `rwc --help`
for the full list of options.

# Step 2: Running the Design in the Interpreter

You don't need an external simulator to run a ReWire design: `rwc` has a
built-in interpreter. The following runs the device for ten clock cycles and
writes the output of each cycle as YAML:

```
$ rwc --interpret --cycles=10 Fibonacci.hs -o fib.yaml
$ cat fib.yaml
- __out0: '0x1'
- __out0: '0x1'
- __out0: '0x2'
- __out0: '0x3'
- __out0: '0x5'
- __out0: '0x8'
- __out0: '0xd'
- __out0: '0x15'
- __out0: '0x22'
- __out0: '0x37'
```

These are indeed the elements of the Fibonacci sequence: 1, 1, 2, 3, 5, 8,
13&nbsp;(`0xd`), 21&nbsp;(`0x15`), 34&nbsp;(`0x22`), 55&nbsp;(`0x37`)&hellip;.
(With no inputs file the inputs default to zero each cycle&mdash;i.e., `pause`
is never pressed.)

To drive specific inputs, write an inputs file in the same YAML format and pass
it to `--interpret`:

```
$ cat inputs.yaml
- __in0: 0x0
- __in0: 0x1
- __in0: 0x0
- __in0: 0x0
- __in0: 0x0
$ rwc --interpret=inputs.yaml --cycles=5 Fibonacci.hs -o fib.yaml
$ cat fib.yaml
- __out0: '0x1'
- __out0: '0x1'
- __out0: '0x1'
- __out0: '0x2'
- __out0: '0x3'
```

Notice that the output holds at `0x1` for the extra cycle in which `pause`
(`__in0`) is high, then resumes&mdash;exactly as the design intends. (If you
renamed the ports with `--inputs`/`--outputs`, use those names as the YAML keys
instead.)

# Step 3: Simulating the Generated HDL

Running the interpreter checks the *design*; to also check the *generated
Verilog or VHDL*, ReWire can emit a self-checking testbench. The `--testbench`
flag, alongside the normal HDL output, writes a testbench (`Fibonacci_tb.sv` for
Verilog, `Fibonacci_tb.vhdl` for VHDL) that drives the design for `--cycles`
cycles and prints each cycle's output in the *same* YAML format the interpreter
produces:

```
$ rwc --testbench --cycles=10 Fibonacci.hs -o Fibonacci.sv
```

You can then simulate it with any standard simulator&mdash;for example, with
[Icarus Verilog](https://steveicarus.github.io/iverilog/):

```
$ iverilog -g2012 -o fib.vvp Fibonacci.sv Fibonacci_tb.sv
$ vvp fib.vvp
```

Because the testbench prints in the interpreter's YAML format, you can diff the
simulator's trace directly against `rwc --interpret`'s output and confirm they
agree. (This is exactly the cosimulation check ReWire's own regression suite
runs, three ways: Icarus Verilog, GHDL for VHDL, and the interpreter.) If your
design uses externs supplied as separate HDL modules, pass those source files to
the simulator as well.

# Step 4: Synthesis and Implementation on an FPGA

To put the design on real hardware, import the generated HDL into your FPGA
vendor's toolchain (Vivado, Quartus, etc.) as you would any other Verilog or
VHDL source, set `top_level` as the top module, and add a constraints file
mapping `clk`, `rst`, and the data ports to the appropriate device pins (a 1-bit
input pin for `pause`, eight output pins for `fib`, and the board's clock and a
reset switch). Then synthesize, implement, and generate a bitstream using the
vendor's normal flow. The details are board- and tool-specific; consult your
vendor's documentation.

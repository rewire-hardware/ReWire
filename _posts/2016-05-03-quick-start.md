---
layout: page
title: "Quick Start"
category: doc
date: 2016-05-03 10:51:43
order: 3
---


In this section we will walk through the process of compiling, simulating, and running a ReWire program. We will be working with every functional programmer's favorite example: the [Fibonacci sequence](http://en.wikipedia.org/wiki/Fibonacci_number). Specifically, we will use ReWire to build a circuit that displays one element of the Fibonacci sequence per clock tick, in order, in 8-bit binary, on a bank of LEDs (labeled "fib[0:7]" in the diagram). To make things interesting, we will add an input (labeled "pause" in the diagram) that causes the circuit to hold its current value when high. Diagrammatically, the inputs look like the following.

```
     ______________________
     |                    |
  ---| pause              |
     |           fib[0:7] |-/-
  ---|>                   |
     |____________________|
```

# Source Code

The source code for our example comes in two pieces: `Fibonacci.hs`, which is a Haskell source file, and `prims.vhd`, which contains the code for a natively defined VHDL function `plusW8`. [These files](https://github.com/mu-chaco/ReWire/tree/master/examples/Fibonacci) are available in the ReWire source tree at `/examples/Fibonacci`. For purposes of this tutorial, don't worry about the exact meaning of this code.

## `Fibonacci.hs`

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

## `prims.vhd`

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

Getting our ReWire program to run on an FPGA requires two basic steps. First, we use the `rwc` compiler to translate the Haskell part of the program (here `Fibonacci.hs`) into VHDL. Second, we hand off the resulting VHDL file, along with our `prims.vhd` in case our program contains any user-defined native VHDL functions, to a standard VHDL synthesis toolchain like ISE or Vivado.

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

The first step, running the compiler, is straightforward:

```
$ cd ReWire/examples/Fibonacci        # or wherever you have placed the source files listed above
$ rwc Fibonacci.hs -o Fibonacci.vhd
```

This will generate a VHDL file called `Fibonacci.vhd` which is ready for simulation and synthesis.

# Setting up the ISE Project

Fire up ISE.

![Screenshot]({{ site.baseurl }}/images/ss1.png)

Select New Project.

![Screenshot]({{ site.baseurl }}/images/ss2.png)

Pick a directory, and call the project Fibonacci. Click Next.

![Screenshot]({{ site.baseurl }}/images/ss3.png)

Under "Evaluation Development Board" select "Spartan-3E Starter Board". Change Preferred Language to VHDL if it's not already set. Click Next, then Finish.

![Screenshot]({{ site.baseurl }}/images/ss4.png)

You now have an empty project. Go to Project -> Add Copy of Source. Browse to the directory that has Fibonacci.vhd and prims.vhd. Select both, and hit Open. Just hit OK on the next dialog. Your project should now have the ReWire file loaded.

![Screenshot]({{ site.baseurl }}/images/ss5.png)

# Simulating the Circuit

Click over to Simulation, click on rewire, then fold out the "ISim Simulator" guy.

![Screenshot]({{ site.baseurl }}/images/ss6.png)

Double-click on "Simulate Behavioral Model". The ISim simulator should load. Simulation -> Restart. We'll just show you the relevant waveforms: clk, input, and output, but your screen will probably have others.

![Screenshot]({{ site.baseurl }}/images/ss8.png)

Right click on clk, force clock.

![Screenshot]({{ site.baseurl }}/images/ss9.png)

0, 1, 4ps, OK.

Right click on input, force constant.

![Screenshot]({{ site.baseurl }}/images/ss10.png)

0, OK.

![Screenshot]({{ site.baseurl }}/images/ss11.png)

Change the run time to 1ps, then hit the button a few times and watch what the circuit does. (You may want to zoom out).

![Screenshot]({{ site.baseurl }}/images/ss12.png)

Is that the Fibonacci sequence? Right click on output, select Radix, change it to Unsigned Decimal. Looks good!

![Screenshot]({{ site.baseurl }}/images/ss13.png)

Let's see if the switch works. Force input to 1 and run a few more cycles.

![Screenshot]({{ site.baseurl }}/images/ss14.png)

Looks good! Now force it back to 0 and run a few more.

![Screenshot]({{ site.baseurl }}/images/ss15.png)

Looks good! (Though if we go too far we overflow, but whatever!)

# Implementing the Circuit on the Board

Gotta map pins so pop back over to ISE. Click "Implementation" in the upper left corner. Project -> New Source -> Implementation Constraints File -> call it Fibonacci.ucf. Punch in this text and save:

```
NET "clk" LOC="C9";
NET "input[0]" LOC="L13";
NET "output[0]" LOC="F9";
NET "output[1]" LOC="E9";
NET "output[2]" LOC="D11";
NET "output[3]" LOC="C11";
NET "output[4]" LOC="F11";
NET "output[5]" LOC="E11";
NET "output[6]" LOC="E12";
NET "output[7]" LOC="F12";
```

Gotta synthesize/implement.

![Screenshot]({{ site.baseurl }}/images/ss16.png)

Click rewire, then double-click Implement Design. This will generate some warnings in the synthesis process but that's okay.

Now double-click generate programming file to generate a bitstream to download to the FPGA. This should only take a sec. Finally, double-click configure target device. It'll ask you to add an iMPACT project file; click OK.

Need to finish this and grab screenshots later...

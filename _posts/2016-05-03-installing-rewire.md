---
layout: page
title: "Installing ReWire"
category: doc
date: 2016-05-03 10:51:28
order: 2
---

# Step 1: Install System Requirements

ReWire is built with [Haskell Stack](https://docs.haskellstack.org/), which
manages the GHC toolchain (currently GHC 9.10) and all Haskell dependencies for
you. You only need Stack itself:

* On Linux:

      $ sudo apt install haskell-stack

* On macOS (via [Homebrew](https://brew.sh)):

      $ brew install haskell-stack

(See the [Stack install guide](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
for other platforms. Stack will download the correct GHC the first time you
build.)

The following tools are *optional*, but useful:

* A VHDL or Verilog synthesis tool suite, if you want to put designs on a real
  FPGA. The vendor toolchains (AMD/Xilinx Vivado, Intel/Altera Quartus, etc.)
  all accept the standard Verilog/VHDL that ReWire emits.
* Open-source simulators for testing designs without a board:
  [Icarus Verilog](https://steveicarus.github.io/iverilog/) (`iverilog`) and
  [Verilator](https://www.veripool.org/verilator/) for Verilog, and
  [GHDL](https://ghdl.github.io/ghdl/) for VHDL. The regression test suite uses
  these.
* [Cryptol](https://cryptol.net/), if you want to use the Cryptol backend
  (`rwc --cryptol`) for verification or simulation.

Note that ReWire also includes a *built-in interpreter* (`rwc --interpret`), so
you can run and test a design with no external simulator at all.

# Step 2: Acquire ReWire Source

Clone the repository from GitHub:

    $ git clone https://github.com/rewire-hardware/ReWire
    $ cd ReWire

# Step 3: Build and Install ReWire

To build everything and install the `rwc` (compiler) and `rwe` (Isabelle
embedder) executables to `~/.local/bin`:

    $ stack install

The first build will take a while, since Stack fetches GHC and compiles all
dependencies. To build without installing, use `stack build`; to run the
compiler straight from the source tree without installing, use
`stack run rwc -- <args>`.

Make sure `~/.local/bin` is on your `PATH`. You should then be able to run:

    $ which rwc
    /Users/me/.local/bin/rwc
    $ rwc --help

See the [Quick Start]({% post_url 2016-05-03-quick-start %})
to compile and run your first design.

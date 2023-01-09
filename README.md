# ReWire

[![Build Status](https://travis-ci.org/mu-chaco/ReWire.svg?branch=master)](https://travis-ci.org/mu-chaco/ReWire)

ReWire is an experimental compiler for a subset of
[Haskell](http://haskell.org/) to Verilog and VHDL, suitable for synthesis and
implementation on FPGAs. ReWire enables a semantics-directed style of
synchronous hardware development, based on reactive resumption monads. See the
[online documentation](http://mu-chaco.github.io/ReWire/) for more information.

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

## Acknowledgments

Distribution Statement ‘A’ (Approved for Public Release, Distribution Unlimited).
This work is supported in part by DARPA. The views, opinions, and/or findings expressed 
are those of the author(s) and should not be interpreted as representing the official 
views or policies of the Department of Defense or the U.S. Government.

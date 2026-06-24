# ReWire

ReWire is a compiler for a subset of
[Haskell](http://haskell.org/) to Verilog and VHDL, suitable for synthesis and
implementation on FPGAs (plus a [Cryptol](https://cryptol.net/) backend
producing a pure model of the generated hardware, for verification and
simulation). ReWire enables a semantics-directed style of synchronous hardware
development, based on reactive resumption monads. See the
[online documentation](http://rewire-hardware.github.io/ReWire/) for more information.


## Installing

Make sure Haskell Stack is installed; e.g., for linux machines:
```
$ sudo apt install haskell-stack
```
Or, if you have a Mac, `haskell-stack` is on homebrew:
```
$ brew install haskell-stack
```

To clone this repo, build, and install the `rwc` executable to `~/.local/bin`:
```
$ git clone https://github.com/rewire-hardware/ReWire
$ cd ReWire
$ stack install
```

## Usage

See `rwc --help` for a list of supported options and the `tests/golden`
directory for some examples. A tutorial is in `tutorial/rewire-by-example`.

## Changelog

See [CHANGES.md](CHANGES.md).

## Acknowledgments

Distribution Statement ‘A’ (Approved for Public Release, Distribution Unlimited).
This work is supported in part by DARPA. The views, opinions, and/or findings expressed 
are those of the author(s) and should not be interpreted as representing the official 
views or policies of the Department of Defense or the U.S. Government.

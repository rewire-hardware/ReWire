# ReWire

ReWire is a compiler for a subset of
[Haskell](http://haskell.org/) to Verilog and VHDL, suitable for synthesis and
implementation on FPGAs (plus a [Cryptol](https://cryptol.net/) backend
producing a pure model of the generated hardware, for verification and
simulation). ReWire enables a semantics-directed style of synchronous hardware
development, based on reactive resumption monads. See the
[online documentation](http://rewire-hardware.github.io/ReWire/) for more information.

## Installing

### Quick start

```
$ git clone https://github.com/rewire-hardware/ReWire
$ cd ReWire
$ ./install.sh
```

The script builds and installs everything below, offers to set up the Lean
toolchain for `--certify`, and finishes with a smoke test that compiles and
certifies an example. Re-running it is safe (it is idempotent); see
`./install.sh --help` for the non-interactive (`--yes`) and `--no-certify`
variants. The rest of this section describes what it does, for manual
installation or troubleshooting.

### Prerequisites

| Tool | Needed for | Notes |
|---|---|---|
| [Haskell Stack](https://docs.haskellstack.org/) | building everything | `brew install haskell-stack` (macOS), `sudo apt install haskell-stack` (Debian/Ubuntu), or `curl -sSL https://get.haskellstack.org/ \| sh`. Stack downloads the pinned GHC itself. |
| [z3](https://github.com/Z3Prover/z3) | the Cryptol FFI | needed on the `PATH` when compiling programs that import Cryptol functions (`rwcry` typechecks the Cryptol source with it). The `--cryptol` *backend* does not need it. |
| [elan](https://github.com/leanprover/elan) (Lean) | building the `--certify` validator | needed only at build time; the installed validator binary is self-contained. `install.sh` offers to set it up (per-user, in `~/.elan`). |

### What gets installed

`stack install` builds and installs three executables to `~/.local/bin`
(more precisely, `stack path --local-bin`):

- **`rwc`** — the compiler: Haskell to Verilog (default), VHDL (`--vhdl`),
  or Cryptol (`--cryptol`), with `--interpret` for cycle-accurate
  simulation and `--certify` for translation-validation certificates. An
  installed `rwc` does not need Stack at run time.
- **`rwcry`** — the Cryptol front end for the
  [Cryptol FFI](doc/cryptol-ffi.md): rwc invokes it out of process when a
  program imports functions from a `.cry` file. It is found next to `rwc`,
  then on the `PATH` (`RWC_RWCRY` overrides). Compiling FFI-using programs
  also needs `z3`.
- **`rwe`** — the embedder, which translates ReWire programs to
  [Isabelle](https://isabelle.in.tum.de/) theories for verification.
  Using its output requires an Isabelle installation with the AFP; see
  [rewire-embedder/README.md](rewire-embedder/README.md).

Building the certificate validator additionally requires the Lean toolchain
(any recent [elan](https://github.com/leanprover/elan) — the build fetches
the exact pinned Lean version automatically):

- **`rwv-cstep-validate`** — the formally verified validator behind
  `rwc --certify` (see [doc/certify.md](doc/certify.md)): built from the
  Lean development in `verify/` with `cd verify && lake build
  rwv-cstep-validate`, and best installed next to `rwc` (which is the
  first place rwc looks; then the `PATH`; `RWC_RWV` overrides). A
  `VALIDATED` verdict from `rwc --certify` is backed by a machine-checked
  proof that the compiled device implements the source state machine —
  without the validator installed, `--certify` compiles normally and
  warns.

### Checking the installation

```
$ rwc --certify -o fibo1.sv tests/golden/fibo1.hs
certify: VALIDATED: the compiled device (fibo1.rwc) implements the Eidos machine (fibo1.eir).
```

(Without the validator installed, the same command produces `fibo1.sv` and
a warning explaining how to build the validator.)

### Environment variables

| Variable | Effect |
|---|---|
| `RWC_RWCRY` | path to the `rwcry` executable (otherwise: next to `rwc`, then `PATH`) |
| `RWC_RWV` | path to the `rwv-cstep-validate` executable (otherwise: next to `rwc`, then `PATH`, then `verify/.lake/build/bin`) |
| `RWC_PACKAGE_PATH` | override the GHC package databases rwc consults (rarely needed; an installed rwc uses the path baked in at build time) |

### Running the test suites

The compiler test suite exercises golden files, cosimulation agreement
across all backends, and certification:

```
$ stack test rewire:rwc-test
```

The cosimulation legs use whichever of these are on the `PATH` and skip
(with a note) otherwise: [Icarus Verilog](http://iverilog.icarus.com/)
(`iverilog`), [Verilator](https://www.veripool.org/verilator/),
[GHDL](https://ghdl.github.io/ghdl/), and
[Cryptol](https://cryptol.net/) (plus `z3` and `rwcry` for the FFI tests,
and the validator for the certify group). `stack test rewire-user` checks
GHC compatibility of the user-facing library, and `stack test
rewire:rwe-test` runs the embedder tests (requires Isabelle and the AFP).

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

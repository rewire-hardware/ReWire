# Changelog

## Unreleased

* The Core interpreter can now evaluate externs with a user-supplied Haskell
  model: the seventh argument of `rwPrimExtern` is now compiled like any other
  definition when it is a reference to a top-level definition whose reachable
  definitions are non-recursive, first-order, monomorphic, and synthesizable --
  the conventional self-referential idiom (`f = extern "f" f`) still means "no
  model". The model is attached to the extern in the Core IR (a `model <defn>`
  suffix in `.rwc` files), the interpreter calls it for unclocked externs, and
  the Cryptol backend emits a call to it instead of an uninterpreted `parameter`.
  Implementations that look like real models but fail the usability checks are
  dropped with a warning, as are models for clocked externs (which are stateful,
  so a pure per-cycle model can't be cycle-accurate).
* Compiler warnings: rwc and rwe can now emit non-fatal warnings
  (`file:line:col: Warning: ...` on stderr), with `-w`/`--no-warn` to
  suppress them and `-Werror` to make them fatal. Initial warnings: a live
  call to the built-in `error` function now compiles to a zero (don't-care)
  value with a warning instead of failing outright; an explicitly named
  `--interpret`/`--testbench` inputs file that can't be read warns before
  driving all inputs with zeros; `--testbench` with a target other than
  Verilog or VHDL warns that no testbench is generated. New warning test
  suite (`tests/warning/`): each test declares expected-warning substrings
  with `-- EXPECT-WARNING:` comments and is checked under default flags,
  `-Werror` (must fail), and `-w` (must be silent).
* New Cryptol backend (`rwc --cryptol`): translates Core to a self-contained
  Cryptol module -- one pure function per Core defn plus a `rw_device` stream
  function modeling the whole device (a sequence of per-cycle inputs to a
  sequence of per-cycle outputs), bit-for-bit equivalent to the Core
  interpreter. Intended for verification (e.g., proving equivalence against a
  hand-written Cryptol spec with SAW) and fast functional simulation.
  Unclocked externs are declared as uninterpreted functions in a `parameter`
  block; clocked externs are rejected. The rwc-test cosimulation check gains a
  fourth leg (when `cryptol` is on the PATH) evaluating `rw_device` against
  the interpreter trace, and `tests/regression/*.cry` golden tests.
* The VHDL backend (`rwc --vhdl`) works again, with full feature parity with
  the Verilog backend.
* New `rwc --testbench[=inputs.yaml]` flag: alongside the Verilog or VHDL
  output, also generate a testbench (`foo_tb.sv`/`foo_tb.vhdl`) that drives
  the design with interp-style inputs for `--cycles` cycles and prints the
  outputs each cycle in the same YAML format `--interpret` produces, so a
  simulation trace can be compared directly against the interpreter. The
  rwc-test cosimulation check now uses it for a three-way agreement test
  (iverilog/vvp vs. ghdl vs. the Core interpreter) with random stimulus.
* Fixed the Verilog backend's arithmetic right shift (`rwPrimRShiftArith`):
  `>>>` on an unsigned operand simulates as a logical shift, so the left
  operand is now wrapped in `$signed(...)`, matching the Core interpreter's
  (sign-extending) semantics.
* Test coverage expansion across the suites: VHDL golden tests
  (`tests/regression/*.vhdl`, a CLI-flags smoke group exercising verbose
  tracing, pass dumps, and signal-naming options, new negative tests, a
  regression test for the bitvector reduction operators, and a much larger
  rewire-user suite (GHC-side device simulation, bit slicing, Finite
  arithmetic).
* rewire-user: new `mod` operation in `ReWire.FiniteComp`.
* Fixed GHC-compatible implementations of several rewire-user primitives that
  disagreed with the compiler: `rwPrimBitSlice`/`rwPrimBitIndex` (bits are
  numbered LSB-at-0, Verilog style) and `rwPrimRNAnd`/`rwPrimRNor`/
  `rwPrimRXNor` (NOT-of-reduction rather than a pairwise fold).
* The Core interpreter now supports the reduction primitives (`RAnd`,
  `RNAnd`, `ROr`, `RNor`, `RXOr`, `RXNor`); as a consequence the Core
  partial evaluator can now constant-fold them.

## 2.7 (2026-06)

* Compile-time performance overhaul, especially for large files: type
  checking, dead-code purging, the partial evaluator, and the Core recursion
  check have been reworked (roughly 3x faster end-to-end on the larger
  regression tests). New `rwc-perf` benchmark suite for tracking compile
  times (`stack bench rewire:rwc-perf`).
* Type checker fixes: ambiguous type variables are now rejected (previously
  an ambiguously-sized value could silently compile to a 0-width wire);
  type-level nat unification no longer accepts negative or non-integral
  solutions; recursive datatypes are now a proper error instead of a hang.
* New negative test suite (`tests/negative/`) checking that invalid programs
  are rejected with the expected errors.
* CLI scaffolding, module loading, export resolution, and pass running are
  now shared between `rwc` and `rwe`.
* rewire-user (now versioned in lockstep with the other packages): vector
  `update` now has a real (GHC-compatible) implementation; fixed the GHC
  implementation of `rwPrimGtEq`; new GHC-compatibility test suite.
* Documentation refresh: changelog split out of the README (this file);
  stale tutorial and design-doc material updated or archived.
* Removed the experimental FIRRTL backend, which had been non-functional
  since 2.1: the `rwc --firrtl` flag, the `firrtl` executable, and the
  `ReWire.FIRRTL.*`/`ReWire.LoFIRRTL.*` modules are gone.

## 2.6 (2025-02)

* Upgrade to GHC 9.10.

## 2.5.2 (2025-01)

* Bug fixes and improved testing.
* The Isabelle embedder (`rwe`), which translates ReWire programs to Isabelle
  theories for verification, split out into its own package
  (`rewire-embedder`).

## 2.5.1 (2024-06)

* Upgrade to GHC 9.6.

## 2.5 (2023-03)

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
  See `rewire-user/src/ReWire.hs`, `rewire-user/src/ReWire/Bits.hs`, and
  `rewire-user/src/RWC/Primitives.hs` for supported operations.

* Improved the ability to test ReWire programs with GHC by giving many more
  RWC primitives GHC-compatible implementations. See
  `rewire-user/src/RWC/Primitives.hs` for a list of RWC primitives and their
  GHC-compatible implementations.

## 2.4 (2023-01)

* Support for larger tuples; improved Verilog generation (e.g., no more
  single-use modules).

## 2.2-2.3 (2022)

* Project restructured as a multi-package Stack project; the user-facing
  "standard library" for ReWire programs broken out into the `rewire-user`
  package, usable from both `rwc` and GHC.

## 2.1 (2022-09)

* Verilog backend (now the default target); experimental FIRRTL support
  (currently non-functional); VHDL backend disabled by default.

## 2.0 (2020-03)

* Major rewrite of the compiler pipeline (the Crust and Core IRs,
  purification pass, and reactive-resumption-based source language roughly
  as they exist today), targeting VHDL.

## 0.1 (2014-2016)

* Original research prototype: Haskell-subset-to-VHDL compiler based on
  reactive resumption monads.

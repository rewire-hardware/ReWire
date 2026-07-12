# Changelog

## Unreleased

* New GHC front end: `rwc` now compiles ReWire programs with an in-process
  GHC session (parse, rename, typecheck, and desugar over the whole module
  graph), translating the resulting Core to the new Eidos IR. Programs are
  typechecked by GHC itself, with GHC's error messages, and type classes
  are now supported: instance methods compile to ordinary
  dictionary-passing definitions that specialization and partial
  evaluation eliminate (single-method classes, whose dictionaries are
  newtypes, are not yet supported). The haskell-src-exts front end is
  retired from `rwc`; the embedder (`rwe`) still uses it. The typelits
  solver plugins are linked into `rwc` and enabled without any dynamic
  loading, and the package databases `rwc` was built against are baked in
  and self-discovered at run time (`RWC_PACKAGE_PATH` overrides; running
  under `stack exec` works as before), so an installed `rwc` normally
  needs neither stack nor the checkout's environment at run time.
* The Crust IR and its monad-transformer purification pass have been
  replaced by Eidos (`ReWire.Eidos.*`), a new typed functional IR
  specified in `doc/eidos.md`, with a textual format (`.eir`, dumped
  beside the output by `--eidos`). The reactive fragment is now compiled
  to an explicit process calculus -- state cells and labeled blocks with
  pause/goto/halt terminators -- and folded directly to Hyle; recursion
  not guarded by `signal` is rejected with a source-located error. Source
  variable names still survive into the generated HDL, interpreter traces
  are bit-for-bit unchanged from the previous pipeline, and the largest
  tests compile roughly 20x faster end-to-end.
* Sequential FSM composition: a reactive computation may now appear on the
  left-hand side of a bind (e.g. `x <- subMachine i; ...`), running a
  sub-state-machine to completion and continuing with its result; the
  sub-machine's states are spliced into the caller per call site.
  Recursion must still reach itself in tail position (a computation that
  re-enters itself through a bind's left-hand side would need an unbounded
  resumption stack and is rejected), and a NOINLINE reactive definition
  may only be called in tail position.
* New `rwc --no-halt` flag: statically reject a device that can halt
  (post-halt outputs are unspecified; a rejected device must pause forever
  instead).
* New Cryptol foreign-function interface: `ReWire.Cryptol.cryptol file fn
  impl` (in rewire-user) compiles the Cryptol function `fn` from module
  `file` at the use-site type and realizes it in hardware along with the
  rest of the program -- unlike `extern` (a black box realized by
  hand-written HDL), the function is present in the generated
  Verilog/VHDL/Cryptol and the interpreter can evaluate it. The Cryptol
  module is loaded and typechecked by the Cryptol implementation itself
  (which needs `z3` on the PATH), the use-site type is checked against
  the function's Cryptol type scheme, and the monomorphized result is
  translated to Hyle definitions by the new `rwcry` executable (the
  `rewire-cryptol` package; rwc invokes it out of process, so rwc itself
  does not link the Cryptol toolchain). The `impl` argument is the
  implementation used when the program runs under GHC (`f = cryptol
  "f.cry" "f" f` for none), mirroring the extern-model idiom. The
  supported fragment: first-order combinational functions over
  Bit/words/vectors/tuples/records/newtypes/enums -- if-then-else, local
  value bindings, case expressions, record construction/selection/update,
  numeric constraint guards, sequence comprehensions (fully unrolled),
  foldl/foldr, indexing with constant or variable indices, and the
  scalar, slicing, and enumeration primitives (records and enums are
  interior-only: the entry point's type must be words/vectors/tuples);
  local function bindings, higher-order functions applied to statically
  known functions (named, lambdas, curried, or chosen by `if`/`case`),
  and type-indexed recursion (e.g. `pow`{e-1}` behind constraint
  guards, unrolled per instantiation); recursive finite comprehensions
  (the message-schedule/key-expansion/CBC-chaining idiom, unrolled
  element-wise), `scanl`, `update`/`updateEnd`, rotates, element shifts,
  `transpose`, carry-less polynomial arithmetic
  (`pmult`/`pdiv`/`pmod`, constant divisor), `lg2`, signed division;
  value recursion, infinite streams, and other stateful Cryptol are
  rejected with located errors.
* `rwc -d`/`--dump` addresses every pipeline pass again (the front-end
  rework had left only the Hyle fold dumpable) and now writes each dump to
  a file beside the output instead of stdout, named for the pass number
  and the IR's format -- e.g., `MiniISA.6.eir` after pass 6 (Eidos) and
  `MiniISA.10.rwc` after pass 10 (Hyle). Passes 1-9 are the front
  end/bridge, the Eidos passes, and the Eidos-to-Hyle fold; 10-11 are the
  Hyle-level optimizer and inliner (also reachable from `--from-core`);
  `rwc -v` shows the bracketed pass numbers. The new `--dump-all` flag
  dumps every pass, and adding `-v` appends the IR's show output to each
  dump, indented for readability, in a comment -- dumps remain valid
  `.eir`/`.rwc` syntax.

## 2.8 (2026-07)

* The `rewire-core` library has been split into three packages: `rewire-frontend`
  (the GHC driver and Core-to-Eidos bridge `ReWire.GHC.*`, the Eidos IR and
  its passes `ReWire.Eidos.*`, and the pass orchestration), `rewire-backend`
  (the Hyle bit-level IR `ReWire.Hyle.*` and the Verilog/VHDL/Cryptol RTL
  backends), and `rewire-base` (the shared base utilities: source annotations,
  diagnostics, config, bit vectors, and pretty-printing). The dependency order is
  `rewire-base` <- `rewire-backend` <- `rewire-frontend`.
  `ReWire.Annotation`/`ReWire.Error` were first decoupled from haskell-src-exts
  (the source annotation is a native span; the HSE conversion now lives in
  `ReWire.HSE.SrcLoc`), and the `ReWire.HSE.*` modules now live in
  `rewire-embedder`, the only package that still depends on haskell-src-exts.
  `rewire-embedder` no longer depends on the Hyle backend, building against just
  `rewire-base` and `rewire-frontend`. Every package now builds with
  `-Wunused-packages`. The `rwc`/`rwe` command-line interfaces are unchanged.
* Error reporting overhaul. Diagnostics from `rwc` and `rwe` now show the
  offending source line with a caret underline beneath the exact span
  (`file:line:col:`, then the source line, then `^^^`) rather than
  re-printing a parsed AST fragment, and can carry secondary `note:`
  locations and `help:` hints (for example, a missing `start` function now
  suggests `--start=`). Error messages no longer leak internal pass names
  (`Purify:`, `hyle check:`, `toHyle:`, ...) or dump raw AST -- the
  "Unsupported ... syntax" errors now show the actual surface syntax.
  Violated compiler invariants are reported as `internal error: ...` with a
  "please report it" hint, clearly distinguishing a bug in rwc from a problem
  with the user's program. Internally, the source annotation threaded through
  every IR was reduced from an embedded haskell-src-exts AST node (with a
  large constraint bag and the exactprint dependency) to a compact source
  span with provenance.
* The Core IR has been replaced by Hyle (`ReWire.Hyle.*`), a new
  bit-level intermediate representation specified in `doc/hyle.md`: a pure,
  first-order, total language over width-indexed bitvectors with explicit
  widths on every operation (resizing only via explicit
  `zext`/`sext`/`trunc`), explicit let-bindings and muxes instead of
  pattern/don't-care conventions, and an explicit device construct --
  named registers with initial values, parallel wire equations, and
  device-level instances for clocked externs. Division by zero now has one defined
  semantics on every target (SMT-LIB: `x/0` is all-ones, `x%0` is `x`).
  The `--core`/`--from-core` flags are unchanged but the `.rwc` format is
  now Hyle's concrete syntax.
* The RTL optimizer now fuses slice/concat plumbing through named wires
  (both expression lets and device-level wires), splits slices of
  concatenations at the piece boundaries, coalesces adjacent slices of the
  same base, folds division by a literal zero, and drops the wires fusion
  leaves unused -- including the argument wires introduced by inlining.
  Generated Verilog and VHDL are substantially smaller and more readable
  (the golden-test corpus shrank by roughly a third); interpreter
  traces are bit-for-bit unchanged.
* Removed the vestigial reference primitives (`rwPrimSetRef`,
  `rwPrimGetRef`, `setRef`, `getRef`, and the `Ref` type): the compiler has
  rejected them since the Hyle migration and no program ever used them.
  Device-level named wires are the supported mechanism in the IR.
* Fixed a Verilog backend bug found by cosimulation: an arithmetic right
  shift nested in an unsigned expression context simulated as a logical shift;
  the shift is now wrapped in `$unsigned(...)` to isolate its signedness.
* The interpreter can now evaluate externs with a user-supplied Haskell
  model: the seventh argument of `rwPrimExtern` is now compiled like any other
  definition when it is a reference to a top-level definition whose reachable
  definitions are non-recursive, first-order, monomorphic, and synthesizable --
  `f = extern "f" f` still means "no model".
* Compiler warnings: rwc and rwe now emit non-fatal warnings
  (`file:line:col: Warning: ...` on stderr), with `-w`/`--no-warn` to
  suppress them and `-Werror` to make them fatal. Initial warnings: a live
  call to the built-in `error` function now compiles to a zero (don't-care)
  value with a warning instead of failing outright; an explicitly named
  `--interpret`/`--testbench` inputs file that can't be read warns before
  driving all inputs with zeros; `--testbench` with a target other than
  Verilog or VHDL warns that no testbench is generated.
* `--cycles` now defaults to the larger of 10 or the number of inputs in the
  `--interpret=`/`--testbench=` inputs file (rather than always 10), so an
  inputs file is interpreted/simulated for at least as many cycles as it
  lists unless `--cycles` is given explicitly.
* New Cryptol backend (`rwc --cryptol`): translates the bit-level IR to a
  self-contained Cryptol module -- one pure function per defn plus a
  `rw_device` stream function modeling the whole device (a sequence of
  per-cycle inputs to a sequence of per-cycle outputs), bit-for-bit
  equivalent to the interpreter. Intended for verification (e.g., proving
  equivalence against a hand-written Cryptol spec with SAW) and fast
  functional simulation.
* The VHDL backend (`rwc --vhdl`) works again, with full feature parity with
  the Verilog backend.
* New `rwc --testbench[=inputs.yaml]` flag: alongside the Verilog or VHDL
  output, also generate a testbench (`foo_tb.sv`/`foo_tb.vhdl`) that drives
  the design with interp-style inputs for `--cycles` cycles and prints the
  outputs each cycle in the same YAML format `--interpret` produces, so a
  simulation trace can be compared directly against the interpreter. The
  rwc-test cosimulation check now uses it for a three-way agreement test
  (iverilog/vvp vs. ghdl vs. the interpreter) with random stimulus.
* Fixed the Verilog backend's arithmetic right shift (`rwPrimRShiftArith`):
  `>>>` on an unsigned operand simulates as a logical shift, so the left
  operand is now wrapped in `$signed(...)`, matching the interpreter's
  (sign-extending) semantics.
* Test coverage expansion across the suites: VHDL golden tests
  (`tests/golden/*.vhdl`, a CLI-flags smoke group exercising verbose
  tracing, pass dumps, and signal-naming options, new negative tests, a
  golden test for the bitvector reduction operators, and a much larger
  rewire-user suite (GHC-side device simulation, bit slicing, Finite
  arithmetic).
* rewire-user: new `mod` operation in `ReWire.FiniteComp`.
* Fixed GHC-compatible implementations of several rewire-user primitives that
  disagreed with the compiler: `rwPrimBitSlice`/`rwPrimBitIndex` (bits are
  numbered LSB-at-0, Verilog style) and `rwPrimRNAnd`/`rwPrimRNor`/
  `rwPrimRXNor` (NOT-of-reduction rather than a pairwise fold).
* The interpreter now supports the reduction primitives (reduction
  AND/NAND/OR/NOR/XOR/XNOR); as a consequence the partial evaluator can now
  constant-fold them.

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

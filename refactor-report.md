# Core→Mantle refactor: running report

Log of issues, limitations, bugs, deferred work, and notable decisions
encountered while executing `core-refactor-plan.md`. Newest entries at the
bottom of each section. Companion docs: `doc/core.md` (the Mantle spec),
`core-refactor-plan.md` (the phase plan).

## Decisions made en route

- **2026-06-11 (phase 0).** Design-review decisions recorded in
  `doc/core.md` §11: name stays "Mantle" permanently; `undef n` prints
  distinctly in the textual format; `ugt`/`uge`/`sgt`/`sge` included as
  primitives (readability of generated IR/RTL is a major goal); no signed
  division; no combinational-feedthrough extern support.
- **2026-06-11 (phase 0).** Clock/reset emission metadata (names, polarity,
  synchronicity) stays in `Config` rather than on the Mantle `Device` node,
  matching current practice — the spec's §3.6 "device carries emission
  metadata" is realized by the Config at emission time. Revisit if devices
  ever need per-device reset configuration.

## Issues / bugs found

- **2026-06-11 (phase 3).** Found stale goldens (rwc/sv/vhdl/cry, 12 tests)
  **pre-existing on master**. Root cause, confirmed by bisection: commit
  4ec3f90 ("rewire-user: add mod to ReWire.FiniteComp") shifted codegen --
  rwc loads the rewire-user *sources* at compile time, so adding a
  definition there advances the unbound-generics freshness counter for
  every program that transitively loads the module, renaming lambda-lifted
  defns and generated wires. Output matches the old goldens at d866c83
  (one commit earlier) and differs from 4ec3f90 on. The full rwc-test
  suite was not rerun after that final, "obviously safe" rewire-user
  commit. Regenerated in commit 6486ac8 (semantics unchanged: yaml/cosim
  unaffected). **Lesson: any rewire-user source change can invalidate
  every golden; always rerun the full suite.** The stale stack data dir
  intermittently masks this (an old rewire-user copy reproduces the old
  names), which made the failure look nondeterministic across runs.
- **2026-06-11 (phase 3).** Spurious rwc-test failure traced to editing
  source files while the suite was running: the "(stack ghc)" legs run
  `stack ghc`, which **rebuilds local packages** if sources changed, so a
  mid-run edit (or a broken work-in-progress module — hpack auto-discovers
  every .hs under src/) fails those legs. Process rule for this refactor:
  never touch *.hs during a suite run; keep WIP modules outside src/ until
  they compile.

- **2026-06-11 (phase 1).** `ReWire.BitVector.nbits` is float-based
  (`ceiling . logBase 2`) and wrong at exact powers of two (`nbits 1 = 0`)
  and unreliable for very wide values (Double precision); the Mantle parser
  uses exact integer comparison for its literal range check instead.
  Pre-existing; callers of `nbits`/`szBitRep` elsewhere should be audited
  eventually.
- **2026-06-11 (phase 1).** Grammar ambiguity found by the round-trip test:
  juxtaposition application would greedily consume the next statement's
  target name (`let u = inv t` followed by `q.din := e` parsed as
  `inv t q.din`). Resolved in the parser: a name atom must not be followed
  by `:` (no `:` exists in the expression grammar, so this is unambiguous).

## Limitations and deferred work

- **2026-06-11 (phase 2).** The bridge rejects programs using references
  (`rwPrimSetRef`/`rwPrimGetRef`) with a clear error. No test exercises
  them, and they only work flattened today (see below). Support lands with
  the direct producer (phase 6) or a dedicated pre-pass, if wanted.
- **2026-06-11 (phase 2).** Register initial values are obtained by
  evaluating `state0` with the Core interpreter. A device with registers
  whose `state0` transitively calls an extern (even a clocked one) cannot
  be bridged; no current test has this shape (extern.hs's `state0` does
  reach its clocked extern, but that device has no registers, so `state0`
  is simply dropped). The direct producer will compute initials
  structurally instead.
- **2026-06-11 (phase 2).** Two uses of the same extern module name with
  different port shapes/kinds in one program is a bridge error ("extern
  used with inconsistent signatures"). Today each Core call site carries
  its own `ExternSig`, so this is technically expressible; the honest fix
  (a separate "external module name" field on the Mantle extern decl,
  decoupling decl identity from the RTL module name) can come later if
  ever needed.
- **2026-06-11 (phase 2).** Extern model merging: if the same extern module
  appears both with and without a model, the bridge keeps the model.
  Conflicting models are an error.

- **Refs (`SetRef`/`GetRef`) are fragile in the current compiler**: the
  Verilog backend reads a ref as a bare name in whatever module the `GetRef`
  lands in (`Core.ToVerilog:319`), which only works when everything relevant
  is inlined into one module. The bridge will enforce the
  device-level-single-driver discipline and reject what it can't hoist; a
  dedicated regression test should be added before relying on refs (noted in
  plan §6).
- **Extern models don't see generics** (carried over from the extern-models
  work): a `model` defn has signature inputs→outputs with no generic
  parameters.

## Phase log

- **Phase 1 (2026-06-11).** `Mantle.Syntax`/`Parse`/`Check` landed. The
  parser elaborates widths bottom-up from declaration signatures (the
  concrete syntax doesn't carry per-node widths).
- **Phase 2 (2026-06-11).** Bridge landed; gate green: all 60 regression
  programs bridge, check, and round-trip (parse-print fixpoint). Clocked
  externs hoist to device instances via bridge-side inlining (extern.hs
  verified by hand: instance + port drives + the dispatch defn inlined into
  the device body, structurally as expected).
- **Phase 3 (2026-06-11, in progress).** `Mantle.Interp` landed; the
  `--interpret` path now runs both interpreters and fails on any trace
  divergence (temporary, removed when Core dies). Direct differential sweep
  over all 60 regression programs: all interpretable tests agree
  (divergences: none; the 5 expected failures are model-less-extern tests
  the Core interpreter also rejects). Full rwc-test suite run in progress.
- **Phase 3 (2026-06-11) complete.** Full suite green (560 tests) with the
  differential check active on every interpreter invocation.
- **Phase 4a (2026-06-11) complete.** Cryptol backend rewritten on Mantle
  (`Mantle.ToCryptol`, the doc/core.md 8.4 embedding): widths exact by
  construction (no width threading), SMT-LIB division-by-zero guards, the
  device as `rw_step` + the standard stream idiom (no resumption-layout
  reconstruction). Gate sequence: full suite with only cry-golden failures
  and **zero cosim failures** (the cryptol leg agreed with
  iverilog/ghdl/interp everywhere), then all 59 `.cry` goldens regenerated,
  then full suite green. Old `Core.ToCryptol` stays in-tree (unreferenced)
  until phase 6.
- **Phase 4b (2026-06-11, in progress).** Verilog backend rewritten on
  Mantle (`Mantle.ToVerilog`): per-construct templates (no
  `wcast`/`expWidth`), defn modules never take clock/reset ports
  (sequential externs are device-level instances), the register process is
  built from explicit registers, and extern instances connect
  **positionally** in declaration order (the hand-written Verilog
  implementations don't know the synthesized names of anonymous ports; the
  VHDL backend keeps the named `p<i>` convention). Zero-width erasure
  (doc/core.md 8.6) implemented after case1 (a fully degenerate device)
  produced `logic [-1:0]`. Two `<<loop>>` bugs found and fixed:
  lazy-StateT evaluation-order cycle in the backend (fixed: strict State)
  and a genuinely cyclic self-referential **strict** HashMap in the inline
  pass (`Data.HashMap.Strict.fromList` forces values; the values looked the
  map up) -- fixed by processing defns callee-first, no lazy knot.
  Output-size note: inlining now happens at the Mantle level (lets become
  named wires) instead of textual expression substitution in the backend;
  after making atomic arguments substitute directly, most outputs are
  comparable or smaller (gfmult -24% bytes), but deep many-argument chains
  grow (Sha256_1 +90% bytes, 14x lines -- named wires instead of 50KB
  single-line expressions). Lint and suite runtimes unaffected; phase 5
  fusion (slice-of-concat, let-coalescing) is the lever if this matters.
- **Phase 4c (2026-06-11).** VHDL backend rewritten on Mantle
  (`Mantle.ToVHDL`), mirroring the Mantle Verilog backend. The
  `wcast`/`expWidth`/width-tracking machinery of the old backend is gone
  (widths exact by construction); `rw_helpers` is now needed only for the
  operations themselves, with additions for Mantle: `rw_sext` and the
  signed comparisons (`rw_lts` etc.), and `rw_mod`'s zero-divisor case
  fixed to return the dividend (SMT-LIB `bvurem`, per doc/core.md 5.2 --
  it previously returned all-ones, but no test ever divided by zero).
  Extern ports connect by name (the `p<i>` convention the hand-written
  VHDL implementations already use). Gate: first run had only vhdl-golden
  failures with ghdl cosim green everywhere; goldens regenerated.
- **Phase 5 pulled early:** `Mantle.Transform.inline` (the uses<=1 heuristic
  from the RTL backends, plus --flatten) + `purgeUnused` written before the
  backend ports so RTL goldens churn once, not twice. Model-referenced
  defns are never inlined (referenced by name from extern decls).

- **Phase 5 (2026-06-11).** Perf re-baseline, `stack bench rewire:rwc-perf`
  at the pre-refactor base (4863c2f) vs. HEAD (all four Mantle backends +
  bridge + differential interp in the pipeline):

  | case | base | HEAD |
  |---|---|---|
  | letchain 32/64/128 | 2.50 / 3.64 / 7.88s | 2.42 / 3.50 / 7.71s |
  | defchain 64/128/256 | 1.54 / 2.02 / 3.23s | 1.49 / 2.03 / 3.16s |
  | statevars 8/16/32 | 1.52 / 2.27 / 6.90s | 1.50 / 2.19 / 6.77s |
  | gfmult | 21.90s | 21.36s |
  | Sha256_1 | 15.76s | **18.51s (+17%)** |
  | OD19Filter | 5.53s | 5.56s |
  | cubehash | 2.41s | 2.31s |

  Scaling slopes identical (letchain 0.5/1.1, defchain 0.4/0.6-0.7,
  statevars 0.5-0.6/1.6). Everything within noise except Sha256_1 (+17%):
  ~730 defns through bridge + check + inline is the worst case for the
  extra pipeline stage. Watch it in phase 6, where the bridge disappears
  into the direct producer.
- **Phase 5 scope note.** `inline` and `purgeUnused` landed early (phase
  4b). Porting `mergeSlices`/`partialEval`/`dedupe` to Mantle is
  **deferred to phase 6**: the Core-level versions still run before the
  bridge, so every Mantle consumer already sees optimized input; the ports
  only become necessary when the direct producer replaces the Core
  pipeline. At that point they also become the lever for the Sha256_1
  regressions above (output size and compile time).

## Phase 6 design notes (from reading Crust.ToCore, 631 lines)

- Most of ToCore carries over unchanged in spirit: `sizeOf`/`ctorWidth`/
  `ctorTag`/`ctorRep` (type sizing and constructor bit layout), the builtin
  table, extern signature parsing, the name map. The Mantle-specific deltas:
  - `transPat`/`M.Match` currently emit Core `[C.Pat]`; the direct producer
    compiles the match to `let d = disc in if eq(slice...) then call else
    els` directly (the bridge's `destruct` logic moves here).
  - `toPrim` maps to Mantle ops per doc/core.md 9.1 (LAnd/LNot/XNor/MSBit/
    Resize/Reverse/Id expansions, as in the bridge's `transPrim`).
  - The `StartDefn`/`Wiring` handling becomes device assembly: registers
    from the state types (`detuple` of the PuRe state type) with initials
    from evaluating `state0` -- use `Mantle.Interp.evalExp` on the
    translated defns (no Core interpreter). The `loop` defn's result is
    split by the same padding|outputs|dispatch arithmetic, but *computed
    at construction* (t_ins'/t_outs'/t_sts' in transDefn already have the
    per-wire widths).
  - Clocked-extern hoisting and refs: same classification/inlining approach
    as the bridge (`envClocked` closure), but over Crust defns.
- Keep the bridge until `ToMantle(crust) == bridge(ToCore(crust))` modulo
  local names on the whole corpus (temporary differential, like phase 3).
- Deletion sweep at the end: ReWire.Core.{Syntax,Parse,Check,Interp,
  Transform,Analysis,ToCryptol,ToVerilog,ToVHDL}, Mantle.FromCore, the
  differential interp scaffolding in FrontEnd, Crust.ToCore. Core.Mangle
  stays. `--core`/`--from-core`/`.rwc` switch to the Mantle format
  (parse/print already round-trips); `.rwc` goldens regenerate; `-d` pass
  numbering and the rwc-test "golden rwc"/"golden cry from rwc" paths
  update (the cry golden test currently recompiles from the .rwc golden
  via --from-core).
- rwe-test (needs Isabelle) should be run before merging; the embedder is
  untouched but shares Flags/Config/Error.

- **Phase 6a (2026-06-12).** `Crust.ToMantle` landed: the direct producer
  (Match compiles straight to slices/eq/mux, prims to the Mantle set, the
  device assembled with register initials evaluated by the *Mantle*
  interpreter, clocked externs hoisted via `Mantle.Transform.inlineBy` plus
  device-level let-flattening). TEMPORARY scaffolding: `getDevice` returns
  both the Core device and the direct program; every `--interpret` run is
  now a *three-way* differential (Core vs. bridge vs. direct), and
  `--mantle` emits the direct output. Gate: zero divergences over the
  corpus; full suite green (560). Bug found by the sweep: a restarting
  device's dispatch calls `$Pure.start` as an ordinary defn -- the bridge
  never saw this (Core's partial evaluator folds the call away), so the
  direct producer must keep state0 in the program when referenced.

- **Phase 6c (2026-06-12).** The flip: `getDevice` now produces Mantle
  directly (Crust passes then `toMantle`); every target consumes the
  optimized direct program; `--core`/`--from-core` and the `.rwc` goldens
  switched to the Mantle concrete syntax. Deleted: `ReWire.Core.{Syntax,
  Parse, Check, Interp, Transform, Analysis, ToCryptol, ToVerilog,
  ToVHDL}`, `Mantle.FromCore` (the bridge), `Crust.ToCore`, and the
  differential scaffolding (~3.5k lines); `Core.Mangle` moved to
  `Mantle.Mangle`; `run`/`Ins`/`subRange`/`inputValue`/`yamlPrefixes`
  moved into `Mantle.Interp`. All goldens regenerated behind a green
  cosim; `.yaml` goldens unchanged throughout, as required.
  **Cosim caught a real backend bug during the flip**: with expressions no
  longer flattened the way the old backend did, `$signed(a) >>> b` could
  appear nested inside an unsigned parent operation, whose
  context-determined signedness turns the shift logical in iverilog
  (ghdl/interp disagreed) -- exactly the trap rtl-backends.md section 1
  documented. Fixed by emitting `$unsigned($signed(a) >>> b)`: function
  arguments are self-determined, isolating the signed context.
- **Phase 6 perf (vs. pre-refactor base).** letchain 2.56/3.70/8.09s
  (base 2.50/3.64/7.88), defchain 1.59/2.12/3.63 (1.54/2.02/3.23),
  statevars 1.62/2.42/6.97 (1.52/2.27/6.90), gfmult 23.34 (21.90),
  Sha256_1 16.25 (15.76; was 18.51 with the bridge), OD19Filter 5.85
  (5.53), cubehash 2.45 (2.41). Slopes unchanged. The phase-5 Sha256_1
  regression is essentially recovered now that only one lowering runs.

## Remaining work

- All plan phases (0-7) are complete. Loose ends, deliberately deferred:
  - **rwe-test** (needs Isabelle + AFP) should be run once before merging;
    the embedder shares only Flags/Config/Error and builds cleanly.
  - **rw_helpers pruning**: the old width-discipline family (rw_resize for
    implicit contexts, rw_land/rw_lor, the negated reductions, rw_cond is
    still used) could shrink now that only the Mantle backend calls it;
    unused functions are harmless, so this is cosmetic.
  - **References** (`rwPrimSetRef`/`rwPrimGetRef`) are now an explicit
    compile error ("not supported"); reinstating them means device-level
    named-wire equations per doc/core.md section 9.1 plus producer-side
    hoisting (no test ever exercised them).
  - **Slice/concat fusion across instance boundaries** and further output
    cosmetics (e.g. the $i-wire chains in deeply inlined Verilog) are
    follow-on optimization work.

## Notes for later phases

- Plan §5 "new tests to add": width-0 edge cases, div/mod-by-zero stimulus,
  multi-register device naming, parse/print round-trip property.

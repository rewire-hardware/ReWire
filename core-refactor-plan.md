# Migration plan: replacing Core with Mantle

Companion to `doc/core.md`, which specifies the new IR ("Mantle"). This
document plans how the codebase gets from here to there. Implementation is
in progress; `refactor-report.md` (repo root) is the running log of issues
and decisions encountered along the way.

## 1. Strategy: bridge first, producer last

The defining choice: **introduce Mantle behind a Core→Mantle translation
("the bridge"), migrate the consumers one at a time, and rewrite the producer
last.** The alternative — rewrite `Crust.ToCore` to emit Mantle directly and
convert all four consumers (interpreter, Verilog, VHDL, Cryptol) in one
stroke — couples every risky change to every other one and leaves no
intermediate state in which the test suite can localize a regression.

With the bridge:

- `Crust.ToCore`, `Crust.Purify`, and everything upstream are untouched
  until the very end; the pipeline runs `… → Core → [bridge] → Mantle → backend`
  for whichever backends have been migrated, while unmigrated backends keep
  consuming Core directly. Both IRs coexist; every commit keeps the full
  suite green.
- The bridge is where the implicit conventions of Core get encoded
  *explicitly, once, in one auditable module*: context widths, pattern
  compilation, the resumption layout, the nil-else don't-care rule. Each
  migrated backend then *deletes* its private copy of that knowledge.
- Migration of each consumer is independently validated by the strongest
  oracle we have: the four-way cosimulation check (iverilog/vvp vs. ghdl vs.
  interpreter vs. cryptol on identical pseudorandom stimulus), plus the
  golden tests and HDL lint.

The end state (phase 6) replaces `Crust.ToCore` + bridge with a direct
`Crust.ToMantle`, then deletes `ReWire.Core.*`.

## 2. Module disposition

| current module (lines)        | fate |
|-------------------------------|------|
| `Core.Syntax`                 | replaced by `Mantle.Syntax` (new: let, if, slices, named params, extern decls, device/register/instance; gone: `Pat`, `Wiring`, `Target`, nil conventions) |
| `Core.Parse`                  | replaced by `Mantle.Parse` (concrete syntax per doc/core.md §10) |
| `Core.Check`                  | replaced by `Mantle.Check` (syntax-directed typing; acyclicity incl. model edges; device single-assignment and scope rules; simpler than today's `primCompat` table) |
| `Core.Interp`                 | replaced by `Mantle.Interp`, a transcription of the denotational semantics. `patMatches'`/`patApply'`/`pausePadding`/`dispatchWires`/`resumptionSize` move into the bridge (their last home), then die with it |
| `Core.Transform` (221)        | ported to `Mantle.Transform`: purgeUnused, dedupe, partial eval (now: constant folding by evaluation + let/slice/concat algebra), slice merging; **new**: inline pass (subsumes the per-backend uses==1 heuristic), coercion fusion (`zext∘zext`, `trunc∘zext`, slice-of-concat, etc.) |
| `Core.Analysis` (111)         | mostly dies: `pureDefns` clock-plumbing is obsolete (only device-level instances are clocked); use-counting moves into the inline pass |
| `Core.Mangle`                 | kept as-is — per-target name legalization is printer business in the new design too |
| `Core.ToVerilog` (675)        | rewritten on Mantle, substantially smaller: `wcast`/`expWidth`, pattern compilation, `compileStart` state-machine construction, clock-port threading all gone; what remains is the §8 template table, the register/instance idioms, and the testbench generator |
| `Core.ToVHDL` (691)           | likewise; `rw_helpers` shrinks from ~30 functions to division guards + shift clamping (exact widths everywhere make the resize family unnecessary) |
| `Core.ToCryptol`              | rewritten as the §8.4 embedding; the width-threading in `transExp` disappears |
| `Crust.ToCore`                | phase 6: becomes `Crust.ToMantle`, absorbing the bridge's lowering decisions; until then unchanged |
| (new) `Mantle.FromCore`       | the bridge; temporary, deleted in phase 6 |

Untouched: `rewire-embedder` (Atmo is upstream of all this), `ReWire.HSE.*`,
`Crust.*` (except `ToCore` at the end), `ReWire.BitVector`,
`Verilog.Syntax`/`VHDL.Syntax`/`Cryptol.Syntax` target ASTs (small additions
at most).

## 3. Where duplicated knowledge consolidates

The payoff table — each row is logic that currently exists in 2–4 places and
ends up in exactly one:

| knowledge                                | today                                            | after |
|------------------------------------------|--------------------------------------------------|-------|
| context/assignment width rules           | ToVerilog (`wcast`/`expWidth`), ToVHDL (`rw_resize` discipline), ToCryptol (width threading) | explicit `zext`/`trunc` nodes, inserted by bridge → producer |
| pattern match/apply over bit offsets     | Interp (`patMatches'`/`patApply'`), used by all three backends | compiled to slices/eq/mux by bridge → producer |
| resumption layout (padding∥out∥dispatch) | Interp + 3 backends via `pausePadding`/`dispatchWires` | producer-emitted slices; IR has named registers |
| state machine + reset construction       | `compileStart` in ToVerilog and ToVHDL           | per-register equations; backends print one register idiom |
| clock/reset plumbing through call tree   | `Core.Analysis.pureDefns` + both RTL backends    | obsolete (instances live at device level) |
| nil-else don't-care convention           | Interp + 3 backends + Check                      | explicit `if`; `undef` node |
| uses==1 inlining                         | ToVerilog + ToVHDL (and its absence in Cryptol)  | `Mantle.Transform.inline` |
| div-by-zero semantics                    | divergent (Verilog `x`, VHDL guard, Cryptol error, interp ⊥) | SMT-LIB convention, all targets (spec §5.2) |
| extern port naming                       | optional in `ExternSig`; VHDL synthesizes `p<i>` | mandatory in extern decls |

## 4. Phases

Each phase lands independently with the full suite green
(`rwc-test`, `rewire-user`, and `rwe-test` when Isabelle is available);
"gate" lists the phase-specific evidence beyond that.

### Phase 0 — review and decisions [DONE]

`doc/core.md` reviewed and its open questions resolved (now §11 "Resolved
design questions": Mantle name permanent, `undef` prints distinctly,
greater-than prims included, no signed division, no combinational
feedthrough). Additionally:

- **Flags and extensions.** Proposal: during migration, a hidden `-d`-style
  dump for bridged Mantle; at the end, `--core` and `--from-core` keep their
  names but the `.rwc` format becomes Mantle's concrete syntax (one golden
  regeneration, no driver renames). The old format gets no compatibility
  parser — `.rwc` files are compiler-generated artifacts, not user code.
- **Namespace.** `ReWire.Mantle.*` permanently (no end-of-project rename).

### Phase 1 — kernel: `Mantle.Syntax`, `Pretty`, `Parse`, `Check` [DONE]

New modules, no consumers yet. The AST mirrors the spec; every node carries
`Annote` and a cached width (`SizeAnnotated` continues). Property test:
parse ∘ print = id on generated terms (and later on every bridged regression
program).

Gate: module unit tests; no behavior change anywhere.

### Phase 2 — the bridge: `Mantle.FromCore` [DONE]

`Core.Device → Mantle program`. The concentrated, careful part — this module
re-states Core's implicit semantics explicitly, one last time:

- **Patterns** → `let d = disc in …` with slices per `PatVar` field, an
  `eq`-conjunction guard per `PatLit` field; nil-else calls translate
  unguarded (don't-care rule); non-nil else becomes the `if`'s else arm.
- **Prim mapping** per doc/core.md §9.1 (`LAnd`→`redor`+`and`, `Resize`→
  `zext`/`trunc` chosen by widths, `Reverse`/`Id`→slices+concat, …).
- **Widths**: Core's checked invariants (arith already width-homogeneous)
  mean few coercions are needed beyond `Resize`, pattern-field padding, and
  else-branch alignment — but every one becomes an explicit node here.
- **Device**: split `loop`/`state0` into registers via the existing
  `pausePadding`/`dispatchWires`/`resumptionSize` (imported from
  `Core.Interp`, their final use): one `tag` register (if the dispatch tag
  is nonempty), one register per `stateWires` entry, init values sliced from
  `state0`'s constant body; the body becomes
  `let r = loop(tag ⧺ states ⧺ inputs…) in` + per-output and per-register
  slice equations. Padding bits are simply not stored (they were
  write-only).
- **Refs**: `GetRef r`/`SetRef r` become a device-level `let r = …` and
  references to it. Constraint check: refs must be set exactly once and only
  in positions that reach device level after the inline pass (today refs
  only work flattened anyway — line `Core.ToVerilog:319` reads the ref as a
  bare name in whatever module it lands in). Reject otherwise, with a clear
  error; no regression test currently violates this.
- **Clocked externs**: hoist each clocked-extern call to a device-level
  `instance` + port equations. Sound when the call site is reached
  unconditionally once per cycle (mux-surrounded calls hoist with the input
  expressions as-is — the instance exists regardless, exactly as in the
  generated RTL today). Calls inside multiply-used defns are the ambiguous
  case today (instance count depends on inlining); the bridge forces the
  question: inline the path to the call, then hoist. Synthesize `p<i>` port
  names for unnamed extern ports here (Mantle requires names), matching the
  VHDL backend's existing convention so the hand-written extern
  implementations in `tests/regression/vhdl/` keep working.

Wire into `ModCache.getDevice` as a numbered pass (run + `Mantle.Check`
after the Core checkpoint) when any Mantle consumer is requested; add a
dump flag.

Gate: `Mantle.Check` passes on the bridged output of every regression test
(incl. the perf-suite programs); round-trip parse/print on the same corpus.

### Phase 3 — `Mantle.Interp` [DONE]

Transcribe the spec's semantics. Then, before swapping anything: run **both**
interpreters over every regression test with the existing pseudorandom
stimulus machinery and assert byte-identical traces (a temporary
differential-test mode in `rwc-test`, kept until phase 6). Divergences at
this stage are bridge bugs or spec bugs — this is the cheapest place to find
them, and it exercises the bridge against the only complete executable model
of Core's semantics.

Then switch `--interpret` (and the cosim reference leg) to the Mantle path.
`.yaml` goldens must not change at all.

Gate: differential interp identity on all tests; `.yaml` goldens
byte-identical; perf within noise on `rwc-perf` (interp leg).

### Phase 4 — backends, one at a time [DONE]

Order: **Cryptol → Verilog → VHDL.** Cryptol first because it is nearly the
§8.4 embedding (smallest rewrite, validates the functional fragment against
the freshly-trusted Mantle interpreter via cosim); Verilog before VHDL
because it is the default target and iverilog/verilator lint plus cosim give
the densest signal; VHDL last, banking the `rw_helpers` diet.

Per backend:

1. Rewrite `ToX` against `Mantle.Syntax` (the old module stays until cut-over).
2. The testbench generators (`testbench` in ToVerilog/ToVHDL) move with
   their backends; they get simpler (register/port names are now IR-level,
   not reconstructed).
3. Validate: HDL lint + full four-way cosim green across the regression
   suite **before** regenerating goldens.
4. Regenerate `.sv`/`.vhdl`/`.cry` goldens (output text will differ —
   explicit coercions, named registers, new wire names). Review a
   representative sample of diffs by hand (small tests + one large one);
   the cosim pass is the semantic evidence, the eyeball pass is for
   gratuitous ugliness (e.g. redundant `zext` chains → feed phase 5).

Implementation notes:

- Verilog/VHDL: emit the §5.2 division-by-zero guard (new behavior for
  Verilog — previously `x`; cosim only gets stronger). Drop `wcast`; with
  equal widths everywhere, expression emission is the template table.
- VHDL: shrink `rw_helpers.vhdl` to guarded div/mod and clamped shifts;
  keep the package-emission mechanism (`VHDL.Helpers`) unchanged.
- Cryptol: the `Sha256_1` cryptol-tool blowup is orthogonal (it's the
  Cryptol typechecker on huge modules) and remains excluded from the cosim
  leg by the existing extern gating; expect comparable module sizes.

Gate (per backend): lint + cosim green pre-golden-regen; goldens accepted
post-review; flags smoke group passes.

### Phase 5 — `Mantle.Transform` and the inline pass [DONE: inline/purge/perf-baseline; mergeSlices/partialEval/dedupe ports deferred to phase 6 -- see refactor-report.md]

Port the Core optimizations (fixpoint-iterated, `--rtl-opt`-tunable, as
today): purge, dedupe, partial evaluation (constant folding is `ℰ` on closed
subterms), slice/concat algebra. Add:

- **inline**: the uses==1 heuristic (plus trivial-body inlining), moved out
  of both RTL backends; `--flatten` becomes "inline everything".
- **coercion fusion** and `undef` absorption — cleans the bridge's output
  and addresses the main perf risk (more IR nodes from explicit coercions).

Re-baseline `rwc-perf` and compare scaling slopes per family against a
pre-migration baseline run; investigate any slope change, not just constants.

Gate: cosim green; goldens stable or improved; perf slopes unchanged,
end-to-end times within ~10% of baseline.

### Phase 6 — direct producer, delete Core [DONE]

Rewrite `Crust.ToCore` → `Crust.ToMantle`: same traversal, but emitting
Mantle directly — patterns to slices/guards at the source of truth
(`transPat`-equivalents), widths explicit from Crust types, extern models
attached to extern decls, the purified state machine emitted as registers
and equations (the `pausePadding` arithmetic finally dies here, replaced by
construction rather than reconstruction). The bridge's regression corpus is
the safety net: temporarily keep both paths and assert
`ToMantle(crust) ≡ bridge(ToCore(crust))` modulo names on the full suite,
then delete the bridge, `Crust.ToCore`, and `ReWire.Core.*`.

CLI: `--core`/`--from-core`/`-d` pass numbering updated; `.rwc` goldens
regenerated in Mantle syntax. The warning/extern-model behaviors carry over
(`W4` zero-for-error becomes `undef`; model verdict logic in
`Crust.Transform` is unchanged).

Gate: full suites; differential producer identity before bridge deletion;
`rwc -v` pass list and docs updated.

### Phase 7 — cleanup

`CHANGES.md` entry; `doc/core.md` status flipped from proposal to
normative; update the test-driver notes and pass documentation in the
README/tutorial where they mention Core constructs; retrospective addendum
to `rtl-backends.md` (local); CLAUDE.md (local) updated for the new module
map. Audit `package.yaml` data-files and the negative/warning suites for
messages that mention "core" phrasing.

## 5. Testing summary

- **Primary oracle**: the existing four-way cosimulation (unchanged
  harness), applied at every phase boundary. It caught the `>>>` signedness
  bug between two mature backends; it is the reason this migration can be
  incremental.
- **Differential identity tests** (temporary): Core-interp vs. Mantle-interp
  (phase 3); bridged vs. direct producer (phase 6).
- **Golden policy**: `.yaml` goldens must never change (interpreter
  semantics is preserved exactly — the div-by-zero change is unobservable
  there since the interpreter currently diverges on it). `.sv`/`.vhdl`/
  `.cry`/`.rwc` goldens regenerate once per relevant phase, only after
  cosim is green, with sampled human review.
- **New tests to add**: width-0 edge cases (ports, registers, slices);
  div/mod-by-zero stimulus (now well-defined across all four targets —
  previously untestable); a multi-register device exercising register
  naming; parse/print round-trip property for Mantle.

## 6. Risks

| risk | mitigation |
|------|------------|
| Golden churn masks a real regression | hard rule: cosim + lint green *before* any golden regeneration; `.yaml` byte-identity throughout |
| Bridge mis-states an implicit Core convention | phase 3 differential interp testing over the whole corpus + random stimulus, before any backend depends on the bridge |
| IR growth from explicit coercions slows compilation | phase 5 fusion; `rwc-perf` slope comparison gates phases 3–6 |
| Clocked-extern hoisting hits a program shape it can't handle | bridge rejects with a precise error; current regression suite has no such shape; the inline-then-hoist fallback covers conditional calls |
| Ref semantics subtly differ once hoisted | refs are barely load-bearing today (only work flattened); bridge enforces single-set; add a dedicated regression test before phase 2 lands |
| Two IRs in-tree confuse contributors | bridge phase kept short; CLAUDE.md notes the layering; only one *producer* exists at all times |
| Verilog div-by-zero guard changes synthesized netlists | semantically a fix (defined instead of `x`); flagged in CHANGES.md; guard is dead logic when the divisor is provably nonzero after constant folding |

## 7. Sequencing and effort (rough)

Phases 1–2 are the design-sensitive work and dominate review effort; phase 3
is small but is the validation keystone; each backend in phase 4 is roughly
"rewrite of a 600–700-line module that gets shorter"; phases 5–6 are
mechanical against an established corpus. Phases must land in order, but
4a/4b/4c are independent of each other and 5 can begin once any backend is
on Mantle. No calendar estimates here — the gates, not dates, are the plan.

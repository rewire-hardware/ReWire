# Certified compilation: `rwc --certify`

`--certify` runs the formally verified translation validator over the
compilation it accompanies: instead of trusting the compiler's middle and
back passes, it checks — with a machine-checked proof standing behind the
check — that the device rwc produced implements the machine-level IR it
was produced from.

Certification is required by default: plain `--certify` fails the
compilation unless the validator returns VALIDATED. `--certify=warn`
compiles best-effort instead, always surfacing the verdict (the
VALIDATED confirmation on stdout, any other status as a dedicated line
on stderr) — the status is not an ordinary warning, so `-w` cannot
suppress it and `-Werror` does not govern it.

## What it produces

On any compilation from Haskell source to a device target (Verilog, VHDL,
Cryptol, or `--core`), `--certify` writes two artifacts beside the output
(for `-o dir/x.sv`: `dir/x.syn` and `dir/x.rwc`; with no `-o`, beside the
source file, overwriting any existing `<src>.rwc` — which is harmless when
that file came from `--core` with the same flags, since the bytes agree):

- **`<out>.syn`** — the Synolon machine IR after the block-graph cleanup
  (pass 8), exactly the file `--synolon` writes; its semantics is
  doc/synolon.md §5.
- **`<out>.rwc`** — the final, fully inlined Hyle program (after the
  Hyle optimize and inline passes, 10–11), byte-identical to the `.rwc`
  that `--core` emits. Every device target consumes this same program —
  the HDL and Cryptol backends, the interpreter, and `--core` — so the
  certified program is always exactly the consumed one; its semantics
  is doc/hyle.md §6.

Both artifacts are published with a same-directory temporary file and a
rename, so an interrupted compilation cannot leave a torn artifact in
place. An output naming that would collide with either artifact (`-o
*.rwc` for an HDL/Cryptol target, or `-o *.syn` for any target) is
refused up front in required mode; under `--certify=warn` it surfaces as
a not-validated status.

It then invokes the validator `rwv-cstep-validate` on the pair over a
versioned machine-readable protocol and surfaces the verdict. The
verdicts form four classes, and each maps to a validator exit code:

- **VALIDATED** (exit 0) — every checked obligation holds; rwc prints a
  one-line confirmation naming the two artifacts.
- **REJECTED** (exit 1) — a well-formedness or proof obligation failed
  on supported inputs: the compilation is wrong, or an artifact is
  ill-formed.
- **ERROR** (exit 2) — malformed input, bad options, or an I/O failure.
- **UNSUPPORTED** (exit 3) — the device uses a feature outside the
  certified profile (see the support matrix below). No verdict is
  claimed either way.

In required mode anything but VALIDATED fails the compilation; the
artifacts are left in place for rerunning the validator by hand:
`rwv-cstep-validate <out>.syn <out>.rwc [-v]`. (A validator built before
the Synolon artifact existed rejects the pair with an ERROR about a
missing `top` line: it is reading the `.syn` as the older whole-program
dump. Rebuild it — `lake build` under `verify/` — and make sure the copy
rwc selects, see below, is the rebuilt one.)

`--interpret` and `--from-core` have nothing to certify (no device
output, and no Synolon IR, respectively): required mode fails,
`--certify=warn` reports.

## The response protocol

Because `rwc` invokes the validator executable as a subprocess, we have implemented
a simple handshake protocol with the validator to increase our confidence that
it has validated the correct files. `rwc` invokes the validator with `--protocol=2
--nonce=<fresh>`, and requires all of the following before printing its
VALIDATED confirmation:

- the process exited with status 0;
- stdout carries exactly one line, a JSON object naming the tool
  (`rwv-cstep-validate`), protocol version 2, and a verdict status;
- the response echoes the invocation's nonce; and
- the response's SHA-256 hashes of the source and target artifacts match
  hashes rwc computed independently from the same bytes.

Spawn failures, timeouts, nonzero exits, malformed or ambiguous output,
and mismatched identities all classify as errors. (This binds an honest
validator's response to the invocation; a maliciously substituted
validator executable can still lie — the selected executable is part of
the trust base below.)

## What VALIDATED means

The validator is a Lean 4 program (under `verify/`) whose acceptance
carries a kernel-checked soundness theorem (`validateProc_corresponds`,
in `Rwv.Synolon.Cstep`; axiom-clean up to `propext`, `Classical.choice`,
`Quot.sound`). When it accepts a pair, the theorem concludes the
correspondence of doc/synolon.md §5.6 for the dumped process and program:
for every input trace of well-typed semantic values, whenever the
mechanized Synolon machine semantics produces a trace and the mechanized
Hyle stream semantics produces a trace on the port-split encodings of
those inputs, the device trace *is* the encoding of the machine trace,
cycle for cycle — in full when the machine never halts, and up to (and
excluding) the halting cycle when it does (the doc/synolon.md §5.4
prefix reading).

The gap between the executable and the theorem is closed by one pure
entry point: `Rwv.Synolon.validateBundle` owns every gate between the
artifact texts and the library validator, and its top-level theorem
`validateBundle_sound` (axiom-clean, same three axioms) concludes the
correspondence from a `.validated` result alone — parsing, the
primitive-basis and fresh-unique gates, the foreign-occurrence scan,
both well-formedness judgments, the denoting definition environment,
eta saturation, and the `ForeignC` premise are all discharged
internally, and the conclusion names the exact processing chain, so a
caller knows precisely which program was certified. The driver's
verdict is this function's result.

The verdict is moreover a **forward refinement**, not merely
conditional agreement: `validateBundle_refines` (axiom-clean, same
three axioms) concludes that every successful, well-typed source
execution HAS a successful target execution with an agreeing trace — a
target that can never run cannot satisfy the theorem, so vacuous
validation is structurally impossible rather than gated away. The
target-run existence rests on the mechanized progress result
(`Rwv.Hyle.Progress`): on a program `Program.check` accepts, with a
denoting definition environment, the dynamic semantics hits none of
its error cases. The one checked construct the semantics nevertheless
rejects — device instances — is refused a verdict (UNSUPPORTED) by an
explicit bundle gate.

One boundary caveat qualifies the theorem:

- **The certified source artifact is the eta-saturated program.** The
  drivers saturate under-applied constructor/primitive occurrences to
  signature arity after parsing (the same normalization rwc's own
  pipeline applies before the fold); the mechanized source semantics
  runs on the saturated program, and eta saturation itself is untrusted
  plumbing, not covered by the theorem. The primitive datatype basis is
  part of the artifact's semantics: a declaration that conflicts with
  the canonical basis is refused (never silently replaced), and inputs
  using the fresh-unique range the saturation mints from are refused.

## The trust base

The trust base of a VALIDATED verdict:

The mathematical core —

- the Lean kernel and the three standard axioms above;
- the two mechanized semantics being the intended readings of
  doc/synolon.md §5 and doc/hyle.md §6. Differential testing against
  `rwc --interpret` pins them for the interpreter-evaluable fragment
  (model-carrying externs included — the interpreter evaluates the
  model); for **model-less** externs the interpreter cannot run, so the
  eta tier's differential evidence is a Lean-vs-Lean self-test, not an
  external oracle;
- the parsers that read the two dumps, and the printers that wrote them
  (the round-trip legs of rwc-test exercise both);
- the eta-saturation and canonical-basis preprocessing described above.

The runtime path —

- Lean's native code generation and runtime, which execute the checker;
- the validator executable actually selected (`RWC_RWV`, then next to
  rwc, then the PATH — see below) and the response protocol connecting
  it to rwc;
- filesystem artifact identity: the hashes bind the response to the
  bytes read, but the retained files can be modified afterward.

The validator's internals — its planning, symbolic evaluation, and
normalizers — are *untrusted*: a wrong answer inside them yields
REJECTED, never an unsound VALIDATED.

What is covered is the compiler's middle and back half: the
Synolon-to-Hyle fold (pass 9) and the Hyle-level optimization and inlining
(passes 10–11). Passes before the pass-8 dump (the GHC front end, the
Eidos passes, procification) and the HDL backends after Hyle are not
covered; the cosimulation legs of rwc-test remain the check on those.

## Support matrix

| Feature | Status |
|---|---|
| Pure devices (no foreign calls) | **VALIDATED** |
| Model-less combinational externs, no static generics | **VALIDATED**, universally: the verdict quantifies over all implementations of the extern (the ∀η tier) |
| Model-less combinational externs with static generics | **VALIDATED**, universally per instantiation: the η environment is keyed by (name, generic values), the compiled call carries the source descriptor's values, and distinct instantiations are distinct uninterpreted symbols |
| Model-carrying combinational externs | **VALIDATED**: the occurrence's source-side meaning is its own implementation argument (the model rwc kept beside the extern in the Synolon artifact), evaluated and compiled as an ordinary expression; the target's model meets that independently obtained form through the ordinary translation obligations |
| Cryptol foreign functions (`rwPrimCryptol`) | UNSUPPORTED: no independent source-side semantics exists in the artifact bundle (the pass-8 artifact carries only a placeholder) |
| Clocked (sequential) externs / device instances | UNSUPPORTED (no stream-level instance semantics or proof) |
| Multiple processes | UNSUPPORTED |

Whether an extern occurrence carries a model is decided from the Synolon
artifact alone (the model-less idiom is syntactic: the implementation
argument is the `rwPrimError "Extern expression placeholder"`
application) — changing the target can never select a different
validation rule; a source/target disagreement about model-ness is
REJECTED. Re-enabling the Cryptol row requires source semantics
constructed independently of the target: retained source syntax with
mechanized semantics, or an explicitly trusted oracle artifact under a
distinct verdict.

## Building and locating the validator

The validator is built from the Lean project in `verify/`:

```sh
cd verify && lake build rwv-cstep-validate
```

(with a Lean toolchain installed via elan; the toolchain version is
pinned by `verify/lean-toolchain`). rwc looks for the binary in order:
the `RWC_RWV` environment variable (which must name an executable file —
a broken override fails certification rather than falling through), next
to the rwc executable, then the PATH. There is deliberately no
cwd-relative fallback: the selected executable is trusted, and rwc must
not execute a binary planted in whatever directory it happens to be
invoked from. In a checkout, set
`RWC_RWV=$PWD/verify/.lake/build/bin/rwv-cstep-validate` or install the
binary on the PATH.

## Tests

The rwc-test "certify" group runs `--certify` end-to-end over a
representative subset of the golden tests: the enrolled subset — pure
devices (the type-class tests included) plus the extern-model and
extern-generics tests — must VALIDATE, the clocked-extern test must
fail required-mode certification with UNSUPPORTED, and a warn-mode leg
checks the unsuppressible status line. The group is skipped when the
validator binary is not found, unless `RWC_TEST_REQUIRE_RWV` is set (the
CI certification lane), in which case a missing validator fails.

`verify/test/mutation-tests.py` is the adversarial suite: scripted
semantic mutations of generated artifact pairs (target-only extern-model
edits, primitive-basis substitutions, non-denoting targets,
assignment-coverage collisions, protocol spoofing with fake validator
executables, ...) each asserting the exact verdict class and exit code.
`verify/test/cstep-goldens.py` sweeps the whole dump corpus against
per-fixture expected verdicts and exits nonzero on any unexpected
result.

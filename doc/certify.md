# Certified compilation: `rwc --certify`

`--certify` runs the formally verified translation validator over the
compilation it accompanies: instead of trusting the compiler's middle and
back passes, it checks — with a machine-checked proof standing behind the
check — that the device rwc produced implements the machine-level IR it
was produced from.

Certification is required by default: plain `--certify` fails the
compilation unless the validator returns VALIDATED. `--certify=warn`
compiles best-effort instead, always printing a dedicated certification
status line to stderr — the status is not an ordinary warning, so `-w`
cannot suppress it and `-Werror` does not govern it.

## What it produces

On any compilation from Haskell source to a device target (Verilog, VHDL,
Cryptol, or `--core`), `--certify` writes two artifacts beside the output
(for `-o dir/x.sv`: `dir/x.eir` and `dir/x.rwc`; with no `-o`, beside the
source file, overwriting any existing `<src>.rwc` — which is harmless when
that file came from `--core` with the same flags, since the bytes agree):

- **`<out>.eir`** — the machine-mode Eidos IR after the block-graph
  cleanup (pass 8), exactly the file `--eidos` writes; its semantics is
  doc/eidos.md §7.5.
- **`<out>.rwc`** — the final, fully inlined Hyle program (after the
  Hyle optimize and inline passes, 10–11), byte-identical to the `.rwc`
  that `--core` emits. Every device target consumes this same program —
  the HDL and Cryptol backends, the interpreter, and `--core` — so the
  certified program is always exactly the consumed one; its semantics
  is doc/hyle.md §6.

Both artifacts are published with a same-directory temporary file and a
rename, so an interrupted compilation cannot leave a torn artifact in
place. An output naming that would collide with either artifact (`-o
*.rwc` for an HDL/Cryptol target, or `-o *.eir` for any target) is
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
`rwv-cstep-validate <out>.eir <out>.rwc [-v]`.

`--interpret` and `--from-core` have nothing to certify (no device
output, and no Eidos IR, respectively): required mode fails,
`--certify=warn` reports.

## The response protocol

rwc does not trust the validator's human-readable output. It invokes the
validator with `--protocol=2 --nonce=<fresh>`, and requires all of the
following before printing its VALIDATED confirmation:

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
in `Rwv.Eidos.Cstep`; axiom-clean up to `propext`, `Classical.choice`,
`Quot.sound`). When it accepts a pair, the theorem concludes the
correspondence of doc/eidos.md §7.5.6 for the dumped process and program:
for every input trace of well-typed semantic values, whenever the
mechanized Eidos-M machine semantics produces a trace and the mechanized
Hyle stream semantics produces a trace on the port-split encodings of
those inputs, the device trace *is* the encoding of the machine trace,
cycle for cycle — in full when the machine never halts, and up to (and
excluding) the halting cycle when it does (the doc/eidos.md §7.5.4
prefix reading).

Two boundary caveats qualify the theorem:

- **The correspondence is conditional on both runs succeeding.** The
  validator therefore separately requires the target to be well formed
  (`Rwv.Hyle.Program.check`) and its definition environment to denote
  (`Rwv.Hyle.Sem.mkFEnv`), and the source to satisfy the machine
  well-formedness judgment (`Rwv.Eidos.Check.checkMachine`), so that a
  target that could never execute cannot validate vacuously. A proved
  progress/refinement theorem (a successful source run entails a
  successful target run) does not exist yet.
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
  doc/eidos.md §7.5 and doc/hyle.md §6. Differential testing against
  `rwc --interpret` pins them **for the non-foreign fragment only**: for
  model-less externs the interpreter cannot run, so the eta tier's
  differential evidence is a Lean-vs-Lean self-test, not an external
  oracle;
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
Eidos-to-Hyle fold (pass 9) and the Hyle-level optimization and inlining
(passes 10–11). Passes before the pass-8 dump (the GHC front end, the
Eidos front half, procification) and the HDL backends after Hyle are not
covered; the cosimulation legs of rwc-test remain the check on those.

## Support matrix

| Feature | Status |
|---|---|
| Pure devices (no foreign calls) | **VALIDATED** |
| Model-less combinational externs, no static generics | **VALIDATED**, universally: the verdict quantifies over all implementations of the extern (the ∀η tier) |
| Model-less combinational externs with static generics | UNSUPPORTED (the mechanized extern environment is not keyed by generics) |
| Model-carrying combinational externs | UNSUPPORTED: the model's source-side meaning currently exists only as compiler output, and validating against that would let the target define the semantics it is checked against |
| Cryptol foreign functions (`rwPrimCryptol`) | UNSUPPORTED, for the same reason: no independent source-side semantics exists in the artifact bundle |
| Clocked (sequential) externs / device instances | UNSUPPORTED (no stream-level instance semantics or proof) |
| Multiple processes | UNSUPPORTED |

Whether an extern occurrence carries a model is decided from the Eidos
artifact alone (the model-less idiom is syntactic: the implementation
argument is the `rwPrimError "Extern expression placeholder"`
application), and the target's extern declaration is cross-checked to
agree — changing the target can never select a different validation
rule. Re-enabling the model-carrying and Cryptol rows requires source
semantics constructed independently of the target (for extern models,
by evaluating the model's Eidos definition; for Cryptol, retained
source syntax with mechanized semantics, or an explicitly trusted
oracle artifact under a distinct verdict).

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
representative subset of the golden tests: the pure subset must
VALIDATE, the foreign-tier tests (clocked and model-carrying externs)
must fail required-mode certification with UNSUPPORTED, and a warn-mode
leg checks the unsuppressible status line. The group is skipped when the
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

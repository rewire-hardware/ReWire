# Certified compilation: `rwc --certify`

`--certify` runs the formally verified translation validator over the
compilation it accompanies: instead of trusting the compiler's middle and
back passes, it checks — with a machine-checked proof standing behind the
check — that the device rwc produced implements the machine-level IR it
was produced from.

## What it produces

On any compilation from Haskell source to a device target (Verilog, VHDL,
Cryptol, or `--core`), `--certify` writes two artifacts beside the output
(for `-o dir/x.sv`: `dir/x.eir` and `dir/x.certify.rwc`):

- **`<out>.eir`** — the machine-mode Eidos IR after the block-graph
  cleanup (pass 8), exactly the file `--eidos` writes; its semantics is
  doc/eidos.md §7.5.
- **`<out>.certify.rwc`** — the final, fully inlined Hyle program (after
  the Hyle optimize and inline passes, 10–11). Every device target
  consumes this same program — the HDL and Cryptol backends, the
  interpreter, and the `.rwc` that `--core` emits — so the certified
  program is always exactly the consumed one; its semantics is
  doc/hyle.md §6.

It then invokes the validator `rwv-cstep-validate` on the pair and
surfaces the verdict:

- **VALIDATED** — a one-line confirmation naming the two artifacts is
  printed to stdout.
- **Anything else** — REJECTED, a device outside the validated fragment,
  or a missing validator binary — is a warning (so `-Werror` makes it
  fatal); certification never passes silently. The artifacts are left in
  place for rerunning the validator by hand:
  `rwv-cstep-validate <out>.eir <out>.certify.rwc [-v]`.

`--interpret` and `--from-core` have nothing to certify (no device
output, and no Eidos IR, respectively) and warn.

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
excluding) the halting cycle when it does (the §7.5.4 prefix reading).

The trust base of a VALIDATED verdict is exactly:

- the Lean kernel and the three standard axioms above;
- the two mechanized semantics being the intended readings of
  doc/eidos.md §7.5 and doc/hyle.md §6 (each pinned separately by
  differential testing against `rwc --interpret`);
- the parsers that read the two dumps, and the printers that wrote them
  (the round-trip legs of rwc-test exercise both).

The validator itself — its planning, symbolic evaluation, and
normalizers — is *untrusted*: a wrong answer anywhere inside it yields
REJECTED, never an unsound VALIDATED.

What is covered is the compiler's middle and back half: the
Eidos-to-Hyle fold (pass 9) and the Hyle-level optimization and inlining
(passes 10–11). Passes before the pass-8 dump (the GHC front end, the
Eidos front half, procification) and the HDL backends after Hyle are not
covered; the cosimulation legs of rwc-test remain the check on those.

Scope: the extern-free fragment. Devices with device instances (clocked
externs) or extern calls (including Cryptol foreign functions) are
outside the validated fragment; `--certify` warns rather than claiming a
verdict for them.

## Building and locating the validator

The validator is built from the Lean project in `verify/`:

```sh
cd verify && lake build rwv-cstep-validate
```

(with a Lean toolchain installed via elan; the toolchain version is
pinned by `verify/lean-toolchain`). rwc looks for the binary in order:
the `RWC_RWV` environment variable, next to the rwc executable, the
PATH, and `verify/.lake/build/bin` relative to the current directory
(the in-checkout build location).

The rwc-test "certify" group runs `--certify` end-to-end over a
representative subset of the golden tests (plus the extern tests, which
must warn); it is skipped when the validator binary is not found.

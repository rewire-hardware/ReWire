/-
rwv-eidos-diff: the differential-testing driver for the mechanized
Eidos-M machine semantics (doc/eidos.md §7.5) against rwc's compiled
Hyle program — the §7.5.6 correspondence, checked per test by trace
comparison.

    rwv-eidos-diff <file.eir> <file.rwc> [--cycles N] [--seed S]
        [--stim FILE] [--fuel N] [--foreign FILE.rwc] [--eta-synth]

parses the pass-8 Eidos dump (`rwc --eidos`; must contain exactly one
proc), parses the compiled .rwc to obtain the device's port names and
widths, generates a deterministic pseudorandom ALGEBRAIC stimulus of
the proc's input type τ_I, and

  * checks that `Val.detupleSizes` at τ_I/τ_O reproduces exactly the
    .rwc device's declared input/output port widths (a loud error
    otherwise — this validates the port convention against the real
    device);
  * writes the stimulus to FILE (with --stim) in rwc's inputs-file
    format — a YAML block sequence of `port: value` maps, every port
    written every cycle (so rwc's sticky-map semantics is inert), the
    values the decimal readings of `Val.portSplit v` at τ_I;
  * runs the §7.5.4 halt-prefix machine semantics (`Proc.run`) on the
    algebraic inputs and prints the output trace to stdout in exactly
    rwc's YAML trace format (via Rwv.Diff.printTrace, port-splitting
    each output value at τ_O against the device's output port names),

so with the same stimulus file

    rwc <file.rwc> --from-core --interpret=FILE --cycles N -o hs.yaml

must agree byte for byte (up to the halt prefix). Driven across the
golden corpus by verify/test/eidos-diff-goldens.py.

## The stimulus generator

Generating the stimulus on the algebraic side (and encoding it through
`rep`/`portSplit`) avoids needing a bits→algebraic decoder; padding
bits in ADT encodings are zero by construction on both sides. The
generator is a 32-bit xorshift PRNG with the exact constants of
apps/rwc-test/Cosim.hs (xorshift32 with shifts 13/17/5; seed
`seed0 name` = fold h·31+ord(c) over the test's base name from
0x12345678), seeded from the .eir file's base name (or --seed, which
overrides). One cycle's value of type τ is drawn structurally:

  * `Vec n τ`   — n elements, generated left to right;
  * `Finite n`  — ⌈nbits n / 32⌉ (min 1) 32-bit draws, MSB-first,
                  reduced mod n (uniform-ish: the mod bias is ≤ n/2³²);
  * `Integer`   — four 32-bit draws, MSB-first (the 128-bit residue);
  * `Proxy k`   — the proxy value (no draw);
  * an ADT (including Bool, unit, and the tuple family) — one draw
    mod #constructors picks the constructor (no draw when there is
    only one), then its fields are generated left to right at the
    instance types (result-type matching as in `DEnv.ctorWidth`).

The draw sequence therefore depends only on the seed and the structure
of τ_I, making runs reproducible per test name.

## Eta-saturation (a pre-pass over the parsed program)

Pass-8 dumps contain under-applied constructor and primitive
occurrences, which the committed evaluator (deliberately, decision
note 4 of Rwv/Eidos/Eval.lean) rejects: eta-reduced definitions whose
body is a bare builtin (`Main.plusW8 = rwPrimAdd`, tests/golden/
dissex.hs), and pattern-match-failure joins in curried definitions
whose `rwPrimError` occurrence is applied to the message only, at a
FUNCTION result type (`Main.memLookup`'s `$fail`, tests/golden/
SecMemCon.hs) — the reference translation handles both by eta-expanding
to signature arity during its mono+ANF normalization (doc/eidos.md §6;
ToHyle's etaExpand), compiling the error case to `undef` (= zero, like
`zeroVal`) at the eta-expanded, representable width.

So before evaluating we eta-saturate: every constructor or primitive
head applied to fewer term arguments than its carried instantiated
type's arrow spine is wrapped in lambdas supplying the missing
arguments (`p ā` with k of n args becomes `λx̄. p ā x̄`). This is
semantics-preserving where the evaluator was already defined
(call-by-value beta at value arguments) and gives the two corpus shapes
their reference meaning: the bare-builtin body becomes a lambda
closure, and `rwPrimError`'s variadic row then sees enough arguments
to land on a representable (zero-value) result type. Fresh binder
uniques are minted from -10⁹ down — far below both the bridge's
non-negative term uniques and the prim basis' small negatives — so
they cannot capture.

## The foreign tier (Cryptol splices and extern models)

Programs using the Cryptol FFI or model-carrying combinational externs
evaluate through the DEnv foreign hooks (Eval decision note 9): the
driver instantiates them from the compiled .rwc itself — the trust
boundary under which the rwcry-spliced `cry$…` definitions (resp. the
extern's model definition) ARE the builtin's semantics:

  * `xtF` is the Hyle program's own extern-model composition
    (`Sem.xenv` then `Sem.mkFEnv`'s denotations), keyed by extern
    name — exactly `evalExp`'s xcall reading;
  * `cryF` needs the (file, function, monotype) ↦ entry-name map,
    which rwc does not emit structurally; the driver scrapes it from
    the `--| cryptol <file>::<fn> at <cty>` doc lines rwc prints on
    each spliced entry definition, transcribing ToHyle's `cryTy`
    rendering to match the use-site monotype against `<cty>`. The
    scrape is UNTRUSTED plumbing: a wrong entry shows up as a trace
    mismatch (or a decode canonicality error), never silently.

Exit codes: 0 success (including an early halt, which is reported on
stderr and prints only the trace prefix), 1 parse/mismatch/evaluation
failure, 2 usage error.
-/
import Rwv.Eidos.Parse
import Rwv.Eidos.PrimBasis
import Rwv.Eidos.Check
import Rwv.Eidos.Machine
import Rwv.Eidos.EtaSat
import Rwv.Eidos.ForeignEnv
import Rwv.Hyle.Parse
import Rwv.Diff

open Rwv.Eidos
open Rwv.Hyle (BV)
open Std (HashMap)

/-! ## Arguments -/

structure Args where
  eirFile : String
  rwcFile : String
  cycles  : Nat := 20
  seed    : Option Nat := none
  stimOut : Option String := none
  fuel    : Nat := 100000000
  foreignF : Option String := none
  etaSynth : Bool := false

def usage : String :=
  "usage: rwv-eidos-diff <file.eir> <file.rwc> [--cycles N] [--seed S] [--stim FILE] [--fuel N] [--foreign FILE.rwc] [--eta-synth]"

private def natOpt (flag val : String) : Except String Nat :=
  match val.toNat? with
  | some v => pure v
  | none   => throw s!"{flag}: expected a non-negative integer, got '{val}'"

def parseArgs (argv : List String) : Except String Args := do
  let mut positional : List String := []
  let mut cycles : Nat := 20
  let mut seed : Option Nat := none
  let mut stim : Option String := none
  let mut fuel : Nat := 100000000
  let mut foreignF : Option String := none
  let mut etaSynth : Bool := false
  let mut rest := argv
  repeat
    match rest with
    | [] => break
    | "--eta-synth" :: more => etaSynth := true; rest := more
    | "--cycles" :: v :: more => cycles := (← natOpt "--cycles" v); rest := more
    | "--seed"   :: v :: more => seed := some (← natOpt "--seed" v); rest := more
    | "--fuel"   :: v :: more => fuel := (← natOpt "--fuel" v); rest := more
    | "--stim"   :: v :: more => stim := some v; rest := more
    | "--foreign" :: v :: more => foreignF := some v; rest := more
    | [f] =>
        if f = "--eta-synth" then
          etaSynth := true
          rest := []
        else if f = "--cycles" || f = "--seed" || f = "--stim" || f = "--fuel" || f = "--foreign" then
          throw s!"{f}: missing argument"
        else
          if f.startsWith "-" && f ≠ "-" then throw s!"unknown option: {f}"
          positional := positional ++ [f]
          rest := []
    | arg :: more =>
        if arg.startsWith "--cycles=" then cycles := (← natOpt "--cycles" ((arg.drop 9).toString))
        else if arg.startsWith "--seed=" then seed := some (← natOpt "--seed" ((arg.drop 7).toString))
        else if arg.startsWith "--fuel=" then fuel := (← natOpt "--fuel" ((arg.drop 7).toString))
        else if arg.startsWith "--stim=" then stim := some ((arg.drop 7).toString)
        else if arg.startsWith "--foreign=" then foreignF := some ((arg.drop 10).toString)
        else if arg.startsWith "-" && arg ≠ "-" then throw s!"unknown option: {arg}"
        else positional := positional ++ [arg]
        rest := more
  match positional with
  | [eir, rwc] => return { eirFile := eir, rwcFile := rwc, cycles, seed, stimOut := stim, fuel,
                           foreignF, etaSynth }
  | _          => throw usage

/-- The base name of a path: strip directories and the last extension
(`verify/test/out-eidos/fibo1.eir` ↦ `fibo1`) — the default PRNG seed
key. -/
def baseName (path : String) : String :=
  let name := ((path.splitOn "/").getLast?.getD path)
  match name.splitOn "." with
  | []  => name
  | [n] => n
  | ps  => String.intercalate "." ps.dropLast

/-! ## The PRNG (Cosim-compatible xorshift32) -/

abbrev Rng := UInt32

def xorshift32 (x : Rng) : Rng :=
  let x := x ^^^ (x <<< 13)
  let x := x ^^^ (x >>> 17)
  x ^^^ (x <<< 5)

/-- `Cosim.seed0`: fold h·31+ord(c) over the name, from 0x12345678. -/
def seed0 (name : String) : Rng :=
  name.foldl (fun h c => h * 31 + UInt32.ofNat c.toNat) 0x12345678

/-- One 32-bit draw: advance, emit the new state (as Cosim does). -/
def draw32 (s : Rng) : Nat × Rng :=
  let s := xorshift32 s
  (s.toNat, s)

/-- `k` draws assembled MSB-first. -/
def drawWords (k : Nat) (s : Rng) : Nat × Rng :=
  (List.range k).foldl (init := (0, s)) fun (v, s) _ =>
    let (x, s) := draw32 s
    (v * 2 ^ 32 + x, s)

/-! ## The stimulus generator -/

/-- A pseudorandom value of a representable type (see the header for
the drawing scheme). -/
partial def genVal (Δ : DEnv) (t : Ty) (s : Rng) : Except String (Val × Rng) :=
  match Ty.flatten t with
  | (.con "Vec", [n, te]) =>
      match Ty.evalNat n with
      | some k => do
          let (elems, s) ← (List.range k).foldlM (init := ((#[] : Array Val), s))
            fun (acc, s) _ => do
              let (v, s) ← genVal Δ te s
              pure (acc.push v, s)
          pure (.vec elems.toList, s)
      | none => throw "stimulus: open Vec length"
  | (.con "Finite", [n]) =>
      match Ty.evalNat n with
      | some 0 => throw "stimulus: Finite 0 is uninhabited"
      | some k =>
          let (v, s) := drawWords (max 1 ((nbits k + 31) / 32)) s
          pure (.finite k (v % k), s)
      | none => throw "stimulus: open Finite bound"
  | (.con "Integer", []) =>
      let (v, s) := drawWords 4 s
      pure (.integer (BitVec.ofNat 128 v), s)
  | (.con "Proxy", _) => pure (.proxy, s)
  | (.con c, _) =>
      match Δ.ctors.get? c with
      | some [] => throw s!"stimulus: cannot generate a value of the abstract/uninhabited type {c}"
      | some cs => do
          let (idx, s) := if cs.length > 1 then
              let (x, s) := draw32 s
              (x % cs.length, s)
            else (0, s)
          let cn := cs[idx]!
          match Δ.ctorSig.get? cn with
          | none => throw s!"stimulus: unknown constructor signature: {cn}"
          | some sig => do
              let (targs, tres) := Ty.flattenArrow sig.ty
              let sub ← DEnv.matchTy tres t
              let (fields, s) ← targs.foldlM (init := ((#[] : Array Val), s))
                fun (acc, s) ta => do
                  let (v, s) ← genVal Δ (DEnv.substTv sub ta) s
                  pure (acc.push v, s)
              pure (.con t cn fields.toList, s)
      | none => throw s!"stimulus: unknown type {c}"
  | _ => throw "stimulus: unrepresentable input type"

/-- N cycles of stimulus. -/
def genStimulus (Δ : DEnv) (t : Ty) (n : Nat) (s : Rng) : Except String (List Val) := do
  let (vs, _) ← (List.range n).foldlM (init := ((#[] : Array Val), s)) fun (acc, s) _ => do
    let (v, s) ← genVal Δ t s
    pure (acc.push v, s)
  pure vs.toList

/-! ## Stimulus file writing (rwc's inputs format) -/

/-- The stimulus as rwc reads it: one block-sequence entry per cycle,
`port: decimal` pairs in port order, every port every cycle (making the
loader's sticky semantics inert); `[]` when there are no input ports
(the cycle count then comes from --cycles on both sides). -/
def stimText (names : List String) (cycles : List (List BV)) : String :=
  if cycles.all (·.isEmpty) then "[]\n"
  else String.join (cycles.map fun vs =>
    match names.zip vs with
    | []            => "- {}\n"
    | first :: rest =>
        "- " ++ entry first ++ "\n"
          ++ String.join (rest.map fun p => "  " ++ entry p ++ "\n"))
where
  entry (p : String × BV) : String := s!"{p.1}: {p.2.nat}"

/-! ## The synthesized η (--eta-synth)

For model-less combinational externs the Haskell interpreter refuses
the program, so no external reference trace exists. Under --eta-synth
the driver synthesizes a DETERMINISTIC pseudorandom algebraic
interpretation per extern (a canonical value of the occurrence's
result type, drawn from a seed mixing the extern's name and the
argument bits, then `rep`ped — canonical by construction, so the
decode gate always passes), installs it as the bit-level extern
environment `E` for BOTH mechanized semantics, and checks the §7.5.6
correspondence INTERNALLY: the Eidos-M trace against the mechanized
Hyle device run at the same `E` (the ∀η statement at one concrete η).
The result types are scraped from the .eir's own rwPrimExtern
occurrences (the impl monotype, argument 7). -/

/-- Collect `(extern name, impl monotype)` from every saturated
rwPrimExtern occurrence. -/
partial def scanExp (acc : List (String × Ty)) (e : Exp) : List (String × Ty) :=
  let (hd, args) := Eval.flattenApp e
  let acc := match hd, args with
    | .prim ty .«extern»,
      _ps :: _clk :: _rst :: _as :: _rs :: .litStr s :: _impl :: _inst :: _rest =>
        match (Ty.flattenArrow ty).1[6]? with
        | some ity => (s, ity) :: acc
        | none => acc
    | _, _ => acc
  let acc := args.foldl scanExp acc
  match hd with
  | .lam _ b => scanExp acc b
  | .letE (.nonRec _ rhs) b => scanExp (scanExp acc rhs) b
  | .letE (.recB bs) b => scanExp (bs.foldl (fun a pr => scanExp a pr.2) acc) b
  | .letE (.join _ _ jb) b => scanExp (scanExp acc jb) b
  | .jump _ es => es.foldl scanExp acc
  | .cases _ scrut _ alts =>
      alts.foldl (fun a alt => match alt with | .mk _ _ body => scanExp a body)
        (scanExp acc scrut)
  | .litVec _ es => es.foldl scanExp acc
  | .litList _ es => es.foldl scanExp acc
  | _ => acc

partial def scanTerm (acc : List (String × Ty)) : Term → List (String × Ty)
  | .pause out _ args => (out :: args).foldl scanExp acc
  | .goto _ args => args.foldl scanExp acc
  | .halt e => scanExp acc e
  | .cases scrut alts =>
      alts.foldl (fun a alt => match alt with | .mk _ _ t => scanTerm a t)
        (scanExp acc scrut)

def scanCmd (acc : List (String × Ty)) : Cmd → List (String × Ty)
  | .bind _ e => scanExp acc e
  | .get _ _ => acc
  | .put _ e => scanExp acc e

/-- All extern occurrences of a program, deduplicated per name with a
consistency check on the impl monotypes. -/
def scanProgram (p : Program) : Except String (List (String × Ty)) := do
  let occs := p.procs.foldl (init := p.defns.foldl (fun a d => scanExp a d.body) [])
    fun a pr => pr.blocks.foldl (init := a) fun a (_, b) =>
      scanTerm (b.cmds.foldl scanCmd a) b.term
  occs.foldlM (init := []) fun acc (s, ity) =>
    match acc.lookup s with
    | some ity' =>
        if Cexp.teq ity ity' then pure acc
        else throw s!"--eta-synth: extern {s} used at two impl monotypes"
    | none => pure ((s, ity) :: acc)

/-- The synthesized bit-level extern environment: per scraped extern,
a canonical value of the result type drawn deterministically from the
name and the argument bits, `rep`ped. -/
def synthEta (Δ : DEnv) (repFuel : Nat) (etaTys : List (String × Ty)) :
    Rwv.Hyle.Sem.EEnv := fun s _gs =>
  (etaTys.lookup s).map fun ity bv => do
    let res := (Ty.flattenArrow ity).2
    let (v, _) ← genVal Δ res (seed0 s!"{s}/{bv.width}/{bv.bits.toNat}")
    Val.rep Δ repFuel v

/-! ## Main -/

/-- Structural fuel for rep/sizeOf/detupleSizes (bounds type/value
depth, not work — generous). -/
def repFuel : Nat := 1000000

def err (msg : String) : IO UInt32 := do
  IO.eprintln s!"rwv-eidos-diff: {msg}"
  return 1

def main (argv : List String) : IO UInt32 := do
  match parseArgs argv with
  | .error e => IO.eprintln s!"rwv-eidos-diff: {e}"; return 2
  | .ok args => do
    -- The Eidos side: parse, add the prim basis, find the single proc.
    let eirTxt ← IO.FS.readFile ⟨args.eirFile⟩
    match parseEir eirTxt args.eirFile with
    | .error e => err s!"{args.eirFile}: parse error: {e}"
    | .ok p₀ => do
      let p₁ := addPrims p₀
      -- The machine-mode well-formedness judgment (Rwv.Eidos.Check),
      -- on the program as rwc dumped it (pre eta-saturation, which is
      -- this driver's local workaround) — so the differential harness
      -- exercises the judgment corpus-wide before every run.
      match p₁.checkMachine with
      | .error e => err s!"{args.eirFile}: machine-mode well-formedness: {e}"
      | .ok () => do
      match etaSaturate 1000000000 p₁ with
      | .error e => err s!"{args.eirFile}: eta-saturation: {e}"
      | .ok p => do
      let Δ := DEnv.ofDatas p.datas
      let defns := mkDefnMap p.defns
      match p.procs with
      | [] => err s!"{args.eirFile}: no proc (a machine-level pass-8 dump is required)"
      | _ :: _ :: _ => err s!"{args.eirFile}: {p.procs.length} procs (expected exactly one)"
      | [pr] =>
        -- The Hyle side: the device's port names and widths.
        let rwcTxt ← IO.FS.readFile ⟨args.rwcFile⟩
        match Rwv.Hyle.parseProgram rwcTxt args.rwcFile with
        | .error e => err s!"{args.rwcFile}: parse error: {e}"
        | .ok hp => do
          let dev := hp.device
          -- The foreign tier: hook the Cryptol splices and extern
          -- models into Δ — from --foreign's program when given (the
          -- pre-optimization dump, where zero-argument splices have
          -- not yet been constant-folded away), else from the
          -- compiled program itself.
          let (frTxt, frProg) ←
            match args.foreignF with
            | none => pure (rwcTxt, hp)
            | some path => do
                let t ← IO.FS.readFile ⟨path⟩
                match Rwv.Hyle.parseProgram t path with
                | .error e =>
                    IO.eprintln s!"rwv-eidos-diff: {path}: parse error: {e}"
                    return 1
                | .ok fp => pure (t, fp)
          let Δ := addForeign Δ frTxt frProg
          -- The synthesized η for model-less externs.
          let etaTys ← if args.etaSynth then
              match scanProgram p with
              | .error e => return ← err e
              | .ok tys => pure tys
            else pure []
          let E := synthEta Δ repFuel etaTys
          -- Port-convention validation: detupleSizes at τ_I/τ_O must
          -- reproduce the device's declared port widths exactly.
          match Val.detupleSizes Δ repFuel pr.inTy, Val.detupleSizes Δ repFuel pr.outTy with
          | .error e, _ => err s!"detupleSizes at the input type: {e}"
          | _, .error e => err s!"detupleSizes at the output type: {e}"
          | .ok inSzs, .ok outSzs => do
            if inSzs ≠ dev.inputs.map (·.2) then
              return ← err s!"input port widths disagree: detupleSizes τ_I = {inSzs}, but device inputs are {dev.inputs}"
            if outSzs ≠ dev.outputs.map (·.2) then
              return ← err s!"output port widths disagree: detupleSizes τ_O = {outSzs}, but device outputs are {dev.outputs}"
            -- The stimulus: algebraic values, port-split for rwc.
            let seed := match args.seed with
              | some n => UInt32.ofNat n
              | none   => seed0 (baseName args.eirFile)
            match genStimulus Δ pr.inTy args.cycles seed with
            | .error e => err e
            | .ok inputs => do
              match inputs.mapM (Val.portSplit Δ repFuel pr.inTy) with
              | .error e => err s!"portSplit at the input type: {e}"
              | .ok inBVs => do
                if let some path := args.stimOut then
                  IO.FS.writeFile ⟨path⟩ (stimText (dev.inputs.map (·.1)) inBVs)
                -- The machine run and the trace.
                match pr.run Δ defns args.fuel 100000 inputs E with
                | .error e => err s!"machine run: {e}"
                | .ok tr => do
                  match tr.outs.mapM (Val.portSplit Δ repFuel pr.outTy) with
                  | .error e => err s!"portSplit at the output type: {e}"
                  | .ok outBVs => do
                    match Rwv.Diff.printTrace (dev.outputs.map (·.1)) outBVs with
                    | .ok out => IO.print out
                    | .error e => IO.eprintln s!"rwv-eidos-diff: {e}"; return 1
                    if tr.halted.isSome then
                      IO.eprintln s!"rwv-eidos-diff: note: halted after {tr.outs.length} observable cycle(s) (of {args.cycles} inputs); the trace is the halt prefix"
                    -- The internal correspondence check — the mechanized
                    -- Hyle device at the SAME synthesized η.
                    if args.etaSynth then
                      match hp.run inBVs E with
                      | .error e =>
                          return ← err s!"eta self-test: mechanized Hyle run failed: {e}"
                      | .ok hyTrace =>
                          if outBVs == hyTrace.take outBVs.length then
                            IO.eprintln s!"rwv-eidos-diff: eta self-test OK ({outBVs.length} cycle(s), externs: {String.intercalate "," (etaTys.map (·.1))})"
                          else
                            return ← err "eta self-test: the Eidos-M trace and the mechanized Hyle trace DISAGREE at the synthesized η"
                    return 0

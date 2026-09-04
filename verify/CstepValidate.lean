/-
rwv-cstep-validate: the headline driver for the machine-step validator
(Rwv.Eidos.Cstep), with an optional per-label measurement mode.

    rwv-cstep-validate <file.syn> <file.rwc> [--fuel=N] [--measure]
        [--protocol=2] [--nonce=STR] [-v]

parses the machine-level pass-8 dump (`.syn`; a legacy `.eir` machine
dump with a `top` line is accepted too; must contain exactly one proc)
and the
final (post-pass-11) .rwc, and applies, in order:

  * an ERROR gate: unreadable/undecodable files, parse failures, a
    redeclaration of a primitive-basis datatype that is not
    structurally identical to the canonical declaration (a conflicting
    redeclaration is never silently replaced), or a term unique inside
    the fresh range eta-saturation mints from;
  * a foreign gate, classified from the SOURCE artifact alone
    (Eval.externModelless on the Eidos implementation argument — the
    same classifier the evaluator and the verified compiler dispatch
    on; the model-less idiom is the rwPrimError
    "Extern expression placeholder" application). Model-carrying
    rwPrimExtern occurrences are IN scope: their source-side meaning
    is their own implementation argument, evaluated and compiled as an
    ordinary expression, so the target's model is checked against an
    independently obtained form — never against itself. rwPrimCryptol
    occurrences are UNSUPPORTED (their semantics still exists only as
    compiler output), as are clocked externs and multi-proc programs;
    a source/target disagreement about whether an extern carries a
    model is REJECTED;
  * a REJECTED gate: the Eidos machine well-formedness judgment
    (Rwv.Eidos.Check.checkMachine, on the canonical-basis pre-eta
    program), the Hyle well-formedness judgment
    (Rwv.Hyle.Program.check), and the target definition environment
    (Rwv.Hyle.Sem.mkFEnv) — a target that cannot denote or execute
    must not validate vacuously;
  * eta saturation to signature arity (Rwv.Eidos.etaSaturate — the
    same normalization rwc's own pipeline applies before the fold; the
    validated artifact is the saturated proc);
  * the library validator validateProcE (DAG dispatcher with tree-tier
    fallback — checkLabelD), whose verdict is the headline.

The verdict is one of four classes, printed as a `summary:` line and
reflected in the exit code:

  VALIDATED    every checked obligation holds          exit 0
  REJECTED     a well-formedness or proof obligation
               failed on supported inputs              exit 1
  ERROR        malformed input, bad options, I/O
               failure                                 exit 2
  UNSUPPORTED  the input uses a feature outside the
               certified fragment                      exit 3

With --protocol=2 the driver emits exactly one JSON object as the only
stdout line — {"tool","protocol","status","detail","nonce","source":
{"path","sha256"},"target":{"path","sha256"}} — with the SHA-256 of
the exact bytes read; human output goes to stderr. --nonce=STR is
echoed verbatim so a caller can bind the response to its invocation.

With --measure (incompatible with --protocol) it additionally
recomputes the step-record layout and the register/port plan,
symbolically evaluates the device step once (Bridge.symStep), and per
pause target compiles the Eidos machine step through the goto closure
(Cstep.goCmds) and compares it — output port for output port, register
for register — against the device step specialized to the target's
tag. This tree-tier loop materializes full per-slice NF trees and can
exhaust memory on the giant tests, which is why it is off by default;
the final `summary:` line (the last one wins) then carries the tally.
The measurement exit code is nonzero on any MISMATCH, on an INIT
failure, and on a non-VALIDATED headline; GAP lines (fragment misses
of the measurement loop's tree tier) are advisory.

Measurement verdicts per label (worst leg over all compared slices):
  OK-V      every slice equal after NF.cfold (the unconditional leg)
  OK-W      every slice equal after cfoldW3 (width-aware leg)
  OK-DAG    every slice equal after BridgeDag renormalization
  MISMATCH  some slice disagrees on all legs
  GAP:...   the Eidos-side step compiler rejected the block
            (fragment gap, message quoted)

Plus an INIT line (the initial-state check: entry run + encode vs
declared register initials).

This driver is UNTRUSTED measurement plumbing; the verified statements
live in Rwv.Eidos.Cstep.
-/
import Rwv.Sha256
import Rwv.Eidos.Parse
import Rwv.Eidos.PrimBasis
import Rwv.Eidos.EtaSat
import Rwv.Eidos.Check
import Rwv.Eidos.Cstep
import Rwv.Bundle
import Rwv.Hyle.Parse
import Rwv.Hyle.Check
import Rwv.Hyle.Bridge
import Rwv.Hyle.BridgeDag

open Rwv.Eidos
open Rwv.Eidos.Cstep
open Rwv.Eidos.Cexp (sliceNF catNF)
open Rwv.Hyle (BV)
open Rwv.Hyle.Bridge (NF)
open Std (HashMap)

/-- Convert a bridge normal form into the hash-consing DAG through the
normalizing constructors (CexpValidate's convertor). -/
partial def nfToDag (d : Rwv.Hyle.BridgeDag.Dag) : NF →
    Rwv.Hyle.BridgeDag.Dag × Nat
  | .var w x => Rwv.Hyle.BridgeDag.Dag.mkVar d w x
  | .lit v => Rwv.Hyle.BridgeDag.Dag.mkLit d v
  | .prim1 op a =>
      let (d, ra) := nfToDag d a
      Rwv.Hyle.BridgeDag.Dag.mk1D d op ra
  | .prim2 op a b =>
      let (d, ra) := nfToDag d a
      let (d, rb) := nfToDag d b
      Rwv.Hyle.BridgeDag.Dag.mk2D d op ra rb
  | .cat a b =>
      let (d, ra) := nfToDag d a
      let (d, rb) := nfToDag d b
      Rwv.Hyle.BridgeDag.Dag.mkCatD d ra rb
  | .slice i w e =>
      let (d, re) := nfToDag d e
      Rwv.Hyle.BridgeDag.Dag.mkSliceD d i w re
  | .ite c t e =>
      let (d, rc) := nfToDag d c
      let (d, rt) := nfToDag d t
      let (d, re) := nfToDag d e
      Rwv.Hyle.BridgeDag.Dag.mkIteD d rc rt re
  | .xcall w x gs a =>
      let (d, ra) := nfToDag d a
      Rwv.Hyle.BridgeDag.Dag.mkXcallD d w x gs ra

/-- DAG-engine comparison of two normal forms over shared variables. -/
def dagEq (n₁ n₂ : NF) : Bool :=
  let (d, r₁) := nfToDag Rwv.Hyle.BridgeDag.Dag.empty n₁
  let (d, r₂) := nfToDag d n₂
  if r₁ == r₂ then true
  else
    match Rwv.Hyle.BridgeDag.renorm d with
    | .error _ => false
    | .ok (e₁, m₁) =>
        match Rwv.Hyle.BridgeDag.renorm e₁ with
        | .error _ => false
        | .ok (_, m₂) =>
            Rwv.Hyle.BridgeDag.mIdx m₂ (Rwv.Hyle.BridgeDag.mIdx m₁ r₁)
              == Rwv.Hyle.BridgeDag.mIdx m₂ (Rwv.Hyle.BridgeDag.mIdx m₁ r₂)

/-- Per-slice verdict, best leg first. 0 = cfold, 1 = cfoldW3,
2 = DAG, 3 = mismatch. -/
def sliceVerdict (a b : NF) : Nat :=
  if a.cfold == b.cfold then 0
  else if Rwv.Hyle.Bridge.cfoldW3 a == Rwv.Hyle.Bridge.cfoldW3 b then 1
  else if dagEq a b then 2
  else 3

def verdictName : Nat → String
  | 0 => "OK-V"
  | 1 => "OK-W"
  | 2 => "OK-DAG"
  | _ => "MISMATCH"

structure Tally where
  okV : Nat := 0
  okW : Nat := 0
  okDag : Nat := 0
  mismatch : Nat := 0
  gap : Nat := 0

/-! ## Verdicts and the response protocol -/

inductive Verdict where
  | validated
  | rejected (reason : String)
  | unsupported (reason : String)
  | error (reason : String)

def Verdict.status : Verdict → String
  | .validated => "validated"
  | .rejected _ => "rejected"
  | .unsupported _ => "unsupported"
  | .error _ => "error"

def Verdict.detail : Verdict → String
  | .validated => ""
  | .rejected r | .unsupported r | .error r => r

def Verdict.summaryLine : Verdict → String
  | .validated => "summary: VALIDATED"
  | .rejected r => s!"summary: REJECTED ({r})"
  | .unsupported r => s!"summary: UNSUPPORTED ({r})"
  | .error r => s!"summary: ERROR ({r})"

def Verdict.exitCode : Verdict → UInt32
  | .validated => 0
  | .rejected _ => 1
  | .error _ => 2
  | .unsupported _ => 3

def jsonEscape (s : String) : String := Id.run do
  let mut out := ""
  for c in s.toList do
    if c == '"' then out := out ++ "\\\""
    else if c == '\\' then out := out ++ "\\\\"
    else if c == '\n' then out := out ++ "\\n"
    else if c == '\r' then out := out ++ "\\r"
    else if c == '\t' then out := out ++ "\\t"
    else if c.toNat < 0x20 then
      let hx := String.ofList (Nat.toDigits 16 c.toNat)
      out := out ++ "\\u" ++ "".pushn '0' (4 - hx.length) ++ hx
    else out := out.push c
  return out

structure Cfg where
  fuel     : Nat := 1000000
  verbose  : Bool := false
  measure  : Bool := false
  protocol : Bool := false
  nonce    : String := ""

structure ArtifactId where
  path : String := ""
  sha256 : String := ""

/-- Print a human line: stdout normally, stderr in protocol mode (the
protocol reserves stdout for the single JSON response). -/
def say (cfg : Cfg) (s : String) : IO Unit :=
  if cfg.protocol then IO.eprintln s else IO.println s

/-- Emit the verdict — the human summary line, plus (in protocol mode)
the single JSON response on stdout — and return the exit code. -/
def emit (cfg : Cfg) (src tgt : ArtifactId) (v : Verdict) : IO UInt32 := do
  say cfg v.summaryLine
  if cfg.protocol then
    IO.println <| "{\"tool\":\"rwv-cstep-validate\",\"protocol\":2"
      ++ s!",\"status\":\"{v.status}\""
      ++ s!",\"detail\":\"{jsonEscape v.detail}\""
      ++ s!",\"nonce\":\"{jsonEscape cfg.nonce}\""
      ++ s!",\"source\":\{\"path\":\"{jsonEscape src.path}\",\"sha256\":\"{src.sha256}\"}"
      ++ s!",\"target\":\{\"path\":\"{jsonEscape tgt.path}\",\"sha256\":\"{tgt.sha256}\"}}"
  return v.exitCode

def usage : String := String.intercalate "\n"
  [ "usage: rwv-cstep-validate <file.syn> <file.rwc> [options]"
  , ""
  , "options:"
  , "  --fuel=N       evaluation fuel (default 1000000)"
  , "  --measure      per-label tree-tier measurement loop (memory-hungry)"
  , "  --protocol=2   machine-readable mode: exactly one JSON response on"
  , "                 stdout; human output on stderr"
  , "  --nonce=STR    echoed in the JSON response"
  , "  -v             verbose"
  , ""
  , "exit codes: 0 VALIDATED, 1 REJECTED, 2 ERROR, 3 UNSUPPORTED" ]

def parseArgs (argv : List String) : Except String (Cfg × String × String) := do
  let mut cfg : Cfg := {}
  let mut pos : List String := []
  for a in argv do
    if a = "-v" then cfg := { cfg with verbose := true }
    else if a = "--measure" then cfg := { cfg with measure := true }
    else if a.startsWith "--fuel=" then
      match (a.drop "--fuel=".length).toNat? with
      | some n =>
          if n = 0 then throw "--fuel: expected a positive integer"
          cfg := { cfg with fuel := n }
      | none => throw s!"--fuel: expected a positive integer, got '{a.drop 7}'"
    else if a.startsWith "--protocol=" then
      match (a.drop "--protocol=".length).toString with
      | "2" => cfg := { cfg with protocol := true }
      | v => throw s!"--protocol: unsupported version '{v}' (this validator speaks 2)"
    else if a.startsWith "--nonce=" then
      cfg := { cfg with nonce := (a.drop "--nonce=".length).toString }
    else if a.startsWith "-" && a ≠ "-" then
      throw s!"unknown option: {a}"
    else pos := pos ++ [a]
  if cfg.measure && cfg.protocol then
    throw "--measure and --protocol are incompatible"
  match pos with
  | [eirFile, rwcFile] => return (cfg, eirFile, rwcFile)
  | _ => throw s!"expected exactly two file arguments, got {pos.length}"

def readArtifact (path : String) : IO (Except String (String × ArtifactId)) := do
  try
    let bytes ← IO.FS.readBinFile ⟨path⟩
    match String.fromUTF8? bytes with
    | some txt => return .ok (txt, { path, sha256 := Rwv.Sha256.hex bytes })
    | none => return .error s!"{path}: not valid UTF-8"
  catch ex =>
    return .error s!"{path}: {ex}"

def main (argv : List String) : IO UInt32 := do
  match parseArgs argv with
  | .error e =>
      IO.eprintln s!"rwv-cstep-validate: {e}"
      IO.eprintln usage
      -- A caller that asked for the protocol still gets a response.
      if argv.contains "--protocol=2" then
        let _ ← emit { protocol := true } {} {} (.error e)
      return 2
  | .ok (cfg, eirFile, rwcFile) => do
    let mut srcId : ArtifactId := { path := eirFile }
    let mut tgtId : ArtifactId := { path := rwcFile }
    let eirTxt ← match ← readArtifact eirFile with
      | .error e => return ← emit cfg srcId tgtId (.error e)
      | .ok (txt, id) => do
          srcId := id
          pure txt
    let rwcTxt ← match ← readArtifact rwcFile with
      | .error e => return ← emit cfg srcId tgtId (.error e)
      | .ok (txt, id) => do
          tgtId := id
          pure txt
    -- The verdict is the pure bundle validator's (Rwv.Bundle): every
    -- gate between the artifact texts and the library validator lives
    -- there, so this executable's success is literally
    -- `validateBundle_sound`'s hypothesis.
    let verdict := match validateBundle eirFile eirTxt rwcFile rwcTxt cfg.fuel with
      | .validated => Verdict.validated
      | .rejected r => Verdict.rejected r
      | .unsupported r => Verdict.unsupported r
      | .error r => Verdict.error r
    unless cfg.measure do
      return ← emit cfg srcId tgtId verdict
    -- Measurement mode (incompatible with --protocol): the headline
    -- verdict above is authoritative and printed FIRST — the loop
    -- below can OOM on the giants — then the per-label tree-tier
    -- diagnostics re-derive the intermediates the bundle computed
    -- internally.
    IO.println verdict.summaryLine
    let vOk := match verdict with
      | .validated => true
      | _ => false
    match parseEir eirTxt eirFile, Rwv.Hyle.parseProgram rwcTxt rwcFile with
    | .error _, _ | _, .error _ =>
        -- The bundle already reported the parse failure.
        return verdict.exitCode
    | .ok p₀, .ok hp => do
      match etaSaturate Bundle.structuralFuel (addPrims p₀) with
      | .error e => IO.println s!"SKIP      (eta-saturation: {e})"; return 1
      | .ok p => do
      match p.procs with
      | [pr] => do
        let Δ := DEnv.ofDatas p.datas
        let edm := mkDefnMap p.defns
        -- Layout and plan.
        let fuel := cfg.fuel
        let lo ← match mkLayoutL Δ fuel pr with
          | .ok lo => pure lo
          | .error e => IO.println s!"SKIP      (layout: {e})"; return 1
        let plan ← match mkPlan Δ fuel pr lo hp.device with
          | .ok plan => pure plan
          | .error e => IO.println s!"SKIP      (plan: {e})"; return 1
        if cfg.verbose then
          IO.println s!"layout: recW={lo.recW} pTagW={lo.pTagW} outW={lo.outW} \
            rTagW={lo.rTagW} rPayW={lo.rPayW} cellsW={lo.cellsW} \
            targets={lo.targets.map (fun t => (t.uniq, t.tag, t.argWs))} \
            halts={lo.halts.length}"
        -- The device step, symbolically, once.
        let hyleFuel := Rwv.Hyle.Bridge.progFuel hp
        let ss ← match Rwv.Hyle.Bridge.symStep (Rwv.Hyle.Bridge.dmapOf hp)
            (Rwv.Hyle.Sem.xenv hp) hyleFuel hp.device with
          | .ok ss => pure ss
          | .error e => IO.println s!"SKIP      (symStep: {e})"; return 1
        let blocks : HashMap Int Block :=
          HashMap.ofList (pr.blocks.map fun (l, b) => (l.uniq, b))
        let C : Ctx := { Δ, edm, lo, blocks, cexpFuel := fuel, outTy := pr.outTy }
        -- Per-label checks.
        let mut t : Tally := {}
        let mut labelNames : HashMap Int String :=
          HashMap.ofList (pr.blocks.map fun (l, _) => (l.uniq, l.occ))
        for tgt in lo.targets do
          let nm := (labelNames.get? tgt.uniq).getD (toString tgt.uniq)
          match blocks.get? tgt.uniq with
          | none =>
              IO.println s!"GAP       tag {tgt.tag} ({nm})  (no block)"
              t := { t with gap := t.gap + 1 }
          | some blk => do
            let tagVar : NF := match plan.tagReg with
              | some (r, w) => .var w r
              | none => .lit BV.nil
            let argNFs := (tgt.argWs.zip (offsetsOf tgt.argWs)).map fun (w, off) =>
              sliceNF off w tagVar
            let inNF := Rwv.Eidos.Cexp.catNF
              (plan.inPorts.map fun (x, w) => ((.var w x : NF), w))
            let Γ₀ := (blk.params.zip ((argNFs ++ [inNF]).zip
                (tgt.argTys ++ [pr.inTy]))).foldl
              (fun m (x, nt) => m.insert x.uniq nt) (∅ : HashMap Int (NF × Ty))
            match goCmds C fuel Γ₀ (cells0 plan) blk.cmds blk.term with
            | .error msg =>
                IO.println s!"GAP       tag {tgt.tag} ({nm})  ({msg})"
                t := { t with gap := t.gap + 1 }
            | .ok rec => do
                let θ := tagSubst plan C.lo tgt.tag
                let outWs := hp.device.outputs.map (·.2)
                let outOffs := (offsetsOf outWs).map (· + C.lo.rW + C.lo.cellsW)
                let regWs := hp.device.registers.map (·.width)
                let regOffs := offsetsOf regWs
                let mut worst := 0
                let mut worstAt := ""
                for (((o, w), off), (_, nf)) in
                    (hp.device.outputs.zip outOffs).zip ss.outs do
                  let v := sliceVerdict (sliceNF off w rec) (substNF θ nf)
                  if v > worst then worst := v; worstAt := s!"output {o}"
                for ((r, off), (_, nf)) in
                    (hp.device.registers.zip regOffs).zip ss.nexts do
                  let v := sliceVerdict (sliceNF off r.width rec) (substNF θ nf)
                  if v > worst then worst := v; worstAt := s!"register {r.name}"
                let vn := verdictName worst
                if worst > 0 || cfg.verbose then
                  IO.println s!"{vn.pushn ' ' (10 - vn.length)}tag {tgt.tag} ({nm}){if worst ≥ 2 then s!"  (at {worstAt})" else ""}"
                match worst with
                | 0 => t := { t with okV := t.okV + 1 }
                | 1 => t := { t with okW := t.okW + 1 }
                | 2 => t := { t with okDag := t.okDag + 1 }
                | _ => t := { t with mismatch := t.mismatch + 1 }
        -- The initial-state check.
        let initRes := checkInit C plan hp.device pr fuel fuel fuel
        let initOk := initRes.isOk
        let initV := match initRes with
          | .ok _ => "OK"
          | .error e => s!"FAIL ({e})"
        IO.println s!"INIT      {initV}"
        -- Full summary (last summary line wins in the sweep harness).
        IO.println s!"{verdict.summaryLine}; {t.okV} ok-v, {t.okW} ok-w, \
          {t.okDag} ok-dag, {t.mismatch} mismatch, {t.gap} gap; init {initV}"
        return (if t.mismatch > 0 || !initOk || !vOk then 1 else 0)
      | [] => IO.println "SKIP      (no proc in the source dump)"; return 1
      | _ :: _ :: _ => IO.println "SKIP      (multiple procs)"; return 1

/-
rwv-cstep-validate: the headline driver for the machine-step validator
(Rwv.Eidos.Cstep), with an optional per-label measurement mode.

    rwv-cstep-validate <file.eir> <file.rwc> [--fuel=N] [--measure]
        [--protocol=2] [--nonce=STR] [-v]

parses the pass-8 Eidos dump (must contain exactly one proc) and the
final (post-pass-11) .rwc, and applies, in order:

  * an ERROR gate: unreadable/undecodable files, parse failures, a
    redeclaration of a primitive-basis datatype that is not
    structurally identical to the canonical declaration (a conflicting
    redeclaration is never silently replaced), or a term unique inside
    the fresh range eta-saturation mints from;
  * an UNSUPPORTED gate: foreign features whose source-side semantics
    cannot be constructed from the Eidos artifact alone. rwPrimCryptol
    occurrences and model-carrying rwPrimExtern occurrences are
    unsupported — their meaning currently exists only as compiler
    output, and validating against that would let the target program
    define the very semantics it is checked against. Whether an extern
    occurrence carries a model is decided from the Eidos implementation
    argument only (the model-less idiom is the rwPrimError
    "Extern expression placeholder" application); the target
    declaration is then cross-checked to also be model-less. Clocked
    externs and multi-proc programs are likewise unsupported;
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
  | .xcall w x a =>
      let (d, ra) := nfToDag d a
      Rwv.Hyle.BridgeDag.Dag.mkXcallD d w x ra

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

/-! ## The foreign-occurrence scan

Classification of every `rwPrimExtern`/`rwPrimCryptol` occurrence from
the source artifact alone. The model-less extern idiom is syntactic —
the implementation argument is the `rwPrimError` application to the
literal `"Extern expression placeholder"` — so whether an occurrence
is in the certified fragment never depends on the target program. -/

namespace ForeignScan

def placeholderText : String := "Extern expression placeholder"

partial def hasPlaceholder : Exp → Bool
  | .litStr s => s == placeholderText
  | .app f (.eArg a) => hasPlaceholder f || hasPlaceholder a
  | .app f (.tArg _) => hasPlaceholder f
  | .lam _ e => hasPlaceholder e
  | .letE (.nonRec _ r) e => hasPlaceholder r || hasPlaceholder e
  | .letE (.recB bs) e => bs.any (fun b => hasPlaceholder b.2) || hasPlaceholder e
  | .letE (.join _ _ j) e => hasPlaceholder j || hasPlaceholder e
  | .jump _ es => es.any hasPlaceholder
  | .cases _ sc _ alts =>
      hasPlaceholder sc
      || alts.any (fun alt => match alt with | .mk _ _ b => hasPlaceholder b)
  | .litList _ es => es.any hasPlaceholder
  | .litVec _ es => es.any hasPlaceholder
  | _ => false

/-- Scan an expression; `tgtModeled` reports whether the target
program declares a model for a given extern name (used only to
cross-check consistency with the source-side classification, never to
choose it). A thrown message is an UNSUPPORTED reason. -/
partial def scanExp (tgtModeled : String → Bool) (e : Exp) : Except String Unit := do
  let (h, args) := EtaSat.flattenApp' e
  for a in args do
    match a with
    | .eArg ae => scanExp tgtModeled ae
    | .tArg _ => pure ()
  match h with
  | .prim _ .cryptol =>
      throw "rwPrimCryptol: Cryptol foreign functions are outside the certified \
        profile (their semantics exists only as compiler output)"
  | .prim _ .«extern» =>
      let eargs := args.filterMap fun | .eArg a => some a | .tArg _ => none
      match eargs with
      | _ps :: clk :: _rst :: _as :: _rs :: nameE :: impl :: _rest =>
          let s := match nameE with | .litStr s => s | _ => "?"
          match clk with
          | .litStr "" =>
              if hasPlaceholder impl then
                if tgtModeled s then
                  throw s!"extern {s}: the source occurrence is model-less but \
                    the target declares a model"
                else pure ()
              else
                throw s!"extern {s}: model-carrying externs are outside the \
                  certified profile (their semantics exists only as compiler output)"
          | _ =>
              throw s!"extern {s}: sequential (clocked) externs are outside the \
                certified fragment"
      | _ => throw "rwPrimExtern: under-applied foreign occurrence"
  | .lam _ b => scanExp tgtModeled b
  | .letE bnd b => do
      match bnd with
      | .nonRec _ r => scanExp tgtModeled r
      | .recB bs => bs.forM fun p => scanExp tgtModeled p.2
      | .join _ _ j => scanExp tgtModeled j
      scanExp tgtModeled b
  | .cases _ sc _ alts => do
      scanExp tgtModeled sc
      alts.forM fun alt => match alt with | .mk _ _ b => scanExp tgtModeled b
  | .jump _ es => es.forM (scanExp tgtModeled)
  | .litList _ es => es.forM (scanExp tgtModeled)
  | .litVec _ es => es.forM (scanExp tgtModeled)
  | _ => pure ()

partial def scanTerm (tgtModeled : String → Bool) : Term → Except String Unit
  | .pause o _ as => do
      scanExp tgtModeled o
      as.forM (scanExp tgtModeled)
  | .goto _ as => as.forM (scanExp tgtModeled)
  | .halt e => scanExp tgtModeled e
  | .cases sc alts => do
      scanExp tgtModeled sc
      alts.forM fun alt => match alt with | .mk _ _ t => scanTerm tgtModeled t

def scanCmd (tgtModeled : String → Bool) : Cmd → Except String Unit
  | .bind _ e => scanExp tgtModeled e
  | .get _ _ => pure ()
  | .put _ e => scanExp tgtModeled e

def scanBlock (tgtModeled : String → Bool) (b : Block) : Except String Unit := do
  b.cmds.forM (scanCmd tgtModeled)
  scanTerm tgtModeled b.term

def scanProgram (tgtModeled : String → Bool) (p : Program) : Except String Unit := do
  p.defns.forM fun d => scanExp tgtModeled d.body
  p.procs.forM fun pr => do
    pr.cells.forM fun c =>
      match c.init with
      | some e => scanExp tgtModeled e
      | none => pure ()
    scanBlock tgtModeled pr.entry
    pr.blocks.forM fun lb => scanBlock tgtModeled lb.2

end ForeignScan

/-! ## The reserved fresh-unique range

Eta saturation mints binder uniques from -10⁹ down on the invariant
that the input never uses that range (the bridge mints non-negative
term uniques; the prim basis' type variables use small negatives). An
input inside the range is refused rather than risked. -/

namespace UniqScan

def floor : Int := -1000000000

partial def minIdExp : Exp → Int
  | .var x => x.uniq
  | .lam x e => min x.uniq (minIdExp e)
  | .app f (.eArg a) => min (minIdExp f) (minIdExp a)
  | .app f (.tArg _) => minIdExp f
  | .letE (.nonRec x r) e => min x.uniq (min (minIdExp r) (minIdExp e))
  | .letE (.recB bs) e =>
      bs.foldl (fun acc b => min acc (min b.1.uniq (minIdExp b.2))) (minIdExp e)
  | .letE (.join _ ps j) e =>
      ps.foldl (fun acc x => min acc x.uniq) (min (minIdExp j) (minIdExp e))
  | .jump _ es => es.foldl (fun acc e => min acc (minIdExp e)) 0
  | .cases _ sc x alts =>
      alts.foldl
        (fun acc alt => match alt with
          | .mk _ bs b => bs.foldl (fun a y => min a y.uniq) (min acc (minIdExp b)))
        (min x.uniq (minIdExp sc))
  | .litList _ es => es.foldl (fun acc e => min acc (minIdExp e)) 0
  | .litVec _ es => es.foldl (fun acc e => min acc (minIdExp e)) 0
  | _ => 0

partial def minIdTerm : Term → Int
  | .pause o _ as => as.foldl (fun acc e => min acc (minIdExp e)) (minIdExp o)
  | .goto _ as => as.foldl (fun acc e => min acc (minIdExp e)) 0
  | .halt e => minIdExp e
  | .cases sc alts =>
      alts.foldl
        (fun acc alt => match alt with
          | .mk _ bs t => bs.foldl (fun a y => min a y.uniq) (min acc (minIdTerm t)))
        (minIdExp sc)

def minIdCmd : Cmd → Int
  | .bind x e => min x.uniq (minIdExp e)
  | .get x _ => x.uniq
  | .put _ e => minIdExp e

def minIdBlock (b : Block) : Int :=
  b.cmds.foldl (fun acc c => min acc (minIdCmd c))
    (b.params.foldl (fun acc x => min acc x.uniq) (minIdTerm b.term))

def minIdProgram (p : Program) : Int :=
  let dmin := p.defns.foldl
    (fun acc d => d.params.foldl (fun a x => min a x.uniq)
      (min acc (min d.name.uniq (minIdExp d.body)))) 0
  p.procs.foldl
    (fun acc pr =>
      let cmin := pr.cells.foldl
        (fun a c => match c.init with | some e => min a (minIdExp e) | none => a) acc
      pr.blocks.foldl (fun a lb => min a (min lb.1.uniq (minIdBlock lb.2)))
        (min cmin (minIdBlock pr.entry)))
    dmin

end UniqScan

/-- A conflicting redeclaration of a primitive-basis datatype: the
name of the first user declaration that shadows a basis name without
being structurally identical to the canonical declaration. -/
def basisConflict (p : Program) : Option String := Id.run do
  let canon : HashMap String String :=
    HashMap.ofList (primDatas.map fun d => (d.name, reprStr d))
  for d in p.datas do
    match canon.get? d.name with
    | some r => if reprStr d != r then return some d.name
    | none => pure ()
  return none

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
  [ "usage: rwv-cstep-validate <file.eir> <file.rwc> [options]"
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
    match parseEir eirTxt eirFile, Rwv.Hyle.parseProgram rwcTxt rwcFile with
    | .error e, _ => emit cfg srcId tgtId (.error s!"{eirFile}: {e}")
    | _, .error e => emit cfg srcId tgtId (.error s!"{rwcFile}: {e}")
    | .ok p₀, .ok hp => do
      -- ERROR gates: a conflicting primitive-basis redeclaration is
      -- never silently replaced, and inputs inside the fresh-unique
      -- range eta-saturation mints from are refused, not risked.
      if let some n := basisConflict p₀ then
        return ← emit cfg srcId tgtId (.error
          s!"conflicting redeclaration of primitive datatype {n} \
            (must be structurally identical to the canonical declaration)")
      let p₁ := addPrims p₀
      if UniqScan.minIdProgram p₁ ≤ UniqScan.floor then
        return ← emit cfg srcId tgtId (.error
          s!"input uses a term unique at or below {UniqScan.floor} \
            (reserved for freshly minted eta binders)")
      -- The UNSUPPORTED gate: foreign occurrences classified from the
      -- source artifact alone.
      let tgtModeled : String → Bool := fun s =>
        ((Rwv.Hyle.Sem.xenv hp).get? s).isSome
      if let .error r := ForeignScan.scanProgram tgtModeled p₁ then
        return ← emit cfg srcId tgtId (.unsupported r)
      -- REJECTED gates: source machine well-formedness (pre-eta, on
      -- the canonical basis), target well-formedness, and a denoting
      -- target definition environment.
      if let .error e := p₁.checkMachine then
        return ← emit cfg srcId tgtId (.rejected s!"source well-formedness: {e}")
      if let .error e := hp.check then
        return ← emit cfg srcId tgtId (.rejected s!"target well-formedness: {e}")
      if let .error e := Rwv.Hyle.Sem.mkFEnv hp then
        return ← emit cfg srcId tgtId (.rejected
          s!"target definition environment does not denote: {e}")
      match etaSaturate 1000000000 p₁ with
      | .error e => emit cfg srcId tgtId (.error s!"{eirFile}: eta-saturation: {e}")
      | .ok p => do
      match p.procs with
      | [] => emit cfg srcId tgtId (.error "no proc in the Eidos dump \
          (a machine-level pass-8 dump is required)")
      | _ :: _ :: _ => emit cfg srcId tgtId (.unsupported "multiple procs")
      | [pr] => do
        let Δ := DEnv.ofDatas p.datas
        let edm := mkDefnMap p.defns
        unless Rwv.Eidos.Cexp.denvOk Δ do
          return ← emit cfg srcId tgtId (.rejected "denvOk failed (prim basis discipline)")
        -- Headline FIRST (the library validator: DAG dispatcher with
        -- tree-tier fallback). The measurement loop below can OOM on
        -- the giants, so the verdict must be out before it runs.
        let vres := validateProcE Δ edm pr hp cfg.fuel
        let verdict := match vres with
          | .ok _ => Verdict.validated
          | .error e => Verdict.rejected e
        unless cfg.measure do
          return ← emit cfg srcId tgtId verdict
        IO.println verdict.summaryLine
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
        return (if t.mismatch > 0 || !initOk || !vres.isOk then 1 else 0)

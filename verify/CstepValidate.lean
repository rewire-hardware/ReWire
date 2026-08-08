/-
rwv-cstep-validate: the headline driver for the Phase 4b-ii validator
(Rwv.Eidos.Cstep), with an optional per-label measurement mode.

    rwv-cstep-validate <file.eir> <file.rwc> [--fuel=N] [--measure]
        [--foreign=FILE.rwc] [-v]

parses the pass-8 Eidos dump (must contain exactly one proc) and the
final (post-pass-11) .rwc, eta-saturates the Eidos program to
signature arity (Rwv.Eidos.etaSaturate — the same normalization rwc's
own pipeline applies before the fold; the validated artifact is the
saturated proc, matching what the differential harness runs), installs
the foreign tier (Rwv.Eidos.addForeign — Cryptol splices and extern
models read from --foreign's program when given, which should be the
pre-optimization `-d 9` dump so constant-folded splices are still
present, else from the .rwc itself), and prints the headline
VALIDATED/REJECTED from the library's validateProcE FIRST, as a
`summary:` line (the library validator dispatches per label through
the DAG leg first, falling back to the tree-tier cfold/cfoldW3 legs —
checkLabelD).

With --measure it then additionally recomputes the step-record layout
and the register/port plan, symbolically evaluates the device step
once (Bridge.symStep), and per pause target compiles the Eidos machine
step through the goto closure (Cstep.goCmds) and compares it — output
port for output port, register for register — against the device step
specialized to the target's tag. This tree-tier loop materializes full
per-slice NF trees and can exhaust memory on the giant tests, which is
why it is off by default; the final `summary:` line (the last one
wins) then carries the tally.

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
import Rwv.Eidos.Parse
import Rwv.Eidos.PrimBasis
import Rwv.Eidos.EtaSat
import Rwv.Eidos.ForeignEnv
import Rwv.Eidos.Cstep
import Rwv.Hyle.Parse
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

def main (argv : List String) : IO UInt32 := do
  let mut fuel : Nat := 1000000
  let mut verbose := false
  let mut measure := false
  let mut foreignF : Option String := none
  let mut pos : List String := []
  for a in argv do
    if a = "-v" then verbose := true
    else if a = "--measure" then measure := true
    else if a.startsWith "--fuel=" then
      fuel := ((a.drop 7).toNat?).getD fuel
    else if a.startsWith "--foreign=" then
      foreignF := some ((a.drop 10).toString)
    else pos := pos ++ [a]
  match pos with
  | [eirFile, rwcFile] => do
    let eirTxt ← IO.FS.readFile ⟨eirFile⟩
    let rwcTxt ← IO.FS.readFile ⟨rwcFile⟩
    match parseEir eirTxt eirFile, Rwv.Hyle.parseProgram rwcTxt rwcFile with
    | .error e, _ => IO.eprintln s!"cstep-validate: {eirFile}: {e}"; return 1
    | _, .error e => IO.eprintln s!"cstep-validate: {rwcFile}: {e}"; return 1
    | .ok p₀, .ok hp => do
      match etaSaturate 1000000000 (addPrims p₀) with
      | .error e => IO.eprintln s!"cstep-validate: {eirFile}: eta-saturation: {e}"; return 1
      | .ok p => do
      match p.procs with
      | [pr] => do
        -- The foreign tier: Cryptol splices and extern models, from
        -- --foreign's program (the pre-optimization dump) when given.
        let (frTxt, frProg) ←
          match foreignF with
          | none => pure (rwcTxt, hp)
          | some path => do
              let t ← IO.FS.readFile ⟨path⟩
              match Rwv.Hyle.parseProgram t path with
              | .error e =>
                  IO.eprintln s!"cstep-validate: {path}: parse error: {e}"
                  return 1
              | .ok fp => pure (t, fp)
        let Δ := addForeign (DEnv.ofDatas p.datas) frTxt frProg
        let edm := mkDefnMap p.defns
        unless Rwv.Eidos.Cexp.denvOk Δ do
          IO.eprintln "cstep-validate: denvOk failed (prim basis discipline)"
          return 1
        -- Headline FIRST (the library validator: DAG dispatcher with
        -- tree-tier fallback). The measurement loop below can OOM on
        -- the giants, so the verdict must be out before it runs.
        let vres := validateProcE Δ edm pr hp fuel
        let headline := match vres with
          | .ok _ => "VALIDATED"
          | .error e => s!"REJECTED ({e})"
        IO.println s!"summary: {headline}"
        unless measure do
          return (if vres.isOk then 0 else 1)
        -- Layout and plan.
        let lo ← match mkLayoutL Δ fuel pr with
          | .ok lo => pure lo
          | .error e => IO.println s!"SKIP      (layout: {e})"; return 1
        let plan ← match mkPlan Δ fuel pr lo hp.device with
          | .ok plan => pure plan
          | .error e => IO.println s!"SKIP      (plan: {e})"; return 1
        if verbose then
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
                if worst > 0 || verbose then
                  IO.println s!"{vn.pushn ' ' (10 - vn.length)}tag {tgt.tag} ({nm}){if worst ≥ 2 then s!"  (at {worstAt})" else ""}"
                match worst with
                | 0 => t := { t with okV := t.okV + 1 }
                | 1 => t := { t with okW := t.okW + 1 }
                | 2 => t := { t with okDag := t.okDag + 1 }
                | _ => t := { t with mismatch := t.mismatch + 1 }
        -- The initial-state check.
        let initV := match checkInit C plan hp.device pr fuel fuel fuel with
          | .ok _ => "OK"
          | .error e => s!"FAIL ({e})"
        IO.println s!"INIT      {initV}"
        -- Full summary (last summary line wins in the sweep harness).
        IO.println s!"summary: {headline}; {t.okV} ok-v, {t.okW} ok-w, \
          {t.okDag} ok-dag, {t.mismatch} mismatch, {t.gap} gap; init {initV}"
        return (if t.mismatch > 0 || !vres.isOk then 1 else 0)
      | [] => IO.println "SKIP      (no proc in the Eidos dump)"; return 1
      | _ => IO.println "SKIP      (multiple procs)"; return 1
  | _ =>
      IO.eprintln "usage: rwv-cstep-validate <file.eir> <file.rwc> [--fuel=N] [-v]"
      return 2

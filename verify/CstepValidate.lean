/-
rwv-cstep-validate: the per-label machine-step measurement driver for
the Phase 4b-ii validator (Rwv.Eidos.Cstep).

    rwv-cstep-validate <file.eir> <file.rwc> [--fuel=N] [-v]

parses the pass-8 Eidos dump (must contain exactly one proc) and the
final (post-pass-11) .rwc, recomputes the step-record layout and the
register/port plan, symbolically evaluates the device step once
(Bridge.symStep), and then per pause target compiles the Eidos machine
step through the goto closure (Cstep.goCmds) and compares it — output
port for output port, register for register — against the device step
specialized to the target's tag.

Verdicts per label (worst leg over all compared slices):
  OK-V      every slice equal after NF.cfold (the unconditional leg)
  OK-W      every slice equal after cfoldW3 (width-aware leg)
  OK-DAG    every slice equal after BridgeDag renormalization
            (untrusted engine leg, measurement only)
  MISMATCH  some slice disagrees on all legs
  GAP:...   the Eidos-side step compiler rejected the block
            (fragment gap, message quoted)

Plus an INIT line (the initial-state check: entry run + encode vs
declared register initials) and a headline VALIDATED/REJECTED from the
library's validateProcE (whose comparison legs are cfold/cfoldW3
only — a test that is OK only through the DAG leg is NOT validated).

This driver is UNTRUSTED measurement plumbing; the verified statements
live in Rwv.Eidos.Cstep.
-/
import Rwv.Eidos.Parse
import Rwv.Eidos.PrimBasis
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
  let mut pos : List String := []
  for a in argv do
    if a = "-v" then verbose := true
    else if a.startsWith "--fuel=" then
      fuel := ((a.drop 7).toNat?).getD fuel
    else pos := pos ++ [a]
  match pos with
  | [eirFile, rwcFile] => do
    let eirTxt ← IO.FS.readFile ⟨eirFile⟩
    let rwcTxt ← IO.FS.readFile ⟨rwcFile⟩
    match parseEir eirTxt eirFile, Rwv.Hyle.parseProgram rwcTxt rwcFile with
    | .error e, _ => IO.eprintln s!"cstep-validate: {eirFile}: {e}"; return 1
    | _, .error e => IO.eprintln s!"cstep-validate: {rwcFile}: {e}"; return 1
    | .ok p₀, .ok hp => do
      let p := addPrims p₀
      match p.procs with
      | [pr] => do
        let Δ := DEnv.ofDatas p.datas
        let edm := mkDefnMap p.defns
        unless Rwv.Eidos.Cexp.denvOk Δ do
          IO.eprintln "cstep-validate: denvOk failed (prim basis discipline)"
          return 1
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
        let ss ← match Rwv.Hyle.Bridge.symStep (Rwv.Hyle.Bridge.dmapOf hp) hyleFuel
            hp.device with
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
        -- Headline (the library validator: cfold/cfoldW3 legs only).
        let headline := match validateProcE Δ edm pr hp fuel with
          | .ok _ => "VALIDATED"
          | .error e => s!"REJECTED ({e})"
        IO.println s!"summary: {headline}; {t.okV} ok-v, {t.okW} ok-w, \
          {t.okDag} ok-dag, {t.mismatch} mismatch, {t.gap} gap; init {initV}"
        return (if t.mismatch > 0 then 1 else 0)
      | [] => IO.println "SKIP      (no proc in the Eidos dump)"; return 1
      | _ => IO.println "SKIP      (multiple procs)"; return 1
  | _ =>
      IO.eprintln "usage: rwv-cstep-validate <file.eir> <file.rwc> [--fuel=N] [-v]"
      return 2

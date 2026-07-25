/-
rwv-cexp-validate: the per-definition measurement driver for the
verified Eidos-side expression compiler (Rwv.Eidos.Cexp, Phase 4a).

    rwv-cexp-validate <file.eir> <file.rwc> [--fuel N] [-v]

parses the pass-8 Eidos dump and the compiled .rwc, matches Eidos pure
definitions to Hyle definitions by the fold's naming convention
(`$LL.` markers stripped — ToHyle's defnBase — with pickFresh-style
numeric disambiguation left to exact-name matching), and for each
matched pair checks

    cexp (Eidos body over the Hyle parameter names)
      ≡  Bridge.symExp (Hyle body)

first by syntactic equality of `NF.cfold` normal forms — exactly the
leg the VERIFIED `checkDefnPair` certifies (`checkDefnPair_sound`,
unconditional constant folding) — then by `cfoldW3` (the bridge's
width-aware normalizer, soundness pending a `VarsWF` invariant for
cexp output), then by the BridgeDag hash-consing engine (build both
sides into one DAG with the normalizing constructors, renormalize,
compare roots).

Verdicts per Eidos definition:
  OK-V      cfold-syntactic equality (covered by checkDefnPair_sound)
  OK-W      cfoldW3-syntactic equality (width-aware leg)
  OK-DAG    equal after DAG normalization (engine leg only)
  MISMATCH  all legs disagree — a genuine or normalization miss
  GAP:...   cexp rejected the body (fragment gap, message quoted)
  SKIP:...  no matched Hyle defn / carrier defn / arity drift

This driver is UNTRUSTED measurement plumbing; the verified statement
is Rwv.Eidos.Cexp.checkDefnPair_sound.
-/
import Rwv.Eidos.Parse
import Rwv.Eidos.PrimBasis
import Rwv.Eidos.Cexp
import Rwv.Hyle.Parse
import Rwv.Hyle.Bridge
import Rwv.Hyle.BridgeDag

open Rwv.Eidos
open Rwv.Eidos.Cexp
open Rwv.Hyle (BV)
open Rwv.Hyle.Bridge (NF)
open Std (HashMap)

/-- ToHyle's `defnBase`: strip the `$LL.` lifted-definition marker. -/
def defnBase (occ : String) : String :=
  if occ.startsWith "$LL." then ((occ.drop 4).toString) else occ

/-- Is this Eidos definition a carrier the fold's `emit` filters out?
(Approximation for matching: builtin-named signature carriers and
polymorphic definitions; reactive-typed defns simply won't have Hyle
counterparts.) -/
def isCarrier (d : Defn) : Bool :=
  d.name.occ.startsWith "rwPrim" || !d.name.sig.tvs.isEmpty

/-- Convert a bridge normal form into the hash-consing DAG through the
normalizing constructors. -/
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

structure Tally where
  okV : Nat := 0
  ok : Nat := 0
  okDag : Nat := 0
  mismatch : Nat := 0
  gap : Nat := 0
  skip : Nat := 0

def main (argv : List String) : IO UInt32 := do
  let mut fuel : Nat := 2000000
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
    | .error e, _ => IO.eprintln s!"cexp-validate: {eirFile}: {e}"; return 1
    | _, .error e => IO.eprintln s!"cexp-validate: {rwcFile}: {e}"; return 1
    | .ok p₀, .ok hp => do
      let p := addPrims p₀
      let Δ := DEnv.ofDatas p.datas
      let edm := mkDefnMap p.defns
      let hdm : HashMap String Rwv.Hyle.Defn :=
        HashMap.ofList (hp.defns.map fun d => (d.name, d))
      unless denvOk Δ do
        IO.eprintln "cexp-validate: denvOk failed (prim basis Bool/Vec discipline)"
        return 1
      let hyleFuel := Rwv.Hyle.Bridge.progFuel hp
      let hDmap := Rwv.Hyle.Bridge.dmapOf hp
      let mut t : Tally := {}
      for d in p.defns do
        let nm := s!"{d.name.occ}#{d.name.uniq}"
        if isCarrier d then
          if verbose then IO.println s!"SKIP      {nm}  (carrier)"
          t := { t with skip := t.skip + 1 }
        else
          match hdm.get? (defnBase d.name.occ) with
          | none =>
              if verbose then
                IO.println s!"SKIP      {nm}  (no Hyle defn '{defnBase d.name.occ}': inlined away, reactive, or renamed)"
              t := { t with skip := t.skip + 1 }
          | some h =>
              if d.params.length ≠ h.params.length then
                IO.println s!"SKIP      {nm}  (arity {d.params.length} vs {h.params.length}: eta drift)"
                t := { t with skip := t.skip + 1 }
              else do
                -- Γ: Eidos param unique ↦ (Hyle param var at its declared
                -- width, the Eidos parameter type).
                let prs := d.params.zip (h.params.zip h.sig.params)
                let Γ : HashMap Int (NF × Ty) :=
                  prs.foldr (fun (pr : Id × String × Nat) m =>
                    m.insert pr.1.uniq (.var pr.2.2 pr.2.1, pr.1.sig.ty)) ∅
                match cexp Δ edm fuel Γ d.body with
                | .error msg =>
                    IO.println s!"GAP       {nm}  ({msg})"
                    t := { t with gap := t.gap + 1 }
                | .ok (ne, _ty) =>
                    let ρ0 : HashMap String NF :=
                      (h.params.zip h.sig.params).foldl
                        (fun m pr => m.insert pr.1 (.var pr.2 pr.1)) ∅
                    match Rwv.Hyle.Bridge.symExp hDmap hyleFuel ρ0 h.body with
                    | .error msg =>
                        IO.println s!"SKIP      {nm}  (Hyle symExp: {msg})"
                        t := { t with skip := t.skip + 1 }
                    | .ok nh =>
                        if ne.cfold == nh.cfold then do
                          -- the leg the VERIFIED checkDefnPair certifies
                          if verbose then IO.println s!"OK-V      {nm}"
                          t := { t with okV := t.okV + 1 }
                        else if Rwv.Hyle.Bridge.cfoldW3 ne == Rwv.Hyle.Bridge.cfoldW3 nh then do
                          IO.println s!"OK-W      {nm}"
                          t := { t with ok := t.ok + 1 }
                        else if dagEq ne nh then do
                          IO.println s!"OK-DAG    {nm}"
                          t := { t with okDag := t.okDag + 1 }
                        else do
                          IO.println s!"MISMATCH  {nm}"
                          t := { t with mismatch := t.mismatch + 1 }
      IO.println s!"summary: {t.okV} ok-v, {t.ok} ok-w, {t.okDag} ok-dag, {t.mismatch} mismatch, {t.gap} gap, {t.skip} skip"
      return (if t.mismatch > 0 then 1 else 0)
  | _ => IO.eprintln "usage: rwv-cexp-validate <file.eir> <file.rwc> [--fuel=N] [-v]"; return 2

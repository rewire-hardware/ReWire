/-
The bundle validator: ONE pure entry point owning every premise the
public soundness theorem needs, so that an executable's success is
literally the theorem's hypothesis. `validateBundle` takes the exact
artifact texts and applies, in order: both parsers; the
primitive-basis conflict gate (a redeclaration of a basis datatype
that is not structurally identical to the canonical declaration is
never silently replaced); the reserved fresh-unique gate (inputs
inside the range eta saturation mints from are refused, not risked);
the foreign-occurrence scan (Cryptol and clocked externs are outside
the certified profile; whether an extern occurrence carries a model is
decided from the Eidos implementation argument alone, and a
source/target disagreement about model-ness is rejected); the source
machine well-formedness judgment (`Program.checkMachine`, on the
canonical-basis pre-eta program); target well-formedness
(`Rwv.Hyle.Program.check`) and a denoting target definition
environment (`Rwv.Hyle.Sem.mkFEnv`) — a target that can never execute
must not validate vacuously; eta saturation to signature arity; and
the library validator (`validateProcE`).

`validateBundle_sound` is the top-level theorem: a `.validated` result
alone yields the §7.5.6 correspondence for the eta-saturated program
the artifacts determine — every side condition (including the
`ForeignC` premise, discharged at the empty foreign environment the
bundle actually builds) is internal. The scans themselves are
untrusted gates: the proof only ever splits on their results, so a
scan bug can suppress a verdict, never manufacture one.

The traversal helpers are fueled rather than partial (house style: the
library carries no partial definitions); exhaustion fails closed — the
scan reports an error and the unique-minimum collapses to the refused
floor.
-/
import Rwv.Eidos.Parse
import Rwv.Eidos.PrimBasis
import Rwv.Eidos.EtaSat
import Rwv.Eidos.Check
import Rwv.Eidos.Cstep
import Rwv.Hyle.Parse
import Rwv.Hyle.Check

namespace Rwv.Eidos

open Std (HashMap)

namespace Bundle

/-- A foreign-scan finding: `unsupported` names a feature outside the
certified profile; `rejected` names a source/target inconsistency on
supported inputs (the symbolic comparison would reject it anyway —
the scan just says why first). -/
inductive ScanErr where
  | unsupported (reason : String)
  | rejected (reason : String)

def scanFuelErr : ScanErr := .rejected "foreign scan: fuel exhausted (rwv bug?)"

mutual

/-- Scan an expression. Foreign occurrences are classified from the
source artifact alone (`Eval.externModelless` — the same classifier
the evaluator and the verified compiler dispatch on); `tgtModeled`
reports whether the target program declares a model for a given extern
name, used only to cross-check consistency with the source-side
classification, never to choose it. -/
def scanExp (tm : String → Bool) : Nat → Exp → Except ScanErr Unit
  | 0, _ => throw scanFuelErr
  | fuel + 1, e => do
      let (h, args) := EtaSat.flattenApp' e
      args.forM fun a =>
        match a with
        | .eArg ae => scanExp tm fuel ae
        | .tArg _ => pure ()
      match h with
      | .prim _ .cryptol =>
          throw (.unsupported "rwPrimCryptol: Cryptol foreign functions are outside \
            the certified profile (their semantics exists only as compiler output)")
      | .prim _ .«extern» =>
          let eargs := args.filterMap fun | .eArg a => some a | .tArg _ => none
          match eargs with
          | _ps :: clk :: _rst :: _as :: _rs :: nameE :: impl :: _rest =>
              let s := match nameE with | .litStr s => s | _ => "?"
              match clk with
              | .litStr "" =>
                  if Eval.externModelless impl then
                    if tm s then
                      throw (.rejected s!"extern {s}: the source occurrence is \
                        model-less but the target declares a model")
                    else pure ()
                  else
                    if tm s then pure ()
                    else
                      throw (.rejected s!"extern {s}: the source occurrence carries \
                        a model but the target declares none")
              | _ =>
                  throw (.unsupported s!"extern {s}: sequential (clocked) externs \
                    are outside the certified fragment")
          | _ => throw (.unsupported "rwPrimExtern: under-applied foreign occurrence")
      | .lam _ b => scanExp tm fuel b
      | .letE bnd b => do
          match bnd with
          | .nonRec _ r => scanExp tm fuel r
          | .recB bs => bs.forM fun p => scanExp tm fuel p.2
          | .join _ _ j => scanExp tm fuel j
          scanExp tm fuel b
      | .cases _ sc _ alts => do
          scanExp tm fuel sc
          alts.forM (scanAlt tm fuel)
      | .jump _ es => es.forM (scanExp tm fuel)
      | .litList _ es => es.forM (scanExp tm fuel)
      | .litVec _ es => es.forM (scanExp tm fuel)
      | _ => pure ()

def scanAlt (tm : String → Bool) : Nat → Alt → Except ScanErr Unit
  | 0, _ => throw scanFuelErr
  | fuel + 1, .mk _ _ b => scanExp tm fuel b

end

mutual

def scanTerm (tm : String → Bool) : Nat → Term → Except ScanErr Unit
  | 0, _ => throw scanFuelErr
  | fuel + 1, .pause o _ as => do
      scanExp tm fuel o
      as.forM (scanExp tm fuel)
  | fuel + 1, .goto _ as => as.forM (scanExp tm fuel)
  | fuel + 1, .halt e => scanExp tm fuel e
  | fuel + 1, .cases sc alts => do
      scanExp tm fuel sc
      alts.forM (scanTAlt tm fuel)

def scanTAlt (tm : String → Bool) : Nat → TAlt → Except ScanErr Unit
  | 0, _ => throw scanFuelErr
  | fuel + 1, .mk _ _ t => scanTerm tm fuel t

end

def scanCmd (tm : String → Bool) (fuel : Nat) : Cmd → Except ScanErr Unit
  | .bind _ e => scanExp tm fuel e
  | .get _ _ => pure ()
  | .put _ e => scanExp tm fuel e

def scanBlock (tm : String → Bool) (fuel : Nat) (b : Block) : Except ScanErr Unit := do
  b.cmds.forM (scanCmd tm fuel)
  scanTerm tm fuel b.term

def scanProgram (tm : String → Bool) (fuel : Nat) (p : Program) : Except ScanErr Unit := do
  p.defns.forM fun d => scanExp tm fuel d.body
  p.procs.forM fun pr => do
    pr.cells.forM fun c =>
      match c.init with
      | some e => scanExp tm fuel e
      | none => pure ()
    scanBlock tm fuel pr.entry
    pr.blocks.forM fun lb => scanBlock tm fuel lb.2

/-! ## The reserved fresh-unique range

Eta saturation mints binder uniques from -10⁹ down on the invariant
that the input never uses that range (the bridge mints non-negative
term uniques; the prim basis' type variables use small negatives). An
input inside the range is refused rather than risked. Fuel exhaustion
collapses to the floor, which the caller refuses — fail closed. -/

def uniqFloor : Int := -1000000000

def minIdExp : Nat → Exp → Int
  | 0, _ => uniqFloor
  | fuel + 1, e =>
    match e with
    | .var x => x.uniq
    | .lam x b => min x.uniq (minIdExp fuel b)
    | .app f (.eArg a) => min (minIdExp fuel f) (minIdExp fuel a)
    | .app f (.tArg _) => minIdExp fuel f
    | .letE (.nonRec x r) b => min x.uniq (min (minIdExp fuel r) (minIdExp fuel b))
    | .letE (.recB bs) b =>
        bs.foldl (fun acc p => min acc (min p.1.uniq (minIdExp fuel p.2))) (minIdExp fuel b)
    | .letE (.join _ ps j) b =>
        ps.foldl (fun acc x => min acc x.uniq) (min (minIdExp fuel j) (minIdExp fuel b))
    | .jump _ es => es.foldl (fun acc a => min acc (minIdExp fuel a)) 0
    | .cases _ sc x alts =>
        alts.foldl
          (fun acc alt => match alt with
            | .mk _ bs b => bs.foldl (fun a y => min a y.uniq) (min acc (minIdExp fuel b)))
          (min x.uniq (minIdExp fuel sc))
    | .litList _ es => es.foldl (fun acc a => min acc (minIdExp fuel a)) 0
    | .litVec _ es => es.foldl (fun acc a => min acc (minIdExp fuel a)) 0
    | _ => 0

def minIdTerm : Nat → Term → Int
  | 0, _ => uniqFloor
  | fuel + 1, t =>
    match t with
    | .pause o _ as => as.foldl (fun acc e => min acc (minIdExp fuel e)) (minIdExp fuel o)
    | .goto _ as => as.foldl (fun acc e => min acc (minIdExp fuel e)) 0
    | .halt e => minIdExp fuel e
    | .cases sc alts =>
        alts.foldl
          (fun acc alt => match alt with
            | .mk _ bs tt => bs.foldl (fun a y => min a y.uniq) (min acc (minIdTerm fuel tt)))
          (minIdExp fuel sc)

def minIdCmd (fuel : Nat) : Cmd → Int
  | .bind x e => min x.uniq (minIdExp fuel e)
  | .get x _ => x.uniq
  | .put _ e => minIdExp fuel e

def minIdBlock (fuel : Nat) (b : Block) : Int :=
  b.cmds.foldl (fun acc c => min acc (minIdCmd fuel c))
    (b.params.foldl (fun acc x => min acc x.uniq) (minIdTerm fuel b.term))

def minIdProgram (fuel : Nat) (p : Program) : Int :=
  let dmin := p.defns.foldl
    (fun acc d => d.params.foldl (fun a x => min a x.uniq)
      (min acc (min d.name.uniq (minIdExp fuel d.body)))) 0
  p.procs.foldl
    (fun acc pr =>
      let cmin := pr.cells.foldl
        (fun a c => match c.init with | some e => min a (minIdExp fuel e) | none => a) acc
      pr.blocks.foldl (fun a lb => min a (min lb.1.uniq (minIdBlock fuel lb.2)))
        (min cmin (minIdBlock fuel pr.entry)))
    dmin

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

/-- Generous structural fuel for the eta saturation and the scans (one
unit per node; far above any real artifact). -/
def structuralFuel : Nat := 1000000000

end Bundle

/-- The bundle verdict: the four outcome classes the drivers print. -/
inductive BundleResult where
  | validated
  | rejected (reason : String)
  | unsupported (reason : String)
  | error (reason : String)
deriving Repr

/-- The bundle validator (see the module header): every gate between
the artifact texts and the library validator, in order. -/
def validateBundle (srcName srcTxt tgtName tgtTxt : String) (fuel : Nat) : BundleResult :=
  match parseEir srcTxt srcName with
  | .error e => .error s!"{srcName}: {e}"
  | .ok p₀ =>
    match Rwv.Hyle.parseProgram tgtTxt tgtName with
    | .error e => .error s!"{tgtName}: {e}"
    | .ok hp =>
      match Bundle.basisConflict p₀ with
      | some n => .error s!"conflicting redeclaration of primitive datatype {n} \
          (must be structurally identical to the canonical declaration)"
      | none =>
        let p₁ := addPrims p₀
        if Bundle.minIdProgram Bundle.structuralFuel p₁ ≤ Bundle.uniqFloor then
          .error s!"input uses a term unique at or below {Bundle.uniqFloor} \
            (reserved for freshly minted eta binders)"
        else
          match Bundle.scanProgram (fun s => ((Rwv.Hyle.Sem.xenv hp).get? s).isSome)
              Bundle.structuralFuel p₁ with
          | .error (.unsupported r) => .unsupported r
          | .error (.rejected r) => .rejected r
          | .ok () =>
            match p₁.checkMachine with
            | .error e => .rejected s!"source well-formedness: {e}"
            | .ok () =>
              match hp.check with
              | .error e => .rejected s!"target well-formedness: {e}"
              | .ok () =>
                match Rwv.Hyle.Sem.mkFEnv hp with
                | .error e => .rejected s!"target definition environment does not \
                    denote: {e}"
                | .ok _ =>
                  match etaSaturate Bundle.structuralFuel p₁ with
                  | .error e => .error s!"{srcName}: eta-saturation: {e}"
                  | .ok p =>
                    match p.procs with
                    | [] => .error "no proc in the Eidos dump (a machine-level \
                        pass-8 dump is required)"
                    | _ :: _ :: _ => .unsupported "multiple procs"
                    | [pr] =>
                      match Cstep.validateProcE (DEnv.ofDatas p.datas)
                          (mkDefnMap p.defns) pr hp fuel with
                      | .ok _ => .validated
                      | .error e => .rejected e

/-- The `ForeignC` premise at the environment the bundle actually
builds: `DEnv.ofDatas` leaves every foreign field at its default
(empty definition/extern tables, no Cryptol keys), so the premise
holds against the empty implementing pair. -/
theorem foreignC_ofDatas (datas : List DataDefn) :
    ∃ X F, Rwv.Eidos.Cexp.ForeignC (DEnv.ofDatas datas) X F :=
  ⟨(DEnv.ofDatas datas).hyleX, (∅ : Rwv.Hyle.Sem.FEnv),
    Rwv.Eidos.Cexp.foreignC_empty
      (fun f d hd => absurd hd (by simp [DEnv.ofDatas]))
      rfl
      (fun _ _ _ => rfl)⟩

/-- The top-level soundness theorem: a `.validated` bundle result
alone — no side conditions — yields the §7.5.6 correspondence, at
every evaluation/goto fuel at least the bundle's and every extern
interpretation, for the eta-saturated program the artifacts determine.
The artifacts' processing chain is part of the conclusion, so a caller
knows exactly which program was certified. -/
theorem validateBundle_sound {srcName srcTxt tgtName tgtTxt : String} {fuel : Nat}
    (h : validateBundle srcName srcTxt tgtName tgtTxt fuel = .validated) :
    ∃ p₀ hp p pr,
      parseEir srcTxt srcName = .ok p₀
      ∧ Rwv.Hyle.parseProgram tgtTxt tgtName = .ok hp
      ∧ etaSaturate Bundle.structuralFuel (addPrims p₀) = .ok p
      ∧ p.procs = [pr]
      ∧ ∀ ef gf (E : Rwv.Hyle.Sem.EEnv), fuel ≤ ef →
          Corresponds (DEnv.ofDatas p.datas) (mkDefnMap p.defns) ef gf pr hp E := by
  cases hpe : parseEir srcTxt srcName with
  | error e => rw [validateBundle, hpe] at h; exact absurd h (by simp)
  | ok p₀ =>
  cases hhp : Rwv.Hyle.parseProgram tgtTxt tgtName with
  | error e => rw [validateBundle, hpe, hhp] at h; exact absurd h (by simp)
  | ok hp =>
  rw [validateBundle, hpe, hhp] at h
  dsimp only at h
  cases hbc : Bundle.basisConflict p₀ with
  | some n => rw [hbc] at h; exact absurd h (by simp)
  | none =>
  rw [hbc] at h
  dsimp only at h
  by_cases huniq : Bundle.minIdProgram Bundle.structuralFuel (addPrims p₀) ≤ Bundle.uniqFloor
  · rw [if_pos huniq] at h; exact absurd h (by simp)
  · rw [if_neg huniq] at h
    cases hscan : Bundle.scanProgram (fun s => ((Rwv.Hyle.Sem.xenv hp).get? s).isSome)
        Bundle.structuralFuel (addPrims p₀) with
    | error se => rw [hscan] at h; cases se <;> exact absurd h (by simp)
    | ok u =>
    cases u
    rw [hscan] at h
    dsimp only at h
    cases hcm : (addPrims p₀).checkMachine with
    | error e => rw [hcm] at h; exact absurd h (by simp)
    | ok u =>
    cases u
    rw [hcm] at h
    dsimp only at h
    cases hhc : hp.check with
    | error e => rw [hhc] at h; exact absurd h (by simp)
    | ok u =>
    cases u
    rw [hhc] at h
    dsimp only at h
    cases hfe : Rwv.Hyle.Sem.mkFEnv hp with
    | error e => rw [hfe] at h; exact absurd h (by simp)
    | ok F =>
    rw [hfe] at h
    dsimp only at h
    cases heta : etaSaturate Bundle.structuralFuel (addPrims p₀) with
    | error e => rw [heta] at h; exact absurd h (by simp)
    | ok p =>
    rw [heta] at h
    dsimp only at h
    match hprocs : p.procs with
    | [] => rw [hprocs] at h; exact absurd h (by simp)
    | _ :: _ :: _ => rw [hprocs] at h; exact absurd h (by simp)
    | [pr] =>
    rw [hprocs] at h
    dsimp only at h
    cases hv : Cstep.validateProcE (DEnv.ofDatas p.datas) (mkDefnMap p.defns) pr hp fuel with
    | error e => rw [hv] at h; exact absurd h (by simp)
    | ok u =>
    -- `cases h : e` substituted each scrutinee in the goal, so the
    -- pipeline conjuncts are reflexive here.
    refine ⟨p₀, hp, p, pr, rfl, rfl, heta, hprocs, ?_⟩
    intro ef gf E hef
    exact Cstep.validateProc_corresponds
      (by rw [Cstep.validateProc, hv]) hef (foreignC_ofDatas p.datas)

#print axioms Rwv.Eidos.validateBundle_sound

end Rwv.Eidos

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
alone yields the doc/synolon.md §5.6 correspondence for the eta-saturated program
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
import Rwv.Synolon.Parse
import Rwv.Synolon.PrimBasis
import Rwv.Synolon.EtaSat
import Rwv.Synolon.Check
import Rwv.Synolon.Cstep
import Rwv.Hyle.Parse
import Rwv.Hyle.Check
import Rwv.Hyle.Progress

namespace Rwv.Synolon

open Std (HashMap)
open Rwv.Eidos

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
source artifact alone (`Eval.externModelless` on the implementation
argument, `Eval.externGenerics` on the parameter descriptor — the same
classifiers the evaluator and the verified compiler dispatch on); the
target declaration lookup `tm` is used only to cross-check consistency
with the source-side classification (model-ness, and the generic-name
order the fold derives from the same descriptor), never to choose it.
A value disagreement needs no scan: the compiled node's generic values
meet the target call's through the uninterpreted-symbol identity. -/
def scanExp (tm : String → Option Rwv.Hyle.Extern) : Nat → Exp → Except ScanErr Unit
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
          | ps :: clk :: _rst :: _as :: _rs :: nameE :: impl :: _rest =>
              let s := match nameE with | .litStr s => s | _ => "?"
              match clk with
              | .litStr "" =>
                  if Eval.externModelless impl then
                    match tm s with
                    | some ex =>
                        if ex.model.isSome then
                          throw (.rejected s!"extern {s}: the source occurrence is \
                            model-less but the target declares a model")
                        else
                          match Eval.externGenerics ps with
                          | none => throw (.unsupported s!"extern {s}: non-literal \
                              extern parameter (outside the certified fragment)")
                          | some gps =>
                              if ex.generics = gps.map (·.1) then pure ()
                              else throw (.rejected s!"extern {s}: the source \
                                parameter names {gps.map (·.1)} do not match the \
                                target declaration's generics {ex.generics}")
                    | none =>
                        -- No target declaration: the occurrence is unused
                        -- (a used one fails target well-formedness), and an
                        -- unextractable descriptor still refuses a verdict.
                        if (Eval.externGenerics ps).isSome then pure ()
                        else throw (.unsupported s!"extern {s}: non-literal \
                            extern parameter (outside the certified fragment)")
                  else
                    if (tm s).elim false (·.model.isSome) then pure ()
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

def scanAlt (tm : String → Option Rwv.Hyle.Extern) : Nat → Alt → Except ScanErr Unit
  | 0, _ => throw scanFuelErr
  | fuel + 1, .mk _ _ b => scanExp tm fuel b

end

mutual

def scanTerm (tm : String → Option Rwv.Hyle.Extern) : Nat → Term → Except ScanErr Unit
  | 0, _ => throw scanFuelErr
  | fuel + 1, .pause o _ as => do
      scanExp tm fuel o
      as.forM (scanExp tm fuel)
  | fuel + 1, .goto _ as => as.forM (scanExp tm fuel)
  | fuel + 1, .halt e => scanExp tm fuel e
  | fuel + 1, .cases sc alts => do
      scanExp tm fuel sc
      alts.forM (scanTAlt tm fuel)

def scanTAlt (tm : String → Option Rwv.Hyle.Extern) : Nat → TAlt → Except ScanErr Unit
  | 0, _ => throw scanFuelErr
  | fuel + 1, .mk _ _ t => scanTerm tm fuel t

end

def scanCmd (tm : String → Option Rwv.Hyle.Extern) (fuel : Nat) : Cmd → Except ScanErr Unit
  | .bind _ e => scanExp tm fuel e
  | .get _ _ => pure ()
  | .put _ e => scanExp tm fuel e

def scanBlock (tm : String → Option Rwv.Hyle.Extern) (fuel : Nat) (b : Block) : Except ScanErr Unit := do
  b.cmds.forM (scanCmd tm fuel)
  scanTerm tm fuel b.term

def scanProgram (tm : String → Option Rwv.Hyle.Extern) (fuel : Nat) (p : Program) : Except ScanErr Unit := do
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
  match parseSyn srcTxt srcName with
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
          match Bundle.scanProgram (fun s => hp.externs.find? (·.name = s))
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
                  -- The one checked-but-nonprogressing construct
                  -- (Rwv.Hyle.Progress): a target with device
                  -- instances could only ever validate vacuously, so
                  -- it is refused a verdict instead. (Model-less
                  -- generic extern calls read totally through the
                  -- (name, generics)-keyed environment.)
                  if hp.device.instances.isEmpty then
                  match etaSaturate Bundle.structuralFuel p₁ with
                  | .error e => .error s!"{srcName}: eta-saturation: {e}"
                  | .ok p =>
                    match p.procs with
                    | [] => .error "no proc in the source dump (a machine-level \
                        pass-8 dump is required)"
                    | _ :: _ :: _ => .unsupported "multiple procs"
                    | [pr] =>
                      match Cstep.validateProcE (DEnv.ofDatas p.datas)
                          (mkDefnMap p.defns) pr hp fuel with
                      | .ok _ => .validated
                      | .error e => .rejected e
                  else .unsupported "device instances (sequential externs) are \
                    outside the certified fragment"

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

/-- Inversion of a `.validated` bundle result: every fact the gate
tower checked, in one package — the two parses, target
well-formedness, a denoting definition environment, instance-freedom,
the saturation chain, the single process, and the
library validator's acceptance. The downstream theorems
(`validateBundle_sound`, `validateBundle_refines`) consume this. -/
theorem validateBundle_inv {srcName srcTxt tgtName tgtTxt : String} {fuel : Nat}
    (h : validateBundle srcName srcTxt tgtName tgtTxt fuel = .validated) :
    ∃ p₀ hp p pr,
      parseSyn srcTxt srcName = .ok p₀
      ∧ Rwv.Hyle.parseProgram tgtTxt tgtName = .ok hp
      ∧ hp.check = .ok ()
      ∧ (∃ F, Rwv.Hyle.Sem.mkFEnv hp = .ok F)
      ∧ hp.device.instances.isEmpty = true
      ∧ etaSaturate Bundle.structuralFuel (addPrims p₀) = .ok p
      ∧ p.procs = [pr]
      ∧ Cstep.validateProcE (DEnv.ofDatas p.datas) (mkDefnMap p.defns) pr hp fuel = .ok () := by
  cases hpe : parseSyn srcTxt srcName with
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
    cases hscan : Bundle.scanProgram (fun s => hp.externs.find? (·.name = s))
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
    by_cases hinst : hp.device.instances.isEmpty = true
    case neg => rw [if_neg hinst] at h; exact absurd h (by simp)
    case pos =>
    rw [if_pos hinst] at h
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
    cases u
    exact ⟨p₀, hp, p, pr, rfl, rfl, hhc, ⟨F, hfe⟩, hinst, heta, hprocs, hv⟩

/-- The top-level soundness theorem: a `.validated` bundle result
alone — no side conditions — yields the doc/synolon.md §5.6 correspondence, at
every evaluation/goto fuel at least the bundle's and every extern
interpretation, for the eta-saturated program the artifacts determine.
The artifacts' processing chain is part of the conclusion, so a caller
knows exactly which program was certified. -/
theorem validateBundle_sound {srcName srcTxt tgtName tgtTxt : String} {fuel : Nat}
    (h : validateBundle srcName srcTxt tgtName tgtTxt fuel = .validated) :
    ∃ p₀ hp p pr,
      parseSyn srcTxt srcName = .ok p₀
      ∧ Rwv.Hyle.parseProgram tgtTxt tgtName = .ok hp
      ∧ etaSaturate Bundle.structuralFuel (addPrims p₀) = .ok p
      ∧ p.procs = [pr]
      ∧ ∀ ef gf (E : Rwv.Hyle.Sem.EEnv), fuel ≤ ef →
          Corresponds (DEnv.ofDatas p.datas) (mkDefnMap p.defns) ef gf pr hp E := by
  obtain ⟨p₀, hp, p, pr, hpe, hhp, _, _, _, heta, hprocs, hv⟩ := validateBundle_inv h
  refine ⟨p₀, hp, p, pr, hpe, hhp, heta, hprocs, ?_⟩
  intro ef gf E hef
  exact Cstep.validateProc_corresponds
    (by rw [Cstep.validateProc, hv]) hef (foreignC_ofDatas p.datas)

/-! ## Refinement glue

The width discipline `Rwv.Hyle.Progress.Program.run_progress` needs is
recovered from facts the bundle already checked: `portSplit` slices at
exactly the `detupleSizes` widths, `mkPlan` (inverted out of
`validateProcE`) accepted the target only because its input ports ARE
those widths, and `detupleSizes` is stable in its fuel. `mkFEnv` can
fail only in `topoDefns`, which never consults the extern environment,
so a denoting environment at the empty interpretation denotes at every
interpretation. -/

private theorem error_ne_ok {α : Type} {msg : String} {a : α} {P : Prop}
    (h : (Except.error msg : Except String α) = .ok a) : P := by cases h

private theorem except_bind_eq_ok {α β : Type} {x : Except String α}
    {f : α → Except String β} {b : β} (h : (x >>= f) = .ok b) :
    ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x with
  | error e => exact error_ne_ok h
  | ok a => exact ⟨a, rfl, h⟩

private theorem bind_ok {α β : Type} {a : α} {f : α → Except String β} :
    ((Except.ok a : Except String α) >>= f) = f a := rfl

private theorem mapM_ok_mem {α β : Type} {f : α → Except String β} {l : List α}
    {l' : List β} (h : l.mapM f = .ok l') : ∀ y ∈ l', ∃ x ∈ l, f x = .ok y := by
  induction l generalizing l' with
  | nil =>
      rw [List.mapM_nil] at h
      cases h
      intro y hy
      exact absurd hy (by simp)
  | cons x xs ih =>
      rw [List.mapM_cons] at h
      obtain ⟨b, hb, h⟩ := except_bind_eq_ok h
      obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
      cases h
      intro y hy
      rcases List.mem_cons.mp hy with rfl | hy'
      · exact ⟨x, List.mem_cons_self, hb⟩
      · obtain ⟨x', hx', hfx⟩ := ih hbs y hy'
        exact ⟨x', List.mem_cons_of_mem _ hx', hfx⟩

private theorem sizeOf_mapM_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k')
    {l : List Ty} {ns : List Nat} (h : l.mapM (Δ.sizeOf k []) = .ok ns) :
    l.mapM (Δ.sizeOf k' []) = .ok ns := by
  induction l generalizing ns with
  | nil => simpa using h
  | cons t ts ih =>
      rw [List.mapM_cons] at h ⊢
      obtain ⟨n, hn, h⟩ := except_bind_eq_ok h
      obtain ⟨ms, hms, h⟩ := except_bind_eq_ok h
      rw [DEnv.sizeOf_mono Δ hk hn, bind_ok, ih hms, bind_ok]
      exact h

/-- `Val.detupleSizes` is monotone in its fuel (it is `DEnv.sizeOf`
compositions and pure arithmetic). -/
theorem detupleSizes_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k') {t : Ty}
    {szs : List Nat} (h : Val.detupleSizes Δ k t = .ok szs) :
    Val.detupleSizes Δ k' t = .ok szs := by
  rw [Val.detupleSizes] at h ⊢
  dsimp only at h ⊢
  obtain ⟨whole, hw, h⟩ := except_bind_eq_ok h
  obtain ⟨sizes, hs, h⟩ := except_bind_eq_ok h
  rw [DEnv.sizeOf_mono Δ hk hw, bind_ok, sizeOf_mapM_mono hk hs, bind_ok]
  exact h

private theorem foldSlices_widths {n : Nat} (bits : BitVec n) :
    ∀ (sizes : List Nat) (hi : Nat) (acc : List Rwv.Hyle.BV),
      (((sizes.foldl
          (fun (p : Nat × List Rwv.Hyle.BV) w =>
            (p.1 - w, (⟨w, bits.extractLsb' (p.1 - w) w⟩ : Rwv.Hyle.BV) :: p.2))
          (hi, acc)).2).map (·.width))
        = sizes.reverse ++ acc.map (·.width) := by
  intro sizes
  induction sizes with
  | nil => intro hi acc; simp
  | cons w ws ih =>
      intro hi acc
      rw [List.foldl_cons, ih]
      simp [List.reverse_cons, List.append_assoc]

/-- `Val.portSplit` produces slices at exactly the `detupleSizes`
widths — a fact of the construction, independent of the value. -/
theorem portSplit_widths {Δ : DEnv} {fuel : Nat} {t : Ty} {v : Val}
    {bvs : List Rwv.Hyle.BV} (h : Val.portSplit Δ fuel t v = .ok bvs) :
    ∃ szs, Val.detupleSizes Δ fuel t = .ok szs ∧ bvs.map (·.width) = szs := by
  rw [Val.portSplit] at h
  obtain ⟨bv, hbv, h⟩ := except_bind_eq_ok h
  obtain ⟨sizes, hs, h⟩ := except_bind_eq_ok h
  refine ⟨sizes, hs, ?_⟩
  dsimp only at h
  split at h
  · exact error_ne_ok h
  · cases h
    have hw := foldSlices_widths bv.bits sizes bv.width []
    rw [List.map_reverse, hw]
    simp

/-- Inversion of `mkPlan`: it accepted the device only because the
input port widths ARE the `detupleSizes` split of the process input
type. -/
theorem mkPlan_inputs {Δ : DEnv} {fuel : Nat} {p : Proc} {lo : Cstep.Layout}
    {dev : Rwv.Hyle.Device} {plan : Cstep.Plan}
    (h : Cstep.mkPlan Δ fuel p lo dev = .ok plan) :
    ∃ szs, Val.detupleSizes Δ fuel p.inTy = .ok szs ∧ dev.inputs.map (·.2) = szs := by
  rw [Cstep.mkPlan] at h
  split at h
  · obtain ⟨inSzs, hin, h⟩ := except_bind_eq_ok h
    obtain ⟨outSzs, hout, h⟩ := except_bind_eq_ok h
    split at h
    · rename_i hbeq
      exact ⟨inSzs, hin, eq_of_beq hbeq⟩
    · exact error_ne_ok h
  · exact error_ne_ok h

/-- Inversion of `validateProcE` down to its layout/plan stage. -/
theorem validateProcE_plan {Δ : DEnv} {edm : HashMap Int Defn} {p : Proc}
    {H : Rwv.Hyle.Program} {fuel : Nat} {u : Unit}
    (h : Cstep.validateProcE Δ edm p H fuel = .ok u) :
    ∃ lo plan, Cstep.mkLayoutL Δ fuel p = .ok lo
      ∧ Cstep.mkPlan Δ fuel p lo H.device = .ok plan := by
  rw [Cstep.validateProcE] at h
  split at h
  rotate_left
  · exact error_ne_ok h
  dsimp only at h
  split at h
  rotate_left
  · exact error_ne_ok h
  split at h
  rotate_left
  · exact error_ne_ok h
  split at h
  rotate_left
  · exact error_ne_ok h
  split at h
  rotate_left
  · exact error_ne_ok h
  obtain ⟨lo, hlo, h⟩ := except_bind_eq_ok h
  obtain ⟨plan, hplan, _⟩ := except_bind_eq_ok h
  exact ⟨lo, plan, hlo, hplan⟩

private theorem foldlM_pure_ok {α β : Type} (g : β → α → β) (l : List α) (init : β) :
    (l.foldlM (fun b a => (pure (g b a) : Except String β)) init) = .ok (l.foldl g init) := by
  induction l generalizing init with
  | nil => rfl
  | cons a as ih =>
      rw [List.foldlM_cons]
      exact ih (g init a)

/-- A denoting definition environment denotes at EVERY extern
interpretation: only `topoDefns` can fail, and it never consults the
interpretation — the fold itself always succeeds. -/
theorem mkFEnv_ok_any {p : Rwv.Hyle.Program} {F₀ : Rwv.Hyle.Sem.FEnv}
    (h : Rwv.Hyle.Sem.mkFEnv p = .ok F₀) (E : Rwv.Hyle.Sem.EEnv) :
    ∃ F, Rwv.Hyle.Sem.mkFEnv p E = .ok F := by
  rw [Rwv.Hyle.Sem.mkFEnv] at h ⊢
  dsimp only at h ⊢
  obtain ⟨ordered, hord, _⟩ := except_bind_eq_ok h
  rw [hord, bind_ok]
  exact ⟨_, foldlM_pure_ok _ ordered _⟩

/-- The top-level refinement theorem: a `.validated` bundle result
alone yields FORWARD REFINEMENT — every successful, well-typed source
execution has a successful, agreeing target execution — at every
evaluation/goto fuel at least the bundle's and every extern
interpretation. The target-run existence comes from
`Rwv.Hyle.Progress.Program.run_progress` at the facts the bundle
checked (target well-formedness, a denoting definition environment,
instance-freedom), with the stimulus width discipline
recovered from `portSplit`/`mkPlan` agreement on `detupleSizes`. A
target that can never run cannot satisfy this theorem. -/
theorem validateBundle_refines {srcName srcTxt tgtName tgtTxt : String} {fuel : Nat}
    (h : validateBundle srcName srcTxt tgtName tgtTxt fuel = .validated) :
    ∃ p₀ hp p pr,
      parseSyn srcTxt srcName = .ok p₀
      ∧ Rwv.Hyle.parseProgram tgtTxt tgtName = .ok hp
      ∧ etaSaturate Bundle.structuralFuel (addPrims p₀) = .ok p
      ∧ p.procs = [pr]
      ∧ ∀ ef gf (E : Rwv.Hyle.Sem.EEnv), fuel ≤ ef →
          Refines (DEnv.ofDatas p.datas) (mkDefnMap p.defns) ef gf pr hp E := by
  obtain ⟨p₀, hp, p, pr, hpe, hhp, hhc, ⟨F₀, hfe⟩, hinst, heta, hprocs, hv⟩ :=
    validateBundle_inv h
  refine ⟨p₀, hp, p, pr, hpe, hhp, heta, hprocs, ?_⟩
  intro ef gf E hef ins hty encIns henc mt hmt
  -- The agreement half, from the correspondence theorem.
  have hcorr : Corresponds (DEnv.ofDatas p.datas) (mkDefnMap p.defns) ef gf pr hp E :=
    Cstep.validateProc_corresponds (by rw [Cstep.validateProc, hv]) hef
      (foreignC_ofDatas p.datas)
  -- The existence half: the checked target runs on the encoded inputs.
  obtain ⟨lo, plan, hlo, hplan⟩ := validateProcE_plan hv
  obtain ⟨inSzs, hszs, hinputs⟩ := mkPlan_inputs hplan
  have hszs' : Val.detupleSizes (DEnv.ofDatas p.datas) ef pr.inTy = .ok inSzs :=
    detupleSizes_mono hef hszs
  have hstim : ∀ cyc ∈ encIns, cyc.length = hp.device.inputs.length ∧
      ∀ i (h1 : i < cyc.length) (h2 : i < hp.device.inputs.length),
        cyc[i].width = (hp.device.inputs[i]).2 := by
    intro cyc hcyc
    obtain ⟨v, _, hsplit⟩ := mapM_ok_mem henc cyc hcyc
    obtain ⟨szs', hszs'', hwidths⟩ := portSplit_widths hsplit
    rw [hszs'] at hszs''
    cases hszs''
    have hmapeq : cyc.map (·.width) = hp.device.inputs.map (·.2) := by
      rw [hwidths, hinputs]
    constructor
    · have hlen := congrArg List.length hmapeq
      simpa using hlen
    · intro i h1 h2
      have h2' : i < (hp.device.inputs.map (·.2)).length := by simpa using h2
      have hi : (cyc.map (·.width))[i]'(by simpa using h1)
          = (hp.device.inputs.map (·.2))[i]'h2' := by
        simp only [hmapeq]
      simpa using hi
  have hinst' : hp.device.instances = [] := by
    cases hd : hp.device.instances with
    | nil => rfl
    | cons a as => rw [hd] at hinst; exact absurd hinst (by simp)
  obtain ⟨F, hFE⟩ := mkFEnv_ok_any hfe E
  obtain ⟨ht, hht, _⟩ :=
    Rwv.Hyle.Progress.Program.run_progress hhc hinst' hstim hFE
  exact ⟨ht, hht, hcorr ins hty encIns henc mt hmt ht hht⟩

#print axioms Rwv.Synolon.validateBundle_sound
#print axioms Rwv.Synolon.validateBundle_refines

end Rwv.Synolon

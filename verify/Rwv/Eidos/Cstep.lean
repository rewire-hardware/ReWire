/-
The per-label machine-step validator (Phase 4b-ii of the
translation-validation plan): the MACHINE-STEP half of the verified
reference lowering. Where Rwv.Eidos.Cexp (Phase 4a) validates pure
definition bodies, this file validates the machine step itself — the
piece of ReWire.Eidos.ToHyle that assembles the step record and wires
the device:

  * `Layout` / `mkLayoutL` — the untrusted-but-checked mirror of
    ToHyle's `mkLayout`: the step-record accounting (pause record
    `halted | pad | out | label-tag | pad | args | cells`, halt record
    `halted | pad | answer-tag | pad | answer | cells`, the halted bit
    present only when a halt is reachable, label tags assigned in
    block order among pause targets, halt-answer tags in
    first-occurrence order).
  * `Plan` / `mkPlan` — the register/port accounting against the
    actual device: input and output port widths must be the
    `detupleSizes` splits of the process types, the first register
    (when the resumption load is non-empty) is the resumption-tag
    register at width rW, and the remaining registers partition into
    per-cell runs at the cells' `detupleSizes` widths (one cell ↦ a
    run of consecutive registers). Everything is checked, never
    trusted: a wrong layout or plan fails validation, not soundness.
  * `encodeList` / `encodeM` — the state encoding: a machine state
    (label, saved args, cells) as the register store the device would
    hold, `rep`-encoding args into `tag | pad | args` and splitting
    each cell's `rep` across its register run. `stateRel` (the
    relation `R` of the step-obligation schema) is its graph plus
    canonicality (`Cexp.VTy`) of the state's components.
  * `goCmds`/`goTerm`/`goAlts`/`goAlt1` — the symbolic machine-step
    compiler: commands thread a symbolic cell store (bind/get/put),
    terminators assemble pause/halt records (`pauseRec`/`haltRec`,
    mirroring `buildPause`/`buildHalt` with explicit zero pads), gotos
    are chased through the block graph intra-cycle (the goto closure;
    fuel-structural like `cexp`), and terminator cases compile to the
    same tag-slice if-chains as `Cexp.cchain`. Pure expressions
    compile through `Cexp.cexpFull` (the Phase 4b-i compiler — joins,
    the extended first-order rows, live `error`), which bounds this
    fragment.
  * `checkLabel` — the per-label obligation: compile the step from
    pause target L into a record normal form over the device's
    registers and inputs (saved args are slices of the resumption-tag
    register, the resumed input is the concatenation of the input
    ports, cells are concatenations of their register runs);
    symbolically evaluate the device step (`Bridge.symStep`),
    specialize it to label L's tag (`substNF`/`tagSubst` — the
    resumption-tag register rewritten to `tag | its own low bits`,
    sound exactly on stores whose tag field IS that tag, which is what
    `stateRel` supplies); compare output for output and register for
    register against the record's layout slices, after `NF.cfold` and
    the width-aware `cfoldW3` normalizers.
  * `checkInit` — the initial-state obligation: run the committed
    machine semantics concretely (`Machine.initCells` + entry
    `Machine.execBlock`), encode the post-reset state, and compare
    against the device's declared register initials (plus the
    `stateRel` canonicality checks, decided by `vtyB`).
  * `validateProcE` / `validateProc` — the composition over all pause
    targets plus the initial-state check.

Soundness layers proved here (all sorry-free; anything not proved is
not stated):

  * `vtyB_sound` — the decidable canonicality check implies `VTy`.
  * `substNF_eval` / `substNF_varsWF` — the tag specialization is
    denotation-preserving at valuations that fix the substituted
    variable's image, and preserves the width discipline.
  * `encodeList` spec lemmas and `checkInit_sound` — THE initial-state
    theorem: a passing `checkInit` discharges the `hinit` hypothesis
    of `stepObligations_corresponds` for `R := stateRel …`, for EVERY
    evaluation/goto fuel (by the FuelMono determinism argument).
  * The slice toolkit (Phase 4c): `catAll`/`sliceBV` extraction at the
    `offsetsOf` positions, segment extraction, slice reconstruction,
    and the `portSplit`/`detupleSizes`/`encTag`/`encCellRegs`/
    `encodeList` characterizations — the store-side accounting that
    connects `stateRel`'s encoding to the device's registers.
  * `repOkB` / `vty_rep_total` — representation totality: a canonical
    value at a `repOkB`-checked type `rep`s at the checked fuel (the
    per-(type, constructor) payload bound is discharged statically, so
    `portSplit` succeeds at the correspondence statement's own
    evaluation fuel; `repOkB` is checked by the validator on the
    output type).
  * `mkLayoutL_inv` / `mkPlan_inv` — everything the layout/plan
    acceptance guarantees (target/block links, argument sizes, the
    payload bound, the register partition into the tag register and
    per-cell runs).
  * `selectTAlt_char` — the terminator-selection loop (a `for` with
    early return) characterized as the structural `selSpec`.
  * `cstep_sound` — THE machine-step soundness theorem: the goto-fuel
    induction over the four compiler levels (`PCmds`/`PTerm`/`PAlts`/
    `PAlt1`). A compiled block body whose concrete execution
    (`Machine.runCmds` + `execBlock.runTerm`, at ANY evaluation and
    goto fuel) succeeds takes `EnvC`/`CellsC`-related environments to
    a record value `StepValC`-related to the step outcome: the out
    field is the emitted output's representation, the resumption-tag
    field is the next state's `encTag`, the cell fields are the next
    cells' representations, and a halt is unconstrained (`SimP` never
    inspects the right machine on a left halt).

Not yet delivered (Phase 4c remainder; nothing is stated with
`sorry` — the statements below are simply absent):

  * `goCmds`/`goTerm` width discipline (`VarsWF (WP σ)` of the
    compiled record, the mirror of `cexpJ_varsWF` at term level) —
    needed to consume `ceqB`'s width-aware (`cfoldW3`) leg, which is
    the leg most labels pass on.
  * `checkLabel_sound` — the per-label composition: `stateRel s t` +
    `HasTy` input ⟹ the `SimP.agree` body, by instantiating
    `cstep_sound` at the Γ₀/cells0 environments (built from the
    `encodeList` characterization and the `sigmaOf` union-bias facts,
    both proved here), comparing slices via `ceqB` + `substNF`/
    `tagSubst_eval` against `Bridge.symStep_sound`'s device step, and
    re-establishing `stateRel` from `StepValC` + `encodeList_intro`.
  * `validateProc_corresponds` — `validateProc = true →
    Rwv.Eidos.Corresponds` via `Rwv.stepObligations_corresponds`
    (`forAllM` inversion over the targets, `hasTy_vty` under the
    checked `tupleCtorsOk`, `mkFEnv_implements` under the checked
    definition-name distinctness, and `checkInit_sound` for `hinit`).
-/
import Rwv.Eidos.Cexp
import Rwv.Eidos.Machine
import Rwv.Eidos.FuelMono
import Rwv.Hyle.Bridge
import Rwv.Schema

namespace Rwv.Eidos.Cstep

open Std (HashMap)
open Rwv.Hyle (BV Op)
open Rwv.Hyle.Bridge (NF)
open Rwv.Eidos.Cexp (teq teqAll cexp catNF sliceNF denvOk VTy ctorOfB)

/-! ## Widths and offsets -/

/-- ToHyle's `wireOffsets`: the LSB offset of each component of an
MSB-first width list (component i sits above the sum of the widths
after it). -/
def offsetsOf (ws : List Nat) : List Nat :=
  (List.range ws.length).map fun i => (ws.drop (i + 1)).sum

/-! ## The layout (mkLayout, transcribed) -/

/-- A pause target: label unique, tag value (block order among pause
targets), and the saved parameters' types and widths (the block's
parameter telescope without the trailing resumed input). -/
structure LTarget where
  uniq   : Int
  tag    : Nat
  argTys : List Ty
  argWs  : List Nat
deriving Repr

/-- The step-record accounting for one process (ToHyle's `Layout`). -/
structure Layout where
  recW  : Nat                        -- total record width
  pTagW : Nat                        -- halted flag: 1 iff a halt is reachable
  outW  : Nat
  rTagW : Nat                        -- label tag width
  rPayW : Nat                        -- max summed pause-argument width
  cells : List (String × Ty × Nat)   -- cell names, types, widths, declaration order
  targets : List LTarget
  halts : List (Ty × Nat × Nat)      -- halt answer types: type, tag, width
  aTagW : Nat
  aPayW : Nat
deriving Repr

def Layout.rW (lo : Layout) : Nat := lo.rTagW + lo.rPayW
def Layout.cellsW (lo : Layout) : Nat := (lo.cells.map (·.2.2)).sum
def Layout.aW (lo : Layout) : Nat := lo.aTagW + lo.aPayW

mutual

/-- Pause-target label uniques of a terminator (mkLayout's `pt`). -/
def pausesOfTerm : Term → List Int
  | .pause _ l _ => [l.uniq]
  | .goto _ _    => []
  | .halt _      => []
  | .cases _ alts => pausesOfAlts alts

def pausesOfAlts : List TAlt → List Int
  | [] => []
  | .mk _ _ t :: rest => pausesOfTerm t ++ pausesOfAlts rest

end

mutual

/-- Halt-answer expressions of a terminator (mkLayout's `ht`). -/
def haltsOfTerm : Term → List Exp
  | .pause _ _ _ => []
  | .goto _ _    => []
  | .halt e      => [e]
  | .cases _ alts => haltsOfAlts alts

def haltsOfAlts : List TAlt → List Exp
  | [] => []
  | .mk _ _ t :: rest => haltsOfTerm t ++ haltsOfAlts rest

end

/-- A fueled expression-type synthesizer (the halt-answer `typeOf`;
join-free — halt answers are pure atoms in the ANF fragment). -/
def expTy (Δ : DEnv) : Nat → Exp → Except String Ty
  | 0, _ => throw "expTy: out of fuel"
  | fuel + 1, e =>
    match e with
    | .var x       => pure x.sig.ty
    | .con t _     => pure t
    | .prim t _    => pure t
    | .litInt t _  => pure t
    | .litStr _    => pure (.con "String")
    | .litList t _ => pure t
    | .litVec t _  => pure t
    | .lam x b     => do pure (.arrow x.sig.ty (← expTy Δ fuel b))
    | .letE _ body => expTy Δ fuel body
    | .jump _ _    => throw "expTy: jump (join fragment) in a halt answer"
    | .cases t _ _ _ => pure t
    | .app f a     => do
        let ft ← expTy Δ fuel f
        match a with
        | .tArg _ => throw "expTy: type application in a halt answer"
        | .eArg _ =>
            match ft with
            | .arrow _ t₂ => pure t₂
            | _ => throw "expTy: application of a non-arrow"

/-- Order-preserving first-occurrence dedup (mkLayout's `nubOrd` on
rendered type keys, here on structural type equality). -/
def firstOcc (ts : List Ty) : List Ty :=
  ts.foldl (fun acc t => if acc.any (· == t) then acc else acc ++ [t]) []

/-- ToHyle's `mkLayout`, transcribed: the step-record accounting. -/
def mkLayoutL (Δ : DEnv) (fuel : Nat) (p : Proc) : Except String Layout := do
  let outW ← Δ.sizeOf fuel [] p.outTy
  let cells ← p.cells.mapM fun c => do pure (c.name, c.ty, ← Δ.sizeOf fuel [] c.ty)
  let allBlocks := p.entry :: p.blocks.map (·.2)
  let pts := (allBlocks.map fun b => pausesOfTerm b.term).flatten
  let tblocks := p.blocks.filter fun (l, _) => pts.contains l.uniq
  let targets ← ((List.range tblocks.length).zip tblocks).mapM fun (i, (l, b)) => do
    let tys := (b.params.dropLast).map (·.sig.ty)
    let ws ← tys.mapM (Δ.sizeOf fuel [])
    pure { uniq := l.uniq, tag := i, argTys := tys, argWs := ws : LTarget }
  let haltEs := (allBlocks.map fun b => haltsOfTerm b.term).flatten
  let haltTys ← haltEs.mapM (expTy Δ fuel)
  let haltL := firstOcc haltTys
  let haltWs ← haltL.mapM (Δ.sizeOf fuel [])
  let halts := ((List.range haltL.length).zip (haltL.zip haltWs)).map
    fun (i, (t, w)) => (t, i, w)
  let rTagW := nbits targets.length
  let rPayW := (targets.map fun t => t.argWs.sum).foldl max 0
  let aTagW := nbits haltL.length
  let aPayW := haltWs.foldl max 0
  let pTagW := if haltL.isEmpty then 0 else 1
  let cellsW := (cells.map (·.2.2)).sum
  let pauseLoad := outW + rTagW + rPayW + cellsW
  let doneLoad  := aTagW + aPayW + cellsW
  let recW := pTagW + max pauseLoad (if haltL.isEmpty then 0 else doneLoad)
  pure { recW, pTagW, outW, rTagW, rPayW, cells, targets, halts, aTagW, aPayW }

/-! ## The register/port plan (mkDeviceM's wire accounting, checked
against the actual device) -/

/-- One state cell's register run: the device registers (in declared
order) that hold its `detupleSizes` components, MSB-first. -/
structure CellPlan where
  name  : String
  ty    : Ty
  width : Nat
  regs  : List (String × Nat)
deriving Repr

/-- The device-side accounting: the resumption-tag register (when
rW > 0), the per-cell register runs, and the port lists. -/
structure Plan where
  tagReg   : Option (String × Nat)
  cells    : List CellPlan
  inPorts  : List (String × Nat)
  outPorts : List (String × Nat)
deriving Repr

/-- Consume registers matching a width list exactly. -/
def takeRegs : List (String × Nat) → List Nat →
    Except String (List (String × Nat) × List (String × Nat))
  | regs, [] => pure ([], regs)
  | [], _ :: _ => throw "register plan: too few device registers for the state cells"
  | (r, rw) :: rest, w :: ws =>
      if rw = w then do
        let (run, rest') ← takeRegs rest ws
        pure ((r, rw) :: run, rest')
      else throw s!"register plan: register {r} has width {rw}, cell component needs {w}"

/-- One cell's register-run allocation (the body of `mkPlan`'s fold,
named so the inversion can speak about it). -/
def planCell (Δ : DEnv) (fuel : Nat) :
    List CellPlan × List (String × Nat) → String × Ty × Nat →
    Except String (List CellPlan × List (String × Nat))
  | (acc, rem), (nm, ty, w) => do
      let szs ← Val.detupleSizes Δ fuel ty
      if szs.sum == w then do
        let (run, rem') ← takeRegs rem szs
        pure (acc ++ [{ name := nm, ty, width := w, regs := run }], rem')
      else throw s!"cell {nm}: detupleSizes {szs} do not sum to {w}"

/-- Build and check the plan against the device interface. The
name-distinctness check comes first (`mkPlan_nodup` inverts it). -/
def mkPlan (Δ : DEnv) (fuel : Nat) (p : Proc) (lo : Layout) (dev : Rwv.Hyle.Device) :
    Except String Plan := do
  if Rwv.Hyle.Bridge.nodupB (dev.inputs.map (·.1) ++ dev.registers.map (·.name)) then
    let inSzs ← Val.detupleSizes Δ fuel p.inTy
    let outSzs ← Val.detupleSizes Δ fuel p.outTy
    if dev.inputs.map (·.2) == inSzs then
      if dev.outputs.map (·.2) == outSzs then
        if outSzs.sum == lo.outW then
          if Rwv.Hyle.Bridge.nodupB (lo.cells.map (·.1)) then do
            let (tagReg, stRegs) ←
              if lo.rW > 0 then
                match dev.registers.map fun r => (r.name, r.width) with
                | (r, w) :: rest =>
                    if w = lo.rW then pure (some (r, w), rest)
                    else throw s!"first register {r} has width {w}, resumption load is {lo.rW}"
                | [] => throw "no registers, but the resumption load is nonzero"
              else pure (none, dev.registers.map fun r => (r.name, r.width))
            let (cellsR, rest) ← lo.cells.foldlM (init := (([] : List CellPlan), stRegs))
              (planCell Δ fuel)
            if rest.isEmpty then
              pure { tagReg, cells := cellsR, inPorts := dev.inputs, outPorts := dev.outputs }
            else throw s!"unaccounted device registers: {rest.map (·.1)}"
          else throw "duplicate cell names"
        else throw s!"output port widths {outSzs} do not sum to the output width {lo.outW}"
      else throw s!"device outputs {dev.outputs.map (·.2)} ≠ detupleSizes(outTy) {outSzs}"
    else throw s!"device inputs {dev.inputs.map (·.2)} ≠ detupleSizes(inTy) {inSzs}"
  else throw "duplicate input/register names"

/-! ## The state encoding -/

/-- The resumption-tag register's value for a state:
`tag | zero pad | rep args`, width rW. -/
def encTag (lo : Layout) (tag : Nat) (argWs : List Nat) (reps : List BV) : BV :=
  Val.bvConcat (⟨lo.rTagW, BitVec.ofNat _ tag⟩ :: ⟨lo.rPayW - argWs.sum, 0⟩ :: reps)

/-- Split a cell's representation across its register run (MSB-first
consecutive slices). -/
def encCellRegs (regs : List (String × Nat)) (bv : BV) :
    Except String (List (String × BV)) :=
  if (regs.map (·.2)).sum == bv.width then
    pure ((regs.zip (offsetsOf (regs.map (·.2)))).map fun ((r, w), off) =>
      (r, ⟨w, bv.bits.extractLsb' off w⟩))
  else
    throw s!"cell registers {regs.map (·.2)} do not cover the representation ({bv.width})"

/-- One cell's contribution to the state encoding: its representation
split across its register run. -/
def encCellE (Δ : DEnv) (fuel : Nat) (s : MState) (c : CellPlan) :
    Except String (List (String × BV)) :=
  match s.cells.get? c.name with
  | none => throw s!"encode: missing cell {c.name}"
  | some v => do
      let bv ← Val.rep Δ fuel v
      if bv.width == c.width then encCellRegs c.regs bv
      else throw s!"encode: cell {c.name} rep width {bv.width} ≠ {c.width}"

/-- The state encoding, as an association list in device register
order: the resumption-tag register (when present) at
`tag | pad | rep args`, then each cell's `rep` split across its
register run. -/
def encodeList (Δ : DEnv) (fuel : Nat) (lo : Layout) (plan : Plan) (s : MState) :
    Except String (List (String × BV)) := do
  let tgt ← match lo.targets.find? (fun t => t.uniq == s.label) with
    | some tgt => pure tgt
    | none => throw "encode: state label is not a pause target"
  let reps ← s.args.mapM (Val.rep Δ fuel)
  if reps.map (·.width) == tgt.argWs then
    let tagPart : List (String × BV) := match plan.tagReg with
      | none => []
      | some (r, _) => [(r, encTag lo tgt.tag tgt.argWs reps)]
    let cellParts ← plan.cells.mapM (encCellE Δ fuel s)
    pure (tagPart ++ cellParts.flatten)
  else
    throw s!"encode: argument widths {reps.map (·.width)} ≠ layout {tgt.argWs}"

/-- The deliverable-shaped wrapper: the register store a related
device state must hold. -/
def encodeM (Δ : DEnv) (fuel : Nat) (lo : Layout) (plan : Plan) (s : MState) :
    Except String (HashMap String BV) :=
  (HashMap.ofList ·) <$> encodeList Δ fuel lo plan s

/-- The candidate state relation `R` of the step-obligation schema
(Rwv.StepObligations): the machine state is at a pause target whose
block exists with the right arity, its saved arguments and cells are
canonical (`VTy`) at their declared types, and the register store
agrees pointwise with the state's encoding. -/
def stateRel (Δ : DEnv) (lo : Layout) (plan : Plan) (blocks : HashMap Int Block)
    (s : MState) (t : HashMap String BV) : Prop :=
  (∃ tgt ∈ lo.targets, tgt.uniq = s.label) ∧
  (∃ blk, blocks.get? s.label = some blk ∧ s.args.length + 1 = blk.params.length ∧
    ∀ pr ∈ (blk.params.dropLast).zip s.args, VTy Δ pr.2 pr.1.sig.ty) ∧
  (∀ c ∈ plan.cells, ∃ v, s.cells.get? c.name = some v ∧ VTy Δ v c.ty) ∧
  (∃ k enc, encodeList Δ k lo plan s = .ok enc ∧ ∀ pr ∈ enc, t.get? pr.1 = some pr.2)

/-! ## The decidable canonicality check -/

/-- Decide `VTy` (value canonicality at a representable type). Fuel
bounds structural descent; exhaustion is a `false`, never an
unsoundness. -/
def vtyB (Δ : DEnv) : Nat → Val → Ty → Bool
  | 0, _, _ => false
  | fuel + 1, .vec es, t =>
      (match Ty.flatten t with
      | (.con "Vec", [n, te]) =>
          (match Ty.evalNat n with
          | some k => es.length == k && es.all fun e => vtyB Δ fuel e te
          | none => false)
      | _ => false)
  | _ + 1, .integer _, t =>
      (match Ty.flatten t with
      | (.con "Integer", []) => true
      | _ => false)
  | _ + 1, .finite b _, t =>
      (match Ty.flatten t with
      | (.con "Finite", [n]) => Ty.evalNat n == some b
      | _ => false)
  | _ + 1, .proxy, t =>
      (match Ty.flatten t with
      | (.con "Proxy", _) => true
      | _ => false)
  | fuel + 1, .con ty c fields, t =>
      teq ty t &&
      (match Δ.ctorSig.get? c with
      | some sig =>
          (match DEnv.matchTy (Ty.flattenArrow sig.ty).2 t with
          | .ok sub =>
              fields.length == (Ty.flattenArrow sig.ty).1.length &&
              ctorOfB Δ t c &&
              ((Ty.flattenArrow sig.ty).1.zip fields).all fun pr =>
                vtyB Δ fuel pr.2 (DEnv.substTv sub pr.1)
          | .error _ => false)
      | none => false)
  | _ + 1, .str _, _ => false
  | _ + 1, .closL _ _ _, _ => false
  | _ + 1, .closD _ _, _ => false

/-! ## Normal-form substitution (the tag specialization) -/

/-- Substitute normal forms for variables (`none` keeps the
variable). -/
def substNF (θ : String → Option NF) : NF → NF
  | .var w x => (θ x).getD (.var w x)
  | .lit v => .lit v
  | .prim1 op a => .prim1 op (substNF θ a)
  | .prim2 op a b => .prim2 op (substNF θ a) (substNF θ b)
  | .cat a b => .cat (substNF θ a) (substNF θ b)
  | .slice i w e => .slice i w (substNF θ e)
  | .ite c t e => .ite (substNF θ c) (substNF θ t) (substNF θ e)

/-- The label specialization: rewrite the resumption-tag register to
`tag-literal | its own low rPayW bits`. Denotation-preserving exactly
at valuations whose tag field is that literal (`substNF_eval`). -/
def tagSubst (plan : Plan) (lo : Layout) (tag : Nat) (x : String) : Option NF :=
  match plan.tagReg with
  | some (r, w) =>
      if 0 < lo.rTagW ∧ x = r then
        some (.cat (.lit ⟨lo.rTagW, BitVec.ofNat _ tag⟩)
                   (sliceNF 0 lo.rPayW (.var w r)))
      else none
  | none => none

/-! ## The symbolic machine-step compiler -/

/-- A symbolic cell store entry: the cell's current normal form. -/
structure CellNF where
  name  : String
  ty    : Ty
  width : Nat
  nf    : NF

/-- The static compilation context. -/
structure Ctx where
  Δ        : DEnv
  edm      : HashMap Int Defn
  lo       : Layout
  blocks   : HashMap Int Block
  cexpFuel : Nat
  outTy    : Ty

/-- The pause record (ToHyle's `buildPause`):
`halted=1 | pad | out | tag | pad | args | cells`, pads explicit
zeros, zero-width pieces dropped by `catNF`. -/
def pauseRec (C : Ctx) (onf : NF) (tgt : LTarget) (pas : List NF) (cells : List CellNF) : NF :=
  let lo := C.lo
  let padW := lo.recW - lo.pTagW - lo.outW - lo.rW - lo.cellsW
  let rPadW := lo.rPayW - tgt.argWs.sum
  catNF ([((.lit ⟨lo.pTagW, 1⟩ : NF), lo.pTagW),
          ((.lit ⟨padW, 0⟩ : NF), padW),
          (onf, lo.outW),
          ((.lit ⟨lo.rTagW, BitVec.ofNat _ tgt.tag⟩ : NF), lo.rTagW),
          ((.lit ⟨rPadW, 0⟩ : NF), rPadW)]
        ++ pas.zip tgt.argWs
        ++ cells.map fun c => (c.nf, c.width))

/-- The halt record (ToHyle's `buildHalt`):
`halted=0 | pad | answer-tag | pad | answer | cells`. -/
def haltRec (C : Ctx) (anf : NF) (atag aw : Nat) (cells : List CellNF) : NF :=
  let lo := C.lo
  let padW := lo.recW - lo.pTagW - lo.aW - lo.cellsW
  let aPadW := lo.aPayW - aw
  catNF ([((.lit ⟨lo.pTagW, 0⟩ : NF), lo.pTagW),
          ((.lit ⟨padW, 0⟩ : NF), padW),
          ((.lit ⟨lo.aTagW, BitVec.ofNat _ atag⟩ : NF), lo.aTagW),
          ((.lit ⟨aPadW, 0⟩ : NF), aPadW),
          (anf, aw)]
        ++ cells.map fun c => (c.nf, c.width))

mutual

/-- Compile a block body's commands, threading the symbolic
environment and cell store (the symbolic mirror of
`Machine.runCmds`), then the terminator. Every recursive call
consumes fuel (fuel-structural, like `cexp`). -/
def goCmds (C : Ctx) : Nat → HashMap Int (NF × Ty) → List CellNF → List Cmd → Term →
    Except String NF
  | 0, _, _, _, _ => throw "cstep: out of fuel"
  | fuel + 1, Γ, cells, [], term => goTerm C fuel Γ cells term
  | fuel + 1, Γ, cells, cmd :: rest, term =>
      match cmd with
      | .bind x e => do
          let nt ← Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ e
          goCmds C fuel (Γ.insert x.uniq nt) cells rest term
      | .get x c =>
          match cells.find? (fun d => d.name == c) with
          | some d => goCmds C fuel (Γ.insert x.uniq (d.nf, d.ty)) cells rest term
          | none => throw s!"cstep: get from unknown cell {c}"
      | .put c e => do
          let (nf, ty) ← Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ e
          match cells.find? (fun d => d.name == c) with
          | some d =>
              if teq ty d.ty then
                goCmds C fuel Γ
                  (cells.map fun d' => if d'.name == c then { d' with nf } else d')
                  rest term
              else throw s!"cstep: put to cell {c} at the wrong type"
          | none => throw s!"cstep: put to unknown cell {c}"

/-- Compile a terminator: pause/halt assemble records, goto chases the
block graph (the goto closure), terminator cases become tag-slice
if-chains. -/
def goTerm (C : Ctx) : Nat → HashMap Int (NF × Ty) → List CellNF → Term →
    Except String NF
  | 0, _, _, _ => throw "cstep: out of fuel"
  | fuel + 1, Γ, cells, term =>
      match term with
      | .pause out l args => do
          let (onf, oty) ← Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ out
          if teq oty C.outTy then
            match C.lo.targets.find? (fun t => t.uniq == l.uniq) with
            | none => throw s!"cstep: pause to an unknown target {l.occ}"
            | some tgt => do
                let pas ← args.mapM (Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ)
                if teqAll pas tgt.argTys then
                  pure (pauseRec C onf tgt (pas.map (·.1)) cells)
                else throw "cstep: pause argument type mismatch"
          else throw "cstep: pause output type mismatch"
      | .goto l args => do
          match C.blocks.get? l.uniq with
          | none => throw s!"cstep: goto to an unknown block {l.occ}"
          | some blk => do
              let pas ← args.mapM (Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ)
              if teqAll pas (blk.params.map (·.sig.ty)) then
                goCmds C fuel
                  ((blk.params.zip pas).foldl
                    (fun m (x, nt) => m.insert x.uniq nt) (∅ : HashMap Int (NF × Ty)))
                  cells blk.cmds blk.term
              else throw s!"cstep: goto {l.occ} argument mismatch"
      | .halt e => do
          let (anf, aty) ← Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ e
          let (atag, aw) ← match C.lo.halts.find? (fun h => h.1 == aty) with
            | some (_, tag, w) => pure (tag, w)
            | none => throw "cstep: halt at an unknown answer type"
          pure (haltRec C anf atag aw cells)
      | .cases scrut alts => do
          let (dn, dty) ← Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ scrut
          let szT ← C.Δ.sizeOf (C.cexpFuel + 1) [] dty
          match alts with
          | .mk .default bs dt :: rest =>
              if bs.isEmpty then do
                let els ← goTerm C fuel Γ cells dt
                goAlts C fuel Γ cells dty szT dn rest (some els)
              else throw "cstep: default alternative with binders"
          | rest => goAlts C fuel Γ cells dty szT dn rest none

/-- The terminator if-chain (the machine-level `Cexp.cchain`): right
fold with the default (when present) as the initial else; without one
the last alternative is unconditional. -/
def goAlts (C : Ctx) : Nat → HashMap Int (NF × Ty) → List CellNF → Ty → Nat → NF →
    List TAlt → Option NF → Except String NF
  | 0, _, _, _, _, _, _, _ => throw "cstep: out of fuel"
  | _ + 1, _, _, _, _, _, [], some els => pure els
  | _ + 1, _, _, _, _, _, [], none => throw "cstep: empty terminator case"
  | fuel + 1, Γ, cells, dty, szT, dn, [alt], none =>
      goAlt1 C fuel Γ cells dty szT dn alt none
  | fuel + 1, Γ, cells, dty, szT, dn, alt :: rest, macc => do
      let acc ← goAlts C fuel Γ cells dty szT dn rest macc
      goAlt1 C fuel Γ cells dty szT dn alt (some acc)

/-- The abstract bit-reading heads: types whose canonical values are
NOT constructor applications (`Val.integer`/`Val.finite`/`Val.proxy`).
A terminator DataAlt against such a head is rejected — the machine
semantics (`Machine.selectTAlt`) SKIPS a constructor pattern when the
scrutinee is not a constructor value, while the compiled tag test
could fire (`selectTAlt`'s skip has no expression-level counterpart:
`Eval.tryAlts` errors there instead). -/
def abstractHead (dty : Ty) : Bool :=
  match Ty.flatten dty with
  | (.con tc, _) => tc == "Integer" || tc == "Finite" || tc == "Proxy"
  | _ => false

/-- One terminator alternative (the machine-level `Cexp.cAlt`): a
DataAlt tests the tag slice and binds field slices at the wireOffsets
positions; a LitAlt compares the full atom (binder-free — the machine
semantics would bind a LitAlt's binders against a constructor
scrutinee's fields, which the compiled form does not model). -/
def goAlt1 (C : Ctx) : Nat → HashMap Int (NF × Ty) → List CellNF → Ty → Nat → NF →
    TAlt → Option NF → Except String NF
  | 0, _, _, _, _, _, _, _ => throw "cstep: out of fuel"
  | _ + 1, _, _, _, _, _, .mk .default _ _, _ =>
      throw "cstep: default alternative not first"
  | fuel + 1, Γ, cells, dty, szT, dn, .mk (.dataAlt cn) xs t, macc => do
      if abstractHead dty then
        throw "cstep: constructor pattern at an abstract bit-reading head" else
      if ctorOfB C.Δ dty cn then do
        let (tag, w) ← C.Δ.ctorTag dty cn
        match C.Δ.ctorSig.get? cn with
        | none => throw s!"cstep: unknown constructor {cn}"
        | some sig => do
            let sub ← DEnv.matchTy (Ty.flattenArrow sig.ty).2 dty
            let instTys := (Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)
            if xs.length = instTys.length then do
              let szXs ← instTys.mapM (C.Δ.sizeOf (C.cexpFuel + 1) [])
              if w + szXs.sum ≤ szT then do
                let offs := offsetsOf szXs
                let slices := (szXs.zip offs).map fun (sz, off) => sliceNF off sz dn
                let Γ' := (xs.zip (slices.zip instTys)).foldl
                  (fun m (x, nt) => m.insert x.uniq nt) Γ
                let bnf ← goTerm C fuel Γ' cells t
                match macc, w with
                | some acc, _ + 1 =>
                    pure (.ite (.prim2 .eq (sliceNF (szT - w) w dn)
                                           (.lit ⟨w, BitVec.ofNat w tag⟩))
                               bnf acc)
                | _, _ => pure bnf
              else throw s!"cstep: constructor {cn} wider than the discriminant"
            else throw s!"cstep: constructor {cn} binder arity mismatch"
      else throw s!"cstep: constructor {cn} does not belong to the discriminant type"
  | fuel + 1, Γ, cells, _dty, szT, dn, .mk (.litAlt i) bs t, macc =>
      if bs.isEmpty then do
        let bnf ← goTerm C fuel Γ cells t
        match macc with
        | some acc =>
            pure (.ite (.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT i⟩)) bnf acc)
        | none => pure bnf
      else throw "cstep: literal alternative with binders"

end

/-! ## The per-label check -/

/-- Both verified comparison legs: syntactic equality after `cfold`
(unconditional) or after the width-aware `cfoldW3`. -/
def ceqB (a b : NF) : Bool :=
  a.cfold == b.cfold || Rwv.Hyle.Bridge.cfoldW3 a == Rwv.Hyle.Bridge.cfoldW3 b

/-- The initial symbolic cell store: each cell is the concatenation of
its register run. -/
def cells0 (plan : Plan) : List CellNF :=
  plan.cells.map fun c =>
    { name := c.name, ty := c.ty, width := c.width
      nf := catNF (c.regs.map fun (r, w) => ((.var w r : NF), w)) }

/-- One output-port comparison of `checkLabel`. -/
def checkOut (_C : Ctx) (θ : String → Option NF) (rec : NF) (tag : Nat)
    (pr : ((String × Nat) × Nat) × (String × NF)) : Except String Unit :=
  if pr.1.1.1 == pr.2.1 then
    if ceqB (sliceNF pr.1.2 pr.1.1.2 rec) (substNF θ pr.2.2) then pure ()
    else throw s!"label tag {tag}: output {pr.1.1.1} disagrees"
  else throw s!"checkLabel: output order drift ({pr.1.1.1} vs {pr.2.1})"

/-- One register comparison of `checkLabel`. -/
def checkReg (_C : Ctx) (θ : String → Option NF) (rec : NF) (tag : Nat)
    (pr : (Rwv.Hyle.Register × Nat) × (String × NF)) : Except String Unit :=
  if pr.1.1.name == pr.2.1 then
    if ceqB (sliceNF pr.1.2 pr.1.1.width rec) (substNF θ pr.2.2) then pure ()
    else throw s!"label tag {tag}: register {pr.1.1.name} disagrees"
  else throw s!"checkLabel: register order drift ({pr.1.1.name} vs {pr.2.1})"

/-- Pointwise checks along a list (`List.forM`, named so the inversion
can speak about it). -/
def forAllM {α : Type} (f : α → Except String Unit) : List α → Except String Unit
  | [] => pure ()
  | x :: xs => do
      let _ ← f x
      forAllM f xs

/-- The per-label obligation: compile the machine step from pause
target `tgt` into a record normal form over the device's registers
and inputs, and compare — output for output, register for register —
against the device's symbolic step specialized to `tgt`'s tag. -/
def checkLabel (C : Ctx) (plan : Plan) (dev : Rwv.Hyle.Device)
    (ss : Rwv.Hyle.Bridge.StepNF) (inTy : Ty) (fuel : Nat) (tgt : LTarget) :
    Except String Unit := do
  let blk ← match C.blocks.get? tgt.uniq with
    | some blk => pure blk
    | none => throw s!"checkLabel: no block for target {tgt.uniq}"
  if blk.params.length == tgt.argWs.length + 1 then
    match blk.params.getLast? with
    | none => throw "checkLabel: parameterless pause target"
    | some inP =>
      if teq inP.sig.ty inTy then do
        let tagVar : NF := match plan.tagReg with
          | some (r, w) => .var w r
          | none => .lit BV.nil
        let argNFs := (tgt.argWs.zip (offsetsOf tgt.argWs)).map fun (w, off) =>
          sliceNF off w tagVar
        let inNF := catNF (plan.inPorts.map fun (x, w) => ((.var w x : NF), w))
        let Γ₀ := (blk.params.zip ((argNFs ++ [inNF]).zip (tgt.argTys ++ [inTy]))).foldl
          (fun m (x, nt) => m.insert x.uniq nt) (∅ : HashMap Int (NF × Ty))
        let rec ← goCmds C fuel Γ₀ (cells0 plan) blk.cmds blk.term
        let θ := tagSubst plan C.lo tgt.tag
        -- Outputs: the out field sits above the resumption load and cells.
        let outOffs := (offsetsOf (dev.outputs.map (·.2))).map (· + C.lo.rW + C.lo.cellsW)
        let _ ← forAllM (checkOut C θ rec tgt.tag) ((dev.outputs.zip outOffs).zip ss.outs)
        -- Registers: tag register above the cells, cells at the LSB end.
        let regOffs := offsetsOf (dev.registers.map (·.width))
        forAllM (checkReg C θ rec tgt.tag) ((dev.registers.zip regOffs).zip ss.nexts)
      else throw "checkLabel: resumed-input parameter is not at the process input type"
  else throw "checkLabel: block arity does not match the layout target"

/-! ## The initial-state check -/

/-- The decidable core of the initial-state obligation: the post-reset
machine state is at a pause target whose block exists at the right
arity, its saved arguments and cells are canonical (`vtyB`), and its
encoding is exactly the device's declared register initials, in
register order. -/
def initStateOk (Δ : DEnv) (lo : Layout) (plan : Plan) (blocks : HashMap Int Block)
    (dev : Rwv.Hyle.Device) (vfuel : Nat) (s₀ : MState) : Bool :=
  (lo.targets.find? fun t => t.uniq == s₀.label).isSome &&
  (match blocks.get? s₀.label with
   | some blk =>
       s₀.args.length + 1 == blk.params.length &&
       ((blk.params.dropLast).zip s₀.args).all fun pr => vtyB Δ vfuel pr.2 pr.1.sig.ty
   | none => false) &&
  (plan.cells.all fun c =>
    match s₀.cells.get? c.name with
    | some v => vtyB Δ vfuel v c.ty
    | none => false) &&
  (match encodeList Δ vfuel lo plan s₀ with
   | .ok enc => decide (enc = dev.registers.map fun r => (r.name, r.init))
   | .error _ => false)

/-- The initial-state obligation: run the committed machine semantics
concretely (cell initialization + the entry block to its first
pause), check the post-reset state's canonicality, encode it, and
compare against the declared register initials. An entry that HALTS
passes vacuously (the schema's initial hypothesis is about `.step`
outcomes, and fuel determinism rules out a `.step` at any other
fuel). -/
def checkInit (C : Ctx) (plan : Plan) (dev : Rwv.Hyle.Device) (p : Proc)
    (ef gf vfuel : Nat) : Except String Unit := do
  let σ₀ ← Machine.initCells C.Δ C.edm ef p
  match ← Machine.execBlock C.Δ C.edm C.blocks ef gf [] σ₀ p.entry with
  | .halt _ => pure ()
  | .step _ s₀ =>
      if initStateOk C.Δ C.lo plan C.blocks dev vfuel s₀ then pure ()
      else throw "checkInit: post-reset state check failed (canonicality or initials)"

/-! ## The whole-process validator -/

/-- The prim-basis tuple discipline: every tuple datatype is declared
with its single eponymous constructor. -/
def tupleCtorsOk (Δ : DEnv) : Bool :=
  Δ.ctors.toList.all fun pr => !Ty.isTupleCon pr.1 || pr.2.all (· == pr.1)

/-- The whole-process validator, with a diagnostic message on
failure: layout, plan, the device's symbolic step, every pause
target's per-label obligation, and the initial-state obligation.
The up-front environment checks (`denvOk`, the tuple discipline,
distinct Hyle definition names, distinct block-label uniques) are
what the soundness theorem's environment lemmas consume. -/
def validateProcE (Δ : DEnv) (edm : HashMap Int Defn) (p : Proc)
    (H : Rwv.Hyle.Program) (fuel : Nat) : Except String Unit := do
  unless denvOk Δ do throw "validateProc: denvOk failed"
  unless tupleCtorsOk Δ do throw "validateProc: tuple constructor discipline failed"
  unless Rwv.Hyle.Bridge.nodupB (H.defns.map (·.name)) do
    throw "validateProc: duplicate Hyle definition names"
  unless Rwv.Eidos.Cexp.nodupIntB (p.blocks.map (·.1.uniq)) do
    throw "validateProc: duplicate block-label uniques"
  let lo ← mkLayoutL Δ fuel p
  let plan ← mkPlan Δ fuel p lo H.device
  let blocks : HashMap Int Block :=
    HashMap.ofList (p.blocks.map fun (l, b) => (l.uniq, b))
  let C : Ctx := { Δ, edm, lo, blocks, cexpFuel := fuel, outTy := p.outTy }
  let ss ← Rwv.Hyle.Bridge.symStep (Rwv.Hyle.Bridge.dmapOf H)
    (Rwv.Hyle.Bridge.progFuel H) H.device
  let _ ← forAllM (checkLabel C plan H.device ss p.inTy fuel) lo.targets
  checkInit C plan H.device p fuel fuel fuel

/-- The Boolean validator (the shape a soundness statement quantifies
over). -/
def validateProc (Δ : DEnv) (edm : HashMap Int Defn) (p : Proc)
    (H : Rwv.Hyle.Program) (fuel : Nat) : Bool :=
  match validateProcE Δ edm p H fuel with
  | .ok _ => true
  | .error _ => false

/-! ## Local `Except`/list helpers (house style: re-proved) -/

private theorem error_ne_ok {α : Type} {msg : String} {a : α} {P : Prop}
    (h : (Except.error msg : Except String α) = .ok a) : P := by
  cases h

private theorem except_bind_eq_ok {α β : Type} {x : Except String α}
    {f : α → Except String β} {b : β} (h : (x >>= f) = .ok b) :
    ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x with
  | error e => exact error_ne_ok h
  | ok a => exact ⟨a, rfl, h⟩

/-- `nodupB` on a suffix. -/
private theorem nodupB_append_right {xs ys : List String}
    (h : Rwv.Hyle.Bridge.nodupB (xs ++ ys) = true) :
    Rwv.Hyle.Bridge.nodupB ys = true := by
  induction xs with
  | nil => simpa using h
  | cons x xs ih =>
      rw [List.cons_append, Rwv.Hyle.Bridge.nodupB] at h
      simp only [Bool.and_eq_true] at h
      exact ih h.2

/-- `nodupB` gives beq-pairwise-distinctness (the shape
`HashMap.getElem?_ofList_of_mem` consumes). -/
private theorem nodupB_pairwise {l : List String}
    (h : Rwv.Hyle.Bridge.nodupB l = true) :
    l.Pairwise fun a b => (a == b) = false := by
  induction l with
  | nil => exact List.Pairwise.nil
  | cons x xs ih =>
      rw [Rwv.Hyle.Bridge.nodupB] at h
      simp only [Bool.and_eq_true, Bool.not_eq_true'] at h
      obtain ⟨hc, hrest⟩ := h
      refine List.Pairwise.cons ?_ (ih hrest)
      intro b hb
      cases hxb : x == b with
      | false => rfl
      | true =>
          have hx : x = b := eq_of_beq hxb
          subst hx
          have hmem : xs.contains x = true := by simpa using hb
          rw [hmem] at hc
          cases hc

/-- Inversion of a passed `unless`. -/
private theorem unless_eq_ok {c : Prop} [Decidable c] {m : String} {u : Unit}
    (h : (if c then pure () else throw m : Except String Unit) = .ok u) : c := by
  split at h
  · assumption
  · exact error_ne_ok h

/-- A passing plan checked the input/register name discipline — the
hypothesis `initStateOk_sound`/`checkInit_sound` consume. -/
theorem mkPlan_nodup {Δ : DEnv} {fuel : Nat} {p : Proc} {lo : Layout}
    {dev : Rwv.Hyle.Device} {plan : Plan}
    (h : mkPlan Δ fuel p lo dev = .ok plan) :
    Rwv.Hyle.Bridge.nodupB
      (dev.inputs.map (·.1) ++ dev.registers.map (·.name)) = true := by
  rw [mkPlan] at h
  split at h
  · assumption
  · exact error_ne_ok h

/-! ## Soundness of the decidable canonicality check -/

/-- `ctorOfB` decides `Cexp.ctorOf` (Cexp's identical lemma is private
there). -/
private theorem ctorOfB_sound {Δ : DEnv} {t : Ty} {c : String}
    (h : ctorOfB Δ t c = true) : Rwv.Eidos.Cexp.ctorOf Δ t c := by
  rw [ctorOfB] at h
  rw [Rwv.Eidos.Cexp.ctorOf]
  split at h
  · rename_i tc args _heq
    by_cases htup : Ty.isTupleCon tc
    · rw [if_pos htup]
      rw [if_pos htup] at h
      exact eq_of_beq h
    · rw [if_neg htup]
      rw [if_neg htup] at h
      cases hcs : Δ.ctors.get? tc with
      | none => rw [hcs] at h; cases h
      | some cs =>
          rw [hcs] at h
          exact ⟨cs, rfl, by simpa using h⟩
  · cases h

/-- The decidable canonicality check is sound: `vtyB … = true` implies
`Cexp.VTy`. -/
theorem vtyB_sound {Δ : DEnv} : ∀ (fuel : Nat) {v : Val} {t : Ty},
    vtyB Δ fuel v t = true → VTy Δ v t := by
  intro fuel
  induction fuel with
  | zero => intro v t h; rw [vtyB] at h; cases h
  | succ fuel ih =>
      intro v t h
      cases v with
      | vec es =>
          rw [vtyB] at h
          split at h
          · rename_i n te heq
            split at h
            · rename_i k hn
              simp only [Bool.and_eq_true, beq_iff_eq] at h
              exact VTy.vec heq hn h.1 fun e he =>
                ih (List.all_eq_true.mp h.2 e he)
            · cases h
          · cases h
      | integer x =>
          rw [vtyB] at h
          split at h
          · rename_i heq
            exact VTy.integer heq
          · cases h
      | finite b i =>
          rw [vtyB] at h
          split at h
          · rename_i n heq
            exact VTy.finite heq (by simpa using h)
          · cases h
      | proxy =>
          rw [vtyB] at h
          split at h
          · rename_i args heq
            exact VTy.proxy heq rfl
          · cases h
      | str s => rw [vtyB] at h; cases h
      | closL x env body => rw [vtyB] at h; cases h
      | closD f pre => rw [vtyB] at h; cases h
      | con ty c fields =>
          rw [vtyB] at h
          simp only [Bool.and_eq_true] at h
          obtain ⟨hty, h⟩ := h
          have hty' : ty = t := Rwv.Eidos.Cexp.teq_eq hty
          subst hty'
          split at h
          · rename_i sig hsig
            split at h
            · rename_i sub hsub
              simp only [Bool.and_eq_true, beq_iff_eq] at h
              obtain ⟨⟨hlen, hctor⟩, hall⟩ := h
              exact VTy.con hsig hsub hlen (ctorOfB_sound hctor)
                fun pr hpr => ih (List.all_eq_true.mp hall pr hpr)
            · cases h
          · cases h

/-! ## Fuel determinism (via Rwv.Eidos.FuelMono) -/

private theorem initCells_det {Δ : DEnv} {edm : HashMap Int Defn} {p : Proc}
    {a b : Nat} {x y : HashMap String Val}
    (h₁ : Machine.initCells Δ edm a p = .ok x)
    (h₂ : Machine.initCells Δ edm b p = .ok y) : x = y := by
  have k₁ := Machine.initCells_mono (Nat.le_max_left a b) h₁
  have k₂ := Machine.initCells_mono (Nat.le_max_right a b) h₂
  exact Except.ok.inj (k₁.symm.trans k₂)

private theorem execBlock_det {Δ : DEnv} {edm : HashMap Int Defn}
    {blocks : HashMap Int Block} {ef₁ gf₁ ef₂ gf₂ : Nat} {env : Eval.Env}
    {cells : HashMap String Val} {b : Block} {r₁ r₂ : StepOut}
    (h₁ : Machine.execBlock Δ edm blocks ef₁ gf₁ env cells b = .ok r₁)
    (h₂ : Machine.execBlock Δ edm blocks ef₂ gf₂ env cells b = .ok r₂) : r₁ = r₂ := by
  have k₁ := Machine.execBlock_mono (Nat.le_max_left ef₁ ef₂) (Nat.le_max_left gf₁ gf₂) h₁
  have k₂ := Machine.execBlock_mono (Nat.le_max_right ef₁ ef₂) (Nat.le_max_right gf₁ gf₂) h₂
  exact Except.ok.inj (k₁.symm.trans k₂)

/-! ## THE initial-state theorem -/

/-- A passing decidable core establishes the state relation against
the declared register initials. The name-distinctness hypothesis is
`mkPlan`'s check (a passing plan supplies it). -/
theorem initStateOk_sound {Δ : DEnv} {lo : Layout} {plan : Plan}
    {blocks : HashMap Int Block} {dev : Rwv.Hyle.Device} {vf : Nat} {s : MState}
    (hnodup : Rwv.Hyle.Bridge.nodupB
      (dev.inputs.map (·.1) ++ dev.registers.map (·.name)) = true)
    (h : initStateOk Δ lo plan blocks dev vf s = true) :
    stateRel Δ lo plan blocks s (Rwv.Hyle.Sem.initRegs dev) := by
  rw [initStateOk] at h
  simp only [Bool.and_eq_true] at h
  obtain ⟨⟨⟨hfind, hblk⟩, hcells⟩, henc⟩ := h
  refine ⟨?_, ?_, ?_, ?_⟩
  · -- The label is a pause target.
    obtain ⟨tgt, htgt⟩ := Option.isSome_iff_exists.mp hfind
    exact ⟨tgt, List.mem_of_find?_eq_some htgt, by simpa using List.find?_some htgt⟩
  · -- The block exists, at the right arity, with canonical arguments.
    cases hb : blocks.get? s.label with
    | none => rw [hb] at hblk; cases hblk
    | some blk =>
        rw [hb] at hblk
        simp only [Bool.and_eq_true, beq_iff_eq] at hblk
        exact ⟨blk, rfl, hblk.1, fun pr hpr =>
          vtyB_sound vf (List.all_eq_true.mp hblk.2 pr hpr)⟩
  · -- The cells are present and canonical.
    intro c hc
    have hc' := List.all_eq_true.mp hcells c hc
    cases hv : s.cells.get? c.name with
    | none => rw [hv] at hc'; cases hc'
    | some v =>
        rw [hv] at hc'
        exact ⟨v, rfl, vtyB_sound vf hc'⟩
  · -- The encoding agrees with the declared initials, pointwise.
    cases he : encodeList Δ vf lo plan s with
    | error e => rw [he] at henc; cases henc
    | ok enc =>
        rw [he] at henc
        have henc' : enc = dev.registers.map fun r => (r.name, r.init) :=
          of_decide_eq_true henc
        refine ⟨vf, enc, he, ?_⟩
        intro pr hpr
        rw [henc'] at hpr
        have hdist : (dev.registers.map fun r => (r.name, r.init)).Pairwise
            (fun a b => (a.1 == b.1) = false) := by
          have h₁ : (dev.registers.map (·.name)).Pairwise
              (fun a b => (a == b) = false) :=
            nodupB_pairwise (nodupB_append_right hnodup)
          have h₂ : dev.registers.Pairwise
              (fun a b => (a.name == b.name) = false) :=
            (List.pairwise_map).mp h₁
          exact (List.pairwise_map).mpr (by
            refine h₂.imp ?_
            intro a b hab
            simpa using hab)
        rw [Rwv.Hyle.Sem.initRegs, HashMap.get?_eq_getElem?]
        exact HashMap.getElem?_ofList_of_mem (k_beq := beq_self_eq_true pr.1) hdist hpr

set_option linter.unusedVariables false in
/-- THE initial-state theorem: a passing `checkInit` discharges the
`hinit` hypothesis of `Rwv.stepObligations_corresponds` for
`R := stateRel …` — for EVERY evaluation and goto fuel of the
hypothesis (the check ran at its own fuels; `Rwv.Eidos.FuelMono`'s
monotonicity makes any two successful runs agree). An entry block that
halts at the checker's fuels makes the hypothesis vacuous the same
way. -/
theorem checkInit_sound {C : Ctx} {plan : Plan} {dev : Rwv.Hyle.Device} {p : Proc}
    {ef gf vf : Nat}
    (hnodup : Rwv.Hyle.Bridge.nodupB
      (dev.inputs.map (·.1) ++ dev.registers.map (·.name)) = true)
    (hck : checkInit C plan dev p ef gf vf = .ok ()) :
    ∀ (ef' gf' : Nat) (σ₀ : HashMap String Val) (o : Val) (s₀ : MState),
      Machine.initCells C.Δ C.edm ef' p = .ok σ₀ →
      Machine.execBlock C.Δ C.edm C.blocks ef' gf' [] σ₀ p.entry = .ok (.step o s₀) →
      stateRel C.Δ C.lo plan C.blocks s₀ (Rwv.Hyle.Sem.initRegs dev) := by
  intro ef' gf' σ₀ o s₀ hinitC hexecC
  rw [checkInit] at hck
  obtain ⟨σv, hσv, hck⟩ := except_bind_eq_ok hck
  have hσeq : σv = σ₀ := initCells_det hσv hinitC
  subst hσeq
  obtain ⟨so, hso, hck⟩ := except_bind_eq_ok hck
  cases so with
  | halt a =>
      have hcontra := execBlock_det hso hexecC
      cases hcontra
  | step o' s₀' =>
      have heq := execBlock_det hso hexecC
      injection heq with heqO heqS
      subst heqS
      dsimp only at hck
      split at hck
      · rename_i hok
        exact initStateOk_sound hnodup hok
      · exact error_ne_ok hck

/-! ## The tag-specialization layers (for the step half) -/

/-- Substitution is denotation-preserving at valuations that fix every
substituted image. -/
theorem substNF_eval {σ : String → BV} {θ : String → Option NF}
    (hθ : ∀ x n, θ x = some n → n.eval σ = σ x) :
    ∀ nf : NF, (substNF θ nf).eval σ = nf.eval σ := by
  intro nf
  induction nf with
  | var w x =>
      simp only [substNF]
      cases hx : θ x with
      | none => rfl
      | some n => simpa [NF.eval] using hθ x n hx
  | lit v => rfl
  | prim1 op a iha => simp only [substNF, NF.eval, iha]
  | prim2 op a b iha ihb => simp only [substNF, NF.eval, iha, ihb]
  | cat a b iha ihb =>
      -- BV's dependent `++` blocks rewriting under the structure
      -- literal; present the clause as a two-argument function of
      -- whole bundled values first (the recorded house trap).
      show (fun u v : BV => (⟨u.width + v.width, u.bits ++ v.bits⟩ : BV))
          ((substNF θ a).eval σ) ((substNF θ b).eval σ)
        = (fun u v : BV => (⟨u.width + v.width, u.bits ++ v.bits⟩ : BV))
          (a.eval σ) (b.eval σ)
      rw [iha, ihb]
  | slice i w e ihe =>
      show (fun u : BV => (⟨w, u.bits.extractLsb' i w⟩ : BV)) ((substNF θ e).eval σ)
        = (fun u : BV => (⟨w, u.bits.extractLsb' i w⟩ : BV)) (e.eval σ)
      rw [ihe]
  | ite c t e ihc iht ihe => simp only [substNF, NF.eval, ihc, iht, ihe]

/-- Substitution preserves the width discipline when every substituted
image satisfies it. -/
theorem substNF_varsWF {P : String → Nat → Prop} {θ : String → Option NF}
    (hθ : ∀ x n, θ x = some n → n.VarsWF P) :
    ∀ {nf : NF}, nf.VarsWF P → (substNF θ nf).VarsWF P := by
  intro nf
  induction nf with
  | var w x =>
      intro h
      simp only [substNF]
      cases hx : θ x with
      | none => exact h
      | some n => simpa using hθ x n hx
  | lit v => intro h; exact h
  | prim1 op a iha => exact fun h => iha h
  | prim2 op a b iha ihb => exact fun h => ⟨iha h.1, ihb h.2⟩
  | cat a b iha ihb => exact fun h => ⟨iha h.1, ihb h.2⟩
  | slice i w e ihe => exact fun h => ihe h
  | ite c t e ihc iht ihe => exact fun h => ⟨ihc h.1, iht h.2.1, ihe h.2.2⟩

/-- The tag specialization is denotation-preserving at valuations
whose tag-register value is fixed by the specialization image (the
`stateRel` stores: the register holds `tag | pad | args` with the
target's own tag). -/
theorem tagSubst_eval {σ : String → BV} {plan : Plan} {lo : Layout} {tag : Nat}
    (hfix : ∀ r w, plan.tagReg = some (r, w) → 0 < lo.rTagW →
      (NF.cat (.lit ⟨lo.rTagW, BitVec.ofNat _ tag⟩)
        (sliceNF 0 lo.rPayW (.var w r))).eval σ = σ r) :
    ∀ nf, (substNF (tagSubst plan lo tag) nf).eval σ = nf.eval σ := by
  refine substNF_eval ?_
  intro x n hx
  rw [tagSubst] at hx
  split at hx
  · rename_i r w heq
    split at hx
    · rename_i hcond
      injection hx with hx
      subst hx
      rw [hcond.2]
      exact hfix r w heq hcond.1
    · cases hx
  · cases hx

/-- BV extensionality via `getLsbD` (Bridge's `bv_eq_of`, private
there). -/
private theorem bv_eq_ext {x y : BV} (hw : x.width = y.width)
    (hb : ∀ i, i < x.width → x.bits.getLsbD i = y.bits.getLsbD i) : x = y := by
  cases x with | mk wx bx =>
  cases y with | mk wy byy =>
  dsimp only at hw hb
  subst hw
  exact congrArg (BV.mk wx) (BitVec.eq_of_getLsbD_eq_iff.mpr hb)

/-- The generic store-side discharge of `tagSubst_eval`'s fix
condition: a register whose value has the tag literal in its top
`wt` bits is fixed by the `tag-literal | own low bits`
specialization image. (The `stateRel` stores satisfy the hypothesis
by `encTag`'s construction — connecting the two is the step-half
accounting.) -/
theorem tagFix_of_store {σ : String → BV} {r : String} {w wt wl tag : Nat}
    (hw : (σ r).width = wt + wl)
    (htop : ∀ j, j < wt →
      (σ r).bits.getLsbD (wl + j) = (BitVec.ofNat wt tag).getLsbD j) :
    (NF.cat (.lit ⟨wt, BitVec.ofNat wt tag⟩)
      (sliceNF 0 wl (.var w r))).eval σ = σ r := by
  have hslice : (sliceNF 0 wl (.var w r)).eval σ
      = ⟨wl, (σ r).bits.extractLsb' 0 wl⟩ := by
    rw [sliceNF]
    split
    · rename_i h0
      subst h0
      refine bv_eq_ext rfl ?_
      intro i hi
      cases hi
    · rfl
  show (fun u v : BV => (⟨u.width + v.width, u.bits ++ v.bits⟩ : BV))
      ((NF.lit ⟨wt, BitVec.ofNat wt tag⟩).eval σ) ((sliceNF 0 wl (.var w r)).eval σ)
    = σ r
  rw [hslice]
  show (⟨wt + wl, BitVec.ofNat wt tag ++ (σ r).bits.extractLsb' 0 wl⟩ : BV) = σ r
  refine bv_eq_ext (by simpa using hw.symm) ?_
  intro i hi
  rw [BitVec.getLsbD_append]
  by_cases hil : i < wl
  · rw [if_pos hil, BitVec.getLsbD_extractLsb']
    simp [hil]
  · rw [if_neg hil]
    have h2 := htop (i - wl) (by simp at hi; omega)
    rw [show wl + (i - wl) = i by omega] at h2
    exact h2.symm

/-! ## HasTy → VTy (the schema's input predicate, compiler-side)

`Rwv.StepObligations` restricts inputs to `Val.HasTy Δ · p.inTy` (the
hypothesis `Corresponds` supplies); the compiler-side machinery works
with `Cexp.VTy`. `HasTy` is the stronger statement except on the
tuple family, where its constructor-membership clause does not by
itself pin the constructor name to the (tagless) tuple head — the
prim-basis discipline `tupleCtorsOk` closes that gap. -/

private theorem tupleCtorsOk_sound {Δ : DEnv} (h : tupleCtorsOk Δ = true) :
    ∀ tc cs, Δ.ctors.get? tc = some cs → Ty.isTupleCon tc = true →
      ∀ c ∈ cs, c = tc := by
  intro tc cs hget htup c hc
  have hmem : (tc, cs) ∈ Δ.ctors.toList := by
    rw [HashMap.mem_toList_iff_getElem?_eq_some, ← HashMap.get?_eq_getElem?]
    exact hget
  have := List.all_eq_true.mp h _ hmem
  simp only [htup, Bool.not_true, Bool.false_or] at this
  exact eq_of_beq (List.all_eq_true.mp this c hc)

/-- Pair-flipping a zip membership through a map (the field-typing
orientations of `HasTy.con` and `VTy.con`). -/
private theorem zip_swap_mem {α β γ : Type} (f : α → γ) :
    ∀ (tys : List α) (fields : List β) (p : α × β), p ∈ tys.zip fields →
      (p.2, f p.1) ∈ fields.zip (tys.map f) := by
  intro tys
  induction tys with
  | nil => intro fields p hp; cases hp
  | cons a as ih =>
      intro fields p hp
      cases fields with
      | nil => cases hp
      | cons b bs =>
          rcases List.mem_cons.mp hp with hp | hp
          · subst hp
            exact List.mem_cons_self
          · exact List.mem_cons_of_mem _ (ih bs p hp)

/-- `Val.HasTy` implies `Cexp.VTy`, under the prim-basis tuple
discipline: the conversion the final glue applies to the
`Corresponds`-supplied input canonicality. -/
theorem hasTy_vty {Δ : DEnv} (htup : tupleCtorsOk Δ = true) :
    ∀ {v : Val} {t : Ty}, Val.HasTy Δ v t → VTy Δ v t := by
  intro v t h
  induction h with
  | vec hty hn hlen _helems ih => exact VTy.vec hty hn hlen fun e he => ih e he
  | finite hty hn _hlt => exact VTy.finite hty hn
  | integer hty => exact VTy.integer hty
  | proxy hty => exact VTy.proxy hty rfl
  | @con t tc c fields sig sub doms hty hctor hsig hmatch hdoms hlen _hfields ih =>
      refine VTy.con hsig hmatch ?_ ?_ ?_
      · rw [hlen, hdoms, List.length_map]
      · -- ctorOf: rebuild the flattened head from `(Ty.flatten t).1`.
        have hfl : Ty.flatten t = (.con tc, (Ty.flatten t).2) := by
          cases hf : Ty.flatten t with
          | mk hd args =>
              rw [hf] at hty
              dsimp only at hty
              rw [hty]
        rw [Rwv.Eidos.Cexp.ctorOf, hfl]
        dsimp only
        obtain ⟨cs, hget, hcmem⟩ := hctor
        by_cases ht : Ty.isTupleCon tc
        · rw [if_pos ht]
          exact tupleCtorsOk_sound htup tc cs hget ht c hcmem
        · rw [if_neg ht]
          exact ⟨cs, hget, hcmem⟩
      · intro q hq
        have hq' : (q.2, DEnv.substTv sub q.1) ∈ fields.zip doms := by
          rw [hdoms]
          exact zip_swap_mem (DEnv.substTv sub) _ fields q hq
        exact ih (q.2, DEnv.substTv sub q.1) hq'

/-! ## The BV concatenation/slice kit (house style: projection level)

Everything at widths and `getLsbD`, per the bridge's recorded
proof-engineering discipline (`BV` bundles the width, so the dependent
`++` blocks rewriting once widths move into the bits' type).
Re-proved locally: Cexp's identical kit is private there. -/

private theorem bv_ext {x y : BV} (hw : x.width = y.width)
    (h : ∀ i, x.bits.getLsbD i = y.bits.getLsbD i) : x = y := by
  cases x with | mk wx bx =>
  cases y with | mk wy bv =>
  dsimp only at hw h
  subst hw
  exact congrArg (BV.mk wx) (BitVec.eq_of_getLsbD_eq fun i _ => h i)

/-- The bundled concatenation (left operand at the MSB end). -/
private def bvCat (a b : BV) : BV := ⟨a.width + b.width, a.bits ++ b.bits⟩

private theorem bvCat_getLsbD (a b : BV) (i : Nat) :
    (bvCat a b).bits.getLsbD i =
      if i < b.width then b.bits.getLsbD i else a.bits.getLsbD (i - b.width) := by
  simp [bvCat, BitVec.getLsbD_append]

private theorem getLsbD_ge {n : Nat} (x : BitVec n) {i : Nat} (h : n ≤ i) :
    x.getLsbD i = false :=
  x.getLsbD_of_ge i h

private theorem bvCat_width (a b : BV) : (bvCat a b).width = a.width + b.width := rfl

private theorem bvCat_zero_left {a b : BV} (h : a.width = 0) : bvCat a b = b := by
  refine bv_ext (by rw [bvCat_width, h, Nat.zero_add]) ?_
  intro i
  rw [bvCat_getLsbD]
  by_cases hi : i < b.width
  · rw [if_pos hi]
  · rw [if_neg hi, getLsbD_ge a.bits (by omega), getLsbD_ge b.bits (by omega)]

private theorem bvCat_zero_right {a b : BV} (h : b.width = 0) : bvCat a b = a := by
  refine bv_ext (by rw [bvCat_width, h, Nat.add_zero]) ?_
  intro i
  rw [bvCat_getLsbD, if_neg (by omega), h, Nat.sub_zero]

private theorem bvCat_assoc (a b c : BV) : bvCat (bvCat a b) c = bvCat a (bvCat b c) := by
  refine bv_ext (by simp only [bvCat_width]; omega) ?_
  intro i
  rw [bvCat_getLsbD (bvCat a b) c i, bvCat_getLsbD a (bvCat b c) i]
  rcases Nat.lt_or_ge i c.width with hc | hc
  · rw [if_pos hc, if_pos (show i < (bvCat b c).width by rw [bvCat_width]; omega),
        bvCat_getLsbD b c i, if_pos hc]
  · rcases Nat.lt_or_ge (i - c.width) b.width with hb | hb
    · rw [if_neg (by omega), bvCat_getLsbD a b (i - c.width), if_pos hb,
          if_pos (show i < (bvCat b c).width by rw [bvCat_width]; omega),
          bvCat_getLsbD b c i, if_neg (by omega)]
    · rw [if_neg (by omega), bvCat_getLsbD a b (i - c.width), if_neg (by omega),
          if_neg (show ¬ i < (bvCat b c).width by rw [bvCat_width]; omega), bvCat_width]
      congr 1
      omega

/-- `Val.bvConcat` as a fold of the bundled concatenation. -/
private def catAll (xs : List BV) : BV := xs.foldl bvCat BV.nil

private theorem bvConcat_eq (xs : List BV) : Val.bvConcat xs = catAll xs := rfl

private theorem foldl_bvCat (l : List BV) :
    ∀ acc, l.foldl bvCat acc = bvCat acc (catAll l) := by
  induction l with
  | nil => intro acc; exact (bvCat_zero_right rfl).symm
  | cons y ys ih =>
      intro acc
      rw [List.foldl_cons, ih (bvCat acc y), bvCat_assoc]
      congr 1
      simp only [catAll, List.foldl_cons]
      rw [show bvCat BV.nil y = y from bvCat_zero_left rfl, ih y]
      simp only [catAll]

private theorem catAll_cons (x : BV) (xs : List BV) :
    catAll (x :: xs) = bvCat x (catAll xs) := by
  simp only [catAll, List.foldl_cons]
  rw [show bvCat BV.nil x = x from bvCat_zero_left rfl]
  exact foldl_bvCat xs x

private theorem catAll_nil : catAll [] = BV.nil := rfl

private theorem catAll_append (xs ys : List BV) :
    catAll (xs ++ ys) = bvCat (catAll xs) (catAll ys) := by
  induction xs with
  | nil => rw [List.nil_append, catAll_nil, bvCat_zero_left rfl]
  | cons x xs ih =>
      rw [List.cons_append, catAll_cons, ih, catAll_cons, bvCat_assoc]

private theorem catAll_width (xs : List BV) :
    (catAll xs).width = (xs.map (·.width)).sum := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
      rw [catAll_cons, bvCat_width, ih, List.map_cons, List.sum_cons]

/-- The bundled slice (LSB offset, width). -/
private def sliceBV (x : BV) (i w : Nat) : BV := ⟨w, x.bits.extractLsb' i w⟩

private theorem sliceBV_width (x : BV) (i w : Nat) : (sliceBV x i w).width = w := rfl

private theorem sliceBV_getLsbD (x : BV) (i w j : Nat) :
    (sliceBV x i w).bits.getLsbD j = (decide (j < w) && x.bits.getLsbD (i + j)) := by
  simp [sliceBV, BitVec.getLsbD_extractLsb']

private theorem sliceBV_cat_low {a b : BV} {i w : Nat} (h : i + w ≤ b.width) :
    sliceBV (bvCat a b) i w = sliceBV b i w := by
  refine bv_ext rfl ?_
  intro j
  rw [sliceBV_getLsbD, sliceBV_getLsbD]
  by_cases hj : j < w
  · simp only [decide_eq_true hj, Bool.true_and]
    rw [bvCat_getLsbD, if_pos (by omega)]
  · simp [hj]

private theorem sliceBV_cat_high {a b : BV} {i w : Nat} (h : b.width ≤ i) :
    sliceBV (bvCat a b) i w = sliceBV a (i - b.width) w := by
  refine bv_ext rfl ?_
  intro j
  rw [sliceBV_getLsbD, sliceBV_getLsbD]
  by_cases hj : j < w
  · simp only [decide_eq_true hj, Bool.true_and]
    rw [bvCat_getLsbD, if_neg (by omega)]
    congr 1
    omega
  · simp [hj]

private theorem sliceBV_all (x : BV) : sliceBV x 0 x.width = x := by
  refine bv_ext rfl ?_
  intro j
  rw [sliceBV_getLsbD]
  by_cases hj : j < x.width
  · simp [hj]
  · simp only [decide_eq_false (by omega : ¬ j < x.width), Bool.false_and]
    rw [getLsbD_ge x.bits (by omega)]

/-- Slice of a slice: composition at absolute offsets. -/
private theorem sliceBV_sliceBV {x : BV} {a wA b wB : Nat} (h : b + wB ≤ wA) :
    sliceBV (sliceBV x a wA) b wB = sliceBV x (a + b) wB := by
  refine bv_ext rfl ?_
  intro j
  rw [sliceBV_getLsbD, sliceBV_getLsbD, sliceBV_getLsbD]
  by_cases hj : j < wB
  · simp only [decide_eq_true hj, Bool.true_and]
    rw [decide_eq_true (by omega : b + j < wA), Bool.true_and,
        Nat.add_assoc]
  · simp [hj]

/-- The extraction workhorse: slicing a concatenation at a piece's
position yields the piece. -/
private theorem catAll_extract (pre post : List BV) (x : BV) :
    sliceBV (catAll (pre ++ x :: post)) ((post.map (·.width)).sum) x.width = x := by
  rw [catAll_append, catAll_cons]
  rw [sliceBV_cat_low (by rw [bvCat_width, catAll_width]; omega)]
  rw [sliceBV_cat_high (Nat.le_of_eq (catAll_width post))]
  rw [catAll_width, Nat.sub_self]
  exact sliceBV_all x

/-- Segment extraction: slicing a concatenation across a contiguous
run of pieces yields the run's concatenation. -/
private theorem catAll_extract_seg (pre mid post : List BV) :
    sliceBV (catAll (pre ++ mid ++ post)) ((post.map (·.width)).sum)
        ((mid.map (·.width)).sum) = catAll mid := by
  rw [List.append_assoc, catAll_append, catAll_append]
  rw [sliceBV_cat_low (by
    rw [bvCat_width, catAll_width, catAll_width]
    omega)]
  rw [sliceBV_cat_high (Nat.le_of_eq (catAll_width post))]
  rw [catAll_width post, Nat.sub_self, ← catAll_width mid]
  exact sliceBV_all _

/-! ## `offsetsOf` accounting -/

private theorem offsetsOf_length (ws : List Nat) : (offsetsOf ws).length = ws.length := by
  simp [offsetsOf]

private theorem offsetsOf_getElem (ws : List Nat) (i : Nat) (hi : i < ws.length) :
    (offsetsOf ws)[i]'(by rw [offsetsOf_length]; exact hi) = (ws.drop (i + 1)).sum := by
  simp [offsetsOf]

private theorem offsetsOf_cons (w : Nat) (ws : List Nat) :
    offsetsOf (w :: ws) = ws.sum :: offsetsOf ws := by
  refine List.ext_getElem (by simp [offsetsOf_length]) ?_
  intro i h1 h2
  match i with
  | 0 => rw [offsetsOf_getElem _ 0 (by simp)]; simp
  | i + 1 =>
      rw [offsetsOf_getElem _ (i + 1) (by simpa [offsetsOf_length] using h1)]
      simp only [List.drop_succ_cons, List.getElem_cons_succ]
      rw [offsetsOf_getElem ws i (by simpa [offsetsOf_length] using h2)]

private theorem offsetsOf_append (ws₁ ws₂ : List Nat) :
    offsetsOf (ws₁ ++ ws₂)
      = (offsetsOf ws₁).map (· + ws₂.sum) ++ offsetsOf ws₂ := by
  induction ws₁ with
  | nil => simp [offsetsOf]
  | cons w ws ih =>
      rw [List.cons_append, offsetsOf_cons, offsetsOf_cons, ih, List.map_cons,
          List.cons_append, List.sum_append]

/-- Per-index extraction at the `offsetsOf` positions. -/
private theorem catAll_extract_idx (xs : List BV) (i : Nat) (hi : i < xs.length) :
    sliceBV (catAll xs)
        ((offsetsOf (xs.map (·.width)))[i]'(by
          rw [offsetsOf_length, List.length_map]; exact hi))
        (xs[i].width) = xs[i] := by
  have hsplit : xs.take i ++ xs[i] :: xs.drop (i + 1) = xs := by
    rw [List.getElem_cons_drop, List.take_append_drop]
  rw [offsetsOf_getElem _ i (by rw [List.length_map]; exact hi), ← List.map_drop,
      show catAll xs = catAll (xs.take i ++ xs[i] :: xs.drop (i + 1)) from
        (congrArg catAll hsplit).symm]
  exact catAll_extract (xs.take i) (xs.drop (i + 1)) xs[i]

/-- Concatenating the `offsetsOf`-positioned slices of any vector
reconstructs its low `ws.sum` bits. -/
private theorem catAll_slices (ws : List Nat) (bv : BV) :
    catAll ((ws.zip (offsetsOf ws)).map fun p => sliceBV bv p.2 p.1)
      = sliceBV bv 0 ws.sum := by
  induction ws with
  | nil =>
      refine bv_ext rfl ?_
      intro j
      rw [show (([] : List Nat).zip (offsetsOf [])).map (fun p => sliceBV bv p.2 p.1)
            = [] from rfl, catAll_nil, sliceBV_getLsbD]
      simp [BV.nil, BV.ofNat]
  | cons w ws ih =>
      rw [offsetsOf_cons, List.zip_cons_cons, List.map_cons, catAll_cons, ih]
      refine bv_ext (by simp only [bvCat_width, sliceBV_width, List.sum_cons]) ?_
      intro j
      rw [bvCat_getLsbD]
      by_cases hj : j < ws.sum
      · rw [if_pos (show j < (sliceBV bv 0 ws.sum).width from hj), sliceBV_getLsbD,
            sliceBV_getLsbD, decide_eq_true hj, Bool.true_and,
            decide_eq_true (show j < (w :: ws).sum by simp only [List.sum_cons]; omega),
            Bool.true_and]
      · rw [if_neg (show ¬ j < (sliceBV bv 0 ws.sum).width from hj)]
        show (sliceBV bv ws.sum w).bits.getLsbD (j - ws.sum) = _
        rw [sliceBV_getLsbD, sliceBV_getLsbD]
        by_cases hjw : j - ws.sum < w
        · rw [decide_eq_true hjw, Bool.true_and,
              decide_eq_true (show j < (w :: ws).sum by simp only [List.sum_cons]; omega),
              Bool.true_and]
          congr 1
          omega
        · rw [decide_eq_false hjw, Bool.false_and,
              decide_eq_false (show ¬ j < (w :: ws).sum by simp only [List.sum_cons]; omega),
              Bool.false_and]

private theorem drop_sum_le {l : List Nat} {i : Nat} (h : i < l.length) :
    (l.drop (i + 1)).sum + l[i] ≤ l.sum := by
  have hsplit : l.sum = (l.take i).sum + (l[i] :: l.drop (i + 1)).sum := by
    rw [List.getElem_cons_drop h, ← List.sum_append, List.take_append_drop]
  rw [hsplit, List.sum_cons]
  omega

/-! ## Evaluation of the construction helpers -/

private theorem catList_eval (σ : String → BV) :
    ∀ (xs : List NF), (Rwv.Eidos.Cexp.catList xs).eval σ = catAll (xs.map (NF.eval σ)) := by
  intro xs
  match xs with
  | [] => rfl
  | [x] =>
      show x.eval σ = catAll [x.eval σ]
      rw [catAll_cons, catAll_nil, bvCat_zero_right rfl]
  | x :: y :: rest =>
      show bvCat (x.eval σ) ((Rwv.Eidos.Cexp.catList (y :: rest)).eval σ) = _
      rw [catList_eval σ (y :: rest)]
      rw [show (x :: y :: rest).map (NF.eval σ) = x.eval σ :: (y :: rest).map (NF.eval σ)
            from rfl, catAll_cons]

private theorem catNF_eval (σ : String → BV)
    (xs : List (NF × Nat)) (hw : ∀ p ∈ xs, (p.1.eval σ).width = p.2) :
    (catNF xs).eval σ = catAll (xs.map (fun p => p.1.eval σ)) := by
  rw [catNF, catList_eval, List.map_map]
  induction xs with
  | nil => rfl
  | cons x rest ih =>
      rw [List.filter_cons]
      by_cases hx : (x.2 != 0) = true
      · rw [if_pos hx, List.map_cons, List.map_cons, catAll_cons, catAll_cons,
            ih (fun a ha => hw a (List.mem_cons_of_mem _ ha))]
        rfl
      · rw [if_neg hx, List.map_cons, catAll_cons,
            ih (fun a ha => hw a (List.mem_cons_of_mem _ ha))]
        refine (bvCat_zero_left ?_).symm
        show (x.1.eval σ).width = 0
        rw [hw x List.mem_cons_self]
        simpa using hx

private theorem sliceNF_eval (σ : String → BV) (off w : Nat) (e : NF) :
    (sliceNF off w e).eval σ = sliceBV (e.eval σ) off w := by
  rw [sliceNF]
  by_cases hw : w = 0
  · rw [if_pos hw]
    subst hw
    refine bv_ext rfl ?_
    intro i
    rw [sliceBV_getLsbD]
    simp [NF.eval, BV.nil, BV.ofNat]
  · rw [if_neg hw]
    rfl

/-! ## Traversal and determinism helpers (re-proved; Cexp's are private) -/

private theorem except_pure_def {α : Type} (a : α) :
    (pure a : Except String α) = .ok a := rfl

private theorem except_bind_ok {α β : Type} (a : α) (f : α → Except String β) :
    (Except.ok a >>= f) = f a := rfl

private theorem mapM_attach_erase {α β : Type} (f : α → Except String β) :
    ∀ (as : List α), as.attach.mapM (fun ⟨a, _⟩ => f a) = as.mapM f := by
  intro as
  induction as with
  | nil => rfl
  | cons a as ih =>
      simp only [List.attach_cons, List.mapM_cons, List.mapM_map]
      rw [show ((fun (x : {x // x ∈ a :: as}) => f x.val) ∘
            fun (x : {x // x ∈ as}) => (⟨x.val, by simp [x.property]⟩ : {x // x ∈ a :: as}))
          = fun (x : {x // x ∈ as}) => f x.val from rfl]
      rw [ih]

/-- Inversion of a successful `mapM`: pointwise successes, aligned by
index. -/
private theorem mapM_ok_idx {α β : Type} {g : α → Except String β} :
    ∀ {xs : List α} {ys : List β}, xs.mapM g = .ok ys →
      ys.length = xs.length ∧
      ∀ i (hi : i < xs.length), ∃ (hy : i < ys.length), g xs[i] = .ok ys[i] := by
  intro xs
  induction xs with
  | nil =>
      intro ys h
      rw [List.mapM_nil, except_pure_def] at h
      injection h with h
      subst h
      exact ⟨rfl, fun i hi => absurd hi (by simp)⟩
  | cons a as ih =>
      intro ys h
      rw [List.mapM_cons] at h
      obtain ⟨b, hb, h⟩ := except_bind_eq_ok h
      obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
      rw [except_pure_def] at h
      injection h with h
      subst h
      obtain ⟨hlen, hpt⟩ := ih hbs
      refine ⟨by simp [hlen], ?_⟩
      intro i hi
      match i with
      | 0 => exact ⟨by simp, by simpa using hb⟩
      | i + 1 =>
          obtain ⟨hy, hgi⟩ := hpt i (by simpa using hi)
          exact ⟨by simpa using hy, by simpa using hgi⟩

/-- Construction of a successful `mapM` from pointwise successes. -/
private theorem mapM_total_idx {α β : Type} {g : α → Except String β} :
    ∀ {xs : List α}, (∀ i (hi : i < xs.length), ∃ b, g xs[i] = .ok b) →
      ∃ bs, xs.mapM g = .ok bs ∧ bs.length = xs.length ∧
        ∀ i (hi : i < xs.length), ∃ (hb : i < bs.length), g xs[i] = .ok bs[i] := by
  intro xs
  induction xs with
  | nil => intro _; exact ⟨[], rfl, rfl, fun i hi => absurd hi (by simp)⟩
  | cons a as ih =>
      intro h
      obtain ⟨b, hb⟩ := h 0 (by simp)
      obtain ⟨bs, hbs, hlen, hpt⟩ := ih (fun i hi => by
        have := h (i + 1) (by simpa using hi)
        simpa using this)
      refine ⟨b :: bs, ?_, by simp [hlen], ?_⟩
      · rw [show g (a :: as)[0] = g a from rfl] at hb
        rw [List.mapM_cons, hb, except_bind_ok, hbs, except_bind_ok]
        rfl
      · intro i hi
        match i with
        | 0 => exact ⟨by simp, by simpa using hb⟩
        | i + 1 =>
            obtain ⟨hbi, hgi⟩ := hpt i (by simpa using hi)
            exact ⟨by simpa using hbi, by simpa using hgi⟩

private theorem sum_const {c : Nat} : ∀ {l : List Nat}, (∀ a ∈ l, a = c) →
    l.sum = l.length * c := by
  intro l
  induction l with
  | nil => intro _; simp
  | cons a as ih =>
      intro h
      rw [List.sum_cons, h a List.mem_cons_self,
          ih (fun a ha => h a (List.mem_cons_of_mem _ ha)), List.length_cons, Nat.succ_mul]
      omega

private theorem sizeOf_det {Δ : DEnv} {k k' : Nat} {vis : List Ty} {t : Ty} {a b : Nat}
    (h : Δ.sizeOf k vis t = .ok a) (h' : Δ.sizeOf k' vis t = .ok b) : a = b :=
  Except.ok.inj ((Δ.sizeOf_mono (Nat.le_max_left k k') h).symm.trans
    (Δ.sizeOf_mono (Nat.le_max_right k k') h'))

private theorem rep_det {Δ : DEnv} {k k' : Nat} {v : Val} {a b : BV}
    (h : Val.rep Δ k v = .ok a) (h' : Val.rep Δ k' v = .ok b) : a = b :=
  Except.ok.inj ((Val.rep_mono Δ (Nat.le_max_left k k') h).symm.trans
    (Val.rep_mono Δ (Nat.le_max_right k k') h'))

private theorem mapM_rep_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k') :
    ∀ {vs : List Val} {bs : List BV}, vs.mapM (Val.rep Δ k) = .ok bs →
      vs.mapM (Val.rep Δ k') = .ok bs := by
  intro vs
  induction vs with
  | nil => intro bs h; simpa using h
  | cons v vv ih =>
      intro bs h
      rw [List.mapM_cons] at h ⊢
      obtain ⟨b, hb, h⟩ := except_bind_eq_ok h
      obtain ⟨bs', hbs, h⟩ := except_bind_eq_ok h
      rw [Val.rep_mono Δ hk hb, except_bind_ok, ih hbs, except_bind_ok]
      exact h

private theorem mapM_rep_exists {Δ : DEnv} :
    ∀ {vs : List Val} {bs : List BV}, vs.length = bs.length →
      (∀ i (h1 : i < vs.length) (h2 : i < bs.length), ∃ k, Val.rep Δ k vs[i] = .ok bs[i]) →
      ∃ K, vs.mapM (Val.rep Δ K) = .ok bs := by
  intro vs
  induction vs with
  | nil =>
      intro bs hl _
      match bs, hl with
      | [], _ => exact ⟨0, rfl⟩
  | cons v vv ih =>
      intro bs hl hpt
      match bs, hl with
      | b :: bs', hl =>
          obtain ⟨k0, hk0⟩ := hpt 0 (by simp) (by simp)
          obtain ⟨K, hK⟩ := ih (by simpa using hl)
            (fun i h1 h2 => by
              have := hpt (i + 1) (by simpa using h1) (by simpa using h2)
              simpa using this)
          refine ⟨max k0 K, ?_⟩
          rw [List.mapM_cons,
              Val.rep_mono Δ (Nat.le_max_left k0 K) (by simpa using hk0), except_bind_ok,
              mapM_rep_mono (Nat.le_max_right k0 K) hK, except_bind_ok]
          rfl

/-! ## Tag arithmetic and `sizeOf` inversions (re-proved) -/

private theorem nbits_le (n : Nat) : n ≤ 2 ^ nbits n := by
  rw [nbits]
  by_cases h : n ≤ 1
  · rw [if_pos h]; simpa using h
  · rw [if_neg h]
    have := @Nat.lt_log2_self (n - 1)
    omega

private theorem idxOf?_getElem? {l : List String} {c : String} {i : Nat}
    (h : l.idxOf? c = some i) : l[i]? = some c := by
  have hp := List.of_findIdx?_eq_some (p := fun x => x == c) h
  cases hx : l[i]? with
  | none => rw [hx] at hp; exact absurd hp (by simp)
  | some a =>
      rw [hx] at hp
      simp only [beq_iff_eq] at hp
      rw [hp]

private theorem idxOf?_lt {l : List String} {c : String} {i : Nat}
    (h : l.idxOf? c = some i) : i < l.length :=
  List.getElem?_eq_some_iff.mp (idxOf?_getElem? h) |>.1

private theorem idxOf?_inj {l : List String} {c c' : String} {i : Nat}
    (h : l.idxOf? c = some i) (h' : l.idxOf? c' = some i) : c = c' := by
  have := (idxOf?_getElem? h).symm.trans (idxOf?_getElem? h')
  exact Option.some.inj this

/-- Inversion of `DEnv.ctorTag`. -/
private theorem ctorTag_inv {Δ : DEnv} {t : Ty} {c : String} {tag w : Nat}
    (h : Δ.ctorTag t c = .ok (tag, w)) :
    ∃ tc args, Ty.flatten t = (.con tc, args) ∧
      ((Ty.isTupleCon tc = true ∧ tag = 0 ∧ w = 0) ∨
       (Ty.isTupleCon tc = false ∧ ∃ cs, Δ.ctors.get? tc = some cs ∧
         cs.idxOf? c = some tag ∧ w = nbits cs.length)) := by
  rw [DEnv.ctorTag] at h
  rcases hfl : Ty.flatten t with ⟨th, args⟩
  rw [hfl] at h
  cases th with
  | con tc =>
      dsimp only at h
      refine ⟨tc, args, rfl, ?_⟩
      by_cases htup : Ty.isTupleCon tc
      · rw [if_pos htup] at h
        injection h with h
        injection h with h1 h2
        exact .inl ⟨htup, h1.symm, h2.symm⟩
      · rw [if_neg htup] at h
        cases hcs : Δ.ctors.get? tc with
        | none => rw [hcs] at h; exact error_ne_ok h
        | some cs =>
            rw [hcs] at h
            dsimp only at h
            cases hidx : cs.idxOf? c with
            | none => rw [hidx] at h; exact error_ne_ok h
            | some idx =>
                rw [hidx] at h
                dsimp only at h
                injection h with h
                injection h with h1 h2
                exact .inr ⟨by simp [htup], cs, rfl, by rw [hidx, h1], h2.symm⟩
  | app t₁ t₂ => exact error_ne_ok h
  | var a => exact error_ne_ok h
  | nat n => exact error_ne_ok h
  | arrow t₁ t₂ => exact error_ne_ok h

private theorem sizeOf_inv_vec {Δ : DEnv} {k : Nat} {vis : List Ty} {t n te : Ty} {w : Nat}
    (hfl : Ty.flatten t = (.con "Vec", [n, te]))
    (h : Δ.sizeOf (k + 1) vis t = .ok w) :
    ∃ kk we, Ty.evalNat n = some kk ∧ Δ.sizeOf k vis te = .ok we ∧ w = kk * we := by
  rw [DEnv.sizeOf] at h
  split at h
  case h_1 =>
      rename_i nn tee heq
      rw [hfl] at heq
      have hpair : n = nn ∧ te = tee := by simpa using heq
      obtain ⟨h1, h2⟩ := hpair
      subst h1; subst h2
      split at h
      · rename_i kk hkk
        obtain ⟨we, hwe, h⟩ := except_bind_eq_ok h
        injection h with h
        exact ⟨kk, we, hkk, hwe, h.symm⟩
      · exact error_ne_ok h
  case h_2 => rename_i nn heq; rw [hfl] at heq; simp at heq
  case h_3 => rename_i heq; rw [hfl] at heq; simp at heq
  case h_4 => rename_i snd heq; rw [hfl] at heq; simp at heq
  case h_5 =>
      rename_i c args hvec hfin hint hprox heq
      rw [hfl] at heq
      have hp : "Vec" = c ∧ [n, te] = args := by simpa using heq
      exact (hvec n te hp.2.symm hp.1.symm).elim
  case h_6 => rename_i a snd heq; rw [hfl] at heq; simp at heq
  case h_7 => exact error_ne_ok h

private theorem sizeOf_inv_finite {Δ : DEnv} {k : Nat} {vis : List Ty} {t n : Ty} {w : Nat}
    (hfl : Ty.flatten t = (.con "Finite", [n]))
    (h : Δ.sizeOf (k + 1) vis t = .ok w) :
    ∃ kk, Ty.evalNat n = some kk ∧ w = nbits kk := by
  rw [DEnv.sizeOf] at h
  split at h
  case h_1 => rename_i nn tee heq; rw [hfl] at heq; simp at heq
  case h_2 =>
      rename_i nn heq
      rw [hfl] at heq
      have hpair : n = nn := by simpa using heq
      subst hpair
      split at h
      · rename_i kk hkk
        injection h with h
        exact ⟨kk, hkk, h.symm⟩
      · exact error_ne_ok h
  case h_3 => rename_i heq; rw [hfl] at heq; simp at heq
  case h_4 => rename_i snd heq; rw [hfl] at heq; simp at heq
  case h_5 =>
      rename_i c args hvec hfin hint hprox heq
      rw [hfl] at heq
      have hp : "Finite" = c ∧ [n] = args := by simpa using heq
      exact (hfin n hp.2.symm hp.1.symm).elim
  case h_6 => rename_i a snd heq; rw [hfl] at heq; simp at heq
  case h_7 => exact error_ne_ok h

private theorem sizeOf_inv_integer {Δ : DEnv} {k : Nat} {vis : List Ty} {t : Ty} {w : Nat}
    (hfl : Ty.flatten t = (.con "Integer", []))
    (h : Δ.sizeOf (k + 1) vis t = .ok w) : w = 128 := by
  rw [DEnv.sizeOf] at h
  split at h
  case h_1 => rename_i nn tee heq; rw [hfl] at heq; simp at heq
  case h_2 => rename_i nn heq; rw [hfl] at heq; simp at heq
  case h_3 =>
      injection h with h
      exact h.symm
  case h_4 => rename_i snd heq; rw [hfl] at heq; simp at heq
  case h_5 =>
      rename_i c args hvec hfin hint hprox heq
      rw [hfl] at heq
      have hp : "Integer" = c ∧ ([] : List Ty) = args := by simpa using heq
      exact (hint hp.2.symm hp.1.symm).elim
  case h_6 => rename_i a snd heq; rw [hfl] at heq; simp at heq
  case h_7 => exact error_ne_ok h

private theorem sizeOf_inv_proxy {Δ : DEnv} {k : Nat} {vis : List Ty} {t : Ty}
    {args : List Ty} {w : Nat} (hfl : Ty.flatten t = (.con "Proxy", args))
    (h : Δ.sizeOf (k + 1) vis t = .ok w) : w = 0 := by
  rw [DEnv.sizeOf] at h
  split at h
  case h_1 => rename_i nn tee heq; rw [hfl] at heq; simp at heq
  case h_2 => rename_i nn heq; rw [hfl] at heq; simp at heq
  case h_3 => rename_i heq; rw [hfl] at heq; simp at heq
  case h_4 =>
      injection h with h
      exact h.symm
  case h_5 =>
      rename_i c args' hvec hfin hint hprox heq
      rw [hfl] at heq
      have hp : "Proxy" = c ∧ args = args' := by simpa using heq
      exact (hprox hp.1.symm).elim
  case h_6 => rename_i a snd heq; rw [hfl] at heq; simp at heq
  case h_7 => exact error_ne_ok h

/-! ## Representation structure and width from canonicality (re-proved) -/

private theorem bvConcat_nil : Val.bvConcat [] = BV.nil := rfl

private theorem bvConcat3 (a b c : BV) :
    Val.bvConcat [a, b, c] = bvCat a (bvCat b c) := by
  rw [bvConcat_eq, catAll_cons, catAll_cons, catAll_cons, catAll_nil,
      show bvCat c BV.nil = c from bvCat_zero_right rfl]

/-- Inversion of `Val.rep` at a constructor value. -/
private theorem rep_con_inv {Δ : DEnv} {k : Nat} {ty : Ty} {c : String}
    {fields : List Val} {bv : BV} (h : Val.rep Δ k (.con ty c fields) = .ok bv) :
    ∃ k' whole tag tagW bs, k = k' + 1 ∧
      Δ.sizeOf (k' + 1) [] ty = .ok whole ∧
      Δ.ctorTag ty c = .ok (tag, tagW) ∧
      fields.mapM (Val.rep Δ k') = .ok bs ∧
      tagW + (Val.bvConcat bs).width ≤ whole ∧
      bv = Val.bvConcat [⟨tagW, BitVec.ofNat _ tag⟩,
        ⟨whole - tagW - (Val.bvConcat bs).width, 0⟩, Val.bvConcat bs] := by
  cases k with
  | zero => rw [Val.rep] at h; exact error_ne_ok h
  | succ k' =>
      rw [Val.rep] at h
      obtain ⟨whole, hwhole, h⟩ := except_bind_eq_ok h
      obtain ⟨tg, htg, h⟩ := except_bind_eq_ok h
      obtain ⟨tag, tagW⟩ := tg
      obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
      rw [mapM_attach_erase] at hbs
      dsimp only at h
      split at h
      · rename_i hle
        injection h with h
        exact ⟨k', whole, tag, tagW, bs, rfl, hwhole, htg, hbs, hle, h.symm⟩
      · exact error_ne_ok h

/-- The representation width of a canonical value is its type's
`sizeOf` (Cexp's private `vty_rep_width`, re-proved). -/
private theorem vty_rep_width {Δ : DEnv} :
    ∀ {v : Val} {t : Ty}, VTy Δ v t → ∀ {k : Nat} {bv : BV}, Val.rep Δ k v = .ok bv →
      ∀ {k' : Nat} {w : Nat}, Δ.sizeOf k' [] t = .ok w → bv.width = w := by
  intro v t hv
  induction hv with
  | vec hfl hn hlen helems ih =>
      rename_i elems t n te kk
      intro k bv hrep k' w hsz
      cases k with
      | zero => rw [Val.rep] at hrep; exact error_ne_ok hrep
      | succ k =>
          rw [Val.rep] at hrep
          obtain ⟨bs, hbs, hrep⟩ := except_bind_eq_ok hrep
          rw [mapM_attach_erase] at hbs
          injection hrep with hrep
          subst hrep
          cases k' with
          | zero => rw [DEnv.sizeOf] at hsz; exact error_ne_ok hsz
          | succ k' =>
              obtain ⟨kk', we, hkk', hwe, hw⟩ := sizeOf_inv_vec hfl hsz
              rw [hkk'] at hn
              injection hn with hn
              subst hn
              subst hw
              obtain ⟨hblen, hpt⟩ := mapM_ok_idx hbs
              have hwidths : ∀ x ∈ bs, x.width = we := by
                intro x hx
                obtain ⟨j, hj, hxj⟩ := List.getElem_of_mem hx
                obtain ⟨hj', hrepj⟩ := hpt j (by omega)
                subst hxj
                exact ih elems[j] (List.getElem_mem _) hrepj hwe
              rw [bvConcat_eq, catAll_width]
              rw [sum_const (c := we) (by
                intro a ha
                obtain ⟨x, hx, hxa⟩ := List.mem_map.mp ha
                rw [← hxa]
                exact hwidths x hx)]
              simp only [List.length_map, hblen, hlen]
  | integer hfl =>
      intro k bv hrep k' w hsz
      cases k with
      | zero => rw [Val.rep] at hrep; exact error_ne_ok hrep
      | succ k =>
          rw [Val.rep] at hrep
          injection hrep with hrep
          subst hrep
          cases k' with
          | zero => rw [DEnv.sizeOf] at hsz; exact error_ne_ok hsz
          | succ k' =>
              rw [sizeOf_inv_integer hfl hsz]
  | finite hfl hn =>
      intro k bv hrep k' w hsz
      cases k with
      | zero => rw [Val.rep] at hrep; exact error_ne_ok hrep
      | succ k =>
          rw [Val.rep] at hrep
          injection hrep with hrep
          subst hrep
          cases k' with
          | zero => rw [DEnv.sizeOf] at hsz; exact error_ne_ok hsz
          | succ k' =>
              obtain ⟨kk', hkk', hw⟩ := sizeOf_inv_finite hfl hsz
              rw [hkk'] at hn
              injection hn with hn
              subst hn
              subst hw
              rfl
  | proxy hfl _ =>
      intro k bv hrep k' w hsz
      cases k with
      | zero => rw [Val.rep] at hrep; exact error_ne_ok hrep
      | succ k =>
          rw [Val.rep] at hrep
          injection hrep with hrep
          subst hrep
          cases k' with
          | zero => rw [DEnv.sizeOf] at hsz; exact error_ne_ok hsz
          | succ k' =>
              rw [sizeOf_inv_proxy hfl hsz]
              rfl
  | con hsig hmatch hlen hctor hfields ih =>
      rename_i t c fields sig sub
      intro k bv hrep k' w hsz
      obtain ⟨k'', whole, tag, tagW, bs, hk, hwhole, htg, hbs, hle, hbv⟩ := rep_con_inv hrep
      subst hbv
      rw [bvConcat3, bvCat_width, bvCat_width]
      have : whole = w := sizeOf_det hwhole hsz
      dsimp only [BV.width]
      omega

/-! ## Representation totality (the `repOkB` check)

`Val.rep` on a constructor value checks the static bound
`tagW + Σ field widths ≤ whole` at run time; the check below
discharges it once per (type, constructor) at validation time, making
`rep` total on canonical values at checked types — the fact the
output-encoding leg of the final theorem needs (`portSplit` runs at
the correspondence statement's own evaluation fuel). -/

def repOkB (Δ : DEnv) : Nat → Ty → Bool
  | 0, _ => false
  | fuel + 1, t =>
      match Ty.flatten t with
      | (.con "Vec", [_, te]) => repOkB Δ fuel te
      | (.con tc, _) =>
          (match Δ.sizeOf (fuel + 1) [] t with
           | .ok whole =>
               (if Ty.isTupleCon tc then [tc] else (Δ.ctors.get? tc).getD []).all fun c =>
                 (match Δ.ctorTag t c, Δ.ctorSig.get? c with
                  | .ok (_, tagW), some sig =>
                      (match DEnv.matchTy (Ty.flattenArrow sig.ty).2 t with
                       | .ok sub =>
                           (match ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)).mapM
                               (Δ.sizeOf fuel []) with
                            | .ok ws =>
                                decide (tagW + ws.sum ≤ whole)
                                  && ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)).all
                                       (repOkB Δ fuel)
                            | .error _ => false)
                       | .error _ => false)
                  | _, _ => false)
           | .error _ => false)
      | _ => true

private theorem vec_not_tuple {Δ : DEnv} (h : denvOk Δ = true) :
    Ty.isTupleCon "Vec" = false := by
  simp only [denvOk, Bool.and_eq_true, Bool.not_eq_eq_eq_not, Bool.not_true] at h
  exact h.1.2

private theorem vec_abstract {Δ : DEnv} (h : denvOk Δ = true) {cs : List String}
    (hcs : Δ.ctors.get? "Vec" = some cs) : cs = [] := by
  simp only [denvOk, Bool.and_eq_true] at h
  have h2 := h.2
  rw [hcs] at h2
  cases cs with
  | nil => rfl
  | cons a rest => exact absurd h2 (by simp)

/-- Representation totality: a canonical value at a `repOkB`-checked
type has a representation at the same fuel. -/
theorem vty_rep_total {Δ : DEnv} (hΔ : denvOk Δ = true) :
    ∀ {v : Val} {t : Ty}, VTy Δ v t → ∀ {k : Nat}, repOkB Δ k t = true →
      ∃ bv, Val.rep Δ k v = .ok bv := by
  intro v t hv
  induction hv with
  | vec hfl hn hlen helems ih =>
      rename_i elems t n te kk
      intro k hok
      cases k with
      | zero => rw [repOkB] at hok; cases hok
      | succ k =>
          rw [repOkB] at hok
          split at hok
          case h_2 =>
              rename_i tc args hnv heq
              exfalso
              have hpair := heq.symm.trans hfl
              have h2 : args = [n, te] := congrArg Prod.snd hpair
              have h3 : tc = "Vec" := by
                have h1 : Ty.con tc = Ty.con "Vec" := congrArg Prod.fst hpair
                injection h1
              exact hnv n te h2 h3
          case h_3 =>
              rename_i hnv _
              exact (hnv n te hfl).elim
          case h_1 =>
              rename_i x te' heq
              rw [hfl] at heq
              have hte : te = te' := by
                have h2 : ([n, te] : List Ty) = [x, te'] := congrArg Prod.snd heq
                simp only [List.cons.injEq, and_true] at h2
                exact h2.2
              subst hte
              obtain ⟨bs, hbs, _, _⟩ := mapM_total_idx (g := Val.rep Δ k)
                (xs := elems) (fun i hi => ih elems[i] (List.getElem_mem _) hok)
              refine ⟨Val.bvConcat bs, ?_⟩
              rw [Val.rep, mapM_attach_erase, hbs, except_bind_ok]
              rfl
  | integer hfl =>
      intro k hok
      cases k with
      | zero => rw [repOkB] at hok; cases hok
      | succ k => exact ⟨_, by rw [Val.rep]; rfl⟩
  | finite hfl hn =>
      intro k hok
      cases k with
      | zero => rw [repOkB] at hok; cases hok
      | succ k => exact ⟨_, by rw [Val.rep]; rfl⟩
  | proxy hfl _ =>
      intro k hok
      cases k with
      | zero => rw [repOkB] at hok; cases hok
      | succ k => exact ⟨_, by rw [Val.rep]; rfl⟩
  | con hsig hmatch hlen hctor hfields ih =>
      rename_i t c fields sig sub
      intro k hok
      cases k with
      | zero => rw [repOkB] at hok; cases hok
      | succ k =>
          rw [repOkB] at hok
          split at hok
          case h_1 =>
              -- The Vec row: impossible for a constructor value (Vec is
              -- abstract under denvOk).
              rename_i x te heq
              exfalso
              rw [Rwv.Eidos.Cexp.ctorOf, heq] at hctor
              dsimp only at hctor
              rw [if_neg (by simp [vec_not_tuple hΔ])] at hctor
              obtain ⟨cs, hcs, hmem⟩ := hctor
              rw [vec_abstract hΔ hcs] at hmem
              exact absurd hmem (by simp)
          case h_2 =>
              rename_i tc args hnotvec heq
              split at hok
              rotate_left
              · cases hok
              rename_i whole hwhole
              -- c is among the checked constructors.
              have hcmem : c ∈ (if Ty.isTupleCon tc then [tc]
                  else (Δ.ctors.get? tc).getD []) := by
                rw [Rwv.Eidos.Cexp.ctorOf, heq] at hctor
                dsimp only at hctor
                by_cases htup : Ty.isTupleCon tc
                · rw [if_pos htup] at hctor
                  rw [if_pos htup, hctor]
                  exact List.mem_cons_self
                · rw [if_neg htup] at hctor
                  rw [if_neg htup]
                  obtain ⟨cs, hcs, hmem⟩ := hctor
                  rw [hcs]
                  exact hmem
              have hcchk := List.all_eq_true.mp hok c hcmem
              cases htg : Δ.ctorTag t c with
              | error e =>
                  rw [htg] at hcchk
                  dsimp only at hcchk
                  cases hcchk
              | ok tg =>
                  obtain ⟨tag, tagW⟩ := tg
                  rw [htg, hsig] at hcchk
                  dsimp only at hcchk
                  rw [hmatch] at hcchk
                  dsimp only at hcchk
                  split at hcchk
                  rotate_left
                  · cases hcchk
                  rename_i ws hws
                  simp only [Bool.and_eq_true, decide_eq_true_eq] at hcchk
                  obtain ⟨hbound, hall⟩ := hcchk
                  obtain ⟨hwlen, hwpt⟩ := mapM_ok_idx hws
                  have hwlen' : ws.length = fields.length := by
                    rw [hwlen, List.length_map, hlen]
                  -- per-field canonicality at the instantiated types
                  have hfvty : ∀ i (hi : i < fields.length),
                      VTy Δ (fields[i]'hi)
                        (DEnv.substTv sub ((Ty.flattenArrow sig.ty).1[i]'(by omega))) := by
                    intro i hi
                    have hmem : (((Ty.flattenArrow sig.ty).1[i]'(by omega)), fields[i]'hi)
                        ∈ (Ty.flattenArrow sig.ty).1.zip fields := by
                      rw [show (((Ty.flattenArrow sig.ty).1[i]'(by omega)), fields[i]'hi)
                            = ((Ty.flattenArrow sig.ty).1.zip fields)[i]'(by
                              rw [List.length_zip]; omega)
                          from List.getElem_zip.symm]
                      exact List.getElem_mem _
                    exact hfields _ hmem
                  -- per-field membership in the checked dom list
                  have hdmem : ∀ i (hi : i < fields.length),
                      DEnv.substTv sub ((Ty.flattenArrow sig.ty).1[i]'(by omega))
                        ∈ (Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub) := by
                    intro i hi
                    rw [show DEnv.substTv sub ((Ty.flattenArrow sig.ty).1[i]'(by omega))
                          = ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub))[i]'(by
                            rw [List.length_map]; omega)
                        from (List.getElem_map _).symm]
                    exact List.getElem_mem _
                  obtain ⟨bs, hbs, hblen, hbpt⟩ := mapM_total_idx (g := Val.rep Δ k)
                    (xs := fields) (fun i hi =>
                      ih (((Ty.flattenArrow sig.ty).1[i]'(by omega)), fields[i]'hi)
                        (by
                          rw [show (((Ty.flattenArrow sig.ty).1[i]'(by omega)), fields[i]'hi)
                                = ((Ty.flattenArrow sig.ty).1.zip fields)[i]'(by
                                  rw [List.length_zip]; omega)
                              from List.getElem_zip.symm]
                          exact List.getElem_mem _)
                        (List.all_eq_true.mp hall _ (hdmem i hi)))
                  -- the concatenated field width is the checked sum
                  have hbw : ∀ i (hi : i < bs.length), bs[i].width = ws[i]'(by omega) := by
                    intro i hi
                    obtain ⟨_, hrepi⟩ := hbpt i (by omega)
                    obtain ⟨_, hszi⟩ := hwpt i (by omega)
                    rw [show ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub))[i]'(by omega)
                          = DEnv.substTv sub ((Ty.flattenArrow sig.ty).1[i]'(by omega))
                        from List.getElem_map _] at hszi
                    exact vty_rep_width (hfvty i (by omega)) hrepi hszi
                  have hsum : (Val.bvConcat bs).width = ws.sum := by
                    rw [bvConcat_eq, catAll_width]
                    congr 1
                    refine List.ext_getElem (by rw [List.length_map]; omega) ?_
                    intro i h1 h2
                    rw [List.getElem_map]
                    exact hbw i (by omega)
                  refine ⟨Val.bvConcat [⟨tagW, BitVec.ofNat _ tag⟩,
                    ⟨whole - tagW - (Val.bvConcat bs).width, 0⟩, Val.bvConcat bs], ?_⟩
                  rw [Val.rep, hwhole, except_bind_ok, htg, except_bind_ok]
                  dsimp only
                  rw [mapM_attach_erase, hbs, except_bind_ok,
                      if_pos (by rw [hsum]; exact hbound)]
                  rfl
          case h_3 =>
              -- non-constructor heads: no canonical constructor values
              rename_i hnv hne
              exfalso
              rw [Rwv.Eidos.Cexp.ctorOf] at hctor
              rcases hfl2 : Ty.flatten t with ⟨th, args⟩
              rw [hfl2] at hctor
              cases th with
              | con tc => exact hne tc args hfl2
              | app t₁ t₂ => exact hctor.elim
              | var a => exact hctor.elim
              | nat n => exact hctor.elim
              | arrow t₁ t₂ => exact hctor.elim

/-! ## `detupleSizes` and `portSplit` characterizations -/

private theorem mapM_sizeOf_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k') {vis : List Ty} :
    ∀ {ts : List Ty} {ws : List Nat}, ts.mapM (Δ.sizeOf k vis) = .ok ws →
      ts.mapM (Δ.sizeOf k' vis) = .ok ws := by
  intro ts
  induction ts with
  | nil => intro ws h; simpa using h
  | cons t ts ih =>
      intro ws h
      rw [List.mapM_cons] at h ⊢
      obtain ⟨w, hw, h⟩ := except_bind_eq_ok h
      obtain ⟨ws', hws, h⟩ := except_bind_eq_ok h
      rw [Δ.sizeOf_mono hk hw, except_bind_ok, ih hws, except_bind_ok]
      exact h

private theorem detupleSizes_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k') {t : Ty}
    {szs : List Nat} (h : Val.detupleSizes Δ k t = .ok szs) :
    Val.detupleSizes Δ k' t = .ok szs := by
  rw [Val.detupleSizes] at h ⊢
  obtain ⟨whole, hwhole, h⟩ := except_bind_eq_ok h
  dsimp only at h
  obtain ⟨sizes, hsizes, h⟩ := except_bind_eq_ok h
  rw [Δ.sizeOf_mono hk hwhole, except_bind_ok]
  dsimp only
  rw [mapM_sizeOf_mono hk hsizes, except_bind_ok]
  exact h

private theorem detupleSizes_det {Δ : DEnv} {k k' : Nat} {t : Ty} {a b : List Nat}
    (h : Val.detupleSizes Δ k t = .ok a) (h' : Val.detupleSizes Δ k' t = .ok b) : a = b :=
  Except.ok.inj ((detupleSizes_mono (Nat.le_max_left k k') h).symm.trans
    (detupleSizes_mono (Nat.le_max_right k k') h'))

/-- The descending offsets `portSplit`'s fold walks (`hi` down). -/
private def offsFrom : Nat → List Nat → List Nat
  | _, [] => []
  | hi, w :: ws => (hi - w) :: offsFrom (hi - w) ws

private theorem offsFrom_sum : ∀ (ws : List Nat), offsFrom ws.sum ws = offsetsOf ws := by
  intro ws
  induction ws with
  | nil => rfl
  | cons w ws ih =>
      rw [show (w :: ws).sum = w + ws.sum from List.sum_cons, offsFrom, offsetsOf_cons,
          show w + ws.sum - w = ws.sum by omega, ih]

/-- The fold inside `portSplit`, characterized. -/
private theorem portSplit_fold (bv : BV) :
    ∀ (szs : List Nat) (hi : Nat) (acc : List BV),
      szs.foldl (fun (p : Nat × List BV) w =>
          (p.1 - w, (⟨w, bv.bits.extractLsb' (p.1 - w) w⟩ : BV) :: p.2)) (hi, acc)
        = (hi - szs.sum,
           ((szs.zip (offsFrom hi szs)).map fun p => sliceBV bv p.2 p.1).reverse ++ acc) := by
  intro szs
  induction szs with
  | nil => intro hi acc; simp [offsFrom]
  | cons w ws ih =>
      intro hi acc
      rw [List.foldl_cons, ih, offsFrom, List.zip_cons_cons, List.map_cons,
          List.reverse_cons, List.append_assoc, List.singleton_append,
          List.sum_cons, Nat.sub_sub]
      rfl

/-- `portSplit`, characterized: the representation, the port sizes,
their exact cover, and the result as the `offsetsOf`-positioned
slices. -/
private theorem portSplit_char {Δ : DEnv} {k : Nat} {t : Ty} {v : Val} {bs : List BV}
    (h : Val.portSplit Δ k t v = .ok bs) :
    ∃ bv szs, Val.rep Δ k v = .ok bv ∧ Val.detupleSizes Δ k t = .ok szs ∧
      szs.sum = bv.width ∧
      bs = (szs.zip (offsetsOf szs)).map fun p => sliceBV bv p.2 p.1 := by
  rw [Val.portSplit] at h
  obtain ⟨bv, hbv, h⟩ := except_bind_eq_ok h
  obtain ⟨szs, hszs, h⟩ := except_bind_eq_ok h
  dsimp only at h
  split at h
  · exact error_ne_ok h
  · rename_i hsum
    have hsum' : szs.sum = bv.width := by omega
    refine ⟨bv, szs, hbv, hszs, hsum', ?_⟩
    rw [show szs.foldl (fun (p : Nat × List BV) w =>
            (p.1 - w, (⟨w, bv.bits.extractLsb' (p.1 - w) w⟩ : BV) :: p.2)) (bv.width, [])
          = (bv.width - szs.sum,
             ((szs.zip (offsFrom bv.width szs)).map fun p => sliceBV bv p.2 p.1).reverse
               ++ []) from portSplit_fold bv szs bv.width []] at h
    injection h with h
    rw [← h, List.append_nil, List.reverse_reverse, ← hsum', offsFrom_sum]

/-- `portSplit`, constructed: from a representation and an exact port
cover. -/
private theorem portSplit_intro {Δ : DEnv} {k : Nat} {t : Ty} {v : Val} {bv : BV}
    {szs : List Nat} (hbv : Val.rep Δ k v = .ok bv)
    (hszs : Val.detupleSizes Δ k t = .ok szs) (hsum : szs.sum = bv.width) :
    Val.portSplit Δ k t v
      = .ok ((szs.zip (offsetsOf szs)).map fun p => sliceBV bv p.2 p.1) := by
  rw [Val.portSplit, hbv, except_bind_ok, hszs, except_bind_ok]
  dsimp only
  rw [if_neg (by omega)]
  rw [show szs.foldl (fun (p : Nat × List BV) w =>
          (p.1 - w, (⟨w, bv.bits.extractLsb' (p.1 - w) w⟩ : BV) :: p.2)) (bv.width, [])
        = (bv.width - szs.sum,
           ((szs.zip (offsFrom bv.width szs)).map fun p => sliceBV bv p.2 p.1).reverse
             ++ []) from portSplit_fold bv szs bv.width []]
  dsimp only
  rw [List.append_nil, List.reverse_reverse, ← hsum, offsFrom_sum]
  rfl

/-! ## The state-encoding kit: `encTag`, `encCellRegs`, `encodeList` -/

private theorem encTag_split (lo : Layout) (tag : Nat) (argWs : List Nat) (reps : List BV) :
    encTag lo tag argWs reps
      = bvCat (bvCat ⟨lo.rTagW, BitVec.ofNat _ tag⟩ ⟨lo.rPayW - argWs.sum, 0⟩)
          (catAll reps) := by
  rw [encTag, bvConcat_eq, catAll_cons, catAll_cons, ← bvCat_assoc]

private theorem encTag_width {lo : Layout} {tag : Nat} {argWs : List Nat} {reps : List BV}
    (hw : reps.map (·.width) = argWs) (hb : argWs.sum ≤ lo.rPayW) :
    (encTag lo tag argWs reps).width = lo.rTagW + lo.rPayW := by
  rw [encTag_split, bvCat_width, bvCat_width, catAll_width, hw]
  show lo.rTagW + (lo.rPayW - argWs.sum) + argWs.sum = _
  omega

/-- The tag field: the top `rTagW` bits of the encoding are the tag
literal (`tagFix_of_store`'s hypothesis). -/
private theorem encTag_top {lo : Layout} {tag : Nat} {argWs : List Nat} {reps : List BV}
    (hw : reps.map (·.width) = argWs) (hb : argWs.sum ≤ lo.rPayW) :
    ∀ j, j < lo.rTagW →
      (encTag lo tag argWs reps).bits.getLsbD (lo.rPayW + j)
        = (BitVec.ofNat lo.rTagW tag).getLsbD j := by
  intro j hj
  rw [encTag_split, bvCat_getLsbD]
  have hcw : (catAll reps).width = argWs.sum := by rw [catAll_width, hw]
  rw [if_neg (by omega), bvCat_getLsbD]
  have hpw : (BV.mk (lo.rPayW - argWs.sum) 0).width = lo.rPayW - argWs.sum := rfl
  rw [if_neg (by rw [hpw]; omega)]
  congr 1
  rw [hpw, hcw]
  omega

/-- The saved-argument fields: slicing the encoding at the
`offsetsOf argWs` positions recovers each argument's representation. -/
private theorem encTag_arg {lo : Layout} {tag : Nat} {argWs : List Nat} {reps : List BV}
    (hw : reps.map (·.width) = argWs) (hb : argWs.sum ≤ lo.rPayW) :
    ∀ i (hi : i < argWs.length),
      sliceBV (encTag lo tag argWs reps)
          ((offsetsOf argWs)[i]'(by rw [offsetsOf_length]; exact hi))
          (argWs[i]) = reps[i]'(by rw [← hw] at hi; simpa using hi) := by
  intro i hi
  have hrl : reps.length = argWs.length := by rw [← hw]; simp
  have hcw : (catAll reps).width = argWs.sum := by rw [catAll_width, hw]
  rw [encTag_split]
  rw [sliceBV_cat_low (by
    rw [hcw, offsetsOf_getElem _ i hi]
    exact drop_sum_le hi)]
  have hidx := catAll_extract_idx reps i (by omega)
  rw [offsetsOf_getElem _ i (by rw [List.length_map]; omega)] at hidx
  rw [offsetsOf_getElem _ i hi]
  have hgw : argWs[i]'hi = (reps[i]'(by omega)).width := by
    have h1 : argWs[i]? = (reps.map (fun x => x.width))[i]? := by rw [hw]
    rw [List.getElem?_eq_getElem hi,
        List.getElem?_eq_getElem (by rw [List.length_map]; omega),
        List.getElem_map] at h1
    exact Option.some.inj h1
  have hds : ((reps.map (fun x => x.width)).drop (i + 1)).sum = (argWs.drop (i + 1)).sum := by
    rw [hw]
  rw [hds] at hidx
  rw [hgw]
  exact hidx

/-- `encCellRegs`, characterized. -/
private theorem encCellRegs_inv {regs : List (String × Nat)} {bv : BV}
    {prs : List (String × BV)} (h : encCellRegs regs bv = .ok prs) :
    (regs.map (·.2)).sum = bv.width ∧
    prs = (regs.zip (offsetsOf (regs.map (·.2)))).map
      fun p => (p.1.1, sliceBV bv p.2 p.1.2) := by
  rw [encCellRegs] at h
  split at h
  · rename_i hsum
    have h' : Except.ok ((regs.zip (offsetsOf (regs.map (·.2)))).map
        fun p => (p.1.1, (⟨p.1.2, bv.bits.extractLsb' p.2 p.1.2⟩ : BV)))
        = (.ok prs : Except String (List (String × BV))) := h
    injection h' with h'
    refine ⟨by simpa using hsum, ?_⟩
    rw [← h']
    rfl
  · exact error_ne_ok h

/-- `encCellRegs`, constructed. -/
private theorem encCellRegs_intro {regs : List (String × Nat)} {bv : BV}
    (hsum : (regs.map (·.2)).sum = bv.width) :
    encCellRegs regs bv = .ok ((regs.zip (offsetsOf (regs.map (·.2)))).map
      fun p => (p.1.1, sliceBV bv p.2 p.1.2)) := by
  cases henc : encCellRegs regs bv with
  | error e =>
      exfalso
      rw [encCellRegs] at henc
      split at henc
      · exact nomatch henc
      · rename_i hne
        exact hne (by simpa using hsum)
  | ok prs =>
      obtain ⟨_, hprs⟩ := encCellRegs_inv henc
      rw [hprs]

private theorem encCellE_inv {Δ : DEnv} {k : Nat} {s : MState} {c : CellPlan}
    {prs : List (String × BV)} (h : encCellE Δ k s c = .ok prs) :
    ∃ v bv, s.cells.get? c.name = some v ∧ Val.rep Δ k v = .ok bv ∧
      bv.width = c.width ∧ (c.regs.map (·.2)).sum = bv.width ∧
      prs = (c.regs.zip (offsetsOf (c.regs.map (·.2)))).map
        fun p => (p.1.1, sliceBV bv p.2 p.1.2) := by
  rw [encCellE] at h
  split at h
  · exact error_ne_ok h
  · rename_i v hv
    obtain ⟨bv, hbv, h⟩ := except_bind_eq_ok h
    split at h
    · rename_i hwidth
      obtain ⟨hsum, hprs⟩ := encCellRegs_inv h
      exact ⟨v, bv, hv, hbv, by simpa using hwidth, hsum, hprs⟩
    · exact error_ne_ok h

/-- `encodeList`, characterized. -/
private theorem encodeList_inv {Δ : DEnv} {k : Nat} {lo : Layout} {plan : Plan}
    {s : MState} {enc : List (String × BV)} (h : encodeList Δ k lo plan s = .ok enc) :
    ∃ tgt reps parts,
      lo.targets.find? (fun t => t.uniq == s.label) = some tgt ∧
      s.args.mapM (Val.rep Δ k) = .ok reps ∧
      reps.map (·.width) = tgt.argWs ∧
      plan.cells.mapM (encCellE Δ k s) = .ok parts ∧
      enc = (match plan.tagReg with
             | none => []
             | some (r, _) => [(r, encTag lo tgt.tag tgt.argWs reps)]) ++ parts.flatten := by
  rw [encodeList] at h
  split at h
  rotate_left
  · exact error_ne_ok h
  rename_i tgt htgt
  rw [except_pure_def, except_bind_ok] at h
  dsimp only at h
  obtain ⟨reps, hreps, h⟩ := except_bind_eq_ok h
  split at h
  rotate_left
  · exact error_ne_ok h
  rename_i hwidths
  obtain ⟨parts, hparts, h⟩ := except_bind_eq_ok h
  rw [except_pure_def] at h
  injection h with h
  exact ⟨tgt, reps, parts, htgt, hreps, by simpa using hwidths, hparts, h.symm⟩

/-- `encodeList`, constructed. -/
private theorem encodeList_intro {Δ : DEnv} {k : Nat} {lo : Layout} {plan : Plan}
    {s : MState} {tgt : LTarget} {reps : List BV} {parts : List (List (String × BV))}
    (hfind : lo.targets.find? (fun t => t.uniq == s.label) = some tgt)
    (hreps : s.args.mapM (Val.rep Δ k) = .ok reps)
    (hw : reps.map (·.width) = tgt.argWs)
    (hparts : plan.cells.mapM (encCellE Δ k s) = .ok parts) :
    encodeList Δ k lo plan s
      = .ok ((match plan.tagReg with
              | none => []
              | some (r, _) => [(r, encTag lo tgt.tag tgt.argWs reps)]) ++ parts.flatten) := by
  rw [encodeList, hfind]
  show (do
      let reps ← s.args.mapM (Val.rep Δ k)
      if reps.map (·.width) == tgt.argWs then do
        let cellParts ← plan.cells.mapM (encCellE Δ k s)
        pure ((match plan.tagReg with
               | none => []
               | some (r, _) => [(r, encTag lo tgt.tag tgt.argWs reps)]) ++ cellParts.flatten)
      else throw s!"encode: argument widths {reps.map (·.width)} ≠ layout {tgt.argWs}"
    : Except String (List (String × BV))) = _
  rw [hreps, except_bind_ok,
      if_pos (show (reps.map (·.width) == tgt.argWs) = true by simpa using hw),
      hparts, except_bind_ok]
  rfl

/-! ## `find?` at distinct keys -/

private theorem nodupIntB_nodup : ∀ {l : List Int},
    Rwv.Eidos.Cexp.nodupIntB l = true → l.Nodup := by
  intro l
  induction l with
  | nil => intro _; exact List.nodup_nil
  | cons x xs ih =>
      intro h
      simp only [Rwv.Eidos.Cexp.nodupIntB, Bool.and_eq_true, Bool.not_eq_true'] at h
      rw [List.nodup_cons]
      exact ⟨by simpa using h.1, ih h.2⟩

private theorem find?_uniq {tgts : List LTarget} {tgt : LTarget} {lbl : Int}
    (hnd : (tgts.map (·.uniq)).Nodup) (hmem : tgt ∈ tgts) (huq : tgt.uniq = lbl) :
    tgts.find? (fun t => t.uniq == lbl) = some tgt := by
  induction tgts with
  | nil => exact absurd hmem (by simp)
  | cons a rest ih =>
      simp only [List.map_cons, List.nodup_cons] at hnd
      rcases List.mem_cons.mp hmem with hmem | hmem
      · subst hmem
        exact List.find?_cons_of_pos (by simpa using huq)
      · have hne : (a.uniq == lbl) = false := by
          rw [beq_eq_false_iff_ne]
          intro hc
          rw [← huq] at hc
          exact hnd.1 (List.mem_map.mpr ⟨tgt, hmem, hc.symm⟩)
        rw [List.find?_cons_of_neg (by simp [hne])]
        exact ih hnd.2 hmem

/-! ## Layout and plan inversions -/

private theorem foldl_max_init : ∀ (l : List Nat) (a : Nat), a ≤ l.foldl max a := by
  intro l
  induction l with
  | nil => intro a; exact Nat.le_refl a
  | cons x xs ih => intro a; exact Nat.le_trans (Nat.le_max_left a x) (ih (max a x))

private theorem mem_le_foldl_max : ∀ (l : List Nat) (a : Nat) {x : Nat}, x ∈ l →
    x ≤ l.foldl max a := by
  intro l
  induction l with
  | nil => intro a x hx; cases hx
  | cons y ys ih =>
      intro a x hx
      rcases List.mem_cons.mp hx with hx | hx
      · subst hx
        exact Nat.le_trans (Nat.le_max_right a x) (foldl_max_init ys (max a x))
      · exact ih (max a y) hx

/-- `takeRegs`, characterized: it splits the register list at the
width list, matching widths exactly. -/
private theorem takeRegs_inv : ∀ {ws : List Nat} {regs run rest : List (String × Nat)},
    takeRegs regs ws = .ok (run, rest) → regs = run ++ rest ∧ run.map (·.2) = ws := by
  intro ws
  induction ws with
  | nil =>
      intro regs run rest h
      rw [takeRegs] at h
      rw [except_pure_def] at h
      injection h with h
      injection h with h1 h2
      subst h1; subst h2
      exact ⟨rfl, rfl⟩
  | cons w ws ih =>
      intro regs run rest h
      cases regs with
      | nil => rw [takeRegs] at h; exact error_ne_ok h
      | cons p regs' =>
          obtain ⟨r, rw'⟩ := p
          rw [takeRegs] at h
          split at h
          · rename_i heq
            obtain ⟨rr, hrr, h⟩ := except_bind_eq_ok h
            obtain ⟨run', rest'⟩ := rr
            dsimp only at h
            rw [except_pure_def] at h
            injection h with h
            injection h with h1 h2
            subst h1; subst h2
            obtain ⟨hre, hm⟩ := ih hrr
            subst heq
            exact ⟨by rw [hre]; rfl, by rw [List.map_cons, hm]⟩
          · exact error_ne_ok h

/-- The register-allocation fold of `mkPlan`, characterized. -/
private theorem planCell_fold_inv {Δ : DEnv} {fuel : Nat} :
    ∀ (cellsL : List (String × Ty × Nat)) (acc : List CellPlan)
      (rem : List (String × Nat)) {out : List CellPlan × List (String × Nat)},
      cellsL.foldlM (init := (acc, rem)) (planCell Δ fuel) = .ok out →
      ∃ news, out.1 = acc ++ news ∧ rem = (news.map (·.regs)).flatten ++ out.2 ∧
        news.map (fun c => (c.name, c.ty, c.width)) = cellsL ∧
        ∀ c ∈ news, Val.detupleSizes Δ fuel c.ty = .ok (c.regs.map (·.2)) ∧
          (c.regs.map (·.2)).sum = c.width := by
  intro cellsL
  induction cellsL with
  | nil =>
      intro acc rem out h
      rw [List.foldlM_nil, except_pure_def] at h
      injection h with h
      subst h
      exact ⟨[], by simp, by simp, rfl, fun c hc => absurd hc (by simp)⟩
  | cons cl rest ih =>
      intro acc rem out h
      rw [List.foldlM_cons] at h
      obtain ⟨st, hst, h⟩ := except_bind_eq_ok h
      obtain ⟨nm, ty, w⟩ := cl
      rw [planCell] at hst
      obtain ⟨szs, hszs, hst⟩ := except_bind_eq_ok hst
      split at hst
      rotate_left
      · exact error_ne_ok hst
      rename_i hsum
      obtain ⟨rr, hrr, hst⟩ := except_bind_eq_ok hst
      obtain ⟨run, rem'⟩ := rr
      dsimp only at hst
      rw [except_pure_def] at hst
      injection hst with hst
      subst hst
      obtain ⟨hregs, hws⟩ := takeRegs_inv hrr
      obtain ⟨news, hn1, hn2, hn3, hn4⟩ := ih _ _ h
      refine ⟨{ name := nm, ty, width := w, regs := run } :: news, ?_, ?_, ?_, ?_⟩
      · rw [hn1, List.append_assoc]
        rfl
      · rw [hregs, hn2, List.map_cons, List.flatten_cons, List.append_assoc]
      · rw [List.map_cons, hn3]
      · intro c hc
        rcases List.mem_cons.mp hc with hc | hc
        · subst hc
          dsimp only
          rw [hws]
          exact ⟨hszs, by simpa using hsum⟩
        · exact hn4 c hc

/-- Everything `mkLayoutL` guarantees about a layout it accepts. -/
structure LayoutInv (Δ : DEnv) (fuel : Nat) (p : Proc) (lo : Layout) : Prop where
  outsz : Δ.sizeOf fuel [] p.outTy = .ok lo.outW
  cellsz : ∀ pr ∈ lo.cells, Δ.sizeOf fuel [] pr.2.1 = .ok pr.2.2
  tgts : ∀ tgt ∈ lo.targets, ∃ l b, (l, b) ∈ p.blocks ∧ l.uniq = tgt.uniq ∧
    tgt.argTys = b.params.dropLast.map (·.sig.ty) ∧
    tgt.argTys.mapM (Δ.sizeOf fuel []) = .ok tgt.argWs
  paybound : ∀ tgt ∈ lo.targets, tgt.argWs.sum ≤ lo.rPayW
  uniqsub : (lo.targets.map (·.uniq)).Sublist (p.blocks.map (·.1.uniq))
  rwdef : lo.rW = lo.rTagW + lo.rPayW

private theorem targets_mapM_facts {Δ : DEnv} {fuel : Nat} {p : Proc}
    {tbs : List (Id × Block)} (hsub : tbs.Sublist p.blocks) {targets : List LTarget}
    (h : ((List.range tbs.length).zip tbs).mapM (fun ((i, (l, b)) : Nat × (Id × Block)) => do
        let tys := (b.params.dropLast).map (fun (x : Id) => x.sig.ty)
        let ws ← tys.mapM (Δ.sizeOf fuel [])
        pure { uniq := l.uniq, tag := i, argTys := tys, argWs := ws : LTarget })
      = .ok targets) :
    (∀ tgt ∈ targets, ∃ l b, (l, b) ∈ p.blocks ∧ l.uniq = tgt.uniq ∧
      tgt.argTys = b.params.dropLast.map (·.sig.ty) ∧
      tgt.argTys.mapM (Δ.sizeOf fuel []) = .ok tgt.argWs) ∧
    targets.map (·.uniq) = tbs.map (·.1.uniq) := by
  obtain ⟨hlen, hpt⟩ := mapM_ok_idx h
  have hlen' : targets.length = tbs.length := by
    rw [hlen, List.length_zip, List.length_range]
    omega
  have hidx : ∀ i (hi : i < targets.length),
      (targets[i]).uniq = (tbs[i]'(by omega)).1.uniq ∧
      (targets[i]).argTys = ((tbs[i]'(by omega)).2.params.dropLast).map (·.sig.ty) ∧
      (targets[i]).argTys.mapM (Δ.sizeOf fuel []) = .ok (targets[i]).argWs := by
    intro i hi
    obtain ⟨hz, hgi⟩ := hpt i (by rw [List.length_zip, List.length_range]; omega)
    rw [show ((List.range tbs.length).zip tbs)[i]'(by
          rw [List.length_zip, List.length_range]; omega)
        = (i, tbs[i]'(by omega)) from by rw [List.getElem_zip, List.getElem_range]] at hgi
    obtain ⟨ws, hws, hgi⟩ := except_bind_eq_ok hgi
    rw [except_pure_def] at hgi
    injection hgi with hgi
    rw [← hgi]
    exact ⟨rfl, rfl, by rw [hws]⟩
  refine ⟨?_, ?_⟩
  · intro tgt hmem
    obtain ⟨i, hi, hti⟩ := List.getElem_of_mem hmem
    obtain ⟨h1, h2, h3⟩ := hidx i hi
    subst hti
    exact ⟨(tbs[i]'(by omega)).1, (tbs[i]'(by omega)).2,
      hsub.mem (List.getElem_mem (show i < tbs.length by omega)), h1.symm, h2, h3⟩
  · refine List.ext_getElem (by simp [hlen']) ?_
    intro i h1 h2
    rw [List.getElem_map, List.getElem_map]
    exact (hidx i (by simpa using h1)).1

private theorem mkLayoutL_inv {Δ : DEnv} {fuel : Nat} {p : Proc} {lo : Layout}
    (h : mkLayoutL Δ fuel p = .ok lo) : LayoutInv Δ fuel p lo := by
  rw [mkLayoutL] at h
  obtain ⟨outW, houtW, h⟩ := except_bind_eq_ok h
  obtain ⟨cells, hcells, h⟩ := except_bind_eq_ok h
  obtain ⟨targets, htargets, h⟩ := except_bind_eq_ok h
  obtain ⟨haltTys, hhaltTys, h⟩ := except_bind_eq_ok h
  obtain ⟨haltWs, hhaltWs, h⟩ := except_bind_eq_ok h
  rw [except_pure_def] at h
  injection h with h
  subst h
  obtain ⟨htgts, huniqs⟩ := targets_mapM_facts List.filter_sublist htargets
  refine ⟨houtW, ?_, htgts, ?_, ?_, rfl⟩
  · -- cell sizes, from the cells mapM
    intro pr hpr
    obtain ⟨hclen, hcpt⟩ := mapM_ok_idx hcells
    obtain ⟨i, hi, hpri⟩ := List.getElem_of_mem hpr
    have hi' : i < cells.length := hi
    have hpri' : cells[i]'hi' = pr := hpri
    obtain ⟨hz, hgi⟩ := hcpt i (by omega)
    obtain ⟨w, hw, hgi⟩ := except_bind_eq_ok hgi
    rw [except_pure_def] at hgi
    injection hgi with hgi
    rw [← hpri', ← hgi]
    exact hw
  · -- the payload bound
    intro tgt hmem
    exact mem_le_foldl_max _ 0 (List.mem_map.mpr ⟨tgt, hmem, rfl⟩)
  · -- target uniques are a sublist of the block uniques
    rw [huniqs]
    exact List.filter_sublist.map _

/-- Everything `mkPlan` guarantees about a plan it accepts. -/
structure PlanInv (Δ : DEnv) (fuel : Nat) (p : Proc) (lo : Layout)
    (dev : Rwv.Hyle.Device) (plan : Plan) : Prop where
  ndio : Rwv.Hyle.Bridge.nodupB (dev.inputs.map (·.1) ++ dev.registers.map (·.name)) = true
  insz : Val.detupleSizes Δ fuel p.inTy = .ok (dev.inputs.map (·.2))
  outsz : Val.detupleSizes Δ fuel p.outTy = .ok (dev.outputs.map (·.2))
  outsum : (dev.outputs.map (·.2)).sum = lo.outW
  ndcell : Rwv.Hyle.Bridge.nodupB (lo.cells.map (·.1)) = true
  inports : plan.inPorts = dev.inputs
  outports : plan.outPorts = dev.outputs
  tagw : ∀ r w, plan.tagReg = some (r, w) → w = lo.rW
  tagnone : plan.tagReg = none → lo.rW = 0
  regsplit : dev.registers.map (fun r => (r.name, r.width))
      = (match plan.tagReg with | some rw => [rw] | none => [])
        ++ (plan.cells.map (·.regs)).flatten
  cellshape : plan.cells.map (fun c => (c.name, c.ty, c.width)) = lo.cells
  cellszs : ∀ c ∈ plan.cells, Val.detupleSizes Δ fuel c.ty = .ok (c.regs.map (·.2)) ∧
      (c.regs.map (·.2)).sum = c.width

private theorem mkPlan_inv {Δ : DEnv} {fuel : Nat} {p : Proc} {lo : Layout}
    {dev : Rwv.Hyle.Device} {plan : Plan} (h : mkPlan Δ fuel p lo dev = .ok plan) :
    PlanInv Δ fuel p lo dev plan := by
  rw [mkPlan] at h
  split at h
  rotate_left
  · exact error_ne_ok h
  rename_i hnd
  obtain ⟨inSzs, hinSzs, h⟩ := except_bind_eq_ok h
  obtain ⟨outSzs, houtSzs, h⟩ := except_bind_eq_ok h
  split at h
  rotate_left
  · exact error_ne_ok h
  rename_i hins
  split at h
  rotate_left
  · exact error_ne_ok h
  rename_i houts
  split at h
  rotate_left
  · exact error_ne_ok h
  rename_i hosum
  split at h
  rotate_left
  · exact error_ne_ok h
  rename_i hndc
  -- the tag-register split, then the shared tail
  have htail : ∀ (tagReg : Option (String × Nat)) (stRegs : List (String × Nat)),
      (do
        let (cellsR, rest) ← lo.cells.foldlM (init := (([] : List CellPlan), stRegs))
          (planCell Δ fuel)
        if rest.isEmpty then
          pure { tagReg, cells := cellsR, inPorts := dev.inputs,
                 outPorts := dev.outputs : Plan }
        else throw s!"unaccounted device registers: {rest.map (·.1)}"
        : Except String Plan) = .ok plan →
      plan.tagReg = tagReg ∧ plan.inPorts = dev.inputs ∧ plan.outPorts = dev.outputs ∧
      stRegs = (plan.cells.map (·.regs)).flatten ∧
      plan.cells.map (fun c => (c.name, c.ty, c.width)) = lo.cells ∧
      ∀ c ∈ plan.cells, Val.detupleSizes Δ fuel c.ty = .ok (c.regs.map (·.2)) ∧
        (c.regs.map (·.2)).sum = c.width := by
    intro tagReg stRegs htl
    obtain ⟨cr, hcr, htl⟩ := except_bind_eq_ok htl
    obtain ⟨cellsR, rest⟩ := cr
    dsimp only at htl
    split at htl
    rotate_left
    · exact error_ne_ok htl
    rename_i hempty
    rw [except_pure_def] at htl
    injection htl with htl
    subst htl
    have hrest : rest = [] := List.isEmpty_iff.mp hempty
    subst hrest
    obtain ⟨news, hn1, hn2, hn3, hn4⟩ := planCell_fold_inv lo.cells [] stRegs hcr
    have hnews : cellsR = news := by simpa using hn1
    subst hnews
    exact ⟨rfl, rfl, rfl, by rw [hn2, List.append_nil], hn3, hn4⟩
  have hcore : ∃ tagReg stRegs,
      (∀ r w, tagReg = some (r, w) → w = lo.rW) ∧
      (tagReg = none → lo.rW = 0) ∧
      dev.registers.map (fun r => (r.name, r.width))
        = (match tagReg with | some rw => [rw] | none => []) ++ stRegs ∧
      plan.tagReg = tagReg ∧ plan.inPorts = dev.inputs ∧ plan.outPorts = dev.outputs ∧
      stRegs = (plan.cells.map (·.regs)).flatten ∧
      plan.cells.map (fun c => (c.name, c.ty, c.width)) = lo.cells ∧
      (∀ c ∈ plan.cells, Val.detupleSizes Δ fuel c.ty = .ok (c.regs.map (·.2)) ∧
        (c.regs.map (·.2)).sum = c.width) := by
    by_cases h0 : lo.rW > 0
    · rw [if_pos h0] at h
      cases hrm : dev.registers.map (fun r => (r.name, r.width)) with
      | nil => rw [hrm] at h; exact error_ne_ok h
      | cons pr rrest =>
          obtain ⟨r0, w0⟩ := pr
          rw [hrm] at h
          dsimp only at h
          split at h
          · rename_i hw0
            rw [except_pure_def, except_bind_ok] at h
            obtain ⟨hp1, hp2, hp3, hp4, hp5, hp6⟩ := htail (some (r0, w0)) rrest h
            refine ⟨some (r0, w0), rrest, ?_, ?_, rfl, hp1, hp2, hp3, hp4, hp5, hp6⟩
            · intro r w hrw
              injection hrw with hrw
              exact ((congrArg Prod.snd hrw).symm).trans hw0
            · intro hc; cases hc
          · exact error_ne_ok h
    · rw [if_neg h0] at h
      rw [except_pure_def, except_bind_ok] at h
      obtain ⟨hp1, hp2, hp3, hp4, hp5, hp6⟩ :=
        htail none (dev.registers.map (fun r => (r.name, r.width))) h
      refine ⟨none, dev.registers.map (fun r => (r.name, r.width)), ?_, ?_, rfl,
        hp1, hp2, hp3, hp4, hp5, hp6⟩
      · intro r w hrw
        cases hrw
      · intro _
        omega
  obtain ⟨tagReg, stRegs, hc1, hc2, hc3, hc4, hc5, hc6, hc7, hc8, hc9⟩ := hcore
  subst hc4
  refine ⟨hnd, ?_, ?_, ?_, hndc, hc5, hc6, hc1, hc2, ?_, hc8, hc9⟩
  · rw [hinSzs]
    congr 1
    exact (eq_of_beq hins).symm
  · rw [houtSzs]
    congr 1
    exact (eq_of_beq houts).symm
  · rw [show (dev.outputs.map (·.2)).sum = outSzs.sum from by rw [eq_of_beq houts]]
    exact eq_of_beq hosum
  · rw [hc3, hc7]

/-! ## The terminator-selection specification

`Machine.selectTAlt` is a `for` loop with early return; `selSpec` is
its structural reading (first match wins, defaults skipped, the
fallback computed from the original list), connected by
`selectTAlt_char`. -/

private def selSpec (Δ : DEnv) (fuel : Nat) (scrut : Val) :
    List TAlt → Except String (List Id × Term) → Except String (List Id × Term)
  | [], after => after
  | .mk (.dataAlt c) bs t :: rest, after =>
      (match scrut with
      | .con _ c' _ =>
          if c = c' then .ok (bs, t) else selSpec Δ fuel scrut rest after
      | _ => selSpec Δ fuel scrut rest after)
  | .mk (.litAlt n) bs t :: rest, after => do
      if (← Eval.litMatches Δ fuel scrut n) = true then pure (bs, t)
      else selSpec Δ fuel scrut rest after
  | .mk .default _ _ :: rest, after => selSpec Δ fuel scrut rest after

/-- The loop body of `Machine.selectTAlt`, in structural form. -/
private def selBody (Δ : DEnv) (fuel : Nat) (scrut : Val) (alt : TAlt) :
    Except String (ForInStep ((Option (List Id × Term)) × Unit)) :=
  match alt with
  | .mk (.dataAlt c) bs t =>
      (match scrut with
      | .con _ c' _ =>
          if c = c' then pure (.done (some (bs, t), ()))
          else pure (.yield (none, ()))
      | _ => pure (.yield (none, ())))
  | .mk (.litAlt n) bs t => do
      if (← Eval.litMatches Δ fuel scrut n) = true then pure (.done (some (bs, t), ()))
      else pure (.yield (none, ()))
  | .mk .default _ _ => pure (.yield (none, ()))

private theorem selLoop_char (Δ : DEnv) (fuel : Nat) (scrut : Val)
    (f : TAlt → (Option (List Id × Term)) × Unit →
      Except String (ForInStep ((Option (List Id × Term)) × Unit)))
    (hf : ∀ alt st, f alt st = selBody Δ fuel scrut alt)
    (k : (Option (List Id × Term)) × Unit → Except String (List Id × Term))
    (hk : ∀ r, k (some r, ()) = pure r) :
    ∀ (l : List TAlt),
      (forIn l ((none : Option (List Id × Term)), ()) f >>= k)
        = selSpec Δ fuel scrut l (k (none, ())) := by
  intro l
  induction l with
  | nil =>
      rw [List.forIn_nil, except_pure_def, except_bind_ok]
      rfl
  | cons alt rest ih =>
      rw [List.forIn_cons, hf alt, bind_assoc]
      obtain ⟨con, bs, t⟩ := alt
      cases con with
      | dataAlt c =>
          cases scrut with
          | con ty c' fields =>
              rw [show selBody Δ fuel (Val.con ty c' fields) (.mk (.dataAlt c) bs t)
                    = (if c = c' then pure (.done (some (bs, t), ()))
                       else pure (.yield (none, ()))) from rfl,
                  show selSpec Δ fuel (Val.con ty c' fields)
                        (.mk (.dataAlt c) bs t :: rest) (k (none, ()))
                    = (if c = c' then .ok (bs, t)
                       else selSpec Δ fuel (Val.con ty c' fields) rest (k (none, ()))) from rfl]
              by_cases hc : c = c'
              · rw [if_pos hc, if_pos hc, except_pure_def, except_bind_ok]
                dsimp only
                rw [except_pure_def, except_bind_ok, hk]
                try rfl
              · rw [if_neg hc, if_neg hc, except_pure_def, except_bind_ok]
                dsimp only
                exact ih
          | vec elems =>
              rw [show selBody Δ fuel (Val.vec elems) (.mk (.dataAlt c) bs t)
                    = pure (.yield (none, ())) from rfl,
                  show selSpec Δ fuel (Val.vec elems) (.mk (.dataAlt c) bs t :: rest) (k (none, ()))
                    = selSpec Δ fuel (Val.vec elems) rest (k (none, ())) from rfl,
                  except_pure_def, except_bind_ok]
              dsimp only
              exact ih
          | integer x =>
              rw [show selBody Δ fuel (Val.integer x) (.mk (.dataAlt c) bs t)
                    = pure (.yield (none, ())) from rfl,
                  show selSpec Δ fuel (Val.integer x) (.mk (.dataAlt c) bs t :: rest) (k (none, ()))
                    = selSpec Δ fuel (Val.integer x) rest (k (none, ())) from rfl,
                  except_pure_def, except_bind_ok]
              dsimp only
              exact ih
          | finite b i =>
              rw [show selBody Δ fuel (Val.finite b i) (.mk (.dataAlt c) bs t)
                    = pure (.yield (none, ())) from rfl,
                  show selSpec Δ fuel (Val.finite b i) (.mk (.dataAlt c) bs t :: rest) (k (none, ()))
                    = selSpec Δ fuel (Val.finite b i) rest (k (none, ())) from rfl,
                  except_pure_def, except_bind_ok]
              dsimp only
              exact ih
          | str s =>
              rw [show selBody Δ fuel (Val.str s) (.mk (.dataAlt c) bs t)
                    = pure (.yield (none, ())) from rfl,
                  show selSpec Δ fuel (Val.str s) (.mk (.dataAlt c) bs t :: rest) (k (none, ()))
                    = selSpec Δ fuel (Val.str s) rest (k (none, ())) from rfl,
                  except_pure_def, except_bind_ok]
              dsimp only
              exact ih
          | proxy =>
              rw [show selBody Δ fuel Val.proxy (.mk (.dataAlt c) bs t)
                    = pure (.yield (none, ())) from rfl,
                  show selSpec Δ fuel Val.proxy (.mk (.dataAlt c) bs t :: rest) (k (none, ()))
                    = selSpec Δ fuel Val.proxy rest (k (none, ())) from rfl,
                  except_pure_def, except_bind_ok]
              dsimp only
              exact ih
          | closL x env body =>
              rw [show selBody Δ fuel (Val.closL x env body) (.mk (.dataAlt c) bs t)
                    = pure (.yield (none, ())) from rfl,
                  show selSpec Δ fuel (Val.closL x env body)
                        (.mk (.dataAlt c) bs t :: rest) (k (none, ()))
                    = selSpec Δ fuel (Val.closL x env body) rest (k (none, ())) from rfl,
                  except_pure_def, except_bind_ok]
              dsimp only
              exact ih
          | closD g pre =>
              rw [show selBody Δ fuel (Val.closD g pre) (.mk (.dataAlt c) bs t)
                    = pure (.yield (none, ())) from rfl,
                  show selSpec Δ fuel (Val.closD g pre) (.mk (.dataAlt c) bs t :: rest) (k (none, ()))
                    = selSpec Δ fuel (Val.closD g pre) rest (k (none, ())) from rfl,
                  except_pure_def, except_bind_ok]
              dsimp only
              exact ih
      | litAlt n =>
          rw [show selBody Δ fuel scrut (.mk (.litAlt n) bs t)
                = (Eval.litMatches Δ fuel scrut n >>= fun b =>
                    if b = true then pure (.done (some (bs, t), ()))
                    else pure (.yield (none, ()))) from rfl,
              show selSpec Δ fuel scrut (.mk (.litAlt n) bs t :: rest) (k (none, ()))
                = (Eval.litMatches Δ fuel scrut n >>= fun b =>
                    if b = true then pure (bs, t)
                    else selSpec Δ fuel scrut rest (k (none, ()))) from rfl,
              bind_assoc]
          cases hlm : Eval.litMatches Δ fuel scrut n with
          | error e => rfl
          | ok b =>
              rw [except_bind_ok, except_bind_ok]
              cases b with
              | true =>
                  rw [if_pos rfl, if_pos rfl, except_pure_def, except_bind_ok]
                  dsimp only
                  rw [except_pure_def, except_bind_ok, hk]
                  try rfl
              | false =>
                  rw [if_neg (by simp), if_neg (by simp), except_pure_def, except_bind_ok]
                  dsimp only
                  exact ih
      | default =>
          rw [show selBody Δ fuel scrut (.mk .default bs t)
                = pure (.yield (none, ())) from rfl,
              show selSpec Δ fuel scrut (.mk .default bs t :: rest) (k (none, ()))
                = selSpec Δ fuel scrut rest (k (none, ())) from rfl,
              except_pure_def, except_bind_ok]
          dsimp only
          exact ih

/-- The selection loop, characterized: `selectTAlt` is `selSpec` with
the original list's first default as the fallback. -/
private theorem selectTAlt_char (Δ : DEnv) (fuel : Nat) (scrut : Val) (alts : List TAlt) :
    Machine.selectTAlt Δ fuel scrut alts
      = selSpec Δ fuel scrut alts
          (match alts.find? (fun | .mk .default _ _ => true | _ => false) with
           | some (.mk _ bs t) => pure (bs, t)
           | none => throw "terminator case: no matching alternative and no default") := by
  rw [Machine.selectTAlt]
  refine Eq.trans (selLoop_char Δ fuel scrut _ ?_ _ ?_ alts) ?_
  · intro alt st
    obtain ⟨con, bs, t⟩ := alt
    cases con with
    | dataAlt c => cases scrut <;> rfl
    | litAlt n => rfl
    | default => rfl
  · intro r
    rfl
  · rfl

/-! ## Environment and store helpers (re-proved; Cexp's are private) -/

private theorem lookup_cons {β : Type} {k k' : Int} {v : β} {l : List (Int × β)} :
    List.lookup k ((k', v) :: l) = if k = k' then some v else List.lookup k l := by
  simp only [List.lookup]
  by_cases h : k = k'
  · simp [h]
  · simp [beq_eq_false_iff_ne.mpr h, h]

private theorem get?_insert {β : Type} {m : HashMap Int β} {k k' : Int} {v : β} :
    (m.insert k v).get? k' = if k' = k then some v else m.get? k' := by
  rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert, HashMap.get?_eq_getElem?]
  by_cases h : k' = k
  · simp [h]
  · simp [h, Ne.symm h]

open Rwv.Eidos.Cexp (EnvC) in
private theorem envC_empty {Δ : DEnv} {σ : String → BV} :
    EnvC Δ σ (∅ : HashMap Int (NF × Ty)) ([] : Eval.Env) := by
  constructor
  · intro x nt h
    rw [HashMap.get?_eq_getElem?] at h
    simp at h
  · intro x _
    rfl

open Rwv.Eidos.Cexp (EnvC) in
private theorem envC_cons {Δ : DEnv} {σ : String → BV} {Γ : HashMap Int (NF × Ty)}
    {env : Eval.Env} (h : EnvC Δ σ Γ env) {u : Int} {n : NF} {t : Ty} {v : Val}
    (hv : VTy Δ v t) (hrep : ∃ k, Val.rep Δ k v = .ok (n.eval σ)) :
    EnvC Δ σ (Γ.insert u (n, t)) ((u, v) :: env) := by
  constructor
  · intro x nt hx
    rw [get?_insert] at hx
    rw [lookup_cons]
    by_cases hxu : x = u
    · rw [if_pos hxu] at hx
      rw [if_pos hxu]
      injection hx with hx
      subst hx
      exact ⟨v, rfl, hv, hrep⟩
    · rw [if_neg hxu] at hx
      rw [if_neg hxu]
      exact h.fwd x nt hx
  · intro x hx
    rw [get?_insert] at hx
    rw [lookup_cons]
    by_cases hxu : x = u
    · rw [if_pos hxu] at hx
      exact absurd hx (by simp)
    · rw [if_neg hxu] at hx
      rw [if_neg hxu]
      exact h.miss x hx

open Rwv.Eidos.Cexp (EnvC) in
/-- The parameter-binding correspondence: a `foldl insert` over a zip
on the symbolic side matches a `foldl prepend` over the values on the
concrete side (both later-wins). -/
private theorem envC_foldl_zip {Δ : DEnv} {σ : String → BV} :
    ∀ (params : List Id) (pas : List (NF × Ty)) (vs : List Val)
      {Γ₀ : HashMap Int (NF × Ty)} {env₀ : Eval.Env},
      EnvC Δ σ Γ₀ env₀ →
      pas.length = params.length → vs.length = params.length →
      (∀ i (h1 : i < params.length) (h2 : i < pas.length) (h3 : i < vs.length),
        VTy Δ (vs[i]'h3) ((pas[i]'h2).2) ∧
        ∃ k, Val.rep Δ k (vs[i]'h3) = .ok ((pas[i]'h2).1.eval σ)) →
      EnvC Δ σ
        ((params.zip pas).foldl (fun m (x, nt) => m.insert x.uniq nt) Γ₀)
        ((params.zip vs).foldl (fun e (p, v) => (p.uniq, v) :: e) env₀) := by
  intro params
  induction params with
  | nil =>
      intro pas vs Γ₀ env₀ h0 _ _ _
      simpa using h0
  | cons p ps ih =>
      intro pas vs Γ₀ env₀ h0 hl1 hl2 hpt
      match pas, vs with
      | [], _ => exact absurd hl1 (by simp)
      | _ :: _, [] => exact absurd hl2 (by simp)
      | nt :: nts, v :: vv =>
          have hhead := hpt 0 (by simp) (by simp) (by simp)
          rw [List.zip_cons_cons, List.zip_cons_cons, List.foldl_cons, List.foldl_cons]
          have hbase : EnvC Δ σ (Γ₀.insert p.uniq nt) ((p.uniq, v) :: env₀) := by
            obtain ⟨n0, t0⟩ := nt
            exact envC_cons h0 (by simpa using hhead.1) (by simpa using hhead.2)
          exact ih nts vv hbase (by simpa using hl1) (by simpa using hl2)
            (fun i h1 h2 h3 => by
              have := hpt (i + 1) (by simpa using h1) (by simpa using h2) (by simpa using h3)
              simpa using this)

private theorem bindFields_nil (env : Eval.Env) (v : Val) :
    Machine.bindFields env v [] = env := by
  cases v <;> rfl

/-- Constructor-field binding as the generic zip fold (the shape
`envC_foldl_zip` consumes). -/
private theorem bindFields_con (env : Eval.Env) (ty : Ty) (c : String)
    (fields : List Val) (bs : List Id) :
    Machine.bindFields env (.con ty c fields) bs
      = (bs.zip fields).foldl (fun e (p, v) => (p.uniq, v) :: e) env := rfl

/-- Block lookup from list membership at distinct label uniques. -/
private theorem blocks_get {p : Proc}
    (hnd : Rwv.Eidos.Cexp.nodupIntB (p.blocks.map (·.1.uniq)) = true)
    {l : Id} {b : Block} (hmem : (l, b) ∈ p.blocks) :
    (HashMap.ofList (p.blocks.map fun (l, b) => (l.uniq, b))).get? l.uniq = some b := by
  have hnd' : (p.blocks.map (·.1.uniq)).Nodup := nodupIntB_nodup hnd
  have hpair : (p.blocks.map fun (l, b) => (l.uniq, b)).Pairwise
      (fun a b => (a.1 == b.1) = false) := by
    have h2 : p.blocks.Pairwise (fun a b => a.1.uniq ≠ b.1.uniq) :=
      (List.pairwise_map).mp hnd'
    refine (List.pairwise_map).mpr (h2.imp ?_)
    intro a b hab
    exact beq_eq_false_iff_ne.mpr (by
      obtain ⟨la, ba⟩ := a
      obtain ⟨lb, bb⟩ := b
      simpa using hab)
  have hpr : (l.uniq, b) ∈ p.blocks.map (fun (l, b) => (l.uniq, b)) :=
    List.mem_map.mpr ⟨(l, b), hmem, rfl⟩
  rw [HashMap.get?_eq_getElem?]
  exact HashMap.getElem?_ofList_of_mem (k_beq := beq_self_eq_true l.uniq) hpair hpr

/-! ## Store and valuation facts (the sigmaOf/union-bias layer) -/

private theorem nodupB_nodup : ∀ {l : List String},
    Rwv.Hyle.Bridge.nodupB l = true → l.Nodup := by
  intro l
  induction l with
  | nil => intro _; exact List.nodup_nil
  | cons x xs ih =>
      intro h
      simp only [Rwv.Hyle.Bridge.nodupB, Bool.and_eq_true, Bool.not_eq_true'] at h
      rw [List.nodup_cons]
      exact ⟨by simpa using h.1, ih h.2⟩

private theorem findSome?_sel_none {β : Type} {k : String} :
    ∀ {l : List (String × β)}, k ∉ l.map Prod.fst →
      l.findSome? (fun p => if p.1 == k then some p.2 else none) = none := by
  intro l
  induction l with
  | nil => intro _; rfl
  | cons p l ih =>
      intro h
      simp only [List.map_cons, List.mem_cons, not_or] at h
      rw [List.findSome?_cons]
      rw [if_neg (by simp only [beq_iff_eq]; exact fun hc => h.1 (hc ▸ rfl))]
      exact ih h.2

private theorem ofList_get?_of_nodup {β : Type} :
    ∀ {l : List (String × β)} {k : String} {v : β},
      (l.map Prod.fst).Nodup → (k, v) ∈ l →
      (HashMap.ofList l).get? k = some v := by
  intro l k v hnd hmem
  rw [HashMap.get?_eq_getElem?, HashMap.ofList_eq_insertMany_empty,
      HashMap.getElem?_insertMany_list, HashMap.getElem?_empty, Option.or_none,
      List.findSomeRev?_eq_findSome?_reverse]
  induction l with
  | nil => exact absurd hmem (by simp)
  | cons p l ih =>
      rw [List.reverse_cons, List.findSome?_append]
      simp only [List.map_cons] at hnd
      have hnd' := List.nodup_cons.mp hnd
      rcases List.mem_cons.mp hmem with hmem | hmem
      · have hknotl : k ∉ l.map Prod.fst := by
          rw [← hmem] at hnd'
          exact hnd'.1
        rw [show l.reverse.findSome? (fun p => if p.1 == k then some p.2 else none)
              = none from by
              refine findSome?_sel_none ?_
              simpa using hknotl]
        simp only [Option.none_or, List.findSome?_cons]
        rw [← hmem]
        simp
      · rw [ih hnd'.2 hmem]
        rfl

/-- The union bias of `Sem.step`'s environment: registers win. -/
private theorem stepEnv_get (inputs : List (String × Nat)) (regs : HashMap String BV)
    (ins : List BV) (x : String) :
    (Rwv.Hyle.Bridge.stepEnv inputs regs ins).get? x
      = (regs.get? x).or ((HashMap.ofList ((inputs.map Prod.fst).zip ins)).get? x) := by
  rw [Rwv.Hyle.Bridge.stepEnv,
      show ∀ (a b : HashMap String BV), a.union b = a ∪ b from fun _ _ => rfl,
      HashMap.get?_eq_getElem?, HashMap.getElem?_union,
      HashMap.get?_eq_getElem?, HashMap.get?_eq_getElem?]

/-- The valuation at a register name is the store's value. -/
private theorem sigmaOf_reg {inputs : List (String × Nat)} {regs : HashMap String BV}
    {ins : List BV} {x : String} {v : BV} (h : regs.get? x = some v) :
    Rwv.Hyle.Bridge.sigmaOf inputs regs ins x = v := by
  rw [Rwv.Hyle.Bridge.sigmaOf, stepEnv_get, h]
  rfl

private theorem zip_fst_sublist {α β : Type} : ∀ (l₁ : List α) (l₂ : List β),
    ((l₁.zip l₂).map Prod.fst).Sublist l₁ := by
  intro l₁
  induction l₁ with
  | nil => intro l₂; simp
  | cons a l ih =>
      intro l₂
      cases l₂ with
      | nil => simp
      | cons b l₂ =>
          rw [List.zip_cons_cons, List.map_cons]
          exact (ih l₂).cons₂ a

/-- The valuation at an input-port name is the stimulus value (the
store holds only register names). -/
private theorem sigmaOf_input {inputs : List (String × Nat)} {regs : HashMap String BV}
    {ins : List BV} {x : String} {v : BV}
    (hnotin : regs.get? x = none)
    (hnd : (inputs.map Prod.fst).Nodup)
    (hmem : (x, v) ∈ (inputs.map Prod.fst).zip ins) :
    Rwv.Hyle.Bridge.sigmaOf inputs regs ins x = v := by
  rw [Rwv.Hyle.Bridge.sigmaOf, stepEnv_get, hnotin, Option.none_or,
      ofList_get?_of_nodup (((zip_fst_sublist _ _).nodup) hnd) hmem]
  rfl

/-! ## Small arithmetic/eval helpers for the selection argument -/

private theorem nbits_zero {n : Nat} (h : nbits n = 0) : n ≤ 1 := by
  rw [nbits] at h
  by_cases hn : n ≤ 1
  · exact hn
  · rw [if_neg hn] at h
    omega

private theorem ofNat_beq_true {w a b : Nat} (ha : a < 2 ^ w) (hb : b < 2 ^ w)
    (h : (BitVec.ofNat w a == BitVec.ofNat w b) = true) : a = b := by
  have h2 : BitVec.ofNat w a = BitVec.ofNat w b := beq_iff_eq.mp h
  have h3 := congrArg BitVec.toNat h2
  rw [BitVec.toNat_ofNat, BitVec.toNat_ofNat, Nat.mod_eq_of_lt ha, Nat.mod_eq_of_lt hb] at h3
  exact h3

private theorem ofNat_beq_false {w a b : Nat} (hne : a ≠ b) (ha : a < 2 ^ w)
    (hb : b < 2 ^ w) : (BitVec.ofNat w a == BitVec.ofNat w b) = false := by
  cases hh : (BitVec.ofNat w a == BitVec.ofNat w b) with
  | false => rfl
  | true => exact absurd (ofNat_beq_true ha hb hh) hne

private theorem ite_eval_of_cond {σ : String → BV} {c t e : NF} {b : Bool}
    (hc : c.eval σ = Rwv.Hyle.Sem.b1 b) :
    (NF.ite c t e).eval σ = if b then t.eval σ else e.eval σ := by
  show (if (c.eval σ).nat ≠ 0 then t.eval σ else e.eval σ) = _
  rw [hc]
  cases b with
  | true => rw [if_pos (by decide), if_pos rfl]
  | false => rw [if_neg (by decide), if_neg (by simp)]

private theorem eq_eval (σ : String → BV) (a b : NF) :
    (NF.prim2 .eq a b).eval σ
      = Rwv.Hyle.Sem.b1 ((a.eval σ).bits == (b.eval σ).bits.setWidth (a.eval σ).width) :=
  rfl

private theorem goAlt1_default {C : Ctx} {N : Nat} {Γ : HashMap Int (NF × Ty)}
    {cells : List CellNF} {dty : Ty} {szT : Nat} {dn : NF} {bs : List Id} {t : Term}
    {macc : Option NF} {r : NF}
    (h : goAlt1 C N Γ cells dty szT dn (.mk .default bs t) macc = .ok r) : False := by
  cases N with
  | zero => rw [goAlt1] at h; exact error_ne_ok h
  | succ N => rw [goAlt1] at h; exact error_ne_ok h

private theorem goAlts_no_default {C : Ctx} :
    ∀ {N : Nat} {Γ : HashMap Int (NF × Ty)} {cells : List CellNF} {dty : Ty} {szT : Nat}
      {dn : NF} {alts : List TAlt} {macc : Option NF} {rec : NF},
      goAlts C N Γ cells dty szT dn alts macc = .ok rec →
      ∀ bs t, TAlt.mk .default bs t ∉ alts := by
  intro N
  induction N with
  | zero =>
      intro Γ cells dty szT dn alts macc rec h
      rw [goAlts] at h
      exact error_ne_ok h
  | succ N ih =>
      intro Γ cells dty szT dn alts macc rec h bs t hmem
      cases alts with
      | nil => exact absurd hmem (by simp)
      | cons alt rest =>
          cases rest with
          | nil =>
              cases macc with
              | none =>
                  rw [show goAlts C (N + 1) Γ cells dty szT dn [alt] none
                        = goAlt1 C N Γ cells dty szT dn alt none from rfl] at h
                  rcases List.mem_cons.mp hmem with hmem | hmem
                  · subst hmem
                    exact goAlt1_default h
                  · exact absurd hmem (by simp)
              | some els =>
                  rw [show goAlts C (N + 1) Γ cells dty szT dn [alt] (some els)
                        = (do
                            let acc ← goAlts C N Γ cells dty szT dn [] (some els)
                            goAlt1 C N Γ cells dty szT dn alt (some acc)) from rfl] at h
                  obtain ⟨acc, hacc, h⟩ := except_bind_eq_ok h
                  rcases List.mem_cons.mp hmem with hmem | hmem
                  · subst hmem
                    exact goAlt1_default h
                  · exact absurd hmem (by simp)
          | cons r2 rs =>
              rw [show goAlts C (N + 1) Γ cells dty szT dn (alt :: r2 :: rs) macc
                    = (do
                        let acc ← goAlts C N Γ cells dty szT dn (r2 :: rs) macc
                        goAlt1 C N Γ cells dty szT dn alt (some acc)) from rfl] at h
              obtain ⟨acc, hacc, h⟩ := except_bind_eq_ok h
              rcases List.mem_cons.mp hmem with hmem | hmem
              · subst hmem
                exact goAlt1_default h
              · exact ih hacc bs t hmem

/-- A canonical value at a datatype head (non-abstract, with the
tested constructor genuinely among its constructors) is a constructor
application carrying the scrutinee type. -/
private theorem vty_con_of_ctorOfB {Δ : DEnv} (hΔ : denvOk Δ = true) {v : Val} {t : Ty}
    {cn : String} (hv : VTy Δ v t) (hab : abstractHead t = false)
    (hcb : ctorOfB Δ t cn = true) : ∃ cv fields, v = .con t cv fields := by
  cases hv with
  | vec hfl _ _ _ =>
      exfalso
      rw [ctorOfB, hfl] at hcb
      dsimp only at hcb
      rw [if_neg (by simp [vec_not_tuple hΔ])] at hcb
      cases hcs : Δ.ctors.get? "Vec" with
      | none => rw [hcs] at hcb; cases hcb
      | some cs =>
          rw [hcs] at hcb
          rw [vec_abstract hΔ hcs] at hcb
          cases hcb
  | integer hfl =>
      exfalso
      rw [abstractHead, hfl] at hab
      simp at hab
  | finite hfl _ =>
      exfalso
      rw [abstractHead, hfl] at hab
      simp at hab
  | proxy hfl _ =>
      exfalso
      rw [abstractHead, hfl] at hab
      simp at hab
  | con hsig hmatch hlen hctor hfields =>
      rename_i c fields sig sub
      exact ⟨c, fields, rfl⟩

/-! ## The machine-step soundness statements -/

/-- The static facts the step induction consumes (assembled from the
layout/plan inversions and the validator's up-front checks). -/
structure SInv (C : Ctx) (plan : Plan) : Prop where
  hden : denvOk C.Δ = true
  outsz : ∃ k, C.Δ.sizeOf k [] C.outTy = .ok C.lo.outW
  tgts : ∀ tgt ∈ C.lo.targets, ∃ blk, C.blocks.get? tgt.uniq = some blk ∧
    tgt.argTys = blk.params.dropLast.map (·.sig.ty) ∧
    ∃ k, tgt.argTys.mapM (C.Δ.sizeOf k []) = .ok tgt.argWs
  paybound : ∀ tgt ∈ C.lo.targets, tgt.argWs.sum ≤ C.lo.rPayW
  cellsz : ∀ c ∈ plan.cells, ∃ k, C.Δ.sizeOf k [] c.ty = .ok c.width
  cellsw : (plan.cells.map (·.width)).sum = C.lo.cellsW

/-- The symbolic cell store corresponds to the concrete one: same
shape as the plan's cells, and every entry's normal form denotes the
concrete cell value's representation. -/
def CellsC (C : Ctx) (plan : Plan) (σ : String → BV) (cells : List CellNF)
    (store : HashMap String Val) : Prop :=
  cells.map (fun d => (d.name, d.ty, d.width))
      = plan.cells.map (fun c => (c.name, c.ty, c.width)) ∧
  (cells.map (·.name)).Nodup ∧
  ∀ d ∈ cells, ∃ v k, store.get? d.name = some v ∧ VTy C.Δ v d.ty ∧
    Val.rep C.Δ k v = .ok (d.nf.eval σ) ∧ (d.nf.eval σ).width = d.width

/-- The step-record correspondence: what a compiled record's value
says about a machine-step outcome. A halt is unconstrained (the
schema's `SimP` never inspects the right machine on a left halt); a
pause pins the output field, the resumption-tag field, and the cell
fields of the record to the next state's encoding. -/
def StepValC (C : Ctx) (plan : Plan) (σ : String → BV) (rv : BV) : StepOut → Prop
  | .halt _ => True
  | .step o s' =>
      VTy C.Δ o C.outTy ∧
      (∃ bo k, Val.rep C.Δ k o = .ok bo ∧ bo.width = C.lo.outW ∧
        sliceBV rv (C.lo.cellsW + C.lo.rW) C.lo.outW = bo) ∧
      ∃ tgt, tgt ∈ C.lo.targets ∧ tgt.uniq = s'.label ∧
        s'.args.length = tgt.argTys.length ∧
        (∀ pr ∈ tgt.argTys.zip s'.args, VTy C.Δ pr.2 pr.1) ∧
        (∃ reps k, s'.args.mapM (Val.rep C.Δ k) = .ok reps ∧
          reps.map (·.width) = tgt.argWs ∧
          sliceBV rv C.lo.cellsW C.lo.rW = encTag C.lo tgt.tag tgt.argWs reps) ∧
        (∀ pr ∈ plan.cells.zip (offsetsOf (plan.cells.map (·.width))),
          ∃ v bv k, s'.cells.get? pr.1.name = some v ∧ VTy C.Δ v pr.1.ty ∧
            Val.rep C.Δ k v = .ok bv ∧ bv.width = pr.1.width ∧
            sliceBV rv pr.2 pr.1.width = bv)

/-- Soundness of the command compiler at a fuel. -/
def PCmds (C : Ctx) (plan : Plan) (σ : String → BV) (N : Nat) : Prop :=
  ∀ Γ cells cmds term rec env store ef gf so,
    goCmds C N Γ cells cmds term = .ok rec →
    Rwv.Eidos.Cexp.EnvC C.Δ σ Γ env →
    CellsC C plan σ cells store →
    (do
      let (env', store') ← Machine.runCmds C.Δ C.edm ef env store cmds
      Machine.execBlock.runTerm C.Δ C.edm C.blocks ef gf env' store' term) = .ok so →
    StepValC C plan σ (rec.eval σ) so

/-- Soundness of the terminator compiler at a fuel. -/
def PTerm (C : Ctx) (plan : Plan) (σ : String → BV) (N : Nat) : Prop :=
  ∀ Γ cells term rec env store ef gf so,
    goTerm C N Γ cells term = .ok rec →
    Rwv.Eidos.Cexp.EnvC C.Δ σ Γ env →
    CellsC C plan σ cells store →
    Machine.execBlock.runTerm C.Δ C.edm C.blocks ef gf env store term = .ok so →
    StepValC C plan σ (rec.eval σ) so

/-- Soundness of the terminator if-chain at a fuel. -/
def PAlts (C : Ctx) (plan : Plan) (σ : String → BV) (N : Nat) : Prop :=
  ∀ Γ cells dty szT dn alts macc rec env store sv
    (after : Except String (List Id × Term)) ef gf so,
    goAlts C N Γ cells dty szT dn alts macc = .ok rec →
    Rwv.Eidos.Cexp.EnvC C.Δ σ Γ env →
    CellsC C plan σ cells store →
    VTy C.Δ sv dty →
    (∃ ks, Val.rep C.Δ ks sv = .ok (dn.eval σ)) →
    (∃ kt, C.Δ.sizeOf kt [] dty = .ok szT) →
    (∀ els, macc = some els → ∀ bs t', after = .ok (bs, t') → bs = [] ∧
       ∀ ef' gf' so',
         Machine.execBlock.runTerm C.Δ C.edm C.blocks ef' gf' env store t' = .ok so' →
         StepValC C plan σ (els.eval σ) so') →
    (macc = none → ∀ bs t', after ≠ .ok (bs, t')) →
    ∀ bs t', selSpec C.Δ ef sv alts after = .ok (bs, t') →
    Machine.execBlock.runTerm C.Δ C.edm C.blocks ef gf
      (Machine.bindFields env sv bs) store t' = .ok so →
    StepValC C plan σ (rec.eval σ) so

/-- Soundness of a single chain link at a fuel. -/
def PAlt1 (C : Ctx) (plan : Plan) (σ : String → BV) (N : Nat) : Prop :=
  ∀ Γ cells dty szT dn alt rest macc bnf env store sv
    (after : Except String (List Id × Term)) ef gf so,
    goAlt1 C N Γ cells dty szT dn alt macc = .ok bnf →
    Rwv.Eidos.Cexp.EnvC C.Δ σ Γ env →
    CellsC C plan σ cells store →
    VTy C.Δ sv dty →
    (∃ ks, Val.rep C.Δ ks sv = .ok (dn.eval σ)) →
    (∃ kt, C.Δ.sizeOf kt [] dty = .ok szT) →
    (macc = none → rest = [] ∧ ∀ bs t', after ≠ .ok (bs, t')) →
    (∀ acc, macc = some acc → ∀ bs t' so',
       selSpec C.Δ ef sv rest after = .ok (bs, t') →
       Machine.execBlock.runTerm C.Δ C.edm C.blocks ef gf
         (Machine.bindFields env sv bs) store t' = .ok so' →
       StepValC C plan σ (acc.eval σ) so') →
    ∀ bs t', selSpec C.Δ ef sv (alt :: rest) after = .ok (bs, t') →
    Machine.execBlock.runTerm C.Δ C.edm C.blocks ef gf
      (Machine.bindFields env sv bs) store t' = .ok so →
    StepValC C plan σ (bnf.eval σ) so

private theorem pcmds_zero {C : Ctx} {plan : Plan} {σ : String → BV} :
    PCmds C plan σ 0 := by
  intro Γ cells cmds term rec env store ef gf so h
  rw [goCmds] at h
  exact error_ne_ok h

private theorem pterm_zero {C : Ctx} {plan : Plan} {σ : String → BV} :
    PTerm C plan σ 0 := by
  intro Γ cells term rec env store ef gf so h
  rw [goTerm] at h
  exact error_ne_ok h

private theorem palts_zero {C : Ctx} {plan : Plan} {σ : String → BV} :
    PAlts C plan σ 0 := by
  intro Γ cells dty szT dn alts macc rec env store sv after ef gf so h
  rw [goAlts] at h
  exact error_ne_ok h

private theorem palt1_zero {C : Ctx} {plan : Plan} {σ : String → BV} :
    PAlt1 C plan σ 0 := by
  intro Γ cells dty szT dn alt rest macc bnf env store sv after ef gf so h
  rw [goAlt1] at h
  exact error_ne_ok h

private theorem nodup_map_mem_eq {α : Type} {f : α → String} :
    ∀ {l : List α}, (l.map f).Nodup → ∀ {a b : α}, a ∈ l → b ∈ l → f a = f b → a = b := by
  intro l
  induction l with
  | nil => intro _ a b ha; exact absurd ha (by simp)
  | cons x xs ih =>
      intro hnd a b ha hb hf
      rw [List.map_cons, List.nodup_cons] at hnd
      rcases List.mem_cons.mp ha with ha | ha
      · rcases List.mem_cons.mp hb with hb | hb
        · rw [ha, hb]
        · exfalso
          refine hnd.1 (List.mem_map.mpr ⟨b, hb, ?_⟩)
          rw [← hf, ha]
      · rcases List.mem_cons.mp hb with hb | hb
        · exfalso
          refine hnd.1 (List.mem_map.mpr ⟨a, ha, ?_⟩)
          rw [hf, hb]
        · exact ih hnd.2 ha hb hf

/-- The command step: bind extends both environments, get reads the
corresponding cell, put updates the corresponding cell. -/
private theorem pcmds_step {C : Ctx} {plan : Plan} {σ : String → BV} {N : Nat}
    (hS : SInv C plan) (hterm : PTerm C plan σ N) (hcmds : PCmds C plan σ N) :
    PCmds C plan σ (N + 1) := by
  intro Γ cells cmds term rec env store ef gf so hgo hE hC hconc
  cases cmds with
  | nil =>
      rw [show goCmds C (N + 1) Γ cells [] term = goTerm C N Γ cells term from rfl] at hgo
      rw [show Machine.runCmds C.Δ C.edm ef env store [] = pure (env, store) from rfl,
          except_pure_def, except_bind_ok] at hconc
      exact hterm Γ cells term rec env store ef gf so hgo hE hC hconc
  | cons cmd rest =>
      obtain ⟨st', hcmds1, hkont⟩ := except_bind_eq_ok hconc
      obtain ⟨env', store'⟩ := st'
      rw [Machine.runCmds, List.foldlM_cons] at hcmds1
      obtain ⟨st1, hbody, hfold⟩ := except_bind_eq_ok hcmds1
      obtain ⟨env₁, store₁⟩ := st1
      have hrest : (do
          let (env', store') ← Machine.runCmds C.Δ C.edm ef env₁ store₁ rest
          Machine.execBlock.runTerm C.Δ C.edm C.blocks ef gf env' store' term)
          = .ok so := by
        rw [Machine.runCmds, hfold, except_bind_ok]
        exact hkont
      cases cmd with
      | bind x e =>
          rw [show goCmds C (N + 1) Γ cells (.bind x e :: rest) term = (do
              let nt ← Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ e
              goCmds C N (Γ.insert x.uniq nt) cells rest term) from rfl] at hgo
          obtain ⟨nt, hnt, hgo⟩ := except_bind_eq_ok hgo
          obtain ⟨nf₁, ty₁⟩ := nt
          -- the concrete body: evaluate, prepend
          obtain ⟨v, heval, hbody⟩ := except_bind_eq_ok hbody
          rw [except_pure_def] at hbody
          injection hbody with hbody
          injection hbody with hb1 hb2
          subst hb1; subst hb2
          obtain ⟨hvty, k, hrep⟩ := Rwv.Eidos.Cexp.cexpFull_sound hS.hden C.cexpFuel Γ e
            nf₁ ty₁ ef env [] v hnt heval hE
          exact hcmds _ cells rest term rec _ store ef gf so hgo
            (envC_cons hE hvty ⟨k, hrep⟩) hC hrest
      | get x cname =>
          rw [show goCmds C (N + 1) Γ cells (.get x cname :: rest) term
                = (match cells.find? (fun d => d.name == cname) with
                   | some d => goCmds C N (Γ.insert x.uniq (d.nf, d.ty)) cells rest term
                   | none => throw s!"cstep: get from unknown cell {cname}")
              from rfl] at hgo
          cases hfd : cells.find? (fun d => d.name == cname) with
          | none => rw [hfd] at hgo; exact error_ne_ok hgo
          | some d =>
              rw [hfd] at hgo
              have hdmem : d ∈ cells := List.mem_of_find?_eq_some hfd
              have hdname : d.name = cname := by
                have := List.find?_some hfd
                simpa using this
              obtain ⟨v₀, k₀, hget, hvty, hrep, hwidth⟩ := hC.2.2 d hdmem
              -- the concrete body reads the same cell
              have hbody' : (match store.get? cname with
                  | some v => pure (((x.uniq, v) :: env), store)
                  | none => (throw s!"get: unknown cell {cname}"
                      : Except String (Eval.Env × HashMap String Val)))
                  = .ok (env₁, store₁) := hbody
              rw [← hdname, hget] at hbody'
              dsimp only at hbody'
              rw [except_pure_def] at hbody'
              injection hbody' with hbody'
              injection hbody' with hb1 hb2
              subst hb1; subst hb2
              exact hcmds _ cells rest term rec _ store ef gf so hgo
                (envC_cons hE hvty ⟨k₀, hrep⟩) hC hrest
      | put cname e =>
          rw [show goCmds C (N + 1) Γ cells (.put cname e :: rest) term = (do
              let (nf, ty) ← Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ e
              match cells.find? (fun d => d.name == cname) with
              | some d =>
                  if teq ty d.ty then
                    goCmds C N Γ
                      (cells.map fun d' => if d'.name == cname then { d' with nf } else d')
                      rest term
                  else throw s!"cstep: put to cell {cname} at the wrong type"
              | none => throw s!"cstep: put to unknown cell {cname}") from rfl] at hgo
          obtain ⟨nt, hnt, hgo⟩ := except_bind_eq_ok hgo
          obtain ⟨nf₁, ty₁⟩ := nt
          cases hfd : cells.find? (fun d => d.name == cname) with
          | none => rw [hfd] at hgo; exact error_ne_ok hgo
          | some d =>
              rw [hfd] at hgo
              dsimp only at hgo
              split at hgo
              rotate_left
              · exact error_ne_ok hgo
              rename_i hteq
              have hdmem : d ∈ cells := List.mem_of_find?_eq_some hfd
              have hdname : d.name = cname := by
                have := List.find?_some hfd
                simpa using this
              -- the concrete body: evaluate, insert
              obtain ⟨v, heval, hbody⟩ := except_bind_eq_ok hbody
              rw [except_pure_def] at hbody
              injection hbody with hbody
              injection hbody with hb1 hb2
              subst hb1; subst hb2
              obtain ⟨hvty, k, hrep⟩ := Rwv.Eidos.Cexp.cexpFull_sound hS.hden C.cexpFuel Γ e
                nf₁ ty₁ ef env [] v hnt heval hE
              have htyd : ty₁ = d.ty := Rwv.Eidos.Cexp.teq_eq hteq
              subst htyd
              -- the updated symbolic cells still correspond
              refine hcmds Γ _ rest term rec env _ ef gf so hgo hE ⟨?_, ?_, ?_⟩ hrest
              · rw [show (cells.map fun d' =>
                      if d'.name == cname then { d' with nf := nf₁ } else d').map
                        (fun d => (d.name, d.ty, d.width))
                    = cells.map (fun d => (d.name, d.ty, d.width)) from by
                    rw [List.map_map]
                    refine List.map_congr_left ?_
                    intro a _
                    by_cases hna : (a.name == cname) = true
                    · simp only [Function.comp, hna, if_pos]
                    · simp only [Function.comp, hna]
                      rw [if_neg (by simp [hna])]]
                exact hC.1
              · rw [show (cells.map fun d' =>
                      if d'.name == cname then { d' with nf := nf₁ } else d').map (·.name)
                    = cells.map (·.name) from by
                    rw [List.map_map]
                    refine List.map_congr_left ?_
                    intro a _
                    by_cases hna : (a.name == cname) = true
                    · simp only [Function.comp, hna, if_pos]
                    · simp only [Function.comp, hna]
                      rw [if_neg (by simp [hna])]]
                exact hC.2.1
              · intro d' hd'
                obtain ⟨d₀, hd₀, hupd⟩ := List.mem_map.mp hd'
                by_cases hn0 : (d₀.name == cname) = true
                · rw [if_pos hn0] at hupd
                  have hd0d : d₀ = d :=
                    nodup_map_mem_eq hC.2.1 hd₀ hdmem
                      (by rw [eq_of_beq hn0, hdname])
                  subst hd0d
                  subst hupd
                  refine ⟨v, k, ?_, hvty, hrep, ?_⟩
                  · show ((store.insert cname v).get? d₀.name) = some v
                    rw [hdname, HashMap.get?_eq_getElem?]
                    exact HashMap.getElem?_insert_self
                  · -- the new width, from the cell's declared size
                    show (nf₁.eval σ).width = d₀.width
                    have hd₀tr : (d₀.name, d₀.ty, d₀.width)
                        ∈ plan.cells.map (fun c => (c.name, c.ty, c.width)) := by
                      rw [← hC.1]
                      exact List.mem_map.mpr ⟨d₀, hd₀, rfl⟩
                    obtain ⟨c, hcm, hctr⟩ := List.mem_map.mp hd₀tr
                    obtain ⟨kk, hsz⟩ := hS.cellsz c hcm
                    injection hctr with h1 h2
                    injection h2 with h2 h3
                    rw [h2] at hsz
                    rw [← h3]
                    exact vty_rep_width hvty hrep hsz
                · rw [if_neg (by simp only [Bool.not_eq_true] at hn0; simp [hn0])] at hupd
                  subst hupd
                  obtain ⟨v₀, k₀, hget, hvty₀, hrep₀, hw₀⟩ := hC.2.2 d₀ hd₀
                  refine ⟨v₀, k₀, ?_, hvty₀, hrep₀, hw₀⟩
                  rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert,
                      if_neg (by
                        simp only [beq_iff_eq]
                        intro hc
                        rw [← hc] at hn0
                        simp at hn0), ← HashMap.get?_eq_getElem?]
                  exact hget

private theorem len_le_one_mem_eq {α : Type} {l : List α} (h : l.length ≤ 1)
    {a b : α} (ha : a ∈ l) (hb : b ∈ l) : a = b := by
  cases l with
  | nil => exact absurd ha (by simp)
  | cons x xs =>
      cases xs with
      | nil =>
          have h1 : a = x := by simpa using ha
          have h2 : b = x := by simpa using hb
          rw [h1, h2]
      | cons y ys => simp at h

/-- One chain link: the compiled tag test fires exactly when the
machine's constructor-name (or literal) match does. -/
private theorem palt1_step {C : Ctx} {plan : Plan} {σ : String → BV} {N : Nat}
    (hS : SInv C plan) (hterm : PTerm C plan σ N) :
    PAlt1 C plan σ (N + 1) := by
  intro Γ cells dty szT dn alt rest macc bnf env store sv after ef gf so
    hgo hE hC hvty hrepE hszE hnone hcont bs t' hsel hrun
  obtain ⟨ks, hks⟩ := hrepE
  obtain ⟨kt, hkt⟩ := hszE
  have hw : (dn.eval σ).width = szT := vty_rep_width hvty hks hkt
  obtain ⟨con, abs, at'⟩ := alt
  cases con with
  | default => exact (goAlt1_default hgo).elim
  | litAlt n =>
      rw [goAlt1] at hgo
      split at hgo
      rotate_left
      · exact error_ne_ok hgo
      rename_i habs
      obtain ⟨bnf', hbnf', hgoL⟩ := except_bind_eq_ok hgo
      clear hgo
      have habs' : abs = [] := List.isEmpty_iff.mp habs
      subst habs'
      -- the concrete literal test
      rw [show selSpec C.Δ ef sv (.mk (.litAlt n) [] at' :: rest) after
            = (Eval.litMatches C.Δ ef sv n >>= fun b =>
                if b = true then pure ([], at')
                else selSpec C.Δ ef sv rest after) from rfl] at hsel
      obtain ⟨b, hlm, hsel⟩ := except_bind_eq_ok hsel
      rw [Eval.litMatches] at hlm
      obtain ⟨x, hx, hlm⟩ := except_bind_eq_ok hlm
      rw [except_pure_def] at hlm
      injection hlm with hlm
      have hxd : x = dn.eval σ := rep_det hx hks
      subst hxd
      subst hlm
      -- the compiled test's value
      have hcondv : (NF.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT n⟩)).eval σ
          = Rwv.Hyle.Sem.b1 ((dn.eval σ).bits == BitVec.ofInt (dn.eval σ).width n) := by
        rw [eq_eval]
        rw [show ((NF.lit ⟨szT, BitVec.ofInt szT n⟩).eval σ) = ⟨szT, BitVec.ofInt szT n⟩
              from rfl]
        rw [show (BV.mk szT (BitVec.ofInt szT n)).bits.setWidth (dn.eval σ).width
              = BitVec.ofInt (dn.eval σ).width n by
            rw [hw]
            exact BitVec.setWidth_eq _]
      cases hb : ((dn.eval σ).bits == BitVec.ofInt (dn.eval σ).width n) with
      | true =>
          rw [hb, if_pos rfl, except_pure_def] at hsel
          injection hsel with hsel
          injection hsel with hs1 hs2
          subst hs1; subst hs2
          rw [bindFields_nil] at hrun
          have hmain := hterm Γ cells at' bnf' env store ef gf so hbnf' hE hC hrun
          cases macc with
          | none =>
              rw [except_pure_def] at hgoL
              injection hgoL with hgoL
              subst hgoL
              exact hmain
          | some acc =>
              rw [except_pure_def] at hgoL
              injection hgoL with hgoL
              subst hgoL
              rw [show (NF.ite (.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT n⟩)) bnf' acc).eval σ
                    = if true then bnf'.eval σ else acc.eval σ from
                    ite_eval_of_cond (by rw [hcondv, hb])]
              rw [if_pos rfl]
              exact hmain
      | false =>
          rw [hb, if_neg (by simp)] at hsel
          cases macc with
          | none =>
              obtain ⟨hrest0, hne⟩ := hnone rfl
              subst hrest0
              exact absurd hsel (hne bs t')
          | some acc =>
              have hmain := hcont acc rfl bs t' so hsel hrun
              rw [except_pure_def] at hgoL
              injection hgoL with hgoL
              subst hgoL
              rw [show (NF.ite (.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT n⟩)) bnf' acc).eval σ
                    = if false then bnf'.eval σ else acc.eval σ from
                    ite_eval_of_cond (by rw [hcondv, hb])]
              rw [if_neg (by simp)]
              exact hmain
  | dataAlt cn =>
      rw [goAlt1] at hgo
      split at hgo
      · exact error_ne_ok hgo
      rename_i hab
      have hab' : abstractHead dty = false := by
        cases hh : abstractHead dty with
        | false => rfl
        | true => exact absurd hh hab
      split at hgo
      rotate_left
      · exact error_ne_ok hgo
      rename_i hcb
      obtain ⟨tg, htag, hgo1⟩ := except_bind_eq_ok hgo
      obtain ⟨tag, w⟩ := tg
      cases hsig2 : C.Δ.ctorSig.get? cn with
      | none => rw [hsig2] at hgo1; dsimp only at hgo1; exact error_ne_ok hgo1
      | some sig =>
      rw [hsig2] at hgo1
      dsimp only at hgo1
      obtain ⟨sub, hsub2, hgo2⟩ := except_bind_eq_ok hgo1
      clear hgo hgo1
      split at hgo2
      rotate_left
      · exact error_ne_ok hgo2
      rename_i hxlen
      obtain ⟨szXs, hszXs, hgo3⟩ := except_bind_eq_ok hgo2
      clear hgo2
      split at hgo3
      rotate_left
      · exact error_ne_ok hgo3
      rename_i hwle
      obtain ⟨bnf', hbnf', hgo4⟩ := except_bind_eq_ok hgo3
      clear hgo3
      -- the scrutinee is a constructor value at the scrutinee type
      obtain ⟨cv, fields, hsv⟩ := vty_con_of_ctorOfB hS.hden hvty hab' hcb
      subst hsv
      cases hvty with
      | con hsigv hmatchv hlenv hctorv hfieldsv =>
      rename_i sigv subv
      -- the representation, dissected
      obtain ⟨ks', whole, tagv, tagWv, bsR, hks1, hwhole, htagv, hbs, hguard, hbv⟩ :=
        rep_con_inv hks
      have hwhsz : whole = szT := sizeOf_det hwhole hkt
      rw [hwhsz] at hguard hbv
      -- the two constructors live in the same head datatype
      obtain ⟨tc, argsT, hflT, hdisj⟩ := ctorTag_inv htag
      obtain ⟨tc', argsT', hflT', hdisj'⟩ := ctorTag_inv htagv
      rw [hflT] at hflT'
      have htcc : tc = tc' := by
        have := congrArg Prod.fst hflT'
        simpa using this
      subst htcc
      -- membership of both constructor names
      have hcnOf : Rwv.Eidos.Cexp.ctorOf C.Δ dty cn := ctorOfB_sound hcb
      rw [Rwv.Eidos.Cexp.ctorOf, hflT] at hcnOf hctorv
      dsimp only at hcnOf hctorv
      -- tag widths agree, and the tags decide constructor equality
      have hkey : tagWv = w ∧ (cn = cv ↔ tagv = tag) ∧
          (w = 0 → cn = cv) ∧ (∀ w0, w = w0 + 1 → tag < 2 ^ w ∧ tagv < 2 ^ w) := by
        rcases hdisj with ⟨htup, htag0, hw0⟩ | ⟨htup, cs, hcs, hidxn, hwn⟩
        · rcases hdisj' with ⟨_, htagv0, hwv0⟩ | ⟨htup', _, _, _⟩
          · subst htag0; subst hw0; subst htagv0; subst hwv0
            rw [if_pos htup] at hcnOf hctorv
            exact ⟨rfl, ⟨fun _ => rfl, fun _ => hcnOf.trans hctorv.symm⟩,
              fun _ => hcnOf.trans hctorv.symm, fun w0 hw0 => absurd hw0 (by omega)⟩
          · exact absurd htup (by simp [htup'])
        · rcases hdisj' with ⟨htup', _, _⟩ | ⟨_, cs', hcs', hidxv, hwv⟩
          · exact absurd htup' (by simp [htup])
          · rw [hcs] at hcs'
            injection hcs' with hcs'
            subst hcs'
            subst hwn; subst hwv
            rw [if_neg (by simp [htup])] at hcnOf hctorv
            have hcnmem : cn ∈ cs := by
              obtain ⟨cs2, hcs2, hmem⟩ := hcnOf
              rw [hcs] at hcs2
              injection hcs2 with hcs2
              subst hcs2
              exact hmem
            have hcvmem : cv ∈ cs := by
              obtain ⟨cs2, hcs2, hmem⟩ := hctorv
              rw [hcs] at hcs2
              injection hcs2 with hcs2
              subst hcs2
              exact hmem
            refine ⟨rfl, ⟨?_, ?_⟩, ?_, ?_⟩
            · intro hcc
              subst hcc
              rw [hidxn] at hidxv
              injection hidxv with h
              exact h.symm
            · intro htt
              exact (idxOf?_inj hidxv (htt ▸ hidxn)).symm
            · intro hw0
              exact len_le_one_mem_eq (nbits_zero hw0) hcnmem hcvmem
            · intro w0 hw0
              exact ⟨Nat.lt_of_lt_of_le (idxOf?_lt hidxn) (nbits_le _),
                     Nat.lt_of_lt_of_le (idxOf?_lt hidxv) (nbits_le _)⟩
      obtain ⟨hww, hcniff, hw0cc, hbounds⟩ := hkey
      subst hww
      -- the tag slice of the discriminant
      have hFw : (bvCat (⟨szT - tagWv - (Val.bvConcat bsR).width, 0⟩ : BV)
          (Val.bvConcat bsR)).width = szT - tagWv := by
        rw [bvCat_width]
        show szT - tagWv - (Val.bvConcat bsR).width + (Val.bvConcat bsR).width
          = szT - tagWv
        omega
      have hslice : sliceBV (dn.eval σ) (szT - tagWv) tagWv
          = (⟨tagWv, BitVec.ofNat tagWv tagv⟩ : BV) := by
        rw [hbv, bvConcat3, sliceBV_cat_high (Nat.le_of_eq hFw), hFw, Nat.sub_self]
        exact sliceBV_all _
      -- the concrete selection at a constructor scrutinee
      rw [show selSpec C.Δ ef (.con dty cv fields) (.mk (.dataAlt cn) abs at' :: rest) after
            = (if cn = cv then .ok (abs, at')
               else selSpec C.Δ ef (.con dty cv fields) rest after) from rfl] at hsel
      by_cases hcc : cn = cv
      · -- selected: bind the fields, run the branch
        rw [if_pos hcc] at hsel
        injection hsel with hsel
        injection hsel with hs1 hs2
        subst hs1; subst hs2
        -- the signatures and substitutions coincide
        rw [hcc] at hsig2
        rw [hsigv] at hsig2
        injection hsig2 with hsig2
        subst hsig2
        have hsubv : subv = sub := by
          rw [hmatchv] at hsub2
          exact Except.ok.inj hsub2
        subst hsubv
        -- field widths at the checked sizes
        obtain ⟨hbslen, hbspt⟩ := mapM_ok_idx hbs
        obtain ⟨hszlen, hszpt⟩ := mapM_ok_idx hszXs
        have hlenf : fields.length = (Ty.flattenArrow sigv.ty).1.length := hlenv
        have hlenx : abs.length
            = ((Ty.flattenArrow sigv.ty).1.map (DEnv.substTv subv)).length := hxlen
        have hfw : ∀ i (hi : i < bsR.length),
            bsR[i].width = szXs[i]'(by
              rw [hszlen, List.length_map]
              omega) := by
          intro i hi
          obtain ⟨_, hri⟩ := hbspt i (by omega)
          obtain ⟨_, hsi⟩ := hszpt i (by rw [List.length_map]; omega)
          rw [show ((Ty.flattenArrow sigv.ty).1.map (DEnv.substTv subv))[i]'(by
                rw [List.length_map]; omega)
              = DEnv.substTv subv ((Ty.flattenArrow sigv.ty).1[i]'(by omega))
              from List.getElem_map _] at hsi
          have hfv : VTy C.Δ (fields[i]'(by omega))
              (DEnv.substTv subv ((Ty.flattenArrow sigv.ty).1[i]'(by omega))) := by
            refine hfieldsv (((Ty.flattenArrow sigv.ty).1[i]'(by omega)),
              fields[i]'(by omega)) ?_
            rw [show (((Ty.flattenArrow sigv.ty).1[i]'(by omega)), fields[i]'(by omega))
                  = ((Ty.flattenArrow sigv.ty).1.zip fields)[i]'(by
                    rw [List.length_zip]; omega)
                from List.getElem_zip.symm]
            exact List.getElem_mem _
          exact vty_rep_width hfv hri hsi
        have hsum : (Val.bvConcat bsR).width = szXs.sum := by
          rw [bvConcat_eq, catAll_width]
          congr 1
          refine List.ext_getElem (by rw [List.length_map, hbslen, hszlen, List.length_map,
            hlenf]) ?_
          intro i h1 h2
          rw [List.getElem_map]
          exact hfw i (by rw [List.length_map] at h1; omega)
        -- the extended environment corresponds
        have hE' : Rwv.Eidos.Cexp.EnvC C.Δ σ
            ((abs.zip ((((szXs.zip (offsetsOf szXs)).map fun (sz, off) =>
                sliceNF off sz dn).zip
                ((Ty.flattenArrow sigv.ty).1.map (DEnv.substTv subv))))).foldl
              (fun m (x, nt) => m.insert x.uniq nt) Γ)
            ((abs.zip fields).foldl (fun e (p, v) => (p.uniq, v) :: e) env) := by
          refine envC_foldl_zip abs _ fields hE ?_ ?_ ?_
          · simp only [List.length_zip, List.length_map, offsetsOf_length, Nat.min_self]
            rw [List.length_map] at hlenx
            rw [hszlen, List.length_map]
            omega
          · rw [List.length_map] at hlenx
            omega
          · intro i h1 h2 h3
            have hi1 : i < (Ty.flattenArrow sigv.ty).1.length := by omega
            have hi2 : i < szXs.length := by
              rw [hszlen, List.length_map]
              omega
            have hpa : (((szXs.zip (offsetsOf szXs)).map fun (sz, off) =>
                  sliceNF off sz dn).zip
                  ((Ty.flattenArrow sigv.ty).1.map (DEnv.substTv subv)))[i]'(by
                    rw [List.length_zip, List.length_map, List.length_zip,
                      offsetsOf_length, Nat.min_self, List.length_map]
                    omega)
                = (sliceNF ((offsetsOf szXs)[i]'(by rw [offsetsOf_length]; omega))
                     (szXs[i]'hi2) dn,
                   DEnv.substTv subv ((Ty.flattenArrow sigv.ty).1[i]'hi1)) := by
              rw [List.getElem_zip, List.getElem_map, List.getElem_map,
                  List.getElem_zip]
            rw [hpa]
            constructor
            · refine hfieldsv (((Ty.flattenArrow sigv.ty).1[i]'hi1), fields[i]'h3) ?_
              rw [show (((Ty.flattenArrow sigv.ty).1[i]'hi1), fields[i]'h3)
                    = ((Ty.flattenArrow sigv.ty).1.zip fields)[i]'(by
                      rw [List.length_zip]; omega)
                  from List.getElem_zip.symm]
              exact List.getElem_mem _
            · refine ⟨ks', ?_⟩
              obtain ⟨_, hri⟩ := hbspt i (by omega)
              rw [hri]
              congr 1
              rw [sliceNF_eval, hbv, bvConcat3]
              rw [sliceBV_cat_low (by
                rw [bvCat_width, hsum]
                have := drop_sum_le (l := szXs) (i := i) (by omega)
                rw [offsetsOf_getElem _ i (by omega)]
                have hpadw : (BV.mk (szT - tagWv - (Val.bvConcat bsR).width) 0).width
                    = szT - tagWv - (Val.bvConcat bsR).width := rfl
                omega)]
              rw [sliceBV_cat_low (by
                rw [hsum, offsetsOf_getElem _ i (by omega)]
                exact drop_sum_le (by omega))]
              have hidx := catAll_extract_idx bsR i (by omega)
              rw [offsetsOf_getElem _ i (by rw [List.length_map]; omega)] at hidx
              rw [offsetsOf_getElem _ i (by omega)]
              rw [show ((bsR.map (fun x => x.width)).drop (i + 1)).sum
                    = ((szXs.drop (i + 1)).sum) from by
                  congr 1
                  refine List.ext_getElem (by
                    rw [List.length_drop, List.length_drop, List.length_map, hbslen,
                        hszlen, List.length_map, hlenf]) ?_
                  intro j hj1 hj2
                  rw [List.getElem_drop, List.getElem_drop, List.getElem_map]
                  exact hfw (i + 1 + j) (by
                    rw [List.length_drop, List.length_map] at hj1
                    omega)] at hidx
              rw [show (bsR[i]'(by omega)).width = szXs[i]'hi2 from hfw i (by omega)] at hidx
              rw [bvConcat_eq, hidx]
        -- run the branch
        rw [bindFields_con] at hrun
        have hmain := hterm _ cells at' bnf' _ store ef gf so hbnf' hE' hC hrun
        -- assemble the compiled link's value
        rcases macc with _ | acc
        · cases tagWv with
          | zero =>
              dsimp only at hgo4
              rw [except_pure_def] at hgo4
              injection hgo4 with hgo4
              subst hgo4
              exact hmain
          | succ w0 =>
              dsimp only at hgo4
              rw [except_pure_def] at hgo4
              injection hgo4 with hgo4
              subst hgo4
              exact hmain
        · cases tagWv with
          | zero =>
              dsimp only at hgo4
              rw [except_pure_def] at hgo4
              injection hgo4 with hgo4
              subst hgo4
              exact hmain
          | succ w0 =>
              dsimp only at hgo4
              rw [except_pure_def] at hgo4
              injection hgo4 with hgo4
              subst hgo4
              -- the test fires: same constructor, same tag
              have htt : tagv = tag := hcniff.1 hcc
              subst htt
              have hcond : (NF.prim2 .eq (sliceNF (szT - (w0 + 1)) (w0 + 1) dn)
                  (.lit ⟨w0 + 1, BitVec.ofNat (w0 + 1) tagv⟩)).eval σ
                  = Rwv.Hyle.Sem.b1 ((BitVec.ofNat (w0 + 1) tagv)
                      == BitVec.ofNat (w0 + 1) tagv) := by
                rw [eq_eval, sliceNF_eval, hslice]
                show Rwv.Hyle.Sem.b1 ((BitVec.ofNat (w0 + 1) tagv)
                    == (BitVec.ofNat (w0 + 1) tagv).setWidth (w0 + 1)) = _
                rw [BitVec.setWidth_eq]
              rw [show (NF.ite (.prim2 .eq (sliceNF (szT - (w0 + 1)) (w0 + 1) dn)
                    (.lit ⟨w0 + 1, BitVec.ofNat (w0 + 1) tagv⟩)) bnf' acc).eval σ
                  = if true then bnf'.eval σ else acc.eval σ from
                  ite_eval_of_cond (by rw [hcond, beq_self_eq_true])]
              rw [if_pos rfl]
              exact hmain
      · -- not selected: the tag test cannot fire
        rw [if_neg hcc] at hsel
        cases macc with
        | none =>
            obtain ⟨hrest0, hne⟩ := hnone rfl
            subst hrest0
            exact absurd hsel (hne bs t')
        | some acc =>
            have hmain := hcont acc rfl bs t' so hsel hrun
            cases tagWv with
            | zero => exact absurd (hw0cc rfl) hcc
            | succ w0 =>
                dsimp only at hgo4
                rw [except_pure_def] at hgo4
                injection hgo4 with hgo4
                subst hgo4
                have htagne : tagv ≠ tag := fun h => hcc (hcniff.2 h)
                obtain ⟨hbt, hbtv⟩ := hbounds w0 rfl
                have hcond : (NF.prim2 .eq (sliceNF (szT - (w0 + 1)) (w0 + 1) dn)
                    (.lit ⟨w0 + 1, BitVec.ofNat (w0 + 1) tag⟩)).eval σ
                    = Rwv.Hyle.Sem.b1 ((BitVec.ofNat (w0 + 1) tagv)
                        == BitVec.ofNat (w0 + 1) tag) := by
                  rw [eq_eval, sliceNF_eval, hslice]
                  show Rwv.Hyle.Sem.b1 ((BitVec.ofNat (w0 + 1) tagv)
                      == (BitVec.ofNat (w0 + 1) tag).setWidth (w0 + 1)) = _
                  rw [BitVec.setWidth_eq]
                rw [show (NF.ite (.prim2 .eq (sliceNF (szT - (w0 + 1)) (w0 + 1) dn)
                      (.lit ⟨w0 + 1, BitVec.ofNat (w0 + 1) tag⟩)) bnf' acc).eval σ
                    = if false then bnf'.eval σ else acc.eval σ from
                    ite_eval_of_cond (by rw [hcond, ofNat_beq_false htagne hbtv hbt])]
                rw [if_neg (by simp)]
                exact hmain

/-- The chain step: fold `palt1_step` along the alternatives. -/
private theorem palts_step {C : Ctx} {plan : Plan} {σ : String → BV} {N : Nat}
    (halts : PAlts C plan σ N) (halt1 : PAlt1 C plan σ N) :
    PAlts C plan σ (N + 1) := by
  intro Γ cells dty szT dn alts macc rec env store sv after ef gf so
    hgo hE hC hvty hrep hsz hafter hnone bs t' hsel hrun
  cases alts with
  | nil =>
      cases macc with
      | none =>
          rw [show goAlts C (N + 1) Γ cells dty szT dn [] none
                = (throw "cstep: empty terminator case" : Except String NF) from rfl] at hgo
          exact error_ne_ok hgo
      | some els =>
          rw [show goAlts C (N + 1) Γ cells dty szT dn [] (some els)
                = (pure els : Except String NF) from rfl, except_pure_def] at hgo
          injection hgo with hgo
          subst hgo
          rw [show selSpec C.Δ ef sv [] after = after from rfl] at hsel
          obtain ⟨hbs0, hdOk⟩ := hafter els rfl bs t' hsel
          subst hbs0
          rw [bindFields_nil] at hrun
          exact hdOk ef gf so hrun
  | cons alt rest' =>
      cases rest' with
      | nil =>
          cases macc with
          | none =>
              rw [show goAlts C (N + 1) Γ cells dty szT dn [alt] none
                    = goAlt1 C N Γ cells dty szT dn alt none from rfl] at hgo
              exact halt1 Γ cells dty szT dn alt [] none rec env store sv after ef gf so
                hgo hE hC hvty hrep hsz (fun _ => ⟨rfl, hnone rfl⟩)
                (fun acc hacc => nomatch hacc) bs t' hsel hrun
          | some els =>
              rw [show goAlts C (N + 1) Γ cells dty szT dn [alt] (some els)
                    = (do
                        let acc ← goAlts C N Γ cells dty szT dn [] (some els)
                        goAlt1 C N Γ cells dty szT dn alt (some acc)) from rfl] at hgo
              obtain ⟨acc, hacc, hgo1⟩ := except_bind_eq_ok hgo
              refine halt1 Γ cells dty szT dn alt [] (some acc) rec env store sv after ef gf so
                hgo1 hE hC hvty hrep hsz (fun hn => nomatch hn) ?_ bs t' hsel hrun
              intro acc' hacc' bs' t'' so' hsel' hrun'
              injection hacc' with hacc'
              subst hacc'
              exact halts Γ cells dty szT dn [] (some els) acc env store sv after ef gf so'
                hacc hE hC hvty hrep hsz hafter (fun hn => nomatch hn) bs' t'' hsel' hrun'
      | cons a2 rest2 =>
          rw [show goAlts C (N + 1) Γ cells dty szT dn (alt :: a2 :: rest2) macc
                = (do
                    let acc ← goAlts C N Γ cells dty szT dn (a2 :: rest2) macc
                    goAlt1 C N Γ cells dty szT dn alt (some acc)) from rfl] at hgo
          obtain ⟨acc, hacc, hgo1⟩ := except_bind_eq_ok hgo
          refine halt1 Γ cells dty szT dn alt (a2 :: rest2) (some acc) rec env store sv after
            ef gf so hgo1 hE hC hvty hrep hsz (fun hn => nomatch hn) ?_ bs t' hsel hrun
          intro acc' hacc' bs' t'' so' hsel' hrun'
          injection hacc' with hacc'
          subst hacc'
          exact halts Γ cells dty szT dn (a2 :: rest2) macc acc env store sv after ef gf so'
            hacc hE hC hvty hrep hsz hafter hnone bs' t'' hsel' hrun'

private theorem goAlts_nil_none_err {C : Ctx} {N : Nat} {Γ : HashMap Int (NF × Ty)}
    {cells : List CellNF} {dty : Ty} {szT : Nat} {dn : NF} {r : NF}
    (h : goAlts C N Γ cells dty szT dn [] none = .ok r) : False := by
  cases N with
  | zero => rw [goAlts] at h; exact error_ne_ok h
  | succ N => rw [goAlts] at h; exact error_ne_ok h

private theorem zip_map_fst {α β γ : Type} (g : α → γ) :
    ∀ (l : List α) (w : List β), l.length = w.length →
      (l.zip w).map (fun p => g p.1) = l.map g := by
  intro l
  induction l with
  | nil =>
      intro w _
      rfl
  | cons a as ih =>
      intro w h
      cases w with
      | nil => simp at h
      | cons b bs =>
          rw [List.zip_cons_cons, List.map_cons, List.map_cons, ih bs (by simpa using h)]

/-- The pause record's field slices, purely at the BV level: the out
field above the resumption load and cells, the resumption-tag field
above the cells, and the per-cell fields at the `offsetsOf`
positions. -/
private theorem pause_slices (lo : Layout) (e3 : BV) (tag : Nat) (argWs : List Nat)
    (reps cbvs : List BV) (cws : List Nat)
    (he3 : e3.width = lo.outW)
    (hreps : reps.map (·.width) = argWs)
    (hcb : cbvs.map (·.width) = cws)
    (hcsum : cws.sum = lo.cellsW)
    (hpay : argWs.sum ≤ lo.rPayW) :
    sliceBV (catAll ([⟨lo.pTagW, 1⟩,
        ⟨lo.recW - lo.pTagW - lo.outW - lo.rW - lo.cellsW, 0⟩,
        e3, ⟨lo.rTagW, BitVec.ofNat _ tag⟩, ⟨lo.rPayW - argWs.sum, 0⟩]
        ++ reps ++ cbvs)) (lo.cellsW + lo.rW) lo.outW = e3 ∧
    sliceBV (catAll ([⟨lo.pTagW, 1⟩,
        ⟨lo.recW - lo.pTagW - lo.outW - lo.rW - lo.cellsW, 0⟩,
        e3, ⟨lo.rTagW, BitVec.ofNat _ tag⟩, ⟨lo.rPayW - argWs.sum, 0⟩]
        ++ reps ++ cbvs)) lo.cellsW lo.rW = encTag lo tag argWs reps ∧
    ∀ i (hi : i < cbvs.length),
      sliceBV (catAll ([⟨lo.pTagW, 1⟩,
          ⟨lo.recW - lo.pTagW - lo.outW - lo.rW - lo.cellsW, 0⟩,
          e3, ⟨lo.rTagW, BitVec.ofNat _ tag⟩, ⟨lo.rPayW - argWs.sum, 0⟩]
          ++ reps ++ cbvs))
        ((offsetsOf cws)[i]'(by rw [offsetsOf_length, ← hcb, List.length_map]; exact hi))
        (cws[i]'(by rw [← hcb, List.length_map]; exact hi))
        = cbvs[i] := by
  have hrsum : (reps.map (·.width)).sum = argWs.sum := by rw [hreps]
  have hbsum : (cbvs.map (·.width)).sum = lo.cellsW := by rw [hcb, hcsum]
  refine ⟨?_, ?_, ?_⟩
  · -- the out field
    have hx := catAll_extract
      [⟨lo.pTagW, 1⟩, ⟨lo.recW - lo.pTagW - lo.outW - lo.rW - lo.cellsW, 0⟩]
      ((⟨lo.rTagW, BitVec.ofNat _ tag⟩ : BV) :: (⟨lo.rPayW - argWs.sum, 0⟩ : BV)
        :: (reps ++ cbvs)) e3
    rw [show (((⟨lo.rTagW, BitVec.ofNat _ tag⟩ : BV) :: (⟨lo.rPayW - argWs.sum, 0⟩ : BV)
          :: (reps ++ cbvs)).map (·.width)).sum
        = lo.rTagW + (lo.rPayW - argWs.sum) + (argWs.sum + lo.cellsW) from by
        simp only [List.map_cons, List.sum_cons, List.map_append, List.sum_append]
        rw [hrsum, hbsum]
        omega] at hx
    rw [show lo.rTagW + (lo.rPayW - argWs.sum) + (argWs.sum + lo.cellsW)
          = lo.cellsW + lo.rW from by rw [Layout.rW]; omega] at hx
    rw [he3] at hx
    exact hx
  · -- the resumption-tag field
    have hx := catAll_extract_seg
      [⟨lo.pTagW, 1⟩, ⟨lo.recW - lo.pTagW - lo.outW - lo.rW - lo.cellsW, 0⟩, e3]
      ((⟨lo.rTagW, BitVec.ofNat _ tag⟩ : BV) :: (⟨lo.rPayW - argWs.sum, 0⟩ : BV) :: reps)
      cbvs
    rw [show (((⟨lo.rTagW, BitVec.ofNat _ tag⟩ : BV) :: (⟨lo.rPayW - argWs.sum, 0⟩ : BV)
          :: reps).map (·.width)).sum = lo.rW from by
        simp only [List.map_cons, List.sum_cons]
        rw [hrsum, Layout.rW]
        omega] at hx
    rw [hbsum] at hx
    rw [show catAll ((⟨lo.rTagW, BitVec.ofNat _ tag⟩ : BV)
          :: (⟨lo.rPayW - argWs.sum, 0⟩ : BV) :: reps)
        = encTag lo tag argWs reps from by
        rw [catAll_cons, catAll_cons, encTag_split, ← bvCat_assoc]] at hx
    exact hx
  · -- the cell fields
    intro i hi
    have hlow : sliceBV (catAll (([⟨lo.pTagW, 1⟩,
          ⟨lo.recW - lo.pTagW - lo.outW - lo.rW - lo.cellsW, 0⟩,
          e3, ⟨lo.rTagW, BitVec.ofNat _ tag⟩, ⟨lo.rPayW - argWs.sum, 0⟩] ++ reps) ++ cbvs))
        ((offsetsOf cws)[i]'(by rw [offsetsOf_length, ← hcb, List.length_map]; exact hi))
        (cws[i]'(by rw [← hcb, List.length_map]; exact hi))
        = sliceBV (catAll cbvs)
            ((offsetsOf cws)[i]'(by rw [offsetsOf_length, ← hcb, List.length_map]; exact hi))
            (cws[i]'(by rw [← hcb, List.length_map]; exact hi)) := by
      rw [catAll_append]
      refine sliceBV_cat_low ?_
      rw [catAll_width, hbsum, ← hcsum,
          offsetsOf_getElem _ i (by rw [← hcb, List.length_map]; exact hi)]
      exact drop_sum_le (by rw [← hcb, List.length_map]; exact hi)
    rw [show ([⟨lo.pTagW, 1⟩, ⟨lo.recW - lo.pTagW - lo.outW - lo.rW - lo.cellsW, 0⟩,
          e3, ⟨lo.rTagW, BitVec.ofNat _ tag⟩, ⟨lo.rPayW - argWs.sum, 0⟩]
          ++ reps ++ cbvs)
        = (([⟨lo.pTagW, 1⟩, ⟨lo.recW - lo.pTagW - lo.outW - lo.rW - lo.cellsW, 0⟩,
          e3, ⟨lo.rTagW, BitVec.ofNat _ tag⟩, ⟨lo.rPayW - argWs.sum, 0⟩] ++ reps) ++ cbvs)
        from rfl, hlow]
    have hidx := catAll_extract_idx cbvs i hi
    rw [offsetsOf_getElem _ i (by rw [List.length_map]; exact hi)] at hidx
    rw [offsetsOf_getElem _ i (by rw [← hcb, List.length_map]; exact hi)]
    have hgw : cws[i]'(by rw [← hcb, List.length_map]; exact hi)
        = (cbvs[i]'hi).width := by
      have h1 : cws[i]? = (cbvs.map (·.width))[i]? := by rw [hcb]
      rw [List.getElem?_eq_getElem (by rw [← hcb, List.length_map]; exact hi),
          List.getElem?_eq_getElem (by rw [List.length_map]; exact hi),
          List.getElem_map] at h1
      exact Option.some.inj h1
    have hds : ((cbvs.map (fun x => x.width)).drop (i + 1)).sum
        = (cws.drop (i + 1)).sum := by rw [hcb]
    rw [hds] at hidx
    rw [hgw]
    exact hidx

/-- The terminator step: pause assembles the record, goto recurses
through the block, halt is vacuous, cases dispatches through the
chain. -/
private theorem pterm_step {C : Ctx} {plan : Plan} {σ : String → BV} {N : Nat}
    (hS : SInv C plan) (hcmds : PCmds C plan σ N) (hterm : PTerm C plan σ N)
    (halts : PAlts C plan σ N) : PTerm C plan σ (N + 1) := by
  intro Γ cells term rec env store ef gf so hgo hE hC hconc
  cases term with
  | halt e =>
      rw [Machine.execBlock.runTerm] at hconc
      obtain ⟨a, _ha, hconc⟩ := except_bind_eq_ok hconc
      rw [except_pure_def] at hconc
      injection hconc with hconc
      subst hconc
      exact trivial
  | goto l args =>
      rw [goTerm] at hgo
      rw [Machine.execBlock.runTerm] at hconc
      cases hblk : C.blocks.get? l.uniq with
      | none => rw [hblk] at hgo; exact error_ne_ok hgo
      | some blk =>
          rw [hblk] at hgo hconc
          dsimp only at hgo hconc
          obtain ⟨pas, hpas, hgoA⟩ := except_bind_eq_ok hgo
          clear hgo
          split at hgoA
          rotate_left
          · exact error_ne_ok hgoA
          rename_i hteqa
          obtain ⟨vs, hvs, hconcA⟩ := except_bind_eq_ok hconc
          clear hconc
          split at hconcA
          · exact error_ne_ok hconcA
          rename_i hvlen'
          cases gf with
          | zero => exact error_ne_ok hconcA
          | succ gf' =>
              dsimp only at hconcA
              obtain ⟨st, hst, hconcB⟩ := except_bind_eq_ok hconcA
              clear hconcA
              obtain ⟨env₂, store₂⟩ := st
              -- the two parameter bindings correspond
              obtain ⟨hplen, hppt⟩ := mapM_ok_idx hpas
              obtain ⟨hvlen, hvpt⟩ := mapM_ok_idx hvs
              have hplen' : pas.length = blk.params.length := by
                have := Rwv.Eidos.Cexp.teqAll_length hteqa
                simpa using this
              have htys := Rwv.Eidos.Cexp.teqAll_types hteqa
              have hE' := envC_foldl_zip blk.params pas vs envC_empty
                (by omega) (by omega)
                (fun i h1 h2 h3 => by
                  obtain ⟨_, hpi⟩ := hppt i (by omega)
                  obtain ⟨_, hvi⟩ := hvpt i (by omega)
                  exact Rwv.Eidos.Cexp.cexpFull_sound hS.hden C.cexpFuel Γ (args[i]'(by omega))
                    (pas[i]'h2).1 (pas[i]'h2).2 ef env [] (vs[i]'h3) (by
                      rw [show ((pas[i]'h2).1, (pas[i]'h2).2) = pas[i]'h2 from rfl]
                      exact hpi) hvi hE)
              have hst' : Machine.runCmds C.Δ C.edm ef
                  ((blk.params.zip vs).foldl (fun e (p, v) => (p.uniq, v) :: e) [])
                  store blk.cmds = .ok (env₂, store₂) := hst
              refine hcmds _ cells blk.cmds blk.term rec _ store ef gf' so hgoA hE' hC ?_
              rw [hst', except_bind_ok]
              exact hconcB
  | pause out l args =>
      rw [goTerm] at hgo
      obtain ⟨ot, hot, hgoA⟩ := except_bind_eq_ok hgo
      clear hgo
      obtain ⟨onf, oty⟩ := ot
      dsimp only at hgoA
      split at hgoA
      rotate_left
      · exact error_ne_ok hgoA
      rename_i hteqo
      cases hfind : C.lo.targets.find? (fun t => t.uniq == l.uniq) with
      | none => rw [hfind] at hgoA; exact error_ne_ok hgoA
      | some tgt =>
      rw [hfind] at hgoA
      dsimp only at hgoA
      obtain ⟨pas, hpas, hgoB⟩ := except_bind_eq_ok hgoA
      clear hgoA
      split at hgoB
      rotate_left
      · exact error_ne_ok hgoB
      rename_i hteqa
      rw [except_pure_def] at hgoB
      injection hgoB with hgoB
      -- the concrete pause
      rw [Machine.execBlock.runTerm] at hconc
      obtain ⟨o, ho, hconcA⟩ := except_bind_eq_ok hconc
      clear hconc
      obtain ⟨vs, hvs, hconcB⟩ := except_bind_eq_ok hconcA
      clear hconcA
      rw [except_pure_def] at hconcB
      injection hconcB with hconcB
      subst hconcB
      subst hgoB
      -- output facts
      obtain ⟨hovty, ko, horep⟩ := Rwv.Eidos.Cexp.cexpFull_sound hS.hden C.cexpFuel Γ out
        onf oty ef env [] o hot ho hE
      have hotyE : oty = C.outTy := Rwv.Eidos.Cexp.teq_eq hteqo
      subst hotyE
      obtain ⟨kout, hkout⟩ := hS.outsz
      have hbw : (onf.eval σ).width = C.lo.outW := vty_rep_width hovty horep hkout
      -- target facts
      have htgtmem : tgt ∈ C.lo.targets := List.mem_of_find?_eq_some hfind
      have htgtuq : tgt.uniq = l.uniq := by
        have := List.find?_some hfind
        simpa using this
      obtain ⟨blk0, hblk0, hty0, kA, hAms⟩ := hS.tgts tgt htgtmem
      obtain ⟨hAlen, hApt⟩ := mapM_ok_idx hAms
      have hpay := hS.paybound tgt htgtmem
      -- argument facts
      obtain ⟨hplen, hppt⟩ := mapM_ok_idx hpas
      obtain ⟨hvlen, hvpt⟩ := mapM_ok_idx hvs
      have hptys := Rwv.Eidos.Cexp.teqAll_types hteqa
      have hplen' : pas.length = tgt.argTys.length := by
        have := Rwv.Eidos.Cexp.teqAll_length hteqa
        simpa using this
      have hargpt : ∀ i (h3 : i < vs.length),
          VTy C.Δ (vs[i]'h3) ((pas[i]'(by omega)).2) ∧
          ∃ k, Val.rep C.Δ k (vs[i]'h3) = .ok ((pas[i]'(by omega)).1.eval σ) := by
        intro i h3
        obtain ⟨_, hpi⟩ := hppt i (by omega)
        obtain ⟨_, hvi⟩ := hvpt i (by omega)
        exact Rwv.Eidos.Cexp.cexpFull_sound hS.hden C.cexpFuel Γ (args[i]'(by omega))
          (pas[i]'(by omega)).1 (pas[i]'(by omega)).2 ef env [] (vs[i]'h3) (by
            rw [show ((pas[i]'(by omega)).1, (pas[i]'(by omega)).2)
                  = pas[i]'(by omega) from rfl]
            exact hpi) hvi hE
      have hatys : ∀ i (hi : i < pas.length), (pas[i]'hi).2 = tgt.argTys[i]'(by omega) := by
        intro i hi
        have h1 : (pas.map (·.2))[i]? = tgt.argTys[i]? := by rw [hptys]
        rw [List.getElem?_eq_getElem (by rw [List.length_map]; omega),
            List.getElem?_eq_getElem (by omega), List.getElem_map] at h1
        exact Option.some.inj h1
      -- per-argument sizes and rep widths
      have hrepw : ∀ i (hi : i < pas.length),
          ((pas[i]'hi).1.eval σ).width = tgt.argWs[i]'(by omega) := by
        intro i hi
        obtain ⟨_, hszi⟩ := hApt i (by omega)
        obtain ⟨hvt, k, hrp⟩ := hargpt i (by omega)
        rw [hatys i hi] at hvt
        exact vty_rep_width hvt hrp hszi
      -- the record's piece widths
      have hcw : ∀ d ∈ cells, (d.nf.eval σ).width = d.width := by
        intro d hd
        obtain ⟨_, _, _, _, _, hwd⟩ := hC.2.2 d hd
        exact hwd
      have hw : ∀ p ∈ ([((NF.lit ⟨C.lo.pTagW, 1⟩ : NF), C.lo.pTagW),
              ((NF.lit ⟨C.lo.recW - C.lo.pTagW - C.lo.outW - C.lo.rW - C.lo.cellsW, 0⟩ : NF),
                C.lo.recW - C.lo.pTagW - C.lo.outW - C.lo.rW - C.lo.cellsW),
              (onf, C.lo.outW),
              ((NF.lit ⟨C.lo.rTagW, BitVec.ofNat _ tgt.tag⟩ : NF), C.lo.rTagW),
              ((NF.lit ⟨C.lo.rPayW - tgt.argWs.sum, 0⟩ : NF), C.lo.rPayW - tgt.argWs.sum)]
            ++ (pas.map (·.1)).zip tgt.argWs
            ++ cells.map fun c => (c.nf, c.width)),
          (p.1.eval σ).width = p.2 := by
        intro p hp
        rcases List.mem_append.mp hp with hp | hp
        · rcases List.mem_append.mp hp with hp | hp
          · simp only [List.mem_cons] at hp
            rcases hp with rfl | rfl | rfl | rfl | rfl | hp
            · rfl
            · rfl
            · exact hbw
            · rfl
            · rfl
            · exact absurd hp (by simp)
          · obtain ⟨i, hi, hpi⟩ := List.getElem_of_mem hp
            rw [List.length_zip, List.length_map] at hi
            rw [List.getElem_zip, List.getElem_map] at hpi
            subst hpi
            exact hrepw i (by omega)
        · obtain ⟨c, hcm, hcp⟩ := List.mem_map.mp hp
          subst hcp
          exact hcw c hcm
      -- widths of the compiled argument representations
      have hrw : (pas.map (fun p => p.1.eval σ)).map (·.width) = tgt.argWs := by
        refine List.ext_getElem (by rw [List.length_map, List.length_map]; omega) ?_
        intro i h1 h2
        rw [List.getElem_map, List.getElem_map]
        exact hrepw i (by rw [List.length_map, List.length_map] at h1; omega)
      -- the record's value as a concatenation of the piece values
      have hmapev : (([((NF.lit ⟨C.lo.pTagW, 1⟩ : NF), C.lo.pTagW),
              ((NF.lit ⟨C.lo.recW - C.lo.pTagW - C.lo.outW - C.lo.rW - C.lo.cellsW, 0⟩ : NF),
                C.lo.recW - C.lo.pTagW - C.lo.outW - C.lo.rW - C.lo.cellsW),
              (onf, C.lo.outW),
              ((NF.lit ⟨C.lo.rTagW, BitVec.ofNat _ tgt.tag⟩ : NF), C.lo.rTagW),
              ((NF.lit ⟨C.lo.rPayW - tgt.argWs.sum, 0⟩ : NF), C.lo.rPayW - tgt.argWs.sum)]
            ++ (pas.map (·.1)).zip tgt.argWs
            ++ cells.map fun c => (c.nf, c.width)).map (fun p => p.1.eval σ))
          = ([(⟨C.lo.pTagW, 1⟩ : BV),
              (⟨C.lo.recW - C.lo.pTagW - C.lo.outW - C.lo.rW - C.lo.cellsW, 0⟩ : BV),
              onf.eval σ, (⟨C.lo.rTagW, BitVec.ofNat _ tgt.tag⟩ : BV),
              (⟨C.lo.rPayW - tgt.argWs.sum, 0⟩ : BV)]
             ++ pas.map (fun p => p.1.eval σ) ++ cells.map (fun c => c.nf.eval σ)) := by
        rw [List.map_append, List.map_append,
            zip_map_fst (NF.eval σ) (pas.map (·.1)) tgt.argWs (by
              rw [List.length_map]
              omega),
            List.map_map, List.map_map]
        rfl
      have hcwlist : cells.map (·.width) = plan.cells.map (·.width) := by
        have h := congrArg (List.map (fun t : String × Ty × Nat => t.2.2)) hC.1
        rw [List.map_map, List.map_map] at h
        exact h
      obtain ⟨hsl1, hsl2, hsl3⟩ := pause_slices C.lo (onf.eval σ) tgt.tag tgt.argWs
        (pas.map (fun p => p.1.eval σ)) (cells.map (fun c => c.nf.eval σ))
        (plan.cells.map (·.width)) hbw hrw
        (by
          rw [List.map_map]
          refine Eq.trans (List.map_congr_left ?_) hcwlist
          intro d hd
          exact hcw d hd)
        hS.cellsw hpay
      rw [pauseRec, catNF_eval σ _ hw, hmapev]
      refine ⟨hovty, ⟨onf.eval σ, ko, horep, hbw, hsl1⟩, tgt, htgtmem, htgtuq, ?_, ?_, ?_, ?_⟩
      · show vs.length = tgt.argTys.length
        omega
      · intro pr hpr
        obtain ⟨i, hi, hpri⟩ := List.getElem_of_mem hpr
        rw [List.length_zip] at hi
        rw [List.getElem_zip] at hpri
        rw [← hpri]
        show VTy C.Δ (vs[i]'(by omega)) (tgt.argTys[i]'(by omega))
        obtain ⟨hvt, _⟩ := hargpt i (by omega)
        rw [hatys i (by omega)] at hvt
        exact hvt
      · obtain ⟨K, hK⟩ := mapM_rep_exists (Δ := C.Δ) (vs := vs)
          (bs := pas.map (fun p => p.1.eval σ)) (by rw [List.length_map]; omega)
          (fun i h1 h2 => by
            obtain ⟨_, k, hrp⟩ := hargpt i h1
            refine ⟨k, ?_⟩
            rw [List.getElem_map]
            exact hrp)
        exact ⟨pas.map (fun p => p.1.eval σ), K, hK, hrw, hsl2⟩
      · intro pr hpr
        obtain ⟨i, hi, hpri⟩ := List.getElem_of_mem hpr
        rw [List.length_zip, offsetsOf_length, List.length_map, Nat.min_self] at hi
        rw [List.getElem_zip] at hpri
        have hclen : cells.length = plan.cells.length := by
          have h := congrArg List.length hC.1
          simpa using h
        have htri : (cells[i]'(by omega)).name = (plan.cells[i]'hi).name ∧
            (cells[i]'(by omega)).ty = (plan.cells[i]'hi).ty ∧
            (cells[i]'(by omega)).width = (plan.cells[i]'hi).width := by
          have h1 : (cells.map (fun d => (d.name, d.ty, d.width)))[i]?
              = (plan.cells.map (fun c => (c.name, c.ty, c.width)))[i]? := by rw [hC.1]
          rw [List.getElem?_eq_getElem (by rw [List.length_map]; omega),
              List.getElem?_eq_getElem (by rw [List.length_map]; exact hi),
              List.getElem_map, List.getElem_map] at h1
          have h2 := Option.some.inj h1
          injection h2 with e1 e23
          injection e23 with e2 e3
          exact ⟨e1, e2, e3⟩
        obtain ⟨v, k, hget, hvty, hrep, hwid⟩ :=
          hC.2.2 (cells[i]'(by omega)) (List.getElem_mem _)
        refine ⟨v, (cells[i]'(by omega)).nf.eval σ, k, ?_, ?_, hrep, ?_, ?_⟩
        · rw [← hpri]
          show store.get? (plan.cells[i]'hi).name = some v
          rw [← htri.1]
          exact hget
        · rw [← hpri]
          show VTy C.Δ v (plan.cells[i]'hi).ty
          rw [← htri.2.1]
          exact hvty
        · rw [← hpri]
          show ((cells[i]'(by omega)).nf.eval σ).width = (plan.cells[i]'hi).width
          rw [← htri.2.2]
          exact hwid
        · rw [← hpri]
          have h3 := hsl3 i (by rw [List.length_map]; omega)
          rw [show (cells.map (fun c => c.nf.eval σ))[i]'(by rw [List.length_map]; omega)
                = (cells[i]'(by omega)).nf.eval σ from List.getElem_map _] at h3
          rw [show (plan.cells.map (·.width))[i]'(by rw [List.length_map]; exact hi)
                = (plan.cells[i]'hi).width from List.getElem_map _] at h3
          exact h3
  | cases scrutE alts =>
      rw [goTerm] at hgo
      obtain ⟨dt0, hdn, hgoA⟩ := except_bind_eq_ok hgo
      clear hgo
      obtain ⟨dn, dty⟩ := dt0
      dsimp only at hgoA
      obtain ⟨szT, hszT, hgoB⟩ := except_bind_eq_ok hgoA
      clear hgoA
      rw [Machine.execBlock.runTerm] at hconc
      obtain ⟨sv, hsv, hconcA⟩ := except_bind_eq_ok hconc
      clear hconc
      obtain ⟨selr, hselr, hconcB⟩ := except_bind_eq_ok hconcA
      clear hconcA
      obtain ⟨bs, t'⟩ := selr
      dsimp only at hconcB
      cases gf with
      | zero => exact error_ne_ok hconcB
      | succ gf' =>
      obtain ⟨hsvty, ksv, hsvrep⟩ := Rwv.Eidos.Cexp.cexpFull_sound hS.hden C.cexpFuel Γ
        scrutE dn dty ef env [] sv hdn hsv hE
      rw [selectTAlt_char] at hselr
      cases alts with
      | nil => exact (goAlts_nil_none_err hgoB).elim
      | cons a rest2 =>
          obtain ⟨acon, cbs, ct⟩ := a
          cases acon with
          | default =>
              rw [show (match (TAlt.mk .default cbs ct :: rest2 : List TAlt) with
                    | .mk .default bs dt :: rest =>
                        if bs.isEmpty then do
                          let els ← goTerm C N Γ cells dt
                          goAlts C N Γ cells dty szT dn rest (some els)
                        else throw "cstep: default alternative with binders"
                    | rest => goAlts C N Γ cells dty szT dn rest none)
                  = (if cbs.isEmpty then do
                      let els ← goTerm C N Γ cells ct
                      goAlts C N Γ cells dty szT dn rest2 (some els)
                    else (throw "cstep: default alternative with binders"
                        : Except String NF)) from rfl] at hgoB
              split at hgoB
              rotate_left
              · exact error_ne_ok hgoB
              rename_i hcbs
              have hcbs' : cbs = [] := List.isEmpty_iff.mp hcbs
              subst hcbs'
              obtain ⟨els, hels, hgoC⟩ := except_bind_eq_ok hgoB
              rw [List.find?_cons_of_pos (by rfl)] at hselr
              rw [show selSpec C.Δ ef sv (TAlt.mk .default [] ct :: rest2)
                    (match (some (TAlt.mk .default [] ct) : Option TAlt) with
                     | some (.mk _ bs t) => pure (bs, t)
                     | none => throw "terminator case: no matching alternative and no default")
                  = selSpec C.Δ ef sv rest2 (pure ([], ct)) from rfl] at hselr
              refine halts Γ cells dty szT dn rest2 (some els) rec env store sv
                (pure ([], ct)) ef gf' so hgoC hE hC hsvty ⟨ksv, hsvrep⟩
                ⟨C.cexpFuel + 1, hszT⟩ ?_ (fun hn => nomatch hn) bs t' hselr hconcB
              intro els' hels' bs' t'' hafter'
              injection hels' with hels'
              subst hels'
              rw [except_pure_def] at hafter'
              injection hafter' with hafter'
              injection hafter' with ha1 ha2
              subst ha1; subst ha2
              refine ⟨rfl, ?_⟩
              intro ef2 gf2 so2 hrun2
              exact hterm Γ cells ct els env store ef2 gf2 so2 hels hE hC hrun2
          | dataAlt cn =>
              rw [show (match (TAlt.mk (.dataAlt cn) cbs ct :: rest2 : List TAlt) with
                    | .mk .default bs dt :: rest =>
                        if bs.isEmpty then do
                          let els ← goTerm C N Γ cells dt
                          goAlts C N Γ cells dty szT dn rest (some els)
                        else throw "cstep: default alternative with binders"
                    | rest => goAlts C N Γ cells dty szT dn rest none)
                  = goAlts C N Γ cells dty szT dn (TAlt.mk (.dataAlt cn) cbs ct :: rest2) none
                  from rfl] at hgoB
              cases hfind : (TAlt.mk (.dataAlt cn) cbs ct :: rest2).find?
                  (fun | .mk .default _ _ => true | _ => false) with
              | none =>
                  rw [hfind] at hselr
                  dsimp only at hselr
                  refine halts Γ cells dty szT dn _ none rec env store sv _ ef gf' so
                    hgoB hE hC hsvty ⟨ksv, hsvrep⟩ ⟨C.cexpFuel + 1, hszT⟩
                    (fun els' hels' => nomatch hels') (fun _ bs' t'' hc => error_ne_ok hc)
                    bs t' hselr hconcB
              | some da =>
                  exfalso
                  have hpred := List.find?_some hfind
                  obtain ⟨dcon, dbs2, dt2⟩ := da
                  cases dcon with
                  | default =>
                      exact goAlts_no_default hgoB dbs2 dt2 (List.mem_of_find?_eq_some hfind)
                  | dataAlt c2 => simp at hpred
                  | litAlt n2 => simp at hpred
          | litAlt n =>
              rw [show (match (TAlt.mk (.litAlt n) cbs ct :: rest2 : List TAlt) with
                    | .mk .default bs dt :: rest =>
                        if bs.isEmpty then do
                          let els ← goTerm C N Γ cells dt
                          goAlts C N Γ cells dty szT dn rest (some els)
                        else throw "cstep: default alternative with binders"
                    | rest => goAlts C N Γ cells dty szT dn rest none)
                  = goAlts C N Γ cells dty szT dn (TAlt.mk (.litAlt n) cbs ct :: rest2) none
                  from rfl] at hgoB
              cases hfind : (TAlt.mk (.litAlt n) cbs ct :: rest2).find?
                  (fun | .mk .default _ _ => true | _ => false) with
              | none =>
                  rw [hfind] at hselr
                  dsimp only at hselr
                  refine halts Γ cells dty szT dn _ none rec env store sv _ ef gf' so
                    hgoB hE hC hsvty ⟨ksv, hsvrep⟩ ⟨C.cexpFuel + 1, hszT⟩
                    (fun els' hels' => nomatch hels') (fun _ bs' t'' hc => error_ne_ok hc)
                    bs t' hselr hconcB
              | some da =>
                  exfalso
                  have hpred := List.find?_some hfind
                  obtain ⟨dcon, dbs2, dt2⟩ := da
                  cases dcon with
                  | default =>
                      exact goAlts_no_default hgoB dbs2 dt2 (List.mem_of_find?_eq_some hfind)
                  | dataAlt c2 => simp at hpred
                  | litAlt n2 => simp at hpred

/-- THE machine-step soundness theorem (the goto-fuel induction over
the four compiler levels): at every fuel, the symbolic machine-step
compiler is representation-correspondent to the committed machine
semantics — a compiled command sequence (`PCmds`), terminator
(`PTerm`), terminator chain (`PAlts`), or chain link (`PAlt1`) whose
concrete execution succeeds takes related environments and cell
stores to a record value related to the step outcome (`StepValC`):
the out field holds the emitted output's representation, the
resumption-tag field the next state's `encTag`, and the cell fields
the next cells' representations — for EVERY concrete evaluation and
goto fuel. -/
theorem cstep_sound {C : Ctx} {plan : Plan} {σ : String → BV} (hS : SInv C plan) :
    ∀ N, PCmds C plan σ N ∧ PTerm C plan σ N ∧ PAlts C plan σ N ∧ PAlt1 C plan σ N := by
  intro N
  induction N with
  | zero => exact ⟨pcmds_zero, pterm_zero, palts_zero, palt1_zero⟩
  | succ N ih =>
      obtain ⟨hc, ht, has, ha1⟩ := ih
      exact ⟨pcmds_step hS ht hc, pterm_step hS hc ht has,
             palts_step has ha1, palt1_step hS ht⟩

/-! ## The width discipline of compiled step records

The step compiler draws its variables from Γ, the cell store, and the
compiled pure expressions (`cexpFull_varsWF`), so any width predicate
holding there holds of the record — the `VarsWF` invariant the
width-aware comparison leg (`cfoldW3`) needs. -/

open Rwv.Eidos.Cexp (GammaWF) in
private theorem gammaWF_insert' {P : String → Nat → Prop} {Γ : HashMap Int (NF × Ty)}
    (h : GammaWF P Γ) {u : Int} {nt : NF × Ty} (hnt : NF.VarsWF P nt.1) :
    GammaWF P (Γ.insert u nt) := by
  intro u' nt' h'
  rw [get?_insert] at h'
  by_cases he : u' = u
  · rw [if_pos he] at h'
    injection h' with h'
    subst h'
    exact hnt
  · rw [if_neg he] at h'
    exact h u' nt' h'

open Rwv.Eidos.Cexp (GammaWF) in
private theorem gammaWF_foldl_zip {P : String → Nat → Prop} :
    ∀ (params : List Id) (pas : List (NF × Ty)) {Γ₀ : HashMap Int (NF × Ty)},
      GammaWF P Γ₀ → (∀ nt ∈ pas, NF.VarsWF P nt.1) →
      GammaWF P ((params.zip pas).foldl (fun m (x, nt) => m.insert x.uniq nt) Γ₀) := by
  intro params
  induction params with
  | nil => intro pas Γ₀ h0 _; simpa using h0
  | cons p ps ih =>
      intro pas Γ₀ h0 hpt
      cases pas with
      | nil => simpa using h0
      | cons nt nts =>
          rw [List.zip_cons_cons, List.foldl_cons]
          exact ih nts (gammaWF_insert' h0 (hpt nt List.mem_cons_self))
            (fun q hq => hpt q (List.mem_cons_of_mem _ hq))

private theorem catList_varsWF {P : String → Nat → Prop} :
    ∀ {xs : List NF}, (∀ x ∈ xs, NF.VarsWF P x) →
      NF.VarsWF P (Rwv.Eidos.Cexp.catList xs) := by
  intro xs
  match xs with
  | [] => intro _; trivial
  | [x] => intro h; exact h x List.mem_cons_self
  | x :: y :: rest =>
      intro h
      exact ⟨h x List.mem_cons_self,
        catList_varsWF (fun z hz => h z (List.mem_cons_of_mem _ hz))⟩

private theorem catNF_varsWF {P : String → Nat → Prop} {xs : List (NF × Nat)}
    (h : ∀ p ∈ xs, NF.VarsWF P p.1) : NF.VarsWF P (catNF xs) := by
  rw [catNF]
  refine catList_varsWF ?_
  intro x hx
  obtain ⟨p, hp, hpx⟩ := List.mem_map.mp hx
  subst hpx
  exact h p (List.mem_filter.mp hp).1

private theorem sliceNF_varsWF {P : String → Nat → Prop} {off w : Nat} {e : NF}
    (h : NF.VarsWF P e) : NF.VarsWF P (sliceNF off w e) := by
  rw [sliceNF]
  by_cases hw : w = 0
  · rw [if_pos hw]; trivial
  · rw [if_neg hw]; exact h

/-- Width-discipline soundness of the step compiler, at every fuel:
the compiled record's variables satisfy any predicate holding on Γ
and the cell store. -/
theorem goCmds_varsWF {C : Ctx} {P : String → Nat → Prop} :
    ∀ N,
      (∀ Γ cells cmds term rec, goCmds C N Γ cells cmds term = .ok rec →
        Rwv.Eidos.Cexp.GammaWF P Γ → (∀ d ∈ cells, NF.VarsWF P d.nf) →
        NF.VarsWF P rec) ∧
      (∀ Γ cells term rec, goTerm C N Γ cells term = .ok rec →
        Rwv.Eidos.Cexp.GammaWF P Γ → (∀ d ∈ cells, NF.VarsWF P d.nf) →
        NF.VarsWF P rec) ∧
      (∀ Γ cells dty szT dn alts macc rec,
        goAlts C N Γ cells dty szT dn alts macc = .ok rec →
        Rwv.Eidos.Cexp.GammaWF P Γ → (∀ d ∈ cells, NF.VarsWF P d.nf) →
        NF.VarsWF P dn → (∀ acc, macc = some acc → NF.VarsWF P acc) →
        NF.VarsWF P rec) ∧
      (∀ Γ cells dty szT dn alt macc rec,
        goAlt1 C N Γ cells dty szT dn alt macc = .ok rec →
        Rwv.Eidos.Cexp.GammaWF P Γ → (∀ d ∈ cells, NF.VarsWF P d.nf) →
        NF.VarsWF P dn → (∀ acc, macc = some acc → NF.VarsWF P acc) →
        NF.VarsWF P rec) := by
  intro N
  induction N with
  | zero =>
      refine ⟨?_, ?_, ?_, ?_⟩
      · intro Γ cells cmds term rec h; rw [goCmds] at h; exact error_ne_ok h
      · intro Γ cells term rec h; rw [goTerm] at h; exact error_ne_ok h
      · intro Γ cells dty szT dn alts macc rec h; rw [goAlts] at h; exact error_ne_ok h
      · intro Γ cells dty szT dn alt macc rec h; rw [goAlt1] at h; exact error_ne_ok h
  | succ N ih =>
      obtain ⟨ihc, iht, ihas, iha1⟩ := ih
      refine ⟨?_, ?_, ?_, ?_⟩
      · -- goCmds
        intro Γ cells cmds term rec h hΓ hcw
        cases cmds with
        | nil =>
            rw [show goCmds C (N + 1) Γ cells [] term = goTerm C N Γ cells term from rfl] at h
            exact iht Γ cells term rec h hΓ hcw
        | cons cmd rest =>
            cases cmd with
            | bind x e =>
                rw [show goCmds C (N + 1) Γ cells (.bind x e :: rest) term = (do
                    let nt ← Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ e
                    goCmds C N (Γ.insert x.uniq nt) cells rest term) from rfl] at h
                obtain ⟨nt, hnt, h⟩ := except_bind_eq_ok h
                exact ihc _ cells rest term rec h
                  (gammaWF_insert' hΓ (Rwv.Eidos.Cexp.cexpFull_varsWF hΓ (by
                    rw [show (nt.1, nt.2) = nt from rfl]
                    exact hnt))) hcw
            | get x c =>
                rw [show goCmds C (N + 1) Γ cells (.get x c :: rest) term
                      = (match cells.find? (fun d => d.name == c) with
                         | some d => goCmds C N (Γ.insert x.uniq (d.nf, d.ty)) cells rest term
                         | none => throw s!"cstep: get from unknown cell {c}") from rfl] at h
                cases hfd : cells.find? (fun d => d.name == c) with
                | none => rw [hfd] at h; exact error_ne_ok h
                | some d =>
                    rw [hfd] at h
                    exact ihc _ cells rest term rec h
                      (gammaWF_insert' hΓ (hcw d (List.mem_of_find?_eq_some hfd))) hcw
            | put c e =>
                rw [show goCmds C (N + 1) Γ cells (.put c e :: rest) term = (do
                    let (nf, ty) ← Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ e
                    match cells.find? (fun d => d.name == c) with
                    | some d =>
                        if teq ty d.ty then
                          goCmds C N Γ
                            (cells.map fun d' =>
                              if d'.name == c then { d' with nf } else d')
                            rest term
                        else throw s!"cstep: put to cell {c} at the wrong type"
                    | none => throw s!"cstep: put to unknown cell {c}") from rfl] at h
                obtain ⟨nt, hnt, h⟩ := except_bind_eq_ok h
                obtain ⟨nf₁, ty₁⟩ := nt
                cases hfd : cells.find? (fun d => d.name == c) with
                | none => rw [hfd] at h; exact error_ne_ok h
                | some d =>
                    rw [hfd] at h
                    dsimp only at h
                    split at h
                    rotate_left
                    · exact error_ne_ok h
                    refine ihc Γ _ rest term rec h hΓ ?_
                    intro d' hd'
                    obtain ⟨d₀, hd₀, hupd⟩ := List.mem_map.mp hd'
                    by_cases hn0 : (d₀.name == c) = true
                    · rw [if_pos hn0] at hupd
                      subst hupd
                      exact Rwv.Eidos.Cexp.cexpFull_varsWF hΓ hnt
                    · rw [if_neg (by simp only [Bool.not_eq_true] at hn0; simp [hn0])] at hupd
                      subst hupd
                      exact hcw d₀ hd₀
      · -- goTerm
        intro Γ cells term rec h hΓ hcw
        cases term with
        | pause out l args =>
            rw [goTerm] at h
            obtain ⟨ot, hot, h⟩ := except_bind_eq_ok h
            obtain ⟨onf, oty⟩ := ot
            dsimp only at h
            split at h
            rotate_left
            · exact error_ne_ok h
            cases hfind : C.lo.targets.find? (fun t => t.uniq == l.uniq) with
            | none => rw [hfind] at h; exact error_ne_ok h
            | some tgt =>
            rw [hfind] at h
            dsimp only at h
            obtain ⟨pas, hpas, h⟩ := except_bind_eq_ok h
            split at h
            rotate_left
            · exact error_ne_ok h
            rw [except_pure_def] at h
            injection h with h
            subst h
            rw [pauseRec]
            refine catNF_varsWF ?_
            intro p hp
            rcases List.mem_append.mp hp with hp | hp
            · rcases List.mem_append.mp hp with hp | hp
              · simp only [List.mem_cons] at hp
                rcases hp with rfl | rfl | rfl | rfl | rfl | hp
                · trivial
                · trivial
                · exact Rwv.Eidos.Cexp.cexpFull_varsWF hΓ hot
                · trivial
                · trivial
                · exact absurd hp (by simp)
              · obtain ⟨i, hi, hpi⟩ := List.getElem_of_mem hp
                rw [List.getElem_zip] at hpi
                subst hpi
                obtain ⟨hplen, hppt⟩ := mapM_ok_idx hpas
                rw [List.length_zip, List.length_map] at hi
                obtain ⟨_, hpe⟩ := hppt i (by omega)
                rw [List.getElem_map]
                refine Rwv.Eidos.Cexp.cexpFull_varsWF hΓ (show
                  Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ (args[i]'(by omega))
                    = .ok ((pas[i]'(by omega)).1, (pas[i]'(by omega)).2) from by
                  rw [show ((pas[i]'(by omega)).1, (pas[i]'(by omega)).2)
                        = pas[i]'(by omega) from rfl]
                  exact hpe)
            · obtain ⟨d, hd, hdp⟩ := List.mem_map.mp hp
              subst hdp
              exact hcw d hd
        | goto l args =>
            rw [goTerm] at h
            cases hblk : C.blocks.get? l.uniq with
            | none => rw [hblk] at h; exact error_ne_ok h
            | some blk =>
                rw [hblk] at h
                dsimp only at h
                obtain ⟨pas, hpas, h⟩ := except_bind_eq_ok h
                split at h
                rotate_left
                · exact error_ne_ok h
                refine ihc _ cells blk.cmds blk.term rec h ?_ hcw
                refine gammaWF_foldl_zip blk.params pas (fun u nt hu => by
                  rw [HashMap.get?_eq_getElem?] at hu
                  simp at hu) ?_
                intro nt hnt
                obtain ⟨i, hi, hnti⟩ := List.getElem_of_mem hnt
                obtain ⟨_, hpe⟩ := (mapM_ok_idx hpas).2 i (by
                  have := (mapM_ok_idx hpas).1
                  omega)
                subst hnti
                exact Rwv.Eidos.Cexp.cexpFull_varsWF hΓ (by
                  rw [show ((pas[i]'hi).1, (pas[i]'hi).2) = pas[i]'hi from rfl]
                  exact hpe)
        | halt e =>
            rw [goTerm] at h
            obtain ⟨at0, hat, h⟩ := except_bind_eq_ok h
            obtain ⟨anf, aty⟩ := at0
            dsimp only at h
            cases hfd : C.lo.halts.find? (fun h => h.1 == aty) with
            | none =>
                rw [hfd] at h
                exact error_ne_ok h
            | some tw =>
                rw [hfd] at h
                obtain ⟨t0, atag, aw⟩ := tw
                dsimp only at h
                rw [except_pure_def, except_bind_ok, except_pure_def] at h
                injection h with h
                subst h
                rw [haltRec]
                refine catNF_varsWF ?_
                intro p hp
                rcases List.mem_append.mp hp with hp | hp
                · simp only [List.mem_cons] at hp
                  rcases hp with rfl | rfl | rfl | rfl | rfl | hp
                  · trivial
                  · trivial
                  · trivial
                  · trivial
                  · exact Rwv.Eidos.Cexp.cexpFull_varsWF hΓ hat
                  · exact absurd hp (by simp)
                · obtain ⟨d, hd, hdp⟩ := List.mem_map.mp hp
                  subst hdp
                  exact hcw d hd
        | cases scrutE alts =>
            rw [goTerm] at h
            obtain ⟨dt0, hdn, h⟩ := except_bind_eq_ok h
            obtain ⟨dn, dty⟩ := dt0
            dsimp only at h
            obtain ⟨szT, hszT, h⟩ := except_bind_eq_ok h
            have hdnW : NF.VarsWF P dn := Rwv.Eidos.Cexp.cexpFull_varsWF hΓ hdn
            cases alts with
            | nil => exact (goAlts_nil_none_err h).elim
            | cons a rest2 =>
                obtain ⟨acon, cbs, ct⟩ := a
                cases acon with
                | default =>
                    rw [show (match (TAlt.mk .default cbs ct :: rest2 : List TAlt) with
                          | .mk .default bs dt :: rest =>
                              if bs.isEmpty then do
                                let els ← goTerm C N Γ cells dt
                                goAlts C N Γ cells dty szT dn rest (some els)
                              else throw "cstep: default alternative with binders"
                          | rest => goAlts C N Γ cells dty szT dn rest none)
                        = (if cbs.isEmpty then do
                            let els ← goTerm C N Γ cells ct
                            goAlts C N Γ cells dty szT dn rest2 (some els)
                          else (throw "cstep: default alternative with binders"
                              : Except String NF)) from rfl] at h
                    split at h
                    rotate_left
                    · exact error_ne_ok h
                    obtain ⟨els, hels, h⟩ := except_bind_eq_ok h
                    exact ihas Γ cells dty szT dn rest2 (some els) rec h hΓ hcw hdnW
                      (fun acc hacc => by
                        injection hacc with hacc
                        subst hacc
                        exact iht Γ cells ct els hels hΓ hcw)
                | dataAlt cn =>
                    rw [show (match (TAlt.mk (.dataAlt cn) cbs ct :: rest2 : List TAlt) with
                          | .mk .default bs dt :: rest =>
                              if bs.isEmpty then do
                                let els ← goTerm C N Γ cells dt
                                goAlts C N Γ cells dty szT dn rest (some els)
                              else throw "cstep: default alternative with binders"
                          | rest => goAlts C N Γ cells dty szT dn rest none)
                        = goAlts C N Γ cells dty szT dn
                            (TAlt.mk (.dataAlt cn) cbs ct :: rest2) none from rfl] at h
                    exact ihas Γ cells dty szT dn _ none rec h hΓ hcw hdnW
                      (fun acc hacc => nomatch hacc)
                | litAlt n =>
                    rw [show (match (TAlt.mk (.litAlt n) cbs ct :: rest2 : List TAlt) with
                          | .mk .default bs dt :: rest =>
                              if bs.isEmpty then do
                                let els ← goTerm C N Γ cells dt
                                goAlts C N Γ cells dty szT dn rest (some els)
                              else throw "cstep: default alternative with binders"
                          | rest => goAlts C N Γ cells dty szT dn rest none)
                        = goAlts C N Γ cells dty szT dn
                            (TAlt.mk (.litAlt n) cbs ct :: rest2) none from rfl] at h
                    exact ihas Γ cells dty szT dn _ none rec h hΓ hcw hdnW
                      (fun acc hacc => nomatch hacc)
      · -- goAlts
        intro Γ cells dty szT dn alts macc rec h hΓ hcw hdnW hmW
        cases alts with
        | nil =>
            cases macc with
            | none => exact (goAlts_nil_none_err h).elim
            | some els =>
                rw [show goAlts C (N + 1) Γ cells dty szT dn [] (some els)
                      = (pure els : Except String NF) from rfl, except_pure_def] at h
                injection h with h
                subst h
                exact hmW els rfl
        | cons alt rest' =>
            cases rest' with
            | nil =>
                cases macc with
                | none =>
                    rw [show goAlts C (N + 1) Γ cells dty szT dn [alt] none
                          = goAlt1 C N Γ cells dty szT dn alt none from rfl] at h
                    exact iha1 Γ cells dty szT dn alt none rec h hΓ hcw hdnW
                      (fun acc hacc => nomatch hacc)
                | some els =>
                    rw [show goAlts C (N + 1) Γ cells dty szT dn [alt] (some els)
                          = (do
                              let acc ← goAlts C N Γ cells dty szT dn [] (some els)
                              goAlt1 C N Γ cells dty szT dn alt (some acc)) from rfl] at h
                    obtain ⟨acc, hacc, h1⟩ := except_bind_eq_ok h
                    exact iha1 Γ cells dty szT dn alt (some acc) rec h1 hΓ hcw hdnW
                      (fun acc' hacc' => by
                        injection hacc' with hacc'
                        subst hacc'
                        exact ihas Γ cells dty szT dn [] (some els) acc hacc hΓ hcw hdnW hmW)
            | cons a2 rest2 =>
                rw [show goAlts C (N + 1) Γ cells dty szT dn (alt :: a2 :: rest2) macc
                      = (do
                          let acc ← goAlts C N Γ cells dty szT dn (a2 :: rest2) macc
                          goAlt1 C N Γ cells dty szT dn alt (some acc)) from rfl] at h
                obtain ⟨acc, hacc, h1⟩ := except_bind_eq_ok h
                exact iha1 Γ cells dty szT dn alt (some acc) rec h1 hΓ hcw hdnW
                  (fun acc' hacc' => by
                    injection hacc' with hacc'
                    subst hacc'
                    exact ihas Γ cells dty szT dn (a2 :: rest2) macc acc hacc hΓ hcw hdnW hmW)
      · -- goAlt1
        intro Γ cells dty szT dn alt macc rec h hΓ hcw hdnW hmW
        obtain ⟨con, abs, at'⟩ := alt
        cases con with
        | default => exact (goAlt1_default h).elim
        | litAlt n =>
            rw [goAlt1] at h
            split at h
            rotate_left
            · exact error_ne_ok h
            obtain ⟨bnf', hbnf', h⟩ := except_bind_eq_ok h
            have hbW := iht Γ cells at' bnf' hbnf' hΓ hcw
            cases macc with
            | none =>
                rw [except_pure_def] at h
                injection h with h
                subst h
                exact hbW
            | some acc =>
                rw [except_pure_def] at h
                injection h with h
                subst h
                exact ⟨⟨hdnW, trivial⟩, hbW, hmW acc rfl⟩
        | dataAlt cn =>
            rw [goAlt1] at h
            split at h
            · exact error_ne_ok h
            split at h
            rotate_left
            · exact error_ne_ok h
            obtain ⟨tg, htag, h1⟩ := except_bind_eq_ok h
            clear h
            obtain ⟨tag, w⟩ := tg
            cases hsig2 : C.Δ.ctorSig.get? cn with
            | none => rw [hsig2] at h1; dsimp only at h1; exact error_ne_ok h1
            | some sig =>
            rw [hsig2] at h1
            dsimp only at h1
            obtain ⟨sub, hsub2, h2⟩ := except_bind_eq_ok h1
            clear h1
            split at h2
            rotate_left
            · exact error_ne_ok h2
            obtain ⟨szXs, hszXs, h3⟩ := except_bind_eq_ok h2
            clear h2
            split at h3
            rotate_left
            · exact error_ne_ok h3
            obtain ⟨bnf', hbnf', h4⟩ := except_bind_eq_ok h3
            clear h3
            have hbW := iht _ cells at' bnf' hbnf' (by
              refine gammaWF_foldl_zip _ _ hΓ ?_
              intro nt hnt
              have h1 := (List.of_mem_zip hnt).1
              obtain ⟨q, hq, hqn⟩ := List.mem_map.mp h1
              rw [← hqn]
              exact sliceNF_varsWF hdnW) hcw
            cases macc with
            | none =>
                cases w with
                | zero =>
                    dsimp only at h4
                    rw [except_pure_def] at h4
                    injection h4 with h4
                    subst h4
                    exact hbW
                | succ w0 =>
                    dsimp only at h4
                    rw [except_pure_def] at h4
                    injection h4 with h4
                    subst h4
                    exact hbW
            | some acc =>
                cases w with
                | zero =>
                    dsimp only at h4
                    rw [except_pure_def] at h4
                    injection h4 with h4
                    subst h4
                    exact hbW
                | succ w0 =>
                    dsimp only at h4
                    rw [except_pure_def] at h4
                    injection h4 with h4
                    subst h4
                    exact ⟨⟨sliceNF_varsWF hdnW, trivial⟩, hbW, hmW acc rfl⟩

/-! ## Axiom audit -/

#print axioms vtyB_sound
#print axioms mkPlan_nodup
#print axioms initStateOk_sound
#print axioms checkInit_sound
#print axioms substNF_eval
#print axioms substNF_varsWF
#print axioms tagSubst_eval
#print axioms tagFix_of_store
#print axioms hasTy_vty
#print axioms vty_rep_total
#print axioms selectTAlt_char
#print axioms mkLayoutL_inv
#print axioms mkPlan_inv
#print axioms cstep_sound
#print axioms goCmds_varsWF

end Rwv.Eidos.Cstep

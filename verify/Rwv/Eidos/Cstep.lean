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

The remaining step-half soundness (the analogue of `cexp_sound` at
command/terminator level, composed with `Bridge.symStep_sound` into
`Rwv.StepObligations`) is staged work; the exact obstruction state is
recorded in the phase notes rather than stated with `sorry` here.
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

/-- Build and check the plan against the device interface. The
name-distinctness check comes first (`mkPlan_nodup` inverts it). -/
def mkPlan (Δ : DEnv) (fuel : Nat) (p : Proc) (lo : Layout) (dev : Rwv.Hyle.Device) :
    Except String Plan := do
  unless Rwv.Hyle.Bridge.nodupB (dev.inputs.map (·.1) ++ dev.registers.map (·.name)) do
    throw "duplicate input/register names"
  let inSzs ← Val.detupleSizes Δ fuel p.inTy
  let outSzs ← Val.detupleSizes Δ fuel p.outTy
  unless dev.inputs.map (·.2) == inSzs do
    throw s!"device inputs {dev.inputs.map (·.2)} ≠ detupleSizes(inTy) {inSzs}"
  unless dev.outputs.map (·.2) == outSzs do
    throw s!"device outputs {dev.outputs.map (·.2)} ≠ detupleSizes(outTy) {outSzs}"
  let regs := dev.registers.map fun r => (r.name, r.width)
  let (tagReg, stRegs) ←
    if lo.rW > 0 then
      match regs with
      | (r, w) :: rest =>
          if w = lo.rW then pure (some (r, w), rest)
          else throw s!"first register {r} has width {w}, resumption load is {lo.rW}"
      | [] => throw "no registers, but the resumption load is nonzero"
    else pure (none, regs)
  let (cellsR, rest) ← lo.cells.foldlM (init := (([] : List CellPlan), stRegs))
    fun (acc, rem) (nm, ty, w) => do
      let szs ← Val.detupleSizes Δ fuel ty
      unless szs.sum == w do throw s!"cell {nm}: detupleSizes {szs} do not sum to {w}"
      let (run, rem') ← takeRegs rem szs
      pure (acc ++ [{ name := nm, ty, width := w, regs := run }], rem')
  unless rest.isEmpty do throw s!"unaccounted device registers: {rest.map (·.1)}"
  pure { tagReg, cells := cellsR, inPorts := dev.inputs, outPorts := dev.outputs }

/-! ## The state encoding -/

/-- The resumption-tag register's value for a state:
`tag | zero pad | rep args`, width rW. -/
def encTag (lo : Layout) (tag : Nat) (argWs : List Nat) (reps : List BV) : BV :=
  Val.bvConcat (⟨lo.rTagW, BitVec.ofNat _ tag⟩ :: ⟨lo.rPayW - argWs.sum, 0⟩ :: reps)

/-- Split a cell's representation across its register run (MSB-first
consecutive slices). -/
def encCellRegs (regs : List (String × Nat)) (bv : BV) :
    Except String (List (String × BV)) := do
  let ws := regs.map (·.2)
  unless ws.sum == bv.width do
    throw s!"cell registers {ws} do not cover the representation ({bv.width})"
  pure ((regs.zip (offsetsOf ws)).map fun ((r, w), off) =>
    (r, ⟨w, bv.bits.extractLsb' off w⟩))

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
  unless reps.map (·.width) == tgt.argWs do
    throw s!"encode: argument widths {reps.map (·.width)} ≠ layout {tgt.argWs}"
  let tagPart := match plan.tagReg with
    | none => []
    | some (r, _) => [(r, encTag lo tgt.tag tgt.argWs reps)]
  let cellParts ← plan.cells.mapM fun c => do
    match s.cells.get? c.name with
    | none => throw s!"encode: missing cell {c.name}"
    | some v => do
        let bv ← Val.rep Δ fuel v
        unless bv.width == c.width do
          throw s!"encode: cell {c.name} rep width {bv.width} ≠ {c.width}"
        encCellRegs c.regs bv
  pure (tagPart ++ cellParts.flatten)

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
          unless teq oty C.outTy do throw "cstep: pause output type mismatch"
          let tgt ← match C.lo.targets.find? (fun t => t.uniq == l.uniq) with
            | some tgt => pure tgt
            | none => throw s!"cstep: pause to an unknown target {l.occ}"
          let pas ← args.mapM (Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ)
          unless teqAll pas tgt.argTys do throw "cstep: pause argument type mismatch"
          pure (pauseRec C onf tgt (pas.map (·.1)) cells)
      | .goto l args => do
          match C.blocks.get? l.uniq with
          | none => throw s!"cstep: goto to an unknown block {l.occ}"
          | some blk => do
              let pas ← args.mapM (Rwv.Eidos.Cexp.cexpFull C.Δ C.edm C.cexpFuel Γ)
              unless teqAll pas (blk.params.map (·.sig.ty)) do
                throw s!"cstep: goto {l.occ} argument mismatch"
              let Γ' := (blk.params.zip pas).foldl
                (fun m (x, nt) => m.insert x.uniq nt) (∅ : HashMap Int (NF × Ty))
              goCmds C fuel Γ' cells blk.cmds blk.term
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
          | .mk .default _ dt :: rest => do
              let els ← goTerm C fuel Γ cells dt
              goAlts C fuel Γ cells dty szT dn rest (some els)
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

/-- One terminator alternative (the machine-level `Cexp.cAlt`): a
DataAlt tests the tag slice and binds field slices at the wireOffsets
positions; a LitAlt compares the full atom. -/
def goAlt1 (C : Ctx) : Nat → HashMap Int (NF × Ty) → List CellNF → Ty → Nat → NF →
    TAlt → Option NF → Except String NF
  | 0, _, _, _, _, _, _, _ => throw "cstep: out of fuel"
  | _ + 1, _, _, _, _, _, .mk .default _ _, _ =>
      throw "cstep: default alternative not first"
  | fuel + 1, Γ, cells, dty, szT, dn, .mk (.dataAlt cn) xs t, macc => do
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
  | fuel + 1, Γ, cells, _dty, szT, dn, .mk (.litAlt i) _ t, macc => do
      let bnf ← goTerm C fuel Γ cells t
      match macc with
      | some acc =>
          pure (.ite (.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT i⟩)) bnf acc)
      | none => pure bnf

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
  unless blk.params.length == tgt.argWs.length + 1 do
    throw "checkLabel: block arity does not match the layout target"
  let some inP := blk.params.getLast? | throw "checkLabel: parameterless pause target"
  unless teq inP.sig.ty inTy do
    throw "checkLabel: resumed-input parameter is not at the process input type"
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
  let outWs := dev.outputs.map (·.2)
  let outOffs := (offsetsOf outWs).map (· + C.lo.rW + C.lo.cellsW)
  for (((o, w), off), (o', nf)) in (dev.outputs.zip outOffs).zip ss.outs do
    unless o == o' do throw s!"checkLabel: output order drift ({o} vs {o'})"
    unless ceqB (sliceNF off w rec) (substNF θ nf) do
      throw s!"label tag {tgt.tag}: output {o} disagrees"
  -- Registers: tag register above the cells, cells at the LSB end.
  let regWs := dev.registers.map (·.width)
  let regOffs := offsetsOf regWs
  for ((r, off), (r', nf)) in (dev.registers.zip regOffs).zip ss.nexts do
    unless r.name == r' do throw s!"checkLabel: register order drift ({r.name} vs {r'})"
    unless ceqB (sliceNF off r.width rec) (substNF θ nf) do
      throw s!"label tag {tgt.tag}: register {r.name} disagrees"
  pure ()

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

/-- The whole-process validator, with a diagnostic message on
failure: layout, plan, the device's symbolic step, every pause
target's per-label obligation, and the initial-state obligation. -/
def validateProcE (Δ : DEnv) (edm : HashMap Int Defn) (p : Proc)
    (H : Rwv.Hyle.Program) (fuel : Nat) : Except String Unit := do
  unless denvOk Δ do throw "validateProc: denvOk failed"
  let lo ← mkLayoutL Δ fuel p
  let plan ← mkPlan Δ fuel p lo H.device
  let blocks : HashMap Int Block :=
    HashMap.ofList (p.blocks.map fun (l, b) => (l.uniq, b))
  let C : Ctx := { Δ, edm, lo, blocks, cexpFuel := fuel, outTy := p.outTy }
  let hyleFuel := Rwv.Hyle.Bridge.progFuel H
  let ss ← Rwv.Hyle.Bridge.symStep (Rwv.Hyle.Bridge.dmapOf H) hyleFuel H.device
  for tgt in lo.targets do
    checkLabel C plan H.device ss p.inTy fuel tgt
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

/-- The prim-basis tuple discipline: every tuple datatype is declared
with its single eponymous constructor. -/
def tupleCtorsOk (Δ : DEnv) : Bool :=
  Δ.ctors.toList.all fun pr => !Ty.isTupleCon pr.1 || pr.2.all (· == pr.1)

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

end Rwv.Eidos.Cstep

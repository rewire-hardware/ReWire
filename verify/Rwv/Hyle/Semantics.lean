/-
The dynamic semantics of Hyle: a transcription of doc/hyle.md §5–6
(values and primitive denotations; expressions, definitions, and the
instance-free device step and stream semantics), mirroring the Haskell
reference interpreter (rewire-backend ReWire.Hyle.Interp) construct for
construct.

Definitions are denoted in dependency order — the program's acyclic
call graph (doc/hyle.md §4.3) makes §6.2 a well-founded definition, and
here that shows up as `topoDefns` ordering the fold that builds the
definition environment, after which expression evaluation is
structurally recursive.

Evaluation runs in `Except String`: on well-formed programs (the §4
checker) no error case is reachable except the deliberate one —
device instances (the instance-free fragment, §6.4). Model-less
combinational extern calls, which the Haskell interpreter rejects
(doc/hyle.md §6.1), deliberately diverge here: they read totally
through `Sem.xapply` (the η reading). Errors are precise rather than
defaulted so the differential harness against the Haskell interpreter
cannot mask disagreements.
-/
import Rwv.Hyle.Syntax
import Std.Data.HashMap

namespace Rwv.Hyle

open Std (HashMap)

/-! ## Primitive denotations (doc/hyle.md §5.2) -/

namespace Sem

/-- Modular exponentiation by squaring (the ⟨x^y⟩ₙ denotation must not
compute x^y as a natural number — exponents are up to 2^width). -/
def powMod (b e m : Nat) : Nat :=
  if m = 0 then 0 else go (b % m) e
where
  go (b e : Nat) : Nat :=
    if h : e = 0 then 1 % m
    else
      let r := go (b * b % m) (e / 2)
      if e % 2 = 1 then r * b % m else r
  termination_by e
  decreasing_by exact Nat.div_lt_self (Nat.pos_of_ne_zero h) (by omega)

def parity (x : BV) : Bool :=
  (List.range x.width).foldl (fun acc i => acc ^^ x.bits.getLsbD i) false

/-- Width-1 boolean. -/
def b1 (b : Bool) : BV := ⟨1, if b then 1 else 0⟩

private def arity2 (op : String) (k : BV → BV → BV) : List BV → Except String BV
  | [x, y] => .ok (k x y)
  | args   => .error s!"{op}: arity mismatch (expected 2, got {args.length})"

private def arity1 (op : String) (k : BV → BV) : List BV → Except String BV
  | [x]  => .ok (k x)
  | args => .error s!"{op}: arity mismatch (expected 1, got {args.length})"

/-- Reconcile the second operand's width to the first's. On checked
programs the widths are already equal (G1) and this is the identity. -/
private def bin (op : String) (k : ∀ n, BitVec n → BitVec n → BitVec n) : List BV → Except String BV :=
  arity2 op fun x y => ⟨x.width, k x.width x.bits (y.bits.setWidth x.width)⟩

private def cmp (op : String) (k : ∀ n, BitVec n → BitVec n → Bool) : List BV → Except String BV :=
  arity2 op fun x y => b1 (k x.width x.bits (y.bits.setWidth x.width))

/-- ⟦op⟧, the table of doc/hyle.md §5.2. Division and modulus by zero
follow SMT-LIB (`bvudiv`: 2ⁿ−1; `bvurem`: x — which is Lean's `Nat`
convention for `%`, hence `umod`); shifts by ≥ n give zero (all-sign
for `ashr`); the reduction and comparison edge cases at width 0 fall
out of the same definitions as in the spec. -/
def evalOp : Op → List BV → Except String BV
  | .add    => bin "add" fun _ => (· + ·)
  | .sub    => bin "sub" fun _ => (· - ·)
  | .mul    => bin "mul" fun _ => (· * ·)
  | .udiv   => bin "udiv" fun _ => BitVec.smtUDiv
  | .umod   => bin "umod" fun _ => (· % ·)
  | .pow    => bin "pow" fun n x y => BitVec.ofNat n (powMod x.toNat y.toNat (2 ^ n))
  | .and    => bin "and" fun _ => (· &&& ·)
  | .or     => bin "or"  fun _ => (· ||| ·)
  | .xor    => bin "xor" fun _ => (· ^^^ ·)
  | .not    => arity1 "not" fun x => ⟨x.width, ~~~ x.bits⟩
  | .shl    => arity2 "shl"  fun x y => ⟨x.width, x.bits <<< y.nat⟩
  | .lshr   => arity2 "lshr" fun x y => ⟨x.width, x.bits >>> y.nat⟩
  | .ashr   => arity2 "ashr" fun x y => ⟨x.width, x.bits.sshiftRight y.nat⟩
  | .eq     => cmp "eq" fun _ x y => x == y
  | .ne     => cmp "ne" fun _ x y => x != y
  | .ult    => cmp "ult" fun _ => BitVec.ult
  | .ule    => cmp "ule" fun _ => BitVec.ule
  | .ugt    => cmp "ugt" fun _ x y => y.ult x
  | .uge    => cmp "uge" fun _ x y => y.ule x
  | .slt    => cmp "slt" fun _ => BitVec.slt
  | .sle    => cmp "sle" fun _ => BitVec.sle
  | .sgt    => cmp "sgt" fun _ x y => y.slt x
  | .sge    => cmp "sge" fun _ x y => y.sle x
  | .redand => arity1 "redand" fun x => b1 (x.bits == BitVec.allOnes x.width)
  | .redor  => arity1 "redor"  fun x => b1 (x.bits != 0)
  | .redxor => arity1 "redxor" fun x => b1 (parity x)
  | .zext m  => arity1 "zext"  fun x => ⟨m, x.bits.setWidth m⟩
  | .sext m  => arity1 "sext"  fun x => ⟨m, x.bits.signExtend m⟩
  | .trunc m => arity1 "trunc" fun x => ⟨m, x.bits.setWidth m⟩
  | .rep k   => arity1 "rep"   fun x => ⟨_, x.bits.replicate k⟩

/-! ## Expressions and definitions (doc/hyle.md §6.2) -/

/-- Denotations of definitions (and, via their models, of combinational
externs): what 𝔉⟦·⟧ assigns. Built below in dependency order. -/
abbrev FEnv := HashMap String (List BV → Except String BV)

/-- The static extern table: extern name ↦ model definition name, for
model-carrying combinational externs. A lookup miss is a model-less
extern, whose interpretation (if any) comes from the extern
environment `EEnv` below. -/
abbrev XEnv := HashMap String String

/-- Bit-level interpretations of MODEL-LESS combinational externs
(the η tier): per extern name AND static generic instantiation (the
positional generic values of the call — doc/hyle.md §6.1 defines η
over ℕ^g × BV, since distinct instantiations of a parameterized
extern are distinct functions), a function of the CONCATENATION of
the input ports (MSB-first, in port order). The correspondence
statement quantifies over this environment with both semantics
reading the SAME one — the algebraic η_alg enters by instantiating
it at `rep ∘ η ∘ decode` (Rwv.Eidos.Cstep.etaB). -/
abbrev EEnv := String → List Nat → Option (BV → Except String BV)

/-- The empty extern environment: every model-less extern
uninterpreted. The default everywhere, under which every definition
and theorem means exactly what it meant before the extension. -/
def eEmpty : EEnv := fun _ _ => none

/-- Concatenate bit vectors, left = most significant (the Eidos side's
`Val.bvConcat`, transcribed). -/
def bvcat (xs : List BV) : BV :=
  xs.foldl (fun acc x => ⟨_, acc.bits ++ x.bits⟩) BV.nil

/-- The TOTAL reading of a model-less extern application at a cached
result width: the interpretation's value when it exists, has a value,
and carries the cached width; the zero vector otherwise. Totality (the
width clamp included) is what keeps the symbolic evaluator's strong
soundness — and every annotation-width fact — unconditional in `E`;
the Eidos-side row (`Eval.evalExt`) keeps loud errors and a decode
canonicality gate, so the clamp is inert wherever the correspondence
statement has content. -/
def xapply (E : EEnv) (ext : String) (gs : List Nat) (w : Nat) (bv : BV) : BV :=
  match E ext gs with
  | some f =>
      match f bv with
      | .ok r => if r.width = w then r else BV.zero w
      | .error _ => BV.zero w
  | none => BV.zero w

/-- `xapply` always returns the cached width. -/
theorem xapply_width (E : EEnv) (ext : String) (gs : List Nat) (w : Nat) (bv : BV) :
    (xapply E ext gs w bv).width = w := by
  rw [xapply]
  split
  · split
    · split
      · assumption
      · rfl
    · rfl
  · rfl

end Sem

open Sem

/-- ℰ⟦e⟧ρ (doc/hyle.md §6.2): structurally recursive expression
evaluation against an environment of already-denoted definitions. The
mux is short-circuiting, as in the interpreter (mathematically eager —
both arms denote, and evaluation is effect-free on checked programs, so
the difference is unobservable). The trailing extern environment `E`
(defaulted empty) interprets MODEL-LESS extern calls — with a model
the §6.1 model path is unchanged, errors included; without one, the
call reads TOTALLY through `Sem.xapply` at the call's static generic
instantiation (the environment is keyed by (name, generics), so
distinct instantiations are distinct uninterpreted functions). -/
def evalExp (F : Sem.FEnv) (X : Sem.XEnv) (ρ : HashMap String BV) (e : Exp)
    (E : Sem.EEnv := Sem.eEmpty) : Except String BV :=
  match e with
  | .lit v    => .ok v
  | .undef w  => .ok (BV.zero w)   -- undef denotes zero (§5.1)
  | .var _ x  =>
      match ρ.get? x with
      | some v => .ok v
      | none   => .error s!"unbound variable {x}"
  | .cat e₁ e₂ => do
      let v₁ ← evalExp F X ρ e₁ E
      let v₂ ← evalExp F X ρ e₂ E
      .ok ⟨_, v₁.bits ++ v₂.bits⟩
  | .slice i w e => do
      let v ← evalExp F X ρ e E
      .ok ⟨w, v.bits.extractLsb' i w⟩
  | .prim _ op args => do
      let vs ← args.attach.mapM fun ⟨a, _⟩ => evalExp F X ρ a E
      evalOp op vs
  | .call _ f args => do
      let vs ← args.attach.mapM fun ⟨a, _⟩ => evalExp F X ρ a E
      match F.get? f with
      | some fn => fn vs
      | none    => .error s!"unknown definition {f}"
  | .xcall w ext gs args => do
      let vs ← args.attach.mapM fun ⟨a, _⟩ => evalExp F X ρ a E
      match X.get? ext with
      | some model =>
          match F.get? model with
          | some fn => fn vs
          | none    => .error s!"extern {ext}: unknown model {model}"
      | none => .ok (Sem.xapply E ext gs w (Sem.bvcat vs))
  | .ite _ c t e => do
      let vc ← evalExp F X ρ c E
      if vc.nat ≠ 0 then evalExp F X ρ t E else evalExp F X ρ e E
  | .letE _ x rhs body => do
      let v ← evalExp F X ρ rhs E
      evalExp F X (ρ.insert x v) body E

namespace Sem

/-- The names of definitions an expression's evaluation depends on:
called definitions, plus the models of any externs it calls (resolved
against the static extern table). -/
def deps (X : XEnv) : Exp → List String
  | .lit _ | .undef _ | .var _ _ => []
  | .cat e₁ e₂       => deps X e₁ ++ deps X e₂
  | .slice _ _ e     => deps X e
  | .prim _ _ args   => args.attach.flatMap fun ⟨a, _⟩ => deps X a
  | .call _ f args   => f :: args.attach.flatMap fun ⟨a, _⟩ => deps X a
  | .xcall _ ext _ args =>
      (X.get? ext).toList ++ args.attach.flatMap fun ⟨a, _⟩ => deps X a
  | .ite _ c t e     => deps X c ++ deps X t ++ deps X e
  | .letE _ _ rhs b  => deps X rhs ++ deps X b

/-- Order the definitions so every definition follows its dependencies:
the §6.2 well-founded order made explicit. Kahn-style selection with a
pass counter; failure to progress means recursion, which the checker
rejects (doc/hyle.md §4.3). -/
def topoDefns (X : XEnv) (defns : List Defn) : Except String (List Defn) :=
  go defns.length [] defns
where
  go : Nat → List Defn → List Defn → Except String (List Defn)
    | _, acc, [] => .ok acc.reverse
    | 0, _, remaining =>
        .error s!"recursion among definitions: {String.intercalate ", " (remaining.map (·.name))}"
    | fuel + 1, acc, remaining =>
        let done := acc.map (·.name)
        let (ready, rest) := remaining.partition fun d =>
          (deps X d.body).all fun f => done.contains f || remaining.all (·.name ≠ f)
        if ready.isEmpty then
          .error s!"recursion among definitions: {String.intercalate ", " (remaining.map (·.name))}"
        else
          go fuel (ready.reverse ++ acc) rest

/-- The static extern table of a program. -/
def xenv (p : Program) : XEnv :=
  p.externs.foldl (init := (∅ : XEnv)) fun m e =>
    match e.model with
    | some f => m.insert e.name f
    | none   => m

/-- 𝔉⟦·⟧ (doc/hyle.md §6.2): denote every definition, folding in
dependency order so each body evaluates against an environment that
already contains its callees. The extern environment threads into
every closure — a definition's meaning depends on the interpretations
of the model-less externs it calls. -/
def mkFEnv (p : Program) (E : EEnv := eEmpty) : Except String FEnv := do
  let X := xenv p
  let ordered ← topoDefns X p.defns
  ordered.foldlM (init := (∅ : FEnv)) fun F d =>
    let fn : List BV → Except String BV := fun vs =>
      if vs.length = d.params.length then
        evalExp F X (HashMap.ofList (d.params.zip vs)) d.body E
      else
        .error s!"{d.name}: arity mismatch (expected {d.params.length}, got {vs.length})"
    pure (F.insert d.name fn)

end Sem

/-! ## The device step and stream semantics (doc/hyle.md §6.3–§6.4) -/

namespace Sem

/-- One device step (doc/hyle.md §6.3): build the environment from
inputs and registers, fold the statements in order, and read off the
outputs and next register values — each assigned exactly once on
checked programs (missing or duplicated assignments are errors here).
Instance statements are outside the instance-free fragment. -/
def step (F : FEnv) (X : XEnv) (dev : Device) (regs : HashMap String BV) (ins : List BV)
    (E : EEnv := eEmpty) : Except String (List BV × HashMap String BV) := do
  if ins.length ≠ dev.inputs.length then
    .error s!"stimulus arity: got {ins.length} inputs, device has {dev.inputs.length}"
  let ρ₀ := HashMap.ofList ((dev.inputs.map Prod.fst).zip ins) |>.union regs
  let (_, outs, nexts) ← dev.body.foldlM
      (init := (ρ₀, (∅ : HashMap String BV), (∅ : HashMap String BV)))
      fun (ρ, outs, nexts) stmt => do
        match stmt with
        | .sLet x e => do
            let v ← evalExp F X ρ e E
            pure (ρ.insert x v, outs, nexts)
        | .sOutput o e => do
            if outs.contains o then .error s!"output {o} assigned twice"
            let v ← evalExp F X ρ e E
            pure (ρ, outs.insert o v, nexts)
        | .sNext r e => do
            if nexts.contains r then .error s!"register {r} assigned twice"
            let v ← evalExp F X ρ e E
            pure (ρ, outs, nexts.insert r v)
        | .sInstIn inst _ _ => .error s!"device instance {inst}: outside the instance-free fragment"
  let outVals ← dev.outputs.mapM fun (o, _) =>
    match outs.get? o with
    | some v => pure v
    | none   => .error s!"output {o} never assigned"
  let regVals ← dev.registers.foldlM (init := (∅ : HashMap String BV)) fun m r =>
    match nexts.get? r.name with
    | some v => pure (m.insert r.name v)
    | none   => .error s!"register {r.name} never assigned"
  pure (outVals, regVals)

/-- The declared register initials: s(0) of the §6.4 recurrence. -/
def initRegs (dev : Device) : HashMap String BV :=
  HashMap.ofList (dev.registers.map fun r => (r.name, r.init))

/-- One iteration of the §6.4 recurrence as `run`'s fold body: step
the device from the current register store, pushing the cycle's
outputs onto the (reversed) trace accumulator. Named (rather than
inline in `run`) so proofs can reason about the fold by its
equations. -/
def foldStep (F : FEnv) (X : XEnv) (dev : Device) (E : EEnv := eEmpty) :
    HashMap String BV × List (List BV) → List BV →
    Except String (HashMap String BV × List (List BV))
  | (regs, acc), ins => do
      let (outs, regs') ← step F X dev regs ins E
      pure (regs', outs :: acc)

/-- The n-prefix of 𝔇⟦device⟧ (doc/hyle.md §6.4, instance-free): the
Mealy unfolding from the declared initials over a finite stimulus, the
semantics `--interpret` and the golden traces realize. -/
def run (F : FEnv) (X : XEnv) (dev : Device) (stimulus : List (List BV))
    (E : EEnv := eEmpty) : Except String (List (List BV)) := do
  let (_, outsRev) ← stimulus.foldlM (init := (initRegs dev, ([] : List (List BV)))) (foldStep F X dev E)
  pure outsRev.reverse

end Sem

/-- Run a whole program on a finite stimulus (inputs positionally per
the device's input ports, one list per cycle). -/
def Program.run (p : Program) (stimulus : List (List BV))
    (E : Sem.EEnv := Sem.eEmpty) : Except String (List (List BV)) := do
  let F ← Sem.mkFEnv p E
  Sem.run F (Sem.xenv p) p.device stimulus E

end Rwv.Hyle

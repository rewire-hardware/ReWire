/-
The verified reflection bridge: a Hyle≃Hyle equivalence checker whose
`true` verdict is connected by proof to the committed semantics
(Rwv.Hyle.Semantics), replacing the untrusted-generator trust of
`rwv-hyle-equiv`.

The pieces:

* `NF` — a first-order normal-form term language over a device's free
  variables (inputs and registers), with a TOTAL denotation `NF.eval`
  (primitive applications default on `Sem.evalOp`'s error cases; the
  symbolic evaluator only builds arity-correct applications, so the
  default is unreachable on its output, and totality keeps the
  denotation clean). Primitives are split by arity (`prim1`/`prim2` —
  every Hyle operation is unary or binary, doc/hyle.md §3.3) rather
  than carrying a `List NF`, which keeps the type non-nested and its
  induction principle and decidable equality free.

* `symExp` — a fueled symbolic evaluator mirroring `evalExp` construct
  for construct, inlining calls through the program's definition map.
  EVERY recursive call consumes fuel (not just call inlining), so the
  definition is structural in the fuel and its soundness proof is a
  plain induction on fuel with a case split on the expression — no
  nested-list induction. The checker picks a fuel bound from term
  sizes (`progFuel`); an exhausted fuel is a checker `false`, never an
  unsoundness.

* `symExp_sound` — THE bridge theorem: a successful symbolic
  evaluation is denotation-preserving, for any definition environment
  `F` that implements the definition map (`FImplements`, established
  for `Sem.mkFEnv`'s output by `mkFEnv_implements`). The call case
  rests on a characterization of `mkFEnv`'s fold (each definition's
  closure evaluates its body against the FINAL environment — the
  prefix environment it captured agrees with the final one on the
  body's dependencies, by `topoDefns`'s ordering guarantee and
  `evalExp`'s dependency congruence).

* `symStep`/`symStep_sound` — the device step, symbolically: fold the
  body statements exactly as `Sem.step` does, producing per-output and
  per-register-next normal forms; soundness says `Sem.step` computes
  their denotations under the environment built from the concrete
  inputs and register store.

* `checkEquiv`/`checkEquiv_sound` — the checker: both programs check,
  no instances, identical device interfaces (inputs, outputs,
  registers including widths and initials), both definition
  environments denote, and the two symbolic steps agree after constant
  folding (`NF.cfold`, folding through `Sem.evalOp` itself). Soundness:
  a `true` verdict gives `p₁.run stim = p₂.run stim` for EVERY
  stimulus — including error agreement (the only reachable `run` error
  on a `checkEquiv`-approved pair is per-cycle stimulus arity, which
  depends only on the shared interface).

Everything here is proved against the committed semantics without
modifying it (the η tier extends the committed semantics itself — see
below); per house style (Rwv.Schema), small `Except`/list helpers are
re-proved locally rather than exported from committed files.

THE COMB-EXTERN η TIER: `NF` gains the uninterpreted-function node
`xcall w ext a` — a MODEL-LESS combinational extern call over the
packed (concatenated, MSB-first) compiled arguments (`NF.xpack`),
denoted through the committed semantics' own total reading
`Sem.xapply E ext w` at the extern environment `E : Sem.EEnv` that now
threads (defaulted empty) through `NF.eval`, `evalExp`, `mkFn`,
`FImplements`, `EnvCorr`, and every denotation lemma in this file. The
symbolic evaluator takes the program's extern table `X : Sem.XEnv` and
BUILDS the node exactly where `evalExp` would consult `E` (model-less,
generic-free), so the strong
soundness statements survive verbatim: the model-less extern reading is
total (the width clamp included), so `symExp` success still forces
`evalExp` success — both sides are literally `Sem.xapply` of the same
packed bits (`xpack_eval`). The equivalence checkers discharge xcall
nodes by structural equality — same extern name, same cached width,
equal packed argument after normalization — which is sound for ANY
interpretation (`checkEquiv_sound`/`checkEquivW_sound` now conclude run
equality at EVERY extern environment, both runs at the same one). The
width layer is E-unconditional: `xapply` clamps to the cached width, so
`annWidth (.xcall w _ _) = some w` holds at every valuation and every
environment, and the width-aware rewrites treat the node as an opaque
atom while recursing into its packed argument. `NF.xcallFree` is the
decidable gate under which a denotation cannot consult `E` at all
(`xcallFree_eval`) — what downstream compilation uses to keep
denotations pinned at the empty environment (spliced Cryptol
definitions) valid at every environment.

THE MODEL-CARRYING EXTERN ROW: a MODEL-CARRYING xcall (an X-hit)
reads in `evalExp` as a call to its model definition through `F` —
ignoring the generics and the declared width, exactly the committed
semantics — so `symExp` INLINES the model definition's body the way
it inlines a `.call` (fuel-decremented, params zipped), and
`symExp_sound`'s xcall case gains a sub-case that is its call case
verbatim keyed through `X`. The compiled forms this arm produces
contain no xcall node for the extern itself (the model body is
spliced), so the `xcallFree` discipline downstream is unchanged.
-/
import Rwv.Hyle.Syntax
import Rwv.Hyle.Semantics
import Rwv.Hyle.Check
import Std.Data.HashMap

namespace Rwv.Hyle.Bridge

open Std (HashMap)
open Rwv.Hyle

/-! ## The normal-form term language -/

/-- Normal forms over a device's free variables: no lets (sharing is
by tree duplication — the hash-consed DAG tier is Rwv.Hyle.BridgeDag),
no calls (inlined by the symbolic evaluator). A MODEL-LESS extern call
is an uninterpreted-function node `xcall` whose single child is the
CONCATENATION of the compiled arguments (`NF.xpack`), read through the
extern environment's total `Sem.xapply` — one child keeps the type
non-nested, so decidable equality and the induction principle stay
free, and the checker's discharge (same name, same cached width, equal
packed argument) is sound for ANY interpretation. The width on
`var`/`slice` is carried for interface sanity only and the denotation
does not consult it; the width on `xcall` IS consulted — `xapply`
clamps to it, which keeps every annotation-width fact unconditional in
the extern environment. -/
inductive NF where
  | var   (w : Nat) (x : String)
  | lit   (v : BV)
  | prim1 (op : Op) (a : NF)
  | prim2 (op : Op) (a b : NF)
  | cat   (a b : NF)
  | slice (i w : Nat) (e : NF)
  | ite   (c t e : NF)
  | xcall (w : Nat) (ext : String) (a : NF)
deriving DecidableEq, Repr

/-- The total denotation. Primitive applications take `Sem.evalOp`'s
value and default to `BV.nil` on its (arity) error cases — unreachable
for `symExp`-produced terms, which are arity-checked at construction.
The mux is mathematically eager, like the §6.2 denotation. The
trailing extern environment `E` (defaulted empty) is consulted only
by `xcall` nodes, through the same total `Sem.xapply` reading the
committed semantics gives a model-less extern call. -/
def NF.eval (σ : String → BV) (E : Sem.EEnv := Sem.eEmpty) : NF → BV
  | .var _ x => σ x
  | .lit v => v
  | .prim1 op a =>
      match Sem.evalOp op [a.eval σ E] with
      | .ok v => v
      | .error _ => BV.nil
  | .prim2 op a b =>
      match Sem.evalOp op [a.eval σ E, b.eval σ E] with
      | .ok v => v
      | .error _ => BV.nil
  | .cat a b => ⟨_, (a.eval σ E).bits ++ (b.eval σ E).bits⟩
  | .slice i w e => ⟨w, (e.eval σ E).bits.extractLsb' i w⟩
  | .ite c t e => if (c.eval σ E).nat ≠ 0 then t.eval σ E else e.eval σ E
  | .xcall w x a => Sem.xapply E x w (a.eval σ E)

/-- Pack a list of compiled arguments as one concatenation, mirroring
`Sem.bvcat`'s fold shape exactly (left fold from the empty literal). -/
def NF.xpack (ns : List NF) : NF := ns.foldl .cat (.lit BV.nil)

/-- The packed arguments denote the concatenation of the argument
denotations. -/
theorem NF.xpack_eval {σ : String → BV} {E : Sem.EEnv} (ns : List NF) :
    (NF.xpack ns).eval σ E = Sem.bvcat (ns.map (·.eval σ E)) := by
  suffices h : ∀ (acc : NF) (ns : List NF),
      (ns.foldl NF.cat acc).eval σ E
        = (ns.map (·.eval σ E)).foldl (fun a x => ⟨_, a.bits ++ x.bits⟩) (acc.eval σ E) from
    h (.lit BV.nil) ns
  intro acc ns
  induction ns generalizing acc with
  | nil => rfl
  | cons n ns ih =>
      rw [List.foldl_cons, List.map_cons, List.foldl_cons, ih]
      rfl

/-- The arity at which `Sem.evalOp` succeeds: 1 for the unary
operations (not, reductions, coercions, rep), 2 for everything else
(§5.2 — `evalOp` errors exactly on arity). -/
def opArity : Op → Nat
  | .not | .redand | .redor | .redxor
  | .zext _ | .sext _ | .trunc _ | .rep _ => 1
  | _ => 2

/-! ## The symbolic evaluator -/

/-- Symbolic evaluation, mirroring `evalExp` construct for construct
(var: environment lookup; let: bind the rhs normal form; call: inline
the callee's body through the definition map, arity-checked; xcall:
a MODEL-LESS generic-free extern call builds the uninterpreted node
over the packed arguments, a MODEL-CARRYING one inlines its model
definition like a call (`evalExp`'s model path ignores the generics
and the declared width) — the extern table `X` decides which,
exactly as it does in `evalExp`;
ite/cat/slice/prim: structural, prim arity-checked so the total
denotation's default is unreachable). Every recursion consumes fuel,
so the definition is structural in `fuel`. -/
def symExp (dmap : HashMap String Defn) (X : Sem.XEnv) :
    Nat → HashMap String NF → Exp → Except String NF
  | 0, _, _ => .error "symExp: out of fuel"
  | fuel + 1, ρ, e =>
    match e with
    | .lit v => .ok (.lit v)
    | .undef w => .ok (.lit (BV.zero w))   -- undef denotes zero (§5.1)
    | .var _ x =>
        match ρ.get? x with
        | some n => .ok n
        | none => .error s!"unbound variable {x}"
    | .cat e₁ e₂ => do
        .ok (.cat (← symExp dmap X fuel ρ e₁) (← symExp dmap X fuel ρ e₂))
    | .slice i w e => do
        .ok (.slice i w (← symExp dmap X fuel ρ e))
    | .prim _ op args => do
        let ns ← args.mapM (symExp dmap X fuel ρ)
        match ns with
        | [a] => if opArity op = 1 then .ok (.prim1 op a) else .error "prim: arity mismatch"
        | [a, b] => if opArity op = 2 then .ok (.prim2 op a b) else .error "prim: arity mismatch"
        | _ => .error "prim: arity mismatch"
    | .call _ f args => do
        let ns ← args.mapM (symExp dmap X fuel ρ)
        match dmap.get? f with
        | none => .error s!"unknown definition {f}"
        | some d =>
            if ns.length = d.params.length then
              symExp dmap X fuel (HashMap.ofList (d.params.zip ns)) d.body
            else .error s!"{f}: arity mismatch"
    | .xcall w ext gs args => do
        let ns ← args.mapM (symExp dmap X fuel ρ)
        match X.get? ext with
        | some model =>
            -- The model-carrying extern call reads exactly like a call
            -- to its model definition (`evalExp`'s model path ignores
            -- the generics and the declared width): inline it.
            (match dmap.get? model with
            | none => .error s!"extern {ext}: unknown model {model}"
            | some d =>
                if ns.length = d.params.length then
                  symExp dmap X fuel (HashMap.ofList (d.params.zip ns)) d.body
                else .error s!"extern {ext}: model arity mismatch")
        | none =>
            if gs.isEmpty then .ok (.xcall w ext (.xpack ns))
            else .error s!"extern {ext}: generic model-less externs are out of scope"
    | .ite _ c t e => do
        .ok (.ite (← symExp dmap X fuel ρ c) (← symExp dmap X fuel ρ t)
                  (← symExp dmap X fuel ρ e))
    | .letE _ x rhs body => do
        let n ← symExp dmap X fuel ρ rhs
        symExp dmap X fuel (ρ.insert x n) body

/-- The symbolic device step: outputs and register next-states in
declared order, as normal forms over the input and register names. -/
structure StepNF where
  outs  : List (String × NF)
  nexts : List (String × NF)
deriving DecidableEq, Repr

/-- The symbolic initial environment: every device input and register
maps to itself as a variable. -/
def initSymEnv (dev : Device) : HashMap String NF :=
  (dev.inputs ++ dev.registers.map fun r => (r.name, r.width)).foldl
    (fun ρ p => ρ.insert p.1 (NF.var p.2 p.1)) ∅

/-- One statement of the symbolic device step: mirrors `Sem.step`'s
fold body (lets extend the environment; outputs and nexts are recorded
once, with the same duplicate checks in the same order; instance
statements are outside the instance-free fragment). -/
def symBody (dmap : HashMap String Defn) (X : Sem.XEnv) (fuel : Nat) :
    HashMap String NF × HashMap String NF × HashMap String NF → Stmt →
    Except String (HashMap String NF × HashMap String NF × HashMap String NF) :=
  fun (ρ, outs, nexts) stmt => do
    match stmt with
    | .sLet x e => do
        let n ← symExp dmap X fuel ρ e
        pure (ρ.insert x n, outs, nexts)
    | .sOutput o e => do
        if outs.contains o then .error s!"output {o} assigned twice"
        let n ← symExp dmap X fuel ρ e
        pure (ρ, outs.insert o n, nexts)
    | .sNext r e => do
        if nexts.contains r then .error s!"register {r} assigned twice"
        let n ← symExp dmap X fuel ρ e
        pure (ρ, outs, nexts.insert r n)
    | .sInstIn inst _ _ =>
        .error s!"device instance {inst}: outside the instance-free fragment"

/-- Read off the outputs and register nexts in declared order. -/
def symFinish (dev : Device) :
    HashMap String NF × HashMap String NF × HashMap String NF → Except String StepNF :=
  fun (_, outs, nexts) => do
    let outsL ← dev.outputs.mapM fun (o, _) =>
      match outs.get? o with
      | some n => pure (o, n)
      | none => .error s!"output {o} never assigned"
    let nextsL ← dev.registers.mapM fun r =>
      match nexts.get? r.name with
      | some n => pure (r.name, n)
      | none => .error s!"register {r.name} never assigned"
    pure ⟨outsL, nextsL⟩

/-- The device step, symbolically. -/
def symStep (dmap : HashMap String Defn) (X : Sem.XEnv) (fuel : Nat) (dev : Device) :
    Except String StepNF :=
  dev.body.foldlM (symBody dmap X fuel) (initSymEnv dev, ∅, ∅) >>= symFinish dev

/-! ## The constant folder -/

namespace NF

/-- Fold an all-literal unary primitive through `Sem.evalOp` itself
(identical edge-case conventions by construction). -/
def mk1 (op : Op) : NF → NF
  | .lit v =>
      match Sem.evalOp op [v] with
      | .ok r => .lit r
      | .error _ => .prim1 op (.lit v)
  | a => .prim1 op a

/-- Fold an all-literal binary primitive through `Sem.evalOp`. -/
def mk2 (op : Op) : NF → NF → NF
  | .lit v, .lit w =>
      (match Sem.evalOp op [v, w] with
      | .ok r => .lit r
      | .error _ => .prim2 op (.lit v) (.lit w))
  | a, b => .prim2 op a b

/-- Fold a concatenation of literals. -/
def mkCat : NF → NF → NF
  | .lit v, .lit w => .lit ⟨_, v.bits ++ w.bits⟩
  | a, b => .cat a b

/-- Fold a slice of a literal. -/
def mkSlice (i w : Nat) : NF → NF
  | .lit v => .lit ⟨w, v.bits.extractLsb' i w⟩
  | e => .slice i w e

/-- Select on a literal mux condition. -/
def mkIte : NF → NF → NF → NF
  | .lit v, t, e => if v.nat ≠ 0 then t else e
  | c, t, e => .ite c t e

/-- Bottom-up constant folding: all-literal primitives fold through
`Sem.evalOp`, literal concatenations and slices fold to literals, and
literal mux conditions select. This absorbs the constant-folding leg
of `Hyle.Transform` (which folds via the interpreter's evaluator);
the `partialEval` rewrite set is `cfoldW`, in the width-aware
normalizer below. -/
def cfold : NF → NF
  | .var w x => .var w x
  | .lit v => .lit v
  | .prim1 op a => mk1 op a.cfold
  | .prim2 op a b => mk2 op a.cfold b.cfold
  | .cat a b => mkCat a.cfold b.cfold
  | .slice i w e => mkSlice i w e.cfold
  | .ite c t e => mkIte c.cfold t.cfold e.cfold
  | .xcall w x a => .xcall w x a.cfold

end NF

/-- Constant-fold both components of a labeled normal form. -/
def cfoldPairs (l : List (String × NF)) : List (String × NF) :=
  l.map fun p => (p.1, p.2.cfold)

/-- No uninterpreted extern nodes anywhere in the term: the decidable
gate under which a normal form's denotation cannot consult the extern
environment (`xcallFree_eval`) — what pins denotations computed at the
EMPTY environment (spliced Cryptol definitions) at every environment. -/
def NF.xcallFree : NF → Bool
  | .var _ _ | .lit _ => true
  | .prim1 _ a => a.xcallFree
  | .prim2 _ a b => a.xcallFree && b.xcallFree
  | .cat a b => a.xcallFree && b.xcallFree
  | .slice _ _ e => e.xcallFree
  | .ite c t e => c.xcallFree && t.xcallFree && e.xcallFree
  | .xcall _ _ _ => false

/-- An `xcall`-free normal form denotes identically at every extern
environment. -/
theorem NF.xcallFree_eval {σ : String → BV} {E E' : Sem.EEnv} :
    ∀ {n : NF}, n.xcallFree = true → n.eval σ E = n.eval σ E' := by
  intro n
  induction n with
  | var w x => intro _; rfl
  | lit v => intro _; rfl
  | prim1 op a iha =>
      intro h
      simp only [NF.eval, iha (by simpa [NF.xcallFree] using h)]
  | prim2 op a b iha ihb =>
      intro h
      simp only [NF.xcallFree, Bool.and_eq_true] at h
      simp only [NF.eval, iha h.1, ihb h.2]
  | cat a b iha ihb =>
      intro h
      simp only [NF.xcallFree, Bool.and_eq_true] at h
      simp only [NF.eval]
      rw [iha h.1, ihb h.2]
  | slice i w e ihe =>
      intro h
      simp only [NF.eval]
      rw [ihe (by simpa [NF.xcallFree] using h)]
  | ite c t e ihc iht ihe =>
      intro h
      simp only [NF.xcallFree, Bool.and_eq_true] at h
      simp only [NF.eval]
      rw [ihc h.1.1, iht h.1.2, ihe h.2]
  | xcall w x a iha =>
      intro h
      exact absurd h (by simp [NF.xcallFree])

/-! ## The checker -/

/-- The definition map of a program. -/
def dmapOf (p : Program) : HashMap String Defn :=
  HashMap.ofList (p.defns.map fun d => (d.name, d))

/-- Structural size of an expression (a computable stand-in for
`sizeOf`, whose derived instance does not compile on the nested
`Exp`). -/
def expSize : Exp → Nat
  | .lit _ | .undef _ | .var _ _ => 1
  | .cat e₁ e₂ => 1 + expSize e₁ + expSize e₂
  | .slice _ _ e => 1 + expSize e
  | .prim _ _ args => 1 + (args.attach.map fun ⟨a, _⟩ => expSize a).sum
  | .call _ _ args => 1 + (args.attach.map fun ⟨a, _⟩ => expSize a).sum
  | .xcall _ _ _ args => 1 + (args.attach.map fun ⟨a, _⟩ => expSize a).sum
  | .ite _ c t e => 1 + expSize c + expSize t + expSize e
  | .letE _ _ rhs body => 1 + expSize rhs + expSize body

/-- Size of a device statement's expression. -/
def stmtSize : Stmt → Nat
  | .sLet _ e | .sOutput _ e | .sNext _ e | .sInstIn _ _ e => 1 + expSize e

/-- A fuel bound sufficient for any acyclic program: symbolic
evaluation descends either structurally (bounded by the term's size)
or through a call edge (each acyclic chain visits a definition once,
bounded by the sum of body sizes). Exhausted fuel only ever makes the
checker answer `false`. -/
def progFuel (p : Program) : Nat :=
  2 * p.defns.foldl (fun n d => n + expSize d.body)
        (p.device.body.foldl (fun n s => n + stmtSize s) 8) + 8

/-- Boolean success of an `Except`. -/
def okB : Except String α → Bool
  | .ok _ => true
  | .error _ => false

/-- Boolean distinctness of a list of names. -/
def nodupB : List String → Bool
  | [] => true
  | x :: xs => !xs.contains x && nodupB xs

/-- A register as a comparable tuple (name, width, initial). -/
def regTuples (rs : List Register) : List (String × Nat × BV) :=
  rs.map fun r => (r.name, r.width, r.init)

/-- The verified equivalence checker. `true` requires: both programs
pass the §4 checker; no externs or device instances; distinct
definition names; both definition environments denote
(`Sem.mkFEnv` succeeds); identical device interfaces — inputs,
outputs, and registers with widths AND initials, in declared order —
and the two symbolic steps, constant-folded, are syntactically equal
normal forms, output for output and register for register. -/
def checkEquiv (p₁ p₂ : Program) : Bool :=
  match Sem.mkFEnv p₁, Sem.mkFEnv p₂,
        symStep (dmapOf p₁) (Sem.xenv p₁) (progFuel p₁) p₁.device,
        symStep (dmapOf p₂) (Sem.xenv p₂) (progFuel p₂) p₂.device with
  | .ok _, .ok _, .ok s₁, .ok s₂ =>
         okB p₁.check && okB p₂.check
      && p₁.externs.isEmpty && p₂.externs.isEmpty
      && p₁.device.instances.isEmpty && p₂.device.instances.isEmpty
      && nodupB (p₁.defns.map (·.name)) && nodupB (p₂.defns.map (·.name))
      && decide (p₁.device.inputs = p₂.device.inputs)
      && decide (p₁.device.outputs = p₂.device.outputs)
      && decide (regTuples p₁.device.registers = regTuples p₂.device.registers)
      && decide (cfoldPairs s₁.outs = cfoldPairs s₂.outs)
      && decide (cfoldPairs s₁.nexts = cfoldPairs s₂.nexts)
  | _, _, _, _ => false

/-! ## Local `Except` and list helpers

Re-proved here per house style: Correspond2's and Schema's identical
helpers are private to those files. -/

/-- `pure` on `Except` is `.ok`. -/
private theorem except_pure_def {α : Type} (a : α) :
    (pure a : Except String α) = .ok a := rfl

/-- `bind` on `Except` propagates errors. -/
private theorem except_bind_error {α β : Type} (e : String) (f : α → Except String β) :
    ((Except.error e : Except String α) >>= f) = .error e := rfl

/-- `bind` on `Except` applies the continuation to a success. -/
private theorem except_bind_ok {α β : Type} (a : α) (f : α → Except String β) :
    (Except.ok a >>= f) = f a := rfl

/-- Inversion for a successful `Except` bind. -/
private theorem except_bind_eq_ok {α β : Type} {x : Except String α}
    {f : α → Except String β} {b : β} (h : (x >>= f) = .ok b) :
    ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x with
  | error e => exact absurd h (by simp [except_bind_error])
  | ok a => exact ⟨a, rfl, h⟩

/-- Attachment is invisible to `mapM` of a function on the elements. -/
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

/-- Plain `mapM` congruence. -/
private theorem mapM_congr {α β : Type} {f g : α → Except String β} :
    ∀ {as : List α}, (∀ a ∈ as, f a = g a) → as.mapM f = as.mapM g := by
  intro as
  induction as with
  | nil => intro _; rfl
  | cons a as ih =>
      intro h
      rw [List.mapM_cons, List.mapM_cons, h a List.mem_cons_self,
          ih (fun a ha => h a (List.mem_cons_of_mem _ ha))]

/-- Transport a successful `mapM` along a pointwise success
implication that maps the results. -/
private theorem mapM_ok_map {α β γ : Type} {g : α → Except String β}
    {f : α → Except String γ} {h : β → γ} :
    ∀ {as : List α} {bs : List β}, as.mapM g = .ok bs →
      (∀ a ∈ as, ∀ b, g a = .ok b → f a = .ok (h b)) →
      as.mapM f = .ok (bs.map h) := by
  intro as
  induction as with
  | nil =>
      intro bs hg _
      rw [List.mapM_nil, except_pure_def] at hg
      injection hg with hg
      subst hg
      rfl
  | cons a as ih =>
      intro bs hg hpt
      rw [List.mapM_cons] at hg
      obtain ⟨b, hb, h₁⟩ := except_bind_eq_ok hg
      obtain ⟨bs', hbs, h₂⟩ := except_bind_eq_ok h₁
      have h₃ : (Except.ok (b :: bs') : Except String (List β)) = .ok bs := h₂
      injection h₃ with h₃
      subst h₃
      rw [List.mapM_cons, hpt a List.mem_cons_self b hb, except_bind_ok,
          ih hbs (fun a ha => hpt a (List.mem_cons_of_mem _ ha)), except_bind_ok,
          except_pure_def, List.map_cons]

/-- A successful `mapM` preserves length. -/
private theorem mapM_ok_length {α β : Type} {g : α → Except String β} :
    ∀ {xs : List α} {ys : List β}, xs.mapM g = .ok ys → ys.length = xs.length := by
  intro xs
  induction xs with
  | nil =>
      intro ys h
      rw [List.mapM_nil, except_pure_def] at h
      injection h with h
      subst h
      rfl
  | cons x xs ih =>
      intro ys h
      rw [List.mapM_cons] at h
      obtain ⟨y, _hy, h₁⟩ := except_bind_eq_ok h
      obtain ⟨ys', hys, h₂⟩ := except_bind_eq_ok h₁
      have h₃ : (Except.ok (y :: ys') : Except String (List β)) = .ok ys := h₂
      injection h₃ with h₃
      subst h₃
      simp [List.length_cons, ih hys]

/-- `foldlM` with a pure body is `foldl`. -/
private theorem foldlM_pure {α β : Type} (g : β → α → β) :
    ∀ (l : List α) (init : β),
      (l.foldlM (fun b a => pure (g b a)) init : Except String β) = .ok (l.foldl g init) := by
  intro l
  induction l with
  | nil => intro init; rfl
  | cons a l ih =>
      intro init
      rw [List.foldlM_cons, List.foldl_cons, except_pure_def, except_bind_ok]
      exact ih (g init a)

/-! ## `evalExp` dependency congruence

Expression evaluation consults the definition environment only at the
expression's `deps` (called definitions and consulted extern models),
so two environments that agree there evaluate identically. This is the
lemma that lets `mkFEnv`'s intermediate fold environments stand in for
the final one. -/

private theorem evalExp_congr {X : Sem.XEnv} {E : Sem.EEnv} {F₁ F₂ : Sem.FEnv} :
    ∀ (e : Exp) (ρ : HashMap String BV),
      (∀ g ∈ Sem.deps X e, F₁.get? g = F₂.get? g) →
      evalExp F₁ X ρ e E = evalExp F₂ X ρ e E
  | .lit _, _, _ => by simp only [evalExp]
  | .undef _, _, _ => by simp only [evalExp]
  | .var _ _, _, _ => by simp only [evalExp]
  | .cat e₁ e₂, ρ, h => by
      simp only [evalExp]
      rw [evalExp_congr e₁ ρ (fun g hg => h g (by
            simp only [Sem.deps, List.mem_append]; exact .inl hg)),
          evalExp_congr e₂ ρ (fun g hg => h g (by
            simp only [Sem.deps, List.mem_append]; exact .inr hg))]
  | .slice i w e, ρ, h => by
      simp only [evalExp]
      rw [evalExp_congr e ρ (fun g hg => h g (by simpa [Sem.deps] using hg))]
  | .prim w op args, ρ, h => by
      simp only [evalExp]
      rw [mapM_congr (as := args.attach) (fun a _ => evalExp_congr a.val ρ (fun g hg =>
        h g (by
          simp only [Sem.deps, List.mem_flatMap]
          exact ⟨⟨a.val, a.property⟩, List.mem_attach _ _, hg⟩)))]
  | .call w f args, ρ, h => by
      simp only [evalExp]
      rw [mapM_congr (as := args.attach) (fun a _ => evalExp_congr a.val ρ (fun g hg =>
        h g (by
          simp only [Sem.deps, List.mem_cons, List.mem_flatMap]
          exact .inr ⟨⟨a.val, a.property⟩, List.mem_attach _ _, hg⟩)))]
      apply bind_congr
      intro vs
      rw [h f (by simp [Sem.deps])]
  | .xcall w x cs args, ρ, h => by
      simp only [evalExp]
      rw [mapM_congr (as := args.attach) (fun a _ => evalExp_congr a.val ρ (fun g hg =>
        h g (by
          simp only [Sem.deps, List.mem_append, List.mem_flatMap]
          exact .inr ⟨⟨a.val, a.property⟩, List.mem_attach _ _, hg⟩)))]
      apply bind_congr
      intro vs
      cases hx : X.get? x with
      | none => rfl
      | some model =>
          dsimp only
          rw [h model (by
            simp only [Sem.deps, List.mem_append]
            exact .inl (by rw [hx]; exact List.mem_cons_self))]
  | .ite w c t e, ρ, h => by
      simp only [evalExp]
      rw [evalExp_congr c ρ (fun g hg => h g (by
            simp only [Sem.deps, List.mem_append]; exact .inl (.inl hg))),
          evalExp_congr t ρ (fun g hg => h g (by
            simp only [Sem.deps, List.mem_append]; exact .inl (.inr hg))),
          evalExp_congr e ρ (fun g hg => h g (by
            simp only [Sem.deps, List.mem_append]; exact .inr hg))]
  | .letE w x rhs body, ρ, h => by
      simp only [evalExp]
      rw [evalExp_congr rhs ρ (fun g hg => h g (by
            simp only [Sem.deps, List.mem_append]; exact .inl hg))]
      apply bind_congr
      intro v
      exact evalExp_congr body (ρ.insert x v) (fun g hg => h g (by
        simp only [Sem.deps, List.mem_append]; exact .inr hg))
  termination_by e => sizeOf e
  decreasing_by all_goals
    (first
      | (have := List.sizeOf_lt_of_mem a.property; simp +arith; omega)
      | (simp +arith; omega)
      | simp +arith)

/-! ## HashMap transport helpers -/

/-- A successful `ofList` lookup comes from a pair in the list. -/
private theorem ofList_get?_some {β : Type} {l : List (String × β)} {k : String} {b : β}
    (h : (HashMap.ofList l).get? k = some b) : (k, b) ∈ l := by
  rw [HashMap.get?_eq_getElem?, HashMap.ofList_eq_insertMany_empty,
      HashMap.getElem?_insertMany_list, HashMap.getElem?_empty, Option.or_none] at h
  rw [List.findSomeRev?_eq_findSome?_reverse] at h
  obtain ⟨⟨a, b'⟩, hmem, hab⟩ := List.exists_of_findSome?_eq_some h
  dsimp only at hab
  split at hab
  · rename_i heq
    injection hab with hab
    subst hab
    have : a = k := by simpa using heq
    subst this
    exact List.mem_reverse.mp hmem
  · exact absurd hab (by simp)

/-- `findSome?` commutes with an `Option.map`-composed selector. -/
private theorem findSome?_option_map {α β γ : Type} {g : α → Option β} {h : β → γ} :
    ∀ (l : List α), l.findSome? (fun a => (g a).map h) = (l.findSome? g).map h := by
  intro l
  induction l with
  | nil => rfl
  | cons a l ih =>
      rw [List.findSome?_cons, List.findSome?_cons]
      cases hg : g a with
      | none => simpa using ih
      | some b => simp

/-- Mapping the values of an association list maps its `ofList`
lookups (no key-distinctness needed: both sides resolve duplicates the
same way). -/
private theorem get?_ofList_map_snd {β γ : Type} (h : β → γ) (l : List (String × β))
    (k : String) :
    (HashMap.ofList (l.map fun p => (p.1, h p.2))).get? k
      = ((HashMap.ofList l).get? k).map h := by
  rw [HashMap.get?_eq_getElem?, HashMap.get?_eq_getElem?,
      HashMap.ofList_eq_insertMany_empty, HashMap.ofList_eq_insertMany_empty,
      HashMap.getElem?_insertMany_list, HashMap.getElem?_insertMany_list,
      HashMap.getElem?_empty, HashMap.getElem?_empty, Option.or_none, Option.or_none,
      List.findSomeRev?_eq_findSome?_reverse, List.findSomeRev?_eq_findSome?_reverse,
      ← List.map_reverse, List.findSome?_map]
  have : ((fun x : String × γ => if x.1 == k then some x.2 else none) ∘
            fun p : String × β => (p.1, h p.2))
        = fun p : String × β => ((if p.1 == k then some p.2 else none).map h) := by
    funext p
    by_cases hp : p.1 == k <;> simp [Function.comp, hp]
  rw [this, findSome?_option_map]

/-! ## Environment correspondence and the bridge theorem -/

/-- The closure `Sem.mkFEnv` builds for a definition (§6.2),
parameterized by the environment its body evaluates against. -/
def mkFn (X : Sem.XEnv) (F : Sem.FEnv) (d : Defn) (E : Sem.EEnv := Sem.eEmpty) :
    List BV → Except String BV :=
  fun vs =>
    if vs.length = d.params.length then
      evalExp F X (HashMap.ofList (d.params.zip vs)) d.body E
    else
      .error s!"{d.name}: arity mismatch (expected {d.params.length}, got {vs.length})"

/-- `F` implements `dmap`: every mapped definition is denoted in `F`
by the arity-guarded closure evaluating its body against `F` ITSELF
(the final environment — `mkFEnv`'s fold builds closures over prefix
environments, and `mkFEnv_implements` pays the debt of showing they
agree). -/
def FImplements (dmap : HashMap String Defn) (X : Sem.XEnv) (F : Sem.FEnv)
    (E : Sem.EEnv := Sem.eEmpty) : Prop :=
  ∀ f d, dmap.get? f = some d → ∃ fn, F.get? f = some fn ∧ ∀ vs, fn vs = mkFn X F d E vs

/-- The environment correspondence of `symExp_sound`: the symbolic
environment maps a name to a normal form exactly when the concrete
environment maps it to that normal form's denotation. (Only the
forward direction is needed: a concrete binding never consulted
symbolically is irrelevant.) -/
def EnvCorr (σ : String → BV) (ρ : HashMap String NF) (ρ' : HashMap String BV)
    (E : Sem.EEnv := Sem.eEmpty) : Prop :=
  ∀ x n, ρ.get? x = some n → ρ'.get? x = some (n.eval σ E)

private theorem envCorr_insert {σ : String → BV} {E : Sem.EEnv} {ρ : HashMap String NF}
    {ρ' : HashMap String BV} (h : EnvCorr σ ρ ρ' E) (x : String) (n : NF) :
    EnvCorr σ (ρ.insert x n) (ρ'.insert x (n.eval σ E)) E := by
  intro y m hy
  rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert] at hy ⊢
  split at hy
  · injection hy with hy; subst hy; simp_all
  · rename_i hne
    rw [if_neg hne]
    exact h y m hy

/-- The zip environments of a call site correspond: the concrete
closure environment is the symbolic one with every normal form
σ-evaluated. -/
private theorem envCorr_zip {σ : String → BV} {E : Sem.EEnv} (ps : List String) (ns : List NF) :
    EnvCorr σ (HashMap.ofList (ps.zip ns))
              (HashMap.ofList (ps.zip (ns.map (NF.eval σ E)))) E := by
  intro x n hx
  have hz : ps.zip (ns.map (NF.eval σ E)) = (ps.zip ns).map fun p => (p.1, p.2.eval σ E) := by
    rw [List.zip_map_right]
    rfl
  rw [hz, get?_ofList_map_snd, hx]
  rfl

/-- `Sem.evalOp` is total at arity 1 for the unary operations (§5.2:
its only error cases are arity mismatches). -/
private theorem evalOp_unary_ok (op : Op) (h : opArity op = 1) (x : BV) :
    ∃ v, Sem.evalOp op [x] = .ok v := by
  cases op <;> first
    | exact ⟨_, rfl⟩
    | simp [opArity] at h

/-- `Sem.evalOp` is total at arity 2 for the binary operations. -/
private theorem evalOp_binary_ok (op : Op) (h : opArity op = 2) (x y : BV) :
    ∃ v, Sem.evalOp op [x, y] = .ok v := by
  cases op <;> first
    | exact ⟨_, rfl⟩
    | simp [opArity] at h

/-- THE bridge theorem: a successful symbolic evaluation is
denotation-preserving. If `symExp` (inlining through `dmap`) produces
a normal form, then `evalExp` — against any definition environment
`F` implementing `dmap` and any concrete environment corresponding to
the symbolic one — produces exactly that normal form's denotation.
The `ite` case reconciles `evalExp`'s short-circuit with `NF.eval`'s
totality: the untaken branch's normal form still has a value, and
equality only needs the taken branch to agree. -/
theorem symExp_sound {dmap : HashMap String Defn} {X : Sem.XEnv} {F : Sem.FEnv}
    {E : Sem.EEnv} {σ : String → BV} (hImpl : FImplements dmap X F E) :
    ∀ (fuel : Nat) (e : Exp) (ρ : HashMap String NF) (ρ' : HashMap String BV) (nf : NF),
      EnvCorr σ ρ ρ' E →
      symExp dmap X fuel ρ e = .ok nf →
      evalExp F X ρ' e E = .ok (nf.eval σ E) := by
  intro fuel
  induction fuel with
  | zero =>
      intro e ρ ρ' nf _ hs
      exact absurd hs (by simp [symExp])
  | succ fuel ih =>
      intro e ρ ρ' nf hc hs
      cases e with
      | lit v =>
          simp only [symExp] at hs
          injection hs with hs
          subst hs
          simp only [evalExp, NF.eval]
      | undef w =>
          simp only [symExp] at hs
          injection hs with hs
          subst hs
          simp only [evalExp, NF.eval]
      | var w x =>
          simp only [symExp] at hs
          cases hx : ρ.get? x with
          | none => rw [hx] at hs; exact absurd hs (by simp)
          | some n =>
              rw [hx] at hs
              injection hs with hs
              subst hs
              simp only [evalExp, hc x n hx]
      | cat e₁ e₂ =>
          simp only [symExp] at hs
          obtain ⟨n₁, h₁, hs⟩ := except_bind_eq_ok hs
          obtain ⟨n₂, h₂, hs⟩ := except_bind_eq_ok hs
          have hs : (Except.ok (NF.cat n₁ n₂) : Except String NF) = .ok nf := hs
          injection hs with hs
          subst hs
          simp only [evalExp]
          rw [ih e₁ ρ ρ' n₁ hc h₁, except_bind_ok, ih e₂ ρ ρ' n₂ hc h₂, except_bind_ok]
          simp only [NF.eval]
      | slice i w e =>
          simp only [symExp] at hs
          obtain ⟨n, h₁, hs⟩ := except_bind_eq_ok hs
          have hs : (Except.ok (NF.slice i w n) : Except String NF) = .ok nf := hs
          injection hs with hs
          subst hs
          simp only [evalExp]
          rw [ih e ρ ρ' n hc h₁, except_bind_ok]
          simp only [NF.eval]
      | prim w op args =>
          simp only [symExp] at hs
          obtain ⟨ns, hns, hs⟩ := except_bind_eq_ok hs
          have hmapM : args.mapM (fun a => evalExp F X ρ' a E) = .ok (ns.map (NF.eval σ E)) :=
            mapM_ok_map hns (fun a _ n hn => ih a ρ ρ' n hc hn)
          simp only [evalExp]
          have hattach : (args.attach.mapM fun x => evalExp F X ρ' x.val E)
              = args.mapM (fun a => evalExp F X ρ' a E) :=
            mapM_attach_erase (fun a => evalExp F X ρ' a E) args
          rw [hattach, hmapM, except_bind_ok]
          match ns, hs with
          | [a], hs => ?one
          | [a, b], hs => ?two
          | [], hs => exact absurd hs (by simp)
          | _ :: _ :: _ :: _, hs => exact absurd hs (by simp)
          case one =>
            dsimp only at hs
            split at hs
            · rename_i hop
              injection hs with hs
              subst hs
              obtain ⟨v, hv⟩ := evalOp_unary_ok op hop (a.eval σ E)
              rw [List.map_cons, List.map_nil, hv]
              simp only [NF.eval, hv]
            · exact absurd hs (by simp)
          case two =>
            dsimp only at hs
            split at hs
            · rename_i hop
              injection hs with hs
              subst hs
              obtain ⟨v, hv⟩ := evalOp_binary_ok op hop (a.eval σ E) (b.eval σ E)
              rw [List.map_cons, List.map_cons, List.map_nil, hv]
              simp only [NF.eval, hv]
            · exact absurd hs (by simp)
      | call w f args =>
          simp only [symExp] at hs
          obtain ⟨ns, hns, hs⟩ := except_bind_eq_ok hs
          have hmapM : args.mapM (fun a => evalExp F X ρ' a E) = .ok (ns.map (NF.eval σ E)) :=
            mapM_ok_map hns (fun a _ n hn => ih a ρ ρ' n hc hn)
          simp only [evalExp]
          have hattach : (args.attach.mapM fun x => evalExp F X ρ' x.val E)
              = args.mapM (fun a => evalExp F X ρ' a E) :=
            mapM_attach_erase (fun a => evalExp F X ρ' a E) args
          rw [hattach, hmapM, except_bind_ok]
          cases hd : dmap.get? f with
          | none => rw [hd] at hs; exact absurd hs (by simp)
          | some d =>
              rw [hd] at hs
              dsimp only at hs
              split at hs
              · rename_i hlen
                obtain ⟨fn, hfn, hspec⟩ := hImpl f d hd
                simp only [hfn]
                rw [hspec (ns.map (NF.eval σ E))]
                simp only [mkFn]
                rw [if_pos (by simp [hlen])]
                exact ih d.body _ _ nf (envCorr_zip d.params ns) hs
              · exact absurd hs (by simp)
      | xcall w ext gs args =>
          -- The uninterpreted-function node: both sides read the SAME
          -- total `Sem.xapply` at the SAME packed bits (the argument
          -- IHs plus `xpack_eval`), so the strong direction survives —
          -- `evalExp`'s model-less extern path never errors.
          simp only [symExp] at hs
          obtain ⟨ns, hns, hs⟩ := except_bind_eq_ok hs
          have hmapM : args.mapM (fun a => evalExp F X ρ' a E) = .ok (ns.map (NF.eval σ E)) :=
            mapM_ok_map hns (fun a _ n hn => ih a ρ ρ' n hc hn)
          simp only [evalExp]
          have hattach : (args.attach.mapM fun x => evalExp F X ρ' x.val E)
              = args.mapM (fun a => evalExp F X ρ' a E) :=
            mapM_attach_erase (fun a => evalExp F X ρ' a E) args
          rw [hattach, hmapM, except_bind_ok]
          cases hx : X.get? ext with
          | some model =>
              rw [hx] at hs
              dsimp only at hs
              cases hd : dmap.get? model with
              | none => rw [hd] at hs; exact absurd hs (by simp)
              | some d =>
                  rw [hd] at hs
                  dsimp only at hs
                  split at hs
                  · rename_i hlen
                    obtain ⟨fn, hfn, hspec⟩ := hImpl model d hd
                    simp only [hfn]
                    rw [hspec (ns.map (NF.eval σ E))]
                    simp only [mkFn]
                    rw [if_pos (by simp [hlen])]
                    exact ih d.body _ _ nf (envCorr_zip d.params ns) hs
                  · exact absurd hs (by simp)
          | none =>
              rw [hx] at hs
              dsimp only at hs
              split at hs
              · rename_i hgs
                injection hs with hs
                subst hs
                rw [if_pos hgs]
                simp only [NF.eval, NF.xpack_eval]
              · exact absurd hs (by simp)
      | ite w c t e =>
          simp only [symExp] at hs
          obtain ⟨nc, h₁, hs⟩ := except_bind_eq_ok hs
          obtain ⟨nt, h₂, hs⟩ := except_bind_eq_ok hs
          obtain ⟨ne, h₃, hs⟩ := except_bind_eq_ok hs
          have hs : (Except.ok (NF.ite nc nt ne) : Except String NF) = .ok nf := hs
          injection hs with hs
          subst hs
          simp only [evalExp]
          rw [ih c ρ ρ' nc hc h₁, except_bind_ok]
          simp only [NF.eval]
          split
          · exact ih t ρ ρ' nt hc h₂
          · exact ih e ρ ρ' ne hc h₃
      | letE w x rhs body =>
          simp only [symExp] at hs
          obtain ⟨n, h₁, hs⟩ := except_bind_eq_ok hs
          simp only [evalExp]
          rw [ih rhs ρ ρ' n hc h₁, except_bind_ok]
          exact ih body _ _ nf (envCorr_insert hc x n) hs

/-! ## `topoDefns` ordering analysis

`mkFEnv` folds the topologically ordered definitions left to right,
so each closure captures the PREFIX environment. To relate those
closures to evaluation against the final environment (the shape
`FImplements` demands), we extract from `topoDefns` the property that
makes the prefix agree with the whole on each body's dependencies: no
definition's dependencies mention the names of itself or of any later
definition. -/

/-- No forward or self references: each definition's dependencies
avoid the names of itself and of every later definition. -/
def TopoList (X : Sem.XEnv) : List Defn → Prop
  | [] => True
  | d :: rest => (∀ g ∈ Sem.deps X d.body, g ∉ (d :: rest).map (·.name)) ∧ TopoList X rest

/-- `TopoList` as a statement about every split of the list. -/
private theorem topoList_iff_splits {X : Sem.XEnv} :
    ∀ {l : List Defn}, TopoList X l ↔
      ∀ pre d suf, l = pre ++ d :: suf →
        ∀ g ∈ Sem.deps X d.body, g ∉ (d :: suf).map (·.name) := by
  intro l
  induction l with
  | nil =>
      constructor
      · intro _ pre d suf heq
        exact absurd heq (by cases pre <;> simp)
      · intro _
        trivial
  | cons a l ih =>
      constructor
      · intro h pre d suf heq
        obtain ⟨ha, hl⟩ := h
        cases pre with
        | nil =>
            injection heq with h1 h2
            subst h1; subst h2
            exact ha
        | cons p pre' =>
            injection heq with h1 h2
            subst h1
            exact ih.mp hl pre' d suf h2
      · intro h
        refine ⟨h [] a l rfl, ih.mpr fun pre d suf heq => h (a :: pre) d suf (by rw [heq]; rfl)⟩

/-- The invariant `topoDefns.go` maintains on its (reversed)
accumulator: every accumulated definition's dependencies avoid its own
name, the names accumulated after it, and every remaining name. -/
private def AccInv (X : Sem.XEnv) (rem acc : List Defn) : Prop :=
  ∀ pre d suf, acc = pre ++ d :: suf →
    ∀ g ∈ Sem.deps X d.body, g ∉ (d :: pre).map (·.name) ∧ g ∉ rem.map (·.name)

/-- At the end of the fold (`rem = []`), the accumulator invariant is
exactly `TopoList` of the reversed accumulator. -/
private theorem topoList_of_accInv {X : Sem.XEnv} {acc : List Defn}
    (h : AccInv X [] acc) : TopoList X acc.reverse := by
  rw [topoList_iff_splits]
  intro pre d suf heq g hg
  have hacc : acc = suf.reverse ++ d :: pre.reverse := by
    have h' := congrArg List.reverse heq
    rw [List.reverse_reverse] at h'
    rw [h']
    simp
  have := (h suf.reverse d pre.reverse hacc g hg).1
  intro hmem
  apply this
  simp only [List.map_cons, List.mem_cons, List.mem_map] at hmem ⊢
  rcases hmem with h1 | ⟨e, he, hne⟩
  · exact .inl h1
  · exact .inr ⟨e, List.mem_reverse.mpr he, hne⟩

/-- Unpack the readiness predicate `topoDefns.go` filters by. -/
private theorem all_deps_spec {X : Sem.XEnv} {done : List String} {rem : List Defn}
    {d : Defn}
    (hP : ((Sem.deps X d.body).all fun f => done.contains f || rem.all (·.name ≠ f)) = true) :
    ∀ g ∈ Sem.deps X d.body, g ∈ done ∨ ∀ e ∈ rem, e.name ≠ g := by
  intro g hg
  have h := List.all_eq_true.mp hP g hg
  rcases Bool.or_eq_true_iff.mp h with h | h
  · exact .inl (by simpa using h)
  · right
    intro e he
    simpa using List.all_eq_true.mp h e he

/-- The master invariant lemma for `topoDefns.go`: a successful run
from a state satisfying the invariants yields a topologically ordered
output with distinct names containing everything processed and
remaining. -/
private theorem go_spec (X : Sem.XEnv) :
    ∀ (fuel : Nat) (acc rem out : List Defn),
      Sem.topoDefns.go X fuel acc rem = .ok out →
      ((acc ++ rem).map (·.name)).Nodup →
      AccInv X rem acc →
      TopoList X out ∧ (out.map (·.name)).Nodup ∧ ∀ d, d ∈ acc ∨ d ∈ rem → d ∈ out := by
  intro fuel
  induction fuel with
  | zero =>
      intro acc rem out h hnd hinv
      cases rem with
      | nil =>
          have hout : out = acc.reverse := by
            simp only [Sem.topoDefns.go] at h
            injection h with h
            exact h.symm
          subst hout
          simp only [List.append_nil] at hnd
          refine ⟨topoList_of_accInv hinv, ?_, ?_⟩
          · rw [List.map_reverse]
            exact (List.reverse_perm _).symm.nodup hnd
          · intro d hd
            rcases hd with hd | hd
            · exact List.mem_reverse.mpr hd
            · exact absurd hd (by simp)
      | cons r rs => exact absurd h (by simp [Sem.topoDefns.go])
  | succ fuel ih =>
      intro acc rem out h hnd hinv
      cases rem with
      | nil =>
          have hout : out = acc.reverse := by
            simp only [Sem.topoDefns.go] at h
            injection h with h
            exact h.symm
          subst hout
          simp only [List.append_nil] at hnd
          refine ⟨topoList_of_accInv hinv, ?_, ?_⟩
          · rw [List.map_reverse]
            exact (List.reverse_perm _).symm.nodup hnd
          · intro d hd
            rcases hd with hd | hd
            · exact List.mem_reverse.mpr hd
            · exact absurd hd (by simp)
      | cons r rs =>
          rw [show Sem.topoDefns.go X (fuel + 1) acc (r :: rs)
              = (match (r :: rs).partition fun d =>
                   (Sem.deps X d.body).all fun f =>
                     (acc.map (·.name)).contains f || (r :: rs).all (·.name ≠ f) with
                 | (ready, rest) =>
                   if ready.isEmpty then
                     .error s!"recursion among definitions: {String.intercalate ", " ((r :: rs).map (·.name))}"
                   else
                     Sem.topoDefns.go X fuel (ready.reverse ++ acc) rest) from rfl] at h
          rw [List.partition_eq_filter_filter] at h
          dsimp only at h
          split at h
          · exact absurd h (by simp)
          · rename_i hne
            -- names for the two filtered halves
            generalize hready : (r :: rs).filter (fun d =>
              (Sem.deps X d.body).all fun f =>
                (acc.map (·.name)).contains f || (r :: rs).all (·.name ≠ f)) = ready at *
            generalize hrest : (r :: rs).filter (not ∘ fun d =>
              (Sem.deps X d.body).all fun f =>
                (acc.map (·.name)).contains f || (r :: rs).all (·.name ≠ f)) = rest at *
            -- membership and readiness facts
            have hready_mem : ∀ e ∈ ready, e ∈ r :: rs ∧
                ((Sem.deps X e.body).all fun f =>
                  (acc.map (·.name)).contains f || (r :: rs).all (·.name ≠ f)) = true := by
              intro e he
              rw [← hready] at he
              exact List.mem_filter.mp he
            have hrest_mem : ∀ e ∈ rest, e ∈ r :: rs := by
              intro e he
              rw [← hrest] at he
              exact (List.mem_filter.mp he).1
            -- the permutation for name distinctness
            have hperm : List.Perm (acc ++ (r :: rs)) ((ready.reverse ++ acc) ++ rest) := by
              have h1 : List.Perm (ready ++ rest) (r :: rs) := by
                rw [← hready, ← hrest]
                exact List.filter_append_perm _ _
              have h2 : List.Perm (acc ++ (r :: rs)) ((ready ++ rest) ++ acc) :=
                (List.Perm.append_left acc h1.symm).trans List.perm_append_comm
              have h3 : List.Perm ((ready ++ rest) ++ acc) ((ready.reverse ++ acc) ++ rest) := by
                rw [List.append_assoc, List.append_assoc]
                exact List.Perm.append (List.reverse_perm ready).symm List.perm_append_comm
              exact h2.trans h3
            have hnd' : (((ready.reverse ++ acc) ++ rest).map (·.name)).Nodup :=
              (hperm.map (·.name)).nodup hnd
            -- name-level disjointness from the old Nodup
            have hdisj : ∀ g, g ∈ acc.map (·.name) → g ∉ (r :: rs).map (·.name) := by
              rw [List.map_append] at hnd
              have hsplit := List.nodup_append.mp hnd
              intro g hga hgr
              exact hsplit.2.2 g hga g hgr rfl
            -- the new accumulator invariant
            have hinv' : AccInv X rest (ready.reverse ++ acc) := by
              intro pre d suf heq g hg
              have hrestN : g ∉ rest.map (·.name) → True := fun _ => trivial
              have hgoal₂ : (∀ e ∈ r :: rs, e.name ≠ g) → g ∉ rest.map (·.name) := by
                intro hall hmem
                obtain ⟨e, he, hge⟩ := List.mem_map.mp hmem
                exact hall e (hrest_mem e he) hge
              have haccN₂ : g ∈ acc.map (·.name) → g ∉ rest.map (·.name) := by
                intro hga hmem
                obtain ⟨e, he, hge⟩ := List.mem_map.mp hmem
                exact hdisj g hga (List.mem_map.mpr ⟨e, hrest_mem e he, hge⟩)
              rcases List.append_eq_append_iff.mp heq with ⟨mid, hpre, hacc⟩ | ⟨mid, hrev, hmid⟩
              · -- d lies in the old accumulator
                obtain ⟨hg1, hg2⟩ := hinv mid d suf hacc g hg
                constructor
                · intro hmem
                  simp only [List.map_cons, List.mem_cons, List.mem_map] at hmem
                  rcases hmem with h1 | ⟨e, he, hge⟩
                  · exact hg1 (by simp [h1])
                  · rw [hpre] at he
                    rcases List.mem_append.mp he with he | he
                    · exact hg2 (List.mem_map.mpr ⟨e, by
                        have := List.mem_reverse.mp he
                        exact (hready_mem e this).1, hge⟩)
                    · exact hg1 (by
                        simp only [List.map_cons, List.mem_cons, List.mem_map]
                        exact .inr ⟨e, he, hge⟩)
                · intro hmem
                  obtain ⟨e, he, hge⟩ := List.mem_map.mp hmem
                  exact hg2 (List.mem_map.mpr ⟨e, hrest_mem e he, hge⟩)
              · cases mid with
                | nil =>
                    -- pre is the whole reversed ready batch; d heads the old accumulator
                    rw [List.append_nil] at hrev
                    have hacc : acc = d :: suf := by
                      simpa using hmid.symm
                    obtain ⟨hg1, hg2⟩ := hinv [] d suf hacc g hg
                    constructor
                    · intro hmem
                      simp only [List.map_cons, List.mem_cons, List.mem_map] at hmem
                      rcases hmem with h1 | ⟨e, he, hge⟩
                      · exact hg1 (by simp [h1])
                      · rw [← hrev] at he
                        exact hg2 (List.mem_map.mpr ⟨e,
                          (hready_mem e (List.mem_reverse.mp he)).1, hge⟩)
                    · intro hmem
                      obtain ⟨e, he, hge⟩ := List.mem_map.mp hmem
                      exact hg2 (List.mem_map.mpr ⟨e, hrest_mem e he, hge⟩)
                | cons d' mid' =>
                    -- d lies in the ready batch
                    have hd1 : d' = d := by
                      injection hmid with h1 _
                      exact h1.symm
                    rw [hd1] at hrev
                    have hdready : d ∈ ready := by
                      have : d ∈ ready.reverse := by rw [hrev]; simp
                      exact List.mem_reverse.mp this
                    obtain ⟨hdrem, hPd⟩ := hready_mem d hdready
                    have hspec := all_deps_spec hPd g hg
                    have hpre_names : ∀ e ∈ pre, e ∈ r :: rs := by
                      intro e he
                      have : e ∈ ready.reverse := by rw [hrev]; exact List.mem_append.mpr (.inl he)
                      exact (hready_mem e (List.mem_reverse.mp this)).1
                    rcases hspec with hga | hall
                    · constructor
                      · intro hmem
                        simp only [List.map_cons, List.mem_cons, List.mem_map] at hmem
                        rcases hmem with h1 | ⟨e, he, hge⟩
                        · exact hdisj g hga (List.mem_map.mpr ⟨d, hdrem, h1.symm⟩)
                        · exact hdisj g hga (List.mem_map.mpr ⟨e, hpre_names e he, hge⟩)
                      · exact haccN₂ hga
                    · constructor
                      · intro hmem
                        simp only [List.map_cons, List.mem_cons, List.mem_map] at hmem
                        rcases hmem with h1 | ⟨e, he, hge⟩
                        · exact hall d hdrem h1.symm
                        · exact hall e (hpre_names e he) hge
                      · exact hgoal₂ hall
            obtain ⟨htopo, hndo, hmem⟩ := ih (ready.reverse ++ acc) rest out h hnd' hinv'
            refine ⟨htopo, hndo, ?_⟩
            intro d hd
            rcases hd with hd | hd
            · exact hmem d (.inl (List.mem_append.mpr (.inr hd)))
            · by_cases hPd : ((Sem.deps X d.body).all fun f =>
                  (acc.map (·.name)).contains f || (r :: rs).all (·.name ≠ f)) = true
              · have : d ∈ ready := by
                  rw [← hready]
                  exact List.mem_filter.mpr ⟨hd, hPd⟩
                exact hmem d (.inl (List.mem_append.mpr (.inl (List.mem_reverse.mpr this))))
              · have : d ∈ rest := by
                  rw [← hrest]
                  exact List.mem_filter.mpr ⟨hd, by simpa [Function.comp] using hPd⟩
                exact hmem d (.inr this)

/-- `topoDefns`' output: topologically ordered, name-distinct, and
containing every definition. -/
private theorem topoDefns_spec {X : Sem.XEnv} {defns ordered : List Defn}
    (h : Sem.topoDefns X defns = .ok ordered)
    (hnd : (defns.map (·.name)).Nodup) :
    TopoList X ordered ∧ (ordered.map (·.name)).Nodup ∧ ∀ d ∈ defns, d ∈ ordered := by
  have h' : Sem.topoDefns.go X defns.length [] defns = Except.ok ordered := h
  obtain ⟨ht, hn, hm⟩ := go_spec X defns.length [] defns ordered h'
    (by simpa using hnd)
    (by intro pre d suf heq; exact absurd heq (by cases pre <;> simp))
  exact ⟨ht, hn, fun d hd => hm d (.inr hd)⟩

/-! ## The `mkFEnv` fold characterization -/

/-- One step of `mkFEnv`'s fold. -/
private def stepF (X : Sem.XEnv) (E : Sem.EEnv) (F : Sem.FEnv) (d : Defn) : Sem.FEnv :=
  F.insert d.name (mkFn X F d E)

/-- The fold only touches the names it inserts. -/
private theorem foldl_stepF_get?_not_mem (X : Sem.XEnv) (E : Sem.EEnv) :
    ∀ (l : List Defn) (F₀ : Sem.FEnv) (g : String), g ∉ l.map (·.name) →
      (l.foldl (stepF X E) F₀).get? g = F₀.get? g := by
  intro l
  induction l with
  | nil => intro _ _ _; rfl
  | cons d l ih =>
      intro F₀ g hg
      have hgd : g ≠ d.name ∧ g ∉ l.map (·.name) := by
        simp only [List.map_cons, List.mem_cons, not_or] at hg
        exact hg
      rw [List.foldl_cons, ih _ g hgd.2]
      simp only [stepF]
      rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert,
          if_neg (by simp only [beq_iff_eq]; exact fun h => hgd.1 h.symm)]
      rfl

/-- The fold characterization: over a topologically ordered,
name-distinct list, every definition is denoted by the arity-guarded
closure of its body AGAINST THE WHOLE FOLD — the prefix environment
the closure captured agrees with the final one on the body's
dependencies (`evalExp_congr` + `TopoList`). -/
private theorem fold_spec (X : Sem.XEnv) (E : Sem.EEnv) :
    ∀ (l : List Defn) (F₀ : Sem.FEnv), TopoList X l → (l.map (·.name)).Nodup →
      ∀ d ∈ l, ∃ fn, (l.foldl (stepF X E) F₀).get? d.name = some fn ∧
        ∀ vs, fn vs = mkFn X (l.foldl (stepF X E) F₀) d E vs := by
  intro l
  induction l with
  | nil => intro F₀ _ _ d hd; exact absurd hd (by simp)
  | cons a l ih =>
      intro F₀ htopo hnd d hd
      obtain ⟨hhead, htail⟩ := htopo
      have hnd' : (∀ x ∈ l, ¬x.name = a.name) ∧ (l.map (·.name)).Nodup := by
        simpa using hnd
      have hnotmem : a.name ∉ l.map (·.name) := by
        intro hmem
        obtain ⟨e, he, hge⟩ := List.mem_map.mp hmem
        exact hnd'.1 e he hge
      rw [List.foldl_cons]
      cases List.mem_cons.mp hd with
      | inl hda =>
          subst hda
          refine ⟨mkFn X F₀ d E, ?_, ?_⟩
          · rw [foldl_stepF_get?_not_mem X E l _ _ hnotmem]
            simp only [stepF]
            rw [HashMap.get?_eq_getElem?]
            exact HashMap.getElem?_insert_self
          · intro vs
            simp only [mkFn]
            split
            · apply evalExp_congr
              intro g hg
              have hgnot := hhead g hg
              simp only [List.map_cons, List.mem_cons, not_or] at hgnot
              rw [foldl_stepF_get?_not_mem X E l _ g hgnot.2]
              simp only [stepF]
              rw [HashMap.get?_eq_getElem?, HashMap.get?_eq_getElem?, HashMap.getElem?_insert,
                  if_neg (by simp only [beq_iff_eq]; exact fun h => hgnot.1 h.symm)]
            · rfl
      | inr hdl =>
          exact ih (stepF X E F₀ a) htail hnd'.2 d hdl

/-- Distinct-name test soundness. -/
private theorem nodupB_nodup : ∀ {l : List String}, nodupB l = true → l.Nodup := by
  intro l
  induction l with
  | nil => intro _; exact List.nodup_nil
  | cons x xs ih =>
      intro h
      simp only [nodupB, Bool.and_eq_true, Bool.not_eq_true'] at h
      rw [List.nodup_cons]
      exact ⟨by simpa using h.1, ih h.2⟩

/-- The main environment fact: a successful `mkFEnv` implements the
definition map (given distinct definition names). -/
theorem mkFEnv_implements {p : Program} {E : Sem.EEnv} {F : Sem.FEnv}
    (hnd : (p.defns.map (·.name)).Nodup)
    (hF : Sem.mkFEnv p E = .ok F) :
    FImplements (dmapOf p) (Sem.xenv p) F E := by
  simp only [Sem.mkFEnv] at hF
  obtain ⟨ordered, hord, hfold⟩ := except_bind_eq_ok hF
  have hfold' : ordered.foldlM (fun F d => pure (stepF (Sem.xenv p) E F d)) ∅
      = Except.ok F := hfold
  rw [foldlM_pure] at hfold'
  have hFeq : F = ordered.foldl (stepF (Sem.xenv p) E) ∅ := by
    injection hfold' with h
    exact h.symm
  obtain ⟨htopo, hndo, hmem⟩ := topoDefns_spec hord hnd
  intro f d hfd
  have hpair : (f, d) ∈ p.defns.map (fun d => (d.name, d)) := ofList_get?_some hfd
  obtain ⟨d', hd', hpd⟩ := List.mem_map.mp hpair
  have h1 : d'.name = f := congrArg Prod.fst hpd
  have h2 : d' = d := congrArg Prod.snd hpd
  subst h2
  subst h1
  obtain ⟨fn, hfn, hspec⟩ := fold_spec (Sem.xenv p) E ordered ∅ htopo hndo d' (hmem d' hd')
  rw [← hFeq] at hfn hspec
  exact ⟨fn, hfn, hspec⟩

/-! ## Constant-folder soundness -/

private theorem mk1_eval (σ : String → BV) (E : Sem.EEnv) (op : Op) (a : NF) :
    (NF.mk1 op a).eval σ E = (NF.prim1 op a).eval σ E := by
  cases a <;> try rfl
  case lit v =>
    simp only [NF.mk1]
    cases hv : Sem.evalOp op [v] with
    | ok r => simp [NF.eval, hv]
    | error e => simp [NF.eval, hv]

private theorem mk2_eval (σ : String → BV) (E : Sem.EEnv) (op : Op) (a b : NF) :
    (NF.mk2 op a b).eval σ E = (NF.prim2 op a b).eval σ E := by
  cases a <;> cases b <;> try rfl
  case lit.lit v w =>
    simp only [NF.mk2]
    cases hv : Sem.evalOp op [v, w] with
    | ok r => simp [NF.eval, hv]
    | error e => simp [NF.eval, hv]

private theorem mkCat_eval (σ : String → BV) (E : Sem.EEnv) (a b : NF) :
    (NF.mkCat a b).eval σ E = (NF.cat a b).eval σ E := by
  cases a <;> cases b <;> rfl

private theorem mkSlice_eval (σ : String → BV) (E : Sem.EEnv) (i w : Nat) (e : NF) :
    (NF.mkSlice i w e).eval σ E = (NF.slice i w e).eval σ E := by
  cases e <;> rfl

private theorem mkIte_eval (σ : String → BV) (E : Sem.EEnv) (c t e : NF) :
    (NF.mkIte c t e).eval σ E = (NF.ite c t e).eval σ E := by
  cases c <;> try rfl
  case lit v =>
    simp only [NF.mkIte]
    by_cases h : v.nat ≠ 0 <;> simp [NF.eval, h]

/-- Constant folding is denotation-preserving (each smart constructor
folds through `Sem.evalOp` itself, so this is by that same table). -/
theorem cfold_eval (σ : String → BV) (E : Sem.EEnv := Sem.eEmpty) :
    ∀ (nf : NF), (nf.cfold).eval σ E = nf.eval σ E := by
  intro nf
  induction nf with
  | var w x => rfl
  | lit v => rfl
  | prim1 op a iha =>
      simp only [NF.cfold]
      rw [mk1_eval]
      simp only [NF.eval, iha]
  | prim2 op a b iha ihb =>
      simp only [NF.cfold]
      rw [mk2_eval]
      simp only [NF.eval, iha, ihb]
  | cat a b iha ihb =>
      simp only [NF.cfold]
      rw [mkCat_eval]
      simp only [NF.eval]
      rw [iha, ihb]
  | slice i w e ihe =>
      simp only [NF.cfold]
      rw [mkSlice_eval]
      simp only [NF.eval]
      rw [ihe]
  | ite c t e ihc iht ihe =>
      simp only [NF.cfold]
      rw [mkIte_eval]
      simp only [NF.eval, ihc, iht, ihe]
  | xcall w x a iha =>
      simp only [NF.cfold, NF.eval, iha]

/-! ## Soundness of the symbolic step -/

/-- The environment `Sem.step` evaluates against (its `ρ₀`),
parameterized by the input port names so interface equality transports
it between devices. -/
def stepEnv (inputs : List (String × Nat)) (regs : HashMap String BV) (ins : List BV) :
    HashMap String BV :=
  HashMap.ofList ((inputs.map Prod.fst).zip ins) |>.union regs

/-- The valuation of the device's free variables induced by a concrete
step: exactly `stepEnv`'s lookup (defaulted — the default is never
consulted on names the symbolic step uses). -/
def sigmaOf (inputs : List (String × Nat)) (regs : HashMap String BV) (ins : List BV) :
    String → BV :=
  fun x => ((stepEnv inputs regs ins).get? x).getD BV.nil

/-- Everything `initSymEnv` binds is a variable bound to its own name,
drawn from the inputs or registers. -/
private theorem foldl_insert_var_get? {x : String} {n : NF} :
    ∀ (l : List (String × Nat)) (m : HashMap String NF),
      (l.foldl (fun ρ p => ρ.insert p.1 (NF.var p.2 p.1)) m).get? x = some n →
      (∃ w, n = NF.var w x ∧ (x, w) ∈ l) ∨ m.get? x = some n := by
  intro l
  induction l with
  | nil => intro m h; exact .inr h
  | cons p l ih =>
      intro m h
      rcases ih (m.insert p.1 (NF.var p.2 p.1)) h with h' | h'
      · obtain ⟨w, hn, hw⟩ := h'
        exact .inl ⟨w, hn, List.mem_cons_of_mem _ hw⟩
      · rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert] at h'
        split at h'
        · rename_i heq
          injection h' with h'
          have : p.1 = x := by simpa using heq
          exact .inl ⟨p.2, by rw [← h', this], by rw [← this]; exact List.mem_cons_self⟩
        · exact .inr h'

/-- The initial correspondence: the symbolic initial environment
(inputs and registers as variables) corresponds to `Sem.step`'s
concrete initial environment under `sigmaOf`. -/
private theorem initSymEnv_corr {dev : Device} {E : Sem.EEnv}
    {regs : HashMap String BV} {ins : List BV}
    (hlen : ins.length = dev.inputs.length)
    (hdom : ∀ r ∈ dev.registers, regs.contains r.name) :
    EnvCorr (sigmaOf dev.inputs regs ins) (initSymEnv dev) (stepEnv dev.inputs regs ins) E := by
  intro x n hx
  rcases foldl_insert_var_get? _ _ hx with h | h
  · obtain ⟨w, hn, hw⟩ := h
    subst hn
    have hc : (stepEnv dev.inputs regs ins).contains x = true := by
      rw [stepEnv, show ∀ (a b : HashMap String BV), a.union b = a ∪ b from fun _ _ => rfl,
          HashMap.contains_union]
      rcases List.mem_append.mp hw with hw | hw
      · apply Bool.or_eq_true_iff.mpr
        left
        rw [HashMap.contains_ofList]
        have hxin : x ∈ dev.inputs.map Prod.fst := List.mem_map.mpr ⟨(x, w), hw, rfl⟩
        have hfst : ((dev.inputs.map Prod.fst).zip ins).map Prod.fst = dev.inputs.map Prod.fst :=
          List.map_fst_zip (by simp [hlen])
        rw [hfst]
        simpa using hxin
      · apply Bool.or_eq_true_iff.mpr
        right
        obtain ⟨r, hr, hrx⟩ := List.mem_map.mp hw
        have : r.name = x := congrArg Prod.fst hrx
        rw [← this]
        exact hdom r hr
    rw [HashMap.contains_eq_isSome_getElem?, ← HashMap.get?_eq_getElem?] at hc
    obtain ⟨v, hv⟩ := Option.isSome_iff_exists.mp hc
    rw [hv]
    simp only [NF.eval, sigmaOf, hv, Option.getD_some]
  · rw [HashMap.get?_eq_getElem?, HashMap.getElem?_empty] at h
    exact absurd h (by simp)

/-- Correspondence of the recorded output/next maps: the concrete map
is the symbolic one, pointwise σ-evaluated. -/
private def MapCorr (σ : String → BV) (E : Sem.EEnv) (mS : HashMap String NF)
    (mC : HashMap String BV) : Prop :=
  ∀ k, mC.get? k = (mS.get? k).map (NF.eval σ E)

private theorem mapCorr_empty {σ : String → BV} {E : Sem.EEnv} : MapCorr σ E ∅ ∅ := by
  intro k
  rw [HashMap.get?_eq_getElem?, HashMap.get?_eq_getElem?, HashMap.getElem?_empty,
      HashMap.getElem?_empty]
  rfl

private theorem mapCorr_contains {σ : String → BV} {E : Sem.EEnv} {mS : HashMap String NF}
    {mC : HashMap String BV} (h : MapCorr σ E mS mC) (k : String) :
    mC.contains k = mS.contains k := by
  rw [HashMap.contains_eq_isSome_getElem?, HashMap.contains_eq_isSome_getElem?,
      ← HashMap.get?_eq_getElem?, ← HashMap.get?_eq_getElem?, h k]
  cases mS.get? k <;> rfl

private theorem mapCorr_insert {σ : String → BV} {E : Sem.EEnv} {mS : HashMap String NF}
    {mC : HashMap String BV} (h : MapCorr σ E mS mC) (x : String) (n : NF) :
    MapCorr σ E (mS.insert x n) (mC.insert x (n.eval σ E)) := by
  intro k
  rw [HashMap.get?_eq_getElem?, HashMap.get?_eq_getElem?, HashMap.getElem?_insert,
      HashMap.getElem?_insert]
  split
  · rfl
  · rw [← HashMap.get?_eq_getElem?, ← HashMap.get?_eq_getElem?]
    exact h k

/-- `Sem.step`'s fold body, named (definitionally equal to the lambda
inside the committed `Sem.step`; `step_unfold` checks this by `rfl`). -/
private def concBody (F : Sem.FEnv) (X : Sem.XEnv) (E : Sem.EEnv) :
    HashMap String BV × HashMap String BV × HashMap String BV → Stmt →
    Except String (HashMap String BV × HashMap String BV × HashMap String BV) :=
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

/-- `Sem.step`'s output/next read-off, named. -/
private def concFinish (dev : Device) :
    HashMap String BV × HashMap String BV × HashMap String BV →
    Except String (List BV × HashMap String BV) :=
  fun (_, outs, nexts) => do
    let outVals ← dev.outputs.mapM fun (o, _) =>
      match outs.get? o with
      | some v => pure v
      | none   => .error s!"output {o} never assigned"
    let regVals ← dev.registers.foldlM (init := (∅ : HashMap String BV)) fun m r =>
      match nexts.get? r.name with
      | some v => pure (m.insert r.name v)
      | none   => .error s!"register {r.name} never assigned"
    pure (outVals, regVals)

/-- The committed `Sem.step`, as the composition of the named pieces. -/
private theorem step_unfold (F : Sem.FEnv) (X : Sem.XEnv) (E : Sem.EEnv) (dev : Device)
    (regs : HashMap String BV) (ins : List BV) :
    Sem.step F X dev regs ins E =
      if ins.length ≠ dev.inputs.length then
        .error s!"stimulus arity: got {ins.length} inputs, device has {dev.inputs.length}"
      else
        dev.body.foldlM (concBody F X E) (stepEnv dev.inputs regs ins, ∅, ∅)
          >>= concFinish dev := by
  by_cases h : ins.length = dev.inputs.length
  · rw [if_neg (fun hne => hne h)]
    simp only [Sem.step, if_neg (fun hne : ins.length ≠ dev.inputs.length => hne h)]
    rfl
  · rw [if_pos h]
    simp only [Sem.step, if_pos (h : ins.length ≠ dev.inputs.length)]
    rfl

/-- The parallel fold: from corresponding states, a successful
symbolic body fold gives a successful concrete body fold with
corresponding results. -/
private theorem body_fold_sound {dmap : HashMap String Defn} {X : Sem.XEnv} {E : Sem.EEnv}
    {F : Sem.FEnv} {σ : String → BV} {fuel : Nat} (hImpl : FImplements dmap X F E) :
    ∀ (stmts : List Stmt) (ρS outsS nextsS : HashMap String NF)
      (ρC outsC nextsC : HashMap String BV)
      (resS : HashMap String NF × HashMap String NF × HashMap String NF),
      stmts.foldlM (symBody dmap X fuel) (ρS, outsS, nextsS) = .ok resS →
      EnvCorr σ ρS ρC E → MapCorr σ E outsS outsC → MapCorr σ E nextsS nextsC →
      ∃ ρC' outsC' nextsC',
        stmts.foldlM (concBody F X E) (ρC, outsC, nextsC) = .ok (ρC', outsC', nextsC') ∧
        MapCorr σ E resS.2.1 outsC' ∧ MapCorr σ E resS.2.2 nextsC' := by
  intro stmts
  induction stmts with
  | nil =>
      intro ρS outsS nextsS ρC outsC nextsC resS hsym hc ho hn
      rw [List.foldlM_nil, except_pure_def] at hsym
      injection hsym with hsym
      subst hsym
      exact ⟨ρC, outsC, nextsC, rfl, ho, hn⟩
  | cons stmt stmts ih =>
      intro ρS outsS nextsS ρC outsC nextsC resS hsym hc ho hn
      rw [List.foldlM_cons] at hsym ⊢
      obtain ⟨stS, hbody, hrest⟩ := except_bind_eq_ok hsym
      cases stmt with
      | sLet x e =>
          dsimp only [symBody] at hbody
          obtain ⟨n, hne, hpure⟩ := except_bind_eq_ok hbody
          have hpure : (Except.ok (ρS.insert x n, outsS, nextsS) :
              Except String (HashMap String NF × HashMap String NF × HashMap String NF))
              = .ok stS := hpure
          injection hpure with hpure
          subst hpure
          have hv : evalExp F X ρC e E = .ok (n.eval σ E) :=
            symExp_sound hImpl fuel e ρS ρC n hc hne
          obtain ⟨ρC', outsC', nextsC', hfoldC, ho', hn'⟩ :=
            ih _ _ _ (ρC.insert x (n.eval σ E)) outsC nextsC resS hrest
              (envCorr_insert hc x n) ho hn
          refine ⟨ρC', outsC', nextsC', ?_, ho', hn'⟩
          dsimp only [concBody]
          rw [hv, except_bind_ok, except_pure_def, except_bind_ok]
          exact hfoldC
      | sOutput o e =>
          dsimp only [symBody] at hbody
          rw [show outsS.contains o = outsS.contains o from rfl] at hbody
          cases hcont : outsS.contains o with
          | true =>
              rw [hcont] at hbody
              exact nomatch hbody
          | false =>
              rw [hcont] at hbody
              simp only [Bool.false_eq_true, if_false] at hbody
              obtain ⟨n, hne, hpure⟩ := except_bind_eq_ok hbody
              have hpure : (Except.ok (ρS, outsS.insert o n, nextsS) :
                  Except String (HashMap String NF × HashMap String NF × HashMap String NF))
                  = .ok stS := hpure
              injection hpure with hpure
              subst hpure
              have hv : evalExp F X ρC e E = .ok (n.eval σ E) :=
                symExp_sound hImpl fuel e ρS ρC n hc hne
              obtain ⟨ρC', outsC', nextsC', hfoldC, ho', hn'⟩ :=
                ih _ _ _ ρC (outsC.insert o (n.eval σ E)) nextsC resS hrest
                  hc (mapCorr_insert ho o n) hn
              refine ⟨ρC', outsC', nextsC', ?_, ho', hn'⟩
              dsimp only [concBody]
              rw [mapCorr_contains ho o, hcont]
              simp only [Bool.false_eq_true, if_false]
              rw [hv, except_bind_ok, except_pure_def, except_bind_ok]
              exact hfoldC
      | sNext r e =>
          dsimp only [symBody] at hbody
          cases hcont : nextsS.contains r with
          | true =>
              rw [hcont] at hbody
              exact nomatch hbody
          | false =>
              rw [hcont] at hbody
              simp only [Bool.false_eq_true, if_false] at hbody
              obtain ⟨n, hne, hpure⟩ := except_bind_eq_ok hbody
              have hpure : (Except.ok (ρS, outsS, nextsS.insert r n) :
                  Except String (HashMap String NF × HashMap String NF × HashMap String NF))
                  = .ok stS := hpure
              injection hpure with hpure
              subst hpure
              have hv : evalExp F X ρC e E = .ok (n.eval σ E) :=
                symExp_sound hImpl fuel e ρS ρC n hc hne
              obtain ⟨ρC', outsC', nextsC', hfoldC, ho', hn'⟩ :=
                ih _ _ _ ρC outsC (nextsC.insert r (n.eval σ E)) resS hrest
                  hc ho (mapCorr_insert hn r n)
              refine ⟨ρC', outsC', nextsC', ?_, ho', hn'⟩
              dsimp only [concBody]
              rw [mapCorr_contains hn r, hcont]
              simp only [Bool.false_eq_true, if_false]
              rw [hv, except_bind_ok, except_pure_def, except_bind_ok]
              exact hfoldC
      | sInstIn inst port e =>
          dsimp only [symBody] at hbody
          exact absurd hbody (by simp)

/-- Transport the register-next read-off: the concrete fold builds
exactly the store of σ-evaluated symbolic nexts. -/
private theorem regs_fold_sound {σ : String → BV} {E : Sem.EEnv} {nextsS : HashMap String NF}
    {nextsC : HashMap String BV} (h : MapCorr σ E nextsS nextsC) :
    ∀ (rs : List Register) (pairs : List (String × NF)) (m : HashMap String BV),
      rs.mapM (fun r =>
        match nextsS.get? r.name with
        | some n => pure (r.name, n)
        | none => Except.error s!"register {r.name} never assigned") = Except.ok pairs →
      rs.foldlM (init := m) (fun m r =>
        match nextsC.get? r.name with
        | some v => pure (m.insert r.name v)
        | none   => Except.error s!"register {r.name} never assigned")
        = Except.ok (pairs.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) m) := by
  intro rs
  induction rs with
  | nil =>
      intro pairs m hs
      rw [List.mapM_nil, except_pure_def] at hs
      injection hs with hs
      subst hs
      rfl
  | cons r rs ih =>
      intro pairs m hs
      rw [List.mapM_cons] at hs
      obtain ⟨q, hq, hs⟩ := except_bind_eq_ok hs
      obtain ⟨pairs', hps, hs⟩ := except_bind_eq_ok hs
      have hs : (Except.ok (q :: pairs') : Except String (List (String × NF))) = .ok pairs := hs
      injection hs with hs
      subst hs
      cases hget : nextsS.get? r.name with
      | none => rw [hget] at hq; exact absurd hq (by simp)
      | some n =>
          rw [hget] at hq
          have hq : (Except.ok (r.name, n) : Except String (String × NF)) = .ok q := hq
          injection hq with hq
          subst hq
          rw [List.foldlM_cons]
          have hcget : nextsC.get? r.name = some (n.eval σ E) := by rw [h r.name, hget]; rfl
          rw [hcget]
          dsimp only
          rw [except_pure_def, except_bind_ok, List.foldl_cons]
          exact ih pairs' _ hps

/-- The concrete values of a symbolic step at a valuation: outputs in
declared order, and the next register store. -/
def stepOutsVal (σ : String → BV) (ss : StepNF) (E : Sem.EEnv := Sem.eEmpty) : List BV :=
  ss.outs.map fun p => p.2.eval σ E

def stepNextsVal (σ : String → BV) (ss : StepNF) (E : Sem.EEnv := Sem.eEmpty) :
    HashMap String BV :=
  ss.nexts.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) ∅

/-- Soundness of the symbolic device step: whenever it succeeds,
`Sem.step` — from any register store covering the declared registers
and any stimulus of the right arity — computes exactly the symbolic
outputs' and nexts' denotations at the valuation induced by that store
and stimulus. -/
theorem symStep_sound {dmap : HashMap String Defn} {X : Sem.XEnv} {E : Sem.EEnv}
    {F : Sem.FEnv} {fuel : Nat} {dev : Device} {ss : StepNF}
    (hImpl : FImplements dmap X F E)
    (hsym : symStep dmap X fuel dev = .ok ss)
    (regs : HashMap String BV) (ins : List BV)
    (hlen : ins.length = dev.inputs.length)
    (hdom : ∀ r ∈ dev.registers, regs.contains r.name = true) :
    Sem.step F X dev regs ins E =
      .ok (stepOutsVal (sigmaOf dev.inputs regs ins) ss E,
           stepNextsVal (sigmaOf dev.inputs regs ins) ss E) := by
  rw [step_unfold, if_neg (fun hne => hne hlen)]
  rw [symStep] at hsym
  obtain ⟨resS, hfoldS, hfin⟩ := except_bind_eq_ok hsym
  obtain ⟨ρS', outsS, nextsS⟩ := resS
  dsimp only [symFinish] at hfin
  obtain ⟨outsL, hoL, hfin⟩ := except_bind_eq_ok hfin
  obtain ⟨nextsL, hnL, hfin⟩ := except_bind_eq_ok hfin
  have hss : (Except.ok (⟨outsL, nextsL⟩ : StepNF) : Except String StepNF) = .ok ss := hfin
  injection hss with hss
  subst hss
  obtain ⟨ρC', outsC', nextsC', hfoldC, hoCorr, hnCorr⟩ :=
    body_fold_sound hImpl dev.body (initSymEnv dev) ∅ ∅
      (stepEnv dev.inputs regs ins) ∅ ∅ _ hfoldS
      (initSymEnv_corr hlen hdom) mapCorr_empty mapCorr_empty
  rw [hfoldC, except_bind_ok]
  dsimp only [concFinish]
  have houtVals : dev.outputs.mapM (fun p =>
      match outsC'.get? p.1 with
      | some v => pure v
      | none   => Except.error s!"output {p.1} never assigned")
      = Except.ok (outsL.map fun q => q.2.eval (sigmaOf dev.inputs regs ins) E) := by
    refine mapM_ok_map hoL ?_
    intro a _ b hb
    obtain ⟨o, w⟩ := a
    dsimp only at hb ⊢
    cases hget : outsS.get? o with
    | none => rw [hget] at hb; exact absurd hb (by simp)
    | some n =>
        rw [hget] at hb
        have hb : (Except.ok (o, n) : Except String (String × NF)) = .ok b := hb
        injection hb with hb
        subst hb
        have : outsC'.get? o = some (n.eval (sigmaOf dev.inputs regs ins) E) := by
          rw [hoCorr o, hget]; rfl
        rw [this]
        rfl
  rw [houtVals, except_bind_ok]
  rw [regs_fold_sound hnCorr dev.registers nextsL ∅ hnL, except_bind_ok, except_pure_def]
  rfl

/-! ## Checker soundness -/

/-- Equal register tuples mean equal register lists. -/
private theorem registers_eq_of_regTuples : ∀ {rs₁ rs₂ : List Register},
    regTuples rs₁ = regTuples rs₂ → rs₁ = rs₂ := by
  intro rs₁
  induction rs₁ with
  | nil =>
      intro rs₂ h
      cases rs₂ with
      | nil => rfl
      | cons _ _ => exact absurd h (by simp [regTuples])
  | cons r rs ih =>
      intro rs₂ h
      cases rs₂ with
      | nil => exact absurd h (by simp [regTuples])
      | cons r' rs' =>
          simp only [regTuples, List.map_cons, List.cons.injEq] at h
          obtain ⟨h1, h2⟩ := h
          have hr : r = r' := by
            cases r; cases r'
            simpa [Register.mk.injEq, Prod.ext_iff] using h1
          rw [hr, ih (show regTuples rs = regTuples rs' from h2)]

/-- Equal constant-folded pairs give equal denotations, pointwise. -/
private theorem outsVal_eq_of_cfold {σ : String → BV} {E : Sem.EEnv} {l₁ l₂ : List (String × NF)}
    (h : cfoldPairs l₁ = cfoldPairs l₂) :
    (l₁.map fun p => p.2.eval σ E) = l₂.map fun p => p.2.eval σ E := by
  have key : ∀ (l : List (String × NF)),
      (l.map fun p => p.2.eval σ E) = (cfoldPairs l).map fun p => p.2.eval σ E := by
    intro l
    rw [cfoldPairs, List.map_map]
    have hfn : ((fun p : String × NF => p.2.eval σ E) ∘ fun p : String × NF => (p.1, p.2.cfold))
        = fun p : String × NF => p.2.eval σ E := by
      funext p
      simp [Function.comp, cfold_eval]
    rw [hfn]
  rw [key l₁, key l₂, h]

/-- Equal constant-folded pairs give equal next-state stores. -/
private theorem nextsVal_eq_of_cfold {σ : String → BV} {E : Sem.EEnv} {l₁ l₂ : List (String × NF)}
    (h : cfoldPairs l₁ = cfoldPairs l₂) :
    l₁.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) (∅ : HashMap String BV)
      = l₂.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) ∅ := by
  have key : ∀ (l : List (String × NF)),
      l.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) (∅ : HashMap String BV)
        = (cfoldPairs l).foldl (fun m p => m.insert p.1 (p.2.eval σ E)) ∅ := by
    intro l
    rw [cfoldPairs, List.foldl_map]
    have : (fun (m : HashMap String BV) (p : String × NF) =>
              m.insert (p.1, p.2.cfold).1 ((p.1, p.2.cfold).2.eval σ E))
         = fun (m : HashMap String BV) (p : String × NF) => m.insert p.1 (p.2.eval σ E) := by
      funext m p
      simp [cfold_eval]
    rw [this]
  rw [key l₁, key l₂, h]

/-- Contains-preservation through the next-store fold. -/
private theorem foldl_insert_contains_preserve {σ : String → BV} {E : Sem.EEnv} :
    ∀ (l : List (String × NF)) (m : HashMap String BV) (x : String),
      m.contains x = true →
      (l.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) m).contains x = true := by
  intro l
  induction l with
  | nil => intro m x h; exact h
  | cons p l ih =>
      intro m x h
      rw [List.foldl_cons]
      exact ih _ x (by rw [HashMap.contains_insert]; simp [h])

/-- Every key of the pair list is contained in the folded store. -/
private theorem foldl_insert_contains_of_mem {σ : String → BV} {E : Sem.EEnv} :
    ∀ (l : List (String × NF)) (m : HashMap String BV) (x : String),
      x ∈ l.map Prod.fst →
      (l.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) m).contains x = true := by
  intro l
  induction l with
  | nil => intro m x h; exact absurd h (by simp)
  | cons p l ih =>
      intro m x h
      rw [List.map_cons] at h
      rw [List.foldl_cons]
      rcases List.mem_cons.mp h with h | h
      · exact foldl_insert_contains_preserve l _ x
          (by rw [HashMap.contains_insert]; simp [h])
      · exact ih _ x h

/-- The keys of the symbolic nexts read-off are the register names. -/
private theorem mapM_regs_fst :
    ∀ (rs : List Register) (nexts : HashMap String NF) (pairs : List (String × NF)),
      rs.mapM (fun r =>
        match nexts.get? r.name with
        | some n => pure (r.name, n)
        | none => Except.error s!"register {r.name} never assigned") = Except.ok pairs →
      pairs.map Prod.fst = rs.map (·.name) := by
  intro rs
  induction rs with
  | nil =>
      intro nexts pairs h
      rw [List.mapM_nil, except_pure_def] at h
      injection h with h
      subst h
      rfl
  | cons r rs ih =>
      intro nexts pairs h
      rw [List.mapM_cons] at h
      obtain ⟨q, hq, h⟩ := except_bind_eq_ok h
      obtain ⟨pairs', hps, h⟩ := except_bind_eq_ok h
      have h : (Except.ok (q :: pairs') : Except String (List (String × NF))) = .ok pairs := h
      injection h with h
      subst h
      cases hget : nexts.get? r.name with
      | none => rw [hget] at hq; exact absurd hq (by simp)
      | some n =>
          rw [hget] at hq
          have hq : (Except.ok (r.name, n) : Except String (String × NF)) = .ok q := hq
          injection hq with hq
          subst hq
          rw [List.map_cons, List.map_cons, ih nexts pairs' hps]

private theorem symStep_nexts_fst {dmap : HashMap String Defn} {X : Sem.XEnv} {fuel : Nat}
    {dev : Device} {ss : StepNF} (hsym : symStep dmap X fuel dev = .ok ss) :
    ss.nexts.map Prod.fst = dev.registers.map (·.name) := by
  rw [symStep] at hsym
  obtain ⟨resS, _, hfin⟩ := except_bind_eq_ok hsym
  obtain ⟨ρS', outsS, nextsS⟩ := resS
  dsimp only [symFinish] at hfin
  obtain ⟨outsL, _, hfin⟩ := except_bind_eq_ok hfin
  obtain ⟨nextsL, hnL, hfin⟩ := except_bind_eq_ok hfin
  have h : (Except.ok (⟨outsL, nextsL⟩ : StepNF) : Except String StepNF) = .ok ss := hfin
  injection h with h
  subst h
  exact mapM_regs_fst dev.registers nextsS nextsL hnL

/-- The next-state store covers the declared registers. -/
private theorem stepNextsVal_regdom {σ : String → BV} {E : Sem.EEnv} {ss : StepNF} {dev : Device}
    (hfst : ss.nexts.map Prod.fst = dev.registers.map (·.name)) :
    ∀ r ∈ dev.registers, (stepNextsVal σ ss E).contains r.name = true := by
  intro r hr
  exact foldl_insert_contains_of_mem _ _ _
    (by rw [hfst]; exact List.mem_map.mpr ⟨r, hr, rfl⟩)

/-- Two devices with pointwise-equal steps (on stores satisfying an
invariant the step preserves) have equal run folds from any invariant
store. -/
private theorem run_fold_congr {F₁ F₂ : Sem.FEnv} {X₁ X₂ : Sem.XEnv} {E₁ E₂ : Sem.EEnv}
    {dev₁ dev₂ : Device}
    {Inv : HashMap String BV → Prop} {Ok : List BV → Prop}
    (hstep : ∀ regs ins, Inv regs → Ok ins →
      Sem.step F₁ X₁ dev₁ regs ins E₁ = Sem.step F₂ X₂ dev₂ regs ins E₂)
    (hpres : ∀ regs ins outs regs', Inv regs → Ok ins →
      Sem.step F₁ X₁ dev₁ regs ins E₁ = .ok (outs, regs') → Inv regs') :
    ∀ (stim : List (List BV)) (regs : HashMap String BV) (acc : List (List BV)),
      Inv regs → (∀ ins ∈ stim, Ok ins) →
      stim.foldlM (Sem.foldStep F₁ X₁ dev₁ E₁) (regs, acc)
        = stim.foldlM (Sem.foldStep F₂ X₂ dev₂ E₂) (regs, acc) := by
  intro stim
  induction stim with
  | nil => intro regs acc _ _; rfl
  | cons ins stim ih =>
      intro regs acc hdom hok
      have hoki : Ok ins := hok ins List.mem_cons_self
      have hokr : ∀ i ∈ stim, Ok i := fun i hi => hok i (List.mem_cons_of_mem _ hi)
      rw [List.foldlM_cons, List.foldlM_cons]
      cases hstep₁ : Sem.step F₁ X₁ dev₁ regs ins E₁ with
      | error e =>
          have h₂ : Sem.step F₂ X₂ dev₂ regs ins E₂ = .error e :=
            (hstep regs ins hdom hoki).symm.trans hstep₁
          simp only [Sem.foldStep, hstep₁, h₂, except_bind_error]
      | ok pr =>
          obtain ⟨outs, regs'⟩ := pr
          have h₂ : Sem.step F₂ X₂ dev₂ regs ins E₂ = .ok (outs, regs') :=
            (hstep regs ins hdom hoki).symm.trans hstep₁
          simp only [Sem.foldStep, hstep₁, h₂, except_bind_ok, except_pure_def]
          exact ih regs' (outs :: acc) (hpres regs ins outs regs' hdom hoki hstep₁) hokr

/-- A denoting definition environment denotes at EVERY extern
environment: `mkFEnv`'s only failure is `topoDefns`, which never
consults it. -/
private theorem mkFEnv_ok_any {p : Program} {F : Sem.FEnv} (E : Sem.EEnv)
    (hF : Sem.mkFEnv p = .ok F) : ∃ F', Sem.mkFEnv p E = .ok F' := by
  simp only [Sem.mkFEnv] at hF ⊢
  obtain ⟨ordered, hord, _⟩ := except_bind_eq_ok hF
  rw [hord, except_bind_ok]
  exact ⟨ordered.foldl (stepF (Sem.xenv p) E) ∅,
    foldlM_pure (stepF (Sem.xenv p) E) ordered ∅⟩

/-- The verified checker is sound: a `true` verdict gives run equality
on EVERY stimulus AND at EVERY extern environment (both runs at the
same one — the committed `Program.run`, doc/hyle.md §6.4, including
error agreement, whose only reachable case on an approved pair is
per-cycle stimulus arity, determined by the shared interface; the
model-less extern reading is total, so it adds no error cases). -/
theorem checkEquiv_sound {p₁ p₂ : Program} (h : checkEquiv p₁ p₂ = true) :
    ∀ stim (E : Sem.EEnv), p₁.run stim E = p₂.run stim E := by
  intro stim E
  rw [checkEquiv] at h
  cases hF₁ : Sem.mkFEnv p₁ with
  | error e => rw [hF₁] at h; exact absurd h (by simp)
  | ok F₁ =>
  cases hF₂ : Sem.mkFEnv p₂ with
  | error e => rw [hF₁, hF₂] at h; exact absurd h (by simp)
  | ok F₂ =>
  cases hs₁ : symStep (dmapOf p₁) (Sem.xenv p₁) (progFuel p₁) p₁.device with
  | error e => rw [hF₁, hF₂, hs₁] at h; exact absurd h (by simp)
  | ok s₁ =>
  cases hs₂ : symStep (dmapOf p₂) (Sem.xenv p₂) (progFuel p₂) p₂.device with
  | error e => rw [hF₁, hF₂, hs₁, hs₂] at h; exact absurd h (by simp)
  | ok s₂ =>
  rw [hF₁, hF₂, hs₁, hs₂] at h
  dsimp only at h
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  obtain ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨_, _⟩, _⟩, _⟩, _⟩, _⟩, hnd₁⟩, hnd₂⟩, hIn⟩, hOut⟩, hReg⟩, hOuts⟩, hNexts⟩ := h
  have hRegs : p₁.device.registers = p₂.device.registers := registers_eq_of_regTuples hReg
  obtain ⟨F₁E, hF₁E⟩ := mkFEnv_ok_any E hF₁
  obtain ⟨F₂E, hF₂E⟩ := mkFEnv_ok_any E hF₂
  have hImpl₁ : FImplements (dmapOf p₁) (Sem.xenv p₁) F₁E E :=
    mkFEnv_implements (nodupB_nodup hnd₁) hF₁E
  have hImpl₂ : FImplements (dmapOf p₂) (Sem.xenv p₂) F₂E E :=
    mkFEnv_implements (nodupB_nodup hnd₂) hF₂E
  -- pointwise step equality on register-covering stores
  have hstep : ∀ regs ins, (∀ r ∈ p₁.device.registers, regs.contains r.name = true) →
      Sem.step F₁E (Sem.xenv p₁) p₁.device regs ins E
        = Sem.step F₂E (Sem.xenv p₂) p₂.device regs ins E := by
    intro regs ins hdom
    by_cases hlen : ins.length = p₁.device.inputs.length
    · rw [symStep_sound hImpl₁ hs₁ regs ins hlen hdom,
          symStep_sound hImpl₂ hs₂ regs ins (by rw [← hIn]; exact hlen)
            (by rw [← hRegs]; exact hdom)]
      have hσ : sigmaOf p₂.device.inputs regs ins = sigmaOf p₁.device.inputs regs ins := by
        rw [hIn]
      rw [hσ]
      have ho : stepOutsVal (sigmaOf p₁.device.inputs regs ins) s₁ E
          = stepOutsVal (sigmaOf p₁.device.inputs regs ins) s₂ E := outsVal_eq_of_cfold hOuts
      have hn : stepNextsVal (sigmaOf p₁.device.inputs regs ins) s₁ E
          = stepNextsVal (sigmaOf p₁.device.inputs regs ins) s₂ E := nextsVal_eq_of_cfold hNexts
      rw [ho, hn]
    · rw [step_unfold, step_unfold, if_pos hlen, if_pos (by rw [← hIn]; exact hlen)]
      rw [hIn]
  -- domain preservation
  have hpres : ∀ regs ins outs regs',
      (∀ r ∈ p₁.device.registers, regs.contains r.name = true) →
      Sem.step F₁E (Sem.xenv p₁) p₁.device regs ins E = .ok (outs, regs') →
      ∀ r ∈ p₁.device.registers, regs'.contains r.name = true := by
    intro regs ins outs regs' hdom hstep₁
    by_cases hlen : ins.length = p₁.device.inputs.length
    · rw [symStep_sound hImpl₁ hs₁ regs ins hlen hdom] at hstep₁
      injection hstep₁ with hstep₁
      have hregs' : regs' = stepNextsVal (sigmaOf p₁.device.inputs regs ins) s₁ E :=
        (congrArg Prod.snd hstep₁).symm
      rw [hregs']
      exact stepNextsVal_regdom (symStep_nexts_fst hs₁)
    · rw [step_unfold, if_pos hlen] at hstep₁
      exact absurd hstep₁ (by simp)
  -- initial stores agree and cover the registers
  have hInit : Sem.initRegs p₁.device = Sem.initRegs p₂.device := by
    rw [Sem.initRegs, Sem.initRegs, hRegs]
  have hInitDom : ∀ r ∈ p₁.device.registers,
      (Sem.initRegs p₁.device).contains r.name = true := by
    intro r hr
    rw [Sem.initRegs, HashMap.contains_ofList]
    have : r.name ∈ (p₁.device.registers.map fun r => (r.name, r.init)).map Prod.fst := by
      simp only [List.map_map]
      exact List.mem_map.mpr ⟨r, hr, rfl⟩
    simpa using this
  -- assemble the two runs
  have hfold : stim.foldlM (Sem.foldStep F₁E (Sem.xenv p₁) p₁.device E)
        (Sem.initRegs p₁.device, [])
      = stim.foldlM (Sem.foldStep F₂E (Sem.xenv p₂) p₂.device E)
        (Sem.initRegs p₂.device, []) := by
    rw [← hInit]
    exact run_fold_congr (Ok := fun _ => True)
      (fun regs ins hInv _ => hstep regs ins hInv)
      (fun regs ins outs regs' hInv _ h => hpres regs ins outs regs' hInv h)
      stim (Sem.initRegs p₁.device) [] hInitDom (fun _ _ => trivial)
  show Program.run p₁ stim E = Program.run p₂ stim E
  rw [Program.run, Program.run, hF₁E, hF₂E, except_bind_ok, except_bind_ok,
      Sem.run, Sem.run, hfold]

/-! ## The width layer

The `Transform.partialEval` rewrites that relocate slice boundaries
(slice-of-concatenation, identity slices, the width-1 peepholes) are
denotation-preserving only when the free variables actually carry
their annotated widths. `VarsWF` is that discipline as a predicate on
normal forms; `annWidth` computes a width from the annotations; and
the width-aware checker's soundness statement is conditioned on
declared-width stimuli — the honest side condition, since the
unconditioned equality is false for width-relocating rewrites. -/

namespace NF

/-- Every variable occurrence satisfies `P` on its annotation. -/
def VarsWF (P : String → Nat → Prop) : NF → Prop
  | .var w x => P x w
  | .lit _ => True
  | .prim1 _ a => a.VarsWF P
  | .prim2 _ a b => a.VarsWF P ∧ b.VarsWF P
  | .cat a b => a.VarsWF P ∧ b.VarsWF P
  | .slice _ _ e => e.VarsWF P
  | .ite c t e => c.VarsWF P ∧ t.VarsWF P ∧ e.VarsWF P
  | .xcall _ _ a => a.VarsWF P

theorem VarsWF.mono {P Q : String → Nat → Prop} (h : ∀ x w, P x w → Q x w) :
    ∀ {nf : NF}, nf.VarsWF P → nf.VarsWF Q := by
  intro nf
  induction nf with
  | var w x => exact h x w
  | lit v => intro _; trivial
  | prim1 op a iha => exact iha
  | prim2 op a b iha ihb => exact fun hp => ⟨iha hp.1, ihb hp.2⟩
  | cat a b iha ihb => exact fun hp => ⟨iha hp.1, ihb hp.2⟩
  | slice i w e ihe => exact ihe
  | ite c t e ihc iht ihe => exact fun hp => ⟨ihc hp.1, iht hp.2.1, ihe hp.2.2⟩
  | xcall w x a iha => exact iha

end NF

/-- The width `sigma`-respects the annotations of `nf`'s variables. -/
abbrev WP (σ : String → BV) : String → Nat → Prop := fun x w => (σ x).width = w

/-- Result width of a unary operation from its operand width (§5.2). -/
def opWidth1 : Op → Nat → Nat
  | .redand, _ | .redor, _ | .redxor, _ => 1
  | .zext m, _ | .sext m, _ | .trunc m, _ => m
  | .rep k, w => w * k
  | _, w => w

/-- Result width of a binary operation from its LEFT operand width
(the §5.2 denotations reconcile the right operand to the left's). -/
def opWidth2 : Op → Nat → Nat
  | .eq, _ | .ne, _ | .ult, _ | .ule, _ | .ugt, _ | .uge, _
  | .slt, _ | .sle, _ | .sgt, _ | .sge, _ => 1
  | _, w => w

/-- The annotation-trusted width of a normal form. -/
def annWidth : NF → Option Nat
  | .var w _ => some w
  | .lit v => some v.width
  | .prim1 op a => if opArity op = 1 then (annWidth a).map (opWidth1 op) else none
  | .prim2 op a _ => if opArity op = 2 then (annWidth a).map (opWidth2 op) else none
  | .cat a b =>
      match annWidth a, annWidth b with
      | some wa, some wb => some (wa + wb)
      | _, _ => none
  | .slice _ w _ => some w
  | .ite _ t e =>
      match annWidth t, annWidth e with
      | some wt, some we => if wt = we then some wt else none
      | _, _ => none
  | .xcall w _ _ => some w

private theorem evalOp_width1 {op : Op} (hop : opArity op = 1) {x v : BV}
    (hv : Sem.evalOp op [x] = .ok v) : v.width = opWidth1 op x.width := by
  cases op <;> first
    | (injection hv with hv; subst hv; rfl)
    | injection hv

private theorem evalOp_width2 {op : Op} (hop : opArity op = 2) {x y v : BV}
    (hv : Sem.evalOp op [x, y] = .ok v) : v.width = opWidth2 op x.width := by
  cases op <;> first
    | (injection hv with hv; subst hv; rfl)
    | injection hv

/-- At a width-respecting valuation, the annotation-trusted width is
the denotation's width. -/
theorem annWidth_eval {σ : String → BV} {E : Sem.EEnv} :
    ∀ {nf : NF} {w : Nat}, nf.VarsWF (WP σ) → annWidth nf = some w →
      (nf.eval σ E).width = w := by
  intro nf
  induction nf with
  | var w' x =>
      intro w hwf ha
      simp only [annWidth, Option.some.injEq] at ha
      subst ha
      exact hwf
  | lit v =>
      intro w _ ha
      simp only [annWidth, Option.some.injEq] at ha
      subst ha
      rfl
  | prim1 op a iha =>
      intro w hwf ha
      simp only [annWidth] at ha
      split at ha
      · rename_i hop
        cases haw : annWidth a with
        | none => rw [haw] at ha; exact absurd ha (by simp)
        | some wa =>
            rw [haw] at ha
            simp only [Option.map_some, Option.some.injEq] at ha
            subst ha
            obtain ⟨v, hv⟩ := evalOp_unary_ok op hop (a.eval σ E)
            simp only [NF.eval, hv]
            rw [evalOp_width1 hop hv, iha hwf haw]
      · exact absurd ha (by simp)
  | prim2 op a b iha ihb =>
      intro w hwf ha
      simp only [annWidth] at ha
      split at ha
      · rename_i hop
        cases haw : annWidth a with
        | none => rw [haw] at ha; exact absurd ha (by simp)
        | some wa =>
            rw [haw] at ha
            simp only [Option.map_some, Option.some.injEq] at ha
            subst ha
            obtain ⟨v, hv⟩ := evalOp_binary_ok op hop (a.eval σ E) (b.eval σ E)
            simp only [NF.eval, hv]
            rw [evalOp_width2 hop hv, iha hwf.1 haw]
      · exact absurd ha (by simp)
  | cat a b iha ihb =>
      intro w hwf ha
      simp only [annWidth] at ha
      cases haw : annWidth a with
      | none => rw [haw] at ha; exact absurd ha (by simp)
      | some wa =>
          cases hbw : annWidth b with
          | none => rw [haw, hbw] at ha; exact absurd ha (by simp)
          | some wb =>
              rw [haw, hbw] at ha
              simp only [Option.some.injEq] at ha
              subst ha
              show (a.eval σ E).width + (b.eval σ E).width = wa + wb
              rw [iha hwf.1 haw, ihb hwf.2 hbw]
  | slice i w' e ihe =>
      intro w _ ha
      simp only [annWidth, Option.some.injEq] at ha
      subst ha
      rfl
  | ite c t e ihc iht ihe =>
      intro w hwf ha
      simp only [annWidth] at ha
      cases htw : annWidth t with
      | none => rw [htw] at ha; exact absurd ha (by simp)
      | some wt =>
          cases hew : annWidth e with
          | none => rw [htw, hew] at ha; exact absurd ha (by simp)
          | some we =>
              rw [htw, hew] at ha
              dsimp only at ha
              by_cases hte : wt = we
              · rw [if_pos hte] at ha
                injection ha with ha
                subst ha
                subst hte
                simp only [NF.eval]
                split
                · exact iht hwf.2.1 htw
                · exact ihe hwf.2.2 hew
              · rw [if_neg hte] at ha
                exact absurd ha (by simp)
  | xcall w' x a iha =>
      intro w _ ha
      simp only [annWidth, Option.some.injEq] at ha
      subst ha
      exact Sem.xapply_width E x w' (a.eval σ E)

/-- A successful `mapM` transports a pointwise property. -/
private theorem mapM_ok_forall {α β : Type} {g : α → Except String β} {P : β → Prop} :
    ∀ {as : List α} {bs : List β}, as.mapM g = .ok bs →
      (∀ a ∈ as, ∀ b, g a = .ok b → P b) → ∀ b ∈ bs, P b := by
  intro as
  induction as with
  | nil =>
      intro bs hg _
      rw [List.mapM_nil, except_pure_def] at hg
      injection hg with hg
      subst hg
      intro b hb
      exact absurd hb (by simp)
  | cons a as ih =>
      intro bs hg hpt
      rw [List.mapM_cons] at hg
      obtain ⟨b, hb, h₁⟩ := except_bind_eq_ok hg
      obtain ⟨bs', hbs, h₂⟩ := except_bind_eq_ok h₁
      have h₃ : (Except.ok (b :: bs') : Except String (List β)) = .ok bs := h₂
      injection h₃ with h₃
      subst h₃
      intro c hc
      rcases List.mem_cons.mp hc with hc | hc
      · subst hc
        exact hpt a List.mem_cons_self c hb
      · exact ih hbs (fun a ha => hpt a (List.mem_cons_of_mem _ ha)) c hc

/-- Packing preserves the width discipline. -/
private theorem xpack_varsWF {P : String → Nat → Prop} {ns : List NF}
    (h : ∀ n ∈ ns, n.VarsWF P) : (NF.xpack ns).VarsWF P := by
  suffices hgen : ∀ (acc : NF) (ns : List NF), acc.VarsWF P → (∀ n ∈ ns, n.VarsWF P) →
      (ns.foldl NF.cat acc).VarsWF P from hgen (.lit BV.nil) ns trivial h
  intro acc ns
  induction ns generalizing acc with
  | nil => intro hacc _; exact hacc
  | cons n ns ih =>
      intro hacc hns
      rw [List.foldl_cons]
      exact ih (.cat acc n) ⟨hacc, hns n List.mem_cons_self⟩
        (fun m hm => hns m (List.mem_cons_of_mem _ hm))

theorem symExp_varsWF {dmap : HashMap String Defn} {X : Sem.XEnv} {P : String → Nat → Prop} :
    ∀ (fuel : Nat) (e : Exp) (ρ : HashMap String NF) (nf : NF),
      (∀ x n, ρ.get? x = some n → n.VarsWF P) →
      symExp dmap X fuel ρ e = .ok nf → nf.VarsWF P := by
  intro fuel
  induction fuel with
  | zero =>
      intro e ρ nf _ hs
      exact absurd hs (by simp [symExp])
  | succ fuel ih =>
      intro e ρ nf hρ hs
      cases e with
      | lit v =>
          simp only [symExp] at hs
          injection hs with hs
          subst hs
          trivial
      | undef w =>
          simp only [symExp] at hs
          injection hs with hs
          subst hs
          trivial
      | var w x =>
          simp only [symExp] at hs
          cases hx : ρ.get? x with
          | none => rw [hx] at hs; exact absurd hs (by simp)
          | some n =>
              rw [hx] at hs
              injection hs with hs
              subst hs
              exact hρ x n hx
      | cat e₁ e₂ =>
          simp only [symExp] at hs
          obtain ⟨n₁, h₁, hs⟩ := except_bind_eq_ok hs
          obtain ⟨n₂, h₂, hs⟩ := except_bind_eq_ok hs
          have hs : (Except.ok (NF.cat n₁ n₂) : Except String NF) = .ok nf := hs
          injection hs with hs
          subst hs
          exact ⟨ih e₁ ρ n₁ hρ h₁, ih e₂ ρ n₂ hρ h₂⟩
      | slice i w e =>
          simp only [symExp] at hs
          obtain ⟨n, h₁, hs⟩ := except_bind_eq_ok hs
          have hs : (Except.ok (NF.slice i w n) : Except String NF) = .ok nf := hs
          injection hs with hs
          subst hs
          exact ih e ρ n hρ h₁
      | prim w op args =>
          simp only [symExp] at hs
          obtain ⟨ns, hns, hs⟩ := except_bind_eq_ok hs
          have hall : ∀ n ∈ ns, n.VarsWF P :=
            mapM_ok_forall hns (fun a _ n hn => ih a ρ n hρ hn)
          match ns, hs with
          | [a], hs => ?one
          | [a, b], hs => ?two
          | [], hs => exact absurd hs (by simp)
          | _ :: _ :: _ :: _, hs => exact absurd hs (by simp)
          case one =>
            dsimp only at hs
            split at hs
            · injection hs with hs
              subst hs
              exact hall a List.mem_cons_self
            · exact absurd hs (by simp)
          case two =>
            dsimp only at hs
            split at hs
            · injection hs with hs
              subst hs
              exact ⟨hall a List.mem_cons_self,
                     hall b (List.mem_cons_of_mem _ List.mem_cons_self)⟩
            · exact absurd hs (by simp)
      | call w f args =>
          simp only [symExp] at hs
          obtain ⟨ns, hns, hs⟩ := except_bind_eq_ok hs
          have hall : ∀ n ∈ ns, n.VarsWF P :=
            mapM_ok_forall hns (fun a _ n hn => ih a ρ n hρ hn)
          cases hd : dmap.get? f with
          | none => rw [hd] at hs; exact absurd hs (by simp)
          | some d =>
              rw [hd] at hs
              dsimp only at hs
              split at hs
              · refine ih d.body _ nf ?_ hs
                intro x n hx
                have hpair := ofList_get?_some hx
                exact hall n (List.of_mem_zip hpair).2
              · exact absurd hs (by simp)
      | xcall w ext gs args =>
          simp only [symExp] at hs
          obtain ⟨ns, hns, hs⟩ := except_bind_eq_ok hs
          have hall : ∀ n ∈ ns, n.VarsWF P :=
            mapM_ok_forall hns (fun a _ n hn => ih a ρ n hρ hn)
          cases hx : X.get? ext with
          | some model =>
              rw [hx] at hs
              dsimp only at hs
              cases hd : dmap.get? model with
              | none => rw [hd] at hs; exact absurd hs (by simp)
              | some d =>
                  rw [hd] at hs
                  dsimp only at hs
                  split at hs
                  · refine ih d.body _ nf ?_ hs
                    intro x n hx'
                    have hpair := ofList_get?_some hx'
                    exact hall n (List.of_mem_zip hpair).2
                  · exact absurd hs (by simp)
          | none =>
              rw [hx] at hs
              dsimp only at hs
              split at hs
              · injection hs with hs
                subst hs
                exact xpack_varsWF hall
              · exact absurd hs (by simp)
      | ite w c t e =>
          simp only [symExp] at hs
          obtain ⟨nc, h₁, hs⟩ := except_bind_eq_ok hs
          obtain ⟨nt, h₂, hs⟩ := except_bind_eq_ok hs
          obtain ⟨ne, h₃, hs⟩ := except_bind_eq_ok hs
          have hs : (Except.ok (NF.ite nc nt ne) : Except String NF) = .ok nf := hs
          injection hs with hs
          subst hs
          exact ⟨ih c ρ nc hρ h₁, ih t ρ nt hρ h₂, ih e ρ ne hρ h₃⟩
      | letE w x rhs body =>
          simp only [symExp] at hs
          obtain ⟨n, h₁, hs⟩ := except_bind_eq_ok hs
          refine ih body _ nf ?_ hs
          intro y m hy
          rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert] at hy
          split at hy
          · injection hy with hy
            exact hy ▸ ih rhs ρ n hρ h₁
          · exact hρ y m hy

/-- Values in the symbolic step's maps satisfy the discipline. -/
private def MapWF (P : String → Nat → Prop) (m : HashMap String NF) : Prop :=
  ∀ k n, m.get? k = some n → n.VarsWF P

private theorem mapWF_empty {P : String → Nat → Prop} : MapWF P ∅ := by
  intro k n h
  rw [HashMap.get?_eq_getElem?, HashMap.getElem?_empty] at h
  exact absurd h (by simp)

private theorem mapWF_insert {P : String → Nat → Prop} {m : HashMap String NF}
    (h : MapWF P m) {x : String} {n : NF} (hn : n.VarsWF P) : MapWF P (m.insert x n) := by
  intro k n' hk
  rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert] at hk
  split at hk
  · injection hk with hk; subst hk; exact hn
  · exact h k n' hk

/-- The body fold preserves the discipline on all three maps. -/
private theorem body_fold_varsWF {dmap : HashMap String Defn} {X : Sem.XEnv} {fuel : Nat}
    {P : String → Nat → Prop} :
    ∀ (stmts : List Stmt) (ρS outsS nextsS : HashMap String NF)
      (resS : HashMap String NF × HashMap String NF × HashMap String NF),
      stmts.foldlM (symBody dmap X fuel) (ρS, outsS, nextsS) = .ok resS →
      MapWF P ρS → MapWF P outsS → MapWF P nextsS →
      MapWF P resS.2.1 ∧ MapWF P resS.2.2 := by
  intro stmts
  induction stmts with
  | nil =>
      intro ρS outsS nextsS resS hsym hρ ho hn
      rw [List.foldlM_nil, except_pure_def] at hsym
      injection hsym with hsym
      subst hsym
      exact ⟨ho, hn⟩
  | cons stmt stmts ih =>
      intro ρS outsS nextsS resS hsym hρ ho hn
      rw [List.foldlM_cons] at hsym
      obtain ⟨stS, hbody, hrest⟩ := except_bind_eq_ok hsym
      cases stmt with
      | sLet x e =>
          dsimp only [symBody] at hbody
          obtain ⟨n, hne, hpure⟩ := except_bind_eq_ok hbody
          have hpure : (Except.ok (ρS.insert x n, outsS, nextsS) :
              Except String (HashMap String NF × HashMap String NF × HashMap String NF))
              = .ok stS := hpure
          injection hpure with hpure
          subst hpure
          exact ih _ _ _ resS hrest
            (mapWF_insert hρ (symExp_varsWF fuel e ρS n hρ hne)) ho hn
      | sOutput o e =>
          dsimp only [symBody] at hbody
          cases hcont : outsS.contains o with
          | true => rw [hcont] at hbody; exact nomatch hbody
          | false =>
              rw [hcont] at hbody
              simp only [Bool.false_eq_true, if_false] at hbody
              obtain ⟨n, hne, hpure⟩ := except_bind_eq_ok hbody
              have hpure : (Except.ok (ρS, outsS.insert o n, nextsS) :
                  Except String (HashMap String NF × HashMap String NF × HashMap String NF))
                  = .ok stS := hpure
              injection hpure with hpure
              subst hpure
              exact ih _ _ _ resS hrest hρ
                (mapWF_insert ho (symExp_varsWF fuel e ρS n hρ hne)) hn
      | sNext r e =>
          dsimp only [symBody] at hbody
          cases hcont : nextsS.contains r with
          | true => rw [hcont] at hbody; exact nomatch hbody
          | false =>
              rw [hcont] at hbody
              simp only [Bool.false_eq_true, if_false] at hbody
              obtain ⟨n, hne, hpure⟩ := except_bind_eq_ok hbody
              have hpure : (Except.ok (ρS, outsS, nextsS.insert r n) :
                  Except String (HashMap String NF × HashMap String NF × HashMap String NF))
                  = .ok stS := hpure
              injection hpure with hpure
              subst hpure
              exact ih _ _ _ resS hrest hρ ho
                (mapWF_insert hn (symExp_varsWF fuel e ρS n hρ hne))
      | sInstIn inst port e =>
          dsimp only [symBody] at hbody
          exact absurd hbody (by simp)

/-- The variable discipline delivered by `symStep`: every output and
next normal form only mentions device inputs and registers at their
declared widths. -/
theorem symStep_varsWF {dmap : HashMap String Defn} {X : Sem.XEnv} {fuel : Nat} {dev : Device}
    {ss : StepNF} (hsym : symStep dmap X fuel dev = .ok ss) :
    ∀ p ∈ ss.outs ++ ss.nexts,
      p.2.VarsWF (fun x w =>
        (x, w) ∈ dev.inputs ++ dev.registers.map fun r => (r.name, r.width)) := by
  rw [symStep] at hsym
  obtain ⟨resS, hfoldS, hfin⟩ := except_bind_eq_ok hsym
  obtain ⟨ρS', outsS, nextsS⟩ := resS
  have hinit : MapWF (fun x w =>
      (x, w) ∈ dev.inputs ++ dev.registers.map fun r => (r.name, r.width)) (initSymEnv dev) := by
    intro k n hk
    rcases foldl_insert_var_get? _ _ hk with h | h
    · obtain ⟨w, hn, hw⟩ := h
      subst hn
      exact hw
    · rw [HashMap.get?_eq_getElem?, HashMap.getElem?_empty] at h
      exact absurd h (by simp)
  obtain ⟨hoWF, hnWF⟩ := body_fold_varsWF dev.body _ _ _ _ hfoldS hinit mapWF_empty mapWF_empty
  dsimp only [symFinish] at hfin
  obtain ⟨outsL, hoL, hfin⟩ := except_bind_eq_ok hfin
  obtain ⟨nextsL, hnL, hfin⟩ := except_bind_eq_ok hfin
  have hss : (Except.ok (⟨outsL, nextsL⟩ : StepNF) : Except String StepNF) = .ok ss := hfin
  injection hss with hss
  subst hss
  intro p hp
  rcases List.mem_append.mp hp with hp | hp
  · refine mapM_ok_forall (P := fun q : String × NF => q.2.VarsWF fun x w =>
      (x, w) ∈ dev.inputs ++ dev.registers.map fun r => (r.name, r.width)) hoL ?_ p hp
    intro a _ b hb
    obtain ⟨o, w⟩ := a
    dsimp only at hb
    cases hget : outsS.get? o with
    | none => rw [hget] at hb; exact absurd hb (by simp)
    | some n =>
        rw [hget] at hb
        have hb : (Except.ok (o, n) : Except String (String × NF)) = .ok b := hb
        injection hb with hb
        subst hb
        exact hoWF o n hget
  · refine mapM_ok_forall (P := fun q : String × NF => q.2.VarsWF fun x w =>
      (x, w) ∈ dev.inputs ++ dev.registers.map fun r => (r.name, r.width)) hnL ?_ p hp
    intro r _ b hb
    cases hget : nextsS.get? r.name with
    | none => rw [hget] at hb; exact absurd hb (by simp)
    | some n =>
        rw [hget] at hb
        have hb : (Except.ok (r.name, n) : Except String (String × NF)) = .ok b := hb
        injection hb with hb
        subst hb
        exact hnWF r.name n hget

/-! ## The width-aware normalizer

The `Transform.partialEval` rewrite set over normal forms. Rewrites
that need an operand's width read it from `annWidth` and are sound at
width-respecting valuations (`VarsWF (WP σ)`); the rest are sound
unconditionally. -/

/-- BV extensionality via `getLsbD`. -/
private theorem bv_eq_of {x y : BV} (hw : x.width = y.width)
    (hb : ∀ i, i < x.width → x.bits.getLsbD i = y.bits.getLsbD i) : x = y := by
  cases x with | mk wx bx =>
  cases y with | mk wy by' =>
  dsimp only at hw hb
  subst hw
  exact congrArg (BV.mk wx) (BitVec.eq_of_getLsbD_eq_iff.mpr hb)

namespace NF

/-- The slice normalizer: slice of literal folds; slice of slice
fuses (in bounds); slice of concatenation selects or splits at the
low side's annotated width; full-width slices are the identity. -/
def mkSliceW (i w : Nat) : NF → NF
  | .lit v => .lit ⟨w, v.bits.extractLsb' i w⟩
  | .slice j v e =>
      if i + w ≤ v then mkSliceW (j + i) w e
      else .slice i w (.slice j v e)
  | .cat a b =>
      (match annWidth b with
      | some wb =>
          if wb ≤ i then mkSliceW (i - wb) w a
          else if i + w ≤ wb then mkSliceW i w b
          else .cat (mkSliceW 0 (i + w - wb) a) (mkSliceW i (wb - i) b)
      | none => .slice i w (.cat a b))
  | e =>
      (match annWidth e with
      | some we => if i = 0 ∧ w = we then e else .slice i w e
      | none => .slice i w e)

/-- Flatten a concatenation spine into its MSB-first pieces. -/
def catPieces : NF → List NF
  | .cat a b => catPieces a ++ catPieces b
  | e => [e]

/-- Rebuild a right-nested concatenation from MSB-first pieces. -/
def rebuildCat : List NF → NF
  | [] => .lit BV.nil
  | [p] => p
  | p :: ps => .cat p (rebuildCat ps)

/-- One merge step against an already-merged tail: a literal absorbs a
literal head; a slice fuses with an adjacent slice head of the same
base (through the slice normalizer, catching full-width identities). -/
def mergeStep (p : NF) (merged : List NF) : List NF :=
  match p, merged with
  | .lit v, .lit u :: rest => .lit ⟨v.width + u.width, v.bits ++ u.bits⟩ :: rest
  | .slice i₁ w₁ e₁, .slice i₂ w₂ e₂ :: rest =>
      if e₁ = e₂ ∧ i₁ = i₂ + w₂ then mkSliceW i₂ (w₁ + w₂) e₁ :: rest
      else .slice i₁ w₁ e₁ :: .slice i₂ w₂ e₂ :: rest
  | p, merged => p :: merged

/-- Merge adjacent pieces, right to left. -/
def mergePieces : List NF → List NF
  | [] => []
  | p :: rest => mergeStep p (mergePieces rest)

/-- The concatenation normalizer: gather the spine, merge adjacent
pieces, rebuild (partialEval's `mergeCat`). -/
def mkCatW (a b : NF) : NF :=
  rebuildCat (mergePieces (catPieces a ++ catPieces b))

/-- Unary normalizer: double negation cancels; otherwise the constant
folder. -/
def mk1W (op : Op) (a : NF) : NF :=
  match op with
  | .not =>
      (match a with
      | .prim1 .not b => b
      | a => mk1 .not a)
  | op => mk1 op a

/-- Binary normalizer: modulus by a zero literal is the identity; the
1-bit equality peepholes (equality with the width-1 literals is the
identity or the negation, when the operand's annotated width is 1);
otherwise the constant folder. -/
def mk2W (op : Op) (a b : NF) : NF :=
  match op, a, b with
  | .umod, a, .lit u => if u.nat = 0 then a else mk2 .umod a (.lit u)
  | .eq, a, .lit u =>
      if annWidth a = some 1 ∧ u = ⟨1, 1#1⟩ then a
      else if annWidth a = some 1 ∧ u = ⟨1, 0#1⟩ then mk1W .not a
      else mk2 .eq a (.lit u)
  | .eq, .lit u, b =>
      if annWidth b = some 1 ∧ u = ⟨1, 1#1⟩ then b
      else if annWidth b = some 1 ∧ u = ⟨1, 0#1⟩ then mk1W .not b
      else mk2 .eq (.lit u) b
  | op, a, b => mk2 op a b

/-- Mux normalizer: the 1-bit boolean-mux peepholes (a mux between
the two 1-bit literals is the condition or its negation); otherwise
the literal-condition selector. -/
def mkIteW (c t e : NF) : NF :=
  match t, e with
  | .lit v, .lit u =>
      if v.width = 1 ∧ u.width = 1 ∧ v.nat = 1 ∧ u.nat = 0 ∧ annWidth c = some 1 then c
      else if v.width = 1 ∧ u.width = 1 ∧ v.nat = 0 ∧ u.nat = 1 ∧ annWidth c = some 1 then
        mk1W .not c
      else mkIte c (.lit v) (.lit u)
  | t, e => mkIte c t e

/-- The width-aware bottom-up normalizer. -/
def cfoldW : NF → NF
  | .var w x => .var w x
  | .lit v => .lit v
  | .prim1 op a => mk1W op a.cfoldW
  | .prim2 op a b => mk2W op a.cfoldW b.cfoldW
  | .cat a b => mkCatW a.cfoldW b.cfoldW
  | .slice i w e => mkSliceW i w e.cfoldW
  | .ite c t e => mkIteW c.cfoldW t.cfoldW e.cfoldW
  | .xcall w x a => .xcall w x a.cfoldW

end NF

/-! ### Discipline preservation -/

private theorem mkSliceW_varsWF {P : String → Nat → Prop} :
    ∀ (e : NF) (i w : Nat), e.VarsWF P → (NF.mkSliceW i w e).VarsWF P
  | .lit v, i, w, h => trivial
  | .slice j v e, i, w, h => by
      simp only [NF.mkSliceW]
      split
      · exact mkSliceW_varsWF e (j + i) w h
      · exact h
  | .cat a b, i, w, h => by
      simp only [NF.mkSliceW]
      cases annWidth b with
      | none => exact h
      | some wb =>
          dsimp only
          split
          · exact mkSliceW_varsWF a (i - wb) w h.1
          · split
            · exact mkSliceW_varsWF b i w h.2
            · exact ⟨mkSliceW_varsWF a 0 (i + w - wb) h.1,
                     mkSliceW_varsWF b i (wb - i) h.2⟩
  | .var v x, i, w, h => by
      simp only [NF.mkSliceW]
      cases annWidth (NF.var v x) with
      | none => exact h
      | some we =>
          dsimp only
          split
          · exact h
          · exact h
  | .prim1 op a, i, w, h => by
      simp only [NF.mkSliceW]
      cases annWidth (NF.prim1 op a) with
      | none => exact h
      | some we =>
          dsimp only
          split
          · exact h
          · exact h
  | .prim2 op a b, i, w, h => by
      simp only [NF.mkSliceW]
      cases annWidth (NF.prim2 op a b) with
      | none => exact h
      | some we =>
          dsimp only
          split
          · exact h
          · exact h
  | .ite c t e, i, w, h => by
      simp only [NF.mkSliceW]
      cases annWidth (NF.ite c t e) with
      | none => exact h
      | some we =>
          dsimp only
          split
          · exact h
          · exact h
  | .xcall v x a, i, w, h => by
      simp only [NF.mkSliceW]
      cases annWidth (NF.xcall v x a) with
      | none => exact h
      | some we =>
          dsimp only
          split
          · exact h
          · exact h

private theorem mk1_varsWF {P : String → Nat → Prop} {op : Op} {a : NF}
    (h : a.VarsWF P) : (NF.mk1 op a).VarsWF P := by
  cases a <;> try exact h
  case lit v =>
    simp only [NF.mk1]
    cases Sem.evalOp op [v] with
    | ok r => trivial
    | error e => trivial

private theorem mk2_varsWF {P : String → Nat → Prop} {op : Op} {a b : NF}
    (ha : a.VarsWF P) (hb : b.VarsWF P) : (NF.mk2 op a b).VarsWF P := by
  cases a <;> cases b <;> try exact ⟨ha, hb⟩
  case lit.lit v u =>
    simp only [NF.mk2]
    cases Sem.evalOp op [v, u] with
    | ok r => trivial
    | error e => exact ⟨ha, hb⟩

private theorem mk1W_varsWF {P : String → Nat → Prop} {op : Op} {a : NF}
    (h : a.VarsWF P) : (NF.mk1W op a).VarsWF P := by
  unfold NF.mk1W
  split
  · split
    · exact h
    · exact mk1_varsWF h
  · exact mk1_varsWF h

private theorem mk2W_varsWF {P : String → Nat → Prop} {op : Op} {a b : NF}
    (ha : a.VarsWF P) (hb : b.VarsWF P) : (NF.mk2W op a b).VarsWF P := by
  unfold NF.mk2W
  split
  · split
    · exact ha
    · exact mk2_varsWF ha hb
  · split
    · exact ha
    · split
      · exact mk1W_varsWF ha
      · exact mk2_varsWF ha hb
  · split
    · exact hb
    · split
      · exact mk1W_varsWF hb
      · exact mk2_varsWF ha hb
  · exact mk2_varsWF ha hb

private theorem catPieces_varsWF {P : String → Nat → Prop} :
    ∀ {e : NF}, e.VarsWF P → ∀ p ∈ NF.catPieces e, p.VarsWF P := by
  intro e
  induction e with
  | cat a b iha ihb =>
      intro h p hp
      rcases List.mem_append.mp (by simpa [NF.catPieces] using hp) with hp | hp
      · exact iha h.1 p hp
      · exact ihb h.2 p hp
  | var w x => intro h p hp; rw [show NF.catPieces (NF.var w x) = [NF.var w x] from rfl] at hp; simp at hp; subst hp; exact h
  | lit v => intro h p hp; rw [show NF.catPieces (NF.lit v) = [NF.lit v] from rfl] at hp; simp at hp; subst hp; exact h
  | xcall w x a iha => intro h p hp; rw [show NF.catPieces (NF.xcall w x a) = [NF.xcall w x a] from rfl] at hp; simp at hp; subst hp; exact h
  | prim1 op a iha => intro h p hp; rw [show NF.catPieces (NF.prim1 op a) = [NF.prim1 op a] from rfl] at hp; simp at hp; subst hp; exact h
  | prim2 op a b iha ihb => intro h p hp; rw [show NF.catPieces (NF.prim2 op a b) = [NF.prim2 op a b] from rfl] at hp; simp at hp; subst hp; exact h
  | slice i w e ihe => intro h p hp; rw [show NF.catPieces (NF.slice i w e) = [NF.slice i w e] from rfl] at hp; simp at hp; subst hp; exact h
  | ite c t e ihc iht ihe => intro h p hp; rw [show NF.catPieces (NF.ite c t e) = [NF.ite c t e] from rfl] at hp; simp at hp; subst hp; exact h

private theorem rebuildCat_varsWF {P : String → Nat → Prop} :
    ∀ {ps : List NF}, (∀ p ∈ ps, p.VarsWF P) → (NF.rebuildCat ps).VarsWF P := by
  intro ps
  induction ps with
  | nil => intro _; trivial
  | cons p ps ih =>
      intro h
      cases ps with
      | nil => exact h p List.mem_cons_self
      | cons q ps' =>
          exact ⟨h p List.mem_cons_self, ih fun r hr => h r (List.mem_cons_of_mem _ hr)⟩

private theorem mergePieces_varsWF {P : String → Nat → Prop} :
    ∀ (ps : List NF), (∀ p ∈ ps, p.VarsWF P) → ∀ p ∈ NF.mergePieces ps, p.VarsWF P := by
  intro ps
  induction ps with
  | nil => intro _ p hp; exact absurd hp (by simp [NF.mergePieces])
  | cons p rest ih =>
      intro h q hq
      have hhd : p.VarsWF P := h p List.mem_cons_self
      have htl : ∀ r ∈ NF.mergePieces rest, r.VarsWF P :=
        ih fun r hr => h r (List.mem_cons_of_mem _ hr)
      rw [show NF.mergePieces (p :: rest) = NF.mergeStep p (NF.mergePieces rest) from rfl] at hq
      unfold NF.mergeStep at hq
      split at hq
      · rename_i v u rest' heq
        rcases List.mem_cons.mp hq with hq | hq
        · subst hq; trivial
        · exact htl q (by rw [heq]; exact List.mem_cons_of_mem _ hq)
      · rename_i i₁ w₁ e₁ i₂ w₂ e₂ rest' heq
        split at hq
        · rcases List.mem_cons.mp hq with hq | hq
          · subst hq
            exact mkSliceW_varsWF e₁ i₂ (w₁ + w₂) hhd
          · exact htl q (by rw [heq]; exact List.mem_cons_of_mem _ hq)
        · rcases List.mem_cons.mp hq with hq | hq
          · subst hq; exact hhd
          · exact htl q (by rw [heq]; exact hq)
      · rcases List.mem_cons.mp hq with hq | hq
        · subst hq; exact hhd
        · exact htl q hq

private theorem mkCatW_varsWF {P : String → Nat → Prop} {a b : NF}
    (ha : a.VarsWF P) (hb : b.VarsWF P) : (NF.mkCatW a b).VarsWF P := by
  refine rebuildCat_varsWF (mergePieces_varsWF _ ?_)
  intro p hp
  rcases List.mem_append.mp hp with hp | hp
  · exact catPieces_varsWF ha p hp
  · exact catPieces_varsWF hb p hp

private theorem mkIte_varsWF {P : String → Nat → Prop} {c t e : NF}
    (hc : c.VarsWF P) (ht : t.VarsWF P) (he : e.VarsWF P) :
    (NF.mkIte c t e).VarsWF P := by
  cases c <;> try exact ⟨hc, ht, he⟩
  case lit v =>
    simp only [NF.mkIte]
    split
    · exact ht
    · exact he

private theorem mkIteW_varsWF {P : String → Nat → Prop} {c t e : NF}
    (hc : c.VarsWF P) (ht : t.VarsWF P) (he : e.VarsWF P) :
    (NF.mkIteW c t e).VarsWF P := by
  unfold NF.mkIteW
  split
  · split
    · exact hc
    · split
      · exact mk1W_varsWF hc
      · exact mkIte_varsWF hc ht he
  · exact mkIte_varsWF hc ht he

theorem cfoldW_varsWF {P : String → Nat → Prop} :
    ∀ {nf : NF}, nf.VarsWF P → nf.cfoldW.VarsWF P := by
  intro nf
  induction nf with
  | var w x => exact id
  | lit v => exact id
  | xcall w x a iha => exact fun h => iha h
  | prim1 op a iha => exact fun h => mk1W_varsWF (iha h)
  | prim2 op a b iha ihb => exact fun h => mk2W_varsWF (iha h.1) (ihb h.2)
  | cat a b iha ihb => exact fun h => mkCatW_varsWF (iha h.1) (ihb h.2)
  | slice i w e ihe => exact fun h => mkSliceW_varsWF _ i w (ihe h)
  | ite c t e ihc iht ihe =>
      exact fun h => mkIteW_varsWF (ihc h.1) (iht h.2.1) (ihe h.2.2)

/-! ### Denotation preservation of the width-aware normalizer -/

/-- BV extensionality at a common literal width. -/
private theorem bv_eq_of' {x y : BV} {w : Nat} (hwx : x.width = w) (hwy : y.width = w)
    (hb : ∀ i, i < w → x.bits.getLsbD i = y.bits.getLsbD i) : x = y := by
  refine bv_eq_of (by rw [hwx, hwy]) ?_
  intro i hi
  exact hb i (hwx ▸ hi)

/-- A width-1 BV is one of the two bits. -/
private theorem bv1_cases (x : BV) (h : x.width = 1) :
    x = ⟨1, 0#1⟩ ∨ x = ⟨1, 1#1⟩ := by
  cases x with | mk w bits =>
  dsimp only at h
  subst h
  rcases BitVec.eq_zero_or_eq_one bits with h | h
  · left; rw [h]
  · right; rw [h]

private theorem slice_id_default_eval {σ : String → BV} {E : Sem.EEnv} {e : NF} (h : e.VarsWF (WP σ))
    (i w : Nat) :
    ((match annWidth e with
      | some we => if i = 0 ∧ w = we then e else NF.slice i w e
      | none => NF.slice i w e) : NF).eval σ E = (NF.slice i w e).eval σ E := by
  cases hann : annWidth e with
  | none => rfl
  | some we =>
      dsimp only
      by_cases hid : i = 0 ∧ w = we
      · rw [if_pos hid]
        obtain ⟨h0, hwe⟩ := hid
        subst h0
        subst hwe
        have hW := annWidth_eval (E := E) h hann
        refine bv_eq_of' (w := w) hW rfl ?_
        intro k hk
        simp only [NF.eval, BitVec.getLsbD_extractLsb', Nat.zero_add]
        simp [hk]
      · rw [if_neg hid]

/-- Projection views of the slice/cat denotations: everything below
works at the `width`/`getLsbD` level, never rebuilding dependent
appends. -/
private theorem eval_slice_width (σ : String → BV) (E : Sem.EEnv) (i w : Nat) (e : NF) :
    ((NF.slice i w e).eval σ E).width = w := rfl

private theorem eval_cat_width (σ : String → BV) (E : Sem.EEnv) (a b : NF) :
    ((NF.cat a b).eval σ E).width = (a.eval σ E).width + (b.eval σ E).width := rfl

private theorem eval_slice_getLsbD (σ : String → BV) (E : Sem.EEnv) (i w : Nat) (e : NF) (k : Nat) :
    ((NF.slice i w e).eval σ E).bits.getLsbD k
      = (decide (k < w) && (e.eval σ E).bits.getLsbD (i + k)) := by
  show (BitVec.extractLsb' i w (e.eval σ E).bits).getLsbD k = _
  exact BitVec.getLsbD_extractLsb' i w (e.eval σ E).bits k

private theorem eval_cat_getLsbD (σ : String → BV) (E : Sem.EEnv) (a b : NF) (k : Nat) :
    ((NF.cat a b).eval σ E).bits.getLsbD k
      = if k < (b.eval σ E).width then (b.eval σ E).bits.getLsbD k
        else (a.eval σ E).bits.getLsbD (k - (b.eval σ E).width) := by
  show ((a.eval σ E).bits ++ (b.eval σ E).bits).getLsbD k = _
  exact BitVec.getLsbD_append

/-- The concatenation view at a KNOWN low-side width (avoids
dependent rewriting of the width inside `bits`' type). -/
private theorem eval_cat_getLsbD' {σ : String → BV} {E : Sem.EEnv} {b : NF} {wb : Nat}
    (hwb : (b.eval σ E).width = wb) (a : NF) (k : Nat) :
    ((NF.cat a b).eval σ E).bits.getLsbD k
      = if k < wb then (b.eval σ E).bits.getLsbD k
        else (a.eval σ E).bits.getLsbD (k - wb) := by
  subst hwb
  exact eval_cat_getLsbD σ E a b k

private theorem eval_lit_width (σ : String → BV) (E : Sem.EEnv) (v : BV) :
    ((NF.lit v).eval σ E).width = v.width := rfl

private theorem eval_lit_getLsbD (σ : String → BV) (E : Sem.EEnv) (v : BV) (k : Nat) :
    ((NF.lit v).eval σ E).bits.getLsbD k = v.bits.getLsbD k := rfl

/-- Congruence for concatenation denotations (whole-value rewriting
keeps the dependent append well-typed). -/
private theorem cat_eval_congr {σ : String → BV} {E : Sem.EEnv} {a a' b b' : NF}
    (hA : a.eval σ E = a'.eval σ E) (hB : b.eval σ E = b'.eval σ E) :
    (NF.cat a b).eval σ E = (NF.cat a' b').eval σ E := by
  show (⟨_, (a.eval σ E).bits ++ (b.eval σ E).bits⟩ : BV)
     = ⟨_, (a'.eval σ E).bits ++ (b'.eval σ E).bits⟩
  rw [hA, hB]

theorem mkSliceW_eval {σ : String → BV} {E : Sem.EEnv} :
    ∀ (e : NF) (i w : Nat), e.VarsWF (WP σ) →
      (NF.mkSliceW i w e).eval σ E = (NF.slice i w e).eval σ E
  | .lit v, i, w, _ => rfl
  | .slice j v e, i, w, h => by
      simp only [NF.mkSliceW]
      split
      · rename_i hle
        rw [mkSliceW_eval e (j + i) w h]
        refine bv_eq_of' (w := w) (eval_slice_width ..) (eval_slice_width ..) ?_
        intro k hk
        rw [eval_slice_getLsbD, eval_slice_getLsbD, eval_slice_getLsbD]
        have h1 : i + k < v := by omega
        simp only [hk, h1, decide_true, Bool.true_and]
        rw [show j + i + k = j + (i + k) from by omega]
      · rfl
  | .cat a b, i, w, h => by
      simp only [NF.mkSliceW]
      cases hbw : annWidth b with
      | none => rfl
      | some wb =>
          have hwb : (b.eval σ E).width = wb := annWidth_eval h.2 hbw
          dsimp only
          split
          · rename_i h1
            rw [mkSliceW_eval a (i - wb) w h.1]
            refine bv_eq_of' (w := w) (eval_slice_width ..) (eval_slice_width ..) ?_
            intro k hk
            rw [eval_slice_getLsbD, eval_slice_getLsbD, eval_cat_getLsbD' hwb]
            have h2 : ¬ (i + k < wb) := by omega
            rw [if_neg h2, show i + k - wb = i - wb + k from by omega]
          · split
            · rename_i h1 h2
              rw [mkSliceW_eval b i w h.2]
              refine bv_eq_of' (w := w) (eval_slice_width ..) (eval_slice_width ..) ?_
              intro k hk
              rw [eval_slice_getLsbD, eval_slice_getLsbD, eval_cat_getLsbD' hwb]
              have h3 : i + k < wb := by omega
              rw [if_pos h3]
            · rename_i h1 h2
              rw [cat_eval_congr (mkSliceW_eval a 0 (i + w - wb) h.1)
                    (mkSliceW_eval b i (wb - i) h.2)]
              have hlw : ((NF.cat (NF.slice 0 (i + w - wb) a) (NF.slice i (wb - i) b)).eval σ E).width
                  = w := by
                rw [eval_cat_width, eval_slice_width, eval_slice_width]
                omega
              refine bv_eq_of' (w := w) hlw (eval_slice_width ..) ?_
              intro k hk
              rw [eval_cat_getLsbD' (eval_slice_width σ E i (wb - i) b),
                  eval_slice_getLsbD, eval_slice_getLsbD,
                  eval_slice_getLsbD, eval_cat_getLsbD' hwb]
              by_cases hkw : k < wb - i
              · have h3 : i + k < wb := by omega
                rw [if_pos hkw, if_pos h3]
                simp [hk, hkw]
              · have h3 : ¬ (i + k < wb) := by omega
                have h4 : k - (wb - i) < i + w - wb := by omega
                rw [if_neg hkw, if_neg h3]
                simp only [h4, decide_true, Bool.true_and, Nat.zero_add, hk]
                rw [show k - (wb - i) = i + k - wb from by omega]
  | .var v x, i, w, h => by
      simp only [NF.mkSliceW]
      exact slice_id_default_eval h i w
  | .prim1 op a, i, w, h => by
      simp only [NF.mkSliceW]
      exact slice_id_default_eval h i w
  | .prim2 op a b, i, w, h => by
      simp only [NF.mkSliceW]
      exact slice_id_default_eval h i w
  | .ite c t e, i, w, h => by
      simp only [NF.mkSliceW]
      exact slice_id_default_eval h i w
  | .xcall v x a, i, w, h => by
      simp only [NF.mkSliceW]
      exact slice_id_default_eval h i w

/-- The fused adjacent-slice pair, denotationally. -/
private theorem slice_pair_merge_eval {σ : String → BV} {E : Sem.EEnv} (i₂ w₂ w₁ : Nat) (e₁ : NF)
    (ha : e₁.VarsWF (WP σ)) :
    (NF.mkSliceW i₂ (w₁ + w₂) e₁).eval σ E
      = (NF.cat (NF.slice (i₂ + w₂) w₁ e₁) (NF.slice i₂ w₂ e₁)).eval σ E := by
  rw [mkSliceW_eval e₁ i₂ (w₁ + w₂) ha]
  have hrw : ((NF.cat (NF.slice (i₂ + w₂) w₁ e₁) (NF.slice i₂ w₂ e₁)).eval σ E).width
      = w₁ + w₂ := by
    rw [eval_cat_width, eval_slice_width, eval_slice_width]
  refine bv_eq_of' (w := w₁ + w₂) (eval_slice_width ..) hrw ?_
  intro k hk
  rw [eval_slice_getLsbD, eval_cat_getLsbD' (eval_slice_width σ E i₂ w₂ e₁),
      eval_slice_getLsbD, eval_slice_getLsbD]
  by_cases hkw : k < w₂
  · rw [if_pos hkw]
    simp [hkw, hk]
  · have h4 : k - w₂ < w₁ := by omega
    rw [if_neg hkw]
    simp only [h4, decide_true, Bool.true_and, hk]
    rw [show i₂ + w₂ + (k - w₂) = i₂ + k from by omega]

/-- One `rebuildCat` unfolding, denotationally. -/
private theorem rebuildCat_cons_eval {σ : String → BV} {E : Sem.EEnv} (p : NF) {qs : List NF} (h : qs ≠ []) :
    (NF.rebuildCat (p :: qs)).eval σ E = (NF.cat p (NF.rebuildCat qs)).eval σ E := by
  cases qs with
  | nil => exact absurd rfl h
  | cons q qs2 => rfl

/-- Concatenation denotations reassociate. -/
private theorem cat_assoc_eval {σ : String → BV} {E : Sem.EEnv} (a b c : NF) :
    (NF.cat a (NF.cat b c)).eval σ E = (NF.cat (NF.cat a b) c).eval σ E := by
  refine bv_eq_of ?_ ?_
  · rw [eval_cat_width, eval_cat_width, eval_cat_width, eval_cat_width]
    omega
  · intro k _
    rw [eval_cat_getLsbD' (b := NF.cat b c) (eval_cat_width σ E b c) a k,
        eval_cat_getLsbD' (b := c) rfl b k,
        eval_cat_getLsbD' (b := c) rfl (NF.cat a b) k,
        eval_cat_getLsbD' (b := b) rfl a (k - (c.eval σ E).width)]
    by_cases h1 : k < (c.eval σ E).width
    · have h2 : k < (b.eval σ E).width + (c.eval σ E).width := by omega
      rw [if_pos h2, if_pos h1, if_pos h1]
    · by_cases h2 : k < (b.eval σ E).width + (c.eval σ E).width
      · have h3 : k - (c.eval σ E).width < (b.eval σ E).width := by omega
        rw [if_pos h2, if_neg h1, if_neg h1, if_pos h3]
      · have h3 : ¬ (k - (c.eval σ E).width < (b.eval σ E).width) := by omega
        rw [if_neg h2, if_neg h1, if_neg h3]
        rw [show k - ((b.eval σ E).width + (c.eval σ E).width)
            = k - (c.eval σ E).width - (b.eval σ E).width from by omega]

private theorem catPieces_ne_nil : ∀ (e : NF), NF.catPieces e ≠ [] := by
  intro e
  induction e with
  | cat a b iha ihb =>
      intro hc
      rw [show NF.catPieces (NF.cat a b) = NF.catPieces a ++ NF.catPieces b from rfl] at hc
      exact iha (List.append_eq_nil_iff.mp hc).1
  | var w x => simp [NF.catPieces]
  | lit v => simp [NF.catPieces]
  | prim1 op a iha => simp [NF.catPieces]
  | prim2 op a b iha ihb => simp [NF.catPieces]
  | slice i w e ihe => simp [NF.catPieces]
  | ite c t e ihc iht ihe => simp [NF.catPieces]
  | xcall w x a iha => simp [NF.catPieces]

/-- Rebuilding distributes over appended piece lists. -/
private theorem rebuild_append_eval {σ : String → BV} {E : Sem.EEnv} :
    ∀ (ps qs : List NF), ps ≠ [] → qs ≠ [] →
      (NF.rebuildCat (ps ++ qs)).eval σ E
        = (NF.cat (NF.rebuildCat ps) (NF.rebuildCat qs)).eval σ E := by
  intro ps
  induction ps with
  | nil => intro qs h _; exact absurd rfl h
  | cons p ps2 ih =>
      intro qs _ hqs
      cases ps2 with
      | nil =>
          rw [show ([p] ++ qs) = p :: qs from rfl]
          exact rebuildCat_cons_eval p hqs
      | cons r ps3 =>
          rw [show ((p :: r :: ps3) ++ qs) = p :: ((r :: ps3) ++ qs) from rfl]
          have h1 : (NF.rebuildCat (p :: ((r :: ps3) ++ qs))).eval σ E
              = (NF.cat p (NF.rebuildCat ((r :: ps3) ++ qs))).eval σ E :=
            rebuildCat_cons_eval p (by simp)
          have h2 : (NF.cat p (NF.rebuildCat ((r :: ps3) ++ qs))).eval σ E
              = (NF.cat p (NF.cat (NF.rebuildCat (r :: ps3)) (NF.rebuildCat qs))).eval σ E :=
            cat_eval_congr rfl (ih qs (by simp) hqs)
          have h3 := cat_assoc_eval (σ := σ) (E := E) p (NF.rebuildCat (r :: ps3))
            (NF.rebuildCat qs)
          have h4 : (NF.cat (NF.cat p (NF.rebuildCat (r :: ps3))) (NF.rebuildCat qs)).eval σ E
              = (NF.cat (NF.rebuildCat (p :: r :: ps3)) (NF.rebuildCat qs)).eval σ E :=
            cat_eval_congr (rebuildCat_cons_eval p (by simp)).symm rfl
          exact h1.trans (h2.trans (h3.trans h4))

/-- The rebuilt flattened spine denotes the original term. -/
private theorem rebuild_catPieces_eval {σ : String → BV} {E : Sem.EEnv} :
    ∀ (e : NF), (NF.rebuildCat (NF.catPieces e)).eval σ E = e.eval σ E := by
  intro e
  induction e with
  | cat a b iha ihb =>
      rw [show NF.catPieces (NF.cat a b) = NF.catPieces a ++ NF.catPieces b from rfl]
      rw [rebuild_append_eval _ _ (catPieces_ne_nil a) (catPieces_ne_nil b)]
      exact cat_eval_congr iha ihb
  | var w x => rfl
  | lit v => rfl
  | prim1 op a iha => rfl
  | prim2 op a b iha ihb => rfl
  | slice i w e ihe => rfl
  | ite c t e ihc iht ihe => rfl
  | xcall w x a iha => rfl

private theorem mergePieces_ne_nil : ∀ {ps : List NF}, ps ≠ [] → NF.mergePieces ps ≠ [] := by
  intro ps h
  cases ps with
  | nil => exact absurd rfl h
  | cons p rest =>
      rw [show NF.mergePieces (p :: rest) = NF.mergeStep p (NF.mergePieces rest) from rfl]
      unfold NF.mergeStep
      split
      · simp
      · split <;> simp
      · simp

/-- Merging adjacent pieces preserves the rebuilt denotation. -/
private theorem mergePieces_eval {σ : String → BV} {E : Sem.EEnv} :
    ∀ (ps : List NF), (∀ p ∈ ps, p.VarsWF (WP σ)) →
      (NF.rebuildCat (NF.mergePieces ps)).eval σ E = (NF.rebuildCat ps).eval σ E := by
  intro ps
  induction ps with
  | nil => intro _; rfl
  | cons p rest ih =>
      intro h
      have hhd := h p List.mem_cons_self
      have htl := ih fun r hr => h r (List.mem_cons_of_mem _ hr)
      cases rest with
      | nil => rw [show NF.mergePieces [p] = [p] from by cases p <;> rfl]
      | cons r rest1 =>
          have hMne : NF.mergePieces (r :: rest1) ≠ [] := mergePieces_ne_nil (by simp)
          rw [show NF.mergePieces (p :: r :: rest1)
              = NF.mergeStep p (NF.mergePieces (r :: rest1)) from rfl]
          unfold NF.mergeStep
          split
          · rename_i v u rest2 heq
            have hR : (NF.rebuildCat (NF.lit v :: r :: rest1)).eval σ E
                = (NF.cat (NF.lit v) (NF.rebuildCat (NF.lit u :: rest2))).eval σ E := by
              rw [rebuildCat_cons_eval _ (by simp)]
              exact cat_eval_congr rfl (heq ▸ htl.symm)
            rw [hR]
            cases rest2 with
            | nil => rfl
            | cons s rest3 =>
                have hL : (NF.rebuildCat (NF.lit ⟨v.width + u.width, v.bits ++ u.bits⟩
                    :: s :: rest3)).eval σ E
                    = (NF.cat (NF.lit ⟨v.width + u.width, v.bits ++ u.bits⟩)
                        (NF.rebuildCat (s :: rest3))).eval σ E :=
                  rebuildCat_cons_eval _ (by simp)
                have hR2 : (NF.cat (NF.lit v) (NF.rebuildCat (NF.lit u :: s :: rest3))).eval σ E
                    = (NF.cat (NF.lit v)
                        (NF.cat (NF.lit u) (NF.rebuildCat (s :: rest3)))).eval σ E :=
                  cat_eval_congr rfl (rebuildCat_cons_eval _ (by simp))
                rw [hL, hR2, cat_assoc_eval]
                exact cat_eval_congr
                  (show (NF.lit ⟨v.width + u.width, v.bits ++ u.bits⟩).eval σ E
                      = (NF.cat (NF.lit v) (NF.lit u)).eval σ E from rfl) rfl
          · rename_i i₁ w₁ e₁ i₂ w₂ e₂ rest2 heq
            split
            · rename_i hcond
              obtain ⟨he, hi⟩ := hcond
              subst he
              subst hi
              have hR : (NF.rebuildCat (NF.slice (i₂ + w₂) w₁ e₁ :: r :: rest1)).eval σ E
                  = (NF.cat (NF.slice (i₂ + w₂) w₁ e₁)
                      (NF.rebuildCat (NF.slice i₂ w₂ e₁ :: rest2))).eval σ E := by
                rw [rebuildCat_cons_eval _ (by simp)]
                exact cat_eval_congr rfl (heq ▸ htl.symm)
              rw [hR]
              cases rest2 with
              | nil => exact slice_pair_merge_eval i₂ w₂ w₁ e₁ hhd
              | cons s rest3 =>
                  have hL : (NF.rebuildCat (NF.mkSliceW i₂ (w₁ + w₂) e₁
                      :: s :: rest3)).eval σ E
                      = (NF.cat (NF.mkSliceW i₂ (w₁ + w₂) e₁)
                          (NF.rebuildCat (s :: rest3))).eval σ E :=
                    rebuildCat_cons_eval _ (by simp)
                  have hR2 : (NF.cat (NF.slice (i₂ + w₂) w₁ e₁)
                      (NF.rebuildCat (NF.slice i₂ w₂ e₁ :: s :: rest3))).eval σ E
                      = (NF.cat (NF.slice (i₂ + w₂) w₁ e₁)
                          (NF.cat (NF.slice i₂ w₂ e₁) (NF.rebuildCat (s :: rest3)))).eval σ E :=
                    cat_eval_congr rfl (rebuildCat_cons_eval _ (by simp))
                  rw [hL, hR2, cat_assoc_eval]
                  exact cat_eval_congr (slice_pair_merge_eval i₂ w₂ w₁ e₁ hhd) rfl
            · have hL : (NF.rebuildCat (NF.slice i₁ w₁ e₁
                  :: NF.slice i₂ w₂ e₂ :: rest2)).eval σ E
                  = (NF.cat (NF.slice i₁ w₁ e₁)
                      (NF.rebuildCat (NF.slice i₂ w₂ e₂ :: rest2))).eval σ E :=
                rebuildCat_cons_eval _ (by simp)
              have hR : (NF.rebuildCat (NF.slice i₁ w₁ e₁ :: r :: rest1)).eval σ E
                  = (NF.cat (NF.slice i₁ w₁ e₁) (NF.rebuildCat (r :: rest1))).eval σ E :=
                rebuildCat_cons_eval _ (by simp)
              rw [hL, hR]
              exact cat_eval_congr rfl (heq ▸ htl)
          · rename_i pp q mm ex1 ex2
            have hL : (NF.rebuildCat (pp :: NF.mergePieces (r :: rest1))).eval σ E
                = (NF.cat pp (NF.rebuildCat (NF.mergePieces (r :: rest1)))).eval σ E :=
              rebuildCat_cons_eval _ hMne
            have hR : (NF.rebuildCat (pp :: r :: rest1)).eval σ E
                = (NF.cat pp (NF.rebuildCat (r :: rest1))).eval σ E :=
              rebuildCat_cons_eval _ (by simp)
            rw [hL, hR]
            exact cat_eval_congr rfl htl

theorem mkCatW_eval {σ : String → BV} {E : Sem.EEnv} (a b : NF)
    (ha : a.VarsWF (WP σ)) (hb : b.VarsWF (WP σ)) :
    (NF.mkCatW a b).eval σ E = (NF.cat a b).eval σ E := by
  rw [show NF.mkCatW a b
      = NF.rebuildCat (NF.mergePieces (NF.catPieces a ++ NF.catPieces b)) from rfl]
  rw [mergePieces_eval _ (fun p hp => by
    rcases List.mem_append.mp hp with hp | hp
    · exact catPieces_varsWF ha p hp
    · exact catPieces_varsWF hb p hp)]
  rw [rebuild_append_eval _ _ (catPieces_ne_nil a) (catPieces_ne_nil b)]
  exact cat_eval_congr (rebuild_catPieces_eval a) (rebuild_catPieces_eval b)

theorem mk1W_eval (σ : String → BV) (E : Sem.EEnv) (op : Op) (a : NF) :
    (NF.mk1W op a).eval σ E = (NF.prim1 op a).eval σ E := by
  unfold NF.mk1W
  split
  · split
    · rename_i b
      show b.eval σ E = (⟨(b.eval σ E).width, ~~~(~~~(b.eval σ E).bits)⟩ : BV)
      cases hbe : b.eval σ E with
      | mk bw bbits => simp [BitVec.not_not]
    · exact mk1_eval σ E .not _
  · exact mk1_eval σ E _ _

theorem mk2W_eval (σ : String → BV) (E : Sem.EEnv) (op : Op) (a b : NF)
    (ha : a.VarsWF (WP σ)) (hb : b.VarsWF (WP σ)) :
    (NF.mk2W op a b).eval σ E = (NF.prim2 op a b).eval σ E := by
  unfold NF.mk2W
  split
  · split
    · rename_i u hz
      have hu : u.bits = BitVec.ofNat u.width 0 := by
        have h0 : u.bits.toNat = 0 := hz
        have h1 : (BitVec.ofNat u.width 0).toNat = 0 := by simp
        exact BitVec.eq_of_toNat_eq (by rw [h0, h1])
      show a.eval σ E
          = (⟨(a.eval σ E).width, (a.eval σ E).bits % BitVec.setWidth (a.eval σ E).width u.bits⟩ : BV)
      rw [hu]
      cases hae : a.eval σ E with
      | mk aw abits => simp [BitVec.umod_zero]
    · exact mk2_eval σ E .umod _ _
  · split
    · rename_i hcond
      obtain ⟨hann, huv⟩ := hcond
      subst huv
      have hw : (a.eval σ E).width = 1 := annWidth_eval ha hann
      show a.eval σ E = Sem.b1 ((a.eval σ E).bits == BitVec.setWidth (a.eval σ E).width (1#1))
      rcases bv1_cases _ hw with hc | hc <;> rw [hc] <;> first | rfl | decide
    · split
      · rename_i hcond
        obtain ⟨hann, huv⟩ := hcond
        subst huv
        have hw : (a.eval σ E).width = 1 := annWidth_eval ha hann
        rw [mk1W_eval]
        show (⟨(a.eval σ E).width, ~~~(a.eval σ E).bits⟩ : BV)
            = Sem.b1 ((a.eval σ E).bits == BitVec.setWidth (a.eval σ E).width (0#1))
        rcases bv1_cases _ hw with hc | hc <;> rw [hc] <;> first | rfl | decide
      · exact mk2_eval σ E .eq _ _
  · split
    · rename_i hcond
      obtain ⟨hann, huv⟩ := hcond
      subst huv
      have hw : (b.eval σ E).width = 1 := annWidth_eval hb hann
      show b.eval σ E = Sem.b1 ((1#1 : BitVec 1) == BitVec.setWidth 1 (b.eval σ E).bits)
      rcases bv1_cases _ hw with hc | hc <;> rw [hc] <;> first | rfl | decide
    · split
      · rename_i hcond
        obtain ⟨hann, huv⟩ := hcond
        subst huv
        have hw : (b.eval σ E).width = 1 := annWidth_eval hb hann
        rw [mk1W_eval]
        show (⟨(b.eval σ E).width, ~~~(b.eval σ E).bits⟩ : BV)
            = Sem.b1 ((0#1 : BitVec 1) == BitVec.setWidth 1 (b.eval σ E).bits)
        rcases bv1_cases _ hw with hc | hc <;> rw [hc] <;> first | rfl | decide
      · exact mk2_eval σ E .eq _ _
  · exact mk2_eval σ E _ _ _

theorem mkIteW_eval {σ : String → BV} {E : Sem.EEnv} (c t e : NF) (hc : c.VarsWF (WP σ)) :
    (NF.mkIteW c t e).eval σ E = (NF.ite c t e).eval σ E := by
  unfold NF.mkIteW
  split
  · rename_i v u
    split
    · rename_i hcond
      obtain ⟨hv1, hu1, hvn, hun, hann⟩ := hcond
      have hw : (c.eval σ E).width = 1 := annWidth_eval hc hann
      have hv : v = ⟨1, 1#1⟩ := by
        rcases bv1_cases v hv1 with h | h
        · rw [h] at hvn; exact absurd hvn (by simp [BV.nat])
        · exact h
      have hu : u = ⟨1, 0#1⟩ := by
        rcases bv1_cases u hu1 with h | h
        · exact h
        · rw [h] at hun; exact absurd hun (by simp [BV.nat])
      subst hv
      subst hu
      rcases bv1_cases (c.eval σ E) hw with h | h
      · rw [show (NF.ite c (NF.lit ⟨1, 1#1⟩) (NF.lit ⟨1, 0#1⟩)).eval σ E
            = if (c.eval σ E).nat ≠ 0 then (⟨1, 1#1⟩ : BV) else ⟨1, 0#1⟩ from rfl, h]
        simp [BV.nat]
      · rw [show (NF.ite c (NF.lit ⟨1, 1#1⟩) (NF.lit ⟨1, 0#1⟩)).eval σ E
            = if (c.eval σ E).nat ≠ 0 then (⟨1, 1#1⟩ : BV) else ⟨1, 0#1⟩ from rfl, h]
        simp [BV.nat]
    · split
      · rename_i hcond
        obtain ⟨hv1, hu1, hvn, hun, hann⟩ := hcond
        have hw : (c.eval σ E).width = 1 := annWidth_eval hc hann
        have hv : v = ⟨1, 0#1⟩ := by
          rcases bv1_cases v hv1 with h | h
          · exact h
          · rw [h] at hvn; exact absurd hvn (by simp [BV.nat])
        have hu : u = ⟨1, 1#1⟩ := by
          rcases bv1_cases u hu1 with h | h
          · rw [h] at hun; exact absurd hun (by simp [BV.nat])
          · exact h
        subst hv
        subst hu
        rw [mk1W_eval]
        rw [show (NF.prim1 .not c).eval σ E
            = (⟨(c.eval σ E).width, ~~~(c.eval σ E).bits⟩ : BV) from rfl]
        rcases bv1_cases (c.eval σ E) hw with h | h
        · rw [show (NF.ite c (NF.lit ⟨1, 0#1⟩) (NF.lit ⟨1, 1#1⟩)).eval σ E
              = if (c.eval σ E).nat ≠ 0 then (⟨1, 0#1⟩ : BV) else ⟨1, 1#1⟩ from rfl, h]
          simp [BV.nat]
        · rw [show (NF.ite c (NF.lit ⟨1, 0#1⟩) (NF.lit ⟨1, 1#1⟩)).eval σ E
              = if (c.eval σ E).nat ≠ 0 then (⟨1, 0#1⟩ : BV) else ⟨1, 1#1⟩ from rfl, h]
          simp [BV.nat]
      · exact mkIte_eval σ E c _ _
  · exact mkIte_eval σ E _ _ _

/-- The width-aware normalizer is denotation-preserving at any
valuation respecting the term's variable annotations. -/
theorem cfoldW_eval {σ : String → BV} {E : Sem.EEnv} :
    ∀ {nf : NF}, nf.VarsWF (WP σ) → (nf.cfoldW).eval σ E = nf.eval σ E := by
  intro nf
  induction nf with
  | var w x => intro _; rfl
  | lit v => intro _; rfl
  | prim1 op a iha =>
      intro h
      simp only [NF.cfoldW]
      rw [mk1W_eval]
      simp only [NF.eval]
      rw [iha h]
  | prim2 op a b iha ihb =>
      intro h
      simp only [NF.cfoldW]
      rw [mk2W_eval _ _ _ _ _ (cfoldW_varsWF h.1) (cfoldW_varsWF h.2)]
      simp only [NF.eval]
      rw [iha h.1, ihb h.2]
  | cat a b iha ihb =>
      intro h
      simp only [NF.cfoldW]
      rw [mkCatW_eval _ _ (cfoldW_varsWF h.1) (cfoldW_varsWF h.2)]
      simp only [NF.eval]
      rw [iha h.1, ihb h.2]
  | slice i w e ihe =>
      intro h
      simp only [NF.cfoldW]
      rw [mkSliceW_eval e.cfoldW i w (cfoldW_varsWF h)]
      simp only [NF.eval]
      rw [ihe h]
  | ite c t e ihc iht ihe =>
      intro h
      simp only [NF.cfoldW]
      rw [mkIteW_eval _ _ _ (cfoldW_varsWF h.1)]
      simp only [NF.eval]
      rw [ihc h.1, iht h.2.1, ihe h.2.2]
  | xcall w x a iha =>
      intro h
      simp only [NF.cfoldW, NF.eval, iha h]

/-! ## The width-aware checker and its soundness

`checkEquivW` strengthens `checkEquiv` with the width-relocating
rewrite set (`cfoldW`, iterated). Its soundness statement carries the
honest side condition those rewrites demand: the stimulus drives every
input at its declared width (`StimWF`). The unconditional theorem
(`checkEquiv_sound`) is unaffected — width-relocating rewrites are
NOT denotation-preserving at undeclared widths, so this weakening is
forced, not chosen. -/

/-- Three rounds of the width-aware normalizer (a bottom-up pass can
enable parent rewrites; iterating is soundness-free). -/
def cfoldW3 (nf : NF) : NF := nf.cfoldW.cfoldW.cfoldW

theorem cfoldW3_varsWF {P : String → Nat → Prop} {nf : NF} (h : nf.VarsWF P) :
    (cfoldW3 nf).VarsWF P := cfoldW_varsWF (cfoldW_varsWF (cfoldW_varsWF h))

theorem cfoldW3_eval {σ : String → BV} {E : Sem.EEnv} {nf : NF} (h : nf.VarsWF (WP σ)) :
    (cfoldW3 nf).eval σ E = nf.eval σ E := by
  unfold cfoldW3
  rw [cfoldW_eval (cfoldW_varsWF (cfoldW_varsWF h)), cfoldW_eval (cfoldW_varsWF h),
      cfoldW_eval h]

/-- Width-aware constant-folded pairs. -/
def cfoldPairsW (l : List (String × NF)) : List (String × NF) :=
  l.map fun p => (p.1, cfoldW3 p.2)

/-- The stimulus drives the declared input widths, every cycle. -/
def StimWF (dev : Device) (stim : List (List BV)) : Prop :=
  ∀ ins ∈ stim, ins.map (·.width) = dev.inputs.map Prod.snd

/-- The register-store invariant of the width-aware run: every
declared register holds a value of its declared width, and nothing
else is in the store. -/
def RegWF (dev : Device) (regs : HashMap String BV) : Prop :=
  (∀ r ∈ dev.registers, ∃ v, regs.get? r.name = some v ∧ v.width = r.width) ∧
  (∀ x, regs.contains x = true → x ∈ dev.registers.map (·.name))

/-! ### Lookup machinery at distinct keys -/

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
        rw [show l.reverse.findSome? (fun p => if p.1 == k then some p.2 else none) = none from by
              refine findSome?_sel_none ?_
              simpa using hknotl]
        simp only [Option.none_or, List.findSome?_cons]
        rw [← hmem]
        simp
      · rw [ih hnd'.2 hmem]
        rfl

/-- Positional width transport through the input zip. -/
private theorem zip_width_mem :
    ∀ {inputs : List (String × Nat)} {ins : List BV} {x : String} {w : Nat},
      ins.map (·.width) = inputs.map Prod.snd → (x, w) ∈ inputs →
      ∃ v, (x, v) ∈ (inputs.map Prod.fst).zip ins ∧ v.width = w := by
  intro inputs
  induction inputs with
  | nil => intro ins x w _ h; exact absurd h (by simp)
  | cons p rest ih =>
      intro ins x w hstim hin
      obtain ⟨py, pw⟩ := p
      cases ins with
      | nil => exact absurd hstim (by simp)
      | cons v ins' =>
          simp only [List.map_cons, List.cons.injEq] at hstim
          rcases List.mem_cons.mp hin with hin | hin
          · injection hin with h1 h2
            subst h1
            subst h2
            refine ⟨v, ?_, ?_⟩
            · rw [List.map_cons, List.zip_cons_cons]
              exact List.mem_cons_self
            · exact hstim.1
          · obtain ⟨u, hu, huw⟩ := ih hstim.2 hin
            exact ⟨u, by rw [List.map_cons, List.zip_cons_cons]; exact List.mem_cons_of_mem _ hu, huw⟩

/-- The valuation induced by a width-respecting store and stimulus
respects every declared input and register width. -/
private theorem sigma_wp {dev : Device} {regs : HashMap String BV} {ins : List BV}
    (hnd : (dev.inputs.map Prod.fst ++ dev.registers.map (·.name)).Nodup)
    (hwf : RegWF dev regs)
    (hstim : ins.map (·.width) = dev.inputs.map Prod.snd) :
    ∀ x w, (x, w) ∈ dev.inputs ++ dev.registers.map (fun r => (r.name, r.width)) →
      WP (sigmaOf dev.inputs regs ins) x w := by
  intro x w hxw
  have hunion : (stepEnv dev.inputs regs ins).get? x
      = (regs.get? x).or ((HashMap.ofList ((dev.inputs.map Prod.fst).zip ins)).get? x) := by
    rw [stepEnv, show ∀ (a b : HashMap String BV), a.union b = a ∪ b from fun _ _ => rfl]
    rw [HashMap.get?_eq_getElem?, HashMap.getElem?_union]
    rw [HashMap.get?_eq_getElem?, HashMap.get?_eq_getElem?]
  rcases List.mem_append.mp hxw with hin | hrg
  · -- a device input; by distinctness it is not a register
    have hxname : x ∈ dev.inputs.map Prod.fst := List.mem_map.mpr ⟨(x, w), hin, rfl⟩
    have hnotreg : x ∉ dev.registers.map (·.name) := by
      have hdisj := List.nodup_append.mp hnd
      exact fun hc => hdisj.2.2 x hxname x hc rfl
    have hregnone : regs.get? x = none := by
      cases hc : regs.contains x with
      | true => exact absurd (hwf.2 x hc) hnotreg
      | false =>
          have := HashMap.contains_eq_isSome_getElem? (m := regs) (a := x)
          rw [hc] at this
          rw [HashMap.get?_eq_getElem?]
          exact Option.not_isSome_iff_eq_none.mp (by rw [← this]; simp)
    obtain ⟨v, hv, hvw⟩ := zip_width_mem hstim hin
    have hnzip : (((dev.inputs.map Prod.fst).zip ins).map Prod.fst).Nodup := by
      rw [List.map_fst_zip (by
        have := congrArg List.length hstim
        simp only [List.length_map] at this
        simp [this])]
      exact (List.nodup_append.mp hnd).1
    have hget := ofList_get?_of_nodup hnzip hv
    show (sigmaOf dev.inputs regs ins x).width = w
    rw [sigmaOf, hunion, hregnone, Option.none_or, hget, Option.getD_some, hvw]
  · obtain ⟨r, hr, hxr⟩ := List.mem_map.mp hrg
    have hx : r.name = x := congrArg Prod.fst hxr
    have hw : r.width = w := congrArg Prod.snd hxr
    obtain ⟨v, hv, hvw⟩ := hwf.1 r hr
    show (sigmaOf dev.inputs regs ins x).width = w
    rw [sigmaOf, hunion, ← hx, hv]
    show (((some v).or ((HashMap.ofList ((dev.inputs.map Prod.fst).zip ins)).get? r.name)).getD
      BV.nil).width = w
    rw [show ((some v).or ((HashMap.ofList ((dev.inputs.map Prod.fst).zip ins)).get? r.name))
          = some v from rfl,
        Option.getD_some, hvw, hw]

/-! ### Store lemmas for the next-state fold -/

private theorem foldl_insert_get?_not_mem {σ : String → BV} {E : Sem.EEnv} :
    ∀ (l : List (String × NF)) (m : HashMap String BV) (x : String),
      x ∉ l.map Prod.fst →
      (l.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) m).get? x = m.get? x := by
  intro l
  induction l with
  | nil => intro _ _ _; rfl
  | cons p l ih =>
      intro m x h
      simp only [List.map_cons, List.mem_cons, not_or] at h
      rw [List.foldl_cons, ih _ x h.2, HashMap.get?_eq_getElem?, HashMap.getElem?_insert,
          if_neg (by simp only [beq_iff_eq]; exact fun hc => h.1 hc.symm)]
      rfl

private theorem foldl_insert_get?_of_nodup {σ : String → BV} {E : Sem.EEnv} :
    ∀ (l : List (String × NF)) (m : HashMap String BV) (x : String) (nf : NF),
      (l.map Prod.fst).Nodup → (x, nf) ∈ l →
      (l.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) m).get? x = some (nf.eval σ E) := by
  intro l
  induction l with
  | nil => intro _ _ _ _ hmem; exact absurd hmem (by simp)
  | cons p l ih =>
      intro m x nf hnd hmem
      simp only [List.map_cons] at hnd
      have hnd' := List.nodup_cons.mp hnd
      rw [List.foldl_cons]
      rcases List.mem_cons.mp hmem with hmem | hmem
      · have hxnot : x ∉ l.map Prod.fst := by rw [← hmem] at hnd'; exact hnd'.1
        rw [foldl_insert_get?_not_mem l _ x hxnot, ← hmem, HashMap.get?_eq_getElem?]
        exact HashMap.getElem?_insert_self
      · exact ih _ x nf hnd'.2 hmem

private theorem foldl_insert_contains_only {σ : String → BV} {E : Sem.EEnv} :
    ∀ (l : List (String × NF)) (m : HashMap String BV) (x : String),
      (l.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) m).contains x = true →
      x ∈ l.map Prod.fst ∨ m.contains x = true := by
  intro l
  induction l with
  | nil => intro _ _ h; exact .inr h
  | cons p l ih =>
      intro m x h
      rw [List.foldl_cons] at h
      rcases ih _ x h with h | h
      · exact .inl (List.mem_cons_of_mem _ h)
      · rw [HashMap.contains_insert] at h
        rcases Bool.or_eq_true_iff.mp h with h | h
        · have hpx : p.fst = x := by simpa using h
          exact .inl (by simp only [List.map_cons, List.mem_cons]; exact .inl hpx.symm)
        · exact .inr h

private theorem mem_zip_of_left {α β : Type} :
    ∀ {rs : List α} {ps : List β} {r : α}, rs.length = ps.length → r ∈ rs →
      ∃ p, (r, p) ∈ rs.zip ps := by
  intro rs
  induction rs with
  | nil => intro ps r _ h; exact absurd h (by simp)
  | cons a rs ih =>
      intro ps r hlen hr
      cases ps with
      | nil => simp at hlen
      | cons b ps =>
          rcases List.mem_cons.mp hr with hr | hr
          · exact ⟨b, by rw [List.zip_cons_cons, hr]; exact List.mem_cons_self⟩
          · obtain ⟨p, hp⟩ := ih (by simpa using hlen) hr
            exact ⟨p, by rw [List.zip_cons_cons]; exact List.mem_cons_of_mem _ hp⟩

/-! ### The width-aware checker -/

/-- The register-next width discipline: positionally, every symbolic
next has the annotated width of its register (checked on the
normalized form). -/
def nextsWidthsOkB (dev : Device) (ss : StepNF) : Bool :=
  (dev.registers.zip ss.nexts).all fun rp =>
    rp.2.1 == rp.1.name && annWidth (cfoldW3 rp.2.2) == some rp.1.width

/-- The width-aware equivalence checker: `checkEquiv`'s conditions,
plus input/register name distinctness, register initials at declared
widths, register nexts at declared annotated widths — and the
comparison after the width-aware rewrite set. -/
def checkEquivW (p₁ p₂ : Program) : Bool :=
  match Sem.mkFEnv p₁, Sem.mkFEnv p₂,
        symStep (dmapOf p₁) (Sem.xenv p₁) (progFuel p₁) p₁.device,
        symStep (dmapOf p₂) (Sem.xenv p₂) (progFuel p₂) p₂.device with
  | .ok _, .ok _, .ok s₁, .ok s₂ =>
         okB p₁.check && okB p₂.check
      && p₁.externs.isEmpty && p₂.externs.isEmpty
      && p₁.device.instances.isEmpty && p₂.device.instances.isEmpty
      && nodupB (p₁.defns.map (·.name)) && nodupB (p₂.defns.map (·.name))
      && nodupB (p₁.device.inputs.map Prod.fst ++ p₁.device.registers.map (·.name))
      && p₁.device.registers.all (fun r => r.init.width == r.width)
      && nextsWidthsOkB p₁.device s₁
      && decide (p₁.device.inputs = p₂.device.inputs)
      && decide (p₁.device.outputs = p₂.device.outputs)
      && decide (regTuples p₁.device.registers = regTuples p₂.device.registers)
      && decide (cfoldPairsW s₁.outs = cfoldPairsW s₂.outs)
      && decide (cfoldPairsW s₁.nexts = cfoldPairsW s₂.nexts)
  | _, _, _, _ => false

/-! ### Transport of normalized equality to denotations -/

private theorem outsVal_eq_of_cfoldW {σ : String → BV} {E : Sem.EEnv} :
    ∀ {l₁ l₂ : List (String × NF)},
      cfoldPairsW l₁ = cfoldPairsW l₂ →
      (∀ p ∈ l₁, p.2.VarsWF (WP σ)) → (∀ p ∈ l₂, p.2.VarsWF (WP σ)) →
      (l₁.map fun p => p.2.eval σ E) = l₂.map fun p => p.2.eval σ E := by
  intro l₁
  induction l₁ with
  | nil =>
      intro l₂ h _ _
      cases l₂ with
      | nil => rfl
      | cons _ _ => exact absurd h (by simp [cfoldPairsW])
  | cons p l₁ ih =>
      intro l₂ h hwf₁ hwf₂
      cases l₂ with
      | nil => exact absurd h (by simp [cfoldPairsW])
      | cons q l₂ =>
          simp only [cfoldPairsW, List.map_cons, List.cons.injEq] at h
          rw [List.map_cons, List.map_cons]
          have h1 := h.1
          rw [Prod.mk.injEq] at h1
          have hpq : p.2.eval σ E = q.2.eval σ E := by
            rw [← cfoldW3_eval (hwf₁ p List.mem_cons_self),
                ← cfoldW3_eval (hwf₂ q List.mem_cons_self)]
            rw [h1.2]
          rw [hpq, ih (show cfoldPairsW l₁ = cfoldPairsW l₂ from h.2)
                (fun r hr => hwf₁ r (List.mem_cons_of_mem _ hr))
                (fun r hr => hwf₂ r (List.mem_cons_of_mem _ hr))]

private theorem nextsVal_eq_of_cfoldW {σ : String → BV} {E : Sem.EEnv} :
    ∀ {l₁ l₂ : List (String × NF)},
      cfoldPairsW l₁ = cfoldPairsW l₂ →
      (∀ p ∈ l₁, p.2.VarsWF (WP σ)) → (∀ p ∈ l₂, p.2.VarsWF (WP σ)) →
      ∀ (m : HashMap String BV),
        l₁.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) m
          = l₂.foldl (fun m p => m.insert p.1 (p.2.eval σ E)) m := by
  intro l₁
  induction l₁ with
  | nil =>
      intro l₂ h _ _ m
      cases l₂ with
      | nil => rfl
      | cons _ _ => exact absurd h (by simp [cfoldPairsW])
  | cons p l₁ ih =>
      intro l₂ h hwf₁ hwf₂ m
      cases l₂ with
      | nil => exact absurd h (by simp [cfoldPairsW])
      | cons q l₂ =>
          simp only [cfoldPairsW, List.map_cons, List.cons.injEq] at h
          have h1 := h.1
          rw [Prod.mk.injEq] at h1
          have hpq : p.2.eval σ E = q.2.eval σ E := by
            rw [← cfoldW3_eval (hwf₁ p List.mem_cons_self),
                ← cfoldW3_eval (hwf₂ q List.mem_cons_self)]
            rw [h1.2]
          rw [List.foldl_cons, List.foldl_cons, h1.1, hpq]
          exact ih (show cfoldPairsW l₁ = cfoldPairsW l₂ from h.2)
            (fun r hr => hwf₁ r (List.mem_cons_of_mem _ hr))
            (fun r hr => hwf₂ r (List.mem_cons_of_mem _ hr)) _

/-- The width-aware checker is sound on declared-width stimuli: a
`true` verdict gives run equality on every stimulus that drives the
device's inputs at their declared widths. (The side condition is
forced: the width-relocating rewrites — slice-of-concatenation,
identity slices, the 1-bit peepholes — are not denotation-preserving
at undeclared widths, so the unconditional statement is false for the
strengthened normalizer. `checkEquiv_sound` remains the unconditional
statement for the weaker normalizer.) -/
theorem checkEquivW_sound {p₁ p₂ : Program} (h : checkEquivW p₁ p₂ = true) :
    ∀ stim, StimWF p₁.device stim → ∀ (E : Sem.EEnv), p₁.run stim E = p₂.run stim E := by
  intro stim hstim E
  rw [checkEquivW] at h
  cases hF₁ : Sem.mkFEnv p₁ with
  | error e => rw [hF₁] at h; exact absurd h (by simp)
  | ok F₁ =>
  cases hF₂ : Sem.mkFEnv p₂ with
  | error e => rw [hF₁, hF₂] at h; exact absurd h (by simp)
  | ok F₂ =>
  cases hs₁ : symStep (dmapOf p₁) (Sem.xenv p₁) (progFuel p₁) p₁.device with
  | error e => rw [hF₁, hF₂, hs₁] at h; exact absurd h (by simp)
  | ok s₁ =>
  cases hs₂ : symStep (dmapOf p₂) (Sem.xenv p₂) (progFuel p₂) p₂.device with
  | error e => rw [hF₁, hF₂, hs₁, hs₂] at h; exact absurd h (by simp)
  | ok s₂ =>
  rw [hF₁, hF₂, hs₁, hs₂] at h
  dsimp only at h
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  obtain ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨_, _⟩, _⟩, _⟩, _⟩, _⟩, hnd₁⟩, hnd₂⟩, hndIR⟩, hinitw⟩, hnw⟩,
    hIn⟩, hOut⟩, hReg⟩, hOuts⟩, hNexts⟩ := h
  have hRegs : p₁.device.registers = p₂.device.registers := registers_eq_of_regTuples hReg
  have hndIR' := nodupB_nodup hndIR
  have hndR : (p₁.device.registers.map (·.name)).Nodup := (List.nodup_append.mp hndIR').2.1
  obtain ⟨F₁E, hF₁E⟩ := mkFEnv_ok_any E hF₁
  obtain ⟨F₂E, hF₂E⟩ := mkFEnv_ok_any E hF₂
  have hImpl₁ : FImplements (dmapOf p₁) (Sem.xenv p₁) F₁E E :=
    mkFEnv_implements (nodupB_nodup hnd₁) hF₁E
  have hImpl₂ : FImplements (dmapOf p₂) (Sem.xenv p₂) F₂E E :=
    mkFEnv_implements (nodupB_nodup hnd₂) hF₂E
  -- the per-pair discipline facts
  have hVars₁ : ∀ p ∈ s₁.outs ++ s₁.nexts,
      p.2.VarsWF (fun x w =>
        (x, w) ∈ p₁.device.inputs ++ p₁.device.registers.map fun r => (r.name, r.width)) :=
    symStep_varsWF hs₁
  have hVars₂ : ∀ p ∈ s₂.outs ++ s₂.nexts,
      p.2.VarsWF (fun x w =>
        (x, w) ∈ p₁.device.inputs ++ p₁.device.registers.map fun r => (r.name, r.width)) := by
    intro p hp
    have := symStep_varsWF hs₂ p hp
    rw [← hIn, ← hRegs] at this
    exact this
  have hkeys₁ : s₁.nexts.map Prod.fst = p₁.device.registers.map (·.name) :=
    symStep_nexts_fst hs₁
  have hlen₁ : p₁.device.registers.length = s₁.nexts.length := by
    have := congrArg List.length hkeys₁
    simp only [List.length_map] at this
    exact this.symm
  -- the zip-checked next widths
  have hnw' : ∀ rp ∈ p₁.device.registers.zip s₁.nexts,
      rp.2.1 = rp.1.name ∧ annWidth (cfoldW3 rp.2.2) = some rp.1.width := by
    intro rp hrp
    have hthis := List.all_eq_true.mp hnw rp hrp
    simp only [Bool.and_eq_true] at hthis
    exact ⟨by simpa using hthis.1, by simpa using hthis.2⟩
  -- step equality on width-respecting states and stimuli
  have hstep : ∀ regs ins, RegWF p₁.device regs →
      ins.map (·.width) = p₁.device.inputs.map Prod.snd →
      Sem.step F₁E (Sem.xenv p₁) p₁.device regs ins E
        = Sem.step F₂E (Sem.xenv p₂) p₂.device regs ins E := by
    intro regs ins hwf hok
    have hlen : ins.length = p₁.device.inputs.length := by
      have := congrArg List.length hok
      simpa using this
    have hdom : ∀ r ∈ p₁.device.registers, regs.contains r.name = true := by
      intro r hr
      obtain ⟨v, hv, _⟩ := hwf.1 r hr
      rw [HashMap.contains_eq_isSome_getElem?, ← HashMap.get?_eq_getElem?, hv]
      rfl
    rw [symStep_sound hImpl₁ hs₁ regs ins hlen hdom,
        symStep_sound hImpl₂ hs₂ regs ins (by rw [← hIn]; exact hlen)
          (by rw [← hRegs]; exact hdom)]
    have hσ : sigmaOf p₂.device.inputs regs ins = sigmaOf p₁.device.inputs regs ins := by
      rw [hIn]
    rw [hσ]
    have hwp := sigma_wp hndIR' hwf hok
    have hVarsWP₁ : ∀ p ∈ s₁.outs ++ s₁.nexts,
        p.2.VarsWF (WP (sigmaOf p₁.device.inputs regs ins)) :=
      fun p hp => (hVars₁ p hp).mono (fun x w hx => hwp x w hx)
    have hVarsWP₂ : ∀ p ∈ s₂.outs ++ s₂.nexts,
        p.2.VarsWF (WP (sigmaOf p₁.device.inputs regs ins)) :=
      fun p hp => (hVars₂ p hp).mono (fun x w hx => hwp x w hx)
    have ho : stepOutsVal (sigmaOf p₁.device.inputs regs ins) s₁ E
        = stepOutsVal (sigmaOf p₁.device.inputs regs ins) s₂ E :=
      outsVal_eq_of_cfoldW hOuts
        (fun p hp => hVarsWP₁ p (List.mem_append.mpr (.inl hp)))
        (fun p hp => hVarsWP₂ p (List.mem_append.mpr (.inl hp)))
    have hn : stepNextsVal (sigmaOf p₁.device.inputs regs ins) s₁ E
        = stepNextsVal (sigmaOf p₁.device.inputs regs ins) s₂ E :=
      nextsVal_eq_of_cfoldW hNexts
        (fun p hp => hVarsWP₁ p (List.mem_append.mpr (.inr hp)))
        (fun p hp => hVarsWP₂ p (List.mem_append.mpr (.inr hp))) _
    rw [ho, hn]
  -- preservation of the width-respecting store
  have hpres : ∀ regs ins outs regs', RegWF p₁.device regs →
      ins.map (·.width) = p₁.device.inputs.map Prod.snd →
      Sem.step F₁E (Sem.xenv p₁) p₁.device regs ins E = .ok (outs, regs') →
      RegWF p₁.device regs' := by
    intro regs ins outs regs' hwf hok hstep₁
    have hlen : ins.length = p₁.device.inputs.length := by
      have := congrArg List.length hok
      simpa using this
    have hdom : ∀ r ∈ p₁.device.registers, regs.contains r.name = true := by
      intro r hr
      obtain ⟨v, hv, _⟩ := hwf.1 r hr
      rw [HashMap.contains_eq_isSome_getElem?, ← HashMap.get?_eq_getElem?, hv]
      rfl
    rw [symStep_sound hImpl₁ hs₁ regs ins hlen hdom] at hstep₁
    injection hstep₁ with hstep₁
    have hregs' : regs' = stepNextsVal (sigmaOf p₁.device.inputs regs ins) s₁ E :=
      (congrArg Prod.snd hstep₁).symm
    subst hregs'
    have hwp := sigma_wp hndIR' hwf hok
    have hkeysnd : (s₁.nexts.map Prod.fst).Nodup := by rw [hkeys₁]; exact hndR
    constructor
    · intro r hr
      obtain ⟨pr, hpr⟩ := mem_zip_of_left hlen₁ hr
      obtain ⟨hprn, hprw⟩ := hnw' (r, pr) hpr
      have hprmem : pr ∈ s₁.nexts := (List.of_mem_zip hpr).2
      have hprWF : pr.2.VarsWF (WP (sigmaOf p₁.device.inputs regs ins)) :=
        (hVars₁ pr (List.mem_append.mpr (.inr hprmem))).mono (fun x w hx => hwp x w hx)
      refine ⟨pr.2.eval (sigmaOf p₁.device.inputs regs ins) E, ?_, ?_⟩
      · rw [stepNextsVal, ← hprn]
        exact foldl_insert_get?_of_nodup s₁.nexts ∅ pr.1 pr.2 hkeysnd
          (by rw [show (pr.1, pr.2) = pr from rfl]; exact hprmem)
      · rw [← cfoldW3_eval hprWF]
        exact annWidth_eval (cfoldW3_varsWF hprWF) hprw
    · intro x hx
      rcases foldl_insert_contains_only s₁.nexts ∅ x hx with hmem | hmem
      · rw [hkeys₁] at hmem
        exact hmem
      · rw [HashMap.contains_empty] at hmem
        exact absurd hmem (by simp)
  -- initial store facts
  have hInit : Sem.initRegs p₁.device = Sem.initRegs p₂.device := by
    rw [Sem.initRegs, Sem.initRegs, hRegs]
  have hInitWF : RegWF p₁.device (Sem.initRegs p₁.device) := by
    constructor
    · intro r hr
      refine ⟨r.init, ?_, ?_⟩
      · rw [Sem.initRegs]
        refine ofList_get?_of_nodup ?_ (List.mem_map.mpr ⟨r, hr, rfl⟩)
        rw [show (p₁.device.registers.map fun r => (r.name, r.init)).map Prod.fst
            = p₁.device.registers.map (·.name) from by rw [List.map_map]; rfl]
        exact hndR
      · have := List.all_eq_true.mp hinitw r hr
        simpa using this
    · intro x hx
      rw [Sem.initRegs, HashMap.contains_ofList] at hx
      have : x ∈ (p₁.device.registers.map fun r => (r.name, r.init)).map Prod.fst := by
        simpa using hx
      rw [List.map_map] at this
      exact this
  -- assemble
  have hfold : stim.foldlM (Sem.foldStep F₁E (Sem.xenv p₁) p₁.device E)
        (Sem.initRegs p₁.device, [])
      = stim.foldlM (Sem.foldStep F₂E (Sem.xenv p₂) p₂.device E)
        (Sem.initRegs p₂.device, []) := by
    rw [← hInit]
    exact run_fold_congr (Inv := RegWF p₁.device)
      (Ok := fun ins => ins.map (·.width) = p₁.device.inputs.map Prod.snd)
      hstep hpres stim (Sem.initRegs p₁.device) [] hInitWF hstim
  show Program.run p₁ stim E = Program.run p₂ stim E
  rw [Program.run, Program.run, hF₁E, hF₂E, except_bind_ok, except_bind_ok,
      Sem.run, Sem.run, hfold]

end Rwv.Hyle.Bridge

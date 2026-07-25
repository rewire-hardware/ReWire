/-
The per-label step-obligation schema and its glue theorem — the final
M1 composition. `SimP` refines the abstract simulation layer (Rwv.Sim)
to the concrete correspondence shape: the two machines have different
output types related by an output relation Q (the representation
function's graph) rather than equality, and the right machine may run
past the left machine's halt (the §7.5.4 prefix reading). Composed
with the induced-machine run equivalences (Rwv.Correspond2), a `SimP`
between the Eidos-M machine and the input-precomposed Hyle device —
`StepObligations`, the package a validator discharges per label — plus
the initial-state obligation concludes the top-level `Corresponds`
statement (Rwv.Correspond, doc/eidos.md §7.5.6).
-/
import Rwv.Correspond
import Rwv.Correspond2

universe u v

/-- Pointwise lifting of a relation to a pair of lists (Mathlib's
`List.Forall₂`, absent from the toolchain library; same name and
constructors, so the two definitions coincide if a library providing
it is ever added). -/
inductive List.Forall₂ {α : Type u} {β : Type v} (R : α → β → Prop) :
    List α → List β → Prop where
  | nil : Forall₂ R [] []
  | cons {a : α} {b : β} {l₁ : List α} {l₂ : List β} :
      R a b → Forall₂ R l₁ l₂ → Forall₂ R (a :: l₁) (b :: l₂)

/-- Pointwise-related lists have equal lengths. -/
theorem List.Forall₂.length_eq {α : Type u} {β : Type v} {R : α → β → Prop} :
    ∀ {l₁ : List α} {l₂ : List β}, List.Forall₂ R l₁ l₂ → l₁.length = l₂.length := by
  intro l₁ l₂ h
  induction h with
  | nil => rfl
  | cons _ _ ih => simp [List.length_cons, ih]

namespace Rwv

open Std (HashMap)

/-! ## Local `Except` helpers

Re-proved here: Correspond2's identical helpers are private to that
file. -/

/-- `pure` on `Except` is `.ok`. -/
private theorem except_pure_def {α : Type} (a : α) :
    (pure a : Except String α) = .ok a := rfl

/-- `bind` on `Except` propagates errors. -/
private theorem except_bind_error {α β : Type} (e : String) (f : α → Except String β) :
    ((Except.error e : Except String α) >>= f) = .error e := rfl

/-- `bind` on `Except` applies the continuation to a success. -/
private theorem except_bind_ok {α β : Type} (a : α) (f : α → Except String β) :
    (Except.ok a >>= f) = f a := rfl

/-- Inversion for a successful `Except` bind: the first computation
succeeded and the continuation took its value to the result. -/
private theorem except_bind_eq_ok {α β : Type} {x : Except String α}
    {f : α → Except String β} {b : β} (h : (x >>= f) = .ok b) :
    ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x with
  | error e => exact absurd h (by simp [except_bind_error])
  | ok a => exact ⟨a, rfl, h⟩

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

/-- Pointwise success of an `Except`-valued function along two lists is
a successful `mapM`: the bridge from `simP_run`'s `Forall₂` conclusion
to `TraceAgrees`'s `mapM` form. -/
private theorem forall₂_mapM_ok {α β : Type} {g : α → Except String β} :
    ∀ {xs : List α} {ys : List β},
      List.Forall₂ (fun a b => g a = .ok b) xs ys → xs.mapM g = .ok ys := by
  intro xs ys h
  induction h with
  | nil => rfl
  | @cons a b l₁ l₂ hab _ ih =>
      have hab' : g a = .ok b := hab
      simp only [List.mapM_cons, hab', ih, except_bind_ok, except_pure_def]

namespace Sim

/-! ## Prefix simulation with an output relation (the obligation shape)

`SimE` (Rwv.Sim) demands equal outputs and lockstep halting — the
right shape when both machines share an output type. The concrete
correspondence relates an Eidos machine (algebraic outputs) to a Hyle
device (bit-vector outputs, never halting), so the schema layer
weakens both: outputs are related by `Q`, and the left machine may
halt while the right runs on (but not conversely). The conclusion
weakens to match: the left trace is `Q`-pointwise-related to a prefix
of the right trace — exactly `TraceAgrees`'s two cases at once. Error
lockstep is not demanded either; `simP_run` assumes both runs
succeeded, which kills the mixed error branches. -/

/-- Prefix simulation along a state relation `R` with an output
relation `Q`, over inputs satisfying `P` (the input-canonicality
restriction: a validator can only discharge the per-step agreement for
well-typed inputs — a non-canonical input with a well-formed bit image
can drive the two case-selection mechanisms, constructor-name matching
on the Eidos side and tag-bit comparison on the Hyle side, to
different alternatives while both steps succeed): from related states
and a `P`-input, if both machines step they emit `Q`-related outputs
and step to related states, and the right machine does not halt
first. -/
structure SimP {S T I O₁ O₂ : Type} (m₁ : MealyE S I O₁) (m₂ : MealyE T I O₂)
    (R : S → T → Prop) (Q : O₁ → O₂ → Prop) (P : I → Prop) : Prop where
  agree : ∀ s t i, P i → R s t →
    match m₁.step s i, m₂.step t i with
    | .ok (some (o₁, s')), .ok (some (o₂, t')) => Q o₁ o₂ ∧ R s' t'
    | .ok (some _), .ok none => False   -- the right machine may not halt first
    | _, _ => True   -- left halt: fine; error cases: excluded by the success hypotheses

/-- The prefix-simulation metatheorem: from related states, whenever
both machines run successfully on a `P`-pointwise stimulus, the left
trace is `Q`-pointwise-related to a prefix of the right trace. -/
theorem simP_run {S T I O₁ O₂ : Type} {m₁ : MealyE S I O₁} {m₂ : MealyE T I O₂}
    {R : S → T → Prop} {Q : O₁ → O₂ → Prop} {P : I → Prop} (h : SimP m₁ m₂ R Q P) :
    ∀ (is : List I) (s : S) (t : T) (os₁ : List O₁) (os₂ : List O₂),
      (∀ i ∈ is, P i) → R s t → m₁.run s is = .ok os₁ → m₂.run t is = .ok os₂ →
      ∃ os₂', os₂' <+: os₂ ∧ List.Forall₂ Q os₁ os₂' := by
  intro is
  induction is with
  | nil =>
      intro s t os₁ os₂ _ _ h₁ h₂
      simp only [MealyE.run] at h₁ h₂
      injection h₁ with h₁
      injection h₂ with h₂
      subst h₁; subst h₂
      exact ⟨[], List.nil_prefix, List.Forall₂.nil⟩
  | cons i is ih =>
      intro s t os₁ os₂ hP hR h₁ h₂
      have hag := h.agree s t i (hP i List.mem_cons_self) hR
      match hs₁ : m₁.step s i, hs₂ : m₂.step t i with
      | .error e, _ =>
          simp only [MealyE.run, hs₁] at h₁
          exact absurd h₁ (by simp)
      | .ok none, _ =>
          simp only [MealyE.run, hs₁] at h₁
          injection h₁ with h₁
          subst h₁
          exact ⟨[], List.nil_prefix, List.Forall₂.nil⟩
      | .ok (some (o₁, s')), .error e =>
          simp only [MealyE.run, hs₂] at h₂
          exact absurd h₂ (by simp)
      | .ok (some (o₁, s')), .ok none =>
          rw [hs₁, hs₂] at hag
          exact absurd hag (by simp)
      | .ok (some (o₁, s')), .ok (some (o₂, t')) =>
          rw [hs₁, hs₂] at hag
          obtain ⟨hQ, hR'⟩ := hag
          simp only [MealyE.run, hs₁] at h₁
          simp only [MealyE.run, hs₂] at h₂
          match hr₁ : m₁.run s' is, hr₂ : m₂.run t' is with
          | .error e, _ =>
              simp only [hr₁] at h₁
              exact absurd h₁ (by simp)
          | .ok os₁', .error e =>
              simp only [hr₂] at h₂
              exact absurd h₂ (by simp)
          | .ok os₁', .ok os₂'' =>
              simp only [hr₁] at h₁
              simp only [hr₂] at h₂
              injection h₁ with h₁
              injection h₂ with h₂
              subst h₁; subst h₂
              obtain ⟨os₂', hpre, hfa⟩ := ih s' t' os₁' os₂''
                (fun j hj => hP j (List.mem_cons_of_mem _ hj)) hR' hr₁ hr₂
              exact ⟨o₂ :: os₂', List.cons_prefix_cons.mpr ⟨rfl, hpre⟩,
                     List.Forall₂.cons hQ hfa⟩

/-! ## Input precomposition

The Hyle device consumes port-split bit vectors while the Eidos
machine consumes the algebraic values they encode; `mapIn` precomposes
the device with the (possibly failing) encoding so both induced
machines share the algebraic input type. -/

/-- Precompose a machine's input with an `Except`-valued decoding. -/
def MealyE.mapIn {S I I' O : Type} (m : MealyE S I O) (g : I' → Except String I) :
    MealyE S I' O where
  step s i' := do m.step s (← g i')

/-- The precomposed step, without exposing the structure literal. -/
private theorem mapIn_step {S I I' O : Type} (m : MealyE S I O)
    (g : I' → Except String I) (s : S) (i' : I') :
    (m.mapIn g).step s i' = g i' >>= fun i => m.step s i := rfl

/-- Run equivalence for input precomposition: when the whole stimulus
decodes successfully, the precomposed machine's run is the underlying
machine's run on the decoded stimulus. -/
theorem MealyE.run_mapIn {S I I' O : Type} (m : MealyE S I O) (g : I' → Except String I) :
    ∀ (is' : List I') (is : List I) (s : S), is'.mapM g = .ok is →
      (m.mapIn g).run s is' = m.run s is := by
  intro is'
  induction is' with
  | nil =>
      intro is s h
      rw [List.mapM_nil, except_pure_def] at h
      injection h with h
      subst h
      simp only [MealyE.run]
  | cons i' is' ih =>
      intro is s h
      rw [List.mapM_cons] at h
      obtain ⟨i, hg, h₁⟩ := except_bind_eq_ok h
      obtain ⟨is₂, hrest, h₂⟩ := except_bind_eq_ok h₁
      have h₃ : (Except.ok (i :: is₂) : Except String (List I)) = .ok is := h₂
      injection h₃ with h₃
      subst h₃
      have hstep : (m.mapIn g).step s i' = m.step s i := by
        simp only [mapIn_step, hg, except_bind_ok]
      cases hs : m.step s i with
      | error e => simp only [MealyE.run, hstep, hs]
      | ok r =>
          cases r with
          | none => simp only [MealyE.run, hstep, hs]
          | some p =>
              obtain ⟨o, s'⟩ := p
              simp only [MealyE.run, hstep, hs]
              rw [ih is₂ s' hrest]

/-- A machine that never halts produces full-length successful traces:
the length side of the trace-equality upgrade. -/
theorem MealyE.run_length_of_no_halt {S I O : Type} {m : MealyE S I O}
    (hnh : ∀ s i, m.step s i ≠ .ok none) :
    ∀ (is : List I) (s : S) {os : List O},
      m.run s is = .ok os → os.length = is.length := by
  intro is
  induction is with
  | nil =>
      intro s os h
      simp only [MealyE.run] at h
      injection h with h
      subst h
      rfl
  | cons i is ih =>
      intro s os h
      cases hs : m.step s i with
      | error e =>
          simp only [MealyE.run, hs] at h
          exact absurd h (by simp)
      | ok r =>
          cases r with
          | none => exact absurd hs (hnh s i)
          | some p =>
              obtain ⟨o, s'⟩ := p
              simp only [MealyE.run, hs] at h
              cases hr : m.run s' is with
              | error e =>
                  simp only [hr] at h
                  exact absurd h (by simp)
              | ok os' =>
                  simp only [hr] at h
                  injection h with h
                  subst h
                  simp [List.length_cons, ih s' hr]

end Sim

/-! ## Length lemmas, Hyle leg -/

namespace Hyle

/-- The induced step, without exposing the structure literal
(Correspond2's identical lemma is private there). -/
private theorem inducedMealy_step' (F : Sem.FEnv) (X : Sem.XEnv) (dev : Device)
    (regs : HashMap String BV) (ins : List BV) :
    (inducedMealy F X dev).step regs ins = (do
      let (outs, regs') ← Sem.step F X dev regs ins
      pure (some (outs, regs'))) := rfl

/-- The induced Hyle machine never halts: its step wraps `some`
unconditionally (Hyle devices run forever, doc/hyle.md §6.4). -/
private theorem inducedMealy_no_halt (F : Sem.FEnv) (X : Sem.XEnv) (dev : Device) :
    ∀ (regs : HashMap String BV) (ins : List BV),
      (inducedMealy F X dev).step regs ins ≠ .ok none := by
  intro regs ins hc
  rw [inducedMealy_step'] at hc
  obtain ⟨pr, _hpr, hc'⟩ := except_bind_eq_ok hc
  obtain ⟨outs, regs'⟩ := pr
  have hc'' : (Except.ok (some (outs, regs'))
      : Except String (Option (List BV × HashMap String BV))) = .ok none := hc'
  injection hc'' with hc''
  simp at hc''

/-- A successful device run covers the whole stimulus, cycle for
cycle. -/
theorem run_length {F : Sem.FEnv} {X : Sem.XEnv} {dev : Device}
    {stim ht : List (List BV)} (h : Sem.run F X dev stim = .ok ht) :
    ht.length = stim.length := by
  rw [run_eq_mealy] at h
  exact Rwv.Sim.MealyE.run_length_of_no_halt (inducedMealy_no_halt F X dev)
    stim (Sem.initRegs dev) h

end Hyle

/-! ## Length lemmas, Eidos leg -/

namespace Eidos

/-- Once `Proc.run`'s fold has no live state, the remaining inputs are
consumed as no-ops (Correspond2's dead-state lemma, private there). -/
private theorem fold_dead (Δ : DEnv) (defns : HashMap Int Defn)
    (blocks : HashMap Int Block) (evalFuel gotoFuel : Nat) :
    ∀ (inputs : List Val) (acc : List Val) (halted : Option Val),
    inputs.foldlM (init := (acc, halted, (none : Option MState)))
      (Machine.foldStep Δ defns blocks evalFuel gotoFuel)
    = .ok (acc, halted, none) := by
  intro inputs
  induction inputs with
  | nil => intro acc _; rfl
  | cons i is ih =>
      intro acc halted
      rw [List.foldlM_cons]
      cases halted with
      | none => exact ih acc none
      | some a => exact ih acc (some a)

/-- The fold from a live, unhalted state, when it ends unhalted,
pushed one output per input: the length invariant of `Proc.run`'s
fold. (A halt mid-fold is impossible here — the halted flag is sticky
by `fold_dead`, so it would survive to the final state.) -/
private theorem fold_live_length (Δ : DEnv) (defns : HashMap Int Defn)
    (blocks : HashMap Int Block) (evalFuel gotoFuel : Nat) :
    ∀ (inputs : List Val) (acc : List Val) (s : MState)
      (outsRev : List Val) (s? : Option MState),
    inputs.foldlM (init := (acc, (none : Option Val), some s))
      (Machine.foldStep Δ defns blocks evalFuel gotoFuel) = .ok (outsRev, none, s?) →
    outsRev.length = inputs.length + acc.length := by
  intro inputs
  induction inputs with
  | nil =>
      intro acc s outsRev s? h
      rw [List.foldlM_nil, except_pure_def] at h
      injection h with h
      have hacc : acc = outsRev := congrArg Prod.fst h
      subst hacc
      simp
  | cons i is ih =>
      intro acc s outsRev s? h
      rw [List.foldlM_cons] at h
      cases hstep : Machine.step Δ defns blocks evalFuel gotoFuel s i with
      | error e =>
          simp only [Machine.foldStep, hstep, except_bind_error] at h
          exact absurd h (by simp)
      | ok so =>
          cases so with
          | step o s' =>
              simp only [Machine.foldStep, hstep, except_bind_ok, except_pure_def] at h
              have hlen := ih (o :: acc) s' outsRev s? h
              simp only [List.length_cons] at hlen ⊢
              omega
          | halt a =>
              simp only [Machine.foldStep, hstep, except_bind_ok, except_pure_def,
                fold_dead] at h
              injection h with h
              have hmid : (some a : Option Val) = none := congrArg (fun p => p.2.1) h
              simp at hmid

/-- A successful, unhalted machine run emits one output per input: the
Eidos-side length lemma, by the fold invariant. -/
theorem run_no_halt_length {Δ : DEnv} {defns : HashMap Int Defn}
    {evalFuel gotoFuel : Nat} {p : Proc} {inputs : List Val} {mt : MTrace}
    (h : Proc.run Δ defns evalFuel gotoFuel p inputs = .ok mt)
    (hnone : mt.halted = none) : mt.outs.length = inputs.length := by
  have h' : (do
      let σ₀ ← Machine.initCells Δ defns evalFuel p
      match ← Machine.execBlock Δ defns
          (HashMap.ofList (p.blocks.map fun (l, b) => (l.uniq, b)))
          evalFuel gotoFuel [] σ₀ p.entry with
      | .halt a => pure ⟨[], some a⟩
      | .step _o s₀ => do
          let (outsRev, halted, _) ← inputs.foldlM
              (init := (([] : List Val), (Option.none : Option Val), some s₀))
              (Machine.foldStep Δ defns
                (HashMap.ofList (p.blocks.map fun (l, b) => (l.uniq, b)))
                evalFuel gotoFuel)
          pure ⟨outsRev.reverse, halted⟩
      : Except String MTrace) = .ok mt := h
  clear h
  obtain ⟨σ₀, _hσ, h₁⟩ := except_bind_eq_ok h'
  obtain ⟨out, _hexec, h₂⟩ := except_bind_eq_ok h₁
  cases out with
  | halt a =>
      have h₃ : (Except.ok (⟨[], some a⟩ : MTrace) : Except String MTrace) = .ok mt := h₂
      injection h₃ with h₃
      rw [← h₃] at hnone
      exact absurd hnone (by simp)
  | step o s₀ =>
      obtain ⟨tri, hf, h₃⟩ := except_bind_eq_ok h₂
      obtain ⟨outsRev, halted, s?⟩ := tri
      have h₄ : (Except.ok (⟨outsRev.reverse, halted⟩ : MTrace) : Except String MTrace)
          = .ok mt := h₃
      injection h₄ with h₄
      subst h₄
      have hh : halted = none := hnone
      subst hh
      -- The blocks argument is `_`: re-elaborating the `HashMap.ofList`
      -- expression here mints a fresh pattern-match matcher, and unifying
      -- it against `hf`'s copy diverges inside `HashMap.ofList`.
      have hlen := fold_live_length Δ defns _ evalFuel gotoFuel inputs [] s₀ outsRev s? hf
      simpa using hlen

end Eidos

/-! ## The obligation package and the glue theorem -/

/-- The per-label step obligations for a machine-mode process against
a compiled Hyle device: a prefix simulation between the two induced
Mealy machines — the Eidos-M machine on algebraic values, and the Hyle
device precomposed with the input port-split encoding — along a
candidate state relation `R`, with outputs related by the output
port-split encoding, over well-typed inputs (`Val.HasTy` at the
process input type — the hypothesis `Corresponds` supplies; without
it the agreement is undischargeable, see `SimP`). This is the package
a validator discharges per label (by combinational equivalence);
`R` is the canonicality-invariant graph of the state encoding. -/
def StepObligations (Δ : Eidos.DEnv) (defns : HashMap Int Eidos.Defn)
    (evalFuel gotoFuel : Nat) (blocks : HashMap Int Eidos.Block)
    (F : Hyle.Sem.FEnv) (X : Hyle.Sem.XEnv) (dev : Hyle.Device) (p : Eidos.Proc)
    (R : Eidos.MState → HashMap String Hyle.BV → Prop) : Prop :=
  Rwv.Sim.SimP (Eidos.inducedMealy Δ defns evalFuel gotoFuel blocks)
      ((Hyle.inducedMealy F X dev).mapIn (Eidos.Val.portSplit Δ evalFuel p.inTy))
      R
      (fun v bs => Eidos.Val.portSplit Δ evalFuel p.outTy v = .ok bs)
      (fun v => Eidos.Val.HasTy Δ v p.inTy)

/-- The glue theorem: the step obligations, plus the initial-state
obligation (the entry block's post-reset state is `R`-related to the
declared register initials), conclude the top-level §7.5.6
correspondence. The composition: expose `Sem.run` inside
`Program.run`, present both sides as induced machines (Correspond2),
transport the device to algebraic inputs (`run_mapIn`), run the prefix
simulation (`simP_run`), and in the unhalted case upgrade the prefix
to equality by the two length lemmas. -/
theorem stepObligations_corresponds
    {Δ : Eidos.DEnv} {defns : HashMap Int Eidos.Defn} {evalFuel gotoFuel : Nat}
    {p : Eidos.Proc} {H : Hyle.Program} {F : Hyle.Sem.FEnv} {X : Hyle.Sem.XEnv}
    {dev : Hyle.Device} {blocks : HashMap Int Eidos.Block}
    {R : Eidos.MState → HashMap String Hyle.BV → Prop}
    (hF : Hyle.Sem.mkFEnv H = .ok F)
    (hX : X = Hyle.Sem.xenv H)
    (hdev : dev = H.device)
    (hblocks : blocks = HashMap.ofList (p.blocks.map fun (l, b) => (l.uniq, b)))
    (hR : StepObligations Δ defns evalFuel gotoFuel blocks F X dev p R)
    (hinit : ∀ σ₀ o s₀, Eidos.Machine.initCells Δ defns evalFuel p = .ok σ₀ →
        Eidos.Machine.execBlock Δ defns blocks evalFuel gotoFuel [] σ₀ p.entry
          = .ok (.step o s₀) →
        R s₀ (Hyle.Sem.initRegs dev)) :
    Eidos.Corresponds Δ defns evalFuel gotoFuel p H := by
  subst hX hdev
  intro ins hty encIns hmapM mt hmrun ht hhrun
  -- Expose the stream semantics inside `Program.run`.
  have hrunF : (do
      let F' ← Hyle.Sem.mkFEnv H
      Hyle.Sem.run F' (Hyle.Sem.xenv H) H.device encIns) = .ok ht := hhrun
  rw [hF] at hrunF
  have hsem : Hyle.Sem.run F (Hyle.Sem.xenv H) H.device encIns = .ok ht := hrunF
  have hlenHt : ht.length = encIns.length := Hyle.run_length hsem
  -- The Hyle side as the precomposed induced machine.
  have hmealyH : ((Hyle.inducedMealy F (Hyle.Sem.xenv H) H.device).mapIn
      (Eidos.Val.portSplit Δ evalFuel p.inTy)).run (Hyle.Sem.initRegs H.device) ins
      = .ok ht := by
    rw [Sim.MealyE.run_mapIn _ _ ins encIns _ hmapM, ← Hyle.run_eq_mealy]
    exact hsem
  -- The Eidos side as its induced machine.
  obtain ⟨σ₀, hσ, hcase⟩ :=
    Eidos.run_outs_eq_mealy Δ defns evalFuel gotoFuel p ins mt blocks hblocks hmrun
  cases hcase with
  | inl hhalt =>
      -- Entry-block halt: the machine trace is empty, and the empty
      -- encoding is a prefix of any device trace.
      obtain ⟨a, _hexec, hmt⟩ := hhalt
      subst hmt
      exact ⟨[], rfl, List.nil_prefix⟩
  | inr hstep =>
      obtain ⟨o, s₀, hexec, hmealyE⟩ := hstep
      have hR₀ : R s₀ (Hyle.Sem.initRegs H.device) := hinit σ₀ o s₀ hσ hexec
      obtain ⟨os₂', hpre, hfa⟩ := Sim.simP_run hR ins s₀ (Hyle.Sem.initRegs H.device)
        mt.outs ht hty hR₀ hmealyE hmealyH
      refine ⟨os₂', forall₂_mapM_ok hfa, ?_⟩
      cases hh : mt.halted with
      | some a => exact hpre
      | none =>
          -- Unhalted: upgrade the prefix to equality by lengths.
          have h₁ : mt.outs.length = ins.length := Eidos.run_no_halt_length hmrun hh
          have h₂ : mt.outs.length = os₂'.length := List.Forall₂.length_eq hfa
          have h₃ : encIns.length = ins.length := mapM_ok_length hmapM
          have h₄ : os₂'.length = ht.length := by omega
          exact (hpre.eq_of_length h₄).symm

end Rwv

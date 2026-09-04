/-
The simulation metatheorem skeleton, at the abstract level: Mealy
machines with halt (the shared shape of the Synolon machine
semantics, doc/synolon.md §5.3–§5.4, and the Hyle device
semantics, doc/hyle.md §6.4, restricted to finite stimulus), and the
forward-simulation theorem — related initial states plus per-step
agreement give equal observable traces, up to and excluding the halt.

The concrete instantiation (Rwv.Correspond2, Rwv.Schema) takes S =
Eidos machine states with outputs composed through the
representation function, T = Hyle register stores, R = the
canonicality-invariant graph of the state encoding, and discharges
`Sim.agree` per label by combinational equivalence — turning
per-label obligations into trace equality, which is the top-level
theorem's shape.
-/

namespace Rwv.Sim

/-- A Mealy machine with halt: a step yields an output and a next
state, or halts (yielding no defined output for that cycle —
doc/synolon.md §5.4). -/
structure Mealy (S I O : Type) where
  step : S → I → Option (O × S)

/-- The finite observable trace: outputs up to (and excluding) the
halting cycle. -/
def Mealy.run (m : Mealy S I O) : S → List I → List O
  | _, [] => []
  | s, i :: is =>
      match m.step s i with
      | none => []
      | some (o, s') => o :: m.run s' is

/-- Forward simulation up to halt, along a state relation R: related
states halt together, and when they step, they emit equal outputs and
step to related states. -/
structure Sim (m₁ : Mealy S I O) (m₂ : Mealy T I O) (R : S → T → Prop) : Prop where
  halt_iff : ∀ s t i, R s t → (m₁.step s i = none ↔ m₂.step t i = none)
  agree : ∀ s t i o₁ s' o₂ t', R s t →
    m₁.step s i = some (o₁, s') → m₂.step t i = some (o₂, t') →
    o₁ = o₂ ∧ R s' t'

/-- The metatheorem: a simulation from related initial states gives
equal observable traces on every finite stimulus. -/
theorem sim_run {S T I O : Type} {m₁ : Mealy S I O} {m₂ : Mealy T I O}
    {R : S → T → Prop} (h : Sim m₁ m₂ R) :
    ∀ (is : List I) (s : S) (t : T), R s t → m₁.run s is = m₂.run t is := by
  intro is
  induction is with
  | nil => intro s t _; rfl
  | cons i is ih =>
      intro s t hR
      match h₁ : m₁.step s i, h₂ : m₂.step t i with
      | none, none =>
          simp [Mealy.run, h₁, h₂]
      | none, some (o₂, t') =>
          exact absurd ((h.halt_iff s t i hR).mp h₁) (by simp [h₂])
      | some (o₁, s'), none =>
          exact absurd ((h.halt_iff s t i hR).mpr h₂) (by simp [h₁])
      | some (o₁, s'), some (o₂, t') =>
          obtain ⟨ho, hR'⟩ := h.agree s t i o₁ s' o₂ t' hR h₁ h₂
          simp [Mealy.run, h₁, h₂, ho]
          exact ih s' t' hR'

/-! ## The Except-aware layer

The concrete semantics (`Rwv.Synolon.Proc.run`, `Rwv.Hyle.Program.run`)
evaluate in `Except String` — fuel exhaustion and the deliberate
rejections are errors, not machine behavior. `MealyE` is the shape
they actually inhabit; the simulation theorem here concludes equality
of *successful* traces (related machines also fail together, so a
success on either side forces success on both). -/

/-- A Mealy machine with halt, whose step may also fail. -/
structure MealyE (S I O : Type) where
  step : S → I → Except String (Option (O × S))

/-- The finite observable trace, in `Except`: outputs up to (and
excluding) the halting cycle; errors propagate. -/
def MealyE.run (m : MealyE S I O) : S → List I → Except String (List O)
  | _, [] => .ok []
  | s, i :: is =>
      match m.step s i with
      | .error e => .error e
      | .ok none => .ok []
      | .ok (some (o, s')) =>
          match m.run s' is with
          | .error e => .error e
          | .ok os   => .ok (o :: os)

/-- Forward simulation up to halt, with errors in lockstep: related
states fail together, halt together, or step to equal outputs and
related states. -/
structure SimE (m₁ : MealyE S I O) (m₂ : MealyE T I O) (R : S → T → Prop) : Prop where
  agree : ∀ s t i, R s t →
    match m₁.step s i, m₂.step t i with
    | .error _, .error _ => True
    | .ok none, .ok none => True
    | .ok (some (o₁, s')), .ok (some (o₂, t')) => o₁ = o₂ ∧ R s' t'
    | _, _ => False

/-- The Except-aware metatheorem: from related states, the two
machines produce the same successful traces. -/
theorem simE_run {S T I O : Type} {m₁ : MealyE S I O} {m₂ : MealyE T I O}
    {R : S → T → Prop} (h : SimE m₁ m₂ R) :
    ∀ (is : List I) (s : S) (t : T), R s t → ∀ os,
      (m₁.run s is = .ok os ↔ m₂.run t is = .ok os) := by
  intro is
  induction is with
  | nil => intro s t _ os; exact Iff.rfl
  | cons i is ih =>
      intro s t hR os
      have hag := h.agree s t i hR
      match h₁ : m₁.step s i, h₂ : m₂.step t i with
      | .error e₁, .error e₂ =>
          simp [MealyE.run, h₁, h₂]
      | .ok none, .ok none =>
          simp [MealyE.run, h₁, h₂]
      | .ok (some (o₁, s')), .ok (some (o₂, t')) =>
          rw [h₁, h₂] at hag
          obtain ⟨ho, hR'⟩ := hag
          subst ho
          simp only [MealyE.run, h₁, h₂]
          match hr₁ : m₁.run s' is, hr₂ : m₂.run t' is with
          | .error e₁, .error e₂ => simp
          | .ok os₁, .ok os₂ =>
              have h₁₂ := (ih s' t' hR' os₁).mp hr₁
              rw [hr₂] at h₁₂
              cases h₁₂
              simp
          | .error e₁, .ok os₂ =>
              have := (ih s' t' hR' os₂).mpr hr₂
              rw [hr₁] at this
              cases this
          | .ok os₁, .error e₂ =>
              have := (ih s' t' hR' os₁).mp hr₁
              rw [hr₂] at this
              cases this
      | .error e₁, .ok r =>
          rw [h₁, h₂] at hag
          cases r <;> exact absurd hag (by simp)
      | .ok r, .error e₂ =>
          rw [h₁, h₂] at hag
          cases r with
          | none => exact absurd hag (by simp)
          | some p => exact absurd hag (by simp)
      | .ok none, .ok (some p) =>
          rw [h₁, h₂] at hag
          exact absurd hag (by simp)
      | .ok (some p), .ok none =>
          rw [h₁, h₂] at hag
          exact absurd hag (by simp)

end Rwv.Sim

/-
The simulation metatheorem skeleton (the plan's M1), at the abstract
level: Mealy machines with halt (the shared shape of the Eidos-M
machine semantics, doc/eidos.md §7.5.3–§7.5.4, and the Hyle device
semantics, doc/hyle.md §6.4, restricted to finite stimulus), and the
forward-simulation theorem — related initial states plus per-step
agreement give equal observable traces, up to and excluding the halt.

The concrete instantiation (later phases) takes S = Eidos machine
states with outputs composed through the representation function,
T = Hyle register stores, R = the canonicality-invariant graph of the
state encoding, and discharges `Sim.agree` per label by combinational
equivalence — turning per-label obligations into trace equality,
which is the top-level theorem's shape.
-/

namespace Rwv.Sim

/-- A Mealy machine with halt: a step yields an output and a next
state, or halts (yielding no defined output for that cycle —
doc/eidos.md §7.5.4). -/
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

end Rwv.Sim

/-
The concrete instantiation legs of M1: the two stream semantics
(`Rwv.Eidos.Proc.run`, doc/eidos.md §7.5.4, and `Rwv.Hyle.Sem.run`,
doc/hyle.md §6.4) presented as induced `Rwv.Sim.MealyE` machines, with
proved run-equivalence lemmas connecting each concrete run function to
`MealyE.run` of its induced machine. With these, the abstract
simulation metatheorem (`Rwv.Sim.simE_run`) speaks directly about the
concrete semantics: a `SimE` between the induced machines gives
equality of the concrete successful traces.
-/
import Rwv.Eidos.Machine
import Rwv.Hyle.Semantics
import Rwv.Sim

namespace Rwv

open Std (HashMap)

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

/-! ## The Hyle device as an induced Mealy machine (doc/hyle.md §6.4) -/

namespace Hyle

/-- The Mealy machine induced by a Hyle device (instance-free
fragment): states are register stores, one step is `Sem.step`, and the
machine never halts — Hyle devices run forever (doc/hyle.md §6.4). -/
def inducedMealy (F : Sem.FEnv) (X : Sem.XEnv) (dev : Device) :
    Rwv.Sim.MealyE (HashMap String BV) (List BV) (List BV) where
  step regs ins := do
    let (outs, regs') ← Sem.step F X dev regs ins
    pure (some (outs, regs'))

/-- The induced step, without exposing the structure literal. -/
private theorem inducedMealy_step (F : Sem.FEnv) (X : Sem.XEnv) (dev : Device)
    (regs : HashMap String BV) (ins : List BV) :
    (inducedMealy F X dev).step regs ins = (do
      let (outs, regs') ← Sem.step F X dev regs ins
      pure (some (outs, regs'))) := rfl

/-- The fold inside `Sem.run`, related to the induced machine's run
from the same register store: the fold's reversed accumulator, once
re-reversed, is the accumulator so far followed by the machine's
trace. The standard fold-to-recursion correspondence, by list
induction with the store and accumulator generalized. -/
private theorem fold_eq_mealy (F : Sem.FEnv) (X : Sem.XEnv) (dev : Device) :
    ∀ (stim : List (List BV)) (regs : HashMap String BV) (acc : List (List BV)),
    (do
      let (_, outsRev) ← stim.foldlM (init := (regs, acc)) (Sem.foldStep F X dev)
      pure outsRev.reverse : Except String (List (List BV)))
    = (do
        let os ← (inducedMealy F X dev).run regs stim
        pure (acc.reverse ++ os)) := by
  intro stim
  induction stim with
  | nil =>
      intro regs acc
      simp only [List.foldlM_nil, Rwv.Sim.MealyE.run, except_pure_def, except_bind_ok,
        List.append_nil]
  | cons ins stim ih =>
      intro regs acc
      rw [List.foldlM_cons]
      cases hstep : Sem.step F X dev regs ins with
      | error e =>
          simp only [Sem.foldStep, hstep, Rwv.Sim.MealyE.run, inducedMealy_step,
            except_bind_error]
      | ok p =>
          obtain ⟨outs, regs'⟩ := p
          refine Eq.trans ?_ (Eq.trans (ih regs' (outs :: acc)) ?_)
          · simp only [Sem.foldStep, hstep, except_bind_ok, except_pure_def]
          · cases hrun : (inducedMealy F X dev).run regs' stim with
            | error e =>
                simp only [hrun, Rwv.Sim.MealyE.run, inducedMealy_step, hstep,
                  except_bind_ok, except_pure_def, except_bind_error]
            | ok os =>
                simp only [hrun, Rwv.Sim.MealyE.run, inducedMealy_step, hstep,
                  except_bind_ok, except_pure_def, List.reverse_cons, List.append_assoc,
                  List.singleton_append]

/-- Run equivalence, Hyle leg: the §6.4 stream semantics is exactly
the induced machine's run from the declared register initials. -/
theorem run_eq_mealy (F : Sem.FEnv) (X : Sem.XEnv) (dev : Device)
    (stim : List (List BV)) :
    Sem.run F X dev stim = (inducedMealy F X dev).run (Sem.initRegs dev) stim := by
  refine Eq.trans (fold_eq_mealy F X dev stim (Sem.initRegs dev) []) ?_
  cases (inducedMealy F X dev).run (Sem.initRegs dev) stim with
  | error e => rfl
  | ok os => simp only [except_bind_ok, except_pure_def, List.reverse_nil, List.nil_append]

end Hyle

/-! ## The Eidos-M process as an induced Mealy machine (§7.5.3–§7.5.4) -/

namespace Eidos

/-- The Mealy machine induced by an Eidos-M process's block graph:
states are machine states, one step is `Machine.step`, and a halt
terminator halts the machine — the halt answer is not part of the
observable output trace (doc/eidos.md §7.5.4). -/
def inducedMealy (Δ : DEnv) (defns : HashMap Int Defn) (evalFuel gotoFuel : Nat)
    (blocks : HashMap Int Block) : Rwv.Sim.MealyE MState Val Val where
  step s i := do
    match ← Machine.step Δ defns blocks evalFuel gotoFuel s i with
    | .step o s' => pure (some (o, s'))
    | .halt _    => pure none

/-- The induced step, without exposing the structure literal. -/
private theorem inducedMealy_step (Δ : DEnv) (defns : HashMap Int Defn)
    (evalFuel gotoFuel : Nat) (blocks : HashMap Int Block) (s : MState) (i : Val) :
    (inducedMealy Δ defns evalFuel gotoFuel blocks).step s i = (do
      match ← Machine.step Δ defns blocks evalFuel gotoFuel s i with
      | .step o s' => pure (some (o, s'))
      | .halt _    => pure none) := rfl

/-- Once `Proc.run`'s fold has no live state (`s? = none`), the
remaining inputs are consumed as no-ops: the accumulator never changes
(the dead-state lemma). -/
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

/-- The fold inside `Proc.run` from a live state, projected to its
(re-reversed) output accumulator, is the accumulator so far followed
by the induced machine's trace. The subtlety this lemma absorbs: after
a halt the fold keeps consuming inputs as no-ops (`fold_dead`), while
`MealyE.run` stops at the halt — the outputs agree because neither
adds anything past the halt. -/
private theorem fold_outs_eq_mealy (Δ : DEnv) (defns : HashMap Int Defn)
    (blocks : HashMap Int Block) (evalFuel gotoFuel : Nat) :
    ∀ (inputs : List Val) (s : MState) (acc : List Val),
    (do
      let (outsRev, _, _) ← inputs.foldlM
        (init := (acc, (Option.none : Option Val), some s))
        (Machine.foldStep Δ defns blocks evalFuel gotoFuel)
      pure outsRev.reverse : Except String (List Val))
    = (do
        let os ← (inducedMealy Δ defns evalFuel gotoFuel blocks).run s inputs
        pure (acc.reverse ++ os)) := by
  intro inputs
  induction inputs with
  | nil =>
      intro s acc
      simp only [List.foldlM_nil, Rwv.Sim.MealyE.run, except_pure_def, except_bind_ok,
        List.append_nil]
  | cons i is ih =>
      intro s acc
      rw [List.foldlM_cons]
      cases hstep : Machine.step Δ defns blocks evalFuel gotoFuel s i with
      | error e =>
          simp only [Machine.foldStep, hstep, Rwv.Sim.MealyE.run, inducedMealy_step,
            except_bind_error]
      | ok out =>
          cases out with
          | step o s' =>
              refine Eq.trans ?_ (Eq.trans (ih s' (o :: acc)) ?_)
              · simp only [Machine.foldStep, hstep, except_bind_ok, except_pure_def]
              · cases hrun : (inducedMealy Δ defns evalFuel gotoFuel blocks).run s' is with
                | error e =>
                    simp only [hrun, Rwv.Sim.MealyE.run, inducedMealy_step, hstep,
                      except_bind_ok, except_pure_def, except_bind_error]
                | ok os =>
                    simp only [hrun, Rwv.Sim.MealyE.run, inducedMealy_step, hstep,
                      except_bind_ok, except_pure_def, List.reverse_cons,
                      List.append_assoc, List.singleton_append]
          | halt a =>
              refine Eq.trans ?_ (?_ :
                (Except.ok acc.reverse : Except String (List Val)) = _)
              · simp only [Machine.foldStep, hstep, except_bind_ok, except_pure_def,
                  fold_dead]
              · simp only [Rwv.Sim.MealyE.run, inducedMealy_step, hstep,
                  except_bind_ok, except_pure_def, List.append_nil]

/-- Run equivalence, Eidos leg: when `Proc.run` succeeds, the entry
block either halted immediately (empty trace, the process result as
the halt answer) or stepped to an initial machine state from which the
induced machine's run produces exactly the trace's outputs. The
trace's `halted` component is deliberately unrelated to the machine
run past this — the halt answer is not part of the observable output
trace (doc/eidos.md §7.5.4). -/
theorem run_outs_eq_mealy (Δ : DEnv) (defns : HashMap Int Defn)
    (evalFuel gotoFuel : Nat) (p : Proc) (inputs : List Val) (mt : MTrace)
    (blocks : HashMap Int Block)
    (hblocks : blocks = HashMap.ofList (p.blocks.map fun (l, b) => (l.uniq, b)))
    (h : Proc.run Δ defns evalFuel gotoFuel p inputs = .ok mt) :
    ∃ σ₀, Machine.initCells Δ defns evalFuel p = .ok σ₀ ∧
      ((∃ a, Machine.execBlock Δ defns blocks evalFuel gotoFuel [] σ₀ p.entry
              = .ok (.halt a)
            ∧ mt = ⟨[], some a⟩) ∨
       (∃ o s₀, Machine.execBlock Δ defns blocks evalFuel gotoFuel [] σ₀ p.entry
              = .ok (.step o s₀)
            ∧ (inducedMealy Δ defns evalFuel gotoFuel blocks).run s₀ inputs
              = .ok mt.outs)) := by
  subst hblocks
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
  obtain ⟨σ₀, hσ, h₁⟩ := except_bind_eq_ok h'
  obtain ⟨out, hexec, h₂⟩ := except_bind_eq_ok h₁
  refine ⟨σ₀, hσ, ?_⟩
  cases out with
  | halt a =>
      have h₃ : (Except.ok (⟨[], some a⟩ : MTrace) : Except String MTrace) = .ok mt := h₂
      injection h₃ with hmt
      exact .inl ⟨a, hexec, hmt.symm⟩
  | step o s₀ =>
      obtain ⟨tri, hf, h₃⟩ := except_bind_eq_ok h₂
      obtain ⟨outsRev, halted, s?⟩ := tri
      have h₄ : (Except.ok (⟨outsRev.reverse, halted⟩ : MTrace) : Except String MTrace)
          = .ok mt := h₃
      injection h₄ with hmt
      have hfold := fold_outs_eq_mealy Δ defns
        (HashMap.ofList (p.blocks.map fun (l, b) => (l.uniq, b)))
        evalFuel gotoFuel inputs s₀ []
      rw [hf] at hfold
      cases hrun : (inducedMealy Δ defns evalFuel gotoFuel
          (HashMap.ofList (p.blocks.map fun (l, b) => (l.uniq, b)))).run s₀ inputs with
      | error e =>
          rw [hrun] at hfold
          exact absurd hfold (by simp [except_bind_ok, except_pure_def,
            except_bind_error])
      | ok os =>
          rw [hrun] at hfold
          have h₅ : (Except.ok outsRev.reverse : Except String (List Val))
              = .ok (([] : List Val).reverse ++ os) := hfold
          injection h₅ with hos
          refine .inr ⟨o, s₀, hexec, ?_⟩
          rw [hrun, ← hmt]
          exact congrArg Except.ok hos.symm

end Eidos

end Rwv

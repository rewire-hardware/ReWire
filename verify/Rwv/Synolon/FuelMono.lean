/-
Fuel monotonicity for the Synolon semantics, machine half: every
fueled function of the machine semantics (Rwv.Synolon.Machine)
preserves successful results under more fuel, with the SAME value,
compositionally over the expression-level lemmas of
Rwv.Eidos.FuelMono. Consequently "∃ fuel, the run succeeds" is a
canonical fuel-independent semantics: a successful run is stable
under raising either fuel parameter (`Proc.run_fuel_stable`), and any
two successful runs at any fuels agree
(`Proc.run_fuel_deterministic`). The η tier's extern-environment
transport (`Machine.initCells_eta` … `Proc.run_eta`) is the same
family at the `EExt` instance `eext_empty`.
-/
import Rwv.Eidos.FuelMono
import Rwv.Synolon.Machine

namespace Rwv.Synolon

open Std (HashMap)
open Rwv.Hyle (BV)
open Rwv.Eidos
open Rwv.Eidos.FuelMono

/-! ## The machine layer (Rwv.Synolon.Machine)

Compositional over the evaluator lemmas; the terminator runner
`execBlock.runTerm` is monotone in the evaluation fuel and the goto
fuel separately (induction on the goto fuel, which both goto transfer
and terminator-case descent decrement). -/

/-- `Machine.selectTAlt` is monotone in its (evaluation) fuel. -/
theorem Machine.selectTAlt_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k') {scrut : Val}
    {alts : List TAlt} {r : List Id × Term} (h : Machine.selectTAlt Δ k scrut alts = .ok r) :
    Machine.selectTAlt Δ k' scrut alts = .ok r := by
  unfold Machine.selectTAlt at h ⊢
  obtain ⟨s, hs, h⟩ := except_bind_eq_ok h
  refine bind_ok (forIn_mono ?_ hs) ?_
  · intro alt st r' hb
    obtain ⟨con, bs, t⟩ := alt
    cases con with
    | dataAlt c => exact hb
    | litAlt n =>
        obtain ⟨bm, hbm, hb⟩ := except_bind_eq_ok hb
        refine bind_ok (Eval.litMatches_mono hk hbm) ?_
        exact hb
    | default => exact hb
  · exact h

private theorem runCmds_trans {Δ : DEnv} {defns : HashMap Int Defn}
    {E E' : Rwv.Hyle.Sem.EEnv} (hE : EExt E E') {k k' : Nat}
    (hk : k ≤ k') {env₀ : Eval.Env} {cells₀ : HashMap String Val} {cmds : List Cmd}
    {r : Eval.Env × HashMap String Val}
    (h : Machine.runCmds Δ defns k env₀ cells₀ cmds E = .ok r) :
    Machine.runCmds Δ defns k' env₀ cells₀ cmds E' = .ok r := by
  unfold Machine.runCmds at h ⊢
  refine foldlM_mono ?_ h
  intro s cmd r' hb
  obtain ⟨env, cells⟩ := s
  cases cmd with
  | bind x e =>
      obtain ⟨w, hw, hb⟩ := except_bind_eq_ok hb
      refine bind_ok (eval_trans hE hk hw) ?_
      exact hb
  | get x s => exact hb
  | put s a =>
      obtain ⟨w, hw, hb⟩ := except_bind_eq_ok hb
      refine bind_ok (eval_trans hE hk hw) ?_
      exact hb

/-- `Machine.runCmds` is monotone in its evaluation fuel. -/
theorem Machine.runCmds_mono {Δ : DEnv} {defns : HashMap Int Defn}
    {E : Rwv.Hyle.Sem.EEnv} {k k' : Nat}
    (hk : k ≤ k') {env₀ : Eval.Env} {cells₀ : HashMap String Val} {cmds : List Cmd}
    {r : Eval.Env × HashMap String Val}
    (h : Machine.runCmds Δ defns k env₀ cells₀ cmds E = .ok r) :
    Machine.runCmds Δ defns k' env₀ cells₀ cmds E = .ok r :=
  runCmds_trans (eext_refl E) hk h

/-- `Machine.execBlock`'s terminator runner is monotone in both fuels:
the goto fuel bounds intra-cycle transfer (goto chains and
terminator-case descent both decrement it), the evaluation fuel the
pure evaluation inside commands and terminators. -/
private theorem runTerm_mono (Δ : DEnv) (defns : HashMap Int Defn)
    (blocks : HashMap Int Block) {E E' : Rwv.Hyle.Sem.EEnv} (hE : EExt E E') :
    ∀ {gf gf' ef ef' : Nat}, gf ≤ gf' → ef ≤ ef' →
      ∀ {env : Eval.Env} {cells : HashMap String Val} {t : Term} {r : StepOut},
      Machine.execBlock.runTerm Δ defns blocks ef E gf env cells t = .ok r →
      Machine.execBlock.runTerm Δ defns blocks ef' E' gf' env cells t = .ok r := by
  intro gf
  induction gf with
  | zero =>
      intro gf' ef ef' _ hef env cells t r h
      cases t with
      | pause out l args =>
          rw [Machine.execBlock.runTerm] at h ⊢
          obtain ⟨o, ho, h⟩ := except_bind_eq_ok h
          obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
          refine bind_ok (eval_trans hE hef ho) ?_
          refine bind_ok (mapM_mono (fun a b hab => eval_trans hE hef hab) hvs) ?_
          exact h
      | goto l args =>
          rw [Machine.execBlock.runTerm] at h
          cases hB : blocks.get? l.uniq with
          | none => rw [hB] at h; exact error_ne_ok h
          | some blk =>
              rw [hB] at h
              obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
              dsimp only [] at h
              split at h
              · exact error_ne_ok h
              · exact error_ne_ok h
      | halt e =>
          rw [Machine.execBlock.runTerm] at h ⊢
          obtain ⟨a, ha, h⟩ := except_bind_eq_ok h
          refine bind_ok (eval_trans hE hef ha) ?_
          exact h
      | cases scrutE alts =>
          rw [Machine.execBlock.runTerm] at h
          obtain ⟨sv, hsv, h⟩ := except_bind_eq_ok h
          obtain ⟨sel, hsel, h⟩ := except_bind_eq_ok h
          obtain ⟨bs, t'⟩ := sel
          exact error_ne_ok h
  | succ gf ihg =>
      intro gf' ef ef' hgf hef env cells t r h
      obtain ⟨gf', rfl⟩ : ∃ j, gf' = j + 1 := ⟨gf' - 1, by omega⟩
      have hgf' : gf ≤ gf' := by omega
      cases t with
      | pause out l args =>
          rw [Machine.execBlock.runTerm] at h ⊢
          obtain ⟨o, ho, h⟩ := except_bind_eq_ok h
          obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
          refine bind_ok (eval_trans hE hef ho) ?_
          refine bind_ok (mapM_mono (fun a b hab => eval_trans hE hef hab) hvs) ?_
          exact h
      | goto l args =>
          rw [Machine.execBlock.runTerm] at h ⊢
          cases hB : blocks.get? l.uniq with
          | none => rw [hB] at h; exact error_ne_ok h
          | some blk =>
              rw [hB] at h
              obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
              refine bind_ok (mapM_mono (fun a b hab => eval_trans hE hef hab) hvs) ?_
              dsimp only [] at h ⊢
              split at h
              · exact error_ne_ok h
              · rename_i hlen
                rw [if_neg hlen]
                obtain ⟨p, hp, h⟩ := except_bind_eq_ok h
                obtain ⟨env'', cells'⟩ := p
                refine bind_ok (runCmds_trans hE hef hp) ?_
                exact ihg hgf' hef h
      | halt e =>
          rw [Machine.execBlock.runTerm] at h ⊢
          obtain ⟨a, ha, h⟩ := except_bind_eq_ok h
          refine bind_ok (eval_trans hE hef ha) ?_
          exact h
      | cases scrutE alts =>
          rw [Machine.execBlock.runTerm] at h ⊢
          obtain ⟨sv, hsv, h⟩ := except_bind_eq_ok h
          obtain ⟨sel, hsel, h⟩ := except_bind_eq_ok h
          obtain ⟨bs, t'⟩ := sel
          refine bind_ok (eval_trans hE hef hsv) ?_
          refine bind_ok (Machine.selectTAlt_mono hef hsel) ?_
          exact ihg hgf' hef h

private theorem execBlock_trans {Δ : DEnv} {defns : HashMap Int Defn}
    {blocks : HashMap Int Block} {E E' : Rwv.Hyle.Sem.EEnv} (hE : EExt E E')
    {ef ef' gf gf' : Nat} (hef : ef ≤ ef') (hgf : gf ≤ gf')
    {env₀ : Eval.Env} {cells₀ : HashMap String Val} {b : Block} {r : StepOut}
    (h : Machine.execBlock Δ defns blocks ef gf env₀ cells₀ b E = .ok r) :
    Machine.execBlock Δ defns blocks ef' gf' env₀ cells₀ b E' = .ok r := by
  unfold Machine.execBlock at h ⊢
  obtain ⟨p, hp, h⟩ := except_bind_eq_ok h
  obtain ⟨env, cells⟩ := p
  refine bind_ok (runCmds_trans hE hef hp) ?_
  exact runTerm_mono Δ defns blocks hE hgf hef h

/-- `Machine.execBlock` is monotone in the evaluation fuel and the goto
fuel separately. -/
theorem Machine.execBlock_mono {Δ : DEnv} {defns : HashMap Int Defn}
    {blocks : HashMap Int Block} {E : Rwv.Hyle.Sem.EEnv}
    {ef ef' gf gf' : Nat} (hef : ef ≤ ef') (hgf : gf ≤ gf')
    {env₀ : Eval.Env} {cells₀ : HashMap String Val} {b : Block} {r : StepOut}
    (h : Machine.execBlock Δ defns blocks ef gf env₀ cells₀ b E = .ok r) :
    Machine.execBlock Δ defns blocks ef' gf' env₀ cells₀ b E = .ok r :=
  execBlock_trans (eext_refl E) hef hgf h

private theorem initCells_trans {Δ : DEnv} {defns : HashMap Int Defn}
    {E E' : Rwv.Hyle.Sem.EEnv} (hE : EExt E E') {k k' : Nat}
    (hk : k ≤ k') {p : Proc} {σ : HashMap String Val}
    (h : Machine.initCells Δ defns k p E = .ok σ) :
    Machine.initCells Δ defns k' p E' = .ok σ := by
  unfold Machine.initCells at h ⊢
  refine foldlM_mono ?_ h
  intro s c r' hb
  dsimp only [] at hb ⊢
  cases hI : c.init with
  | some e =>
      rw [hI] at hb
      obtain ⟨w, hw, hb⟩ := except_bind_eq_ok hb
      refine bind_ok (eval_trans hE hk hw) ?_
      exact hb
  | none =>
      rw [hI] at hb
      obtain ⟨w, hw, hb⟩ := except_bind_eq_ok hb
      refine bind_ok (DEnv.zeroVal_mono Δ hk hw) ?_
      exact hb

/-- `Machine.initCells` is monotone in its evaluation fuel. -/
theorem Machine.initCells_mono {Δ : DEnv} {defns : HashMap Int Defn}
    {E : Rwv.Hyle.Sem.EEnv} {k k' : Nat}
    (hk : k ≤ k') {p : Proc} {σ : HashMap String Val}
    (h : Machine.initCells Δ defns k p E = .ok σ) :
    Machine.initCells Δ defns k' p E = .ok σ :=
  initCells_trans (eext_refl E) hk h

private theorem step_trans {Δ : DEnv} {defns : HashMap Int Defn}
    {blocks : HashMap Int Block} {E E' : Rwv.Hyle.Sem.EEnv} (hE : EExt E E')
    {ef ef' gf gf' : Nat} (hef : ef ≤ ef') (hgf : gf ≤ gf')
    {s : MState} {input : Val} {r : StepOut}
    (h : Machine.step Δ defns blocks ef gf s input E = .ok r) :
    Machine.step Δ defns blocks ef' gf' s input E' = .ok r := by
  unfold Machine.step at h ⊢
  cases hB : blocks.get? s.label with
  | none => rw [hB] at h; exact error_ne_ok h
  | some blk =>
      rw [hB] at h
      dsimp only [] at h ⊢
      split at h
      · exact error_ne_ok h
      · rename_i hlen
        rw [if_neg hlen]
        exact execBlock_trans hE hef hgf h

/-- `Machine.step` is monotone in the evaluation fuel and the goto fuel
separately. -/
theorem Machine.step_mono {Δ : DEnv} {defns : HashMap Int Defn}
    {blocks : HashMap Int Block} {E : Rwv.Hyle.Sem.EEnv}
    {ef ef' gf gf' : Nat} (hef : ef ≤ ef') (hgf : gf ≤ gf')
    {s : MState} {input : Val} {r : StepOut}
    (h : Machine.step Δ defns blocks ef gf s input E = .ok r) :
    Machine.step Δ defns blocks ef' gf' s input E = .ok r :=
  step_trans (eext_refl E) hef hgf h

/-- `Machine.foldStep` is monotone in the evaluation fuel and the goto
fuel separately. -/
private theorem foldStep_trans {Δ : DEnv} {defns : HashMap Int Defn}
    {blocks : HashMap Int Block} {E E' : Rwv.Hyle.Sem.EEnv} (hE : EExt E E')
    {ef ef' gf gf' : Nat} (hef : ef ≤ ef') (hgf : gf ≤ gf')
    {acc : List Val × Option Val × Option MState} {i : Val}
    {r : List Val × Option Val × Option MState}
    (h : Machine.foldStep Δ defns blocks ef gf E acc i = .ok r) :
    Machine.foldStep Δ defns blocks ef' gf' E' acc i = .ok r := by
  obtain ⟨outs, halted, s?⟩ := acc
  cases s? with
  | none => exact h
  | some s =>
      cases halted with
      | some a => exact h
      | none =>
          obtain ⟨so, hso, h⟩ := except_bind_eq_ok h
          refine bind_ok (step_trans hE hef hgf hso) ?_
          exact h

theorem Machine.foldStep_mono {Δ : DEnv} {defns : HashMap Int Defn}
    {blocks : HashMap Int Block} {E : Rwv.Hyle.Sem.EEnv}
    {ef ef' gf gf' : Nat} (hef : ef ≤ ef') (hgf : gf ≤ gf')
    {acc : List Val × Option Val × Option MState} {i : Val}
    {r : List Val × Option Val × Option MState}
    (h : Machine.foldStep Δ defns blocks ef gf E acc i = .ok r) :
    Machine.foldStep Δ defns blocks ef' gf' E acc i = .ok r :=
  foldStep_trans (eext_refl E) hef hgf h

/-- `Proc.run` is monotone in the evaluation fuel and the goto fuel
separately. -/
private theorem run_trans {Δ : DEnv} {defns : HashMap Int Defn}
    {E E' : Rwv.Hyle.Sem.EEnv} (hE : EExt E E') {ef ef' gf gf' : Nat}
    (hef : ef ≤ ef') (hgf : gf ≤ gf') {p : Proc} {inputs : List Val} {mt : MTrace}
    (h : Proc.run Δ defns ef gf p inputs E = .ok mt) :
    Proc.run Δ defns ef' gf' p inputs E' = .ok mt := by
  unfold Proc.run at h ⊢
  obtain ⟨σ₀, hσ, h⟩ := except_bind_eq_ok h
  obtain ⟨so, hso, h⟩ := except_bind_eq_ok h
  refine bind_ok (initCells_trans hE hef hσ) ?_
  refine bind_ok (execBlock_trans hE hef hgf hso) ?_
  cases so with
  | halt a => exact h
  | step o s₀ =>
      obtain ⟨tri, htri, h⟩ := except_bind_eq_ok h
      refine bind_ok (foldlM_mono (fun s a r hb => foldStep_trans hE hef hgf hb) htri) ?_
      exact h

theorem Proc.run_mono {Δ : DEnv} {defns : HashMap Int Defn} {E : Rwv.Hyle.Sem.EEnv}
    {ef ef' gf gf' : Nat}
    (hef : ef ≤ ef') (hgf : gf ≤ gf') {p : Proc} {inputs : List Val} {mt : MTrace}
    (h : Proc.run Δ defns ef gf p inputs E = .ok mt) :
    Proc.run Δ defns ef' gf' p inputs E = .ok mt :=
  run_trans (eext_refl E) hef hgf h

/-! ## The fuel-independence corollaries -/

/-- A successful `Proc.run` is stable under raising either fuel: the
canonical downstream form. -/
theorem Proc.run_fuel_stable {Δ : DEnv} {defns : HashMap Int Defn} {ef gf : Nat}
    {p : Proc} {ins : List Val} {mt : MTrace}
    (h : Proc.run Δ defns ef gf p ins = .ok mt) :
    ∀ ef' ≥ ef, ∀ gf' ≥ gf, Proc.run Δ defns ef' gf' p ins = .ok mt :=
  fun _ef' hef _gf' hgf => Proc.run_mono hef hgf h

/-- Any two successful `Proc.run`s agree, whatever their fuels: "∃
fuel, the run succeeds" is a fuel-independent semantics. -/
theorem Proc.run_fuel_deterministic {Δ : DEnv} {defns : HashMap Int Defn}
    {E : Rwv.Hyle.Sem.EEnv}
    {ef₁ gf₁ ef₂ gf₂ : Nat} {p : Proc} {ins : List Val} {mt₁ mt₂ : MTrace}
    (h₁ : Proc.run Δ defns ef₁ gf₁ p ins E = .ok mt₁)
    (h₂ : Proc.run Δ defns ef₂ gf₂ p ins E = .ok mt₂) : mt₁ = mt₂ := by
  have k₁ := Proc.run_mono (Nat.le_max_left ef₁ ef₂) (Nat.le_max_left gf₁ gf₂) h₁
  have k₂ := Proc.run_mono (Nat.le_max_right ef₁ ef₂) (Nat.le_max_right gf₁ gf₂) h₂
  exact Except.ok.inj (k₁.symm.trans k₂)

/-! ## The extern-environment transport (the η tier), machine layer

A successful run at the EMPTY extern environment never consulted it
(Rwv.Eidos.FuelMono's `eext_empty` instance of the generalized
transport), so it succeeds identically at EVERY extern environment —
what lets the validator discharge the initial-state obligation ONCE,
at the default environment, and certify it at the statement's
∀-quantified one (`checkInit_sound`'s transport). -/

/-- `Machine.initCells` at the empty extern environment transports to
any. -/
theorem Machine.initCells_eta {Δ : DEnv} {defns : HashMap Int Defn}
    (E : Rwv.Hyle.Sem.EEnv) {k : Nat} {p : Proc} {σ : HashMap String Val}
    (h : Machine.initCells Δ defns k p = .ok σ) :
    Machine.initCells Δ defns k p E = .ok σ :=
  initCells_trans (eext_empty E) (Nat.le_refl k) h

/-- `Machine.execBlock` at the empty extern environment transports to
any. -/
theorem Machine.execBlock_eta {Δ : DEnv} {defns : HashMap Int Defn}
    {blocks : HashMap Int Block} (E : Rwv.Hyle.Sem.EEnv) {ef gf : Nat}
    {env₀ : Eval.Env} {cells₀ : HashMap String Val} {b : Block} {r : StepOut}
    (h : Machine.execBlock Δ defns blocks ef gf env₀ cells₀ b = .ok r) :
    Machine.execBlock Δ defns blocks ef gf env₀ cells₀ b E = .ok r :=
  execBlock_trans (eext_empty E) (Nat.le_refl ef) (Nat.le_refl gf) h

/-- `Machine.step` at the empty extern environment transports to any. -/
theorem Machine.step_eta {Δ : DEnv} {defns : HashMap Int Defn}
    {blocks : HashMap Int Block} (E : Rwv.Hyle.Sem.EEnv) {ef gf : Nat}
    {s : MState} {input : Val} {r : StepOut}
    (h : Machine.step Δ defns blocks ef gf s input = .ok r) :
    Machine.step Δ defns blocks ef gf s input E = .ok r :=
  step_trans (eext_empty E) (Nat.le_refl ef) (Nat.le_refl gf) h

/-- `Proc.run` at the empty extern environment transports to any. -/
theorem Proc.run_eta {Δ : DEnv} {defns : HashMap Int Defn} (E : Rwv.Hyle.Sem.EEnv)
    {ef gf : Nat} {p : Proc} {ins : List Val} {mt : MTrace}
    (h : Proc.run Δ defns ef gf p ins = .ok mt) :
    Proc.run Δ defns ef gf p ins E = .ok mt :=
  run_trans (eext_empty E) (Nat.le_refl ef) (Nat.le_refl gf) h

end Rwv.Synolon

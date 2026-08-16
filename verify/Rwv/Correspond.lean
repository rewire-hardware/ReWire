/-
The Eidos↔Hyle correspondence, stated formally (doc/eidos.md §7.5.6):
well-typedness of semantic values, agreement of a machine trace with a
device trace through the representation function (up to and excluding
the halting cycle), and the top-level correspondence Prop over the two
mechanized semantics. This is the statement the differential harness
tests empirically per golden, and the statement the validator's
soundness theorem concludes (`validateProc_corresponds` and its ∀η
variant in Rwv.Eidos.Cstep, with `Rwv.Sim.simP_run` as the induction
connecting per-step obligations to it).
-/
import Rwv.Eidos.Machine
import Rwv.Hyle.Semantics
import Std.Data.HashMap

namespace Rwv.Eidos

open Std (HashMap)

/-- Well-typedness of a semantic value at a representable type
(doc/eidos.md §7.5.1): the inductive carving of `V_τ` out of `Val`.
Constructor values must be constructors of the type's datatype with
fields well-typed at the instantiated field types. -/
inductive Val.HasTy (Δ : DEnv) : Val → Ty → Prop where
  | vec {es : List Val} {t nt τ : Ty} {n : Nat}
      (hty : Ty.flatten t = (.con "Vec", [nt, τ]))
      (hn : Ty.evalNat nt = some n)
      (hlen : es.length = n)
      (helems : ∀ v ∈ es, Val.HasTy Δ v τ) :
      Val.HasTy Δ (.vec es) t
  | finite {t nt : Ty} {n i : Nat}
      (hty : Ty.flatten t = (.con "Finite", [nt]))
      (hn : Ty.evalNat nt = some n)
      (hlt : i < n) :
      Val.HasTy Δ (.finite n i) t
  | integer {t : Ty} {v : BitVec 128}
      (hty : Ty.flatten t = (.con "Integer", [])) :
      Val.HasTy Δ (.integer v) t
  | proxy {t nt : Ty}
      (hty : Ty.flatten t = (.con "Proxy", [nt])) :
      Val.HasTy Δ .proxy t
  | con {t : Ty} {tc c : String} {fields : List Val}
      {sig : Sig} {sub : HashMap TyVar Ty} {doms : List Ty}
      (hty : (Ty.flatten t).1 = .con tc)
      (hctor : ∃ cs, Δ.ctors.get? tc = some cs ∧ c ∈ cs)
      (hsig : Δ.ctorSig.get? c = some sig)
      (hmatch : DEnv.matchTy (Ty.flattenArrow sig.ty).2 t = .ok sub)
      (hdoms : doms = (Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub))
      (hlen : fields.length = doms.length)
      (hfields : ∀ p ∈ fields.zip doms, Val.HasTy Δ p.1 p.2) :
      Val.HasTy Δ (.con t c fields) t

/-- Agreement of a machine trace with a device trace through the
representation function: the rep-encoded machine outputs equal the
device outputs cycle for cycle; when the machine halted, they are a
prefix (the device's remaining cycles are unspecified, §7.5.4). -/
def TraceAgrees (Δ : DEnv) (fuel : Nat) (outTy : Ty)
    (mt : MTrace) (ht : List (List Rwv.Hyle.BV)) : Prop :=
  ∃ encOuts, mt.outs.mapM (Val.portSplit Δ fuel outTy) = .ok encOuts ∧
    match mt.halted with
    | none   => ht = encOuts
    | some _ => encOuts <+: ht

/-- The §7.5.6 correspondence between a machine-mode process and a
compiled Hyle program, over the two mechanized semantics: for every
well-typed input trace, whenever both sides run successfully — the
machine on the algebraic inputs, the device on their port-split
encodings — the traces agree through the representation function.
(Totality of both runs at sufficient fuel on well-formed programs is a
separate obligation, not mechanized; this is the agreement half.)

The η tier: the statement is parameterized by the bit-level
model-less-extern environment `E`, with BOTH runs reading the SAME
one — the machine through `Eval.evalExt`'s decode-gated foreign row,
the device through `evalExp`'s total `Sem.xapply` reading. The
algebraic quantification (∀ η_alg) is this parameter at
`rep ∘ η ∘ decode` instantiations (`Rwv.Eidos.Cstep.etaB`); the
validator's soundness theorem concludes the statement for EVERY `E`,
which is the ∀η reading. At the default (empty) environment the
definition is exactly the pre-extension statement. -/
def Corresponds (Δ : DEnv) (defns : HashMap Int Defn) (evalFuel gotoFuel : Nat)
    (p : Proc) (H : Rwv.Hyle.Program)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Prop :=
  ∀ (ins : List Val), (∀ v ∈ ins, Val.HasTy Δ v p.inTy) →
    ∀ encIns, ins.mapM (Val.portSplit Δ evalFuel p.inTy) = .ok encIns →
    ∀ mt, Proc.run Δ defns evalFuel gotoFuel p ins E = .ok mt →
    ∀ ht, H.run encIns E = .ok ht →
    TraceAgrees Δ evalFuel p.outTy mt ht

/-- Forward refinement: strictly stronger than `Corresponds` — a
successful, well-typed source execution ENTAILS a successful target
execution, whose trace agrees. A target that can never run cannot
satisfy this statement; vacuity is structurally impossible rather than
gated away. (`Rwv.Hyle.Progress` supplies the target-run existence on
checked programs; `validateBundle_refines` concludes this statement
from a `.validated` bundle result.) -/
def Refines (Δ : DEnv) (defns : HashMap Int Defn) (evalFuel gotoFuel : Nat)
    (p : Proc) (H : Rwv.Hyle.Program)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Prop :=
  ∀ (ins : List Val), (∀ v ∈ ins, Val.HasTy Δ v p.inTy) →
    ∀ encIns, ins.mapM (Val.portSplit Δ evalFuel p.inTy) = .ok encIns →
    ∀ mt, Proc.run Δ defns evalFuel gotoFuel p ins E = .ok mt →
    ∃ ht, H.run encIns E = .ok ht ∧ TraceAgrees Δ evalFuel p.outTy mt ht

/-- Refinement subsumes the conditional agreement: the device's run is
a function of its inputs, so the entailed trace IS any hypothesized
one. -/
theorem Refines.corresponds {Δ : DEnv} {defns : HashMap Int Defn}
    {evalFuel gotoFuel : Nat} {p : Proc} {H : Rwv.Hyle.Program}
    {E : Rwv.Hyle.Sem.EEnv}
    (h : Refines Δ defns evalFuel gotoFuel p H E) :
    Corresponds Δ defns evalFuel gotoFuel p H E := by
  intro ins hty encIns henc mt hmt ht hht
  obtain ⟨ht', hht', hagree⟩ := h ins hty encIns henc mt hmt
  rw [hht] at hht'
  cases hht'
  exact hagree

/-! ## The zero value is well typed

`DEnv.zeroVal` (doc/eidos.md §7.5.1) fails on every type with no
values — `Finite 0` (uninhabited, matching Data.Finite), empty
datatypes, open bounds — so its success is itself the inhabitation
witness: whatever it returns inhabits the requested type,
unconditionally. Its call sites (undef cell initialization in
`Machine.cells0`, the evaluator's live-error row) discharge the
premise with their own success hypotheses. -/

private theorem error_ne_ok' {α : Type} {msg : String} {a : α} {P : Prop}
    (h : (Except.error msg : Except String α) = .ok a) : P := by
  cases h

private theorem bind_eq_ok' {α β : Type} {x : Except String α}
    {f : α → Except String β} {b : β} (h : (x >>= f) = .ok b) :
    ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x with
  | error e => exact error_ne_ok' h
  | ok a => exact ⟨a, rfl, h⟩

/-- Pointwise typing of a `zeroVal`-filled field telescope. -/
private theorem zeroVal_fields {Δ : DEnv} {fuel : Nat}
    (ih : ∀ {t : Ty} {v : Val}, DEnv.zeroVal Δ fuel t = .ok v → Val.HasTy Δ v t)
    (g : Ty → Ty) :
    ∀ {ts : List Ty} {vs : List Val},
      ts.mapM (fun ta => DEnv.zeroVal Δ fuel (g ta)) = .ok vs →
      vs.length = ts.length ∧ ∀ p ∈ vs.zip (ts.map g), Val.HasTy Δ p.1 p.2 := by
  intro ts
  induction ts with
  | nil =>
      intro vs h
      rw [List.mapM_nil] at h
      injection h with h
      subst h
      exact ⟨rfl, by simp⟩
  | cons ta ts iht =>
      intro vs h
      rw [List.mapM_cons] at h
      obtain ⟨b, hb, h⟩ := bind_eq_ok' h
      obtain ⟨bs, hbs, h⟩ := bind_eq_ok' h
      injection h with h
      obtain ⟨hlen, htys⟩ := iht hbs
      refine h ▸ ⟨by simpa using hlen, ?_⟩
      intro p hp
      rw [List.map_cons, List.zip_cons_cons] at hp
      rcases List.mem_cons.mp hp with hp | hp
      · rw [hp]
        exact ih hb
      · exact htys p hp

/-- Whatever `zeroVal` returns inhabits the requested type. -/
theorem DEnv.zeroVal_hasTy {Δ : DEnv} :
    ∀ {fuel : Nat} {t : Ty} {v : Val},
      DEnv.zeroVal Δ fuel t = .ok v → Val.HasTy Δ v t := by
  intro fuel
  induction fuel with
  | zero =>
      intro t v h
      rw [DEnv.zeroVal] at h
      exact error_ne_ok' h
  | succ fuel ih =>
      intro t v h
      rw [DEnv.zeroVal] at h
      split at h
      · -- Vec
        rename_i n te hfl
        split at h
        · rename_i k hk
          obtain ⟨z, hz, h⟩ := bind_eq_ok' h
          injection h with h
          subst h
          exact .vec hfl hk (List.length_replicate ..) fun v hv => by
            rw [List.eq_of_mem_replicate hv]; exact ih hz
        · exact error_ne_ok' h
      · -- Finite
        rename_i nt hfl
        split at h
        · rename_i k hk
          injection h with h
          subst h
          exact .finite hfl hk (Nat.succ_pos k)
        · exact error_ne_ok' h
        · exact error_ne_ok' h
      · -- Integer
        rename_i hfl
        injection h with h
        subst h
        exact .integer hfl
      · -- Proxy
        rename_i x hfl
        injection h with h
        subst h
        exact .proxy hfl
      · -- datatype: first constructor, zero fields (the pattern
        -- overlaps the four builtin-head arms, so the scrutinee
        -- equation follows their exclusion witnesses)
        rename_i c args _ _ _ _ hfl
        split at h
        · rename_i c₀ rest hget
          split at h
          · rename_i sig hsig
            obtain ⟨sub, hsub, h⟩ := bind_eq_ok' h
            obtain ⟨fields, hfields, h⟩ := bind_eq_ok' h
            injection h with h
            subst h
            obtain ⟨hlen, htys⟩ := zeroVal_fields @ih (DEnv.substTv sub) hfields
            exact .con (tc := c) (by rw [hfl])
              ⟨c₀ :: rest, hget, List.mem_cons_self ..⟩ hsig hsub rfl
              (by rw [hlen, List.length_map]) htys
          · exact error_ne_ok' h
        · exact error_ne_ok' h
        · exact error_ne_ok' h
      · exact error_ne_ok' h

/- `Finite 0` is uninhabited (matching Data.Finite): its would-be sole
representative is ill typed, and no zero value exists — at bound zero
or at an open bound. -/
example {Δ : DEnv} :
    ¬ Val.HasTy Δ (.finite 0 0) (.app (.con "Finite") (.nat 0)) := by
  intro h
  cases h with
  | finite _ _ hlt => exact absurd hlt (by omega)

example {Δ : DEnv} {fuel : Nat} :
    DEnv.zeroVal Δ (fuel + 1) (.app (.con "Finite") (.nat 0))
      = .error "zeroVal: Finite 0 is uninhabited" := by
  rw [DEnv.zeroVal]
  rfl

example {Δ : DEnv} {fuel : Nat} {a : TyVar} :
    DEnv.zeroVal Δ (fuel + 1) (.app (.con "Finite") (.var a))
      = .error "zeroVal: open Finite bound" := by
  rw [DEnv.zeroVal]
  rfl

#print axioms Rwv.Eidos.DEnv.zeroVal_hasTy

end Rwv.Eidos

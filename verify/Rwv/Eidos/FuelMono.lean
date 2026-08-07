/-
Fuel monotonicity for the Eidos-M semantics: every fueled function in
the value layer (Rwv.Eidos.Value), the pure evaluator (Rwv.Eidos.Eval),
and the machine semantics (Rwv.Eidos.Machine) preserves successful
results under more fuel — `fuel ≤ fuel' → f fuel … = .ok v →
f fuel' … = .ok v`, with the SAME value. Consequently "∃ fuel, the run
succeeds" is a canonical fuel-independent semantics: a successful run
is stable under raising either fuel parameter (`Proc.run_fuel_stable`),
and any two successful runs at any fuels agree
(`Proc.run_fuel_deterministic`).

Proof structure:
- Generic success-preservation combinators for `Except`-monadic
  traversals (`mapM`, `foldlM`, `forIn`) over pointwise
  success-preserving bodies.
- The value layer (`DEnv.sizeOf`/`DEnv.ctorWidth` mutually,
  `DEnv.zeroVal`, `Val.rep`) by plain induction on the smaller fuel:
  each function consumes one unit on entry and passes the decrement to
  every recursive call, so the induction hypothesis at the decrement
  covers every recursive position.
- The evaluator's eight-function mutual block by one simultaneous
  induction on the smaller fuel, packaged as an `EvalMono` bundle (the
  eight monotonicity statements at a fuel pair); the 64-row builtin
  dispatch reduces to the value-layer lemmas and the bundle.
- The machine layer compositionally, monotone in the evaluation fuel
  and the goto fuel separately (`execBlock`'s terminator runner by
  induction on the goto fuel, which every recursive position
  decrements).
-/
import Rwv.Eidos.Machine

namespace Rwv.Eidos

open Std (HashMap)
open Rwv.Hyle (BV)

/-! ## `Except` plumbing and traversal combinators -/

/-- Forward construction of a successful bind from its components. -/
private theorem bind_ok {α β : Type} {x : Except String α} {f : α → Except String β}
    {a : α} {b : β} (hx : x = .ok a) (hf : f a = .ok b) : (x >>= f) = .ok b := by
  rw [hx]; exact hf

/-- An error is never a success (stated over an arbitrary conclusion so
it can close any goal from an impossible hypothesis; `throw` reduces to
`Except.error`). -/
private theorem error_ne_ok {α : Type} {msg : String} {a : α} {P : Prop}
    (h : (Except.error msg : Except String α) = .ok a) : P := by
  cases h

/-- Inversion for a successful `Except` bind: the first computation
succeeded and the continuation took its value to the result. -/
private theorem except_bind_eq_ok {α β : Type} {x : Except String α}
    {f : α → Except String β} {b : β} (h : (x >>= f) = .ok b) :
    ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x with
  | error e => exact error_ne_ok h
  | ok a => exact ⟨a, rfl, h⟩

/-- Success preservation through `List.mapM`: a pointwise
success-preserving replacement of the body preserves the traversal's
successful result. -/
private theorem mapM_mono {α β : Type} {g g' : α → Except String β}
    (hg : ∀ a b, g a = .ok b → g' a = .ok b) :
    ∀ {xs : List α} {ys : List β}, xs.mapM g = .ok ys → xs.mapM g' = .ok ys := by
  intro xs
  induction xs with
  | nil => intro ys h; simpa using h
  | cons a as ih =>
      intro ys h
      rw [List.mapM_cons] at h ⊢
      obtain ⟨b, hb, h⟩ := except_bind_eq_ok h
      obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
      refine bind_ok (hg a b hb) ?_
      refine bind_ok (ih hbs) ?_
      exact h

/-- Success preservation through `List.foldlM`. -/
private theorem foldlM_mono {σ α : Type} {g g' : σ → α → Except String σ}
    (hg : ∀ s a r, g s a = .ok r → g' s a = .ok r) :
    ∀ {xs : List α} {s r : σ}, xs.foldlM g s = .ok r → xs.foldlM g' s = .ok r := by
  intro xs
  induction xs with
  | nil => intro s r h; simpa using h
  | cons a as ih =>
      intro s r h
      rw [List.foldlM_cons] at h ⊢
      obtain ⟨s', hs, h⟩ := except_bind_eq_ok h
      refine bind_ok (hg s a s' hs) ?_
      exact ih h

/-- Success preservation through `List.forIn` (the shape `do`-notation
`for` loops elaborate to). -/
private theorem forIn_mono {α σ : Type} {g g' : α → σ → Except String (ForInStep σ)}
    (hg : ∀ a s r, g a s = .ok r → g' a s = .ok r) :
    ∀ {xs : List α} {s r : σ}, forIn xs s g = .ok r → forIn xs s g' = .ok r := by
  intro xs
  induction xs with
  | nil => intro s r h; simpa using h
  | cons a as ih =>
      intro s r h
      rw [List.forIn_cons] at h ⊢
      obtain ⟨step, hstep, h⟩ := except_bind_eq_ok h
      refine bind_ok (hg a s step hstep) ?_
      cases step with
      | done b => exact h
      | yield b => exact ih h

/-! ## The value layer (Rwv.Eidos.Value)

`DEnv.sizeOf` and `DEnv.ctorWidth` are mutually recursive on fuel;
`DEnv.zeroVal` recurses on fuel alone; `Val.rep` recurses on fuel and
calls `DEnv.sizeOf` at the undecremented fuel. -/

private theorem sizeOf_ctorWidth_mono (Δ : DEnv) :
    ∀ k k', k ≤ k' →
      (∀ visited t n, Δ.sizeOf k visited t = .ok n → Δ.sizeOf k' visited t = .ok n) ∧
      (∀ visited t c n, Δ.ctorWidth k visited t c = .ok n → Δ.ctorWidth k' visited t c = .ok n) := by
  intro k
  induction k with
  | zero =>
      intro k' _
      refine ⟨fun visited t n h => ?_, fun visited t c n h => ?_⟩
      · rw [DEnv.sizeOf] at h; exact error_ne_ok h
      · rw [DEnv.ctorWidth] at h; exact error_ne_ok h
  | succ k ihk =>
      intro k' hkk'
      obtain ⟨k', rfl⟩ : ∃ j, k' = j + 1 := ⟨k' - 1, by omega⟩
      obtain ⟨ihS, ihC⟩ := ihk k' (by omega)
      constructor
      · intro visited t n h
        rw [DEnv.sizeOf] at h ⊢
        split at h
        · -- Vec
          split at h
          · obtain ⟨w, hw, h⟩ := except_bind_eq_ok h
            refine bind_ok (ihS _ _ _ hw) ?_
            exact h
          · exact error_ne_ok h
        · -- Finite
          exact h
        · -- Integer
          exact h
        · -- Proxy
          exact h
        · -- con c args: tuple / recursive check / datatype
          split at h <;> rename_i htup
          · rw [if_pos htup]
            obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
            refine bind_ok (mapM_mono (fun a b hab => ihS _ _ _ hab) hws) ?_
            exact h
          · rw [if_neg htup]
            split at h <;> rename_i hvis
            · exact error_ne_ok h
            · rw [if_neg hvis]
              split at h
              · obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
                refine bind_ok (mapM_mono (fun a b hab => ihC _ _ _ _ hab) hws) ?_
                exact h
              · exact error_ne_ok h
        · -- var
          exact h
        · -- fallthrough
          exact error_ne_ok h
      · intro visited t c n h
        rw [DEnv.ctorWidth] at h ⊢
        split at h
        · -- some sig
          split at h  -- let (targs, tres) := Ty.flattenArrow sig.ty
          obtain ⟨sub, hsub, h⟩ := except_bind_eq_ok h
          obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
          refine bind_ok hsub ?_
          refine bind_ok (mapM_mono (fun a b hab => ihS _ _ _ hab) hws) ?_
          exact h
        · -- none
          exact h

/-- `DEnv.sizeOf` is monotone in its fuel. -/
theorem DEnv.sizeOf_mono (Δ : DEnv) {k k' : Nat} (hk : k ≤ k') {visited : List Ty}
    {t : Ty} {n : Nat} (h : Δ.sizeOf k visited t = .ok n) : Δ.sizeOf k' visited t = .ok n :=
  (sizeOf_ctorWidth_mono Δ k k' hk).1 visited t n h

/-- `DEnv.ctorWidth` is monotone in its fuel. -/
theorem DEnv.ctorWidth_mono (Δ : DEnv) {k k' : Nat} (hk : k ≤ k') {visited : List Ty}
    {t : Ty} {c : String} {n : Nat} (h : Δ.ctorWidth k visited t c = .ok n) :
    Δ.ctorWidth k' visited t c = .ok n :=
  (sizeOf_ctorWidth_mono Δ k k' hk).2 visited t c n h

/-- `DEnv.zeroVal` is monotone in its fuel. -/
theorem DEnv.zeroVal_mono (Δ : DEnv) :
    ∀ {k k'}, k ≤ k' → ∀ {t v}, Δ.zeroVal k t = .ok v → Δ.zeroVal k' t = .ok v := by
  intro k
  induction k with
  | zero => intro k' _ t v h; rw [DEnv.zeroVal] at h; exact error_ne_ok h
  | succ k ihk =>
      intro k' hkk' t v h
      obtain ⟨k', rfl⟩ : ∃ j, k' = j + 1 := ⟨k' - 1, by omega⟩
      have hk : k ≤ k' := by omega
      rw [DEnv.zeroVal] at h ⊢
      split at h
      · -- Vec
        split at h
        · obtain ⟨z, hz, h⟩ := except_bind_eq_ok h
          refine bind_ok (ihk hk hz) ?_
          exact h
        · exact error_ne_ok h
      · -- Finite
        exact h
      · -- Integer
        exact h
      · -- Proxy
        exact h
      · -- datatype: first constructor applied to zero fields
        split at h
        · -- some (c₀ :: _)
          split at h
          · -- some sig
            split at h  -- let (targs, tres) := Ty.flattenArrow sig.ty
            obtain ⟨sub, hsub, h⟩ := except_bind_eq_ok h
            obtain ⟨fields, hfields, h⟩ := except_bind_eq_ok h
            refine bind_ok hsub ?_
            refine bind_ok (mapM_mono (fun a b hab => ihk hk hab) hfields) ?_
            exact h
          · exact error_ne_ok h
        · exact error_ne_ok h
        · exact error_ne_ok h
      · -- fallthrough
        exact error_ne_ok h

/-- `Val.rep` is monotone in its fuel. -/
theorem Val.rep_mono (Δ : DEnv) :
    ∀ {k k'}, k ≤ k' → ∀ {v x}, Val.rep Δ k v = .ok x → Val.rep Δ k' v = .ok x := by
  intro k
  induction k with
  | zero => intro k' _ v x h; rw [Val.rep] at h; exact error_ne_ok h
  | succ k ihk =>
      intro k' hkk' v x h
      obtain ⟨k', rfl⟩ : ∃ j, k' = j + 1 := ⟨k' - 1, by omega⟩
      have hk : k ≤ k' := by omega
      cases v with
      | vec elems =>
          rw [Val.rep] at h ⊢
          obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
          refine bind_ok (mapM_mono ?_ hbs) ?_
          · rintro ⟨e, he⟩ b hab
            exact ihk hk hab
          · exact h
      | integer n => rw [Val.rep] at h ⊢; exact h
      | finite bound val => rw [Val.rep] at h ⊢; exact h
      | str s => rw [Val.rep] at h; exact error_ne_ok h
      | proxy => rw [Val.rep] at h ⊢; exact h
      | con ty c fields =>
          rw [Val.rep] at h ⊢
          obtain ⟨whole, hwhole, h⟩ := except_bind_eq_ok h
          obtain ⟨tg, htg, h⟩ := except_bind_eq_ok h
          obtain ⟨tag, tagW⟩ := tg
          obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
          refine bind_ok (Δ.sizeOf_mono (Nat.succ_le_succ hk) hwhole) ?_
          refine bind_ok htg ?_
          refine bind_ok (mapM_mono ?_ hbs) ?_
          · rintro ⟨e, he⟩ b hab
            exact ihk hk hab
          · exact h
      | closL y env body => rw [Val.rep] at h; exact error_ne_ok h
      | closD f pre => rw [Val.rep] at h; exact error_ne_ok h

/-! ## The decoder (Rwv.Eidos.Decode) -/

private theorem decodeFields_mono_of {Δ : DEnv} {k k' : Nat}
    (IH : ∀ {t : Ty} {bv : BV} {v : Val},
      decode Δ k t bv = .ok v → decode Δ k' t bv = .ok v) :
    ∀ {tws : List (Ty × Nat)} {bv : BV} {vs : List Val},
      decodeFields Δ k tws bv = .ok vs → decodeFields Δ k' tws bv = .ok vs := by
  intro tws
  induction tws with
  | nil => intro bv vs h; rw [decodeFields] at h ⊢; exact h
  | cons tw rest ih =>
      intro bv vs h
      obtain ⟨t, w⟩ := tw
      rw [decodeFields] at h ⊢
      split at h
      rotate_left
      · exact error_ne_ok h
      rename_i hw
      rw [if_pos hw]
      obtain ⟨v, hv, h⟩ := except_bind_eq_ok h
      obtain ⟨vs', hvs, h⟩ := except_bind_eq_ok h
      exact bind_ok (IH hv) (bind_ok (ih hvs) h)

/-- `decode` is monotone in its fuel: success is stable, with the same
value, under more fuel. -/
theorem decode_mono {Δ : DEnv} :
    ∀ {k k' : Nat}, k ≤ k' → ∀ {t : Ty} {bv : BV} {v : Val},
      decode Δ k t bv = .ok v → decode Δ k' t bv = .ok v := by
  intro k
  induction k with
  | zero => intro k' _ t bv v h; rw [decode] at h; exact error_ne_ok h
  | succ k ihk =>
      intro k' hkk' t bv v h
      obtain ⟨k', rfl⟩ : ∃ j, k' = j + 1 := ⟨k' - 1, by omega⟩
      have hk : k ≤ k' := by omega
      rw [decode] at h ⊢
      split at h
      · -- Vec
        split at h
        rotate_left
        · exact error_ne_ok h
        obtain ⟨we, hwe, h⟩ := except_bind_eq_ok h
        refine bind_ok (Δ.sizeOf_mono hk hwe) ?_
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hw
        rw [if_pos hw]
        obtain ⟨fields, hfs, h⟩ := except_bind_eq_ok h
        exact bind_ok (decodeFields_mono_of (fun {t bv v} h' => ihk hk h') hfs) h
      · -- Finite: fuel-free
        exact h
      · -- Integer: fuel-free
        exact h
      · -- Proxy: fuel-free
        exact h
      · -- Datatype
        obtain ⟨whole, hwhole, h⟩ := except_bind_eq_ok h
        refine bind_ok (Δ.sizeOf_mono (Nat.succ_le_succ hk) hwhole) ?_
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hbw
        rw [if_pos hbw]
        obtain ⟨cw, hcw, h⟩ := except_bind_eq_ok h
        refine bind_ok hcw ?_
        obtain ⟨tt, htt, h⟩ := except_bind_eq_ok h
        refine bind_ok htt ?_
        obtain ⟨tag, tagW'⟩ := tt
        dsimp only at h ⊢
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i htw
        rw [if_pos htw]
        split at h
        rotate_left
        · exact error_ne_ok h
        obtain ⟨sub, hsub, h⟩ := except_bind_eq_ok h
        refine bind_ok hsub ?_
        obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
        refine bind_ok (mapM_mono (fun a b hab => Δ.sizeOf_mono hk hab) hws) ?_
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hle
        rw [if_pos hle]
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hpad
        rw [if_pos hpad]
        obtain ⟨fields, hfs, h⟩ := except_bind_eq_ok h
        exact bind_ok (decodeFields_mono_of (fun {t bv v} h' => ihk hk h') hfs) h
      · exact error_ne_ok h

/-! ## The fueled evaluator helpers outside the mutual block -/

/-- `Eval.valToBits` is monotone in its fuel. -/
theorem Eval.valToBits_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k') {v : Val} {x : BV}
    (h : Eval.valToBits Δ k v = .ok x) : Eval.valToBits Δ k' v = .ok x :=
  Val.rep_mono Δ hk h

/-- `Eval.litMatches` is monotone in its fuel. -/
theorem Eval.litMatches_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k') {v : Val} {n : Int}
    {b : Bool} (h : Eval.litMatches Δ k v n = .ok b) : Eval.litMatches Δ k' v n = .ok b := by
  unfold Eval.litMatches at h ⊢
  obtain ⟨x, hx, h⟩ := except_bind_eq_ok h
  refine bind_ok (Eval.valToBits_mono hk hx) ?_
  exact h

/-- `Eval.bvBinArith` is monotone in its fuel. -/
theorem Eval.bvBinArith_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k') {op : Rwv.Hyle.Op}
    {v w r : Val} (h : Eval.bvBinArith Δ k op v w = .ok r) :
    Eval.bvBinArith Δ k' op v w = .ok r := by
  unfold Eval.bvBinArith at h ⊢
  obtain ⟨x, hx, h⟩ := except_bind_eq_ok h
  obtain ⟨y, hy, h⟩ := except_bind_eq_ok h
  refine bind_ok (Eval.valToBits_mono hk hx) ?_
  refine bind_ok (Eval.valToBits_mono hk hy) ?_
  exact h

/-- `Eval.bvBinCmp` is monotone in its fuel. -/
theorem Eval.bvBinCmp_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k') {op : Rwv.Hyle.Op}
    {v w r : Val} (h : Eval.bvBinCmp Δ k op v w = .ok r) :
    Eval.bvBinCmp Δ k' op v w = .ok r := by
  unfold Eval.bvBinCmp at h ⊢
  obtain ⟨x, hx, h⟩ := except_bind_eq_ok h
  obtain ⟨y, hy, h⟩ := except_bind_eq_ok h
  refine bind_ok (Eval.valToBits_mono hk hx) ?_
  refine bind_ok (Eval.valToBits_mono hk hy) ?_
  exact h

/-- `Eval.bvRed` is monotone in its fuel. -/
theorem Eval.bvRed_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k') {op : Rwv.Hyle.Op}
    {negated : Bool} {v r : Val} (h : Eval.bvRed Δ k op negated v = .ok r) :
    Eval.bvRed Δ k' op negated v = .ok r := by
  unfold Eval.bvRed at h ⊢
  obtain ⟨x, hx, h⟩ := except_bind_eq_ok h
  refine bind_ok (Eval.valToBits_mono hk hx) ?_
  exact h

/-! ## The evaluator's mutual block (Rwv.Eidos.Eval)

One simultaneous induction on the smaller fuel: `EvalMono C k k'`
bundles the eight monotonicity statements at a fuel pair; every
function consumes one unit of fuel on entry and passes the decrement
to every recursive call, so the step lemma at `k+1 ≤ k'+1` needs
exactly the bundle at `k ≤ k'`. -/

private structure EvalMono (C : Eval.Ctx) (k k' : Nat) : Prop where
  core : ∀ env jenv e v, Eval.evalCore C k env jenv e = .ok v →
    Eval.evalCore C k' env jenv e = .ok v
  list : ∀ env jenv es vs, Eval.evalList C k env jenv es = .ok vs →
    Eval.evalList C k' env jenv es = .ok vs
  defn : ∀ d vs v, Eval.callDefn C k d vs = .ok v → Eval.callDefn C k' d vs = .ok v
  app1 : ∀ f a v, Eval.applyValCore C k f a = .ok v → Eval.applyValCore C k' f a = .ok v
  many : ∀ f as v, Eval.applyMany C k f as = .ok v → Eval.applyMany C k' f as = .ok v
  all : ∀ f xs ys, Eval.applyAll C k f xs = .ok ys → Eval.applyAll C k' f xs = .ok ys
  alts : ∀ env jenv binder sv as dflt v, Eval.tryAlts C k env jenv binder sv as dflt = .ok v →
    Eval.tryAlts C k' env jenv binder sv as dflt = .ok v
  builtin : ∀ ty b vs v, Eval.evalBuiltin C k ty b vs = .ok v →
    Eval.evalBuiltin C k' ty b vs = .ok v
  cry : ∀ env jenv pty f n rest v, Eval.evalCry C k env jenv pty f n rest = .ok v →
    Eval.evalCry C k' env jenv pty f n rest = .ok v
  ext : ∀ env jenv pty s rest v, Eval.evalExt C k env jenv pty s rest = .ok v →
    Eval.evalExt C k' env jenv pty s rest = .ok v

/-- At fuel 0 every evaluator function throws, so the bundle holds
vacuously. -/
private theorem evalMono_zero (C : Eval.Ctx) (k' : Nat) : EvalMono C 0 k' where
  core := by intro env jenv e v h; rw [Eval.evalCore] at h; exact error_ne_ok h
  list := by intro env jenv es vs h; rw [Eval.evalList] at h; exact error_ne_ok h
  defn := by intro d vs v h; rw [Eval.callDefn] at h; exact error_ne_ok h
  app1 := by intro f a v h; rw [Eval.applyValCore] at h; exact error_ne_ok h
  many := by intro f as v h; rw [Eval.applyMany] at h; exact error_ne_ok h
  all := by intro f xs ys h; rw [Eval.applyAll] at h; exact error_ne_ok h
  alts := by intro env jenv binder sv as dflt v h; rw [Eval.tryAlts] at h; exact error_ne_ok h
  builtin := by intro ty b vs v h; rw [Eval.evalBuiltin] at h; exact error_ne_ok h
  cry := by intro env jenv pty f n rest v h; rw [Eval.evalCry] at h; exact error_ne_ok h
  ext := by intro env jenv pty s rest v h; rw [Eval.evalExt] at h; exact error_ne_ok h

private theorem evalCore_step {C : Eval.Ctx} {k k' : Nat} (_hk : k ≤ k')
    (ih : EvalMono C k k') :
    ∀ env jenv e v, Eval.evalCore C (k + 1) env jenv e = .ok v →
      Eval.evalCore C (k' + 1) env jenv e = .ok v := by
  intro env jenv e v h
  rw [Eval.evalCore] at h ⊢
  split at h
  · -- (.var x, args)
    rename_i x args _heq
    obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
    refine bind_ok (ih.list _ _ _ _ hvs) ?_
    cases hL : List.lookup x.uniq env with
    | some w =>
        rw [hL] at h
        exact ih.many _ _ _ h
    | none =>
        rw [hL] at h
        cases hD : C.defns.get? x.uniq with
        | some d => rw [hD] at h; exact ih.defn _ _ _ h
        | none => rw [hD] at h; exact error_ne_ok h
  · -- (.con ty c, args): fuel-free tail after the argument list
    obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
    refine bind_ok (ih.list _ _ _ _ hvs) ?_
    exact h
  · -- (.prim ty b, args): the foreign dispatch, then the generic row
    rename_i ty b args _heq
    by_cases hbc : b == .cryptol
    · rw [if_pos hbc] at h ⊢
      split at h
      · exact ih.cry _ _ _ _ _ _ _ h
      · exact error_ne_ok h
    · rw [if_neg hbc] at h ⊢
      by_cases hbx : b == .«extern»
      · rw [if_pos hbx] at h ⊢
        split at h
        · exact ih.ext _ _ _ _ _ _ h
        · exact error_ne_ok h
      · rw [if_neg hbx] at h ⊢
        obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
        refine bind_ok (ih.list _ _ _ _ hvs) ?_
        exact ih.builtin _ _ _ _ h
  · -- (.lam x body, args)
    obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
    refine bind_ok (ih.list _ _ _ _ hvs) ?_
    exact ih.many _ _ _ h
  · -- (.litInt ty n, [])
    exact h
  · -- (.litStr s, [])
    exact h
  · -- (.litVec ty es, [])
    obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
    refine bind_ok (ih.list _ _ _ _ hvs) ?_
    exact h
  · -- (.litList ty es, [])
    obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
    refine bind_ok (ih.list _ _ _ _ hvs) ?_
    exact h
  · -- (.letE bnd body, args)
    rename_i bnd body args _heq
    cases bnd with
    | nonRec x rhs =>
        obtain ⟨w, hw, h⟩ := except_bind_eq_ok h
        obtain ⟨rv, hrv, hw⟩ := except_bind_eq_ok hw
        obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
        refine bind_ok (bind_ok (ih.core _ _ _ _ hrv) (ih.core _ _ _ _ hw)) ?_
        refine bind_ok (ih.list _ _ _ _ hvs) ?_
        exact ih.many _ _ _ h
    | recB bs => exact error_ne_ok h
    | join l ps jbody =>
        obtain ⟨w, hw, h⟩ := except_bind_eq_ok h
        obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
        refine bind_ok (ih.core _ _ _ _ hw) ?_
        refine bind_ok (ih.list _ _ _ _ hvs) ?_
        exact ih.many _ _ _ h
  · -- (.jump l es, [])
    rename_i l es _heq
    obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
    refine bind_ok (ih.list _ _ _ _ hvs) ?_
    cases hJ : List.lookup l.uniq jenv with
    | some jc =>
        obtain ⟨ps, cenv, cjenv, jbody⟩ := jc
        rw [hJ] at h
        dsimp only [] at h ⊢
        split at h
        · rename_i hlen
          rw [if_pos hlen]
          exact ih.core _ _ _ _ h
        · exact error_ne_ok h
    | none =>
        rw [hJ] at h
        exact error_ne_ok h
  · -- (.cases ty scrut binder alts, args)
    obtain ⟨sv, hsv, h⟩ := except_bind_eq_ok h
    obtain ⟨w, hw, h⟩ := except_bind_eq_ok h
    obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
    refine bind_ok (ih.core _ _ _ _ hsv) ?_
    refine bind_ok (ih.alts _ _ _ _ _ _ _ hw) ?_
    refine bind_ok (ih.list _ _ _ _ hvs) ?_
    exact ih.many _ _ _ h
  all_goals exact error_ne_ok h

private theorem evalList_step {C : Eval.Ctx} {k k' : Nat} (_hk : k ≤ k')
    (ih : EvalMono C k k') :
    ∀ env jenv es vs, Eval.evalList C (k + 1) env jenv es = .ok vs →
      Eval.evalList C (k' + 1) env jenv es = .ok vs := by
  intro env jenv es vs h
  cases es with
  | nil => rw [Eval.evalList] at h ⊢; exact h
  | cons e rest =>
      rw [Eval.evalList] at h ⊢
      obtain ⟨w, hw, h⟩ := except_bind_eq_ok h
      obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
      refine bind_ok (ih.core _ _ _ _ hw) ?_
      refine bind_ok (ih.list _ _ _ _ hws) ?_
      exact h

private theorem callDefn_step {C : Eval.Ctx} {k k' : Nat} (_hk : k ≤ k')
    (ih : EvalMono C k k') :
    ∀ d vs v, Eval.callDefn C (k + 1) d vs = .ok v →
      Eval.callDefn C (k' + 1) d vs = .ok v := by
  intro d vs v h
  rw [Eval.callDefn] at h ⊢
  split at h <;> rename_i hlt
  · rw [if_pos hlt]
    exact h
  · rw [if_neg hlt]
    obtain ⟨w, hw, h⟩ := except_bind_eq_ok h
    refine bind_ok (ih.core _ _ _ _ hw) ?_
    exact ih.many _ _ _ h

private theorem applyValCore_step {C : Eval.Ctx} {k k' : Nat} (_hk : k ≤ k')
    (ih : EvalMono C k k') :
    ∀ f a v, Eval.applyValCore C (k + 1) f a = .ok v →
      Eval.applyValCore C (k' + 1) f a = .ok v := by
  intro f a v h
  cases f
  case closL x cenv body =>
    rw [Eval.applyValCore] at h ⊢
    exact ih.core _ _ _ _ h
  case closD g pre =>
    rw [Eval.applyValCore] at h ⊢
    cases hD : C.defns.get? g.uniq with
    | some d => rw [hD] at h; exact ih.defn _ _ _ h
    | none => rw [hD] at h; exact error_ne_ok h
  all_goals simp [Eval.applyValCore] at h

private theorem applyMany_step {C : Eval.Ctx} {k k' : Nat} (_hk : k ≤ k')
    (ih : EvalMono C k k') :
    ∀ f as v, Eval.applyMany C (k + 1) f as = .ok v →
      Eval.applyMany C (k' + 1) f as = .ok v := by
  intro f as v h
  cases as with
  | nil => rw [Eval.applyMany] at h ⊢; exact h
  | cons a rest =>
      rw [Eval.applyMany] at h ⊢
      obtain ⟨w, hw, h⟩ := except_bind_eq_ok h
      refine bind_ok (ih.app1 _ _ _ hw) ?_
      exact ih.many _ _ _ h

private theorem applyAll_step {C : Eval.Ctx} {k k' : Nat} (_hk : k ≤ k')
    (ih : EvalMono C k k') :
    ∀ f xs ys, Eval.applyAll C (k + 1) f xs = .ok ys →
      Eval.applyAll C (k' + 1) f xs = .ok ys := by
  intro f xs ys h
  cases xs with
  | nil => rw [Eval.applyAll] at h ⊢; exact h
  | cons x rest =>
      rw [Eval.applyAll] at h ⊢
      obtain ⟨y, hy, h⟩ := except_bind_eq_ok h
      obtain ⟨ys', hys, h⟩ := except_bind_eq_ok h
      refine bind_ok (ih.app1 _ _ _ hy) ?_
      refine bind_ok (ih.all _ _ _ hys) ?_
      exact h

private theorem tryAlts_step {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k')
    (ih : EvalMono C k k') :
    ∀ env jenv binder sv as dflt v, Eval.tryAlts C (k + 1) env jenv binder sv as dflt = .ok v →
      Eval.tryAlts C (k' + 1) env jenv binder sv as dflt = .ok v := by
  intro env jenv binder sv as dflt v h
  cases as with
  | nil =>
      cases dflt with
      | some alt =>
          obtain ⟨con, bs, body⟩ := alt
          rw [Eval.tryAlts] at h ⊢
          exact ih.core _ _ _ _ h
      | none =>
          rw [Eval.tryAlts] at h
          exact error_ne_ok h
  | cons alt rest =>
      obtain ⟨con, bs, body⟩ := alt
      cases con with
      | default =>
          rw [Eval.tryAlts] at h ⊢
          exact ih.alts _ _ _ _ _ _ _ h
      | dataAlt cn =>
          cases sv
          case con ty cv fields =>
            rw [Eval.tryAlts] at h ⊢
            split at h <;> rename_i hcn
            · split at h <;> rename_i hlen
              · rw [if_pos hcn, if_pos hlen]
                exact ih.core _ _ _ _ h
              · exact error_ne_ok h
            · rw [if_neg hcn]
              exact ih.alts _ _ _ _ _ _ _ h
          all_goals simp [Eval.tryAlts] at h
      | litAlt n =>
          rw [Eval.tryAlts] at h ⊢
          obtain ⟨bm, hbm, h⟩ := except_bind_eq_ok h
          refine bind_ok (Eval.litMatches_mono hk hbm) ?_
          cases bm
          · exact ih.alts _ _ _ _ _ _ _ h
          · exact ih.core _ _ _ _ h

set_option maxHeartbeats 1000000 in
private theorem evalBuiltin_step {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k')
    (ih : EvalMono C k k') :
    ∀ ty b vs v, Eval.evalBuiltin C (k + 1) ty b vs = .ok v →
      Eval.evalBuiltin C (k' + 1) ty b vs = .ok v := by
  intro ty b vs v h
  rw [Eval.evalBuiltin] at h ⊢
  split at h  -- let (doms, res) := Ty.flattenArrow ty
  split at h  -- the 64-row dispatch on (b, vs)
  -- Rows that are error rows, fuel-free rows, or whole-row helper
  -- applications.
  all_goals try first
    | exact error_ne_ok h
    | exact h
    | exact DEnv.zeroVal_mono C.Δ hk h
    | exact Eval.bvBinArith_mono hk h
    | exact Eval.bvBinCmp_mono hk h
    | exact Eval.bvRed_mono hk h
  -- Rows that are bind chains over fuel-free components (passed
  -- through), `valToBits`, and `applyAll`, ending in a fuel-free tail.
  all_goals repeat first
    | exact h
    | exact error_ne_ok h
    | (obtain ⟨w, hw, h⟩ := except_bind_eq_ok h
       first
         | refine bind_ok (Eval.valToBits_mono hk hw) ?_
         | refine bind_ok (ih.all _ _ _ hw) ?_
         | refine bind_ok hw ?_)
  -- The remaining row is bitSlice: its slice-validity branch guards
  -- the fuel-consuming valToBits (`w` is the second Finite pair; the
  -- pair binds reduce by structure eta during the peels above).
  obtain ⟨i₀, i⟩ := w
  dsimp only [] at h ⊢
  split at h
  · exact error_ne_ok h
  · rename_i hij
    rw [if_neg hij]
    obtain ⟨x, hx, h⟩ := except_bind_eq_ok h
    refine bind_ok (Eval.valToBits_mono hk hx) ?_
    exact h

private theorem evalCry_step {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k')
    (ih : EvalMono C k k') :
    ∀ env jenv pty f n rest v, Eval.evalCry C (k + 1) env jenv pty f n rest = .ok v →
      Eval.evalCry C (k' + 1) env jenv pty f n rest = .ok v := by
  intro env jenv pty f n rest v h
  rw [Eval.evalCry] at h ⊢
  obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
  refine bind_ok (ih.list _ _ _ _ hvs) ?_
  obtain ⟨ity, hity, h⟩ := except_bind_eq_ok h
  refine bind_ok hity ?_
  split at h
  rotate_left
  · exact error_ne_ok h
  rename_i hlen
  rw [if_pos hlen]
  cases hden : C.Δ.cryF f n ity with
  | none => rw [hden] at h; exact error_ne_ok h
  | some den =>
      rw [hden] at h
      obtain ⟨reps, hreps, h⟩ := except_bind_eq_ok h
      refine bind_ok (mapM_mono (fun a b hab => Eval.valToBits_mono hk hab) hreps) ?_
      obtain ⟨bv, hbv, h⟩ := except_bind_eq_ok h
      refine bind_ok hbv ?_
      exact decode_mono hk h

private theorem evalExt_step {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k')
    (ih : EvalMono C k k') :
    ∀ env jenv pty s rest v, Eval.evalExt C (k + 1) env jenv pty s rest = .ok v →
      Eval.evalExt C (k' + 1) env jenv pty s rest = .ok v := by
  intro env jenv pty s rest v h
  rw [Eval.evalExt] at h ⊢
  obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
  refine bind_ok (ih.list _ _ _ _ hvs) ?_
  obtain ⟨ity, hity, h⟩ := except_bind_eq_ok h
  refine bind_ok hity ?_
  split at h
  rotate_left
  · exact error_ne_ok h
  rename_i hlen
  rw [if_pos hlen]
  cases hden : C.Δ.xtF s with
  | none => rw [hden] at h; exact error_ne_ok h
  | some den =>
      rw [hden] at h
      obtain ⟨reps, hreps, h⟩ := except_bind_eq_ok h
      refine bind_ok (mapM_mono (fun a b hab => Eval.valToBits_mono hk hab) hreps) ?_
      obtain ⟨bv, hbv, h⟩ := except_bind_eq_ok h
      refine bind_ok hbv ?_
      exact decode_mono hk h

private theorem evalMono_all (C : Eval.Ctx) : ∀ k k', k ≤ k' → EvalMono C k k' := by
  intro k
  induction k with
  | zero => intro k' _; exact evalMono_zero C k'
  | succ k ihk =>
      intro k' hkk'
      obtain ⟨k', rfl⟩ : ∃ j, k' = j + 1 := ⟨k' - 1, by omega⟩
      have hk : k ≤ k' := by omega
      have ih := ihk k' hk
      exact ⟨evalCore_step hk ih, evalList_step hk ih, callDefn_step hk ih,
        applyValCore_step hk ih, applyMany_step hk ih, applyAll_step hk ih,
        tryAlts_step hk ih, evalBuiltin_step hk ih,
        evalCry_step hk ih, evalExt_step hk ih⟩

/-- `Eval.evalCore` is monotone in its fuel. -/
theorem Eval.evalCore_mono {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k') {env : Eval.Env}
    {jenv : Eval.JEnv} {e : Exp} {v : Val} (h : Eval.evalCore C k env jenv e = .ok v) :
    Eval.evalCore C k' env jenv e = .ok v :=
  (evalMono_all C k k' hk).core env jenv e v h

/-- `Eval.evalList` is monotone in its fuel. -/
theorem Eval.evalList_mono {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k') {env : Eval.Env}
    {jenv : Eval.JEnv} {es : List Exp} {vs : List Val}
    (h : Eval.evalList C k env jenv es = .ok vs) : Eval.evalList C k' env jenv es = .ok vs :=
  (evalMono_all C k k' hk).list env jenv es vs h

/-- `Eval.callDefn` is monotone in its fuel. -/
theorem Eval.callDefn_mono {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k') {d : Defn}
    {vs : List Val} {v : Val} (h : Eval.callDefn C k d vs = .ok v) :
    Eval.callDefn C k' d vs = .ok v :=
  (evalMono_all C k k' hk).defn d vs v h

/-- `Eval.applyValCore` is monotone in its fuel. -/
theorem Eval.applyValCore_mono {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k') {f a v : Val}
    (h : Eval.applyValCore C k f a = .ok v) : Eval.applyValCore C k' f a = .ok v :=
  (evalMono_all C k k' hk).app1 f a v h

/-- `Eval.applyMany` is monotone in its fuel. -/
theorem Eval.applyMany_mono {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k') {f v : Val}
    {as : List Val} (h : Eval.applyMany C k f as = .ok v) :
    Eval.applyMany C k' f as = .ok v :=
  (evalMono_all C k k' hk).many f as v h

/-- `Eval.applyAll` is monotone in its fuel. -/
theorem Eval.applyAll_mono {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k') {f : Val}
    {xs ys : List Val} (h : Eval.applyAll C k f xs = .ok ys) :
    Eval.applyAll C k' f xs = .ok ys :=
  (evalMono_all C k k' hk).all f xs ys h

/-- `Eval.tryAlts` is monotone in its fuel. -/
theorem Eval.tryAlts_mono {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k') {env : Eval.Env}
    {jenv : Eval.JEnv} {binder : Id} {sv : Val} {as : List Alt} {dflt : Option Alt} {v : Val}
    (h : Eval.tryAlts C k env jenv binder sv as dflt = .ok v) :
    Eval.tryAlts C k' env jenv binder sv as dflt = .ok v :=
  (evalMono_all C k k' hk).alts env jenv binder sv as dflt v h

/-- `Eval.evalBuiltin` is monotone in its fuel. -/
theorem Eval.evalBuiltin_mono {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k') {ty : Ty}
    {b : Builtin} {vs : List Val} {v : Val} (h : Eval.evalBuiltin C k ty b vs = .ok v) :
    Eval.evalBuiltin C k' ty b vs = .ok v :=
  (evalMono_all C k k' hk).builtin ty b vs v h

/-- `Eval.evalCry` (the Cryptol foreign row) is monotone in its fuel. -/
theorem Eval.evalCry_mono {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k') {env : Eval.Env}
    {jenv : Eval.JEnv} {pty : Ty} {f n : String} {rest : List Exp} {v : Val}
    (h : Eval.evalCry C k env jenv pty f n rest = .ok v) :
    Eval.evalCry C k' env jenv pty f n rest = .ok v :=
  (evalMono_all C k k' hk).cry env jenv pty f n rest v h

/-- `Eval.evalExt` (the model-carrying extern row) is monotone in its
fuel. -/
theorem Eval.evalExt_mono {C : Eval.Ctx} {k k' : Nat} (hk : k ≤ k') {env : Eval.Env}
    {jenv : Eval.JEnv} {pty : Ty} {s : String} {rest : List Exp} {v : Val}
    (h : Eval.evalExt C k env jenv pty s rest = .ok v) :
    Eval.evalExt C k' env jenv pty s rest = .ok v :=
  (evalMono_all C k k' hk).ext env jenv pty s rest v h

/-- The exported evaluator entry point `eval` is monotone in its fuel. -/
theorem eval_mono {Δ : DEnv} {defns : HashMap Int Defn} {k k' : Nat} (hk : k ≤ k')
    {env : Eval.Env} {e : Exp} {v : Val} (h : eval Δ defns k env e = .ok v) :
    eval Δ defns k' env e = .ok v :=
  Eval.evalCore_mono hk h

/-- The exported application entry point `applyVal` is monotone in its
fuel. -/
theorem applyVal_mono {Δ : DEnv} {defns : HashMap Int Defn} {k k' : Nat} (hk : k ≤ k')
    {f a v : Val} (h : applyVal Δ defns k f a = .ok v) : applyVal Δ defns k' f a = .ok v :=
  Eval.applyValCore_mono hk h

/-! ## The machine layer (Rwv.Eidos.Machine)

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

/-- `Machine.runCmds` is monotone in its evaluation fuel. -/
theorem Machine.runCmds_mono {Δ : DEnv} {defns : HashMap Int Defn} {k k' : Nat}
    (hk : k ≤ k') {env₀ : Eval.Env} {cells₀ : HashMap String Val} {cmds : List Cmd}
    {r : Eval.Env × HashMap String Val} (h : Machine.runCmds Δ defns k env₀ cells₀ cmds = .ok r) :
    Machine.runCmds Δ defns k' env₀ cells₀ cmds = .ok r := by
  unfold Machine.runCmds at h ⊢
  refine foldlM_mono ?_ h
  intro s cmd r' hb
  obtain ⟨env, cells⟩ := s
  cases cmd with
  | bind x e =>
      obtain ⟨w, hw, hb⟩ := except_bind_eq_ok hb
      refine bind_ok (eval_mono hk hw) ?_
      exact hb
  | get x s => exact hb
  | put s a =>
      obtain ⟨w, hw, hb⟩ := except_bind_eq_ok hb
      refine bind_ok (eval_mono hk hw) ?_
      exact hb

/-- `Machine.execBlock`'s terminator runner is monotone in both fuels:
the goto fuel bounds intra-cycle transfer (goto chains and
terminator-case descent both decrement it), the evaluation fuel the
pure evaluation inside commands and terminators. -/
private theorem runTerm_mono (Δ : DEnv) (defns : HashMap Int Defn)
    (blocks : HashMap Int Block) :
    ∀ {gf gf' ef ef' : Nat}, gf ≤ gf' → ef ≤ ef' →
      ∀ {env : Eval.Env} {cells : HashMap String Val} {t : Term} {r : StepOut},
      Machine.execBlock.runTerm Δ defns blocks ef gf env cells t = .ok r →
      Machine.execBlock.runTerm Δ defns blocks ef' gf' env cells t = .ok r := by
  intro gf
  induction gf with
  | zero =>
      intro gf' ef ef' _ hef env cells t r h
      cases t with
      | pause out l args =>
          rw [Machine.execBlock.runTerm] at h ⊢
          obtain ⟨o, ho, h⟩ := except_bind_eq_ok h
          obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
          refine bind_ok (eval_mono hef ho) ?_
          refine bind_ok (mapM_mono (fun a b hab => eval_mono hef hab) hvs) ?_
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
          refine bind_ok (eval_mono hef ha) ?_
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
          refine bind_ok (eval_mono hef ho) ?_
          refine bind_ok (mapM_mono (fun a b hab => eval_mono hef hab) hvs) ?_
          exact h
      | goto l args =>
          rw [Machine.execBlock.runTerm] at h ⊢
          cases hB : blocks.get? l.uniq with
          | none => rw [hB] at h; exact error_ne_ok h
          | some blk =>
              rw [hB] at h
              obtain ⟨vs, hvs, h⟩ := except_bind_eq_ok h
              refine bind_ok (mapM_mono (fun a b hab => eval_mono hef hab) hvs) ?_
              dsimp only [] at h ⊢
              split at h
              · exact error_ne_ok h
              · rename_i hlen
                rw [if_neg hlen]
                obtain ⟨p, hp, h⟩ := except_bind_eq_ok h
                obtain ⟨env'', cells'⟩ := p
                refine bind_ok (Machine.runCmds_mono hef hp) ?_
                exact ihg hgf' hef h
      | halt e =>
          rw [Machine.execBlock.runTerm] at h ⊢
          obtain ⟨a, ha, h⟩ := except_bind_eq_ok h
          refine bind_ok (eval_mono hef ha) ?_
          exact h
      | cases scrutE alts =>
          rw [Machine.execBlock.runTerm] at h ⊢
          obtain ⟨sv, hsv, h⟩ := except_bind_eq_ok h
          obtain ⟨sel, hsel, h⟩ := except_bind_eq_ok h
          obtain ⟨bs, t'⟩ := sel
          refine bind_ok (eval_mono hef hsv) ?_
          refine bind_ok (Machine.selectTAlt_mono hef hsel) ?_
          exact ihg hgf' hef h

/-- `Machine.execBlock` is monotone in the evaluation fuel and the goto
fuel separately. -/
theorem Machine.execBlock_mono {Δ : DEnv} {defns : HashMap Int Defn}
    {blocks : HashMap Int Block} {ef ef' gf gf' : Nat} (hef : ef ≤ ef') (hgf : gf ≤ gf')
    {env₀ : Eval.Env} {cells₀ : HashMap String Val} {b : Block} {r : StepOut}
    (h : Machine.execBlock Δ defns blocks ef gf env₀ cells₀ b = .ok r) :
    Machine.execBlock Δ defns blocks ef' gf' env₀ cells₀ b = .ok r := by
  unfold Machine.execBlock at h ⊢
  obtain ⟨p, hp, h⟩ := except_bind_eq_ok h
  obtain ⟨env, cells⟩ := p
  refine bind_ok (Machine.runCmds_mono hef hp) ?_
  exact runTerm_mono Δ defns blocks hgf hef h

/-- `Machine.initCells` is monotone in its evaluation fuel. -/
theorem Machine.initCells_mono {Δ : DEnv} {defns : HashMap Int Defn} {k k' : Nat}
    (hk : k ≤ k') {p : Proc} {σ : HashMap String Val}
    (h : Machine.initCells Δ defns k p = .ok σ) : Machine.initCells Δ defns k' p = .ok σ := by
  unfold Machine.initCells at h ⊢
  refine foldlM_mono ?_ h
  intro s c r' hb
  dsimp only [] at hb ⊢
  cases hI : c.init with
  | some e =>
      rw [hI] at hb
      obtain ⟨w, hw, hb⟩ := except_bind_eq_ok hb
      refine bind_ok (eval_mono hk hw) ?_
      exact hb
  | none =>
      rw [hI] at hb
      obtain ⟨w, hw, hb⟩ := except_bind_eq_ok hb
      refine bind_ok (DEnv.zeroVal_mono Δ hk hw) ?_
      exact hb

/-- `Machine.step` is monotone in the evaluation fuel and the goto fuel
separately. -/
theorem Machine.step_mono {Δ : DEnv} {defns : HashMap Int Defn}
    {blocks : HashMap Int Block} {ef ef' gf gf' : Nat} (hef : ef ≤ ef') (hgf : gf ≤ gf')
    {s : MState} {input : Val} {r : StepOut}
    (h : Machine.step Δ defns blocks ef gf s input = .ok r) :
    Machine.step Δ defns blocks ef' gf' s input = .ok r := by
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
        exact Machine.execBlock_mono hef hgf h

/-- `Machine.foldStep` is monotone in the evaluation fuel and the goto
fuel separately. -/
theorem Machine.foldStep_mono {Δ : DEnv} {defns : HashMap Int Defn}
    {blocks : HashMap Int Block} {ef ef' gf gf' : Nat} (hef : ef ≤ ef') (hgf : gf ≤ gf')
    {acc : List Val × Option Val × Option MState} {i : Val}
    {r : List Val × Option Val × Option MState}
    (h : Machine.foldStep Δ defns blocks ef gf acc i = .ok r) :
    Machine.foldStep Δ defns blocks ef' gf' acc i = .ok r := by
  obtain ⟨outs, halted, s?⟩ := acc
  cases s? with
  | none => exact h
  | some s =>
      cases halted with
      | some a => exact h
      | none =>
          obtain ⟨so, hso, h⟩ := except_bind_eq_ok h
          refine bind_ok (Machine.step_mono hef hgf hso) ?_
          exact h

/-- `Proc.run` is monotone in the evaluation fuel and the goto fuel
separately. -/
theorem Proc.run_mono {Δ : DEnv} {defns : HashMap Int Defn} {ef ef' gf gf' : Nat}
    (hef : ef ≤ ef') (hgf : gf ≤ gf') {p : Proc} {inputs : List Val} {mt : MTrace}
    (h : Proc.run Δ defns ef gf p inputs = .ok mt) :
    Proc.run Δ defns ef' gf' p inputs = .ok mt := by
  unfold Proc.run at h ⊢
  obtain ⟨σ₀, hσ, h⟩ := except_bind_eq_ok h
  obtain ⟨so, hso, h⟩ := except_bind_eq_ok h
  refine bind_ok (Machine.initCells_mono hef hσ) ?_
  refine bind_ok (Machine.execBlock_mono hef hgf hso) ?_
  cases so with
  | halt a => exact h
  | step o s₀ =>
      obtain ⟨tri, htri, h⟩ := except_bind_eq_ok h
      refine bind_ok (foldlM_mono (fun s a r hb => Machine.foldStep_mono hef hgf hb) htri) ?_
      exact h

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
    {ef₁ gf₁ ef₂ gf₂ : Nat} {p : Proc} {ins : List Val} {mt₁ mt₂ : MTrace}
    (h₁ : Proc.run Δ defns ef₁ gf₁ p ins = .ok mt₁)
    (h₂ : Proc.run Δ defns ef₂ gf₂ p ins = .ok mt₂) : mt₁ = mt₂ := by
  have k₁ := Proc.run_mono (Nat.le_max_left ef₁ ef₂) (Nat.le_max_left gf₁ gf₂) h₁
  have k₂ := Proc.run_mono (Nat.le_max_right ef₁ ef₂) (Nat.le_max_right gf₁ gf₂) h₂
  exact Except.ok.inj (k₁.symm.trans k₂)

end Rwv.Eidos

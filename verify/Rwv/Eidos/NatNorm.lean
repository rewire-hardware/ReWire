/-
Type equality modulo nat normalization, as a theory: the facts the
validator's soundness proofs need about `Ty.eq` (Rwv.Eidos.Types —
structural equality after `natNorm`, the equality ReWire's own lint
compares types with) and about the semantic functions that consume
types (`flatten`, `evalNat`, `matchTy`, `substTv`, `sizeOf`,
`ctorTag`), which all read a type through its evaluated nat positions
and are therefore invariant under it.

The structural equality underneath is the derived `BEq Ty` (with type
variables compared by unique), so every statement here is phrased over
`==` rather than `=`: `Ty.Beq` is that relation as a Prop, and the
inversion lemmas below are what make it usable.

Two facts about a datatype environment are needed for the width and
matching functions to be invariant, both decided by `DEnv.natOk` and
folded into the validators' `denvOk` gate:

  * no datatype is named `+`, `-` or `*` (`arithFree`) — otherwise a
    normalizable spine could be sized as a datatype on one side and
    as a literal on the other;
  * every constructor signature's result type is its datatype applied
    to type variables (`sigShapeOk`, the rule ReWire's checker
    enforces on data declarations) — `matchTy` descends only into the
    positions it binds, so two spellings of an instance type bind
    the same variables to `Ty.eq`-equal types.
-/
import Rwv.Eidos.Value

namespace Rwv.Eidos

open Std (HashMap)

/-! ## Local `Except`/list helpers (house style: re-proved) -/

private theorem error_ne_ok {α : Type} {msg : String} {a : α} {P : Prop}
    (h : (Except.error msg : Except String α) = .ok a) : P := by
  cases h

private theorem except_bind_eq_ok {α β : Type} {x : Except String α} {f : α → Except String β}
    {b : β} (h : (x >>= f) = .ok b) : ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x with
  | error e => exact error_ne_ok h
  | ok a => exact ⟨a, rfl, h⟩

private theorem except_bind_ok {α β : Type} {a : α} {f : α → Except String β} :
    (Except.ok a >>= f) = f a := rfl

/-! ## `TyVar` as a well-behaved hash key -/

instance : EquivBEq TyVar where
  symm := fun {a b} h => by
    have h' : a.uniq = b.uniq := eq_of_beq (show (a.uniq == b.uniq) = true from h)
    show (b.uniq == a.uniq) = true
    rw [h']; exact beq_self_eq_true _
  trans := fun {a b c} h1 h2 => by
    have h1' : a.uniq = b.uniq := eq_of_beq (show (a.uniq == b.uniq) = true from h1)
    have h2' : b.uniq = c.uniq := eq_of_beq (show (b.uniq == c.uniq) = true from h2)
    show (a.uniq == c.uniq) = true
    rw [h1', h2']; exact beq_self_eq_true _
  rfl := fun {a} => beq_self_eq_true a.uniq

instance : LawfulHashable TyVar where
  hash_eq := fun a b h => by
    show hash a.uniq = hash b.uniq
    rw [eq_of_beq (show (a.uniq == b.uniq) = true from h)]

namespace Ty

/-- The type-level arithmetic heads. -/
def isArith (c : String) : Bool := c == "+" || c == "-" || c == "*"

/-! ## The derived structural equality, as a Prop -/

/-- Structural `==` on types (type variables by unique). -/
def Beq (t u : Ty) : Prop := (t == u) = true

theorem beq_con_con {a b : String} : ((con a) == (con b)) = (a == b) := rfl
theorem beq_app_app {a b c d : Ty} : ((app a b) == (app c d)) = (a == c && b == d) := rfl
theorem beq_var_var {a b : TyVar} : ((var a) == (var b)) = (a.uniq == b.uniq) := rfl
theorem beq_nat_nat {m n : Nat} : ((nat m) == (nat n)) = (m == n) := rfl
theorem beq_arrow_arrow {a b c d : Ty} :
    ((arrow a b) == (arrow c d)) = (a == c && b == d) := rfl

theorem Beq.con_inv {c : String} : ∀ {u : Ty}, Beq (con c) u → u = con c
  | con _, h => by
      have h' : (c == _) = true := h
      rw [eq_of_beq h']
  | app _ _, h | var _, h | nat _, h | arrow _ _, h => by cases h

theorem Beq.nat_inv {n : Nat} : ∀ {u : Ty}, Beq (nat n) u → u = nat n
  | nat _, h => by
      have h' : (n == _) = true := h
      rw [eq_of_beq h']
  | con _, h | app _ _, h | var _, h | arrow _ _, h => by cases h

theorem Beq.var_inv {a : TyVar} : ∀ {u : Ty}, Beq (var a) u → ∃ b, u = var b ∧ a.uniq = b.uniq
  | var b, h => by
      have h' : (a.uniq == b.uniq) = true := h
      exact ⟨b, rfl, eq_of_beq h'⟩
  | con _, h | app _ _, h | nat _, h | arrow _ _, h => by cases h

theorem Beq.app_inv {a b : Ty} : ∀ {u : Ty}, Beq (app a b) u →
    ∃ c d, u = app c d ∧ Beq a c ∧ Beq b d
  | app c d, h => by
      have h' : (a == c && b == d) = true := h
      rw [Bool.and_eq_true] at h'
      exact ⟨c, d, rfl, h'.1, h'.2⟩
  | con _, h | var _, h | nat _, h | arrow _ _, h => by cases h

theorem Beq.arrow_inv {a b : Ty} : ∀ {u : Ty}, Beq (arrow a b) u →
    ∃ c d, u = arrow c d ∧ Beq a c ∧ Beq b d
  | arrow c d, h => by
      have h' : (a == c && b == d) = true := h
      rw [Bool.and_eq_true] at h'
      exact ⟨c, d, rfl, h'.1, h'.2⟩
  | con _, h | var _, h | nat _, h | app _ _, h => by cases h

theorem Beq.app_intro {a b c d : Ty} (h1 : Beq a c) (h2 : Beq b d) : Beq (app a b) (app c d) := by
  show (a == c && b == d) = true
  rw [Bool.and_eq_true]
  exact ⟨h1, h2⟩

theorem Beq.arrow_intro {a b c d : Ty} (h1 : Beq a c) (h2 : Beq b d) :
    Beq (arrow a b) (arrow c d) := by
  show (a == c && b == d) = true
  rw [Bool.and_eq_true]
  exact ⟨h1, h2⟩

theorem Beq.refl : ∀ (t : Ty), Beq t t
  | con c => by show (c == c) = true; simp
  | app a b => Beq.app_intro (Beq.refl a) (Beq.refl b)
  | var a => by show (a.uniq == a.uniq) = true; simp
  | nat n => by show (n == n) = true; simp
  | arrow a b => Beq.arrow_intro (Beq.refl a) (Beq.refl b)

theorem Beq.symm : ∀ {t u : Ty}, Beq t u → Beq u t
  | con _, u, h => by obtain rfl := Beq.con_inv h; exact Beq.refl _
  | nat _, u, h => by obtain rfl := Beq.nat_inv h; exact Beq.refl _
  | var a, u, h => by
      obtain ⟨b, rfl, hab⟩ := Beq.var_inv h
      show (b.uniq == a.uniq) = true
      rw [hab]; simp
  | app a b, u, h => by
      obtain ⟨c, d, rfl, h1, h2⟩ := Beq.app_inv h
      exact Beq.app_intro (Beq.symm h1) (Beq.symm h2)
  | arrow a b, u, h => by
      obtain ⟨c, d, rfl, h1, h2⟩ := Beq.arrow_inv h
      exact Beq.arrow_intro (Beq.symm h1) (Beq.symm h2)

theorem Beq.trans : ∀ {t u v : Ty}, Beq t u → Beq u v → Beq t v
  | con _, u, v, h1, h2 => by obtain rfl := Beq.con_inv h1; exact h2
  | nat _, u, v, h1, h2 => by obtain rfl := Beq.nat_inv h1; exact h2
  | var a, u, v, h1, h2 => by
      obtain ⟨b, rfl, hab⟩ := Beq.var_inv h1
      obtain ⟨c, rfl, hbc⟩ := Beq.var_inv h2
      show (a.uniq == c.uniq) = true
      rw [hab, hbc]; simp
  | app a b, u, v, h1, h2 => by
      obtain ⟨c, d, rfl, h3, h4⟩ := Beq.app_inv h1
      obtain ⟨e, f, rfl, h5, h6⟩ := Beq.app_inv h2
      exact Beq.app_intro (Beq.trans h3 h5) (Beq.trans h4 h6)
  | arrow a b, u, v, h1, h2 => by
      obtain ⟨c, d, rfl, h3, h4⟩ := Beq.arrow_inv h1
      obtain ⟨e, f, rfl, h5, h6⟩ := Beq.arrow_inv h2
      exact Beq.arrow_intro (Beq.trans h3 h5) (Beq.trans h4 h6)

theorem Beq.of_eq {t u : Ty} (h : t = u) : Beq t u := h ▸ Beq.refl t

/-! ## The structure of `natNorm` -/

theorem natNorm_of_evalNat {t : Ty} {n : Nat} (h : evalNat t = some n) : natNorm t = nat n := by
  rw [natNorm.eq_def, h]

theorem natNorm_app_of_none {a b : Ty} (h : evalNat (app a b) = none) :
    natNorm (app a b) = app (natNorm a) (natNorm b) := by
  rw [natNorm.eq_def, h]

theorem natNorm_arrow {a b : Ty} : natNorm (arrow a b) = arrow (natNorm a) (natNorm b) := rfl
theorem natNorm_con {c : String} : natNorm (con c) = con c := rfl
theorem natNorm_var {a : TyVar} : natNorm (var a) = var a := rfl
theorem natNorm_nat {n : Nat} : natNorm (nat n) = nat n := rfl

theorem natNorm_con_inv {c : String} : ∀ {t : Ty}, natNorm t = con c → t = con c := by
  intro t h
  cases t with
  | con d => rw [natNorm_con] at h; exact h
  | app a b =>
      cases hev : evalNat (app a b) with
      | some n => rw [natNorm_of_evalNat hev] at h; cases h
      | none => rw [natNorm_app_of_none hev] at h; cases h
  | var a => rw [natNorm_var] at h; cases h
  | nat n => rw [natNorm_nat] at h; cases h
  | arrow a b => rw [natNorm_arrow] at h; cases h

theorem natNorm_var_inv {a : TyVar} : ∀ {t : Ty}, natNorm t = var a → t = var a := by
  intro t h
  cases t with
  | con d => rw [natNorm_con] at h; cases h
  | app x y =>
      cases hev : evalNat (app x y) with
      | some n => rw [natNorm_of_evalNat hev] at h; cases h
      | none => rw [natNorm_app_of_none hev] at h; cases h
  | var b => rw [natNorm_var] at h; exact h
  | nat n => rw [natNorm_nat] at h; cases h
  | arrow x y => rw [natNorm_arrow] at h; cases h

theorem natNorm_nat_inv {n : Nat} : ∀ {t : Ty}, natNorm t = nat n → evalNat t = some n := by
  intro t h
  cases t with
  | con d => rw [natNorm_con] at h; cases h
  | app a b =>
      cases hev : evalNat (app a b) with
      | some m => rw [natNorm_of_evalNat hev] at h; injection h with h; rw [h]
      | none => rw [natNorm_app_of_none hev] at h; cases h
  | var a => rw [natNorm_var] at h; cases h
  | nat m => rw [natNorm_nat] at h; injection h with h; rw [h]; rfl
  | arrow a b => rw [natNorm_arrow] at h; cases h

theorem natNorm_app_inv {c d : Ty} : ∀ {t : Ty}, natNorm t = app c d →
    ∃ a b, t = app a b ∧ evalNat (app a b) = none ∧ natNorm a = c ∧ natNorm b = d := by
  intro t h
  cases t with
  | con e => rw [natNorm_con] at h; cases h
  | app a b =>
      cases hev : evalNat (app a b) with
      | some m => rw [natNorm_of_evalNat hev] at h; cases h
      | none =>
          rw [natNorm_app_of_none hev] at h
          injection h with h1 h2
          exact ⟨a, b, rfl, hev, h1, h2⟩
  | var a => rw [natNorm_var] at h; cases h
  | nat m => rw [natNorm_nat] at h; cases h
  | arrow a b => rw [natNorm_arrow] at h; cases h

theorem natNorm_arrow_inv {c d : Ty} : ∀ {t : Ty}, natNorm t = arrow c d →
    ∃ a b, t = arrow a b ∧ natNorm a = c ∧ natNorm b = d := by
  intro t h
  cases t with
  | con e => rw [natNorm_con] at h; cases h
  | app a b =>
      cases hev : evalNat (app a b) with
      | some m => rw [natNorm_of_evalNat hev] at h; cases h
      | none => rw [natNorm_app_of_none hev] at h; cases h
  | var a => rw [natNorm_var] at h; cases h
  | nat m => rw [natNorm_nat] at h; cases h
  | arrow a b =>
      rw [natNorm_arrow] at h
      injection h with h1 h2
      exact ⟨a, b, rfl, h1, h2⟩

/-- The evaluable spines: a literal, or an arithmetic head applied to
two evaluable operands. -/
theorem evalNat_some_inv {t : Ty} {n : Nat} (h : evalNat t = some n) :
    t = nat n ∨ ∃ op a b x y, t = app (app (con op) a) b ∧
      evalNat a = some x ∧ evalNat b = some y ∧
      ((op = "+" ∧ n = x + y) ∨ (op = "-" ∧ n = x - y) ∨ (op = "*" ∧ n = x * y)) := by
  rw [evalNat.eq_def] at h
  split at h
  · injection h with h; subst h; exact .inl rfl
  · rename_i op a b
    refine .inr ⟨op, a, b, ?_⟩
    cases ha : evalNat a with
    | none => rw [ha] at h; cases h
    | some x =>
        rw [ha] at h
        cases hb : evalNat b with
        | none => rw [hb] at h; cases h
        | some y =>
            rw [hb] at h
            refine ⟨x, y, rfl, rfl, rfl, ?_⟩
            simp only [bind, Option.bind] at h
            split at h
            · injection h with h; exact .inl ⟨rfl, h.symm⟩
            · injection h with h; exact .inr (.inl ⟨rfl, h.symm⟩)
            · injection h with h; exact .inr (.inr ⟨rfl, h.symm⟩)
            · cases h
  · cases h

theorem evalNat_app_app_con {op : String} {a b : Ty} {x y : Nat}
    (ha : evalNat a = some x) (hb : evalNat b = some y) :
    evalNat (app (app (con op) a) b) =
      match op with
      | "+" => some (x + y)
      | "-" => some (x - y)
      | "*" => some (x * y)
      | _ => none := by
  rw [evalNat, ha, hb]
  rfl

/-- `evalNat` reads through normalization. -/
theorem evalNat_natNorm : ∀ (t : Ty), evalNat (natNorm t) = evalNat t
  | nat n => rfl
  | con c => rfl
  | var a => rfl
  | arrow a b => rfl
  | app a b => by
      cases hev : evalNat (app a b) with
      | some n => rw [natNorm_of_evalNat hev]; rfl
      | none =>
          rw [natNorm_app_of_none hev]
          cases hev' : evalNat (app (natNorm a) (natNorm b)) with
          | none => rfl
          | some m =>
              exfalso
              rcases evalNat_some_inv hev' with h | ⟨op, a', b', x, y, heq, hx, hy, hop⟩
              · cases h
              · injection heq with h1 h2
                obtain ⟨a₁, a₂, rfl, _, ha₁, ha₂⟩ := natNorm_app_inv h1
                obtain rfl := natNorm_con_inv ha₁
                rw [← ha₂, evalNat_natNorm a₂] at hx
                rw [← h2, evalNat_natNorm b] at hy
                rw [evalNat_app_app_con hx hy] at hev
                rcases hop with ⟨rfl, _⟩ | ⟨rfl, _⟩ | ⟨rfl, _⟩ <;> cases hev

theorem natNorm_idem : ∀ (t : Ty), natNorm (natNorm t) = natNorm t
  | nat n => rfl
  | con c => rfl
  | var a => rfl
  | arrow a b => by rw [natNorm_arrow, natNorm_arrow, natNorm_idem a, natNorm_idem b]
  | app a b => by
      cases hev : evalNat (app a b) with
      | some n => rw [natNorm_of_evalNat hev]; rfl
      | none =>
          rw [natNorm_app_of_none hev]
          have hev' : evalNat (app (natNorm a) (natNorm b)) = none := by
            have := evalNat_natNorm (app a b)
            rw [natNorm_app_of_none hev, hev] at this
            exact this
          rw [natNorm_app_of_none hev', natNorm_idem a, natNorm_idem b]

/-! ## `Ty.eq` is an equivalence -/

theorem eq_iff_beq {t u : Ty} : eq t u = true ↔ Beq (natNorm t) (natNorm u) := Iff.rfl

theorem eq_of_beqN {t u : Ty} (h : Beq (natNorm t) (natNorm u)) : eq t u = true := h
theorem beqN_of_eq {t u : Ty} (h : eq t u = true) : Beq (natNorm t) (natNorm u) := h

theorem eq_refl (t : Ty) : eq t t = true := Beq.refl _

theorem eq_symm {t u : Ty} (h : eq t u = true) : eq u t = true := Beq.symm h

theorem eq_trans {t u v : Ty} (h1 : eq t u = true) (h2 : eq u v = true) : eq t v = true :=
  Beq.trans h1 h2

theorem eq_of_eq {t u : Ty} (h : t = u) : eq t u = true := h ▸ eq_refl t

theorem eq_natNorm_left (t : Ty) : eq (natNorm t) t = true := by
  show Beq (natNorm (natNorm t)) (natNorm t)
  rw [natNorm_idem]
  exact Beq.refl _

/-! ## What `Ty.eq` preserves -/

/-- Nat evaluation. -/
theorem evalNat_eq {t u : Ty} (h : eq t u = true) : evalNat t = evalNat u := by
  cases hev : evalNat t with
  | some n =>
      have h1 : natNorm t = nat n := natNorm_of_evalNat hev
      have h2 : Beq (nat n) (natNorm u) := h1 ▸ h
      exact (natNorm_nat_inv (Beq.nat_inv h2)).symm
  | none =>
      cases hev' : evalNat u with
      | none => rfl
      | some n =>
          have h1 : natNorm u = nat n := natNorm_of_evalNat hev'
          have h2 : Beq (nat n) (natNorm t) := h1 ▸ Beq.symm h
          rw [natNorm_nat_inv (Beq.nat_inv h2)] at hev
          cases hev

/-- A constructor is rigid. -/
theorem eq_con_inv {c : String} {t : Ty} (h : eq t (con c) = true) : t = con c := by
  have h' : Beq (con c) (natNorm t) := Beq.symm h
  exact natNorm_con_inv (Beq.con_inv h')

theorem eq_con_inv' {c : String} {t : Ty} (h : eq (con c) t = true) : t = con c :=
  eq_con_inv (eq_symm h)

/-- Heads whose spines normalization keeps: a non-arithmetic
constructor or a type variable. -/
def headOk : Ty → Prop
  | con c => isArith c = false
  | var _ => True
  | _ => False

theorem headOk_con {c : String} (h : isArith c = false) : headOk (con c) := h

theorem isArith_of_evalNat_app {op : String} {a b : Ty} {n : Nat}
    (h : evalNat (app (app (con op) a) b) = some n) : isArith op = true := by
  rcases evalNat_some_inv h with h | ⟨op', a', b', x, y, heq, _, _, hop⟩
  · cases h
  · injection heq with h1 _
    injection h1 with h1 _
    injection h1 with h1
    subst h1
    rcases hop with ⟨rfl, _⟩ | ⟨rfl, _⟩ | ⟨rfl, _⟩ <;> rfl

/-- A spine at an `headOk` head does not evaluate. -/
theorem evalNat_none_of_flatten : ∀ {t h : Ty} {args : List Ty},
    flatten t = (h, args) → headOk h → evalNat t = none := by
  intro t h args hfl hok
  cases hev : evalNat t with
  | none => rfl
  | some n =>
      exfalso
      rcases evalNat_some_inv hev with rfl | ⟨op, a, b, x, y, rfl, _, _, _⟩
      · simp only [flatten] at hfl
        injection hfl with h1 _
        subst h1
        exact hok
      · simp only [flatten] at hfl
        injection hfl with h1 _
        subst h1
        have hok' : isArith op = false := hok
        rw [isArith_of_evalNat_app hev] at hok'
        cases hok'

/-- `flatten` under `natNorm` at a kept head: the head survives and
the arguments normalize. -/
theorem flatten_natNorm : ∀ {t h : Ty} {args : List Ty},
    flatten t = (h, args) → headOk h → flatten (natNorm t) = (h, args.map natNorm)
  | app a b, h, args, hfl, hok => by
      have hev : evalNat (app a b) = none := evalNat_none_of_flatten hfl hok
      rw [natNorm_app_of_none hev]
      simp only [flatten] at hfl ⊢
      rcases hfa : flatten a with ⟨h', args'⟩
      rw [hfa] at hfl
      dsimp only at hfl
      injection hfl with h1 h2
      subst h1; subst h2
      rw [flatten_natNorm hfa hok]
      simp
  | con c, h, args, hfl, hok => by
      simp only [flatten] at hfl
      injection hfl with h1 h2
      subst h1; subst h2
      rfl
  | var a, h, args, hfl, hok => by
      simp only [flatten] at hfl
      injection hfl with h1 h2
      subst h1; subst h2
      rfl
  | nat n, h, args, hfl, hok => by
      simp only [flatten] at hfl
      injection hfl with h1 _
      subst h1
      exact hok.elim
  | arrow a b, h, args, hfl, hok => by
      simp only [flatten] at hfl
      injection hfl with h1 _
      subst h1
      exact hok.elim

/-- Conversely, a constructor- or variable-headed normal form comes
from a spine of the same head whose arguments normalize to it. -/
theorem flatten_natNorm_inv : ∀ {u h : Ty} {as : List Ty},
    flatten (natNorm u) = (h, as) → headOk h →
    ∃ as', flatten u = (h, as') ∧ as'.map natNorm = as := by
  intro u h as hfl hok
  cases hn : natNorm u with
  | app c d =>
      rw [hn] at hfl
      obtain ⟨a, b, rfl, hev, hc, hd⟩ := natNorm_app_inv hn
      simp only [flatten] at hfl ⊢
      rcases hfc : flatten c with ⟨h', as'⟩
      rw [hfc] at hfl
      dsimp only at hfl
      injection hfl with h1 h2
      subst h1; subst h2
      rw [← hc] at hfc
      obtain ⟨as'', hfa, has⟩ := flatten_natNorm_inv hfc hok
      rw [hfa]
      exact ⟨as'' ++ [b], rfl, by rw [List.map_append, has, ← hd]; rfl⟩
  | con c =>
      rw [hn] at hfl
      simp only [flatten] at hfl
      injection hfl with h1 h2
      subst h1; subst h2
      rw [natNorm_con_inv hn]
      exact ⟨[], rfl, rfl⟩
  | var a =>
      rw [hn] at hfl
      simp only [flatten] at hfl
      injection hfl with h1 h2
      subst h1; subst h2
      rw [natNorm_var_inv hn]
      exact ⟨[], rfl, rfl⟩
  | nat n =>
      rw [hn] at hfl
      simp only [flatten] at hfl
      injection hfl with h1 _
      subst h1
      exact hok.elim
  | arrow a b =>
      rw [hn] at hfl
      simp only [flatten] at hfl
      injection hfl with h1 _
      subst h1
      exact hok.elim

/-- Pointwise relation on lists (same length). -/
def Pointwise (r : Ty → Ty → Prop) : List Ty → List Ty → Prop
  | [], [] => True
  | a :: as, b :: bs => r a b ∧ Pointwise r as bs
  | _, _ => False

namespace Pointwise

theorem nil {r : Ty → Ty → Prop} : Pointwise r [] [] := trivial

theorem cons {r : Ty → Ty → Prop} {a b : Ty} {as bs : List Ty} (h : r a b)
    (hs : Pointwise r as bs) : Pointwise r (a :: as) (b :: bs) := ⟨h, hs⟩

theorem length {r : Ty → Ty → Prop} : ∀ {as bs : List Ty}, Pointwise r as bs →
    as.length = bs.length
  | [], [], _ => rfl
  | _ :: as, _ :: bs, h => by simp [length h.2]
  | [], _ :: _, h => h.elim
  | _ :: _, [], h => h.elim

theorem get {r : Ty → Ty → Prop} : ∀ {as bs : List Ty}, Pointwise r as bs →
    ∀ i (h1 : i < as.length) (h2 : i < bs.length), r as[i] bs[i]
  | _ :: _, _ :: _, h, 0, _, _ => h.1
  | _ :: _, _ :: _, h, i + 1, h1, h2 => get h.2 i (by simpa using h1) (by simpa using h2)
  | [], [], _, i, h1, _ => absurd h1 (by simp)
  | [], _ :: _, h, _, _, _ => h.elim
  | _ :: _, [], h, _, _, _ => h.elim

theorem of_get {r : Ty → Ty → Prop} : ∀ {as bs : List Ty}, as.length = bs.length →
    (∀ i (h1 : i < as.length) (h2 : i < bs.length), r as[i] bs[i]) → Pointwise r as bs
  | [], [], _, _ => trivial
  | _ :: _, _ :: _, hlen, h =>
      ⟨h 0 (by simp) (by simp),
       of_get (by simpa using hlen) fun i h1 h2 =>
         h (i + 1) (by simpa using h1) (by simpa using h2)⟩
  | [], _ :: _, hlen, _ => by simp at hlen
  | _ :: _, [], hlen, _ => by simp at hlen

theorem nil_inv {r : Ty → Ty → Prop} : ∀ {bs : List Ty}, Pointwise r [] bs → bs = []
  | [], _ => rfl
  | _ :: _, h => h.elim

theorem one_inv {r : Ty → Ty → Prop} {a : Ty} : ∀ {bs : List Ty}, Pointwise r [a] bs →
    ∃ b, bs = [b] ∧ r a b
  | [b], h => ⟨b, rfl, h.1⟩
  | [], h => h.elim
  | _ :: _ :: _, h => h.2.elim

theorem two_inv {r : Ty → Ty → Prop} {a b : Ty} : ∀ {cs : List Ty}, Pointwise r [a, b] cs →
    ∃ c d, cs = [c, d] ∧ r a c ∧ r b d
  | [c, d], h => ⟨c, d, rfl, h.1, h.2.1⟩
  | [], h => h.elim
  | [_], h => h.2.elim
  | _ :: _ :: _ :: _, h => h.2.2.elim

theorem append {r : Ty → Ty → Prop} : ∀ {as bs : List Ty}, Pointwise r as bs →
    ∀ {cs ds : List Ty}, Pointwise r cs ds → Pointwise r (as ++ cs) (bs ++ ds)
  | [], [], _, _, _, h2 => h2
  | _ :: _, _ :: _, h, _, _, h2 => ⟨h.1, append h.2 h2⟩
  | [], _ :: _, h, _, _, _ => h.elim
  | _ :: _, [], h, _, _, _ => h.elim

theorem refl {r : Ty → Ty → Prop} (hr : ∀ a, r a a) : ∀ (as : List Ty), Pointwise r as as
  | [] => trivial
  | a :: as => ⟨hr a, refl hr as⟩

theorem symm {r : Ty → Ty → Prop} (hr : ∀ a b, r a b → r b a) :
    ∀ {as bs : List Ty}, Pointwise r as bs → Pointwise r bs as
  | [], [], _ => trivial
  | _ :: _, _ :: _, h => ⟨hr _ _ h.1, symm hr h.2⟩
  | [], _ :: _, h => h.elim
  | _ :: _, [], h => h.elim

theorem trans {r : Ty → Ty → Prop} (hr : ∀ a b c, r a b → r b c → r a c) :
    ∀ {as bs cs : List Ty}, Pointwise r as bs → Pointwise r bs cs → Pointwise r as cs
  | [], [], [], _, _ => trivial
  | _ :: _, _ :: _, _ :: _, h1, h2 => ⟨hr _ _ _ h1.1 h2.1, trans hr h1.2 h2.2⟩
  | [], _ :: _, _, h, _ => h.elim
  | _ :: _, [], _, h, _ => h.elim
  | _ :: _, _ :: _, [], _, h => h.elim

theorem map {r : Ty → Ty → Prop} {f g : Ty → Ty} : ∀ {as bs : List Ty},
    Pointwise (fun a b => r (f a) (g b)) as bs → Pointwise r (as.map f) (bs.map g)
  | [], [], _ => trivial
  | _ :: _, _ :: _, h => ⟨h.1, map h.2⟩
  | [], _ :: _, h => h.elim
  | _ :: _, [], h => h.elim

theorem of_map {r : Ty → Ty → Prop} {f g : Ty → Ty} : ∀ {as bs : List Ty},
    Pointwise r (as.map f) (bs.map g) → Pointwise (fun a b => r (f a) (g b)) as bs
  | [], [], _ => trivial
  | _ :: _, _ :: _, h => ⟨h.1, of_map h.2⟩
  | [], _ :: _, h => h.elim
  | _ :: _, [], h => h.elim

theorem mono {r r' : Ty → Ty → Prop} (hr : ∀ a b, r a b → r' a b) :
    ∀ {as bs : List Ty}, Pointwise r as bs → Pointwise r' as bs
  | [], [], _ => trivial
  | _ :: _, _ :: _, h => ⟨hr _ _ h.1, mono hr h.2⟩
  | [], _ :: _, h => h.elim
  | _ :: _, [], h => h.elim

theorem of_eq : ∀ {as bs : List Ty}, as = bs → Pointwise (fun a b => eq a b = true) as bs
  | as, _, rfl => refl eq_refl as

end Pointwise

/-- Pointwise `Ty.eq`. -/
abbrev EqL : List Ty → List Ty → Prop := Pointwise (fun a b => eq a b = true)

theorem EqL.refl (as : List Ty) : EqL as as := Pointwise.refl eq_refl as
theorem EqL.symm {as bs : List Ty} (h : EqL as bs) : EqL bs as :=
  Pointwise.symm (fun a b (h : eq a b = true) => eq_symm h) h
theorem EqL.trans {as bs cs : List Ty} (h1 : EqL as bs) (h2 : EqL bs cs) : EqL as cs :=
  Pointwise.trans (fun a b c (h1 : eq a b = true) (h2 : eq b c = true) => eq_trans h1 h2) h1 h2

/-- A `Beq`-equal pair of types has `Beq`-equal spines. -/
theorem flatten_beq : ∀ {t u h : Ty} {args : List Ty}, Beq t u → flatten t = (h, args) →
    ∃ h' args', flatten u = (h', args') ∧ Beq h h' ∧ Pointwise Beq args args'
  | app a b, u, h, args, hbeq, hfl => by
      obtain ⟨c, d, rfl, h1, h2⟩ := Beq.app_inv hbeq
      simp only [flatten] at hfl ⊢
      rcases hfa : flatten a with ⟨h', args'⟩
      rw [hfa] at hfl
      dsimp only at hfl
      injection hfl with h3 h4
      subst h3; subst h4
      obtain ⟨h'', args'', hfc, hb, hpt⟩ := flatten_beq h1 hfa
      rw [hfc]
      exact ⟨h'', args'' ++ [d], rfl, hb, Pointwise.append hpt ⟨h2, trivial⟩⟩
  | con c, u, h, args, hbeq, hfl => by
      obtain rfl := Beq.con_inv hbeq
      simp only [flatten] at hfl
      injection hfl with h1 h2
      subst h1; subst h2
      exact ⟨_, [], rfl, Beq.refl _, trivial⟩
  | var a, u, h, args, hbeq, hfl => by
      obtain ⟨b, rfl, hab⟩ := Beq.var_inv hbeq
      simp only [flatten] at hfl
      injection hfl with h1 h2
      subst h1; subst h2
      exact ⟨var b, [], rfl, hbeq, trivial⟩
  | nat n, u, h, args, hbeq, hfl => by
      obtain rfl := Beq.nat_inv hbeq
      simp only [flatten] at hfl
      injection hfl with h1 h2
      subst h1; subst h2
      exact ⟨_, [], rfl, Beq.refl _, trivial⟩
  | arrow a b, u, h, args, hbeq, hfl => by
      obtain ⟨c, d, rfl, _, _⟩ := Beq.arrow_inv hbeq
      simp only [flatten] at hfl
      injection hfl with h1 h2
      subst h1; subst h2
      exact ⟨_, [], rfl, hbeq, trivial⟩

/-- THE spine lemma: at a kept head, `Ty.eq` preserves the head and
relates the arguments pointwise. -/
theorem flatten_eq {t u h : Ty} {args : List Ty} (heq : eq t u = true)
    (hfl : flatten t = (h, args)) (hok : headOk h) :
    ∃ h' args', flatten u = (h', args') ∧ Beq h h' ∧ EqL args args' := by
  have h1 := flatten_natNorm hfl hok
  obtain ⟨h', as', hfl', hb, hpt⟩ := flatten_beq heq h1
  have hok' : headOk h' := by
    cases h with
    | con c => obtain rfl := Beq.con_inv hb; exact hok
    | var a => obtain ⟨b, rfl, _⟩ := Beq.var_inv hb; trivial
    | app _ _ | nat _ | arrow _ _ => exact hok.elim
  obtain ⟨args', hflu, hmap⟩ := flatten_natNorm_inv hfl' hok'
  refine ⟨h', args', hflu, hb, ?_⟩
  rw [← hmap] at hpt
  exact Pointwise.of_map hpt

/-- The constructor-headed instance. -/
theorem flatten_eq_con {t u : Ty} {c : String} {args : List Ty} (heq : eq t u = true)
    (hfl : flatten t = (con c, args)) (hc : isArith c = false) :
    ∃ args', flatten u = (con c, args') ∧ EqL args args' := by
  obtain ⟨h', args', hflu, hb, hpt⟩ := flatten_eq heq hfl (headOk_con hc)
  obtain rfl := Beq.con_inv hb
  exact ⟨args', hflu, hpt⟩

/-- The variable-headed instance. -/
theorem flatten_eq_var {t u : Ty} {a : TyVar} {args : List Ty} (heq : eq t u = true)
    (hfl : flatten t = (var a, args)) :
    ∃ b args', flatten u = (var b, args') := by
  obtain ⟨h', args', hflu, hb, _⟩ := flatten_eq heq hfl trivial
  obtain ⟨b, rfl, _⟩ := Beq.var_inv hb
  exact ⟨b, args', hflu⟩

/-- An evaluable application stays evaluable, at the same value,
under `Ty.eq` of its components. -/
theorem evalNat_app_transfer {a a' b b' : Ty} {n : Nat} (ha : eq a a' = true)
    (hb : eq b b' = true) (hev : evalNat (app a b) = some n) :
    evalNat (app a' b') = some n := by
  rcases evalNat_some_inv hev with h | ⟨op, x, y, m, k, heq, hx, hy, hop⟩
  · cases h
  · injection heq with h1 h2
    subst h1; subst h2
    have hna : natNorm (app (con op) x) = app (con op) (natNorm x) := by
      rw [natNorm_app_of_none (by rfl)]
      rfl
    have hb' : Beq (app (con op) (natNorm x)) (natNorm a') := hna ▸ ha
    obtain ⟨c, d, hcd, hc, hd⟩ := Beq.app_inv hb'
    obtain ⟨a₁, a₂, rfl, _, ha₁, ha₂⟩ := natNorm_app_inv hcd
    have hc' : natNorm a₁ = con op := by rw [ha₁]; exact Beq.con_inv hc
    obtain rfl := natNorm_con_inv hc'
    have hxa : eq x a₂ = true := by
      show Beq (natNorm x) (natNorm a₂)
      rw [ha₂]; exact hd
    have hx' : evalNat a₂ = some m := (evalNat_eq hxa).symm.trans hx
    have hy' : evalNat b' = some k := (evalNat_eq hb).symm.trans hy
    rw [evalNat_app_app_con hx' hy']
    rw [evalNat_app_app_con hx hy] at hev
    exact hev

/-- Congruence of application. -/
theorem eq_app_congr {a a' b b' : Ty} (ha : eq a a' = true) (hb : eq b b' = true) :
    eq (app a b) (app a' b') = true := by
  cases hev : evalNat (app a b) with
  | some n =>
      have hev' := evalNat_app_transfer ha hb hev
      show Beq (natNorm (app a b)) (natNorm (app a' b'))
      rw [natNorm_of_evalNat hev, natNorm_of_evalNat hev']
      exact Beq.refl _
  | none =>
      have hev' : evalNat (app a' b') = none := by
        cases h' : evalNat (app a' b') with
        | none => rfl
        | some n =>
            have := evalNat_app_transfer (eq_symm ha) (eq_symm hb) h'
            rw [this] at hev
            cases hev
      show Beq (natNorm (app a b)) (natNorm (app a' b'))
      rw [natNorm_app_of_none hev, natNorm_app_of_none hev']
      exact Beq.app_intro ha hb

/-- Congruence of arrows. -/
theorem eq_arrow_congr {a a' b b' : Ty} (ha : eq a a' = true) (hb : eq b b' = true) :
    eq (arrow a b) (arrow a' b') = true := by
  show Beq (natNorm (arrow a b)) (natNorm (arrow a' b'))
  rw [natNorm_arrow, natNorm_arrow]
  exact Beq.arrow_intro ha hb

/-- Spines with a non-empty argument list come from an application. -/
theorem flatten_snoc_inv : ∀ {t h x : Ty} {as : List Ty}, flatten t = (h, as ++ [x]) →
    ∃ t₁, t = app t₁ x ∧ flatten t₁ = (h, as) := by
  intro t h x as hfl
  cases t with
  | app t₁ t₂ =>
      simp only [flatten] at hfl
      rcases hf : flatten t₁ with ⟨h', as'⟩
      rw [hf] at hfl
      dsimp only at hfl
      injection hfl with h1 h2
      subst h1
      obtain ⟨h3, h4⟩ := List.append_inj' h2 rfl
      subst h3
      injection h4 with h4
      subst h4
      exact ⟨t₁, rfl, hf⟩
  | con _ | var _ | nat _ | arrow _ _ =>
      simp only [flatten] at hfl
      exact absurd (congrArg Prod.snd hfl) (by simp)

theorem flatten_nil_inv : ∀ {t h : Ty}, flatten t = (h, []) → t = h := by
  intro t h hfl
  cases t with
  | app t₁ t₂ =>
      simp only [flatten] at hfl
      rcases hf : flatten t₁ with ⟨h', as'⟩
      rw [hf] at hfl
      dsimp only at hfl
      exact absurd (congrArg Prod.snd hfl) (by simp)
  | con _ | var _ | nat _ | arrow _ _ => exact congrArg Prod.fst hfl

/-- `Ty.eq` on non-evaluable applications is componentwise. -/
theorem eq_app_inv {a b c d : Ty} (h : eq (app a b) (app c d) = true)
    (hev : evalNat (app a b) = none) : eq a c = true ∧ eq b d = true := by
  have hev' : evalNat (app c d) = none := (evalNat_eq h).symm.trans hev
  have h' : Beq (app (natNorm a) (natNorm b)) (app (natNorm c) (natNorm d)) := by
    have := beqN_of_eq h
    rw [natNorm_app_of_none hev, natNorm_app_of_none hev'] at this
    exact this
  obtain ⟨c', d', hcd, h1, h2⟩ := Beq.app_inv h'
  injection hcd with h3 h4
  subst h3; subst h4
  exact ⟨h1, h2⟩

theorem isTupleCon_not_arith {c : String} (h : isTupleCon c = true) : isArith c = false := by
  simp only [isTupleCon, Bool.and_eq_true] at h
  have h1 := h.1.1
  cases hc : isArith c
  · rfl
  · exfalso
    simp only [isArith, Bool.or_eq_true, beq_iff_eq] at hc
    rcases hc with (rfl | rfl) | rfl <;> simp at h1

/-! ## Substitutions modulo `Ty.eq` -/

/-- Two substitutions agree modulo `Ty.eq`. -/
def SubEqv (s s' : HashMap TyVar Ty) : Prop :=
  ∀ v, (∀ a, s.get? v = some a → ∃ b, s'.get? v = some b ∧ eq a b = true) ∧
       (∀ b, s'.get? v = some b → ∃ a, s.get? v = some a ∧ eq a b = true)

theorem SubEqv.refl (s : HashMap TyVar Ty) : SubEqv s s :=
  fun _ => ⟨fun a h => ⟨a, h, eq_refl a⟩, fun b h => ⟨b, h, eq_refl b⟩⟩

theorem SubEqv.none {s s' : HashMap TyVar Ty} (h : SubEqv s s') {v : TyVar}
    (hn : s.get? v = none) : s'.get? v = none := by
  cases hg : s'.get? v with
  | none => rfl
  | some b =>
      obtain ⟨a, ha, _⟩ := (h v).2 b hg
      rw [ha] at hn
      cases hn

theorem SubEqv.insert {s s' : HashMap TyVar Ty} (h : SubEqv s s') {v : TyVar} {a b : Ty}
    (hab : eq a b = true) : SubEqv (s.insert v a) (s'.insert v b) := by
  intro w
  simp only [HashMap.get?_eq_getElem?, HashMap.getElem?_insert]
  constructor
  · intro x hx
    by_cases hvw : (v == w) = true
    · rw [if_pos hvw] at hx ⊢
      injection hx with hx
      subst hx
      exact ⟨b, rfl, hab⟩
    · rw [if_neg hvw] at hx ⊢
      obtain ⟨y, hy, hxy⟩ := (h w).1 x (by rw [HashMap.get?_eq_getElem?]; exact hx)
      exact ⟨y, by rw [HashMap.get?_eq_getElem?] at hy; exact hy, hxy⟩
  · intro y hy
    by_cases hvw : (v == w) = true
    · rw [if_pos hvw] at hy ⊢
      injection hy with hy
      subst hy
      exact ⟨a, rfl, hab⟩
    · rw [if_neg hvw] at hy ⊢
      obtain ⟨x, hx, hxy⟩ := (h w).2 y (by rw [HashMap.get?_eq_getElem?]; exact hy)
      exact ⟨x, by rw [HashMap.get?_eq_getElem?] at hx; exact hx, hxy⟩

/-- Substitution is a congruence for `Ty.eq`. -/
theorem substTv_congr {s s' : HashMap TyVar Ty} (h : SubEqv s s') :
    ∀ (t : Ty), eq (DEnv.substTv s t) (DEnv.substTv s' t) = true
  | var v => by
      simp only [DEnv.substTv]
      cases hg : s.get? v with
      | some a =>
          obtain ⟨b, hb, hab⟩ := (h v).1 a hg
          rw [hb]
          exact hab
      | none =>
          rw [SubEqv.none h hg]
          exact eq_refl _
  | app a b => by
      simp only [DEnv.substTv]
      exact eq_app_congr (substTv_congr h a) (substTv_congr h b)
  | arrow a b => by
      simp only [DEnv.substTv]
      exact eq_arrow_congr (substTv_congr h a) (substTv_congr h b)
  | con c => eq_refl _
  | nat n => eq_refl _

/-! ## `matchTy` modulo `Ty.eq` -/

private theorem toList_ofList_singleton {v : TyVar} {b : Ty} :
    (HashMap.ofList [(v, b)]).toList = [(v, b)] := by
  rw [HashMap.ofList_singleton]
  have := @HashMap.toList_insert_perm TyVar Ty _ _ (∅ : HashMap TyVar Ty) _ _ v b
  rw [HashMap.toList_empty] at this
  simp only [List.filter_nil] at this
  exact List.perm_singleton.mp this

/-- The variable-binding step of `matchTy`: a bare variable in the
pattern's last argument position binds the corresponding subterm;
first occurrence wins, and a later occurrence must agree modulo
`natNorm`. -/
theorem matchTy_app_var {p : Ty} {v : TyVar} {a b : Ty} :
    DEnv.matchTy (app p (var v)) (app a b) =
      (do
        let s₁ ← DEnv.matchTy p a
        match s₁.get? v with
        | some t' =>
            if (natNorm b == natNorm t') then pure s₁
            else throw "matchTy: inconsistent assignment of a type variable"
        | none => pure (s₁.insert v b)) := by
  rw [DEnv.matchTy]
  cases hs : DEnv.matchTy p a with
  | error e => rfl
  | ok s₁ =>
      rw [show DEnv.matchTy (var v) b = (pure (HashMap.ofList [(v, b)]) : Except String _)
            from rfl]
      show (forIn (HashMap.ofList [(v, b)]) s₁ _ >>= fun s => (pure s : Except String _)) = _
      rw [HashMap.forIn_eq_forIn_toList, toList_ofList_singleton, List.forIn_cons]
      show (_ : Except String (HashMap TyVar Ty)) = (match s₁.get? v with
        | some t' =>
            if (natNorm b == natNorm t') then (pure s₁ : Except String _)
            else throw "matchTy: inconsistent assignment of a type variable"
        | none => pure (s₁.insert v b))
      cases hg : s₁.get? v with
      | some t' =>
          simp only [hg]
          by_cases hb : (natNorm b == natNorm t') = true
          · simp only [hb, if_true]
            rfl
          · simp only [hb]
            rfl
      | none =>
          simp only [hg]
          rfl

/-- A pattern that is not an application against anything, and an
application pattern against a non-application, both bind nothing. -/
theorem matchTy_con {c : String} : ∀ (t : Ty), DEnv.matchTy (con c) t = .ok ∅
  | con _ => rfl
  | app _ _ => rfl
  | var _ => rfl
  | nat _ => rfl
  | arrow _ _ => rfl

theorem matchTy_app_con {p q : Ty} {c : String} : DEnv.matchTy (app p q) (con c) = .ok ∅ := rfl

/-- All-variable argument lists (the shape of a constructor's result
type: its datatype applied to its parameters). -/
def isVar : Ty → Bool
  | var _ => true
  | _ => false

def varArgs (as : List Ty) : Bool := as.all isVar

theorem varArgs_snoc_inv {as : List Ty} {q : Ty} (h : varArgs (as ++ [q]) = true) :
    varArgs as = true ∧ ∃ w, q = var w := by
  simp only [varArgs, List.all_append, List.all_cons, List.all_nil, Bool.and_true,
    Bool.and_eq_true] at h
  refine ⟨h.1, ?_⟩
  have h2 := h.2
  cases q with
  | var w => exact ⟨w, rfl⟩
  | con _ | app _ _ | nat _ | arrow _ _ => cases h2

/-- THE matching lemma: against a datatype-applied-to-variables
pattern, `Ty.eq`-equal instance types (at a kept constructor head)
match to `SubEqv`-equal substitutions. -/
theorem matchTy_congr : ∀ (tres : Ty) {tc : String} {vs : List Ty},
    flatten tres = (con tc, vs) → varArgs vs = true →
    ∀ {t u : Ty} {c : String} {args : List Ty} {sub : HashMap TyVar Ty},
      flatten t = (con c, args) → isArith c = false → eq t u = true →
      DEnv.matchTy tres t = .ok sub →
      ∃ sub', DEnv.matchTy tres u = .ok sub' ∧ SubEqv sub sub'
  | con d, tc, vs, hres, _, t, u, c, args, sub, hflt, hc, htu, hm => by
      rw [matchTy_con] at hm
      injection hm with hm
      subst hm
      exact ⟨∅, matchTy_con u, SubEqv.refl _⟩
  | var w, tc, vs, hres, _, _, _, _, _, _, _, _, _, _ => by
      simp only [flatten] at hres
      cases (Prod.mk.inj hres).1
  | nat n, tc, vs, hres, _, _, _, _, _, _, _, _, _, _ => by
      simp only [flatten] at hres
      cases (Prod.mk.inj hres).1
  | arrow _ _, tc, vs, hres, _, _, _, _, _, _, _, _, _, _ => by
      simp only [flatten] at hres
      cases (Prod.mk.inj hres).1
  | app p q, tc, vs, hres, hvs, t, u, c, args, sub, hflt, hc, htu, hm => by
      -- the pattern's spine
      simp only [flatten] at hres
      rcases hfp : flatten p with ⟨hp, ps⟩
      rw [hfp] at hres
      dsimp only at hres
      injection hres with h1 h2
      subst h1; subst h2
      obtain ⟨hvps, w, rfl⟩ := varArgs_snoc_inv hvs
      -- the instance type's spine
      cases t with
      | app t₁ t₂ =>
          have hev : evalNat (app t₁ t₂) = none :=
            evalNat_none_of_flatten hflt (headOk_con hc)
          simp only [flatten] at hflt
          rcases hft : flatten t₁ with ⟨ht, ts⟩
          rw [hft] at hflt
          dsimp only at hflt
          injection hflt with h3 h4
          subst h3; subst h4
          -- `u` has the same spine
          obtain ⟨args', hflu, hpt⟩ :=
            flatten_eq_con htu (by simp only [flatten]; rw [hft]) hc
          obtain ⟨us, u₂, rfl⟩ : ∃ us u₂, args' = us ++ [u₂] := by
            rcases List.eq_nil_or_concat args' with rfl | ⟨us, u₂, rfl⟩
            · have hl := Pointwise.length hpt
              rw [List.length_append, List.length_singleton, List.length_nil] at hl
              omega
            · exact ⟨us, u₂, List.concat_eq_append⟩
          obtain ⟨u₁, rfl, hfu⟩ := flatten_snoc_inv hflu
          obtain ⟨h12, h34⟩ := eq_app_inv htu hev
          -- the step on both sides
          rw [matchTy_app_var] at hm ⊢
          obtain ⟨s₁, hs₁, hm⟩ := except_bind_eq_ok hm
          obtain ⟨s₁', hs₁', hse⟩ :=
            matchTy_congr p hfp hvps hft hc h12 hs₁
          rw [hs₁', except_bind_ok]
          try dsimp only at hm ⊢
          cases hg : s₁.get? w with
          | some t' =>
              rw [hg] at hm
              obtain ⟨t'', hg', htt⟩ := (hse w).1 t' hg
              rw [hg']
              dsimp only at hm ⊢
              split at hm
              rotate_left
              · exact error_ne_ok hm
              rename_i hcons
              injection hm with hm
              subst hm
              have hcons' : (natNorm u₂ == natNorm t'') = true :=
                Beq.trans (Beq.symm (beqN_of_eq h34)) (Beq.trans hcons (beqN_of_eq htt))
              rw [if_pos hcons']
              exact ⟨s₁', rfl, hse⟩
          | none =>
              rw [hg] at hm
              rw [SubEqv.none hse hg]
              try dsimp only at hm ⊢
              injection hm with hm
              subst hm
              exact ⟨_, rfl, SubEqv.insert hse h34⟩
      | con d =>
          have hp : d = c ∧ args = [] := by simpa [flatten] using hflt
          obtain ⟨rfl, rfl⟩ := hp
          obtain rfl : u = con d := by
            obtain ⟨args', hflu, hpt⟩ := flatten_eq_con htu rfl hc
            cases args' with
            | nil => exact flatten_nil_inv hflu
            | cons _ _ => exact hpt.elim
          rw [matchTy_app_con] at hm ⊢
          injection hm with hm
          subst hm
          exact ⟨∅, rfl, SubEqv.refl _⟩
      | var _ | nat _ | arrow _ _ =>
          simp only [flatten] at hflt
          cases (Prod.mk.inj hflt).1

end Ty

/-! ## The datatype-environment gate for nat normalization -/

/-- A constructor signature's result type is its datatype applied to
type variables (the checker's rule for data declarations). -/
def sigShapeOk (sig : Sig) : Bool :=
  match Ty.flatten (Ty.flattenArrow sig.ty).2 with
  | (.con _, args) => Ty.varArgs args
  | _ => false

namespace DEnv

/-- No datatype is named after a type-level arithmetic operator. -/
def arithFree (Δ : DEnv) : Bool :=
  (Δ.ctors.get? "+").isNone && (Δ.ctors.get? "-").isNone && (Δ.ctors.get? "*").isNone

/-- Every constructor signature is well-shaped. -/
def sigsOk (Δ : DEnv) : Bool := Δ.ctorSig.toList.all fun pr => sigShapeOk pr.2

/-- The gate: both facts, decided. -/
def natOk (Δ : DEnv) : Bool := Δ.arithFree && Δ.sigsOk

theorem natOk_arith {Δ : DEnv} (h : natOk Δ = true) {c : String} (hc : Ty.isArith c = true) :
    Δ.ctors.get? c = none := by
  simp only [natOk, arithFree, Bool.and_eq_true, Option.isNone_iff_eq_none] at h
  simp only [Ty.isArith, Bool.or_eq_true, beq_iff_eq] at hc
  rcases hc with (rfl | rfl) | rfl
  · exact h.1.1.1
  · exact h.1.1.2
  · exact h.1.2

theorem natOk_sig {Δ : DEnv} (h : natOk Δ = true) {c : String} {sig : Sig}
    (hc : Δ.ctorSig.get? c = some sig) : sigShapeOk sig = true := by
  simp only [natOk, Bool.and_eq_true] at h
  have hmem : (c, sig) ∈ Δ.ctorSig.toList := by
    rw [HashMap.mem_toList_iff_getElem?_eq_some, ← HashMap.get?_eq_getElem?]
    exact hc
  exact List.all_eq_true.mp h.2 _ hmem

/-- A declared datatype head is not arithmetic. -/
theorem natOk_ctors_not_arith {Δ : DEnv} (h : natOk Δ = true) {c : String} {cs : List String}
    (hc : Δ.ctors.get? c = some cs) : Ty.isArith c = false := by
  cases ha : Ty.isArith c
  · rfl
  · rw [natOk_arith h ha] at hc; cases hc

theorem sigShapeOk_inv {sig : Sig} (h : sigShapeOk sig = true) :
    ∃ tc vs, Ty.flatten (Ty.flattenArrow sig.ty).2 = (.con tc, vs) ∧ Ty.varArgs vs = true := by
  rw [sigShapeOk] at h
  split at h
  · rename_i tc vs heq
    exact ⟨tc, vs, heq, h⟩
  · cases h

/-! ## Width invariance -/

private theorem mapM_ok_pw {r : Ty → Ty → Prop} {f g : Ty → Except String Nat} :
    ∀ {xs xs' : List Ty} {ys : List Nat}, Ty.Pointwise r xs xs' →
      (∀ a a' b, r a a' → f a = .ok b → g a' = .ok b) →
      xs.mapM f = .ok ys → xs'.mapM g = .ok ys
  | [], [], ys, _, _, h => by simpa using h
  | x :: xs, x' :: xs', ys, hpw, hfg, h => by
      rw [List.mapM_cons] at h ⊢
      obtain ⟨b, hb, h⟩ := except_bind_eq_ok h
      obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
      rw [hfg x x' b hpw.1 hb, except_bind_ok, mapM_ok_pw hpw.2 hfg hbs, except_bind_ok]
      exact h
  | [], _ :: _, _, hpw, _, _ => hpw.elim
  | _ :: _, [], _, hpw, _, _ => hpw.elim

private theorem mapM_ok_of_pointwise {α β : Type} {f g : α → Except String β} :
    ∀ {xs : List α} {ys : List β}, (∀ a b, f a = .ok b → g a = .ok b) →
      xs.mapM f = .ok ys → xs.mapM g = .ok ys
  | [], ys, _, h => by simpa using h
  | x :: xs, ys, hfg, h => by
      rw [List.mapM_cons] at h ⊢
      obtain ⟨b, hb, h⟩ := except_bind_eq_ok h
      obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
      rw [hfg x b hb, except_bind_ok, mapM_ok_of_pointwise hfg hbs, except_bind_ok]
      exact h

private theorem length_two {α : Type} : ∀ {l : List α}, l.length = 2 → ∃ a b, l = [a, b]
  | [a, b], _ => ⟨a, b, rfl⟩
  | [], h => by simp at h
  | [_], h => by simp at h
  | _ :: _ :: _ :: _, h => by simp at h

/-- The visited-set test is a function of the `Ty.eq` classes. -/
theorem any_eq_congr : ∀ {vis vis' : List Ty}, Ty.EqL vis vis' → ∀ {t t' : Ty}, Ty.eq t t' = true →
    vis.any (Ty.eq · t) = vis'.any (Ty.eq · t')
  | [], [], _, _, _, _ => rfl
  | a :: vis, a' :: vis', h, t, t', htt => by
      simp only [List.any_cons]
      rw [any_eq_congr h.2 htt]
      congr 1
      rw [Bool.eq_iff_iff]
      constructor
      · intro hat
        exact Ty.eq_trans (Ty.eq_symm h.1) (Ty.eq_trans hat htt)
      · intro hat
        exact Ty.eq_trans h.1 (Ty.eq_trans hat (Ty.eq_symm htt))
  | [], _ :: _, h, _, _, _ => h.elim
  | _ :: _, [], h, _, _, _ => h.elim

/-! The per-shape unfoldings of `sizeOf` at positive fuel. -/

theorem sizeOf_vec {Δ : DEnv} {k : Nat} {vis : List Ty} {t n te : Ty}
    (hfl : Ty.flatten t = (.con "Vec", [n, te])) :
    Δ.sizeOf (k + 1) vis t =
      match Ty.evalNat n with
      | some m => do pure (m * (← Δ.sizeOf k vis te))
      | none   => throw "sizeOf: can't determine the size of a Vec" := by
  rw [DEnv.sizeOf]
  split
  case h_1 =>
      rename_i n' te' heq
      rw [hfl] at heq
      have hp : n = n' ∧ te = te' := by simpa using heq
      obtain ⟨rfl, rfl⟩ := hp
      rfl
  case h_2 => rename_i heq; simp [hfl] at heq
  case h_3 => rename_i heq; simp [hfl] at heq
  case h_4 => rename_i heq; simp [hfl] at heq
  case h_5 =>
      rename_i c args hvec _ _ _ heq
      rw [hfl] at heq
      have hp : "Vec" = c ∧ [n, te] = args := by simpa using heq
      exact (hvec n te hp.2.symm hp.1.symm).elim
  case h_6 => rename_i heq; simp [hfl] at heq
  case h_7 =>
      rename_i _ _ _ _ hcon _
      exact absurd hfl (hcon _ _)

theorem sizeOf_finite {Δ : DEnv} {k : Nat} {vis : List Ty} {t n : Ty}
    (hfl : Ty.flatten t = (.con "Finite", [n])) :
    Δ.sizeOf (k + 1) vis t =
      match Ty.evalNat n with
      | some m => pure (nbits m)
      | none   => throw "sizeOf: can't determine the size of a Finite" := by
  rw [DEnv.sizeOf]
  split
  case h_1 => rename_i heq; simp [hfl] at heq
  case h_2 =>
      rename_i n' heq
      rw [hfl] at heq
      have hp : n = n' := by simpa using heq
      subst hp
      rfl
  case h_3 => rename_i heq; simp [hfl] at heq
  case h_4 => rename_i heq; simp [hfl] at heq
  case h_5 =>
      rename_i c args _ hfin _ _ heq
      rw [hfl] at heq
      have hp : "Finite" = c ∧ [n] = args := by simpa using heq
      exact (hfin n hp.2.symm hp.1.symm).elim
  case h_6 => rename_i heq; simp [hfl] at heq
  case h_7 =>
      rename_i _ _ _ _ hcon _
      exact absurd hfl (hcon _ _)

theorem sizeOf_integer {Δ : DEnv} {k : Nat} {vis : List Ty} {t : Ty}
    (hfl : Ty.flatten t = (.con "Integer", [])) :
    Δ.sizeOf (k + 1) vis t = pure 128 := by
  rw [DEnv.sizeOf]
  split
  case h_1 => rename_i heq; simp [hfl] at heq
  case h_2 => rename_i heq; simp [hfl] at heq
  case h_3 => rfl
  case h_4 => rename_i heq; simp [hfl] at heq
  case h_5 =>
      rename_i c args _ _ hint _ heq
      rw [hfl] at heq
      have hp : "Integer" = c ∧ [] = args := by simpa using heq
      exact (hint hp.2.symm hp.1.symm).elim
  case h_6 => rename_i heq; simp [hfl] at heq
  case h_7 =>
      rename_i _ _ _ _ hcon _
      exact absurd hfl (hcon _ _)

theorem sizeOf_proxy {Δ : DEnv} {k : Nat} {vis : List Ty} {t : Ty} {args : List Ty}
    (hfl : Ty.flatten t = (.con "Proxy", args)) :
    Δ.sizeOf (k + 1) vis t = pure 0 := by
  rw [DEnv.sizeOf]
  split
  case h_1 => rename_i heq; simp [hfl] at heq
  case h_2 => rename_i heq; simp [hfl] at heq
  case h_3 => rename_i heq; simp [hfl] at heq
  case h_4 => rfl
  case h_5 =>
      rename_i c args' _ _ _ hprox heq
      rw [hfl] at heq
      have hp : "Proxy" = c ∧ args = args' := by simpa using heq
      exact (hprox hp.1.symm).elim
  case h_6 => rename_i heq; simp [hfl] at heq
  case h_7 =>
      rename_i _ _ _ _ hcon _
      exact absurd hfl (hcon _ _)

theorem sizeOf_var {Δ : DEnv} {k : Nat} {vis : List Ty} {t : Ty} {a : TyVar} {args : List Ty}
    (hfl : Ty.flatten t = (.var a, args)) :
    Δ.sizeOf (k + 1) vis t = pure 0 := by
  rw [DEnv.sizeOf]
  split
  case h_1 => rename_i heq; simp [hfl] at heq
  case h_2 => rename_i heq; simp [hfl] at heq
  case h_3 => rename_i heq; simp [hfl] at heq
  case h_4 => rename_i heq; simp [hfl] at heq
  case h_5 => rename_i heq; simp [hfl] at heq
  case h_6 => rfl
  case h_7 =>
      rename_i _ _ _ _ _ hvar
      exact absurd hfl (hvar _ _)

/-- The generic constructor arm (everything `flatten`s to a
constructor head but is not one of the bit-reading shapes). -/
theorem sizeOf_con {Δ : DEnv} {k : Nat} {vis : List Ty} {t : Ty} {c : String} {args : List Ty}
    (hfl : Ty.flatten t = (.con c, args))
    (hnv : c = "Vec" → args.length ≠ 2) (hnf : c = "Finite" → args.length ≠ 1)
    (hni : c = "Integer" → args ≠ []) (hnp : c ≠ "Proxy") :
    Δ.sizeOf (k + 1) vis t =
      (if Ty.isTupleCon c then do
          let ws ← args.mapM (Δ.sizeOf k vis)
          pure ws.sum
        else if vis.any (Ty.eq · t) then
          throw s!"sizeOf: can't determine the size of a recursive datatype: {c}"
        else
          match Δ.ctors.get? c with
          | some cs => do
              let ws ← cs.mapM (Δ.ctorWidth k (t :: vis) t)
              pure (nbits cs.length + (ws.foldl max 0))
          | none => throw s!"sizeOf: couldn't calculate the size of a type ({c})") := by
  rw [DEnv.sizeOf]
  split
  case h_1 =>
      rename_i n te heq
      rw [hfl] at heq
      have hp : c = "Vec" ∧ args = [n, te] := by simpa using heq
      exact absurd (by rw [hp.2]; rfl) (hnv hp.1)
  case h_2 =>
      rename_i n heq
      rw [hfl] at heq
      have hp : c = "Finite" ∧ args = [n] := by simpa using heq
      exact absurd (by rw [hp.2]; rfl) (hnf hp.1)
  case h_3 =>
      rename_i heq
      rw [hfl] at heq
      have hp : c = "Integer" ∧ args = [] := by simpa using heq
      exact absurd hp.2 (hni hp.1)
  case h_4 =>
      rename_i args' heq
      rw [hfl] at heq
      have hp : c = "Proxy" ∧ args = args' := by simpa using heq
      exact absurd hp.1 hnp
  case h_5 =>
      rename_i c' args' _ _ _ _ heq
      rw [hfl] at heq
      have hp : c = c' ∧ args = args' := by simpa using heq
      obtain ⟨rfl, rfl⟩ := hp
      rfl
  case h_6 => rename_i heq; simp [hfl] at heq
  case h_7 =>
      rename_i _ _ _ _ hcon _
      exact absurd hfl (hcon _ _)

/-- Width computations are invariant under `Ty.eq` (pointwise along
the visited list), under the gate. -/
theorem sizeOf_ctorWidth_eq (Δ : DEnv) (hΔ : natOk Δ = true) :
    ∀ k,
      (∀ vis vis' t t' n, Ty.EqL vis vis' → Ty.eq t t' = true →
        Δ.sizeOf k vis t = .ok n → Δ.sizeOf k vis' t' = .ok n) ∧
      (∀ vis vis' t t' c n, Ty.EqL vis vis' → Ty.eq t t' = true →
        (∃ tc args, Ty.flatten t = (.con tc, args) ∧ Ty.isArith tc = false) →
        Δ.ctorWidth k vis t c = .ok n → Δ.ctorWidth k vis' t' c = .ok n) := by
  intro k
  induction k with
  | zero =>
      refine ⟨fun vis vis' t t' n _ _ h => ?_, fun vis vis' t t' c n _ _ _ h => ?_⟩
      · rw [DEnv.sizeOf] at h; exact error_ne_ok h
      · rw [DEnv.ctorWidth] at h; exact error_ne_ok h
  | succ k ihk =>
      obtain ⟨ihS, ihC⟩ := ihk
      constructor
      · intro vis vis' t t' n hvis htt h
        rcases hfl : Ty.flatten t with ⟨hd, args⟩
        cases hd with
        | con c =>
            by_cases hc : Ty.isArith c = true
            · -- an arithmetic head: no datatype, so `sizeOf` fails
              exfalso
              have hnt : Ty.isTupleCon c = false := by
                cases ht : Ty.isTupleCon c
                · rfl
                · rw [Ty.isTupleCon_not_arith ht] at hc; cases hc
              have hnone := natOk_arith hΔ hc
              have hnv : c = "Vec" → args.length ≠ 2 := by
                intro h1; subst h1; cases hc
              have hnf : c = "Finite" → args.length ≠ 1 := by
                intro h1; subst h1; cases hc
              have hni : c = "Integer" → args ≠ [] := by
                intro h1; subst h1; cases hc
              have hnp : c ≠ "Proxy" := by
                intro h1; subst h1; cases hc
              rw [sizeOf_con hfl hnv hnf hni hnp, if_neg (by simp [hnt])] at h
              split at h
              · exact error_ne_ok h
              · rw [hnone] at h
                exact error_ne_ok h
            · have hc' : Ty.isArith c = false := by simpa using hc
              obtain ⟨args', hfl', hpt⟩ := Ty.flatten_eq_con htt hfl hc'
              have hlen := Ty.Pointwise.length hpt
              -- dispatch on the shape
              by_cases hvec : c = "Vec" ∧ args.length = 2
              · obtain ⟨rfl, hl2⟩ := hvec
                obtain ⟨n₁, te, rfl⟩ := length_two hl2
                obtain ⟨n₁', te', rfl⟩ := length_two (hlen ▸ hl2)
                rw [sizeOf_vec hfl] at h
                rw [sizeOf_vec hfl', ← Ty.evalNat_eq hpt.1]
                split at h
                · obtain ⟨w, hw, h⟩ := except_bind_eq_ok h
                  rw [ihS _ _ _ _ _ hvis hpt.2.1 hw, except_bind_ok]
                  exact h
                · exact error_ne_ok h
              by_cases hfin : c = "Finite" ∧ args.length = 1
              · obtain ⟨rfl, hl1⟩ := hfin
                obtain ⟨n₁, rfl⟩ := List.length_eq_one_iff.mp hl1
                obtain ⟨n₁', rfl⟩ := List.length_eq_one_iff.mp (hlen ▸ hl1)
                rw [sizeOf_finite hfl] at h
                rw [sizeOf_finite hfl', ← Ty.evalNat_eq hpt.1]
                exact h
              by_cases hint : c = "Integer" ∧ args = []
              · obtain ⟨rfl, rfl⟩ := hint
                cases args' with
                | nil => rw [sizeOf_integer hfl] at h; rw [sizeOf_integer hfl']; exact h
                | cons _ _ => exact hpt.elim
              by_cases hprox : c = "Proxy"
              · subst hprox
                rw [sizeOf_proxy hfl] at h
                rw [sizeOf_proxy hfl']
                exact h
              -- the generic constructor arm
              have hnv : c = "Vec" → args.length ≠ 2 := fun h1 h2 => hvec ⟨h1, h2⟩
              have hnf : c = "Finite" → args.length ≠ 1 := fun h1 h2 => hfin ⟨h1, h2⟩
              have hni : c = "Integer" → args ≠ [] := fun h1 h2 => hint ⟨h1, h2⟩
              rw [sizeOf_con hfl hnv hnf hni hprox] at h
              rw [sizeOf_con hfl' (by rw [← hlen]; exact hnv) (by rw [← hlen]; exact hnf)
                    (by intro h1 h2; exact hni h1 (List.eq_nil_of_length_eq_zero (by rw [hlen, h2]; rfl))) hprox]
              by_cases htup : Ty.isTupleCon c = true
              · rw [if_pos htup] at h ⊢
                obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
                rw [mapM_ok_pw hpt (fun a a' b hab hb => ihS _ _ _ _ _ hvis hab hb) hws,
                    except_bind_ok]
                exact h
              · rw [if_neg htup] at h ⊢
                rw [← any_eq_congr hvis htt]
                split at h
                · exact error_ne_ok h
                · rename_i hnvis
                  rw [if_neg hnvis]
                  split at h
                  · obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
                    rw [mapM_ok_of_pointwise
                          (fun a b hab => ihC _ _ _ _ _ _ (Ty.Pointwise.cons htt hvis) htt ⟨c, args, hfl, hc'⟩ hab)
                          hws, except_bind_ok]
                    exact h
                  · exact error_ne_ok h
        | var a =>
            obtain ⟨b, args', hfl'⟩ := Ty.flatten_eq_var htt hfl
            rw [sizeOf_var hfl] at h
            rw [sizeOf_var hfl']
            exact h
        | nat _ | arrow _ _ | app _ _ =>
            exfalso
            rw [DEnv.sizeOf] at h
            split at h
            all_goals (rename_i heq; simp [hfl] at heq)
            all_goals try exact error_ne_ok h
      · intro vis vis' t t' c n hvis htt hhead h
        obtain ⟨tc, args, hfl, htc⟩ := hhead
        rw [DEnv.ctorWidth] at h ⊢
        split at h
        · rename_i sig hsig
          split at h
          rename_i targs tres hfa
          obtain ⟨sub, hsub, h⟩ := except_bind_eq_ok h
          obtain ⟨tc', vs, hres, hvs⟩ := sigShapeOk_inv (natOk_sig hΔ hsig)
          rw [hfa] at hres
          obtain ⟨sub', hsub', hse⟩ :=
            Ty.matchTy_congr tres hres hvs hfl htc htt hsub
          rw [hsub', except_bind_ok]
          obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
          rw [mapM_ok_of_pointwise
                (fun a b hab => ihS _ _ _ _ _ hvis (Ty.substTv_congr hse a) hab) hws,
              except_bind_ok]
          exact h
        · exact h

theorem sizeOf_eq {Δ : DEnv} (hΔ : natOk Δ = true) {k : Nat} {vis vis' : List Ty} {t t' : Ty}
    {n : Nat} (hvis : Ty.EqL vis vis') (htt : Ty.eq t t' = true)
    (h : Δ.sizeOf k vis t = .ok n) : Δ.sizeOf k vis' t' = .ok n :=
  (sizeOf_ctorWidth_eq Δ hΔ k).1 vis vis' t t' n hvis htt h

/-- The top-level instance (empty visited list). -/
theorem sizeOf_eq₀ {Δ : DEnv} (hΔ : natOk Δ = true) {k : Nat} {t t' : Ty} {n : Nat}
    (htt : Ty.eq t t' = true) (h : Δ.sizeOf k [] t = .ok n) : Δ.sizeOf k [] t' = .ok n :=
  sizeOf_eq hΔ Ty.Pointwise.nil htt h

/-- Constructor tags are invariant under `Ty.eq`, under the gate. -/
theorem ctorTag_eq {Δ : DEnv} (hΔ : natOk Δ = true) {t t' : Ty} {c : String} {r : Nat × Nat}
    (htt : Ty.eq t t' = true) (h : Δ.ctorTag t c = .ok r) : Δ.ctorTag t' c = .ok r := by
  rw [DEnv.ctorTag] at h ⊢
  split at h
  · rename_i tc args hfl
    by_cases ha : Ty.isArith tc = true
    · exfalso
      have hnt : Ty.isTupleCon tc = false := by
        cases ht : Ty.isTupleCon tc
        · rfl
        · rw [Ty.isTupleCon_not_arith ht] at ha; cases ha
      rw [if_neg (by simp [hnt]), natOk_arith hΔ ha] at h
      exact error_ne_ok h
    · have ha' : Ty.isArith tc = false := by simpa using ha
      obtain ⟨args', hfl', _⟩ := Ty.flatten_eq_con htt hfl ha'
      rw [hfl']
      exact h
  · exact error_ne_ok h

end DEnv

end Rwv.Eidos

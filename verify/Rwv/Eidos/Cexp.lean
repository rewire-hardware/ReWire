/-
The verified Eidos-side expression compiler (Phase 4a of the
translation-validation plan): `cexp` compiles the CORE machine-mode
pure fragment of Eidos (doc/eidos.md §7.5.2, §7.6) into the Hyle-side
normal-form language of the verified reflection bridge
(Rwv.Hyle.Bridge.NF), mirroring the reference lowering
(rewire-frontend ReWire.Eidos.ToHyle: transExp / caseChain / toPrim)
construct for construct:

  * variables (environment lookup by unique),
  * integer literals at `Integer` / `Finite n` / `Vec n Bool`,
  * vector literals (concatenation),
  * saturated constructor applications — tag | zero pad | fields,
    MSB-first, exactly `Val.rep`'s layout (ToHyle ctorTag/ctorRep),
  * `case` — the tag-slice if-chain of ToHyle.caseChain: a DataAlt
    tests the tag slice, a LitAlt compares the full atom, the default
    alternative (first, the Core convention) becomes the final else,
    and the last alternative is unconditional; field binders bind to
    slices at the wireOffsets positions,
  * non-recursive lets (substitution — normal forms have no binders),
  * saturated calls to pure definitions (inlined through the
    definition map, fuel-decremented, exactly as Bridge.symExp inlines
    Hyle calls),
  * the `toPrim` bit-vector rows of §7.6: Add Sub Mul Div Mod Pow,
    And Or XOr XNor Not, LShift RShift RShiftArith, Eq Gt GtEq Lt
    LtEq, LAnd LOr LNot (redor expansions), RAnd RNAnd ROr RNor RXOr
    RXNor, MSBit, plus Bits (the identity) and Resize (zext/trunc).

Phase 4b-i (the second half of this file) completes the fragment
with the FULL compiler `cexpJ`/`cexpFull`: join points and jumps in
pure bodies (a compile-time join environment mirroring the
evaluator's `JEnv`), ToHyle's commuting rewrites (lambda applied,
let-headed and case-headed applications — realized as a `pend`ing
list of already-compiled arguments, with definition calls consuming
a prefix and passing the remainder into the body, mirroring
`applyMany`), the remaining first-order §7.6 rows (the Finite
family, VecReplicate/Concat/Reverse/Slice/RSlice/Index/IndexProxy/
FromList, NatVal, BitSlice/BitIndex at syntactic Finite literals,
and live `error` as the checked zero value), and the HIGHER-ORDER
Vec rows (VecMap, VecGenerate — ToHyle's `applyFn`: unrolled per
element, the function argument applied through the same pending
discipline, so lambdas bind the element normal form and (possibly
partially applied) definition references consume it as their final
argument; soundness relates each element to the evaluator's
`applyAll` loop through the `applyMany` leg). `cexpJ_sound` /
`cexpFull_sound` extend the soundness statement with an `applyMany`
leg and a join-environment correspondence (`JEnvC`), and
`cexpJ_varsWF`/`cexpFull_varsWF` establish the bridge's `VarsWF`
discipline for compiled forms, upgrading `checkDefnPair`'s verified
verdict with the width-aware `cfoldW3` leg (guarded by `paramsOkW`).
Stage B (the η tier) closes the foreign rows in the FULL compiler:
`cexpJ`'s Cryptol arm inlines the spliced entry through the bridge at
Δ's syntactic extern table, gated extern-free on both the compiled
arguments and the inlined form (the spliced denotations `ForeignC`
pins live at the empty extern environment, and extern-free normal
forms cannot see the difference); a MODEL-LESS combinational extern
occurrence compiles to the uninterpreted-call node
`NF.xcall (sizeOf res) name (NF.xpack args)`, sound at EVERY extern
environment because both semantics read the same interpretation on
the same packed bits, with the evaluator's decode gate pinning the
result canonical (`decode_vty`/`decode_rep`) and `vty_rep_width`
making the width clamp inert. Every soundness statement is
∀-quantified over the ambient extern environment `E` (the section
variable below); the compilers are syntactic and never consult it.

Stage C closes three recorded residues:

  * MODEL-CARRYING extern occurrences compile exactly like the
    Cryptol row keyed by the syntactic extern table (`Δ.hyleX`
    designates the model definition; `ForeignC.xt` ties `Δ.xtF` to
    its denotation; `symExp` inlines model-carrying xcalls, so the
    device side reads the same splice) — sound by the Cryptol case's
    argument verbatim.
  * teq-STRICTNESS: the constructor-field, case-alternative-result,
    and pause-output checks compare with `teqN` — `teq`, or Vec/Finite
    spines with `evalNat`-equal nat positions at `ctorFree` heads —
    and `vty_teqN` transports canonicality across (see the teqN
    section header; nat arithmetic inside a datatype's own arguments
    remains outside the fragment and is rejected honestly).
  * FromFinite at ANY §7.6-legal width (n ≤ 2^m): `VTy.finite` now
    tracks `i < bound` (matching `Val.HasTy`), which makes the
    resize value-preserving on both the widening and narrowing side.
    The tracked bound is maintained by every producer — the Finite
    builtin rows all gate or mod by the bound; `rwPrimToFiniteMod` at
    a `Finite 0` RESULT type is now rejected at compile time (the
    committed semantics' unit-degenerate `.finite 0 0` inhabitant is
    the one value the bound excludes).

Still outside: strings, local-variable application, bare lambdas,
and generic extern instantiations.

THE soundness theorem (`cexp_sound`): a successful compilation is
rep-correspondent — if the committed evaluator produces a value, the
compiled normal form's denotation is that value's bit representation:

    cexp Δ dmap fuel Γ e = .ok (nf, ty) →
    evalCore ⟨Δ, dmap⟩ efuel env jenv e = .ok v →
    EnvC (E := E) Δ σ Γ env →
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ E)

Honest side conditions, stated exactly:

  * `EnvC` — environment correspondence: every binding recorded in the
    symbolic environment Γ is present in the concrete environment with
    a canonical (`VTy`-typed) value whose representation is the normal
    form's denotation; a unique ABSENT from Γ is absent from the
    concrete environment (the compiler resolves such names to global
    definitions, so a stray concrete binding would change the meaning).
    No width-annotation discipline is needed: the compiler is fully
    type-driven (`sizeOf` widths, exactly ToHyle's), so all width
    facts flow from `VTy` through `rep`.
  * `VTy Δ v t` — value canonicality: the value is a well-formed
    inhabitant of the (representable) type `t`, with constructor
    values carrying exactly `t` and fields canonical at the
    instantiated field types. This is the "Val.HasTy-style
    canonicality" the plan's §3.2 anticipated: the case-chain proof
    needs the scrutinee's dynamic constructor to live in the static
    discriminant type's constructor list (rep success alone does not
    pin the carried type of the scrutinee to the static type, and with
    a default alternative present a mistyped scrutinee could take
    different branches on the two sides). It is self-sustaining: the
    theorem PRODUCES it for every compiled expression, so it is only
    ever assumed of the initial environment (the per-defn validator
    assumes it of the definition's arguments).
  * `denvOk Δ` — the prim-basis `Bool` declaration is present and
    standard (the bit-level builtin rows produce `Bool`-typed values
    through the shared §7.5.1 bit readings, and their representation
    facts need Bool's two-constructor layout).
  * Both sides read the SAME definition map (the evaluator's context
    is `⟨Δ, dmap⟩`), and the eval-side join environment is arbitrary
    (the fragment contains no joins or jumps).

The fuel/rep-fuel bookkeeping is by existential (`∃ k, rep Δ k v =
.ok …`) — Rwv.Eidos.FuelMono makes success fuel-stable and
fuel-deterministic, so the existential form is canonical.

Per-defn validation (`checkDefnPair` + `checkDefnPair_sound`): for a
corresponding (Eidos pure defn, Hyle defn) pair, compile the Eidos
body with `cexp` over parameter variables named by the Hyle
signature, symbolically evaluate the Hyle body with `Bridge.symExp`
over the same variables, and compare after `Bridge.cfoldW3`
normalization. Soundness composes `cexp_sound` with the bridge's
`symExp_sound` and `cfoldW3_eval`: a `true` verdict means the Hyle
definition, applied to the representations of canonical arguments,
computes the representation of the Eidos definition's value. The
untrusted measurement driver is CexpValidate.lean (rwv-cexp-validate).

Per house style, small `Except`/list/HashMap helpers are re-proved
locally rather than exported from committed files.
-/
import Rwv.Eidos.Eval
import Rwv.Eidos.FuelMono
import Rwv.Hyle.Bridge
import Rwv.Hyle.BridgeDag
import Std.Data.HashMap

namespace Rwv.Eidos.Cexp

open Std (HashMap)
open Rwv.Hyle (BV Op)
open Rwv.Hyle.Bridge (NF annWidth WP)

/- Stage B: the ambient extern environment. Every soundness statement
below that mentions a denotation or an evaluator run is implicitly
∀-quantified over it (auto-bound); the compilers are syntactic and
never consult it. -/
variable {E : Rwv.Hyle.Sem.EEnv}

/-! ## Syntactic type equality

Structural equality on var-free types: the compiler compares types
where the soundness proof must transport canonicality, and syntactic
equality transports for free. Type variables are never equal (the
machine-mode fragment is monomorphic; a variable in a compared
position is a compile-time rejection, not a soundness question). -/

def teq : Ty → Ty → Bool
  | .con a, .con b => a == b
  | .app a b, .app c d => teq a c && teq b d
  | .nat m, .nat n => m == n
  | .arrow a b, .arrow c d => teq a c && teq b d
  | _, _ => false

theorem teq_eq : ∀ {t u : Ty}, teq t u = true → t = u
  | .con a, .con b, h => by
      simp only [teq, beq_iff_eq] at h
      rw [h]
  | .app a b, .app c d, h => by
      simp only [teq, Bool.and_eq_true] at h
      rw [teq_eq h.1, teq_eq h.2]
  | .nat m, .nat n, h => by
      simp only [teq, beq_iff_eq] at h
      rw [h]
  | .arrow a b, .arrow c d, h => by
      simp only [teq, Bool.and_eq_true] at h
      rw [teq_eq h.1, teq_eq h.2]
  | .con _, .app _ _, h | .con _, .var _, h | .con _, .nat _, h | .con _, .arrow _ _, h
  | .app _ _, .con _, h | .app _ _, .var _, h | .app _ _, .nat _, h | .app _ _, .arrow _ _, h
  | .var _, _, h
  | .nat _, .con _, h | .nat _, .app _ _, h | .nat _, .var _, h | .nat _, .arrow _ _, h
  | .arrow _ _, .con _, h | .arrow _ _, .app _ _, h | .arrow _ _, .var _, h
  | .arrow _ _, .nat _, h => by simp [teq] at h

/-- Pointwise `teq` of computed (NF, type) pairs against expected
types (same length required). -/
def teqAll : List (NF × Ty) → List Ty → Bool
  | [], [] => true
  | (_, t) :: ps, u :: us => teq t u && teqAll ps us
  | _, _ => false

theorem teqAll_length : ∀ {ps : List (NF × Ty)} {us : List Ty},
    teqAll ps us = true → ps.length = us.length
  | [], [], _ => rfl
  | (_, _) :: ps, u :: us, h => by
      simp only [teqAll, Bool.and_eq_true] at h
      simpa using teqAll_length (ps := ps) (us := us) h.2
  | [], _ :: _, h => by simp [teqAll] at h
  | _ :: _, [], h => by simp [teqAll] at h

theorem teqAll_types : ∀ {ps : List (NF × Ty)} {us : List Ty},
    teqAll ps us = true → ps.map (·.2) = us
  | [], [], _ => rfl
  | (_, t) :: ps, u :: us, h => by
      simp only [teqAll, Bool.and_eq_true] at h
      simp only [List.map_cons, teq_eq h.1, teqAll_types h.2]
  | [], _ :: _, h => by simp [teqAll] at h
  | _ :: _, [], h => by simp [teqAll] at h

/-! ## Transport-aware type comparison (the teq-strictness relaxation)

The reference compiler compares types with `Ty.eq` — syntactic
equality MODULO `natNorm` (type-level nat arithmetic evaluated) —
where this compiler's checks demand syntactic `teq`, so programs
whose GHC-derived occurrence types carry unevaluated arithmetic
(`Vec (+ 7 1) Bool` against `Vec 8 Bool`) are rejected at the
constructor-field, case-alternative-result, and pause-output checks.
`teqN` closes exactly the canonicality-TRANSPORTABLE fragment of that
gap: syntactic `teq`, or a Vec/Finite spine whose nat positions are
`evalNat`-equal (elementwise recursively for Vec), at a head the
datatype environment declares no constructors for (`ctorFree` — a
`VTy.con` derivation pins its type SYNTACTICALLY inside the value, so
transport at constructor-inhabited heads is unsound in general, while
vec/finite/integer/proxy values carry no types). `vty_teqN` is the
transport: canonicality at `t` moves to any `teqN`-equal `u`. Width
and representation facts ride along for free because the consumers
re-derive them from the TRANSPORTED canonicality (`vty_rep_width` at
the target type), so no `sizeOf`-invariance lemma is needed. Nat
positions still inside a datatype's arguments (a tuple field typed
`(,) (Vec (+ 7 1) Bool) a` against its normalized form) remain
outside the fragment and are still rejected honestly. -/

/-- Not tuple-shaped, and no declared constructors, for a
type-constructor name (checked at validation time so the transport
lemma never has to compute `isTupleCon` on a literal). -/
def ctorFree (Δ : DEnv) (tc : String) : Bool :=
  !Ty.isTupleCon tc &&
    match Δ.ctors.get? tc with
    | some (_ :: _) => false
    | _ => true

/-- Transport-aware comparison (see above): `teq`, or a Vec/Finite
spine with `evalNat`-equal nat positions at a constructor-free head. -/
def teqN (Δ : DEnv) : Ty → Ty → Bool
  | .app (.app tc₁ n₁) te₁, .app (.app tc₂ n₂) te₂ =>
      teq (.app (.app tc₁ n₁) te₁) (.app (.app tc₂ n₂) te₂)
        || (teq tc₁ (.con "Vec") && teq tc₂ (.con "Vec")
            && (Ty.evalNat n₁ == Ty.evalNat n₂) && (Ty.evalNat n₁).isSome
            && ctorFree Δ "Vec" && teqN Δ te₁ te₂)
  | .app tc₁ n₁, .app tc₂ n₂ =>
      teq (.app tc₁ n₁) (.app tc₂ n₂)
        || (teq tc₁ (.con "Finite") && teq tc₂ (.con "Finite")
            && (Ty.evalNat n₁ == Ty.evalNat n₂) && (Ty.evalNat n₁).isSome
            && ctorFree Δ "Finite")
  | t, u => teq t u

/-- The three ways a `teqN` verdict can arise, with the compared
types' syntactic shapes pinned. -/
theorem teqN_cases {Δ : DEnv} : ∀ {t u : Ty}, teqN Δ t u = true →
    teq t u = true
    ∨ (∃ n₁ te₁ n₂ te₂, t = .app (.app (.con "Vec") n₁) te₁
        ∧ u = .app (.app (.con "Vec") n₂) te₂
        ∧ Ty.evalNat n₁ = Ty.evalNat n₂
        ∧ ctorFree Δ "Vec" = true ∧ teqN Δ te₁ te₂ = true)
    ∨ (∃ n₁ n₂, t = .app (.con "Finite") n₁ ∧ u = .app (.con "Finite") n₂
        ∧ Ty.evalNat n₁ = Ty.evalNat n₂ ∧ ctorFree Δ "Finite" = true) := by
  intro t u h
  match t, u with
  | .app (.app tc₁ n₁) te₁, .app (.app tc₂ n₂) te₂ =>
      simp only [teqN, Bool.or_eq_true] at h
      rcases h with h | h
      · exact .inl h
      · simp only [Bool.and_eq_true] at h
        obtain ⟨⟨⟨⟨⟨h1, h2⟩, h3⟩, _⟩, h5⟩, h6⟩ := h
        obtain rfl := teq_eq h1
        obtain rfl := teq_eq h2
        exact .inr (.inl ⟨n₁, te₁, n₂, te₂, rfl, rfl, beq_iff_eq.mp h3, h5, h6⟩)
  | .app (.con c₁) n₁, .app (.con c₂) n₂ =>
      simp only [teqN, Bool.or_eq_true] at h
      rcases h with h | h
      · exact .inl h
      · simp only [Bool.and_eq_true] at h
        obtain ⟨⟨⟨⟨h1, h2⟩, h3⟩, _⟩, h5⟩ := h
        have h1' := teq_eq h1
        have h2' := teq_eq h2
        injection h1' with h1''
        injection h2' with h2''
        subst h1''; subst h2''
        exact .inr (.inr ⟨n₁, n₂, rfl, rfl, beq_iff_eq.mp h3, h5⟩)
  | .app (.con _) _, .app (.app _ _) _ | .app (.con _) _, .app (.var _) _
  | .app (.con _) _, .app (.nat _) _ | .app (.con _) _, .app (.arrow _ _) _ =>
      simp only [teqN, Bool.or_eq_true] at h
      rcases h with h | h
      · exact .inl h
      · exfalso
        simp only [Bool.and_eq_true] at h
        exact absurd h.1.1.1.2 (by simp [teq])
  | .app (.var _) _, .app _ _ | .app (.nat _) _, .app _ _
  | .app (.arrow _ _) _, .app _ _
  | .app (.app _ _) _, .app (.con _) _ | .app (.app _ _) _, .app (.var _) _
  | .app (.app _ _) _, .app (.nat _) _ | .app (.app _ _) _, .app (.arrow _ _) _ =>
      simp only [teqN, Bool.or_eq_true] at h
      rcases h with h | h
      · exact .inl h
      · exfalso
        simp only [Bool.and_eq_true] at h
        exact absurd h.1.1.1.1 (by simp [teq])
  | .app _ _, .con _ | .app _ _, .var _ | .app _ _, .nat _ | .app _ _, .arrow _ _
  | .con _, _ | .var _, _ | .nat _, _ | .arrow _ _, _ =>
      simp only [teqN] at h
      exact .inl h

/-- Pointwise `teqN` of computed (NF, type) pairs against expected
types. -/
def teqAllN (Δ : DEnv) : List (NF × Ty) → List Ty → Bool
  | [], [] => true
  | (_, t) :: ps, u :: us => teqN Δ t u && teqAllN Δ ps us
  | _, _ => false

theorem teqAllN_length {Δ : DEnv} : ∀ {ps : List (NF × Ty)} {us : List Ty},
    teqAllN Δ ps us = true → ps.length = us.length
  | [], [], _ => rfl
  | (_, _) :: ps, u :: us, h => by
      simp only [teqAllN, Bool.and_eq_true] at h
      simpa using teqAllN_length (ps := ps) (us := us) h.2
  | [], _ :: _, h => by simp [teqAllN] at h
  | _ :: _, [], h => by simp [teqAllN] at h

theorem teqAllN_get {Δ : DEnv} : ∀ {ps : List (NF × Ty)} {us : List Ty},
    teqAllN Δ ps us = true → ∀ i (h1 : i < ps.length) (h2 : i < us.length),
      teqN Δ ps[i].2 us[i] = true
  | (_, t) :: ps, u :: us, h, i, h1, h2 => by
      simp only [teqAllN, Bool.and_eq_true] at h
      cases i with
      | zero => exact h.1
      | succ j =>
          exact teqAllN_get h.2 j (by simpa using h1) (by simpa using h2)
  | [], _ :: _, h, _, _, _ => by simp [teqAllN] at h
  | _ :: _, [], h, _, _, _ => by simp [teqAllN] at h
  | [], [], _, _, h1, _ => by simp at h1

/-! Flatten inversions: a spine equation pins the syntax. -/

private theorem flatten_eq_nil : ∀ {t h₀ : Ty}, Ty.flatten t = (h₀, []) → t = h₀ := by
  intro t h₀ h
  cases t <;> simp only [Ty.flatten] at h
  case app t₁ t₂ =>
      rcases hf : Ty.flatten t₁ with ⟨h', args'⟩
      rw [hf] at h
      dsimp only at h
      injection h with _ h2
      exact absurd h2 (by simp)
  all_goals exact congrArg Prod.fst h

private theorem flatten_eq_one : ∀ {t h₀ a : Ty}, Ty.flatten t = (h₀, [a]) →
    t = .app h₀ a := by
  intro t h₀ a h
  cases t <;> simp only [Ty.flatten] at h
  case app t₁ t₂ =>
      rcases hf : Ty.flatten t₁ with ⟨h', args'⟩
      rw [hf] at h
      dsimp only at h
      injection h with h1 h2
      subst h1
      cases args' with
      | nil =>
          rw [List.nil_append] at h2
          injection h2 with h3 _
          subst h3
          rw [flatten_eq_nil hf]
      | cons x xs =>
          rw [List.cons_append] at h2
          injection h2 with _ h4
          exact absurd h4 (by simp)
  all_goals (exfalso; exact absurd (congrArg Prod.snd h) (by simp))

private theorem flatten_eq_two : ∀ {t h₀ a b : Ty}, Ty.flatten t = (h₀, [a, b]) →
    t = .app (.app h₀ a) b := by
  intro t h₀ a b h
  cases t <;> simp only [Ty.flatten] at h
  case app t₁ t₂ =>
      rcases hf : Ty.flatten t₁ with ⟨h', args'⟩
      rw [hf] at h
      dsimp only at h
      injection h with h1 h2
      subst h1
      cases args' with
      | nil =>
          rw [List.nil_append] at h2
          injection h2 with _ h3
          exact absurd h3 (by simp)
      | cons x xs =>
          rw [List.cons_append] at h2
          injection h2 with h3 h4
          subst h3
          cases xs with
          | nil =>
              rw [List.nil_append] at h4
              injection h4 with h5 _
              subst h5
              rw [flatten_eq_one hf]
          | cons y ys =>
              rw [List.cons_append] at h4
              injection h4 with _ h6
              exact absurd h6 (by simp)
  all_goals (exfalso; exact absurd (congrArg Prod.snd h) (by simp))

/-! ## The datatype-environment side condition

The bit-level builtin rows produce `Bool` values through the §7.5.1
bit readings (`Eval.bitsToVec`, `Eval.boolVal`); their representation
facts need the prim-basis `Bool` declaration to be present and
standard. -/

def boolT : Ty := .con "Bool"

def denvOk (Δ : DEnv) : Bool :=
  (Δ.ctors.get? "Bool" == some ["False", "True"])
    && (match Δ.ctorSig.get? "False" with
        | some s => s.tvs.isEmpty && teq s.ty boolT
        | none => false)
    && (match Δ.ctorSig.get? "True" with
        | some s => s.tvs.isEmpty && teq s.ty boolT
        | none => false)
    && !Ty.isTupleCon "Bool"
    && !Ty.isTupleCon "Vec"
    && (match Δ.ctors.get? "Vec" with | some (_ :: _) => false | _ => true)

/-! ## Value canonicality (the Val.HasTy-style side condition)

`VTy Δ v t`: `v` is a canonical inhabitant of the representable type
`t`. Constructor values carry exactly `t` (the evaluator builds them
with the occurrence's instantiated result type, which is the same
term the compiler computes), their constructor genuinely belongs to
`t`'s head datatype (`ctorOf` — membership in the head's constructor
list, or the head itself for the tagless tuple family), and their
fields are canonical at the instantiated field types (via the same
lenient `matchTy` the sizing functions use — determinism of `matchTy`
makes the compiler's and the predicate's substitutions coincide).
Finite values track `i < bound` (matching `Val.HasTy`, stage 4d) —
the fact that makes `rwPrimFromFinite`'s resize value-preserving at
every §7.6-legal width. -/

def ctorOf (Δ : DEnv) (t : Ty) (c : String) : Prop :=
  match Ty.flatten t with
  | (.con tc, _) =>
      if Ty.isTupleCon tc then c = tc
      else ∃ cs, Δ.ctors.get? tc = some cs ∧ c ∈ cs
  | _ => False

inductive VTy (Δ : DEnv) : Val → Ty → Prop where
  | vec {elems : List Val} {t n te : Ty} {k : Nat} :
      Ty.flatten t = (.con "Vec", [n, te]) →
      Ty.evalNat n = some k →
      elems.length = k →
      (∀ e ∈ elems, VTy Δ e te) →
      VTy Δ (.vec elems) t
  | integer {x : BitVec 128} {t : Ty} :
      Ty.flatten t = (.con "Integer", []) →
      VTy Δ (.integer x) t
  | finite {b i : Nat} {t n : Ty} :
      Ty.flatten t = (.con "Finite", [n]) →
      Ty.evalNat n = some b →
      i < b →
      VTy Δ (.finite b i) t
  | proxy {t : Ty} {h : Ty} {args : List Ty} :
      Ty.flatten t = (.con "Proxy", args) →
      h = .con "Proxy" →
      VTy Δ .proxy t
  | con {t : Ty} {c : String} {fields : List Val} {sig : Sig}
      {sub : HashMap TyVar Ty} :
      Δ.ctorSig.get? c = some sig →
      DEnv.matchTy (Ty.flattenArrow sig.ty).2 t = .ok sub →
      fields.length = (Ty.flattenArrow sig.ty).1.length →
      ctorOf Δ t c →
      (∀ p ∈ (Ty.flattenArrow sig.ty).1.zip fields,
        VTy Δ p.2 (DEnv.substTv sub p.1)) →
      VTy Δ (.con t c fields) t

/-- Canonicality transports across `teqN`-equal types (see the `teqN`
section header): the vec case recurses elementwise, finite re-pins
the evaluated bound, integer/proxy re-pin the shape, and a
constructor value forces the `teq` leg because `ctorFree` excludes
its head from the Vec/Finite rows. -/
theorem vty_teqN {Δ : DEnv} {v : Val} {t : Ty}
    (hv : VTy Δ v t) : ∀ {u : Ty}, teqN Δ t u = true → VTy Δ v u := by
  induction hv with
  | vec hfl hn hlen helem ih =>
      intro u h
      obtain rfl := flatten_eq_two hfl
      rcases teqN_cases h with hteq
        | ⟨n₁, te₁, n₂, te₂, ht, hu, hnn, hcf, hte⟩
        | ⟨n₁, n₂, ht, hu, hnn, hcf⟩
      · obtain rfl := teq_eq hteq
        exact VTy.vec hfl hn hlen helem
      · injection ht with ht1 ht2
        injection ht1 with ht3 ht4
        subst ht2; subst ht4; subst hu
        rw [hn] at hnn
        exact VTy.vec (n := n₂) (te := te₂) rfl hnn.symm hlen
          (fun e he => ih e he hte)
      · exact absurd ht (by simp)
  | integer hfl =>
      intro u h
      obtain rfl := flatten_eq_nil hfl
      rcases teqN_cases h with hteq
        | ⟨n₁, te₁, n₂, te₂, ht, hu, hnn, hcf, hte⟩
        | ⟨n₁, n₂, ht, hu, hnn, hcf⟩
      · obtain rfl := teq_eq hteq
        exact VTy.integer hfl
      · exact absurd ht (by simp)
      · exact absurd ht (by simp)
  | finite hfl hn hlt =>
      intro u h
      obtain rfl := flatten_eq_one hfl
      rcases teqN_cases h with hteq
        | ⟨n₁, te₁, n₂, te₂, ht, hu, hnn, hcf, hte⟩
        | ⟨n₁, n₂, ht, hu, hnn, hcf⟩
      · obtain rfl := teq_eq hteq
        exact VTy.finite hfl hn hlt
      · exact absurd ht (by simp)
      · injection ht with ht1 ht2
        subst ht2; subst hu
        rw [hn] at hnn
        exact VTy.finite (n := n₂) rfl hnn.symm hlt
  | proxy hfl hh =>
      intro u h
      rcases teqN_cases h with hteq
        | ⟨n₁, te₁, n₂, te₂, ht, hu, hnn, hcf, hte⟩
        | ⟨n₁, n₂, ht, hu, hnn, hcf⟩
      · obtain rfl := teq_eq hteq
        exact VTy.proxy hfl hh
      · rw [ht] at hfl
        simp only [Ty.flatten] at hfl
        exact absurd (congrArg Prod.fst hfl) (by simp)
      · rw [ht] at hfl
        simp only [Ty.flatten] at hfl
        exact absurd (congrArg Prod.fst hfl) (by simp)
  | con hsig hmatch hlen hctor hfields ih =>
      intro u h
      rcases teqN_cases h with hteq
        | ⟨n₁, te₁, n₂, te₂, ht, hu, hnn, hcf, hte⟩
        | ⟨n₁, n₂, ht, hu, hnn, hcf⟩
      · obtain rfl := teq_eq hteq
        exact VTy.con hsig hmatch hlen hctor hfields
      · subst ht
        simp only [ctorFree, Bool.and_eq_true, Bool.not_eq_eq_eq_not,
          Bool.not_true] at hcf
        obtain ⟨htup, hget⟩ := hcf
        simp only [ctorOf, Ty.flatten] at hctor
        rw [if_neg (by simp [htup])] at hctor
        obtain ⟨cs, hcs, hmem⟩ := hctor
        rw [hcs] at hget
        cases cs with
        | nil => exact absurd hmem (by simp)
        | cons c0 cs0 => exact absurd hget (by simp)
      · subst ht
        simp only [ctorFree, Bool.and_eq_true, Bool.not_eq_eq_eq_not,
          Bool.not_true] at hcf
        obtain ⟨htup, hget⟩ := hcf
        simp only [ctorOf, Ty.flatten] at hctor
        rw [if_neg (by simp [htup])] at hctor
        obtain ⟨cs, hcs, hmem⟩ := hctor
        rw [hcs] at hget
        cases cs with
        | nil => exact absurd hmem (by simp)
        | cons c0 cs0 => exact absurd hget (by simp)

/-! ## Normal-form construction helpers

`catNF` mirrors ToHyle's `A.cat` smart constructor: zero-width
components dropped (statically, by annotation), right-nested
concatenation, the zero-width literal when nothing is left. `sliceNF`
mirrors `slice0`. -/

def catList : List NF → NF
  | [] => .lit BV.nil
  | [x] => x
  | x :: xs => .cat x (catList xs)

/-- Concatenation of width-annotated pieces (the widths are the
TYPE-derived sizes, exactly ToHyle's `A.sizeOf`-driven `A.cat`):
zero-width pieces dropped, right-nested. -/
def catNF (xs : List (NF × Nat)) : NF :=
  catList ((xs.filter fun p => p.2 != 0).map (·.1))

def sliceNF (off w : Nat) (e : NF) : NF :=
  if w = 0 then .lit BV.nil else .slice off w e

/-! ## The builtin rows (ToHyle.toPrim and the Bits/Resize specials)

Fuel-free: all static data comes from the occurrence's instantiated
type (result widths from the result type) and from the compiled
arguments' annotated widths. Only the §7.6 rows named in the header
are accepted. -/

/-- The length of a `Vec n Bool` type (the element type must be
literally `Bool`). -/
def vecBoolLen (who : String) (t : Ty) : Except String Nat :=
  match Ty.flatten t with
  | (.con "Vec", [n, te]) =>
      if teq te boolT then
        match Ty.evalNat n with
        | some k => .ok k
        | none => .error s!"{who}: open Vec length"
      else .error s!"{who}: non-Bool Vec element type"
  | _ => .error s!"{who}: expected a Vec type"

def isBoolT (t : Ty) : Bool := teq t boolT

/-- A binary arithmetic/bitwise/shift row (`Vec n Bool → Vec n Bool →
Vec n Bool`, denotation "as hyle op"): the first operand's type width
must be the result type's width. -/
def arithRow (op : Op) (res ta : Ty) (a b : NF) : Except String (NF × Ty) := do
  let m ← vecBoolLen "arith row" res
  let wa ← vecBoolLen "arith row" ta
  if wa = m then pure (.prim2 op a b, res)
  else .error "cexp: arith row width mismatch"

/-- A comparison row (`… → Bool`). -/
def cmpRow (op : Op) (res : Ty) (a b : NF) : Except String (NF × Ty) :=
  if isBoolT res then pure (.prim2 op a b, res)
  else .error "cexp: comparison row at a non-Bool result type"

/-- A reduction row (`Vec n Bool → Bool`), optionally negated. -/
def redRow (op : Op) (negated : Bool) (res : Ty) (a : NF) : Except String (NF × Ty) :=
  if isBoolT res then
    pure (if negated then (.prim1 .not (.prim1 op a), res) else (.prim1 op a, res))
  else .error "cexp: reduction row at a non-Bool result type"

/-- The row table: given the occurrence's instantiated type and the
compiled arguments (with their synthesized types), the compiled
application. -/
def cprim (pty : Ty) (b : Builtin) (pas : List (NF × Ty)) : Except String (NF × Ty) :=
  let res := (Ty.flattenArrow pty).2
  match b, pas with
  | .bits, [(a, _)] => do
      let k ← vecBoolLen "rwPrimBits" res
      if k = 128 then pure (a, res) else .error "rwPrimBits: result is not Vec 128 Bool"
  | .resize, [(a, ta)] => do
      let m ← vecBoolLen "rwPrimResize" res
      let wa ← vecBoolLen "rwPrimResize" ta
      if m = wa then pure (a, res)
      else if wa < m then pure (.prim1 (.zext m) a, res)
      else pure (.prim1 (.trunc m) a, res)
  | .add, [(a, ta), (b', _)] => arithRow .add  res ta a b'
  | .sub, [(a, ta), (b', _)] => arithRow .sub  res ta a b'
  | .mul, [(a, ta), (b', _)] => arithRow .mul  res ta a b'
  | .div, [(a, ta), (b', _)] => arithRow .udiv res ta a b'
  | .mod, [(a, ta), (b', _)] => arithRow .umod res ta a b'
  | .pow, [(a, ta), (b', _)] => arithRow .pow  res ta a b'
  | .and, [(a, ta), (b', _)] => arithRow .and  res ta a b'
  | .or,  [(a, ta), (b', _)] => arithRow .or   res ta a b'
  | .xor, [(a, ta), (b', _)] => arithRow .xor  res ta a b'
  | .xnor, [(a, ta), (b', _)] => do
      let m ← vecBoolLen "rwPrimXNor" res
      let wa ← vecBoolLen "rwPrimXNor" ta
      if wa = m then pure (.prim1 .not (.prim2 .xor a b'), res)
      else .error "rwPrimXNor: width mismatch"
  | .not, [(a, ta)] => do
      let m ← vecBoolLen "rwPrimNot" res
      let wa ← vecBoolLen "rwPrimNot" ta
      if wa = m then pure (.prim1 .not a, res)
      else .error "rwPrimNot: width mismatch"
  | .lShift,      [(a, ta), (b', _)] => arithRow .shl  res ta a b'
  | .rShift,      [(a, ta), (b', _)] => arithRow .lshr res ta a b'
  | .rShiftArith, [(a, ta), (b', _)] => arithRow .ashr res ta a b'
  | .eq,   [(a, _), (b', _)] => cmpRow .eq  res a b'
  | .gt,   [(a, _), (b', _)] => cmpRow .ugt res a b'
  | .gtEq, [(a, _), (b', _)] => cmpRow .uge res a b'
  | .lt,   [(a, _), (b', _)] => cmpRow .ult res a b'
  | .ltEq, [(a, _), (b', _)] => cmpRow .ule res a b'
  | .lAnd, [(a, _), (b', _)] =>
      cmpRow .and res (.prim1 .redor a) (.prim1 .redor b')
  | .lOr, [(a, _), (b', _)] =>
      cmpRow .or res (.prim1 .redor a) (.prim1 .redor b')
  | .lNot, [(a, _)] => redRow .redor true res a
  | .rAnd,  [(a, _)] => redRow .redand false res a
  | .rNAnd, [(a, _)] => redRow .redand true  res a
  | .rOr,   [(a, _)] => redRow .redor  false res a
  | .rNor,  [(a, _)] => redRow .redor  true  res a
  | .rXOr,  [(a, _)] => redRow .redxor false res a
  | .rXNor, [(a, _)] => redRow .redxor true  res a
  | .msBit, [(a, ta)] => do
      if isBoolT res then do
        let wa ← vecBoolLen "rwPrimMSBit" ta
        if wa ≥ 1 then pure (.slice (wa - 1) 1 a, res)
        else .error "rwPrimMSBit: zero-width argument"
      else .error "rwPrimMSBit: non-Bool result type"
  | _, _ => .error s!"cexp: unsupported builtin {b.name} (outside the Phase 4a fragment)"

/-! ## The compiler -/

/-- Bind binders over Γ, first-wins (matching `List.lookup`'s
first-wins semantics on the evaluator side, duplicate uniques
included). -/
def bindFieldsΓ (xs : List Id) (nts : List (NF × Ty)) (Γ : HashMap Int (NF × Ty)) :
    HashMap Int (NF × Ty) :=
  (xs.zip nts).foldr (fun (x, nt) m => m.insert x.uniq nt) Γ

/-- The first-wins association map of a parameter/pair zip. -/
def mkGamma (params : List Id) (pas : List (NF × Ty)) : HashMap Int (NF × Ty) :=
  bindFieldsΓ params pas ∅

/-- An integer literal at its carried type (ToHyle: `bitVec sz n` at
the type's width), restricted to the three §7.5.1 bit-reading types. -/
def clitInt (ty : Ty) (n : Int) : Except String (NF × Ty) :=
  match Ty.flatten ty with
  | (.con "Integer", []) => .ok (.lit ⟨128, BitVec.ofInt 128 n⟩, ty)
  | (.con "Finite", [bt]) =>
      match Ty.evalNat bt with
      | some k => .ok (.lit ⟨nbits k, BitVec.ofInt (nbits k) n⟩, ty)
      | none => .error "cexp: integer literal at an open Finite bound"
  | (.con "Vec", [lt, et]) =>
      if teq et boolT then
        match Ty.evalNat lt with
        | some w => .ok (.lit ⟨w, BitVec.ofInt w n⟩, ty)
        | none => .error "cexp: integer literal at an open Vec length"
      else .error "cexp: integer literal at a non-Bool Vec type"
  | _ => .error "cexp: integer literal at an unsupported type"

/-- The membership check backing a DataAlt (and the `con` case's
`ctorOf` obligation): the constructor belongs to the discriminant
type's head datatype — the head itself for the tagless tuple family. -/
def ctorOfB (Δ : DEnv) (t : Ty) (c : String) : Bool :=
  match Ty.flatten t with
  | (.con tc, _) =>
      if Ty.isTupleCon tc then c == tc
      else match Δ.ctors.get? tc with
        | some cs => cs.contains c
        | none => false
  | _ => false

mutual

/-- One case alternative, compiled (ToHyle.caseChain's `altExp`): a
DataAlt tests the tag slice and binds field slices at the wireOffsets
positions; a LitAlt compares the full atom; the accumulated `macc` is
the else-branch (`none` for the unconditional last alternative). -/
def cAlt (Δ : DEnv) (dmap : HashMap Int Defn) (fuel : Nat) (Γ' : HashMap Int (NF × Ty))
    (dty : Ty) (szT : Nat) (dn : NF) (resTy : Ty) : Alt → Option NF → Except String NF
  | .mk .default _ _, _ => .error "cexp: default alternative not first"
  | .mk (.dataAlt cn) xs body, macc => do
      if ctorOfB Δ dty cn then do
        let (tag, w) ← Δ.ctorTag dty cn
        match Δ.ctorSig.get? cn with
        | none => .error s!"cexp: unknown constructor {cn}"
        | some sig => do
            let sub ← DEnv.matchTy (Ty.flattenArrow sig.ty).2 dty
            let instTys := (Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)
            if xs.length = instTys.length then do
              let szXs ← instTys.mapM (Δ.sizeOf (fuel + 1) [])
              if w + szXs.sum ≤ szT then do
                -- wireOffsets: field i sits at LSB offset (sum of the
                -- sizes after it).
                let offs := (List.range szXs.length).map fun i =>
                  (szXs.drop (i + 1)).sum
                let slices := (szXs.zip offs).map fun (sz, off) =>
                  (sliceNF off sz dn)
                let Γ'' := bindFieldsΓ xs (slices.zip instTys) Γ'
                let (bnf, bty) ← cexp Δ dmap fuel Γ'' body
                if teq bty resTy then
                  match macc, w with
                  | some acc, _ + 1 =>
                      .ok (.ite (.prim2 .eq (sliceNF (szT - w) w dn)
                                            (.lit ⟨w, BitVec.ofNat w tag⟩))
                                bnf acc)
                  | _, _ => .ok bnf
                else .error "cexp: case alternative result-type mismatch"
              else .error s!"cexp: constructor {cn} wider than the discriminant"
            else .error s!"cexp: constructor {cn} binder arity mismatch"
      else .error s!"cexp: constructor {cn} does not belong to the discriminant type"
  | .mk (.litAlt i) _ body, macc => do
      let (bnf, bty) ← cexp Δ dmap fuel Γ' body
      if teq bty resTy then
        match macc with
        | some acc =>
            .ok (.ite (.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT i⟩)) bnf acc)
        | none => .ok bnf
      else .error "cexp: case alternative result-type mismatch"
termination_by alt _ => (fuel, 1, 0)

/-- The if-chain (ToHyle.caseChain's `go`): right fold with the
default (when present) as the initial else; without one the last
alternative is unconditional. -/
def cchain (Δ : DEnv) (dmap : HashMap Int Defn) (fuel : Nat) (Γ' : HashMap Int (NF × Ty))
    (dty : Ty) (szT : Nat) (dn : NF) (resTy : Ty) :
    List Alt → Option NF → Except String (NF × Ty)
  | [], some els => .ok (els, resTy)
  | [], none => .error "cexp: empty case"
  | [alt], none => do
      let bnf ← cAlt Δ dmap fuel Γ' dty szT dn resTy alt none
      .ok (bnf, resTy)
  | alt :: rest, macc => do
      let (accnf, _) ← cchain Δ dmap fuel Γ' dty szT dn resTy rest macc
      let bnf ← cAlt Δ dmap fuel Γ' dty szT dn resTy alt (some accnf)
      .ok (bnf, resTy)
termination_by rest _ => (fuel, 2, rest.length)

/-- `cexp Δ dmap fuel Γ e`: compile the core fragment to a Hyle-side
normal form and its (synthesized) type. Every recursive call consumes
fuel, so exhausted fuel is a compile-time rejection, never an
unsoundness. -/
def cexp (Δ : DEnv) (dmap : HashMap Int Defn) :
    Nat → HashMap Int (NF × Ty) → Exp → Except String (NF × Ty)
  | 0, _, _ => .error "cexp: out of fuel"
  | fuel + 1, Γ, e =>
    match Eval.flattenApp e with
    | (.var x, args) =>
        match Γ.get? x.uniq with
        | some nt =>
            match args with
            | [] => .ok nt
            | _ :: _ => .error s!"cexp: unsupported application of a local variable: {x.occ}"
        | none =>
            match dmap.get? x.uniq with
            | some d => do
                let pas ← args.mapM (cexp Δ dmap fuel Γ)
                if teqAll pas (d.params.map (·.sig.ty)) then
                  cexp Δ dmap fuel (mkGamma d.params pas) d.body
                else .error s!"cexp: call to {x.occ}: arity or argument-type mismatch"
            | none => .error s!"cexp: unknown definition {x.occ}#{x.uniq}"
    | (.con cty c, args) => do
        let pas ← args.mapM (cexp Δ dmap fuel Γ)
        let resTy := (Ty.flattenArrow cty).2
        if pas.length = (Ty.flattenArrow cty).1.length then
          match Δ.ctorSig.get? c with
          | some sig => do
              let sub ← DEnv.matchTy (Ty.flattenArrow sig.ty).2 resTy
              if teqAll pas ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)) then
                if ctorOfB Δ resTy c then do
                  let whole ← Δ.sizeOf (fuel + 1) [] resTy
                  let (tag, w) ← Δ.ctorTag resTy c
                  let ws ← ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)).mapM
                    (Δ.sizeOf (fuel + 1) [])
                  if w + ws.sum ≤ whole then
                    .ok (catNF ((.lit ⟨w, BitVec.ofNat w tag⟩, w)
                                :: (.lit ⟨whole - w - ws.sum, 0⟩, whole - w - ws.sum)
                                :: (pas.map (·.1)).zip ws), resTy)
                  else .error s!"cexp: constructor {c} wider than its type"
                else .error s!"cexp: constructor {c} does not belong to its result type"
              else .error s!"cexp: constructor {c}: field-type mismatch"
          | none => .error s!"cexp: unknown constructor {c}"
        else .error s!"cexp: unsaturated constructor {c}"
    | (.prim pty b, args) => do
        let pas ← args.mapM (cexp Δ dmap fuel Γ)
        cprim pty b pas
    | (.litInt ty n, []) => clitInt ty n
    | (.litVec vty es, []) => do
        let pas ← es.mapM (cexp Δ dmap fuel Γ)
        match Ty.flatten vty with
        | (.con "Vec", [nt, te]) =>
            match Ty.evalNat nt with
            | some k =>
                if pas.length = k then
                  if teqAll pas (List.replicate pas.length te) then do
                    let se ← Δ.sizeOf (fuel + 1) [] te
                    .ok (catNF ((pas.map (·.1)).map (·, se)), vty)
                  else .error "cexp: vector literal element-type mismatch"
                else .error "cexp: vector literal length mismatch"
            | none => .error "cexp: vector literal at an open Vec length"
        | _ => .error "cexp: vector literal at a non-Vec type"
    | (.letE (.nonRec x rhs) body, []) => do
        let nt ← cexp Δ dmap fuel Γ rhs
        cexp Δ dmap fuel (Γ.insert x.uniq nt) body
    | (.cases resTy scrut binder alts, []) => do
        let (dn, dty) ← cexp Δ dmap fuel Γ scrut
        let szT ← Δ.sizeOf (fuel + 1) [] dty
        let Γ' := Γ.insert binder.uniq (dn, dty)
        match alts with
        | .mk .default _ dbody :: rest => do
            let (dnf, dbt) ← cexp Δ dmap fuel Γ' dbody
            if teq dbt resTy then cchain Δ dmap fuel Γ' dty szT dn resTy rest (some dnf)
            else .error "cexp: default alternative result-type mismatch"
        | rest => cchain Δ dmap fuel Γ' dty szT dn resTy rest none
    | (_, _) => .error "cexp: unsupported expression (outside the Phase 4a fragment)"
termination_by fuel _ _ => (fuel, 0, 0)

end

/-! ## Local `Except`/list/HashMap helpers (house style: re-proved) -/

private theorem except_pure_def {α : Type} (a : α) :
    (pure a : Except String α) = .ok a := rfl

private theorem except_bind_ok {α β : Type} (a : α) (f : α → Except String β) :
    (Except.ok a >>= f) = f a := rfl

private theorem error_ne_ok {α : Type} {msg : String} {a : α} {P : Prop}
    (h : (Except.error msg : Except String α) = .ok a) : P := by
  cases h

private theorem except_bind_eq_ok {α β : Type} {x : Except String α}
    {f : α → Except String β} {b : β} (h : (x >>= f) = .ok b) :
    ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x with
  | error e => exact error_ne_ok h
  | ok a => exact ⟨a, rfl, h⟩

/-- Inversion of a successful `mapM`: pointwise successes, aligned by
index. -/
private theorem mapM_ok_idx {α β : Type} {g : α → Except String β} :
    ∀ {xs : List α} {ys : List β}, xs.mapM g = .ok ys →
      ys.length = xs.length ∧
      ∀ i (hi : i < xs.length), ∃ (hy : i < ys.length), g xs[i] = .ok ys[i] := by
  intro xs
  induction xs with
  | nil =>
      intro ys h
      rw [List.mapM_nil, except_pure_def] at h
      injection h with h
      subst h
      exact ⟨rfl, fun i hi => absurd hi (by simp)⟩
  | cons a as ih =>
      intro ys h
      rw [List.mapM_cons] at h
      obtain ⟨b, hb, h⟩ := except_bind_eq_ok h
      obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
      rw [except_pure_def] at h
      injection h with h
      subst h
      obtain ⟨hlen, hpt⟩ := ih hbs
      refine ⟨by simp [hlen], ?_⟩
      intro i hi
      match i with
      | 0 => exact ⟨by simp, by simpa using hb⟩
      | i + 1 =>
          obtain ⟨hy, hgi⟩ := hpt i (by simpa using hi)
          exact ⟨by simpa using hy, by simpa using hgi⟩

/-- Inversion of a successful `evalList`: pointwise `evalCore`
successes at some fuels, aligned by index. -/
private theorem evalList_ok_idx {C : Eval.Ctx} :
    ∀ {ef : Nat} {env : Eval.Env} {jenv : Eval.JEnv} {es : List Exp} {vs : List Val},
      Eval.evalList C ef env jenv es E = .ok vs →
      vs.length = es.length ∧
      ∀ i (hi : i < es.length), ∃ (hv : i < vs.length), ∃ k,
        Eval.evalCore C k env jenv es[i] E = .ok vs[i] := by
  intro ef
  induction ef with
  | zero =>
      intro env jenv es vs h
      rw [Eval.evalList] at h
      exact error_ne_ok h
  | succ ef ih =>
      intro env jenv es vs h
      cases es with
      | nil =>
          rw [Eval.evalList] at h
          injection h with h
          subst h
          exact ⟨rfl, fun i hi => absurd hi (by simp)⟩
      | cons e rest =>
          rw [Eval.evalList] at h
          obtain ⟨v, hv, h⟩ := except_bind_eq_ok h
          obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
          rw [except_pure_def] at h
          injection h with h
          subst h
          obtain ⟨hlen, hpt⟩ := ih hws
          refine ⟨by simp [hlen], ?_⟩
          intro i hi
          match i with
          | 0 => exact ⟨by simp, ef, by simpa using hv⟩
          | i + 1 =>
              obtain ⟨hw, k, hk⟩ := hpt i (by simpa using hi)
              exact ⟨by simpa using hw, k, by simpa using hk⟩

/-- `List.lookup` through a cons, by key comparison. -/
private theorem lookup_cons {β : Type} {k k' : Int} {v : β} {l : List (Int × β)} :
    List.lookup k ((k', v) :: l) = if k = k' then some v else List.lookup k l := by
  simp only [List.lookup]
  by_cases h : k = k'
  · simp [h]
  · simp [beq_eq_false_iff_ne.mpr h, h]

private theorem get?_insert {β : Type} {m : HashMap Int β} {k k' : Int} {v : β} :
    (m.insert k v).get? k' = if k' = k then some v else m.get? k' := by
  rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert, HashMap.get?_eq_getElem?]
  by_cases h : k' = k
  · simp [h]
  · simp [h, Ne.symm h]

/-! ## The BV concatenation/slice kit

Everything at projection level (widths and `getLsbD`), per the
bridge's recorded proof-engineering discipline: `BV` bundles the
width, so associativity and unit laws are genuine equalities. -/

private theorem bv_ext {x y : BV} (hw : x.width = y.width)
    (h : ∀ i, x.bits.getLsbD i = y.bits.getLsbD i) : x = y := by
  cases x with | mk wx bx =>
  cases y with | mk wy bv =>
  dsimp only at hw h
  subst hw
  exact congrArg (BV.mk wx) (BitVec.eq_of_getLsbD_eq fun i _ => h i)

/-- The bundled concatenation (left operand at the MSB end). -/
def bvCat (a b : BV) : BV := ⟨a.width + b.width, a.bits ++ b.bits⟩

private theorem bvCat_getLsbD (a b : BV) (i : Nat) :
    (bvCat a b).bits.getLsbD i =
      if i < b.width then b.bits.getLsbD i else a.bits.getLsbD (i - b.width) := by
  simp [bvCat, BitVec.getLsbD_append]

private theorem getLsbD_ge {n : Nat} (x : BitVec n) {i : Nat} (h : n ≤ i) :
    x.getLsbD i = false :=
  x.getLsbD_of_ge i h

private theorem bvCat_width (a b : BV) : (bvCat a b).width = a.width + b.width := rfl

private theorem bvCat_zero_left {a b : BV} (h : a.width = 0) : bvCat a b = b := by
  refine bv_ext (by rw [bvCat_width, h, Nat.zero_add]) ?_
  intro i
  rw [bvCat_getLsbD]
  by_cases hi : i < b.width
  · rw [if_pos hi]
  · rw [if_neg hi, getLsbD_ge a.bits (by omega), getLsbD_ge b.bits (by omega)]

private theorem bvCat_zero_right {a b : BV} (h : b.width = 0) : bvCat a b = a := by
  refine bv_ext (by rw [bvCat_width, h, Nat.add_zero]) ?_
  intro i
  rw [bvCat_getLsbD, if_neg (by omega), h, Nat.sub_zero]

private theorem bvCat_assoc (a b c : BV) : bvCat (bvCat a b) c = bvCat a (bvCat b c) := by
  refine bv_ext (by simp only [bvCat_width]; omega) ?_
  intro i
  rw [bvCat_getLsbD (bvCat a b) c i, bvCat_getLsbD a (bvCat b c) i]
  rcases Nat.lt_or_ge i c.width with hc | hc
  · rw [if_pos hc, if_pos (show i < (bvCat b c).width by rw [bvCat_width]; omega),
        bvCat_getLsbD b c i, if_pos hc]
  · rcases Nat.lt_or_ge (i - c.width) b.width with hb | hb
    · rw [if_neg (by omega), bvCat_getLsbD a b (i - c.width), if_pos hb,
          if_pos (show i < (bvCat b c).width by rw [bvCat_width]; omega),
          bvCat_getLsbD b c i, if_neg (by omega)]
    · rw [if_neg (by omega), bvCat_getLsbD a b (i - c.width), if_neg (by omega),
          if_neg (show ¬ i < (bvCat b c).width by rw [bvCat_width]; omega), bvCat_width]
      congr 1
      omega

/-- `Val.bvConcat` as a fold of the bundled concatenation. -/
private def catAll (xs : List BV) : BV := xs.foldl bvCat BV.nil

private theorem bvConcat_eq (xs : List BV) : Val.bvConcat xs = catAll xs := rfl

private theorem foldl_bvCat (l : List BV) :
    ∀ acc, l.foldl bvCat acc = bvCat acc (catAll l) := by
  induction l with
  | nil => intro acc; exact (bvCat_zero_right rfl).symm
  | cons y ys ih =>
      intro acc
      rw [List.foldl_cons, ih (bvCat acc y), bvCat_assoc]
      congr 1
      simp only [catAll, List.foldl_cons]
      rw [show bvCat BV.nil y = y from bvCat_zero_left rfl, ih y]
      simp only [catAll]

private theorem catAll_cons (x : BV) (xs : List BV) :
    catAll (x :: xs) = bvCat x (catAll xs) := by
  simp only [catAll, List.foldl_cons]
  rw [show bvCat BV.nil x = x from bvCat_zero_left rfl]
  exact foldl_bvCat xs x

private theorem catAll_nil : catAll [] = BV.nil := rfl

private theorem catAll_append (xs ys : List BV) :
    catAll (xs ++ ys) = bvCat (catAll xs) (catAll ys) := by
  induction xs with
  | nil => rw [List.nil_append, catAll_nil, bvCat_zero_left rfl]
  | cons x xs ih =>
      rw [List.cons_append, catAll_cons, ih, catAll_cons, bvCat_assoc]

private theorem catAll_width (xs : List BV) :
    (catAll xs).width = (xs.map (·.width)).sum := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
      rw [catAll_cons, bvCat_width, ih, List.map_cons, List.sum_cons]

/-- The bundled slice (LSB offset, width). -/
def sliceBV (x : BV) (i w : Nat) : BV := ⟨w, x.bits.extractLsb' i w⟩

private theorem sliceBV_getLsbD (x : BV) (i w j : Nat) :
    (sliceBV x i w).bits.getLsbD j = (decide (j < w) && x.bits.getLsbD (i + j)) := by
  simp [sliceBV, BitVec.getLsbD_extractLsb']

private theorem sliceBV_cat_low {a b : BV} {i w : Nat} (h : i + w ≤ b.width) :
    sliceBV (bvCat a b) i w = sliceBV b i w := by
  refine bv_ext rfl ?_
  intro j
  rw [sliceBV_getLsbD, sliceBV_getLsbD]
  by_cases hj : j < w
  · simp only [decide_eq_true hj, Bool.true_and]
    rw [bvCat_getLsbD, if_pos (by omega)]
  · simp [hj]

private theorem sliceBV_cat_high {a b : BV} {i w : Nat} (h : b.width ≤ i) :
    sliceBV (bvCat a b) i w = sliceBV a (i - b.width) w := by
  refine bv_ext rfl ?_
  intro j
  rw [sliceBV_getLsbD, sliceBV_getLsbD]
  by_cases hj : j < w
  · simp only [decide_eq_true hj, Bool.true_and]
    rw [bvCat_getLsbD, if_neg (by omega)]
    congr 1
    omega
  · simp [hj]

private theorem sliceBV_all (x : BV) : sliceBV x 0 x.width = x := by
  refine bv_ext rfl ?_
  intro j
  rw [sliceBV_getLsbD]
  by_cases hj : j < x.width
  · simp [hj]
  · simp only [decide_eq_false (by omega : ¬ j < x.width), Bool.false_and]
    rw [getLsbD_ge x.bits (by omega)]

/-- The extraction workhorse: slicing a concatenation at a piece's
position yields the piece. -/
private theorem catAll_extract (pre post : List BV) (x : BV) :
    sliceBV (catAll (pre ++ x :: post)) ((post.map (·.width)).sum) x.width = x := by
  rw [catAll_append, catAll_cons]
  rw [sliceBV_cat_low (by rw [bvCat_width, catAll_width]; omega)]
  rw [sliceBV_cat_high (Nat.le_of_eq (catAll_width post))]
  rw [catAll_width, Nat.sub_self]
  exact sliceBV_all x

private theorem bvConcat_nil : Val.bvConcat [] = BV.nil := rfl

private theorem bvConcat3 (a b c : BV) : Val.bvConcat [a, b, c] = bvCat a (bvCat b c) := by
  rw [bvConcat_eq, catAll_cons, catAll_cons, catAll_cons, catAll_nil,
      show bvCat c BV.nil = c from bvCat_zero_right rfl]

/-! ## More traversal helpers -/

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

private theorem mapM_ok_of_forall {α β : Type} {g : α → Except String β} {f : α → β} :
    ∀ {xs : List α}, (∀ a ∈ xs, g a = .ok (f a)) → xs.mapM g = .ok (xs.map f) := by
  intro xs
  induction xs with
  | nil => intro _; rfl
  | cons a as ih =>
      intro h
      rw [List.mapM_cons, h a List.mem_cons_self, except_bind_ok,
          ih (fun a ha => h a (List.mem_cons_of_mem _ ha)), except_bind_ok,
          except_pure_def, List.map_cons]

private theorem sum_const {c : Nat} : ∀ {l : List Nat}, (∀ a ∈ l, a = c) → l.sum = l.length * c := by
  intro l
  induction l with
  | nil => intro _; simp
  | cons a as ih =>
      intro h
      rw [List.sum_cons, h a List.mem_cons_self, ih (fun a ha => h a (List.mem_cons_of_mem _ ha)),
          List.length_cons, Nat.succ_mul]
      omega

/-! ## Fuel determinism (from Rwv.Eidos.FuelMono's monotonicity) -/

private theorem sizeOf_det {Δ : DEnv} {k k' : Nat} {vis : List Ty} {t : Ty} {a b : Nat}
    (h : Δ.sizeOf k vis t = .ok a) (h' : Δ.sizeOf k' vis t = .ok b) : a = b :=
  Except.ok.inj ((Δ.sizeOf_mono (Nat.le_max_left k k') h).symm.trans
    (Δ.sizeOf_mono (Nat.le_max_right k k') h'))

private theorem rep_det {Δ : DEnv} {k k' : Nat} {v : Val} {a b : BV}
    (h : Val.rep Δ k v = .ok a) (h' : Val.rep Δ k' v = .ok b) : a = b :=
  Except.ok.inj ((Val.rep_mono Δ (Nat.le_max_left k k') h).symm.trans
    (Val.rep_mono Δ (Nat.le_max_right k k') h'))

/-! ## `Bool` facts from `denvOk` -/

private theorem denvOk_ctors {Δ : DEnv} (h : denvOk Δ = true) :
    Δ.ctors.get? "Bool" = some ["False", "True"] := by
  simp only [denvOk, Bool.and_eq_true, beq_iff_eq] at h
  exact h.1.1.1.1.1

private theorem bool_not_tuple {Δ : DEnv} (h : denvOk Δ = true) :
    Ty.isTupleCon "Bool" = false := by
  simp only [denvOk, Bool.and_eq_true, Bool.not_eq_eq_eq_not, Bool.not_true] at h
  exact h.1.1.2

private theorem vec_not_tuple {Δ : DEnv} (h : denvOk Δ = true) :
    Ty.isTupleCon "Vec" = false := by
  simp only [denvOk, Bool.and_eq_true, Bool.not_eq_eq_eq_not, Bool.not_true] at h
  exact h.1.2

private theorem vec_abstract {Δ : DEnv} (h : denvOk Δ = true) {cs : List String}
    (hcs : Δ.ctors.get? "Vec" = some cs) : cs = [] := by
  simp only [denvOk, Bool.and_eq_true] at h
  have h2 := h.2
  rw [hcs] at h2
  cases cs with
  | nil => rfl
  | cons a rest => exact absurd h2 (by simp)

private theorem denvOk_sig {Δ : DEnv} (h : denvOk Δ = true) {c : String}
    (hc : c = "False" ∨ c = "True") : Δ.ctorSig.get? c = some ⟨[], boolT⟩ := by
  simp only [denvOk, Bool.and_eq_true] at h
  rcases hc with hc | hc <;> subst hc
  · have hf := h.1.1.1.1.2
    split at hf
    · rename_i s hs
      simp only [Bool.and_eq_true, List.isEmpty_iff] at hf
      cases s with
      | mk tvs ty =>
          dsimp only at hf
          rw [hs, hf.1, teq_eq hf.2]
    · exact absurd hf (by simp)
  · have ht := h.1.1.1.2
    split at ht
    · rename_i s hs
      simp only [Bool.and_eq_true, List.isEmpty_iff] at ht
      cases s with
      | mk tvs ty =>
          dsimp only at ht
          rw [hs, ht.1, teq_eq ht.2]
    · exact absurd ht (by simp)

private theorem flatten_boolT : Ty.flatten boolT = (.con "Bool", []) := rfl

private theorem ctorWidth_bool {Δ : DEnv} (h : denvOk Δ = true) {c : String}
    (hc : c = "False" ∨ c = "True") (k : Nat) (vis : List Ty) :
    Δ.ctorWidth (k + 1) vis boolT c = .ok 0 := by
  rw [DEnv.ctorWidth, denvOk_sig h hc]
  rfl

private theorem sizeOf_bool {Δ : DEnv} (h : denvOk Δ = true) (k : Nat) :
    Δ.sizeOf (k + 2) [] boolT = .ok 1 := by
  rw [DEnv.sizeOf]
  show (if Ty.isTupleCon "Bool" = true then _ else _) = _
  rw [if_neg (by simp [bool_not_tuple h])]
  show (if ([] : List Ty).any (Ty.eq · boolT) = true then _ else _) = _
  rw [if_neg (by simp)]
  rw [denvOk_ctors h]
  show (do
    let ws ← ["False", "True"].mapM (Δ.ctorWidth (k + 1) (boolT :: []) boolT)
    pure (nbits 2 + ws.foldl max 0) : Except String Nat) = _
  rw [show ["False", "True"].mapM (Δ.ctorWidth (k + 1) (boolT :: []) boolT)
        = .ok [0, 0] by
      rw [List.mapM_cons, ctorWidth_bool h (Or.inl rfl) k, except_bind_ok,
          List.mapM_cons, ctorWidth_bool h (Or.inr rfl) k, except_bind_ok,
          List.mapM_nil]
      rfl]
  rfl

private theorem ctorTag_bool {Δ : DEnv} (h : denvOk Δ = true) (b : Bool) :
    Δ.ctorTag boolT (if b then "True" else "False") = .ok (if b then 1 else 0, 1) := by
  rw [DEnv.ctorTag]
  show (if Ty.isTupleCon "Bool" = true then _ else _) = _
  rw [if_neg (by simp [bool_not_tuple h]), denvOk_ctors h]
  cases b <;> rfl

private theorem rep_boolVal {Δ : DEnv} (h : denvOk Δ = true) (b : Bool) (k : Nat) :
    Val.rep Δ (k + 2) (Eval.boolVal b) = .ok (Rwv.Hyle.Sem.b1 b) := by
  rw [show Eval.boolVal b = .con boolT (if b then "True" else "False") [] by
        cases b <;> rfl]
  rw [Val.rep]
  rw [show Δ.sizeOf (k + 1 + 1) [] boolT = .ok 1 from sizeOf_bool h k, except_bind_ok]
  rw [ctorTag_bool h b, except_bind_ok]
  dsimp only [List.attach_nil, List.mapM_nil]
  rw [except_pure_def, except_bind_ok]
  rw [if_pos (show 1 + (Val.bvConcat []).width ≤ 1 by rw [bvConcat_nil]; exact Nat.le_refl 1)]
  rw [except_pure_def]
  congr 1
  rw [bvConcat3,
      show bvCat (⟨1 - 1 - (Val.bvConcat []).width, 0⟩ : BV) (Val.bvConcat [])
        = Val.bvConcat [] from bvCat_zero_left (by rw [bvConcat_nil]; rfl),
      bvConcat_nil,
      show bvCat (⟨1, BitVec.ofNat 1 (if b then 1 else 0)⟩ : BV) BV.nil
        = ⟨1, BitVec.ofNat 1 (if b then 1 else 0)⟩ from bvCat_zero_right rfl]
  cases b <;> rfl

/-! ## Tag arithmetic -/

private theorem nbits_le (n : Nat) : n ≤ 2 ^ nbits n := by
  rw [nbits]
  by_cases h : n ≤ 1
  · rw [if_pos h]; simpa using h
  · rw [if_neg h]
    have := @Nat.lt_log2_self (n - 1)
    omega

private theorem idxOf?_getElem? {l : List String} {c : String} {i : Nat}
    (h : l.idxOf? c = some i) : l[i]? = some c := by
  have hp := List.of_findIdx?_eq_some (p := fun x => x == c) h
  cases hx : l[i]? with
  | none => rw [hx] at hp; exact absurd hp (by simp)
  | some a =>
      rw [hx] at hp
      simp only [beq_iff_eq] at hp
      rw [hp]

private theorem idxOf?_lt {l : List String} {c : String} {i : Nat}
    (h : l.idxOf? c = some i) : i < l.length :=
  List.getElem?_eq_some_iff.mp (idxOf?_getElem? h) |>.1

private theorem idxOf?_inj {l : List String} {c c' : String} {i : Nat}
    (h : l.idxOf? c = some i) (h' : l.idxOf? c' = some i) : c = c' := by
  have := (idxOf?_getElem? h).symm.trans (idxOf?_getElem? h')
  exact Option.some.inj this

/-- Inversion of `DEnv.ctorTag`. -/
private theorem ctorTag_inv {Δ : DEnv} {t : Ty} {c : String} {tag w : Nat}
    (h : Δ.ctorTag t c = .ok (tag, w)) :
    ∃ tc args, Ty.flatten t = (.con tc, args) ∧
      ((Ty.isTupleCon tc = true ∧ tag = 0 ∧ w = 0) ∨
       (Ty.isTupleCon tc = false ∧ ∃ cs, Δ.ctors.get? tc = some cs ∧
         cs.idxOf? c = some tag ∧ w = nbits cs.length)) := by
  rw [DEnv.ctorTag] at h
  rcases hfl : Ty.flatten t with ⟨th, args⟩
  rw [hfl] at h
  cases th with
  | con tc =>
      dsimp only at h
      refine ⟨tc, args, rfl, ?_⟩
      by_cases htup : Ty.isTupleCon tc
      · rw [if_pos htup] at h
        rw [except_pure_def] at h
        injection h with h
        injection h with h1 h2
        exact .inl ⟨htup, h1.symm, h2.symm⟩
      · rw [if_neg htup] at h
        cases hcs : Δ.ctors.get? tc with
        | none => rw [hcs] at h; exact error_ne_ok h
        | some cs =>
            rw [hcs] at h
            dsimp only at h
            cases hidx : cs.idxOf? c with
            | none => rw [hidx] at h; exact error_ne_ok h
            | some idx =>
                rw [hidx] at h
                dsimp only at h
                rw [except_pure_def] at h
                injection h with h
                injection h with h1 h2
                exact .inr ⟨by simp [htup], cs, rfl, by rw [hidx, h1], h2.symm⟩
  | app t₁ t₂ => exact error_ne_ok h
  | var a => exact error_ne_ok h
  | nat n => exact error_ne_ok h
  | arrow t₁ t₂ => exact error_ne_ok h

/-- A type that flattens headless is that head. -/
private theorem flatten_atom {t h : Ty} (hfl : Ty.flatten t = (h, [])) : t = h := by
  cases t with
  | app t₁ t₂ =>
      have : (Ty.flatten t₁).2 ++ [t₂] = ([] : List Ty) := congrArg Prod.snd hfl
      exact absurd this (by simp)
  | con c => exact congrArg Prod.fst hfl
  | var a => exact congrArg Prod.fst hfl
  | nat n => exact congrArg Prod.fst hfl
  | arrow t₁ t₂ => exact congrArg Prod.fst hfl

/-! ## `sizeOf` inversion at the representable heads -/

private theorem sizeOf_inv_vec {Δ : DEnv} {k : Nat} {vis : List Ty} {t n te : Ty} {w : Nat}
    (hfl : Ty.flatten t = (.con "Vec", [n, te]))
    (h : Δ.sizeOf (k + 1) vis t = .ok w) :
    ∃ kk we, Ty.evalNat n = some kk ∧ Δ.sizeOf k vis te = .ok we ∧ w = kk * we := by
  rw [DEnv.sizeOf] at h
  split at h
  case h_1 =>
      rename_i nn tee heq
      rw [hfl] at heq
      have hpair : n = nn ∧ te = tee := by simpa using heq
      obtain ⟨h1, h2⟩ := hpair
      subst h1; subst h2
      split at h
      · rename_i kk hkk
        obtain ⟨we, hwe, h⟩ := except_bind_eq_ok h
        rw [except_pure_def] at h
        injection h with h
        exact ⟨kk, we, hkk, hwe, h.symm⟩
      · exact error_ne_ok h
  case h_2 => rename_i nn heq; rw [hfl] at heq; simp at heq
  case h_3 => rename_i heq; rw [hfl] at heq; simp at heq
  case h_4 => rename_i snd heq; rw [hfl] at heq; simp at heq
  case h_5 =>
      rename_i c args hvec hfin hint hprox heq
      rw [hfl] at heq
      have hp : "Vec" = c ∧ [n, te] = args := by simpa using heq
      exact (hvec n te hp.2.symm hp.1.symm).elim
  case h_6 => rename_i a snd heq; rw [hfl] at heq; simp at heq
  case h_7 => exact error_ne_ok h

private theorem sizeOf_inv_finite {Δ : DEnv} {k : Nat} {vis : List Ty} {t n : Ty} {w : Nat}
    (hfl : Ty.flatten t = (.con "Finite", [n]))
    (h : Δ.sizeOf (k + 1) vis t = .ok w) :
    ∃ kk, Ty.evalNat n = some kk ∧ w = nbits kk := by
  rw [DEnv.sizeOf] at h
  split at h
  case h_1 => rename_i nn tee heq; rw [hfl] at heq; simp at heq
  case h_2 =>
      rename_i nn heq
      rw [hfl] at heq
      have hpair : n = nn := by simpa using heq
      subst hpair
      split at h
      · rename_i kk hkk
        rw [except_pure_def] at h
        injection h with h
        exact ⟨kk, hkk, h.symm⟩
      · exact error_ne_ok h
  case h_3 => rename_i heq; rw [hfl] at heq; simp at heq
  case h_4 => rename_i snd heq; rw [hfl] at heq; simp at heq
  case h_5 =>
      rename_i c args hvec hfin hint hprox heq
      rw [hfl] at heq
      have hp : "Finite" = c ∧ [n] = args := by simpa using heq
      exact (hfin n hp.2.symm hp.1.symm).elim
  case h_6 => rename_i a snd heq; rw [hfl] at heq; simp at heq
  case h_7 => exact error_ne_ok h

private theorem sizeOf_inv_integer {Δ : DEnv} {k : Nat} {vis : List Ty} {t : Ty} {w : Nat}
    (hfl : Ty.flatten t = (.con "Integer", []))
    (h : Δ.sizeOf (k + 1) vis t = .ok w) : w = 128 := by
  rw [DEnv.sizeOf] at h
  split at h
  case h_1 => rename_i nn tee heq; rw [hfl] at heq; simp at heq
  case h_2 => rename_i nn heq; rw [hfl] at heq; simp at heq
  case h_3 =>
      rw [except_pure_def] at h
      injection h with h
      exact h.symm
  case h_4 => rename_i snd heq; rw [hfl] at heq; simp at heq
  case h_5 =>
      rename_i c args hvec hfin hint hprox heq
      rw [hfl] at heq
      have hp : "Integer" = c ∧ ([] : List Ty) = args := by simpa using heq
      exact (hint hp.2.symm hp.1.symm).elim
  case h_6 => rename_i a snd heq; rw [hfl] at heq; simp at heq
  case h_7 => exact error_ne_ok h

private theorem sizeOf_inv_proxy {Δ : DEnv} {k : Nat} {vis : List Ty} {t : Ty} {args : List Ty}
    {w : Nat} (hfl : Ty.flatten t = (.con "Proxy", args))
    (h : Δ.sizeOf (k + 1) vis t = .ok w) : w = 0 := by
  rw [DEnv.sizeOf] at h
  split at h
  case h_1 => rename_i nn tee heq; rw [hfl] at heq; simp at heq
  case h_2 => rename_i nn heq; rw [hfl] at heq; simp at heq
  case h_3 => rename_i heq; rw [hfl] at heq; simp at heq
  case h_4 =>
      rw [except_pure_def] at h
      injection h with h
      exact h.symm
  case h_5 =>
      rename_i c args' hvec hfin hint hprox heq
      rw [hfl] at heq
      have hp : "Proxy" = c ∧ args = args' := by simpa using heq
      exact (hprox hp.1.symm).elim
  case h_6 => rename_i a snd heq; rw [hfl] at heq; simp at heq
  case h_7 => exact error_ne_ok h

/-! ## Representation width from canonicality -/

private theorem vty_rep_width {Δ : DEnv} :
    ∀ {v : Val} {t : Ty}, VTy Δ v t → ∀ {k : Nat} {bv : BV}, Val.rep Δ k v = .ok bv →
      ∀ {k' : Nat} {w : Nat}, Δ.sizeOf k' [] t = .ok w → bv.width = w := by
  intro v t hv
  induction hv with
  | vec hfl hn hlen helems ih =>
      rename_i elems t n te kk
      intro k bv hrep k' w hsz
      cases k with
      | zero => rw [Val.rep] at hrep; exact error_ne_ok hrep
      | succ k =>
          rw [Val.rep] at hrep
          obtain ⟨bs, hbs, hrep⟩ := except_bind_eq_ok hrep
          rw [mapM_attach_erase] at hbs
          rw [except_pure_def] at hrep
          injection hrep with hrep
          subst hrep
          cases k' with
          | zero => rw [DEnv.sizeOf] at hsz; exact error_ne_ok hsz
          | succ k' =>
              obtain ⟨kk', we, hkk', hwe, hw⟩ := sizeOf_inv_vec hfl hsz
              rw [hkk'] at hn
              injection hn with hn
              subst hn
              subst hw
              obtain ⟨hblen, hpt⟩ := mapM_ok_idx hbs
              have hwidths : ∀ x ∈ bs, x.width = we := by
                intro x hx
                obtain ⟨j, hj, hxj⟩ := List.getElem_of_mem hx
                obtain ⟨hj', hrepj⟩ := hpt j (by omega)
                subst hxj
                exact ih elems[j] (List.getElem_mem _) hrepj hwe
              rw [bvConcat_eq, catAll_width]
              rw [sum_const (c := we) (by
                intro a ha
                obtain ⟨x, hx, hxa⟩ := List.mem_map.mp ha
                rw [← hxa]
                exact hwidths x hx)]
              simp only [List.length_map, hblen, hlen]
  | integer hfl =>
      intro k bv hrep k' w hsz
      cases k with
      | zero => rw [Val.rep] at hrep; exact error_ne_ok hrep
      | succ k =>
          rw [Val.rep] at hrep
          injection hrep with hrep
          subst hrep
          cases k' with
          | zero => rw [DEnv.sizeOf] at hsz; exact error_ne_ok hsz
          | succ k' =>
              rw [sizeOf_inv_integer hfl hsz]
  | finite hfl hn =>
      intro k bv hrep k' w hsz
      cases k with
      | zero => rw [Val.rep] at hrep; exact error_ne_ok hrep
      | succ k =>
          rw [Val.rep] at hrep
          injection hrep with hrep
          subst hrep
          cases k' with
          | zero => rw [DEnv.sizeOf] at hsz; exact error_ne_ok hsz
          | succ k' =>
              obtain ⟨kk', hkk', hw⟩ := sizeOf_inv_finite hfl hsz
              rw [hkk'] at hn
              injection hn with hn
              subst hn
              subst hw
              rfl
  | proxy hfl _ =>
      intro k bv hrep k' w hsz
      cases k with
      | zero => rw [Val.rep] at hrep; exact error_ne_ok hrep
      | succ k =>
          rw [Val.rep] at hrep
          injection hrep with hrep
          subst hrep
          cases k' with
          | zero => rw [DEnv.sizeOf] at hsz; exact error_ne_ok hsz
          | succ k' =>
              rw [sizeOf_inv_proxy hfl hsz]
              rfl
  | con hsig hmatch hlen hctor hfields ih =>
      rename_i t c fields sig sub
      intro k bv hrep k' w hsz
      cases k with
      | zero => rw [Val.rep] at hrep; exact error_ne_ok hrep
      | succ k =>
          rw [Val.rep] at hrep
          obtain ⟨whole, hwhole, hrep⟩ := except_bind_eq_ok hrep
          obtain ⟨tg, htg, hrep⟩ := except_bind_eq_ok hrep
          obtain ⟨tag, tagW⟩ := tg
          obtain ⟨bs, hbs, hrep⟩ := except_bind_eq_ok hrep
          dsimp only at hrep
          split at hrep
          · rename_i hle
            rw [except_pure_def] at hrep
            injection hrep with hrep
            subst hrep
            rw [bvConcat3, bvCat_width, bvCat_width]
            have : whole = w := sizeOf_det hwhole hsz
            dsimp only [BV.width] at hle ⊢
            omega
          · exact error_ne_ok hrep

/-! ## `Bool` value facts -/

private theorem vty_boolVal {Δ : DEnv} (h : denvOk Δ = true) (b : Bool) :
    VTy Δ (Eval.boolVal b) Eval.boolTy := by
  have hsig : Δ.ctorSig.get? (if b then "True" else "False") = some ⟨[], boolT⟩ :=
    denvOk_sig h (by cases b <;> simp)
  rw [show Eval.boolVal b = .con boolT (if b then "True" else "False") [] by cases b <;> rfl,
      show Eval.boolTy = boolT from rfl]
  refine VTy.con (sub := ∅) hsig ?_ ?_ ?_ ?_
  · show DEnv.matchTy (Ty.flattenArrow boolT).2 boolT = .ok ∅
    rfl
  · rfl
  · show ctorOf Δ boolT (if b then "True" else "False")
    rw [ctorOf, flatten_boolT]
    show if Ty.isTupleCon "Bool" then _ else _
    rw [if_neg (by simp [bool_not_tuple h])]
    exact ⟨["False", "True"], denvOk_ctors h, by cases b <;> simp⟩
  · intro p hp
    exact absurd hp (by simp)

private theorem vty_bool_inv {Δ : DEnv} (h : denvOk Δ = true) {v : Val} {t : Ty}
    (ht : t = boolT) (hv : VTy Δ v t) : ∃ b, v = Eval.boolVal b := by
  subst ht
  cases hv with
  | vec hfl _ _ _ => rw [flatten_boolT] at hfl; simp at hfl
  | integer hfl => rw [flatten_boolT] at hfl; simp at hfl
  | finite hfl _ => rw [flatten_boolT] at hfl; simp at hfl
  | proxy hfl _ => rw [flatten_boolT] at hfl; simp at hfl
  | con hsig hmatch hlen hctor hfields =>
      rename_i c fields sig sub
      have hc : c = "False" ∨ c = "True" := by
        rw [ctorOf, flatten_boolT] at hctor
        revert hctor
        show (if Ty.isTupleCon "Bool" then _ else _) → _
        rw [if_neg (by simp [bool_not_tuple h])]
        rintro ⟨cs, hcs, hmem⟩
        rw [denvOk_ctors h] at hcs
        injection hcs with hcs
        subst hcs
        simpa using hmem
      have hsig' := (denvOk_sig h hc).symm.trans hsig
      injection hsig' with hsig'
      subst hsig'
      have : fields = [] := by
        have hl0 : fields.length = 0 := hlen
        exact List.length_eq_zero_iff.mp hl0
      subst this
      rcases hc with hc | hc <;> subst hc
      · exact ⟨false, rfl⟩
      · exact ⟨true, rfl⟩

/-! ## `bitsToVec` representation and canonicality -/

private theorem rep_bitsToVec {Δ : DEnv} (h : denvOk Δ = true) (x : BV) (k : Nat) :
    Val.rep Δ (k + 3) (Eval.bitsToVec x) = .ok x := by
  rw [Eval.bitsToVec, Val.rep, mapM_attach_erase]
  rw [show ((List.range x.width).map fun i => Eval.boolVal (x.bits.getMsbD i)).mapM
        (Val.rep Δ (k + 2))
      = .ok ((List.range x.width).map fun i => Rwv.Hyle.Sem.b1 (x.bits.getMsbD i)) by
    rw [List.mapM_map]
    exact mapM_ok_of_forall fun i _ => rep_boolVal h (x.bits.getMsbD i) k]
  rw [except_bind_ok, except_pure_def]
  congr 1
  -- bvConcat of the MSB-first bit list is the vector itself.
  rw [bvConcat_eq]
  have main : ∀ n, n ≤ x.width →
      catAll ((List.range n).map fun i => Rwv.Hyle.Sem.b1 (x.bits.getMsbD i))
        = sliceBV x (x.width - n) n := by
    intro n
    induction n with
    | zero =>
        intro _
        refine bv_ext rfl ?_
        intro i
        rw [show catAll ((List.range 0).map fun i => Rwv.Hyle.Sem.b1 (x.bits.getMsbD i))
              = BV.nil from rfl]
        rw [sliceBV_getLsbD]
        simp [BV.nil, BV.ofNat]
    | succ n ih =>
        intro hn
        rw [List.range_succ, List.map_append, catAll_append]
        simp only [List.map_cons, List.map_nil]
        rw [show catAll [Rwv.Hyle.Sem.b1 (x.bits.getMsbD n)] =
              Rwv.Hyle.Sem.b1 (x.bits.getMsbD n) by
          rw [catAll_cons, catAll_nil, bvCat_zero_right rfl]]
        rw [ih (by omega)]
        refine bv_ext (by rw [bvCat_width, sliceBV, sliceBV]; rfl) ?_
        intro i
        rw [bvCat_getLsbD]
        by_cases hi : i < 1
        · rw [if_pos (show i < (Rwv.Hyle.Sem.b1 (x.bits.getMsbD n)).width from hi)]
          have hi0 : i = 0 := by omega
          subst hi0
          rw [show (Rwv.Hyle.Sem.b1 (x.bits.getMsbD n)).bits.getLsbD 0 = x.bits.getMsbD n by
                cases hb : x.bits.getMsbD n <;> simp [Rwv.Hyle.Sem.b1]]
          rw [sliceBV_getLsbD, BitVec.getMsbD_eq_getLsbD]
          rw [decide_eq_true (show n < x.width by omega), Bool.true_and,
              decide_eq_true (show (0 : Nat) < n + 1 by omega), Bool.true_and]
          congr 1
          omega
        · rw [if_neg (show ¬ i < (Rwv.Hyle.Sem.b1 (x.bits.getMsbD n)).width from hi)]
          show (sliceBV x (x.width - n) n).bits.getLsbD (i - 1)
              = (sliceBV x (x.width - (n + 1)) (n + 1)).bits.getLsbD i
          rw [sliceBV_getLsbD, sliceBV_getLsbD]
          by_cases hin : i - 1 < n
          · rw [decide_eq_true hin, Bool.true_and,
                decide_eq_true (show i < n + 1 by omega), Bool.true_and]
            congr 1
            omega
          · rw [decide_eq_false hin, Bool.false_and,
                decide_eq_false (show ¬ i < n + 1 by omega), Bool.false_and]
  rw [main x.width (Nat.le_refl _), Nat.sub_self]
  exact sliceBV_all x

private theorem vty_bitsToVec {Δ : DEnv} (h : denvOk Δ = true) {t n te : Ty} {k : Nat}
    (hfl : Ty.flatten t = (.con "Vec", [n, te])) (hn : Ty.evalNat n = some k)
    (hte : te = boolT) {x : BV} (hw : x.width = k) : VTy Δ (Eval.bitsToVec x) t := by
  subst hte
  refine VTy.vec hfl hn (by simp [hw]) ?_
  intro e he
  have he' : e ∈ (List.range x.width).map fun i => Eval.boolVal (x.bits.getMsbD i) := he
  obtain ⟨i, _, hie⟩ := List.mem_map.mp he'
  rw [← hie]
  exact vty_boolVal h _


/-! ## `vecBoolLen` inversion, `ctorOfB` soundness, and Vec-Bool widths -/

private theorem vecBoolLen_inv {who : String} {t : Ty} {k : Nat}
    (h : vecBoolLen who t = .ok k) :
    ∃ n te, Ty.flatten t = (.con "Vec", [n, te]) ∧ te = boolT ∧ Ty.evalNat n = some k := by
  rw [vecBoolLen] at h
  split at h
  case h_1 =>
      rename_i n te heq
      split at h
      · rename_i hte
        split at h
        · rename_i kk hkk
          injection h with h
          subst h
          exact ⟨n, te, heq, teq_eq hte, hkk⟩
        · exact error_ne_ok h
      · exact error_ne_ok h
  case h_2 => exact error_ne_ok h

private theorem ctorOfB_sound {Δ : DEnv} {t : Ty} {c : String}
    (h : ctorOfB Δ t c = true) : ctorOf Δ t c := by
  rw [ctorOfB] at h
  rw [ctorOf]
  split at h
  case h_1 =>
      rename_i tc args heq
      by_cases htup : Ty.isTupleCon tc = true
      · rw [if_pos htup] at h ⊢
        exact beq_iff_eq.mp h
      · rw [if_neg htup] at h ⊢
        split at h
        · rename_i cs hcs
          exact ⟨cs, hcs, by simpa using h⟩
        · exact absurd h (by simp)
  case h_2 => exact absurd h (by simp)

/-- The representation width of a canonical `Vec kk Bool` value is kk
(directly, without a `sizeOf` computation). -/
private theorem vty_vecBool_rep_width {Δ : DEnv} (hΔ : denvOk Δ = true) {v : Val}
    {t n te : Ty} {kk : Nat} (hfl : Ty.flatten t = (.con "Vec", [n, te]))
    (hn : Ty.evalNat n = some kk) (hte : te = boolT) (hv : VTy Δ v t)
    {k : Nat} {bv : BV} (hrep : Val.rep Δ k v = .ok bv) : bv.width = kk := by
  cases hv with
  | vec hfl' hn' hlen helems =>
      rename_i elems n' te' kk'
      rw [hfl] at hfl'
      have hpair : n = n' ∧ te = te' := by simpa using hfl'
      obtain ⟨h1, h2⟩ := hpair
      subst h1; subst h2
      rw [hn] at hn'
      injection hn' with hn'
      subst hn'
      cases k with
      | zero => rw [Val.rep] at hrep; exact error_ne_ok hrep
      | succ k =>
          rw [Val.rep] at hrep
          obtain ⟨bs, hbs, hrep⟩ := except_bind_eq_ok hrep
          rw [mapM_attach_erase] at hbs
          rw [except_pure_def] at hrep
          injection hrep with hrep
          subst hrep
          obtain ⟨hblen, hpt⟩ := mapM_ok_idx hbs
          rw [bvConcat_eq, catAll_width]
          rw [sum_const (c := 1) (by
            intro a ha
            obtain ⟨x, hx, hxa⟩ := List.mem_map.mp ha
            obtain ⟨j, hj, hxj⟩ := List.getElem_of_mem hx
            obtain ⟨hj', hrepj⟩ := hpt j (by omega)
            obtain ⟨b, hb⟩ := vty_bool_inv hΔ hte (helems elems[j] (List.getElem_mem _))
            rw [← hxa, ← hxj]
            subst hxj
            rw [hb] at hrepj
            rw [rep_det hrepj (rep_boolVal hΔ b 0)]
            rfl)]
          simp only [List.length_map, hblen, hlen, Nat.mul_one]
  | integer hfl' => rw [hfl] at hfl'; simp at hfl'
  | finite hfl' _ => rw [hfl] at hfl'; simp at hfl'
  | proxy hfl' _ => rw [hfl] at hfl'; simp at hfl'
  | con hsig hmatch hlen hctor hfields =>
      rename_i c fields sig sub
      exfalso
      rw [ctorOf] at hctor
      rw [hfl] at hctor
      dsimp only at hctor
      rw [if_neg (by simp [vec_not_tuple hΔ])] at hctor
      obtain ⟨cs, hcs, hmem⟩ := hctor
      rw [vec_abstract hΔ hcs] at hmem
      exact absurd hmem (by simp)


/-! ## Environment correspondence (the `EnvC` side condition) -/

/-- The environment correspondence of the soundness theorem: every
Γ-recorded binding has a canonical concrete value whose representation
is the recorded normal form's denotation; a unique absent from Γ is
absent from the concrete environment (the compiler resolves such names
globally). -/
structure EnvC (Δ : DEnv) (σ : String → BV) (Γ : HashMap Int (NF × Ty))
    (env : Eval.Env) : Prop where
  fwd : ∀ x nt, Γ.get? x = some nt → ∃ v, env.lookup x = some v ∧ VTy Δ v nt.2 ∧
      ∃ k, Val.rep Δ k v = .ok (nt.1.eval σ E)
  miss : ∀ x, Γ.get? x = none → env.lookup x = none

private theorem envC_empty {Δ : DEnv} {σ : String → BV} :
    EnvC (E := E) Δ σ (∅ : HashMap Int (NF × Ty)) ([] : Eval.Env) := by
  constructor
  · intro x nt h
    rw [HashMap.get?_eq_getElem?] at h
    simp at h
  · intro x _
    rfl

private theorem envC_cons {Δ : DEnv} {σ : String → BV} {Γ : HashMap Int (NF × Ty)}
    {env : Eval.Env} (h : EnvC (E := E) Δ σ Γ env) {u : Int} {n : NF} {t : Ty} {v : Val}
    (hv : VTy Δ v t) (hrep : ∃ k, Val.rep Δ k v = .ok (n.eval σ E)) :
    EnvC (E := E) Δ σ (Γ.insert u (n, t)) ((u, v) :: env) := by
  constructor
  · intro x nt hx
    rw [get?_insert] at hx
    rw [lookup_cons]
    by_cases hxu : x = u
    · rw [if_pos hxu] at hx
      rw [if_pos hxu]
      injection hx with hx
      subst hx
      exact ⟨v, rfl, hv, hrep⟩
    · rw [if_neg hxu] at hx
      rw [if_neg hxu]
      exact h.fwd x nt hx
  · intro x hx
    rw [get?_insert] at hx
    rw [lookup_cons]
    by_cases hxu : x = u
    · rw [if_pos hxu] at hx
      exact absurd hx (by simp)
    · rw [if_neg hxu] at hx
      rw [if_neg hxu]
      exact h.miss x hx

/-- Binding a telescope: `bindFieldsΓ` on the symbolic side, a zip
prepended on the concrete side, from pointwise facts. -/
private theorem envC_bind {Δ : DEnv} {σ : String → BV} :
    ∀ (params : List Id) (pas : List (NF × Ty)) (vs : List Val)
      {Γ₀ : HashMap Int (NF × Ty)} {env₀ : Eval.Env},
      EnvC (E := E) Δ σ Γ₀ env₀ →
      pas.length = params.length → vs.length = params.length →
      (∀ i (h1 : i < params.length) (h2 : i < pas.length) (h3 : i < vs.length),
        VTy Δ vs[i] (pas[i].2) ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E)) →
      EnvC (E := E) Δ σ (bindFieldsΓ params pas Γ₀) (((params.map (·.uniq)).zip vs) ++ env₀) := by
  intro params
  induction params with
  | nil =>
      intro pas vs Γ₀ env₀ h0 _ _ _
      simpa [bindFieldsΓ] using h0
  | cons p ps ih =>
      intro pas vs Γ₀ env₀ h0 hl1 hl2 hpt
      match pas, vs with
      | [], _ => exact absurd hl1 (by simp)
      | _ :: _, [] => exact absurd hl2 (by simp)
      | nt :: nts, v :: vv =>
          have hhead := hpt 0 (by simp) (by simp) (by simp)
          have step : bindFieldsΓ (p :: ps) (nt :: nts) Γ₀
              = (bindFieldsΓ ps nts Γ₀).insert p.uniq nt := by
            simp only [bindFieldsΓ, List.zip_cons_cons, List.foldr_cons]
          rw [step]
          have henv : ((p :: ps).map (·.uniq)).zip (v :: vv) ++ env₀
              = (p.uniq, v) :: (((ps.map (·.uniq)).zip vv) ++ env₀) := by
            simp
          rw [henv]
          have htail := ih nts vv h0 (by simpa using hl1) (by simpa using hl2)
            (fun i h1 h2 h3 => by
              have := hpt (i + 1) (by simpa using h1) (by simpa using h2) (by simpa using h3)
              simpa using this)
          exact envC_cons htail (by simpa using hhead.1) (by simpa using hhead.2)

/-! ## `rep` fuel plumbing and the constructor layout -/

private theorem mapM_rep_mono {Δ : DEnv} {k k' : Nat} (hk : k ≤ k') :
    ∀ {vs : List Val} {bs : List BV}, vs.mapM (Val.rep Δ k) = .ok bs →
      vs.mapM (Val.rep Δ k') = .ok bs := by
  intro vs
  induction vs with
  | nil => intro bs h; simpa using h
  | cons v vv ih =>
      intro bs h
      rw [List.mapM_cons] at h ⊢
      obtain ⟨b, hb, h⟩ := except_bind_eq_ok h
      obtain ⟨bs', hbs, h⟩ := except_bind_eq_ok h
      rw [Val.rep_mono Δ hk hb, except_bind_ok, ih hbs, except_bind_ok]
      exact h

private theorem mapM_rep_exists {Δ : DEnv} :
    ∀ {vs : List Val} {bs : List BV}, vs.length = bs.length →
      (∀ i (h1 : i < vs.length) (h2 : i < bs.length), ∃ k, Val.rep Δ k vs[i] = .ok bs[i]) →
      ∃ K, vs.mapM (Val.rep Δ K) = .ok bs := by
  intro vs
  induction vs with
  | nil =>
      intro bs hl _
      match bs, hl with
      | [], _ => exact ⟨0, rfl⟩
  | cons v vv ih =>
      intro bs hl hpt
      match bs, hl with
      | b :: bs', hl =>
          obtain ⟨k0, hk0⟩ := hpt 0 (by simp) (by simp)
          obtain ⟨K, hK⟩ := ih (by simpa using hl)
            (fun i h1 h2 => by
              have := hpt (i + 1) (by simpa using h1) (by simpa using h2)
              simpa using this)
          refine ⟨max k0 K, ?_⟩
          rw [List.mapM_cons,
              Val.rep_mono Δ (Nat.le_max_left k0 K) (by simpa using hk0), except_bind_ok,
              mapM_rep_mono (Nat.le_max_right k0 K) hK, except_bind_ok]
          rfl

/-- The constructor representation, assembled. -/
private theorem rep_con_intro {Δ : DEnv} {ty : Ty} {c : String} {fields : List Val}
    {K : Nat} {whole tag tagW : Nat} {bs : List BV}
    (hwhole : Δ.sizeOf (K + 1) [] ty = .ok whole)
    (htag : Δ.ctorTag ty c = .ok (tag, tagW))
    (hbs : fields.mapM (Val.rep Δ K) = .ok bs)
    (hle : tagW + (Val.bvConcat bs).width ≤ whole) :
    Val.rep Δ (K + 1) (.con ty c fields) = .ok
      (bvCat ⟨tagW, BitVec.ofNat tagW tag⟩
        (bvCat ⟨whole - tagW - (Val.bvConcat bs).width, 0⟩ (Val.bvConcat bs))) := by
  rw [Val.rep, hwhole, except_bind_ok, htag, except_bind_ok]
  try dsimp only
  rw [mapM_attach_erase, hbs, except_bind_ok]
  try dsimp only
  rw [if_pos hle, except_pure_def]
  congr 1
  exact bvConcat3 ..

/-- The constructor representation, dissected. -/
private theorem rep_con_inv {Δ : DEnv} {ty : Ty} {c : String} {fields : List Val} {k : Nat}
    {bv : BV} (h : Val.rep Δ k (.con ty c fields) = .ok bv) :
    ∃ k' whole tag tagW bs, k = k' + 1 ∧
      Δ.sizeOf (k' + 1) [] ty = .ok whole ∧
      Δ.ctorTag ty c = .ok (tag, tagW) ∧
      fields.mapM (Val.rep Δ k') = .ok bs ∧
      tagW + (Val.bvConcat bs).width ≤ whole ∧
      bv = bvCat ⟨tagW, BitVec.ofNat tagW tag⟩
        (bvCat ⟨whole - tagW - (Val.bvConcat bs).width, 0⟩ (Val.bvConcat bs)) := by
  cases k with
  | zero => rw [Val.rep] at h; exact error_ne_ok h
  | succ k =>
      rw [Val.rep] at h
      obtain ⟨whole, hwhole, h⟩ := except_bind_eq_ok h
      obtain ⟨tg, htg, h⟩ := except_bind_eq_ok h
      obtain ⟨tag, tagW⟩ := tg
      obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
      rw [mapM_attach_erase] at hbs
      dsimp only at h
      split at h
      · rename_i hle
        rw [except_pure_def] at h
        injection h with h
        exact ⟨k, whole, tag, tagW, bs, rfl, hwhole, htg, hbs, hle,
          h.symm.trans (bvConcat3 ..)⟩
      · exact error_ne_ok h

/-! ## Literal inversions -/

private theorem clitInt_inv {ty : Ty} {n : Int} {nf : NF} {rty : Ty}
    (h : clitInt ty n = .ok (nf, rty)) :
    rty = ty ∧
    ((Ty.flatten ty = (.con "Integer", []) ∧ nf = .lit ⟨128, BitVec.ofInt 128 n⟩) ∨
     (∃ bt k, Ty.flatten ty = (.con "Finite", [bt]) ∧ Ty.evalNat bt = some k ∧
        nf = .lit ⟨nbits k, BitVec.ofInt (nbits k) n⟩) ∨
     (∃ lt et w, Ty.flatten ty = (.con "Vec", [lt, et]) ∧ et = boolT ∧
        Ty.evalNat lt = some w ∧ nf = .lit ⟨w, BitVec.ofInt w n⟩)) := by
  rw [clitInt] at h
  split at h
  case h_1 =>
      rename_i heq
      injection h with h
      injection h with h1 h2
      exact ⟨h2.symm, .inl ⟨heq, h1.symm⟩⟩
  case h_2 =>
      rename_i bt heq
      split at h
      · rename_i k hk
        injection h with h
        injection h with h1 h2
        exact ⟨h2.symm, .inr (.inl ⟨bt, k, heq, hk, h1.symm⟩)⟩
      · exact error_ne_ok h
  case h_3 =>
      rename_i lt et heq
      split at h
      · rename_i hte
        split at h
        · rename_i w hw
          injection h with h
          injection h with h1 h2
          exact ⟨h2.symm, .inr (.inr ⟨lt, et, w, heq, teq_eq hte, hw, h1.symm⟩)⟩
        · exact error_ne_ok h
      · exact error_ne_ok h
  case h_4 => exact error_ne_ok h

private theorem litIntVal_inv_integer {ty : Ty} {n : Int} {v : Val}
    (hfl : Ty.flatten ty = (.con "Integer", []))
    (h : Eval.litIntVal ty n = .ok v) : v = .integer (BitVec.ofInt 128 n) := by
  rw [Eval.litIntVal] at h
  split at h
  case h_1 =>
      injection h with h
      exact h.symm
  all_goals rename_i heq
  case h_2 => rw [hfl] at heq; simp at heq
  case h_3 => rw [hfl] at heq; simp at heq
  case h_4 => exact error_ne_ok h

private theorem litIntVal_inv_finite {ty bt : Ty} {k : Nat} {n : Int} {v : Val}
    (hfl : Ty.flatten ty = (.con "Finite", [bt])) (hbt : Ty.evalNat bt = some k)
    (h : Eval.litIntVal ty n = .ok v) :
    0 ≤ n ∧ n < (k : Int) ∧ v = .finite k n.toNat := by
  rw [Eval.litIntVal] at h
  split at h
  case h_1 => rename_i heq; rw [hfl] at heq; simp at heq
  case h_2 =>
      rename_i bt' heq
      rw [hfl] at heq
      have hb : bt = bt' := by simpa using heq
      subst hb
      rw [hbt] at h
      dsimp only at h
      split at h
      · rename_i hrange
        rw [except_pure_def] at h
        injection h with h
        exact ⟨hrange.1, hrange.2, h.symm⟩
      · exact error_ne_ok h
  case h_3 => rename_i heq; rw [hfl] at heq; simp at heq
  case h_4 => exact error_ne_ok h

private theorem litIntVal_inv_vec {ty lt et : Ty} {w : Nat} {n : Int} {v : Val}
    (hfl : Ty.flatten ty = (.con "Vec", [lt, et])) (hlt : Ty.evalNat lt = some w)
    (h : Eval.litIntVal ty n = .ok v) :
    v = Eval.bitsToVec ⟨w, BitVec.ofInt w n⟩ := by
  rw [Eval.litIntVal] at h
  split at h
  case h_1 => rename_i heq; rw [hfl] at heq; simp at heq
  case h_2 => rename_i bt heq; rw [hfl] at heq; simp at heq
  case h_3 =>
      rename_i lt' et' heq
      rw [hfl] at heq
      have hp : lt = lt' ∧ et = et' := by simpa using heq
      obtain ⟨h1, h2⟩ := hp
      subst h1; subst h2
      split at h
      · rw [hlt] at h
        injection h with h
        exact h.symm
      · exact error_ne_ok h
  case h_4 => exact error_ne_ok h

private theorem ofInt_nonneg {w : Nat} {n : Int} (h0 : 0 ≤ n) :
    BitVec.ofInt w n = BitVec.ofNat w n.toNat := by
  obtain ⟨m, rfl⟩ := Int.eq_ofNat_of_zero_le h0
  simp [BitVec.ofInt_natCast]


/-! ## Builtin-row soundness -/

private theorem vecLen_inv {who : String} {t : Ty} {k : Nat}
    (h : Eval.vecLen who t = .ok k) :
    ∃ n te, Ty.flatten t = (.con "Vec", [n, te]) ∧ Ty.evalNat n = some k := by
  rw [Eval.vecLen] at h
  split at h
  case h_1 =>
      rename_i n te heq
      split at h
      · rename_i kk hkk
        rw [except_pure_def] at h
        injection h with h
        subst h
        exact ⟨n, te, heq, hkk⟩
      · exact error_ne_ok h
  case h_2 => exact error_ne_ok h

private theorem nat_b1 (c : Bool) : (Rwv.Hyle.Sem.b1 c).nat = if c then 1 else 0 := by
  cases c <;> rfl

private theorem decide_b1_ne (c : Bool) :
    (decide ((Rwv.Hyle.Sem.b1 c).nat ≠ 0)) = c := by
  cases c <;> rfl

private theorem decide_b1_eq (c : Bool) :
    (decide ((Rwv.Hyle.Sem.b1 c).nat = 0)) = !c := by
  cases c <;> rfl

private theorem not_b1 (c : Bool) :
    Rwv.Hyle.Sem.evalOp .not [Rwv.Hyle.Sem.b1 c] = .ok (Rwv.Hyle.Sem.b1 (!c)) := by
  cases c <;> rfl

private theorem and_b1 (p q : Bool) :
    Rwv.Hyle.Sem.evalOp .and [Rwv.Hyle.Sem.b1 p, Rwv.Hyle.Sem.b1 q]
      = .ok (Rwv.Hyle.Sem.b1 (p && q)) := by
  cases p <;> cases q <;> rfl

private theorem or_b1 (p q : Bool) :
    Rwv.Hyle.Sem.evalOp .or [Rwv.Hyle.Sem.b1 p, Rwv.Hyle.Sem.b1 q]
      = .ok (Rwv.Hyle.Sem.b1 (p || q)) := by
  cases p <;> cases q <;> rfl

private theorem redor_b1 (x : BV) :
    Rwv.Hyle.Sem.evalOp .redor [x] = .ok (Rwv.Hyle.Sem.b1 (x.bits != 0)) := rfl

private theorem decide_and' (p q : Prop) [Decidable p] [Decidable q] :
    decide (p ∧ q) = (decide p && decide q) := by
  by_cases hp : p <;> by_cases hq : q <;> simp [hp, hq]

private theorem decide_or' (p q : Prop) [Decidable p] [Decidable q] :
    decide (p ∨ q) = (decide p || decide q) := by
  by_cases hp : p <;> by_cases hq : q <;> simp [hp, hq]

private theorem bits_ne_nat (x : BV) : (x.bits != 0) = decide (x.nat ≠ 0) := by
  rcases x with ⟨w, bits⟩
  show (bits != 0#w) = decide (bits.toNat ≠ 0)
  by_cases h : bits = 0#w
  · subst h
    simp
  · have hn : bits.toNat ≠ 0 := fun h0 => h (BitVec.eq_of_toNat_eq (by simpa using h0))
    simp [h, hn]

/-- VTy transport to the `Bool`-headed result type of a comparison
row. -/
private theorem vty_bool_at {Δ : DEnv} (hΔ : denvOk Δ = true) {res : Ty}
    (hres : isBoolT res = true) (c : Bool) : VTy Δ (Eval.boolVal c) res := by
  rw [teq_eq (by simpa [isBoolT] using hres : teq res boolT = true)]
  exact vty_boolVal hΔ c

private theorem arithRow_sound {Δ : DEnv} (hΔ : denvOk Δ = true) {σ : String → BV}
    {op : Op} {res ta : Ty} {a b : NF} {nf : NF} {ty : Ty}
    (hop : ∀ x y : BV, ∃ bits : BitVec x.width,
      Rwv.Hyle.Sem.evalOp op [x, y] = .ok ⟨x.width, bits⟩)
    (hc : arithRow op res ta a b = .ok (nf, ty))
    {va vb : Val} (hva : VTy Δ va ta)
    (hrepa : ∃ k, Val.rep Δ k va = .ok (a.eval σ E))
    (hrepb : ∃ k, Val.rep Δ k vb = .ok (b.eval σ E))
    {efuel : Nat} {v : Val} (hev : Eval.bvBinArith Δ efuel op va vb = .ok v) :
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ E) := by
  rw [arithRow] at hc
  obtain ⟨m, hm, hc⟩ := except_bind_eq_ok hc
  obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
  split at hc
  · rename_i hweq
    rw [except_pure_def] at hc
    injection hc with hc
    injection hc with h1 h2
    subst h1; subst h2
    rw [Eval.bvBinArith] at hev
    obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
    obtain ⟨y, hy, hev⟩ := except_bind_eq_ok hev
    obtain ⟨r, hr, hev⟩ := except_bind_eq_ok hev
    rw [except_pure_def] at hev
    injection hev with hev
    subst hev
    obtain ⟨ka, hka⟩ := hrepa
    obtain ⟨kb, hkb⟩ := hrepb
    have hxa : x = a.eval σ E := rep_det hx hka
    have hyb : y = b.eval σ E := rep_det hy hkb
    subst hxa; subst hyb
    obtain ⟨n', te', hfa, hba, hna⟩ := vecBoolLen_inv hwa
    obtain ⟨n'', te'', hfr, hbr, hnr⟩ := vecBoolLen_inv hm
    obtain ⟨bits, hr'⟩ := hop (a.eval σ E) (b.eval σ E)
    rw [hr'] at hr
    injection hr with hr
    subst hr
    have hxw : (a.eval σ E).width = wa :=
      vty_vecBool_rep_width hΔ hfa hna hba hva hka
    constructor
    · exact vty_bitsToVec hΔ hfr hnr hbr (by simp only [BV.width]; omega)
    · refine ⟨3, ?_⟩
      have hnf : (NF.prim2 op a b).eval σ E = ⟨(a.eval σ E).width, bits⟩ := by
        simp only [NF.eval, hr']
      rw [hnf]
      exact rep_bitsToVec hΔ _ 0
  · exact error_ne_ok hc


private theorem list_len1 {α : Type} {vs : List α} (h : vs.length = 1) : ∃ a, vs = [a] := by
  match vs with
  | [a] => exact ⟨a, rfl⟩
  | [] => simp at h
  | _ :: _ :: _ => simp only [List.length_cons] at h; omega

private theorem list_len2 {α : Type} {vs : List α} (h : vs.length = 2) :
    ∃ a b, vs = [a, b] := by
  match vs with
  | [a, b] => exact ⟨a, b, rfl⟩
  | [] => simp at h
  | [_] => simp at h
  | _ :: _ :: _ :: _ => simp only [List.length_cons] at h; omega


private theorem cmpRow_sound {Δ : DEnv} (hΔ : denvOk Δ = true) {σ : String → BV}
    {op : Op} {res : Ty} {a b : NF} {nf : NF} {ty : Ty}
    (hop : ∀ x y : BV, ∃ c, Rwv.Hyle.Sem.evalOp op [x, y] = .ok (Rwv.Hyle.Sem.b1 c))
    (hc : cmpRow op res a b = .ok (nf, ty))
    {va vb : Val}
    (hrepa : ∃ k, Val.rep Δ k va = .ok (a.eval σ E))
    (hrepb : ∃ k, Val.rep Δ k vb = .ok (b.eval σ E))
    {efuel : Nat} {v : Val} (hev : Eval.bvBinCmp Δ efuel op va vb = .ok v) :
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ E) := by
  rw [cmpRow] at hc
  split at hc
  · rename_i hres
    rw [except_pure_def] at hc
    injection hc with hc
    injection hc with h1 h2
    subst h1; subst h2
    rw [Eval.bvBinCmp] at hev
    obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
    obtain ⟨y, hy, hev⟩ := except_bind_eq_ok hev
    obtain ⟨r, hr, hev⟩ := except_bind_eq_ok hev
    rw [except_pure_def] at hev
    injection hev with hev
    subst hev
    obtain ⟨ka, hka⟩ := hrepa
    obtain ⟨kb, hkb⟩ := hrepb
    have hxa : x = a.eval σ E := rep_det hx hka
    have hyb : y = b.eval σ E := rep_det hy hkb
    subst hxa; subst hyb
    obtain ⟨c, hc'⟩ := hop (a.eval σ E) (b.eval σ E)
    rw [hc'] at hr
    injection hr with hr
    subst hr
    rw [decide_b1_ne c]
    constructor
    · exact vty_bool_at hΔ hres c
    · refine ⟨2, ?_⟩
      have hnf : (NF.prim2 op a b).eval σ E = Rwv.Hyle.Sem.b1 c := by
        simp only [NF.eval, hc']
      rw [hnf]
      exact rep_boolVal hΔ c 0
  · exact error_ne_ok hc

private theorem redRow_sound {Δ : DEnv} (hΔ : denvOk Δ = true) {σ : String → BV}
    {op : Op} {negated : Bool} {res : Ty} {a : NF} {nf : NF} {ty : Ty}
    (hop : ∀ x : BV, ∃ c, Rwv.Hyle.Sem.evalOp op [x] = .ok (Rwv.Hyle.Sem.b1 c))
    (hc : redRow op negated res a = .ok (nf, ty))
    {va : Val} (hrepa : ∃ k, Val.rep Δ k va = .ok (a.eval σ E))
    {efuel : Nat} {v : Val} (hev : Eval.bvRed Δ efuel op negated va = .ok v) :
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ E) := by
  rw [redRow] at hc
  split at hc
  · rename_i hres
    rw [except_pure_def] at hc
    injection hc with hc
    rw [Eval.bvRed] at hev
    obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
    obtain ⟨r, hr, hev⟩ := except_bind_eq_ok hev
    rw [except_pure_def] at hev
    injection hev with hev
    subst hev
    obtain ⟨ka, hka⟩ := hrepa
    have hxa : x = a.eval σ E := rep_det hx hka
    subst hxa
    obtain ⟨c, hc'⟩ := hop (a.eval σ E)
    rw [hc'] at hr
    injection hr with hr
    subst hr
    cases negated with
    | false =>
        have hpair : (NF.prim1 op a, res) = (nf, ty) := by simpa using hc
        injection hpair with h1 h2
        subst h1; subst h2
        rw [show (if (false : Bool) = true then decide ((Rwv.Hyle.Sem.b1 c).nat = 0)
                  else decide ((Rwv.Hyle.Sem.b1 c).nat ≠ 0)) = c by
              rw [if_neg (by simp), decide_b1_ne]]
        constructor
        · exact vty_bool_at hΔ hres c
        · refine ⟨2, ?_⟩
          have hnf : (NF.prim1 op a).eval σ E = Rwv.Hyle.Sem.b1 c := by
            simp only [NF.eval, hc']
          rw [hnf]
          exact rep_boolVal hΔ c 0
    | true =>
        have hpair : (NF.prim1 .not (NF.prim1 op a), res) = (nf, ty) := by simpa using hc
        injection hpair with h1 h2
        subst h1; subst h2
        rw [show (if (true : Bool) = true then decide ((Rwv.Hyle.Sem.b1 c).nat = 0)
                  else decide ((Rwv.Hyle.Sem.b1 c).nat ≠ 0)) = !c by
              rw [if_pos rfl, decide_b1_eq]]
        constructor
        · exact vty_bool_at hΔ hres (!c)
        · refine ⟨2, ?_⟩
          have hnf : (NF.prim1 .not (NF.prim1 op a)).eval σ E = Rwv.Hyle.Sem.b1 (!c) := by
            simp only [NF.eval, hc', not_b1]
          rw [hnf]
          exact rep_boolVal hΔ (!c) 0
  · exact error_ne_ok hc


set_option maxHeartbeats 3200000 in
/-- Soundness of the builtin row table: if the row compiles and the
committed builtin evaluator produces a value, the value is canonical
at the compiled type and its representation is the compiled normal
form's denotation (given the argument facts the main induction
supplies). -/
private theorem cprim_sound {Δ : DEnv} {dmap : HashMap Int Defn} (hΔ : denvOk Δ = true)
    {σ : String → BV} {pty : Ty} {b : Builtin} {pas : List (NF × Ty)} {nf : NF} {ty : Ty}
    {efuel : Nat} {vs : List Val} {v : Val}
    (hc : cprim pty b pas = .ok (nf, ty))
    (hev : Eval.evalBuiltin ⟨Δ, dmap⟩ efuel pty b vs E = .ok v)
    (hlen : vs.length = pas.length)
    (hargs : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
       VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E)) :
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ E) := by
  rcases hfa : Ty.flattenArrow pty with ⟨doms, res⟩
  have hres2 : (Ty.flattenArrow pty).2 = res := by rw [hfa]
  cases efuel with
  | zero => rw [Eval.evalBuiltin] at hev; exact error_ne_ok hev
  | succ efuel =>
  rw [Eval.evalBuiltin] at hev
  rw [hfa] at hev
  cases b <;> try (dsimp only [cprim] at hc; exact error_ne_ok hc)
  case bits =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        dsimp only [cprim] at hc <;> try exact error_ne_ok hc
      rw [hres2] at hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      obtain ⟨k128, hk, hc⟩ := except_bind_eq_ok hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i h128
      rw [except_pure_def] at hc
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      have hva : v1 = .integer x := by
        cases v1 <;> rw [Eval.intVal] at hx <;>
          first
            | (injection hx with hx; rw [hx])
            | exact error_ne_ok hx
      obtain ⟨ka, hka⟩ := h0.2
      rw [hva] at hka
      have haev : a.eval σ E = ⟨128, x⟩ := by
        cases ka with
        | zero => rw [Val.rep] at hka; exact error_ne_ok hka
        | succ ka =>
            rw [Val.rep] at hka
            injection hka with hka
            rw [← hka]
      obtain ⟨n', te', hfr, hbr, hnr⟩ := vecBoolLen_inv hk
      constructor
      · exact vty_bitsToVec hΔ hfr hnr hbr (by simpa using h128.symm)
      · refine ⟨3, ?_⟩
        rw [haev]
        exact rep_bitsToVec hΔ ⟨128, x⟩ 0
  case xnor =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b', tb⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        dsimp only [cprim] at hc <;> try exact error_ne_ok hc
      rw [hres2] at hc
      obtain ⟨v1, v2, rfl⟩ := list_len2 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      have h1 := hargs 1 (by simp) (by simp)
      simp only [List.getElem_cons_zero, List.getElem_cons_succ] at h0 h1
      dsimp only at hev
      obtain ⟨m, hm, hc⟩ := except_bind_eq_ok hc
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hweq
      rw [except_pure_def] at hc
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
      obtain ⟨y, hy, hev⟩ := except_bind_eq_ok hev
      obtain ⟨r, hr, hev⟩ := except_bind_eq_ok hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      obtain ⟨ka, hka⟩ := h0.2
      obtain ⟨kb, hkb⟩ := h1.2
      have hxa : x = a.eval σ E := rep_det hx hka
      have hyb : y = b'.eval σ E := rep_det hy hkb
      subst hxa; subst hyb
      have hrxor : Rwv.Hyle.Sem.evalOp .xor [a.eval σ E, b'.eval σ E]
          = .ok ⟨(a.eval σ E).width,
                 (a.eval σ E).bits ^^^ ((b'.eval σ E).bits.setWidth (a.eval σ E).width)⟩ := rfl
      rw [hrxor] at hr
      injection hr with hr
      subst hr
      obtain ⟨n', te', hfda, hba, hna⟩ := vecBoolLen_inv hwa
      obtain ⟨n'', te'', hfr, hbr, hnr⟩ := vecBoolLen_inv hm
      have hxw : (a.eval σ E).width = wa :=
        vty_vecBool_rep_width hΔ hfda hna hba h0.1 hka
      constructor
      · exact vty_bitsToVec hΔ hfr hnr hbr (by simp only [BV.width]; omega)
      · refine ⟨3, ?_⟩
        have hnf : (NF.prim1 .not (NF.prim2 .xor a b')).eval σ E
            = (⟨(a.eval σ E).width,
                ~~~((a.eval σ E).bits ^^^ ((b'.eval σ E).bits.setWidth (a.eval σ E).width))⟩ : BV) := by
          simp only [NF.eval, hrxor]
          rfl
        rw [hnf]
        exact rep_bitsToVec hΔ _ 0
  case not =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        dsimp only [cprim] at hc <;> try exact error_ne_ok hc
      rw [hres2] at hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      obtain ⟨m, hm, hc⟩ := except_bind_eq_ok hc
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hweq
      rw [except_pure_def] at hc
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      obtain ⟨ka, hka⟩ := h0.2
      have hxa : x = a.eval σ E := rep_det hx hka
      subst hxa
      obtain ⟨n', te', hfda, hba, hna⟩ := vecBoolLen_inv hwa
      obtain ⟨n'', te'', hfr, hbr, hnr⟩ := vecBoolLen_inv hm
      have hxw : (a.eval σ E).width = wa :=
        vty_vecBool_rep_width hΔ hfda hna hba h0.1 hka
      constructor
      · exact vty_bitsToVec hΔ hfr hnr hbr (by simp only [BV.width]; omega)
      · refine ⟨3, ?_⟩
        have hnf : (NF.prim1 .not a).eval σ E
            = (⟨(a.eval σ E).width, ~~~(a.eval σ E).bits⟩ : BV) := by
          simp only [NF.eval]
          rfl
        rw [hnf]
        exact rep_bitsToVec hΔ _ 0
  case lAnd =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b', tb⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        dsimp only [cprim] at hc <;> try exact error_ne_ok hc
      rw [hres2] at hc
      obtain ⟨v1, v2, rfl⟩ := list_len2 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      have h1 := hargs 1 (by simp) (by simp)
      simp only [List.getElem_cons_zero, List.getElem_cons_succ] at h0 h1
      dsimp only at hev
      rw [cmpRow] at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hres
      rw [except_pure_def] at hc
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
      obtain ⟨y, hy, hev⟩ := except_bind_eq_ok hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      obtain ⟨ka, hka⟩ := h0.2
      obtain ⟨kb, hkb⟩ := h1.2
      have hxa : x = a.eval σ E := rep_det hx hka
      have hyb : y = b'.eval σ E := rep_det hy hkb
      subst hxa; subst hyb
      have hval : Eval.boolVal (decide ((a.eval σ E).nat ≠ 0 ∧ (b'.eval σ E).nat ≠ 0))
          = Eval.boolVal (((a.eval σ E).bits != 0) && ((b'.eval σ E).bits != 0)) := by
        rw [decide_and', bits_ne_nat, bits_ne_nat]
      rw [hval]
      constructor
      · exact vty_bool_at hΔ hres _
      · refine ⟨2, ?_⟩
        have hnf : (NF.prim2 .and (NF.prim1 .redor a) (NF.prim1 .redor b')).eval σ E
            = Rwv.Hyle.Sem.b1 (((a.eval σ E).bits != 0) && ((b'.eval σ E).bits != 0)) := by
          simp only [NF.eval, redor_b1, and_b1]
        rw [hnf]
        exact rep_boolVal hΔ _ 0
  case lOr =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b', tb⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        dsimp only [cprim] at hc <;> try exact error_ne_ok hc
      rw [hres2] at hc
      obtain ⟨v1, v2, rfl⟩ := list_len2 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      have h1 := hargs 1 (by simp) (by simp)
      simp only [List.getElem_cons_zero, List.getElem_cons_succ] at h0 h1
      dsimp only at hev
      rw [cmpRow] at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hres
      rw [except_pure_def] at hc
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
      obtain ⟨y, hy, hev⟩ := except_bind_eq_ok hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      obtain ⟨ka, hka⟩ := h0.2
      obtain ⟨kb, hkb⟩ := h1.2
      have hxa : x = a.eval σ E := rep_det hx hka
      have hyb : y = b'.eval σ E := rep_det hy hkb
      subst hxa; subst hyb
      have hval : Eval.boolVal (decide ((a.eval σ E).nat ≠ 0 ∨ (b'.eval σ E).nat ≠ 0))
          = Eval.boolVal (((a.eval σ E).bits != 0) || ((b'.eval σ E).bits != 0)) := by
        rw [decide_or', bits_ne_nat, bits_ne_nat]
      rw [hval]
      constructor
      · exact vty_bool_at hΔ hres _
      · refine ⟨2, ?_⟩
        have hnf : (NF.prim2 .or (NF.prim1 .redor a) (NF.prim1 .redor b')).eval σ E
            = Rwv.Hyle.Sem.b1 (((a.eval σ E).bits != 0) || ((b'.eval σ E).bits != 0)) := by
          simp only [NF.eval, redor_b1, or_b1]
        rw [hnf]
        exact rep_boolVal hΔ _ 0
  case lNot =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        dsimp only [cprim] at hc <;> try exact error_ne_ok hc
      rw [hres2] at hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      rw [redRow] at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hres
      rw [except_pure_def] at hc
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      obtain ⟨ka, hka⟩ := h0.2
      have hxa : x = a.eval σ E := rep_det hx hka
      subst hxa
      have hval : Eval.boolVal (decide ((a.eval σ E).nat = 0))
          = Eval.boolVal (!((a.eval σ E).bits != 0)) := by
        congr 1
        rw [bits_ne_nat]
        by_cases h : (a.eval σ E).nat = 0 <;> simp [h]
      rw [hval]
      constructor
      · exact vty_bool_at hΔ hres _
      · refine ⟨2, ?_⟩
        have hnf : (NF.prim1 .not (NF.prim1 .redor a)).eval σ E
            = Rwv.Hyle.Sem.b1 (!((a.eval σ E).bits != 0)) := by
          simp only [NF.eval, redor_b1, not_b1]
        rw [hnf]
        exact rep_boolVal hΔ _ 0
  case resize =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        dsimp only [cprim] at hc <;> try exact error_ne_ok hc
      rw [hres2] at hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      obtain ⟨m, hm, hc⟩ := except_bind_eq_ok hc
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      obtain ⟨m', hm', hev⟩ := except_bind_eq_ok hev
      obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      obtain ⟨ka, hka⟩ := h0.2
      have hxa : x = a.eval σ E := rep_det hx hka
      subst hxa
      obtain ⟨n', te', hfda, hba, hna⟩ := vecBoolLen_inv hwa
      obtain ⟨n'', te'', hfr, hbr, hnr⟩ := vecBoolLen_inv hm
      obtain ⟨n3, te3, hfr', hnr'⟩ := vecLen_inv hm'
      have hmm : m' = m := by
        rw [hfr] at hfr'
        have hn3 : n'' = n3 := by
          have := hfr'
          simp only [Prod.mk.injEq, List.cons.injEq] at this
          exact this.2.1
        subst hn3
        rw [hnr] at hnr'
        injection hnr' with hmmv
        exact hmmv.symm
      subst hmm
      have hxw : (a.eval σ E).width = wa :=
        vty_vecBool_rep_width hΔ hfda hna hba h0.1 hka
      split at hc
      · rename_i hmwa
        rw [except_pure_def] at hc
        injection hc with hc
        injection hc with hnf hty
        subst hnf; subst hty
        constructor
        · exact vty_bitsToVec hΔ hfr hnr hbr rfl
        · refine ⟨3, ?_⟩
          have hbv : (⟨m', (a.eval σ E).bits.setWidth m'⟩ : BV) = a.eval σ E := by
            refine bv_ext (show m' = (a.eval σ E).width by omega) ?_
            intro i
            show ((a.eval σ E).bits.setWidth m').getLsbD i = _
            rw [BitVec.getLsbD_setWidth]
            by_cases hi : i < m'
            · simp [hi]
            · rw [decide_eq_false hi, Bool.false_and,
                  getLsbD_ge (a.eval σ E).bits (by omega)]
          rw [hbv]
          exact rep_bitsToVec hΔ (a.eval σ E) 0
      · split at hc
        · rename_i hne hlt
          rw [except_pure_def] at hc
          injection hc with hc
          injection hc with hnf hty
          subst hnf; subst hty
          constructor
          · exact vty_bitsToVec hΔ hfr hnr hbr rfl
          · refine ⟨3, ?_⟩
            have hnf : (NF.prim1 (.zext m') a).eval σ E
                = (⟨m', (a.eval σ E).bits.setWidth m'⟩ : BV) := by
              simp only [NF.eval]
              rfl
            rw [hnf]
            exact rep_bitsToVec hΔ _ 0
        · rename_i hne hge
          rw [except_pure_def] at hc
          injection hc with hc
          injection hc with hnf hty
          subst hnf; subst hty
          constructor
          · exact vty_bitsToVec hΔ hfr hnr hbr rfl
          · refine ⟨3, ?_⟩
            have hnf : (NF.prim1 (.trunc m') a).eval σ E
                = (⟨m', (a.eval σ E).bits.setWidth m'⟩ : BV) := by
              simp only [NF.eval]
              rfl
            rw [hnf]
            exact rep_bitsToVec hΔ _ 0
  case msBit =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        dsimp only [cprim] at hc <;> try exact error_ne_ok hc
      rw [hres2] at hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hres
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hwa1
      rw [except_pure_def] at hc
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      split at hev
      rotate_left
      · exact error_ne_ok hev
      · exact error_ne_ok hev
      rename_i h' rest
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      obtain ⟨n', te', hfda, hba, hna⟩ := vecBoolLen_inv hwa
      obtain ⟨ka, hka⟩ := h0.2
      have hva := h0.1
      cases hva with
      | vec hfl' hn' hlen' helems =>
          rename_i n'' te'' kk
          rw [hfda] at hfl'
          have hp : n' = n'' ∧ te' = te'' := by simpa using hfl'
          obtain ⟨hp1, hp2⟩ := hp
          subst hp1; subst hp2
          rw [hna] at hn'
          injection hn' with hn'
          subst hn'
          cases ka with
          | zero => rw [Val.rep] at hka; exact error_ne_ok hka
          | succ ka =>
              rw [Val.rep] at hka
              obtain ⟨bs, hbs, hka⟩ := except_bind_eq_ok hka
              rw [mapM_attach_erase] at hbs
              rw [except_pure_def] at hka
              injection hka with hka
              rw [List.mapM_cons] at hbs
              obtain ⟨bh, hbh, hbs⟩ := except_bind_eq_ok hbs
              obtain ⟨brest, hbrest, hbs⟩ := except_bind_eq_ok hbs
              rw [except_pure_def] at hbs
              injection hbs with hbs
              subst hbs
              -- each element of the vector is a Bool, so each piece
              -- has width one
              have hwidths : ∀ x ∈ brest, x.width = 1 := by
                intro x hx
                obtain ⟨j, hj, hxj⟩ := List.getElem_of_mem hx
                obtain ⟨hblen', hptr⟩ := mapM_ok_idx hbrest
                obtain ⟨hj', hrepj⟩ := hptr j (by omega)
                obtain ⟨bb, hbb⟩ := vty_bool_inv hΔ hba
                  (helems rest[j] (List.mem_cons_of_mem _ (List.getElem_mem _)))
                rw [hbb] at hrepj
                rw [← hxj]
                rw [rep_det hrepj (rep_boolVal hΔ bb 0)]
                rfl
              obtain ⟨bh', hbh'⟩ := vty_bool_inv hΔ hba (helems h' List.mem_cons_self)
              have hbhw : bh.width = 1 := by
                rw [hbh'] at hbh
                rw [rep_det hbh (rep_boolVal hΔ bh' 0)]
                rfl
              -- slice the head out of the concatenation
              have hslice : sliceBV (a.eval σ E) (wa - 1) 1 = bh := by
                rw [← hka, bvConcat_eq]
                have hsum : ((brest.map (·.width)).sum) = wa - 1 := by
                  rw [sum_const (c := 1) (by
                    intro x hx
                    obtain ⟨y, hy, hxy⟩ := List.mem_map.mp hx
                    rw [← hxy]
                    exact hwidths y hy)]
                  have : brest.length = rest.length := (mapM_ok_idx hbrest).1
                  simp only [List.length_map, this]
                  have : (h' :: rest).length = wa := by rw [hlen']
                  simp only [List.length_cons] at this
                  omega
                rw [← hsum, ← hbhw]
                exact catAll_extract [] brest bh
              constructor
              · rw [teq_eq (show teq res boolT = true by simpa [isBoolT] using hres)]
                rw [hbh']
                exact vty_boolVal hΔ bh'
              · refine ⟨ka, ?_⟩
                have hnf : (NF.slice (wa - 1) 1 a).eval σ E = sliceBV (a.eval σ E) (wa - 1) 1 := rfl
                rw [hnf, hslice]
                exact hbh
  case add | sub | mul | div | mod | pow | and | or | xor | lShift | rShift | rShiftArith =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b', tb⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        dsimp only [cprim] at hc <;> try exact error_ne_ok hc
      rw [hres2] at hc
      obtain ⟨v1, v2, rfl⟩ := list_len2 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      have h1 := hargs 1 (by simp) (by simp)
      simp only [List.getElem_cons_zero, List.getElem_cons_succ] at h0 h1
      dsimp only at hev
      exact arithRow_sound hΔ (fun x y => ⟨_, rfl⟩) hc h0.1 h0.2 h1.2 hev
  case eq | gt | gtEq | lt | ltEq =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b', tb⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        dsimp only [cprim] at hc <;> try exact error_ne_ok hc
      rw [hres2] at hc
      obtain ⟨v1, v2, rfl⟩ := list_len2 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      have h1 := hargs 1 (by simp) (by simp)
      simp only [List.getElem_cons_zero, List.getElem_cons_succ] at h0 h1
      dsimp only at hev
      exact cmpRow_sound hΔ (fun x y => ⟨_, rfl⟩) hc h0.2 h1.2 hev
  case rAnd | rNAnd | rOr | rNor | rXOr | rXNor =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        dsimp only [cprim] at hc <;> try exact error_ne_ok hc
      rw [hres2] at hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      exact redRow_sound hΔ (fun x => ⟨_, rfl⟩) hc h0.2 hev

/-! ## Evaluation of the construction helpers -/

private theorem catList_eval (σ : String → BV) :
    ∀ (xs : List NF), (catList xs).eval σ E = catAll (xs.map (NF.eval σ E)) := by
  intro xs
  match xs with
  | [] => rfl
  | [x] =>
      show x.eval σ E = catAll [x.eval σ E]
      rw [catAll_cons, catAll_nil, bvCat_zero_right rfl]
  | x :: y :: rest =>
      show bvCat (x.eval σ E) ((catList (y :: rest)).eval σ E) = _
      rw [catList_eval σ (y :: rest)]
      rw [show (x :: y :: rest).map (NF.eval σ E) = x.eval σ E :: (y :: rest).map (NF.eval σ E)
            from rfl, catAll_cons]

private theorem catNF_eval (σ : String → BV)
    (xs : List (NF × Nat)) (hw : ∀ p ∈ xs, (p.1.eval σ E).width = p.2) :
    (catNF xs).eval σ E = catAll (xs.map (fun p => p.1.eval σ E)) := by
  rw [catNF, catList_eval, List.map_map]
  induction xs with
  | nil => rfl
  | cons x rest ih =>
      rw [List.filter_cons]
      by_cases hx : (x.2 != 0) = true
      · rw [if_pos hx, List.map_cons, List.map_cons, catAll_cons, catAll_cons,
            ih (fun a ha => hw a (List.mem_cons_of_mem _ ha))]
        rfl
      · rw [if_neg hx, List.map_cons, catAll_cons,
            ih (fun a ha => hw a (List.mem_cons_of_mem _ ha))]
        refine (bvCat_zero_left ?_).symm
        show (x.1.eval σ E).width = 0
        rw [hw x List.mem_cons_self]
        simpa using hx

private theorem sliceNF_eval (σ : String → BV) (off w : Nat) (e : NF) :
    (sliceNF off w e).eval σ E = sliceBV (e.eval σ E) off w := by
  rw [sliceNF]
  by_cases hw : w = 0
  · rw [if_pos hw]
    subst hw
    refine bv_ext rfl ?_
    intro i
    rw [sliceBV_getLsbD]
    simp [NF.eval, BV.nil, BV.ofNat]
  · rw [if_neg hw]
    rfl


/-! ## Foreign builtins are outside both row tables

The evaluator dispatches `rwPrimCryptol`/`rwPrimExtern` to the foreign
rows (Eval decision note 9) BEFORE the generic argument-evaluation
path, so the soundness proofs' prim cases must first pin the builtin
away from the foreign guards; these inversions supply the
contradiction from the compiler side (the 4a row table has no foreign
rows, and `cprimF` delegates the foreign builtins to it). -/

private theorem cprim_cry_err {pty : Ty} {pas : List (NF × Ty)} {r : NF × Ty}
    (h : cprim pty .cryptol pas = .ok r) : False := by
  rcases pas with _ | ⟨p, _ | ⟨q, _ | ⟨w, ws⟩⟩⟩ <;> exact error_ne_ok h

private theorem cprim_ext_err {pty : Ty} {pas : List (NF × Ty)} {r : NF × Ty}
    (h : cprim pty .«extern» pas = .ok r) : False := by
  rcases pas with _ | ⟨p, _ | ⟨q, _ | ⟨w, ws⟩⟩⟩ <;> exact error_ne_ok h

/-! ## The soundness theorem -/

/-- The main induction hypothesis, packaged: soundness of `cexp` at a
fixed fuel. -/
private abbrev SoundAt (Δ : DEnv) (dmap : HashMap Int Defn) (σ : String → BV) (fuel : Nat) : Prop :=
  ∀ (Γ : HashMap Int (NF × Ty)) (e : Exp) (nf : NF) (ty : Ty) (efuel : Nat)
    (env : Eval.Env) (jenv : Eval.JEnv) (v : Val),
    cexp Δ dmap fuel Γ e = .ok (nf, ty) →
    Eval.evalCore ⟨Δ, dmap⟩ efuel env jenv e E = .ok v →
    EnvC (E := E) Δ σ Γ env →
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ E)

/-- Sum of a suffix plus the element is at most the total. -/
private theorem drop_sum_le {l : List Nat} {i : Nat} (h : i < l.length) :
    (l.drop (i + 1)).sum + l[i] ≤ l.sum := by
  have hsplit : l.sum = (l.take i).sum + (l[i] :: l.drop (i + 1)).sum := by
    rw [List.getElem_cons_drop h, ← List.sum_append, List.take_append_drop]
  rw [hsplit, List.sum_cons]
  omega

private theorem nbits_zero {n : Nat} (h : nbits n = 0) : n ≤ 1 := by
  rw [nbits] at h
  by_cases hn : n ≤ 1
  · exact hn
  · rw [if_neg hn] at h
    omega

private theorem ofNat_beq_true {w a b : Nat} (ha : a < 2 ^ w) (hb : b < 2 ^ w)
    (h : (BitVec.ofNat w a == BitVec.ofNat w b) = true) : a = b := by
  have h2 : BitVec.ofNat w a = BitVec.ofNat w b := beq_iff_eq.mp h
  have h3 := congrArg BitVec.toNat h2
  rw [BitVec.toNat_ofNat, BitVec.toNat_ofNat, Nat.mod_eq_of_lt ha, Nat.mod_eq_of_lt hb] at h3
  exact h3

private theorem ofNat_beq_false {w a b : Nat} (hne : a ≠ b) (ha : a < 2 ^ w) (hb : b < 2 ^ w) :
    (BitVec.ofNat w a == BitVec.ofNat w b) = false := by
  cases hh : (BitVec.ofNat w a == BitVec.ofNat w b) with
  | false => rfl
  | true => exact absurd (ofNat_beq_true ha hb hh) hne

private theorem ite_eval_of_cond {σ : String → BV} {c t e : NF} {C : Bool}
    (hc : c.eval σ E = Rwv.Hyle.Sem.b1 C) :
    (NF.ite c t e).eval σ E = if C then t.eval σ E else e.eval σ E := by
  show (if (c.eval σ E).nat ≠ 0 then t.eval σ E else e.eval σ E) = _
  rw [hc]
  cases C with
  | true => rw [if_pos (by decide), if_pos rfl]
  | false => rw [if_neg (by decide), if_neg (by simp)]

set_option maxHeartbeats 8000000 in
/-- The chain lemma: with the scrutinee facts in hand, the compiled
if-chain agrees with `tryAlts`' selection. -/
private theorem cchain_sound {Δ : DEnv} {dmap : HashMap Int Defn} {σ : String → BV}
    (hΔ : denvOk Δ = true) {fuel : Nat} (IH : SoundAt (E := E) Δ dmap σ fuel)
    {Γ' : HashMap Int (NF × Ty)} {env : Eval.Env} {jenv : Eval.JEnv}
    {binder : Id} {dn : NF} {dty : Ty} {szT : Nat} {resTy : Ty} {sv : Val}
    (hΓ' : EnvC (E := E) Δ σ Γ' ((binder.uniq, sv) :: env))
    (hsz : Δ.sizeOf (fuel + 1) [] dty = .ok szT)
    (hvty : VTy Δ sv dty)
    {ks : Nat} (hks : Val.rep Δ ks sv = .ok (dn.eval σ E)) :
    ∀ (rest : List Alt) (macc : Option NF) (dflt : Option Alt) (out : NF × Ty)
      (ef2 : Nat) (vout : Val),
      cchain Δ dmap fuel Γ' dty szT dn resTy rest macc = .ok out →
      Eval.tryAlts ⟨Δ, dmap⟩ ef2 env jenv binder sv rest dflt E = .ok vout →
      ((macc = none ∧ dflt = none) ∨
       (∃ els c bs dbody, macc = some els ∧ dflt = some (Alt.mk c bs dbody) ∧
          cexp Δ dmap fuel Γ' dbody = .ok (els, resTy))) →
      out.2 = resTy ∧ VTy Δ vout resTy ∧ ∃ k, Val.rep Δ k vout = .ok (out.1.eval σ E) := by
  -- The per-alternative step.
  have hstep : ∀ (con : AltCon) (xs : List Id) (body : Exp) (macc : Option NF) (bnf : NF)
      (restE : List Alt) (dflt : Option Alt) (ef3 : Nat) (vout : Val),
      cAlt Δ dmap fuel Γ' dty szT dn resTy (.mk con xs body) macc = .ok bnf →
      Eval.tryAlts ⟨Δ, dmap⟩ (ef3 + 1) env jenv binder sv (.mk con xs body :: restE) dflt E
        = .ok vout →
      (macc = none → restE = [] ∧ dflt = none) →
      (∀ acc, macc = some acc →
         Eval.tryAlts ⟨Δ, dmap⟩ ef3 env jenv binder sv restE dflt E = .ok vout →
         VTy Δ vout resTy ∧ ∃ k, Val.rep Δ k vout = .ok (acc.eval σ E)) →
      VTy Δ vout resTy ∧ ∃ k, Val.rep Δ k vout = .ok (bnf.eval σ E) := by
    intro con xs body macc bnf restE dflt ef3 vout hca hev hnone hcont
    cases con with
    | default => rw [cAlt] at hca; exact error_ne_ok hca
    | litAlt i =>
        rw [cAlt] at hca
        obtain ⟨bt, hbt, hca⟩ := except_bind_eq_ok hca
        obtain ⟨bnf', bty⟩ := bt
        dsimp only at hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        rename_i hteq
        rw [teq_eq hteq] at hbt
        rw [Eval.tryAlts] at hev
        try dsimp only at hev
        obtain ⟨bm, hbm, hev⟩ := except_bind_eq_ok hev
        rw [Eval.litMatches] at hbm
        obtain ⟨x, hx, hbm⟩ := except_bind_eq_ok hbm
        rw [except_pure_def] at hbm
        injection hbm with hbm
        have hxd : x = dn.eval σ E := rep_det hx hks
        subst hxd
        subst hbm
        have hw : (dn.eval σ E).width = szT := vty_rep_width hvty hks hsz
        have hcondv : (NF.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT i⟩)).eval σ E
            = Rwv.Hyle.Sem.b1 ((dn.eval σ E).bits == BitVec.ofInt (dn.eval σ E).width i) := by
          rw [show (NF.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT i⟩)).eval σ E
                = Rwv.Hyle.Sem.b1 ((dn.eval σ E).bits ==
                    (BitVec.ofInt szT i).setWidth (dn.eval σ E).width) from rfl]
          rw [show (BitVec.ofInt szT i).setWidth (dn.eval σ E).width
                = BitVec.ofInt (dn.eval σ E).width i by
              rw [hw]
              exact BitVec.setWidth_eq _]
        cases hbm2 : ((dn.eval σ E).bits == BitVec.ofInt (dn.eval σ E).width i) with
        | true =>
            rw [hbm2] at hev
            try dsimp only at hev
            have hmain := IH Γ' body bnf' resTy ef3 ((binder.uniq, sv) :: env) jenv vout hbt hev hΓ'
            refine ⟨hmain.1, ?_⟩
            obtain ⟨k, hk⟩ := hmain.2
            refine ⟨k, ?_⟩
            rw [hk]
            congr 1
            cases macc with
            | none =>
                try dsimp only at hca
                injection hca with hca
                rw [← hca]
            | some acc =>
                try dsimp only at hca
                injection hca with hca
                rw [← hca, ite_eval_of_cond (C := true) (by rw [hcondv, hbm2])]
                rw [if_pos rfl]
        | false =>
            rw [hbm2] at hev
            try dsimp only at hev
            cases macc with
            | none =>
                obtain ⟨hre, hdf⟩ := hnone rfl
                subst hre; subst hdf
                cases ef3 with
                | zero => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
                | succ ef4 => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
            | some acc =>
                obtain ⟨hvt, k, hk⟩ := hcont acc rfl hev
                refine ⟨hvt, k, ?_⟩
                rw [hk]
                congr 1
                try dsimp only at hca
                injection hca with hca
                rw [← hca, ite_eval_of_cond (C := false) (by rw [hcondv, hbm2])]
                rw [if_neg (by simp)]
    | dataAlt cn =>
        rw [cAlt] at hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        rename_i hctb
        obtain ⟨tg, htagn, hca⟩ := except_bind_eq_ok hca
        obtain ⟨tag, w⟩ := tg
        cases hcs2 : Δ.ctorSig.get? cn with
        | none => rw [hcs2] at hca; dsimp only at hca; exact error_ne_ok hca
        | some sig2 =>
        rw [hcs2] at hca
        dsimp only at hca
        obtain ⟨sub2, hsub2, hca⟩ := except_bind_eq_ok hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        rename_i hxlen
        obtain ⟨szXs, hszXs, hca⟩ := except_bind_eq_ok hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        rename_i hwle
        obtain ⟨bt, hbt, hca⟩ := except_bind_eq_ok hca
        obtain ⟨bnf', bty⟩ := bt
        dsimp only at hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        rename_i hteq
        rw [teq_eq hteq] at hbt
        -- the evaluator's step: the scrutinee must be a constructor value
        rw [Eval.tryAlts.eq_def] at hev
        try dsimp only at hev
        split at hev
        rotate_left
        all_goals try exact error_ne_ok hev
        rename_i svty cv fields
        -- canonicality pins the carried type and the field facts
        cases hvty with
        | con hsigv hmatchv hlenv hctorv hfieldsv =>
        rename_i sigv subv
        -- the representation, dissected
        obtain ⟨ks', whole, tagv, tagWv, bsR, hks1, hwhole, htagv, hbs, hguard, hbv⟩ :=
          rep_con_inv hks
        have hwhsz : szT = whole := (sizeOf_det hwhole hsz).symm
        subst hwhsz
        -- the two constructors live in the same head datatype
        obtain ⟨tc, argsT, hflT, hdisj⟩ := ctorTag_inv htagn
        obtain ⟨tc', argsT', hflT', hdisj'⟩ := ctorTag_inv htagv
        rw [hflT] at hflT'
        have htcc : tc = tc' := by
          have := congrArg Prod.fst hflT'
          simpa using this
        subst htcc
        -- names from ctorOf
        have hcnOf : ctorOf Δ dty cn := ctorOfB_sound hctb
        rw [ctorOf, hflT] at hcnOf hctorv
        dsimp only at hcnOf hctorv
        -- tag widths agree, and w = 0 forces the same constructor
        have hkey : tagWv = w ∧ (cn = cv ↔ tagv = tag) := by
          rcases hdisj with ⟨htup, htag0, hw0⟩ | ⟨htup, cs, hcs, hidxn, hwn⟩
          · rcases hdisj' with ⟨_, htagv0, hwv0⟩ | ⟨htup', _, _, _⟩
            · subst htag0; subst hw0; subst htagv0; subst hwv0
              rw [if_pos htup] at hcnOf hctorv
              exact ⟨rfl, ⟨fun _ => rfl, fun _ => hcnOf.trans hctorv.symm⟩⟩
            · exact absurd htup (by simp [htup'])
          · rcases hdisj' with ⟨htup', _, _⟩ | ⟨_, cs', hcs', hidxv, hwv⟩
            · exact absurd htup' (by simp [htup])
            · rw [hcs] at hcs'
              injection hcs' with hcs'
              subst hcs'
              subst hwn; subst hwv
              refine ⟨rfl, ?_, ?_⟩
              · intro hcc
                subst hcc
                rw [hidxn] at hidxv
                injection hidxv with h
                exact h.symm
              · intro htt
                exact (idxOf?_inj hidxv (htt ▸ hidxn)).symm
        obtain ⟨hww, hcniff⟩ := hkey
        subst hww
        -- the width of the whole representation
        have hwsz : (dn.eval σ E).width = szT := vty_rep_width (VTy.con hsigv hmatchv hlenv
          (by rw [ctorOf, hflT]; exact hctorv) hfieldsv) hks hsz
        -- the tag slice of the representation
        have hFw : (bvCat (⟨szT - tagWv - (Val.bvConcat bsR).width, 0⟩ : BV)
            (Val.bvConcat bsR)).width = szT - tagWv := by
          rw [bvCat_width]
          show szT - tagWv - (Val.bvConcat bsR).width + (Val.bvConcat bsR).width = szT - tagWv
          omega
        have hslice : sliceBV (dn.eval σ E) (szT - tagWv) tagWv
            = (⟨tagWv, BitVec.ofNat tagWv tagv⟩ : BV) := by
          rw [hbv, sliceBV_cat_high (Nat.le_of_eq hFw), hFw, Nat.sub_self]
          exact sliceBV_all _
        -- the compiled test's value
        have hcondv : ∀ (w0 : Nat), tagWv = w0 + 1 →
            (NF.prim2 .eq (sliceNF (szT - tagWv) tagWv dn)
              (.lit ⟨tagWv, BitVec.ofNat tagWv tag⟩)).eval σ E
            = Rwv.Hyle.Sem.b1 (BitVec.ofNat tagWv tagv == BitVec.ofNat tagWv tag) := by
          intro w0 hw0
          have h1 : (NF.prim2 .eq (sliceNF (szT - tagWv) tagWv dn)
                (.lit ⟨tagWv, BitVec.ofNat tagWv tag⟩)).eval σ E
              = Rwv.Hyle.Sem.b1
                  (((sliceNF (szT - tagWv) tagWv dn).eval σ E).bits ==
                    BitVec.setWidth ((sliceNF (szT - tagWv) tagWv dn).eval σ E).width
                      (BitVec.ofNat tagWv tag)) := rfl
          rw [h1, sliceNF_eval, hslice]
          rw [show BitVec.setWidth (⟨tagWv, BitVec.ofNat tagWv tagv⟩ : BV).width
                (BitVec.ofNat tagWv tag) = BitVec.ofNat tagWv tag from BitVec.setWidth_eq _]
        cases hcv : (cn == cv) with
        | true =>
            have hcneq : cn = cv := beq_iff_eq.mp hcv
            subst hcneq
            -- same constructor: the signatures and substitutions coincide
            rw [hcs2] at hsigv
            injection hsigv with hsigv
            subst hsigv
            rw [hsub2] at hmatchv
            injection hmatchv with hmatchv
            subst hmatchv
            rw [hcv] at hev
            try dsimp only at hev
            by_cases hblen' : xs.length = fields.length
            case neg =>
                rw [if_neg (show ¬ (xs.length == fields.length) = true by
                      simpa using hblen')] at hev
                exact error_ne_ok hev
            rw [if_pos (show (xs.length == fields.length) = true by
                  simpa using hblen')] at hev
            -- lengths
            obtain ⟨hszlen, hszpt⟩ := mapM_ok_idx hszXs
            rw [List.length_map] at hszlen
            obtain ⟨hbslen, hbspt⟩ := mapM_ok_idx hbs
            have hxleni : xs.length = ((Ty.flattenArrow sig2.ty).1.map (DEnv.substTv sub2)).length := hxlen
            rw [List.length_map] at hxleni
            -- the piece widths are the instantiated field sizes
            have hlistw : szXs = bsR.map (·.width) := by
              refine List.ext_getElem (by simp only [List.length_map]; omega) ?_
              intro i h1 h2
              obtain ⟨hi1, hszi⟩ := hszpt i (by rw [List.length_map]; omega)
              obtain ⟨hi2, hrepi⟩ := hbspt i (by omega)
              have hvf := hfieldsv (((Ty.flattenArrow sig2.ty).1.zip fields)[i]'(by
                simp only [List.length_zip]; omega)) (List.getElem_mem _)
              rw [List.getElem_zip] at hvf
              rw [List.getElem_map] at hszi
              simp only [List.getElem_map]
              exact (vty_rep_width hvf hrepi hszi).symm
            have hsumw : (Val.bvConcat bsR).width = szXs.sum := by
              rw [bvConcat_eq, catAll_width, hlistw]
            -- the branch environment corresponds
            have henv'' : EnvC (E := E) Δ σ
                (bindFieldsΓ xs
                  ((((szXs.zip ((List.range szXs.length).map fun i => (szXs.drop (i + 1)).sum)).map
                      fun p => sliceNF p.2 p.1 dn)).zip
                    ((Ty.flattenArrow sig2.ty).1.map (DEnv.substTv sub2))) Γ')
                (((xs.map (·.uniq)).zip fields) ++ (binder.uniq, Val.con dty cn fields) :: env) := by
              refine envC_bind xs _ fields hΓ' ?_ (by omega) ?_
              · simp only [List.length_zip, List.length_map, List.length_range]
                omega
              · intro i h1 h2 h3
                have hzl : i < ((szXs.zip ((List.range szXs.length).map fun i =>
                    (szXs.drop (i + 1)).sum)).map fun p => sliceNF p.2 p.1 dn).length := by
                  simp only [List.length_map, List.length_zip, List.length_range]
                  omega
                rw [List.getElem_zip]
                constructor
                · -- canonicality at the instantiated field type
                  have hvf := hfieldsv (((Ty.flattenArrow sig2.ty).1.zip fields)[i]'(by
                    simp only [List.length_zip]; omega)) (List.getElem_mem _)
                  rw [List.getElem_zip] at hvf
                  dsimp only
                  rw [List.getElem_map]
                  exact hvf
                · -- representation: the field slice of the scrutinee
                  obtain ⟨hi2, hrepi⟩ := hbspt i (by omega)
                  refine ⟨ks', ?_⟩
                  rw [hrepi]
                  congr 1
                  dsimp only
                  rw [List.getElem_map, List.getElem_zip, List.getElem_map, List.getElem_range,
                      sliceNF_eval]
                  -- extract piece i
                  have hoffw : (szXs.drop (i + 1)).sum
                      = ((bsR.drop (i + 1)).map (·.width)).sum := by
                    rw [hlistw]
                    congr 1
                    first
                      | rw [List.map_drop]
                      | rw [← List.map_drop]
                      | rw [List.drop_map]
                      | rw [← List.drop_map]
                  have hszw : szXs[i]'(by omega) = (bsR[i]'(by omega)).width := by
                    have h5 : szXs[i]'(by omega) = (bsR.map (·.width))[i]'(by
                        simp only [List.length_map]; omega) :=
                      List.getElem_of_eq hlistw _
                    rw [h5, List.getElem_map]
                  rw [hbv]
                  rw [sliceBV_cat_low (by
                    rw [bvCat_width, hsumw]
                    have := drop_sum_le (l := szXs) (i := i) (by omega)
                    omega)]
                  rw [sliceBV_cat_low (by
                    rw [hsumw]
                    have := drop_sum_le (l := szXs) (i := i) (by omega)
                    omega)]
                  dsimp only
                  rw [hoffw, hszw, bvConcat_eq]
                  rw [congrArg catAll (show bsR = bsR.take i ++ bsR[i]'(by omega) :: bsR.drop (i + 1) by
                    rw [List.getElem_cons_drop, List.take_append_drop])]
                  exact (catAll_extract (bsR.take i) (bsR.drop (i + 1)) (bsR[i]'(by omega))).symm
            have hmain := IH _ body bnf' resTy ef3 _ jenv vout hbt hev henv''
            refine ⟨hmain.1, ?_⟩
            obtain ⟨k, hk⟩ := hmain.2
            refine ⟨k, ?_⟩
            rw [hk]
            congr 1
            -- the compiled alternative takes its own branch
            cases macc with
            | none =>
                try dsimp only at hca
                injection hca with hca
                rw [← hca]
            | some acc =>
                cases htw : tagWv with
                | zero =>
                    rw [htw] at hca
                    try dsimp only at hca
                    injection hca with hca
                    rw [← hca]
                | succ w0 =>
                    rw [htw] at hca
                    try dsimp only at hca
                    injection hca with hca
                    rw [← hca]
                    have hcv2 := hcondv w0 htw
                    rw [htw] at hcv2
                    rw [ite_eval_of_cond (C := true) (by
                      rw [hcv2]
                      congr 1
                      rw [(hcniff.mp rfl), beq_self_eq_true])]
                    rw [if_pos rfl]
        | false =>
            have hcnne : cn ≠ cv := by
              intro hcc
              rw [hcc] at hcv
              rw [beq_self_eq_true] at hcv
              cases hcv
            rw [hcv] at hev
            try dsimp only at hev
            cases macc with
            | none =>
                obtain ⟨hre, hdf⟩ := hnone rfl
                subst hre; subst hdf
                cases ef3 with
                | zero => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
                | succ ef4 => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
            | some acc =>
                cases htw : tagWv with
                | zero =>
                    -- a zero-width tag forces a single constructor
                    exfalso
                    rcases hdisj with ⟨htup, _, _⟩ | ⟨htup, cs, hcs, hidxn, hwn⟩
                    · rcases hdisj' with ⟨_, _, _⟩ | ⟨htup', _, _, _⟩
                      · rw [if_pos htup] at hcnOf hctorv
                        exact hcnne (hcnOf.trans hctorv.symm)
                      · exact absurd htup (by simp [htup'])
                    · rcases hdisj' with ⟨htup', _, _⟩ | ⟨_, cs', hcs', hidxv, hwv⟩
                      · exact absurd htup' (by simp [htup])
                      · rw [hcs] at hcs'
                        injection hcs' with hcs'
                        subst hcs'
                        have hlen1 : cs.length ≤ 1 := nbits_zero (by omega)
                        have h1 := idxOf?_lt hidxn
                        have h2 := idxOf?_lt hidxv
                        have htt : tagv = tag := by omega
                        exact hcnne (hcniff.mpr htt)
                | succ w0 =>
                    rw [htw] at hca
                    try dsimp only at hca
                    injection hca with hca
                    obtain ⟨hvt, k, hk⟩ := hcont acc rfl hev
                    refine ⟨hvt, k, ?_⟩
                    rw [hk]
                    congr 1
                    rw [← hca]
                    have htagne : tagv ≠ tag := fun htt => hcnne (hcniff.mpr htt)
                    have hbounds : tagv < 2 ^ tagWv ∧ tag < 2 ^ tagWv := by
                      rcases hdisj with ⟨_, htag0, hw0⟩ | ⟨htup, cs, hcs, hidxn, hwn⟩
                      · omega
                      · rcases hdisj' with ⟨htup', _, _⟩ | ⟨_, cs', hcs', hidxv, hwv⟩
                        · exact absurd htup' (by simp [htup])
                        · rw [hcs] at hcs'
                          injection hcs' with hcs'
                          subst hcs'
                          constructor
                          · rw [hwv]
                            exact Nat.lt_of_lt_of_le (idxOf?_lt hidxv) (nbits_le _)
                          · rw [hwn]
                            exact Nat.lt_of_lt_of_le (idxOf?_lt hidxn) (nbits_le _)
                    have hcv2 := hcondv w0 htw
                    rw [htw] at hcv2
                    rw [ite_eval_of_cond (C := false) (by
                      rw [hcv2]
                      congr 1
                      have hb2 := hbounds
                      rw [htw] at hb2
                      exact ofNat_beq_false htagne hb2.1 hb2.2)]
                    rw [if_neg (by simp)]
  -- The chain induction.
  intro rest
  induction rest with
  | nil =>
      intro macc dflt out ef2 vout hcc hev hrel
      rcases hrel with ⟨hm, hd⟩ | ⟨els, c0, bs0, dbody, hm, hd, hdb⟩
      · subst hm
        rw [cchain] at hcc
        exact error_ne_ok hcc
      · subst hm; subst hd
        rw [cchain] at hcc
        injection hcc with hcc
        cases ef2 with
        | zero => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
        | succ ef3 =>
            rw [Eval.tryAlts] at hev
            try dsimp only at hev
            have hmain := IH Γ' dbody els resTy ef3 ((binder.uniq, sv) :: env) jenv vout hdb hev hΓ'
            refine ⟨by rw [← hcc], hmain.1, ?_⟩
            obtain ⟨k, hk⟩ := hmain.2
            exact ⟨k, by rw [← hcc, hk]⟩
  | cons alt restT ihR =>
      intro macc dflt out ef2 vout hcc hev hrel
      obtain ⟨con, xs, body⟩ := alt
      cases ef2 with
      | zero => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
      | succ ef3 =>
      rcases restT with _ | ⟨r2, rt⟩
      · rcases macc with _ | els
        · -- the unconditional last alternative
          rw [cchain] at hcc
          obtain ⟨bnf, hbnf, hcc⟩ := except_bind_eq_ok hcc
          injection hcc with hcc
          have hd0 : dflt = none := by
            rcases hrel with ⟨_, hd⟩ | ⟨_, _, _, _, hm, _, _⟩
            · exact hd
            · exact absurd hm (by simp)
          subst hd0
          have hstepped := hstep con xs body none bnf [] none ef3 vout hbnf hev
            (fun _ => ⟨rfl, rfl⟩) (fun acc hacc => absurd hacc (by simp))
          exact ⟨by rw [← hcc], hstepped.1, by rw [← hcc]; exact hstepped.2⟩
        · -- last conditional alternative, default fallback
          rw [cchain] at hcc
          obtain ⟨accp, haccp, hcc⟩ := except_bind_eq_ok hcc
          rw [cchain] at haccp
          injection haccp with haccp
          obtain ⟨accnf, accty⟩ := accp
          have haccnf : accnf = els := by
            have h6 := congrArg Prod.fst haccp
            simpa using h6.symm
          try dsimp only at hcc
          obtain ⟨bnf, hbnf, hcc⟩ := except_bind_eq_ok hcc
          injection hcc with hcc
          rw [haccnf] at hbnf
          rcases hrel with ⟨hm, _⟩ | ⟨els2, c0, bs0, dbody, hm, hd, hdb⟩
          · exact absurd hm (by simp)
          have hm2 : els = els2 := Option.some.inj hm
          subst hm2
          subst hd
          have hstepped := hstep con xs body (some els) bnf [] (some (Alt.mk c0 bs0 dbody))
            ef3 vout hbnf hev (fun h => absurd h (by simp))
            (fun acc hacc hev' => by
              injection hacc with hacc
              subst hacc
              cases ef3 with
              | zero => rw [Eval.tryAlts] at hev'; exact error_ne_ok hev'
              | succ ef4 =>
                  rw [Eval.tryAlts] at hev'
                  try dsimp only at hev'
                  exact IH Γ' dbody els resTy ef4 ((binder.uniq, sv) :: env) jenv vout hdb hev' hΓ')
          exact ⟨by rw [← hcc], hstepped.1, by rw [← hcc]; exact hstepped.2⟩
          all_goals (intros; simp_all)
      · -- an interior alternative
        rw [cchain] at hcc
        obtain ⟨accp, haccp, hcc⟩ := except_bind_eq_ok hcc
        obtain ⟨accnf, accty⟩ := accp
        try dsimp only at hcc
        obtain ⟨bnf, hbnf, hcc⟩ := except_bind_eq_ok hcc
        injection hcc with hcc
        have hstepped := hstep con xs body (some accnf) bnf (r2 :: rt) dflt ef3 vout hbnf hev
          (fun h => absurd h (by simp))
          (fun acc hacc hev' => by
            injection hacc with hacc
            subst hacc
            have := ihR macc dflt (accnf, accty) ef3 vout haccp hev' hrel
            exact ⟨this.2.1, this.2.2⟩)
        exact ⟨by rw [← hcc], hstepped.1, by rw [← hcc]; exact hstepped.2⟩
        all_goals (intro h1 h2; exact absurd h2 (by simp))

private theorem applyMany_nil_inv {C : Eval.Ctx} {k : Nat} {f v : Val}
    (h : Eval.applyMany C k f [] E = .ok v) : v = f := by
  cases k with
  | zero => rw [Eval.applyMany] at h; exact error_ne_ok h
  | succ k =>
      rw [Eval.applyMany] at h
      injection h with h
      exact h.symm

private theorem map_zip_fst {α β γ : Type} (f : α → γ) :
    ∀ (l : List α) (ws : List β), l.length = ws.length →
      (l.zip ws).map (fun p => f p.1) = l.map f := by
  intro l
  induction l with
  | nil => intro ws _; rfl
  | cons a as ih =>
      intro ws hl
      match ws with
      | [] => exact absurd hl (by simp)
      | w :: ws =>
          rw [List.zip_cons_cons, List.map_cons, List.map_cons, ih ws (by simpa using hl)]

set_option maxHeartbeats 8000000 in
/-- THE soundness theorem (rep-correspondence): a successful
compilation of the core fragment is faithful — whenever the committed
evaluator produces a value, the value is canonical at the synthesized
type and its bit representation is exactly the compiled normal form's
denotation. Side conditions: `denvOk` (the prim-basis Bool/Vec
discipline), `EnvC` (environment correspondence), and both sides
reading the same definition map. -/
theorem cexp_sound {Δ : DEnv} {dmap : HashMap Int Defn} {σ : String → BV}
    (hΔ : denvOk Δ = true) : ∀ (fuel : Nat), SoundAt (E := E) Δ dmap σ fuel := by
  intro fuel
  induction fuel with
  | zero =>
      intro Γ e nf ty efuel env jenv v hc
      rw [cexp] at hc
      exact error_ne_ok hc
  | succ fuel ih =>
      intro Γ e nf ty efuel env jenv v hc hev hΓ
      cases efuel with
      | zero => rw [Eval.evalCore] at hev; exact error_ne_ok hev
      | succ efuel =>
      rw [cexp] at hc
      rw [Eval.evalCore] at hev
      rcases hfl : Eval.flattenApp e with ⟨hd, args⟩
      rw [hfl] at hc hev
      clear hfl
      cases hd with
      | var x =>
          dsimp only at hc hev
          obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
          cases hΓx : Γ.get? x.uniq with
          | some nt =>
              rw [hΓx] at hc
              cases args with
              | nil =>
                  injection hc with hc
                  obtain ⟨w, hlook, hwty, k, hwrep⟩ := hΓ.fwd x.uniq nt hΓx
                  have hvs0 : vs = [] := by
                    obtain ⟨hlen, _⟩ := evalList_ok_idx hvs
                    exact List.length_eq_zero_iff.mp (by simpa using hlen)
                  subst hvs0
                  cases hL : List.lookup x.uniq env with
                  | none => rw [hlook] at hL; exact absurd hL (by simp)
                  | some w2 =>
                      rw [hL] at hev
                      dsimp only at hev
                      have hw2 : w2 = w := Option.some.inj (hL.symm.trans hlook)
                      subst hw2
                      have hv : v = w2 := applyMany_nil_inv hev
                      subst hv
                      subst hc
                      exact ⟨hwty, k, hwrep⟩
              | cons a as => exact error_ne_ok hc
          | none =>
              rw [hΓx] at hc
              cases hL : List.lookup x.uniq env with
              | some w0 =>
                  rw [hΓ.miss x.uniq hΓx] at hL
                  exact absurd hL (by simp)
              | none =>
              rw [hL] at hev
              dsimp only at hev
              cases hdm : dmap.get? x.uniq with
              | none => rw [hdm] at hc; exact error_ne_ok hc
              | some d =>
                  rw [hdm] at hc
                  dsimp only at hc
                  obtain ⟨pas, hpas, hc⟩ := except_bind_eq_ok hc
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i hteq
                  rw [hdm] at hev
                  dsimp only at hev
                  obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
                  obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
                  have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
                      VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
                    intro i h1 h2
                    obtain ⟨hia, hci⟩ := hpt i (by omega)
                    obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
                    exact ih Γ args[i] (pas[i].1) (pas[i].2) ki env jenv vs[i] hci hei hΓ
                  have hlen1 : pas.length = d.params.length := by
                    have := teqAll_length hteq
                    simpa using this
                  have hlen2 : vs.length = d.params.length := by omega
                  cases efuel with
                  | zero =>
                      rw [Eval.callDefn.eq_def] at hev
                      exact error_ne_ok hev
                  | succ ef2 =>
                      rw [Eval.callDefn.eq_def] at hev
                      dsimp only at hev
                      rw [if_neg (by omega)] at hev
                      obtain ⟨w, hbody, hev⟩ := except_bind_eq_ok hev
                      rw [show vs.drop d.params.length = [] by
                            rw [← hlen2, List.drop_length]] at hev
                      have hv : v = w := applyMany_nil_inv hev
                      subst hv
                      refine ih (mkGamma d.params pas) d.body nf ty ef2 _ [] v hc hbody ?_
                      have := envC_bind (Δ := Δ) (σ := σ) d.params pas vs
                        (Γ₀ := ∅) (env₀ := []) envC_empty hlen1 hlen2
                        (fun i h1 h2 h3 => hptw i h2 h3)
                      simpa [mkGamma] using this
      | con cty c =>
          dsimp only at hc hev
          rcases hfac : Ty.flattenArrow cty with ⟨dts, resTy⟩
          have hfac1 : (Ty.flattenArrow cty).1 = dts := by rw [hfac]
          have hfac2 : (Ty.flattenArrow cty).2 = resTy := by rw [hfac]
          rw [hfac1, hfac2] at hc
          rw [hfac] at hev
          dsimp only at hev
          obtain ⟨pas, hpas, hc⟩ := except_bind_eq_ok hc
          obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
          obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
          obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
          have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
              VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
            intro i h1 h2
            obtain ⟨hia, hci⟩ := hpt i (by omega)
            obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
            exact ih Γ args[i] (pas[i].1) (pas[i].2) ki env jenv vs[i] hci hei hΓ
          split at hc
          rotate_left
          · exact error_ne_ok hc
          rename_i hsat
          cases hcs : Δ.ctorSig.get? c with
          | none => rw [hcs] at hc; exact error_ne_ok hc
          | some sig =>
              rw [hcs] at hc
              dsimp only at hc
              obtain ⟨sub, hsub, hc⟩ := except_bind_eq_ok hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hteq
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hctb
              obtain ⟨whole, hwhole, hc⟩ := except_bind_eq_ok hc
              obtain ⟨tg, htag, hc⟩ := except_bind_eq_ok hc
              obtain ⟨tag, w⟩ := tg
              dsimp only at hc
              obtain ⟨ws, hws, hc⟩ := except_bind_eq_ok hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hle
              injection hc with hc
              injection hc with hnf hty
              subst hnf; subst hty
              split at hev
              rotate_left
              · exact error_ne_ok hev
              rename_i hsat2
              rw [except_pure_def] at hev
              injection hev with hev
              subst hev
              have hlent : pas.length = (Ty.flattenArrow sig.ty).1.length := by
                have h1 := teqAll_length hteq
                simpa using h1
              have hvpas : vs.length = pas.length := by
                have h2 : vs.length = dts.length := by simpa using hsat2
                omega
              have htys := teqAll_types hteq
              have hpasty : ∀ i (h : i < pas.length),
                  pas[i].2 = DEnv.substTv sub ((Ty.flattenArrow sig.ty).1[i]'(by omega)) := by
                intro i h
                have h1 : (pas.map (·.2))[i]'(by simpa using h)
                    = ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub))[i]'(by
                        rw [← htys]; simpa using h) :=
                  List.getElem_of_eq htys _
                simpa using h1
              obtain ⟨hwlen, hwpt⟩ := mapM_ok_idx hws
              rw [List.length_map] at hwlen
              have hwlen' : ws.length = pas.length := by omega
              have hwidths : ∀ i (h : i < pas.length),
                  (pas[i].1.eval σ E).width = ws[i]'(by omega) := by
                intro i h
                obtain ⟨hwi, hszi⟩ := hwpt i (by rw [List.length_map]; omega)
                obtain ⟨hv1, hex⟩ := hptw i h (by omega)
                obtain ⟨k1, hrep1⟩ := hex
                rw [hpasty i h] at hv1
                have hszi' : Δ.sizeOf (fuel + 1) []
                    (DEnv.substTv sub ((Ty.flattenArrow sig.ty).1[i]'(by omega))) = .ok (ws[i]'(by omega)) := by
                  have h2 := hszi
                  rw [List.getElem_map] at h2
                  exact h2
                exact vty_rep_width hv1 hrep1 hszi'
              have hlistw : (pas.map (fun p => p.1.eval σ E)).map (·.width) = ws := by
                refine List.ext_getElem (by simpa using by omega) ?_
                intro i h1 h2
                simp only [List.getElem_map]
                exact hwidths i (by simpa using h1)
              have hsum : (Val.bvConcat (pas.map fun p => p.1.eval σ E)).width = ws.sum := by
                rw [bvConcat_eq, catAll_width, hlistw]
              obtain ⟨K, hK⟩ := mapM_rep_exists (Δ := Δ) (vs := vs)
                (bs := pas.map (fun p => p.1.eval σ E)) (by simpa using hvpas)
                (fun i h1 h2 => by
                  obtain ⟨_, hex⟩ := hptw i (by omega) h1
                  obtain ⟨k1, hrep1⟩ := hex
                  exact ⟨k1, by simpa using hrep1⟩)
              constructor
              · refine VTy.con (sub := sub) hcs hsub (by omega) (ctorOfB_sound hctb) ?_
                intro p hp
                obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                have hj2 : j < pas.length := by
                  simp only [List.length_zip] at hj
                  omega
                rw [← hpj, List.getElem_zip]
                have hvj := (hptw j hj2 (by omega)).1
                rw [hpasty j hj2] at hvj
                exact hvj
              · refine ⟨max (fuel + 1) K + 1, ?_⟩
                have hle2 : w + (Val.bvConcat (pas.map fun p => p.1.eval σ E)).width ≤ whole := by
                  rw [hsum]
                  exact hle
                have hcon := rep_con_intro (K := max (fuel + 1) K)
                  (Δ.sizeOf_mono (by omega) hwhole) htag
                  (mapM_rep_mono (Nat.le_max_right _ _) hK) hle2
                refine hcon.trans ?_
                congr 1
                have hpwidths : ∀ p ∈ ((NF.lit ⟨w, BitVec.ofNat w tag⟩, w)
                    :: (NF.lit ⟨whole - w - ws.sum, 0⟩, whole - w - ws.sum)
                    :: (pas.map (·.1)).zip ws), (p.1.eval σ E).width = p.2 := by
                  intro p hp
                  rcases hp with _ | ⟨_, hp⟩
                  · rfl
                  rcases hp with _ | ⟨_, hp⟩
                  · rfl
                  obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                  have hj2 : j < pas.length := by
                    simp only [List.length_zip, List.length_map] at hj
                    omega
                  rw [← hpj, List.getElem_zip]
                  simp only [List.getElem_map]
                  exact hwidths j hj2
                rw [catNF_eval σ _ hpwidths]
                have hmaps : ((NF.lit (⟨w, BitVec.ofNat w tag⟩ : BV), w)
                    :: (NF.lit (⟨whole - w - ws.sum, 0⟩ : BV), whole - w - ws.sum)
                    :: (pas.map (·.1)).zip ws).map (fun p => p.1.eval σ E)
                    = (⟨w, BitVec.ofNat w tag⟩ : BV) :: (⟨whole - w - ws.sum, 0⟩ : BV)
                      :: (pas.map fun p => p.1.eval σ E) := by
                  rw [List.map_cons, List.map_cons,
                      map_zip_fst (NF.eval σ E) (pas.map (·.1)) ws
                        (by simp only [List.length_map]; omega),
                      List.map_map]
                  rfl
                rw [hmaps, catAll_cons, catAll_cons, ← bvConcat_eq, hsum]
      | prim pty b =>
          dsimp only at hc hev
          obtain ⟨pas, hpas, hc⟩ := except_bind_eq_ok hc
          have hbc : (b == .cryptol) = false := by
            cases hbceq : b == .cryptol
            · rfl
            · exfalso
              rw [beq_iff_eq] at hbceq
              subst hbceq
              exact cprim_cry_err hc
          have hbx : (b == .«extern») = false := by
            cases hbxeq : b == .«extern»
            · rfl
            · exfalso
              rw [beq_iff_eq] at hbxeq
              subst hbxeq
              exact cprim_ext_err hc
          rw [if_neg (by simp [hbc]), if_neg (by simp [hbx])] at hev
          obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
          obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
          obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
          have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
              VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
            intro i h1 h2
            obtain ⟨hia, hci⟩ := hpt i (by omega)
            obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
            exact ih Γ args[i] (pas[i].1) (pas[i].2) ki env jenv vs[i] hci hei hΓ
          exact cprim_sound hΔ hc hev (by omega) hptw
      | litInt tyL n =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil =>
              dsimp only at hc hev
              obtain ⟨hrty, hshape⟩ := clitInt_inv hc
              subst hrty
              rcases hshape with ⟨hfl2, hnf⟩ | ⟨bt, k, hfl2, hk, hnf⟩ | ⟨lt, et, w, hfl2, het, hlt, hnf⟩
              · subst hnf
                have hv := litIntVal_inv_integer hfl2 hev
                subst hv
                exact ⟨VTy.integer hfl2, 1, by rw [Val.rep]; rfl⟩
              · subst hnf
                obtain ⟨h0, hk2, hv⟩ := litIntVal_inv_finite hfl2 hk hev
                subst hv
                refine ⟨VTy.finite hfl2 hk (by omega), 1, ?_⟩
                rw [Val.rep]
                rw [show (NF.lit (⟨nbits k, BitVec.ofInt (nbits k) n⟩ : BV)).eval σ E
                      = (⟨nbits k, BitVec.ofInt (nbits k) n⟩ : BV) from rfl]
                rw [ofInt_nonneg h0]
                rfl
              · subst hnf
                have hv := litIntVal_inv_vec hfl2 hlt hev
                subst hv
                exact ⟨vty_bitsToVec hΔ hfl2 hlt het rfl, 3, rep_bitsToVec hΔ _ 0⟩
      | litVec vty es =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil =>
              dsimp only at hc hev
              obtain ⟨pas, hpas, hc⟩ := except_bind_eq_ok hc
              obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
              rw [except_pure_def] at hev
              injection hev with hev
              subst hev
              obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
              obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
              have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
                  VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
                intro i h1 h2
                obtain ⟨hia, hci⟩ := hpt i (by omega)
                obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
                exact ih Γ es[i] (pas[i].1) (pas[i].2) ki env jenv vs[i] hci hei hΓ
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i nt te heq
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i k hk
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hlenk
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hteq
              obtain ⟨se, hse, hc⟩ := except_bind_eq_ok hc
              injection hc with hc
              injection hc with hnf hty
              subst hnf; subst hty
              have htys := teqAll_types hteq
              have hpaste : ∀ i (h : i < pas.length), pas[i].2 = te := by
                intro i h
                have h1 : (pas.map (·.2))[i]'(by simpa using h)
                    = (List.replicate pas.length te)[i]'(by simpa using h) :=
                  List.getElem_of_eq htys _
                simpa using h1
              have hvtys : ∀ i (h : i < vs.length), VTy Δ vs[i] te := by
                intro i h
                have := (hptw i (by omega) h).1
                rw [hpaste i (by omega)] at this
                exact this
              constructor
              · refine VTy.vec heq hk (by omega) ?_
                intro e' he'
                obtain ⟨j, hj, hej⟩ := List.getElem_of_mem he'
                rw [← hej]
                exact hvtys j hj
              · obtain ⟨K, hK⟩ := mapM_rep_exists (Δ := Δ) (vs := vs)
                  (bs := pas.map (fun p => p.1.eval σ E)) (by simpa using by omega)
                  (fun i h1 h2 => by
                    obtain ⟨_, hex⟩ := hptw i (by omega) h1
                    obtain ⟨k1, hrep1⟩ := hex
                    exact ⟨k1, by simpa using hrep1⟩)
                refine ⟨K + 1, ?_⟩
                rw [Val.rep, mapM_attach_erase, hK, except_bind_ok, except_pure_def]
                congr 1
                have hpwidths : ∀ p ∈ (pas.map (·.1)).map (·, se), (p.1.eval σ E).width = p.2 := by
                  intro p hp
                  obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                  have hj2 : j < pas.length := by
                    simp only [List.length_map] at hj
                    omega
                  rw [← hpj]
                  simp only [List.getElem_map]
                  obtain ⟨hv1, hex⟩ := hptw j hj2 (by omega)
                  obtain ⟨k1, hrep1⟩ := hex
                  rw [hpaste j hj2] at hv1
                  exact vty_rep_width hv1 hrep1 hse
                rw [catNF_eval σ _ hpwidths, List.map_map, List.map_map, bvConcat_eq]
                rfl
      | cases resTy scrut binder alts =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil =>
              dsimp only at hc hev
              obtain ⟨dnt, hdn, hc⟩ := except_bind_eq_ok hc
              obtain ⟨dn, dty⟩ := dnt
              dsimp only at hc
              obtain ⟨szT, hsz, hc⟩ := except_bind_eq_ok hc
              obtain ⟨sv, hsv, hev⟩ := except_bind_eq_ok hev
              obtain ⟨v', hv', hev⟩ := except_bind_eq_ok hev
              obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
              have hvs0 : vs = [] := by
                obtain ⟨hlen, _⟩ := evalList_ok_idx hvs
                exact List.length_eq_zero_iff.mp (by simpa using hlen)
              subst hvs0
              have hv : v = v' := applyMany_nil_inv hev
              subst hv
              obtain ⟨hvty, ks, hks⟩ := ih Γ scrut dn dty efuel env jenv sv hdn hsv hΓ
              have hΓ' : EnvC (E := E) Δ σ (Γ.insert binder.uniq (dn, dty)) ((binder.uniq, sv) :: env) :=
                envC_cons hΓ hvty ⟨ks, hks⟩
              rcases alts with _ | ⟨⟨con0, bs0, dbody⟩, rest⟩
              · dsimp only at hc
                rw [cchain] at hc
                exact error_ne_ok hc
              · cases con0 with
                | default =>
                    try dsimp only at hc
                    obtain ⟨dnt2, hdb, hc⟩ := except_bind_eq_ok hc
                    obtain ⟨dnf, dbt⟩ := dnt2
                    dsimp only at hc
                    split at hc
                    rotate_left
                    · exact error_ne_ok hc
                    rename_i hteq
                    have hdbt : dbt = resTy := teq_eq hteq
                    subst hdbt
                    cases efuel with
                    | zero => rw [Eval.tryAlts] at hv'; exact error_ne_ok hv'
                    | succ ef2 =>
                        rw [Eval.tryAlts] at hv'
                        try dsimp only at hv'
                        obtain ⟨hty2, hvt, hr⟩ := cchain_sound hΔ ih hΓ' hsz hvty hks rest
                          (some dnf) (some (Alt.mk .default bs0 dbody)) (nf, ty) ef2 v hc hv'
                          (.inr ⟨dnf, .default, bs0, dbody, rfl, rfl, hdb⟩)
                        exact ⟨by rw [show ty = dbt from hty2]; exact hvt, hr⟩
                | dataAlt cn =>
                    try dsimp only at hc
                    obtain ⟨hty2, hvt, hr⟩ := cchain_sound hΔ ih hΓ' hsz hvty hks
                      (Alt.mk (AltCon.dataAlt cn) bs0 dbody :: rest) none none (nf, ty)
                      efuel v hc hv' (.inl ⟨rfl, rfl⟩)
                    exact ⟨by rw [show ty = resTy from hty2]; exact hvt, hr⟩
                | litAlt i =>
                    try dsimp only at hc
                    obtain ⟨hty2, hvt, hr⟩ := cchain_sound hΔ ih hΓ' hsz hvty hks
                      (Alt.mk (AltCon.litAlt i) bs0 dbody :: rest) none none (nf, ty)
                      efuel v hc hv' (.inl ⟨rfl, rfl⟩)
                    exact ⟨by rw [show ty = resTy from hty2]; exact hvt, hr⟩
      | litStr sl =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil => dsimp only at hc; exact error_ne_ok hc
      | litList tyl es =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil => dsimp only at hc; exact error_ne_ok hc
      | app f a => dsimp only at hc; exact error_ne_ok hc
      | lam x b => dsimp only at hc; exact error_ne_ok hc
      | letE bnd body =>
          cases bnd with
          | nonRec x rhs =>
              cases args with
              | cons a as => dsimp only at hc; exact error_ne_ok hc
              | nil =>
                  dsimp only at hc hev
                  obtain ⟨nt, hrhs, hc⟩ := except_bind_eq_ok hc
                  obtain ⟨w, hw, hev⟩ := except_bind_eq_ok hev
                  obtain ⟨rv, hrv, hw⟩ := except_bind_eq_ok hw
                  obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
                  have hvs0 : vs = [] := by
                    obtain ⟨hlen, _⟩ := evalList_ok_idx hvs
                    exact List.length_eq_zero_iff.mp (by simpa using hlen)
                  subst hvs0
                  have hv : v = w := applyMany_nil_inv hev
                  subst hv
                  obtain ⟨hvty1, k1, hrep1⟩ := ih Γ rhs nt.1 nt.2 efuel env jenv rv hrhs hrv hΓ
                  exact ih (Γ.insert x.uniq nt) body nf ty efuel ((x.uniq, rv) :: env) jenv v
                    hc hw (envC_cons hΓ hvty1 ⟨k1, hrep1⟩)
          | recB bs =>
              cases args with
              | cons a as => dsimp only at hc; exact error_ne_ok hc
              | nil => dsimp only at hc; exact error_ne_ok hc
          | join l ps e' =>
              cases args with
              | cons a as => dsimp only at hc; exact error_ne_ok hc
              | nil => dsimp only at hc; exact error_ne_ok hc
      | jump l es =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil => dsimp only at hc; exact error_ne_ok hc


/-! ## The per-definition validator (the verified core of
`rwv-cexp-validate`)

The checker compiles the Eidos body over the Hyle definition's
parameter names (each Eidos parameter mapped to a Hyle-side variable
at its declared width), symbolically evaluates the Hyle body with the
bridge's `symExp`, and compares the two normal forms syntactically
after `NF.cfold` — the σ-independent constant folder, whose
`cfold_eval` is UNCONDITIONAL, so the composition needs no
width-discipline side condition. (The measurement driver additionally
reports the `cfoldW3` and DAG-normalized verdicts; upgrading the
verified leg to those normalizers needs a `VarsWF` invariant for
`cexp`'s output — future work, exactly as recorded for the bridge's
width-aware checker.) -/

/-- The compile-time environment for a definition pair: the i-th Eidos
parameter is the i-th Hyle parameter's variable at its declared width,
at the Eidos parameter's declared type. -/
def mkParamGamma (eps : List Id) (hps : List String) (hws : List Nat) :
    HashMap Int (NF × Ty) :=
  (eps.zip ((hps.zip hws).map fun p => ((NF.var p.2 p.1 : NF)))).foldr
    (fun (pr : Id × NF) m => m.insert pr.1.uniq (pr.2, pr.1.sig.ty)) ∅

/-- The bridge-side symbolic environment: each Hyle parameter is
itself, at its declared width. -/
def mkParamRho (hps : List String) (hws : List Nat) : HashMap String NF :=
  (hps.zip hws).foldl (fun m pr => m.insert pr.1 (.var pr.2 pr.1)) ∅


/-! # Phase 4b: the full compiler

The 4a fragment extended with the remaining machine-mode pure forms,
mirroring the reference lowering construct for construct:

  * join points and jumps in pure bodies (`letE join` binds a
    compile-time continuation closure — parameters, captured symbolic
    environment, captured join environment, body — and a jump compiles
    by inlining the join body with the compiled arguments bound,
    fuel-decremented, exactly as the evaluator's `JEnv` works);
  * the commuting rewrites of ToHyle.transExp: a lambda applied
    (arguments bind like a let), a let-headed application (the
    application pushes into the body), a case-headed application (the
    application pushes into the arms) — realized not by re-building
    syntax but by a `pend`ing list of already-compiled arguments that
    heads consume (definition calls may leave a remainder for the
    body's result, mirroring `applyMany`);
  * the remaining first-order §7.6 rows: Finite (literal,
    range-checked), FiniteMin/MaxBound, ToFinite, ToFiniteMod,
    FromFinite (at the exact width — Finite canonicality is not
    tracked by `VTy`, so the widening case is rejected),
    VecReplicate, VecConcat, VecReverse, VecSlice, VecRSlice,
    VecIndexProxy, VecIndex (the dynamic shift construction),
    VecFromList (list-literal argument), NatVal, BitSlice/BitIndex
    (syntactic Finite-literal indices, exactly ToHyle's `finLit`),
    and live `rwPrimError` (zero value / `undef`);
  * the higher-order Vec rows (ToHyle's VecMap/VecGenerate cases and
    `applyFn`): unrolled per element — VecMap slices the compiled
    vector argument MSB-first at the input-element width, VecGenerate
    supplies index literals at width `nbits nElems` — and the function
    argument is compiled applied to the element through the pending
    discipline (`cexpJ f [(elem, teI)]`), which realizes exactly
    `applyFn`'s two shapes: a lambda binds the element in Γ, a
    definition reference (with or without earlier arguments on its
    spine) consumes it as its final argument and inlines.

Still outside: strings, local-variable application, bare lambdas,
model-CARRYING extern occurrences, and generic extern instantiations
(the foreign rows proper — Cryptol splices and model-less
combinational externs — are in scope as of stage B, see the module
header).

The original 4a `cexp`/`cAlt`/`cchain`/`cprim` and every theorem about
them are unchanged above; `cexpFull` (= `cexpJ` with empty join
environment and no pending arguments) strictly extends `cexp`'s
success set and is what the validator now uses. -/

/-! ## Compile-time join closures (mirror `Eval.JoinClos`) -/

/-- A compile-time join continuation: parameters, the captured
symbolic environment and join environment (lexical, like the
evaluator's), and the body (compiled at jump sites). -/
inductive CJoin where
  | mk (params : List Id) (Γ : HashMap Int (NF × Ty)) (js : List (Int × CJoin)) (body : Exp)

/-- The compile-time join environment: label unique ↦ continuation. -/
abbrev CJEnv := List (Int × CJoin)

/-! ## Static type accessors for the new rows -/

/-- The bound of a `Finite n` type. -/
def finBoundT (who : String) (t : Ty) : Except String Nat :=
  match Ty.flatten t with
  | (.con "Finite", [n]) =>
      match Ty.evalNat n with
      | some k => .ok k
      | none => .error s!"{who}: open Finite bound"
  | _ => .error s!"{who}: expected a Finite type"

/-- The length and element type of a `Vec n τ` type. -/
def vecLenElem (who : String) (t : Ty) : Except String (Nat × Ty) :=
  match Ty.flatten t with
  | (.con "Vec", [n, te]) =>
      match Ty.evalNat n with
      | some k => .ok (k, te)
      | none => .error s!"{who}: open Vec length"
  | _ => .error s!"{who}: expected a Vec type"

/-- The index of a `Proxy n` type. -/
def proxyNatT (who : String) (t : Ty) : Except String Nat :=
  match Ty.flatten t with
  | (.con "Proxy", [n]) =>
      match Ty.evalNat n with
      | some k => .ok k
      | none => .error s!"{who}: open Proxy index"
  | _ => .error s!"{who}: expected a Proxy type"

/-- The k-th argument type of the instantiated builtin type (mirrors
`Eval.domTy`). -/
def domTyT (who : String) (doms : List Ty) (k : Nat) : Except String Ty :=
  match doms[k]? with
  | some t => .ok t
  | none => .error s!"{who}: missing argument type in the instantiated builtin type"

/-- ToHyle's `resize`: identity at the same width, `zext` when
widening, `trunc` when narrowing (`wa` is the operand's type-derived
width). -/
def resizeNF (m wa : Nat) (a : NF) : NF :=
  if m = wa then a
  else if wa < m then .prim1 (.zext m) a
  else .prim1 (.trunc m) a

/-- ToHyle's `finLit`: a syntactic `rwPrimFinite` applied to an
integer literal, read off as the value the evaluator computes for it
(`(BitVec.ofInt 128 n).toNat`, the 128-bit residue's numeral). -/
def finLitE (e : Exp) : Option Nat :=
  match Eval.flattenApp e with
  | (.prim _ .finite, [.litInt _ n]) => some (BitVec.ofInt 128 n).toNat
  | _ => none

/-- A computable `VTy` checker (soundness direction only — all the
compiler needs): the error row checks the zero value it compiles to,
rather than assuming a declaration shape `denvOk` does not pin down
(the tuple family). -/
def vtyB (Δ : DEnv) : Nat → Val → Ty → Bool
  | 0, _, _ => false
  | fuel + 1, v, t =>
    match v with
    | .vec elems =>
        (match Ty.flatten t with
        | (.con "Vec", [n, te]) =>
            (match Ty.evalNat n with
            | some k => elems.length == k && elems.all (fun e => vtyB Δ fuel e te)
            | none => false)
        | _ => false)
    | .integer _ =>
        (match Ty.flatten t with | (.con "Integer", []) => true | _ => false)
    | .finite b i =>
        (match Ty.flatten t with
        | (.con "Finite", [n]) => Ty.evalNat n == some b && decide (i < b)
        | _ => false)
    | .proxy =>
        (match Ty.flatten t with | (.con "Proxy", _) => true | _ => false)
    | .con ty c fields =>
        teq ty t &&
        (match Δ.ctorSig.get? c with
        | some sig =>
            (match DEnv.matchTy (Ty.flattenArrow sig.ty).2 t with
            | .ok sub =>
                fields.length == (Ty.flattenArrow sig.ty).1.length &&
                ctorOfB Δ t c &&
                ((Ty.flattenArrow sig.ty).1.zip fields).all
                  (fun p => vtyB Δ fuel p.2 (DEnv.substTv sub p.1))
            | .error _ => false)
        | none => false)
    | .str _ => false
    | .closL _ _ _ => false
    | .closD _ _ => false

/-! ## The extended row table (delegating the 4a rows to `cprim`) -/

/-- The Finite-literal row (ToHyle's `Finite` case: the compiled
argument must be a literal; the value is range-checked and re-emitted
at the Finite width). -/
def rowFinite (res : Ty) : List (NF × Ty) → Except String (NF × Ty)
  | [(a, _)] => do
      let n ← finBoundT "rwPrimFinite" res
      match a with
      | .lit v =>
          if v.nat < n then .ok (.lit ⟨nbits n, BitVec.ofNat (nbits n) v.nat⟩, res)
          else .error s!"rwPrimFinite: {v.nat} is not representable in Finite {n}"
      | _ => .error "rwPrimFinite: can't determine the argument value at compile time"
  | _ => .error "rwPrimFinite: arity mismatch"

/-- FiniteMin/MaxBound (`mx = false`/`true`). -/
def rowFinBound (mx : Bool) (res : Ty) : List (NF × Ty) → Except String (NF × Ty)
  | [] => do
      let n ← finBoundT "rwPrimFiniteBound" res
      if 1 ≤ n then
        .ok (.lit ⟨nbits n, BitVec.ofNat (nbits n) (if mx then n - 1 else 0)⟩, res)
      else .error "rwPrimFiniteBound: Finite 0 is uninhabited"
  | _ => .error "rwPrimFiniteBound: arity mismatch"

def rowToFinite (res : Ty) : List (NF × Ty) → Except String (NF × Ty)
  | [(a, ta)] => do
      let n ← finBoundT "rwPrimToFinite" res
      let wa ← vecBoolLen "rwPrimToFinite" ta
      if 2 ^ wa ≤ n then .ok (resizeNF (nbits n) wa a, res)
      else .error "rwPrimToFinite: bit vector not representable in the Finite bound"
  | _ => .error "rwPrimToFinite: arity mismatch"

def rowToFiniteMod (res : Ty) : List (NF × Ty) → Except String (NF × Ty)
  | [(a, ta)] => do
      let n ← finBoundT "rwPrimToFiniteMod" res
      let wa ← vecBoolLen "rwPrimToFiniteMod" ta
      if 2 ^ wa ≤ n then .ok (resizeNF (nbits n) wa a, res)
      else if n = 0 then
        .error "rwPrimToFiniteMod: Finite 0 result (uninhabited)"
      else
        let w := max wa (nbits n)
        .ok (resizeNF (nbits n) w
              (.prim2 .umod (resizeNF w wa a) (.lit ⟨w, BitVec.ofNat w n⟩)), res)
  | _ => .error "rwPrimToFiniteMod: arity mismatch"

def rowFromFinite (res : Ty) : List (NF × Ty) → Except String (NF × Ty)
  | [(a, ta)] => do
      let n ← finBoundT "rwPrimFromFinite" ta
      let m ← vecBoolLen "rwPrimFromFinite" res
      -- §7.6: any m with n ≤ 2^m — the canonicality (`VTy.finite`'s
      -- tracked bound) makes the resize value-preserving
      if n ≤ 2 ^ m then .ok (resizeNF m (nbits n) a, res)
      else .error "rwPrimFromFinite: Finite bound not representable"
  | _ => .error "rwPrimFromFinite: arity mismatch"

def rowVecReplicate (Δ : DEnv) (szf : Nat) (res : Ty) : List (NF × Ty) → Except String (NF × Ty)
  | [(a, ta)] => do
      let (n, te) ← vecLenElem "rwPrimVecReplicate" res
      if teq ta te then do
        let sz ← Δ.sizeOf szf [] res
        if n = 0 ∨ sz = 0 then .ok (.lit BV.nil, res)
        else .ok (.prim1 (.rep n) a, res)
      else .error "rwPrimVecReplicate: element-type mismatch"
  | _ => .error "rwPrimVecReplicate: arity mismatch"

def rowVecConcat (Δ : DEnv) (szf : Nat) (res : Ty) : List (NF × Ty) → Except String (NF × Ty)
  | [(a, ta), (b', tb)] => do
      let (nr, ter) ← vecLenElem "rwPrimVecConcat" res
      let (n1, te1) ← vecLenElem "rwPrimVecConcat" ta
      let (n2, te2) ← vecLenElem "rwPrimVecConcat" tb
      if teq te1 ter && teq te2 ter && nr == n1 + n2 then do
        let sa ← Δ.sizeOf szf [] ta
        let sb ← Δ.sizeOf szf [] tb
        .ok (catNF [(a, sa), (b', sb)], res)
      else .error "rwPrimVecConcat: type mismatch"
  | _ => .error "rwPrimVecConcat: arity mismatch"

def rowVecReverse (Δ : DEnv) (szf : Nat) (res : Ty) : List (NF × Ty) → Except String (NF × Ty)
  | [(a, ta)] => do
      let (nr, ter) ← vecLenElem "rwPrimVecReverse" res
      let (n1, te1) ← vecLenElem "rwPrimVecReverse" ta
      if teq te1 ter && nr == n1 then do
        let se ← Δ.sizeOf szf [] ter
        .ok (catNF ((List.range n1).map fun k => (sliceNF (k * se) se a, se)), res)
      else .error "rwPrimVecReverse: type mismatch"
  | _ => .error "rwPrimVecReverse: arity mismatch"

def rowVecSlice (Δ : DEnv) (szf : Nat) (doms : List Ty) (res : Ty) :
    List (NF × Ty) → Except String (NF × Ty)
  | [(_, _), (a, ta)] => do
      let pt ← domTyT "rwPrimVecSlice" doms 0
      let i ← proxyNatT "rwPrimVecSlice" pt
      let (m, ter) ← vecLenElem "rwPrimVecSlice" res
      let (len, tea) ← vecLenElem "rwPrimVecSlice" ta
      if teq tea ter then do
        let se ← Δ.sizeOf szf [] ter
        let szA ← Δ.sizeOf szf [] ta
        if i + m ≤ len then
          .ok (sliceNF (szA - i * se - m * se) (m * se) a, res)
        else .error "rwPrimVecSlice: slice out of range"
      else .error "rwPrimVecSlice: element-type mismatch"
  | _ => .error "rwPrimVecSlice: arity mismatch"

def rowVecRSlice (Δ : DEnv) (szf : Nat) (doms : List Ty) (res : Ty) :
    List (NF × Ty) → Except String (NF × Ty)
  | [(_, _), (a, ta)] => do
      let pt ← domTyT "rwPrimVecRSlice" doms 0
      let i ← proxyNatT "rwPrimVecRSlice" pt
      let (m, ter) ← vecLenElem "rwPrimVecRSlice" res
      let (len, tea) ← vecLenElem "rwPrimVecRSlice" ta
      if teq tea ter then do
        let se ← Δ.sizeOf szf [] ter
        let szA ← Δ.sizeOf szf [] ta
        if i + m ≤ len then
          .ok (sliceNF (i * se) (m * se) a, res)
        else .error "rwPrimVecRSlice: slice out of range"
      else .error "rwPrimVecRSlice: element-type mismatch"
  | _ => .error "rwPrimVecRSlice: arity mismatch"

def rowVecIndexProxy (Δ : DEnv) (szf : Nat) (doms : List Ty) (res : Ty) :
    List (NF × Ty) → Except String (NF × Ty)
  | [(a, ta), (_, _)] => do
      let pt ← domTyT "rwPrimVecIndexProxy" doms 1
      let k ← proxyNatT "rwPrimVecIndexProxy" pt
      let (len, tea) ← vecLenElem "rwPrimVecIndexProxy" ta
      if teq tea res then do
        let se ← Δ.sizeOf szf [] res
        let szA ← Δ.sizeOf szf [] ta
        if k < len then .ok (sliceNF (szA - k * se - se) se a, res)
        else .error "rwPrimVecIndexProxy: index out of range"
      else .error "rwPrimVecIndexProxy: element-type mismatch"
  | _ => .error "rwPrimVecIndexProxy: arity mismatch"

def rowVecIndex (Δ : DEnv) (szf : Nat) (res : Ty) : List (NF × Ty) → Except String (NF × Ty)
  | [(a, ta), (i, ti)] => do
      let (len, te) ← vecLenElem "rwPrimVecIndex" ta
      let nb ← finBoundT "rwPrimVecIndex" ti
      if nb == len && teq te res then do
        let se ← Δ.sizeOf szf [] res
        let szA ← Δ.sizeOf szf [] ta
        if 1 ≤ len ∧ len < 2 ^ 128 ∧ len * se < 2 ^ 128 then
          let w := max (nbits nb) 128
          let i' := resizeNF w (nbits nb) i
          let amt := NF.prim2 .mul
            (.prim2 .sub (.prim2 .sub (.lit ⟨w, BitVec.ofNat w len⟩) i') (.lit ⟨w, 1⟩))
            (.lit ⟨w, BitVec.ofNat w se⟩)
          .ok (resizeNF se szA (.prim2 .lshr a amt), res)
        else .error "rwPrimVecIndex: width guard failed"
      else .error "rwPrimVecIndex: type mismatch"
  | _ => .error "rwPrimVecIndex: arity mismatch"

def rowNatVal (doms : List Ty) (res : Ty) : List (NF × Ty) → Except String (NF × Ty)
  | [(_, _)] => do
      let pt ← domTyT "rwPrimNatVal" doms 0
      let k ← proxyNatT "rwPrimNatVal" pt
      match Ty.flatten res with
      | (.con "Integer", []) => .ok (.lit ⟨128, BitVec.ofNat 128 k⟩, res)
      | _ => .error "rwPrimNatVal: non-Integer result type"
  | _ => .error "rwPrimNatVal: arity mismatch"

/-- The Phase 4b builtin rows over compiled arguments: the Finite
family, the first-order Vec family, and NatVal; every other builtin
delegates to the 4a table `cprim`. `szf` is the `sizeOf` fuel. -/
def cprimF (Δ : DEnv) (szf : Nat) (pty : Ty) (b : Builtin) (pas : List (NF × Ty)) :
    Except String (NF × Ty) :=
  match b with
  | .finite => rowFinite (Ty.flattenArrow pty).2 pas
  | .finiteMinBound => rowFinBound false (Ty.flattenArrow pty).2 pas
  | .finiteMaxBound => rowFinBound true (Ty.flattenArrow pty).2 pas
  | .toFinite => rowToFinite (Ty.flattenArrow pty).2 pas
  | .toFiniteMod => rowToFiniteMod (Ty.flattenArrow pty).2 pas
  | .fromFinite => rowFromFinite (Ty.flattenArrow pty).2 pas
  | .vecReplicate => rowVecReplicate Δ szf (Ty.flattenArrow pty).2 pas
  | .vecConcat => rowVecConcat Δ szf (Ty.flattenArrow pty).2 pas
  | .vecReverse => rowVecReverse Δ szf (Ty.flattenArrow pty).2 pas
  | .vecSlice => rowVecSlice Δ szf (Ty.flattenArrow pty).1 (Ty.flattenArrow pty).2 pas
  | .vecRSlice => rowVecRSlice Δ szf (Ty.flattenArrow pty).1 (Ty.flattenArrow pty).2 pas
  | .vecIndexProxy => rowVecIndexProxy Δ szf (Ty.flattenArrow pty).1 (Ty.flattenArrow pty).2 pas
  | .vecIndex => rowVecIndex Δ szf (Ty.flattenArrow pty).2 pas
  | .natVal => rowNatVal (Ty.flattenArrow pty).1 (Ty.flattenArrow pty).2 pas
  | _ => cprim pty b pas

/-- `cprimF` delegates the foreign builtins to the 4a table, which has
no rows for them (the inversion `cexpJ_sound`'s prim case pins the
builtin away from the evaluator's foreign dispatch with). -/
private theorem cprimF_cry_err {Δ : DEnv} {szf : Nat} {pty : Ty} {pas : List (NF × Ty)}
    {r : NF × Ty} (h : cprimF Δ szf pty .cryptol pas = .ok r) : False :=
  cprim_cry_err (show cprim pty .cryptol pas = .ok r from h)

private theorem cprimF_ext_err {Δ : DEnv} {szf : Nat} {pty : Ty} {pas : List (NF × Ty)}
    {r : NF × Ty} (h : cprimF Δ szf pty .«extern» pas = .ok r) : False :=
  cprim_ext_err (show cprim pty .«extern» pas = .ok r from h)

/-! ## The foreign tier (stage A): the Cryptol row's environment

The Cryptol builtin's semantics is the Hyle-side denotation of the
rwcry-spliced definitions (the trust boundary of the validation plan
§1.3); the evaluator reads it from `Δ.cryF`, and the compiler inlines
the entry definition `Δ.cryD` designates out of `Δ.hyleDefs` through
the bridge's `symExp`. `ForeignC` is the honest premise tying the two
sides together — the final theorems assume it of Δ, and the drivers
construct Δ so it holds by construction. -/

/-- The denotation of a named definition in a Hyle definition
environment (what the drivers install in `Δ.cryF`). -/
def callF (F : Rwv.Hyle.Sem.FEnv) (g : String) (vs : List BV) : Except String BV :=
  match F.get? g with
  | some fn => fn vs
  | none => .error s!"foreign: unknown definition {g}"

/-- The foreign-environment premise (stage A, extended by stage B):
SOME Hyle definition environment `F` implements Δ's foreign definition
map at Δ's own syntactic extern table (`hX` pins the table — the
compiler's Cryptol row inlines spliced definitions through `symExp` at
exactly `Δ.hyleX`, so the premise must speak about the same table),
Δ's semantic Cryptol hook is exactly `F`'s denotation of the entry the
syntactic map designates, and — the stage-B extern clause — an extern
ABSENT from the syntactic table carries no model denotation, so the
evaluator's extern row is forced onto the η tier's bit path exactly
where the compiler emits an uninterpreted-call node; dually (the
stage-C clause `xt`) an extern PRESENT in the table denotes exactly
`F` on its model definition, which is what lets the compiler's
model-carrying row inline that definition through the bridge. `impl`
is pinned at the EMPTY extern environment (the drivers build `F` by
`mkFEnv` at the default); the compiler's `xcallFree` gate on spliced
inlinings is what makes that sufficient at every statement
environment. -/
structure ForeignC (Δ : DEnv) (X : Rwv.Hyle.Sem.XEnv) (F : Rwv.Hyle.Sem.FEnv) : Prop where
  impl : Rwv.Hyle.Bridge.FImplements Δ.hyleDefs X F
  hX   : X = Δ.hyleX
  cry  : ∀ f n t g, Δ.cryD f n t = some g →
    ∃ den, Δ.cryF f n t = some den ∧ ∀ vs, den vs = callF F g vs
  ext  : ∀ s, Δ.hyleX.get? s = none → Δ.xtF s = none
  xt   : ∀ s g, Δ.hyleX.get? s = some g →
    ∃ den, Δ.xtF s = some den ∧ ∀ vs, den vs = callF F g vs

/-- The trivially-empty foreign environment satisfies the premise
against any implementing pair (no Cryptol keys are mapped, no extern
models are installed). -/
theorem foreignC_empty {Δ : DEnv} {X : Rwv.Hyle.Sem.XEnv} {F : Rwv.Hyle.Sem.FEnv}
    (himpl : Rwv.Hyle.Bridge.FImplements Δ.hyleDefs X F)
    (hXeq : X = Δ.hyleX)
    (hnone : ∀ f n t, Δ.cryD f n t = none)
    (hxt : ∀ s, Δ.xtF s = none)
    (hX0 : ∀ s, Δ.hyleX.get? s = none) : ForeignC Δ X F :=
  ⟨himpl, hXeq, fun f n t g hg => absurd hg (by rw [hnone f n t]; simp),
   fun s _ => hxt s,
   fun s g hg => absurd hg (by rw [hX0 s]; simp)⟩

/-! ## HashMap transport for the spliced-call environments (house
style: Bridge's private helpers, re-proved) -/

private theorem ofList_get?_some' {β : Type} {l : List (String × β)} {k : String} {b : β}
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

private theorem findSome?_option_map' {α β γ : Type} {g : α → Option β} {h : β → γ} :
    ∀ (l : List α), l.findSome? (fun a => (g a).map h) = (l.findSome? g).map h := by
  intro l
  induction l with
  | nil => rfl
  | cons a l ih =>
      rw [List.findSome?_cons, List.findSome?_cons]
      cases hg : g a with
      | none => simpa using ih
      | some b => simp

private theorem get?_ofList_map_snd' {β γ : Type} (h : β → γ) (l : List (String × β))
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
  rw [this, findSome?_option_map']

/-- The environment correspondence of a spliced call site: binding the
parameters to compiled normal forms symbolically corresponds to
binding them to those forms' denotations concretely (Bridge's
`envCorr_zip`, private there). -/
private theorem envCorr_zip_eval {σ : String → BV} (ps : List String) (ns : List NF) :
    Rwv.Hyle.Bridge.EnvCorr σ (HashMap.ofList (ps.zip ns))
      (HashMap.ofList (ps.zip (ns.map (NF.eval σ E)))) E := by
  intro x n hx
  have hz : ps.zip (ns.map (NF.eval σ E)) = (ps.zip ns).map fun p => (p.1, p.2.eval σ E) := by
    rw [List.zip_map_right]
    rfl
  rw [hz, get?_ofList_map_snd', hx]
  rfl

/-! ## Decoded values are canonical (the VTy half of the round trip) -/

open Decode in
/-- A successful decode's value is canonical at the decoded type: the
decoder's checks are exactly `VTy`'s demands. -/
theorem decode_vty {Δ : DEnv} :
    ∀ {fuel : Nat} {t : Ty} {bv : BV} {v : Val},
      decode Δ fuel t bv = .ok v → VTy Δ v t := by
  intro fuel
  induction fuel with
  | zero => intro t bv v h; rw [decode] at h; exact error_ne_ok h
  | succ fuel ih =>
      intro t bv v h
      rw [decode] at h
      split at h
      · -- Vec
        rename_i n te hfl
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i k hk
        obtain ⟨we, hwe, h⟩ := except_bind_eq_ok h
        split at h
        rotate_left
        · exact error_ne_ok h
        obtain ⟨fields, hfs, h⟩ := except_bind_eq_ok h
        rw [except_pure_def] at h
        injection h with h
        subst h
        refine VTy.vec hfl hk ?_ ?_
        · rw [decodeFields_length hfs]
          simp
        · intro e he
          obtain ⟨i, hi, hie⟩ := List.getElem_of_mem he
          have hi2 : i < (List.replicate k (te, we)).length := by
            rw [← decodeFields_length hfs]
            exact hi
          obtain ⟨bv', hdec⟩ := decodeFields_pointwise hfs i hi hi2
          rw [← hie]
          have hrepl : (List.replicate k (te, we))[i] = (te, we) :=
            List.getElem_replicate ..
          rw [hrepl] at hdec
          exact ih hdec
      · -- Finite
        rename_i n hfl
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i k hk
        split at h
        rotate_left
        · exact error_ne_ok h
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hltk
        rw [except_pure_def] at h
        injection h with h
        subst h
        exact VTy.finite hfl hk hltk
      · -- Integer
        rename_i hfl
        split at h
        rotate_left
        · exact error_ne_ok h
        rw [except_pure_def] at h
        injection h with h
        subst h
        exact VTy.integer hfl
      · -- Proxy
        rename_i args hfl
        split at h
        rotate_left
        · exact error_ne_ok h
        rw [except_pure_def] at h
        injection h with h
        subst h
        exact VTy.proxy hfl rfl
      · -- Datatype
        rename_i hfl
        obtain ⟨whole, hwhole, h⟩ := except_bind_eq_ok h
        split at h
        rotate_left
        · exact error_ne_ok h
        obtain ⟨cw, hcw, h⟩ := except_bind_eq_ok h
        obtain ⟨cn, tagW⟩ := cw
        dsimp only at h
        obtain ⟨tt, htag, h⟩ := except_bind_eq_ok h
        obtain ⟨tag, tagW'⟩ := tt
        dsimp only at h
        split at h
        rotate_left
        · exact error_ne_ok h
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i sig hsig
        obtain ⟨sub, hsub, h⟩ := except_bind_eq_ok h
        obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
        split at h
        rotate_left
        · exact error_ne_ok h
        split at h
        rotate_left
        · exact error_ne_ok h
        obtain ⟨fields, hfs, h⟩ := except_bind_eq_ok h
        rw [except_pure_def] at h
        injection h with h
        subst h
        have hwsl : ws.length
            = ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)).length := by
          obtain ⟨hl, _⟩ := mapM_ok_idx hws
          exact hl
        have hflen : fields.length = (Ty.flattenArrow sig.ty).1.length := by
          rw [decodeFields_length hfs, List.length_zip, hwsl, List.length_map]
          simp
        refine VTy.con hsig hsub hflen ?_ ?_
        · -- ctorOf
          show ctorOf Δ t cn
          rw [ctorOf, hfl]
          dsimp only
          rcases selectCtor_inv hcw with ⟨htup, hcn, _⟩ | ⟨htup, cs, hcs, hmem, _⟩
          · rw [if_pos htup]
            exact hcn
          · rw [if_neg (by rw [htup]; simp)]
            exact ⟨cs, hcs, hmem⟩
        · -- fields pointwise canonical
          intro p hp
          obtain ⟨i, hi, hpi⟩ := List.getElem_of_mem hp
          have hi' := hi
          rw [List.length_zip] at hi'
          have hid : i < (Ty.flattenArrow sig.ty).1.length :=
            Nat.lt_of_lt_of_le hi' (Nat.min_le_left _ _)
          have hif : i < fields.length := Nat.lt_of_lt_of_le hi' (Nat.min_le_right _ _)
          have hits : i < (((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)).zip
              ws).length := by
            rw [List.length_zip, List.length_map]
            rw [hwsl, List.length_map] at *
            omega
          obtain ⟨bv', hdec⟩ := decodeFields_pointwise hfs i hif hits
          have hzi : ((((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)).zip
                ws)[i]'hits).1
              = DEnv.substTv sub ((Ty.flattenArrow sig.ty).1[i]'hid) := by
            simp [List.getElem_zip]
          rw [hzi] at hdec
          rw [← hpi]
          simp only [List.getElem_zip]
          exact ih hdec
      · exact error_ne_ok h

/-! ## The full compiler -/

mutual

/-- One case alternative under the full compiler: as `cAlt`, with the
join environment threaded and the body compiled applied to the
pending arguments. -/
def cAltJ (Δ : DEnv) (dmap : HashMap Int Defn) (fuel : Nat) (Γ' : HashMap Int (NF × Ty))
    (jΓ : CJEnv) (dty : Ty) (szT : Nat) (dn : NF) (resTy : Ty) (pend : List (NF × Ty)) :
    Alt → Option NF → Except String NF
  | .mk .default _ _, _ => .error "cexp: default alternative not first"
  | .mk (.dataAlt cn) xs body, macc => do
      if ctorOfB Δ dty cn then do
        let (tag, w) ← Δ.ctorTag dty cn
        match Δ.ctorSig.get? cn with
        | none => .error s!"cexp: unknown constructor {cn}"
        | some sig => do
            let sub ← DEnv.matchTy (Ty.flattenArrow sig.ty).2 dty
            let instTys := (Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)
            if xs.length = instTys.length then do
              let szXs ← instTys.mapM (Δ.sizeOf (fuel + 1) [])
              if w + szXs.sum ≤ szT then do
                let offs := (List.range szXs.length).map fun i =>
                  (szXs.drop (i + 1)).sum
                let slices := (szXs.zip offs).map fun (sz, off) =>
                  (sliceNF off sz dn)
                let Γ'' := bindFieldsΓ xs (slices.zip instTys) Γ'
                let (bnf, bty) ← cexpJ Δ dmap fuel Γ'' jΓ body pend
                if teqN Δ bty resTy then
                  match macc, w with
                  | some acc, _ + 1 =>
                      .ok (.ite (.prim2 .eq (sliceNF (szT - w) w dn)
                                            (.lit ⟨w, BitVec.ofNat w tag⟩))
                                bnf acc)
                  | _, _ => .ok bnf
                else .error "cexp: case alternative result-type mismatch"
              else .error s!"cexp: constructor {cn} wider than the discriminant"
            else .error s!"cexp: constructor {cn} binder arity mismatch"
      else .error s!"cexp: constructor {cn} does not belong to the discriminant type"
  | .mk (.litAlt i) _ body, macc => do
      let (bnf, bty) ← cexpJ Δ dmap fuel Γ' jΓ body pend
      if teqN Δ bty resTy then
        match macc with
        | some acc =>
            .ok (.ite (.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT i⟩)) bnf acc)
        | none => .ok bnf
      else .error "cexp: case alternative result-type mismatch"
termination_by alt _ => (fuel, 1, 0)

/-- The if-chain under the full compiler. -/
def cchainJ (Δ : DEnv) (dmap : HashMap Int Defn) (fuel : Nat) (Γ' : HashMap Int (NF × Ty))
    (jΓ : CJEnv) (dty : Ty) (szT : Nat) (dn : NF) (resTy : Ty) (pend : List (NF × Ty)) :
    List Alt → Option NF → Except String (NF × Ty)
  | [], some els => .ok (els, resTy)
  | [], none => .error "cexp: empty case"
  | [alt], none => do
      let bnf ← cAltJ Δ dmap fuel Γ' jΓ dty szT dn resTy pend alt none
      .ok (bnf, resTy)
  | alt :: rest, macc => do
      let (accnf, _) ← cchainJ Δ dmap fuel Γ' jΓ dty szT dn resTy pend rest macc
      let bnf ← cAltJ Δ dmap fuel Γ' jΓ dty szT dn resTy pend alt (some accnf)
      .ok (bnf, resTy)
termination_by rest _ => (fuel, 2, rest.length)

/-- `cexpJ Δ dmap fuel Γ jΓ e pend`: compile `e` applied to the
already-compiled `pend`ing arguments, resolving jumps through the
compile-time join environment `jΓ`. Corresponds to evaluating `e` to
a (possibly function) value and then applying it to the pending
arguments' values (`Eval.applyMany`). -/
def cexpJ (Δ : DEnv) (dmap : HashMap Int Defn) :
    Nat → HashMap Int (NF × Ty) → CJEnv → Exp → List (NF × Ty) →
    Except String (NF × Ty)
  | 0, _, _, _, _ => .error "cexp: out of fuel"
  | fuel + 1, Γ, jΓ, e, pend =>
    match Eval.flattenApp e with
    | (.var x, args) =>
        match Γ.get? x.uniq with
        | some nt =>
            match args, pend with
            | [], [] => .ok nt
            | _, _ => .error s!"cexp: unsupported application of a local variable: {x.occ}"
        | none =>
            match dmap.get? x.uniq with
            | some d => do
                let pas ← args.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
                let pall := pas ++ pend
                if d.params.length ≤ pall.length then
                  if teqAll (pall.take d.params.length) (d.params.map (·.sig.ty)) then
                    if d.params.length ≤ pas.length || x.uniq == d.name.uniq then
                      cexpJ Δ dmap fuel (mkGamma d.params (pall.take d.params.length)) []
                        d.body (pall.drop d.params.length)
                    else .error s!"cexp: call to {x.occ}: partial application through an inconsistent map"
                  else .error s!"cexp: call to {x.occ}: argument-type mismatch"
                else .error s!"cexp: call to {x.occ}: unsaturated ({pall.length} of {d.params.length} arguments)"
            | none => .error s!"cexp: unknown definition {x.occ}#{x.uniq}"
    | (.con cty c, args) =>
        match pend with
        | _ :: _ => .error s!"cexp: over-applied constructor {c}"
        | [] => do
            let pas ← args.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
            let resTy := (Ty.flattenArrow cty).2
            if pas.length = (Ty.flattenArrow cty).1.length then
              match Δ.ctorSig.get? c with
              | some sig => do
                  let sub ← DEnv.matchTy (Ty.flattenArrow sig.ty).2 resTy
                  if teqAllN Δ pas ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)) then
                    if ctorOfB Δ resTy c then do
                      let whole ← Δ.sizeOf (fuel + 1) [] resTy
                      let (tag, w) ← Δ.ctorTag resTy c
                      let ws ← ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)).mapM
                        (Δ.sizeOf (fuel + 1) [])
                      if w + ws.sum ≤ whole then
                        .ok (catNF ((.lit ⟨w, BitVec.ofNat w tag⟩, w)
                                    :: (.lit ⟨whole - w - ws.sum, 0⟩, whole - w - ws.sum)
                                    :: (pas.map (·.1)).zip ws), resTy)
                      else .error s!"cexp: constructor {c} wider than its type"
                    else .error s!"cexp: constructor {c} does not belong to its result type"
                  else .error s!"cexp: constructor {c}: field-type mismatch"
              | none => .error s!"cexp: unknown constructor {c}"
            else .error s!"cexp: unsaturated constructor {c}"
    | (.prim pty b, args) =>
        match pend with
        | _ :: _ => .error "cexp: over-applied primitive"
        | [] =>
            match b with
            | .error => do
                -- Live error compiles to the zero (undef) value of the
                -- applied type; the zero value's canonicality and its
                -- all-zeros representation are checked by execution.
                let rty := Eval.dropArrows args.length pty
                let sz ← Δ.sizeOf (fuel + 1) [] rty
                let zv ← Δ.zeroVal (fuel + 1) rty
                if vtyB Δ (fuel + 1) zv rty then
                  match Val.rep Δ (fuel + 1) zv with
                  | .ok bv =>
                      if bv == (⟨sz, 0⟩ : BV) then .ok (.lit ⟨sz, 0⟩, rty)
                      else .error "rwPrimError: zero value is not all-zeros at the result width"
                  | .error _ => .error "rwPrimError: zero value has no representation"
                else .error "rwPrimError: zero value not canonical at the result type"
            | .vecFromList =>
                (match args with
                | [.litList _ els] => do
                    let pas ← els.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
                    let res := (Ty.flattenArrow pty).2
                    match Ty.flatten res with
                    | (.con "Vec", [nt, te]) =>
                        match Ty.evalNat nt with
                        | some k =>
                            if pas.length = k then
                              if teqAll pas (List.replicate pas.length te) then do
                                let se ← Δ.sizeOf (fuel + 1) [] te
                                .ok (catNF ((pas.map (·.1)).map (·, se)), res)
                              else .error "rwPrimVecFromList: element-type mismatch"
                            else .error "rwPrimVecFromList: length mismatch"
                        | none => .error "rwPrimVecFromList: open Vec length"
                    | _ => .error "rwPrimVecFromList: non-Vec result type"
                | _ => .error "rwPrimVecFromList: argument must be a list literal")
            | .bitIndex =>
                (match args with
                | [argE, iE] =>
                    (match finLitE iE with
                    | some i => do
                        let (a, ta) ← cexpJ Δ dmap fuel Γ jΓ argE []
                        let wa ← vecBoolLen "rwPrimBitIndex" ta
                        if isBoolT (Ty.flattenArrow pty).2 then
                          if i < wa then .ok (sliceNF i 1 a, (Ty.flattenArrow pty).2)
                          else .error "rwPrimBitIndex: index out of range"
                        else .error "rwPrimBitIndex: non-Bool result type"
                    | none => .error "rwPrimBitIndex: index must be a Finite literal")
                | _ => .error "rwPrimBitIndex: arity mismatch")
            | .bitSlice =>
                (match args with
                | [argE, jE, iE] =>
                    (match finLitE jE, finLitE iE with
                    | some j, some i =>
                        if i ≤ j + 1 then do
                          let (a, ta) ← cexpJ Δ dmap fuel Γ jΓ argE []
                          let wa ← vecBoolLen "rwPrimBitSlice" ta
                          let mr ← vecBoolLen "rwPrimBitSlice" (Ty.flattenArrow pty).2
                          if mr = j + 1 - i ∧ i + (j + 1 - i) ≤ wa then
                            .ok (sliceNF i (j + 1 - i) a, (Ty.flattenArrow pty).2)
                          else .error "rwPrimBitSlice: slice out of range"
                        else .error s!"rwPrimBitSlice: invalid slice (j: {j}, i: {i})"
                    | _, _ => .error "rwPrimBitSlice: indices must be Finite literals")
                | _ => .error "rwPrimBitSlice: arity mismatch")
            | .vecMap =>
                -- ToHyle's VecMap: unroll per element (MSB-first input
                -- slices), applying the function argument through the
                -- pending-argument discipline (a lambda binds the
                -- element; a — possibly partially applied — definition
                -- reference consumes it as its final argument).
                (match args with
                | [fE, argE] => do
                    let (nElems, teO) ← vecLenElem "rwPrimVecMap" (Ty.flattenArrow pty).2
                    let seO ← Δ.sizeOf (fuel + 1) [] teO
                    let (a, ta) ← cexpJ Δ dmap fuel Γ jΓ argE []
                    let (len, teI) ← vecLenElem "rwPrimVecMap" ta
                    let seI ← Δ.sizeOf (fuel + 1) [] teI
                    let szV ← Δ.sizeOf (fuel + 1) [] ta
                    if len = nElems then do
                      let ps ← (List.range nElems).mapM fun i => do
                        let (ei, ti) ← cexpJ Δ dmap fuel Γ jΓ fE
                          [(sliceNF (szV - (i + 1) * seI) seI a, teI)]
                        if teq ti teO then .ok (ei, seO)
                        else .error "rwPrimVecMap: element result-type mismatch"
                      .ok (catNF ps, (Ty.flattenArrow pty).2)
                    else .error "rwPrimVecMap: length mismatch"
                | _ => .error "rwPrimVecMap: arity mismatch")
            | .vecGenerate =>
                -- ToHyle's VecGenerate: apply the function argument to
                -- each index literal at the `nbits nElems` width.
                (match args with
                | [fE] => do
                    let (nElems, teO) ← vecLenElem "rwPrimVecGenerate" (Ty.flattenArrow pty).2
                    let seO ← Δ.sizeOf (fuel + 1) [] teO
                    let ft ← domTyT "rwPrimVecGenerate" (Ty.flattenArrow pty).1 0
                    match ft with
                    | .arrow tFin _ => do
                        let nb ← finBoundT "rwPrimVecGenerate" tFin
                        if nb = nElems then do
                          let ps ← (List.range nElems).mapM fun i => do
                            let (ei, ti) ← cexpJ Δ dmap fuel Γ jΓ fE
                              [(.lit ⟨nbits nElems, BitVec.ofNat (nbits nElems) i⟩, tFin)]
                            if teq ti teO then .ok (ei, seO)
                            else .error "rwPrimVecGenerate: element result-type mismatch"
                          .ok (catNF ps, (Ty.flattenArrow pty).2)
                        else .error "rwPrimVecGenerate: generator domain-bound mismatch"
                    | _ => .error "rwPrimVecGenerate: non-arrow function-argument type"
                | _ => .error "rwPrimVecGenerate: arity mismatch")
            | .cryptol =>
                -- The foreign row (stage A): inline the spliced entry
                -- definition Δ.cryD designates through the bridge's
                -- symbolic evaluator — the compiled form is exactly
                -- what the Hyle side's Call to it inlines to. The
                -- inlining runs at Δ's syntactic extern table, and the
                -- result is gated `xcallFree` (stage B): the spliced
                -- denotations `ForeignC` pins live at the EMPTY extern
                -- environment, and extern-free normal forms are the
                -- ones whose denotation cannot see the difference.
                (match args with
                | .litStr f :: .litStr n :: _impl :: rest => do
                    let ity ← Eval.domTy "rwPrimCryptol" (Ty.flattenArrow pty).1 2
                    if rest.length = (Ty.flattenArrow ity).1.length then
                      match Δ.cryD f n ity with
                      | some g =>
                          (match Δ.hyleDefs.get? g with
                          | some hd => do
                              let pas ← rest.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
                              if pas.length = hd.params.length then
                                if (pas.map (·.1)).all NF.xcallFree then do
                                  let nf ← Rwv.Hyle.Bridge.symExp Δ.hyleDefs Δ.hyleX Δ.hyleFuel
                                    (HashMap.ofList (hd.params.zip (pas.map (·.1)))) hd.body
                                  if nf.xcallFree then
                                    .ok (nf, (Ty.flattenArrow ity).2)
                                  else .error "rwPrimCryptol: spliced definition reaches an extern (out of scope)"
                                else .error "rwPrimCryptol: extern-derived argument (out of scope)"
                              else .error "rwPrimCryptol: spliced definition arity mismatch"
                          | none => .error s!"rwPrimCryptol: unknown spliced definition {g}")
                      | none => .error "rwPrimCryptol: no entry mapping for this instantiation"
                    else .error "rwPrimCryptol: unsaturated foreign application"
                | _ => .error "rwPrimCryptol: malformed foreign application")
            | .«extern» =>
                -- The model-less combinational extern row (stage B,
                -- the η tier): compile the value arguments, pack their
                -- concatenation, and emit the uninterpreted-call node
                -- at the result type's width. The syntactic gate
                -- `Δ.hyleX.get? s = none` is what `ForeignC`'s extern
                -- clause converts into "the evaluator takes the η
                -- path"; a model-carrying extern is the (rejected)
                -- stage-A residue.
                (match args with
                | _ps :: _clk :: _rst :: _as :: _rs :: .litStr s :: _impl :: _inst :: rest => do
                    let ity ← Eval.domTy "rwPrimExtern" (Ty.flattenArrow pty).1 6
                    if rest.length = (Ty.flattenArrow ity).1.length then
                      match Δ.hyleX.get? s with
                      | some g =>
                          -- The MODEL-CARRYING extern row (stage C):
                          -- exactly the Cryptol row keyed by the
                          -- extern table — inline the model definition
                          -- through the bridge at Δ's syntactic extern
                          -- table, gated extern-free on the compiled
                          -- arguments and the inlined form (the model
                          -- denotations `ForeignC.xt` pins live at the
                          -- empty extern environment).
                          (match Δ.hyleDefs.get? g with
                          | some hd => do
                              let pas ← rest.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
                              if pas.length = hd.params.length then
                                if (pas.map (·.1)).all NF.xcallFree then do
                                  let nf ← Rwv.Hyle.Bridge.symExp Δ.hyleDefs Δ.hyleX Δ.hyleFuel
                                    (HashMap.ofList (hd.params.zip (pas.map (·.1)))) hd.body
                                  if nf.xcallFree then
                                    .ok (nf, (Ty.flattenArrow ity).2)
                                  else .error s!"rwPrimExtern {s}: model reaches an extern (out of scope)"
                                else .error s!"rwPrimExtern {s}: extern-derived argument (out of scope)"
                              else .error s!"rwPrimExtern {s}: model arity mismatch"
                          | none => .error s!"rwPrimExtern {s}: unknown model {g}")
                      | none => do
                          let pas ← rest.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
                          let res := (Ty.flattenArrow ity).2
                          let w ← Δ.sizeOf (fuel + 1) [] res
                          .ok (.xcall w s (.xpack (pas.map (·.1))), res)
                    else .error "rwPrimExtern: unsaturated foreign application"
                | _ => .error "rwPrimExtern: malformed foreign application")
            | _ => do
                let pas ← args.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
                cprimF Δ (fuel + 1) pty b pas
    | (.litInt ty n, []) =>
        match pend with
        | [] => clitInt ty n
        | _ :: _ => .error "cexp: applied integer literal"
    | (.litVec vty es, []) =>
        match pend with
        | _ :: _ => .error "cexp: applied vector literal"
        | [] => do
            let pas ← es.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
            match Ty.flatten vty with
            | (.con "Vec", [nt, te]) =>
                match Ty.evalNat nt with
                | some k =>
                    if pas.length = k then
                      if teqAll pas (List.replicate pas.length te) then do
                        let se ← Δ.sizeOf (fuel + 1) [] te
                        .ok (catNF ((pas.map (·.1)).map (·, se)), vty)
                      else .error "cexp: vector literal element-type mismatch"
                    else .error "cexp: vector literal length mismatch"
                | none => .error "cexp: vector literal at an open Vec length"
            | _ => .error "cexp: vector literal at a non-Vec type"
    | (.lam x b, args) => do
        let pas ← args.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
        match pas ++ pend with
        | [] => .error "cexp: unsupported lambda expression"
        | nt :: rest => cexpJ Δ dmap fuel (Γ.insert x.uniq nt) [] b rest
    | (.letE (.nonRec x rhs) body, args) => do
        let nt ← cexpJ Δ dmap fuel Γ jΓ rhs []
        let pas ← args.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
        cexpJ Δ dmap fuel (Γ.insert x.uniq nt) jΓ body (pas ++ pend)
    | (.letE (.join l ps jb) body, args) => do
        let pas ← args.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
        cexpJ Δ dmap fuel Γ ((l.uniq, .mk ps Γ jΓ jb) :: jΓ) body (pas ++ pend)
    | (.jump l es, []) => do
        -- (A jump's value may be further applied — the pending
        -- arguments flow into the join body, mirroring `applyMany`
        -- after the evaluator's jump.)
        let pas ← es.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
        match jΓ.lookup l.uniq with
        | some (.mk ps Γc jc jb) =>
            if teqAll pas (ps.map (·.sig.ty)) then
              cexpJ Δ dmap fuel (bindFieldsΓ ps pas Γc) jc jb pend
            else .error s!"cexp: jump to {l.occ}: arity or argument-type mismatch"
        | none => .error s!"cexp: jump to an unbound join point {l.occ}#{l.uniq}"
    | (.cases resTy scrut binder alts, args) => do
        let (dn, dty) ← cexpJ Δ dmap fuel Γ jΓ scrut []
        let szT ← Δ.sizeOf (fuel + 1) [] dty
        let pas ← args.mapM (cexpJ Δ dmap fuel Γ jΓ · [])
        let pall := pas ++ pend
        let resTy' ← Ty.peel pall.length resTy
        let Γ' := Γ.insert binder.uniq (dn, dty)
        match alts with
        | .mk .default _ dbody :: rest => do
            let (dnf, dbt) ← cexpJ Δ dmap fuel Γ' jΓ dbody pall
            if teqN Δ dbt resTy' then cchainJ Δ dmap fuel Γ' jΓ dty szT dn resTy' pall rest (some dnf)
            else .error "cexp: default alternative result-type mismatch"
        | rest => cchainJ Δ dmap fuel Γ' jΓ dty szT dn resTy' pall rest none
    | (_, _) => .error "cexp: unsupported expression (outside the Phase 4b fragment)"
termination_by fuel _ _ _ _ => (fuel, 0, 0)

end

/-- The full compiler at the top level: empty join environment, no
pending arguments. Strictly extends `cexp`'s success set (and agrees
with it wherever `cexp` succeeds). -/
def cexpFull (Δ : DEnv) (dmap : HashMap Int Defn) (fuel : Nat)
    (Γ : HashMap Int (NF × Ty)) (e : Exp) : Except String (NF × Ty) :=
  cexpJ Δ dmap fuel Γ [] e []

/-! ## Join-environment correspondence

Mirrors the evaluator's `JEnv` discipline: a compile-time join closure
corresponds to a runtime one when the parameters and body coincide and
the captured environments correspond (the join component in lookup
form, keeping the inductive strictly positive — and making the empty
compile-time environment correspond to ANY runtime one, which is what
the closure-body and definition-call cases need). -/

inductive JC (Δ : DEnv) (σ : String → BV) (E : Rwv.Hyle.Sem.EEnv) :
    CJoin → Eval.JoinClos → Prop where
  | mk {ps : List Id} {Γc : HashMap Int (NF × Ty)} {cjs : CJEnv} {body : Exp}
      {envc : Eval.Env} {cjenv : Eval.JEnv} :
      EnvC (E := E) Δ σ Γc envc →
      (∀ l cj, cjs.lookup l = some cj → (cjenv.lookup l).isSome) →
      (∀ l cj jc, cjs.lookup l = some cj → cjenv.lookup l = some jc → JC Δ σ E cj jc) →
      JC Δ σ E (.mk ps Γc cjs body) (.mk ps envc cjenv body)

/-- The join environments correspond on every compile-time-recorded
label. -/
def JEnvC (Δ : DEnv) (σ : String → BV) (jΓ : CJEnv) (jenv : Eval.JEnv) : Prop :=
  ∀ l cj, jΓ.lookup l = some cj → ∃ jc, jenv.lookup l = some jc ∧ JC (E := E) Δ σ cj jc

private theorem jenvC_nil {Δ : DEnv} {σ : String → BV} {jenv : Eval.JEnv} :
    JEnvC (E := E) Δ σ [] jenv := by
  intro l cj h
  simp [List.lookup] at h

/-- Package `EnvC` and `JEnvC` facts into a closure correspondence. -/
private theorem jc_intro {Δ : DEnv} {σ : String → BV} {ps : List Id}
    {Γc : HashMap Int (NF × Ty)} {cjs : CJEnv} {body : Exp} {envc : Eval.Env}
    {cjenv : Eval.JEnv} (hE : EnvC (E := E) Δ σ Γc envc) (hJ : JEnvC (E := E) Δ σ cjs cjenv) :
    JC (E := E) Δ σ (.mk ps Γc cjs body) (.mk ps envc cjenv body) := by
  refine JC.mk hE ?_ ?_
  · intro l cj h
    obtain ⟨jc, hjc, _⟩ := hJ l cj h
    rw [hjc]
    rfl
  · intro l cj jc h1 h2
    obtain ⟨jc', hjc', hJC⟩ := hJ l cj h1
    rw [hjc'] at h2
    injection h2 with h2
    subst h2
    exact hJC

/-- Unpack a closure correspondence back into the lookup form. -/
private theorem jenvC_of_jc {Δ : DEnv} {σ : String → BV} {cjs : CJEnv}
    {cjenv : Eval.JEnv} {ps ps' : List Id} {Γc : HashMap Int (NF × Ty)} {body body' : Exp}
    {envc : Eval.Env}
    (h : JC (E := E) Δ σ (.mk ps Γc cjs body) (.mk ps' envc cjenv body')) :
    JEnvC (E := E) Δ σ cjs cjenv := by
  cases h with
  | mk hE hcov hpt =>
      intro l cj hl
      cases hjc : cjenv.lookup l with
      | none =>
          have := hcov l cj hl
          rw [hjc] at this
          exact absurd this (by simp)
      | some jc => exact ⟨jc, rfl, hpt l cj jc hl hjc⟩

private theorem jenvC_cons {Δ : DEnv} {σ : String → BV} {jΓ : CJEnv} {jenv : Eval.JEnv}
    (h : JEnvC (E := E) Δ σ jΓ jenv) {l : Int} {cj : CJoin} {jc : Eval.JoinClos}
    (hcj : JC (E := E) Δ σ cj jc) : JEnvC (E := E) Δ σ ((l, cj) :: jΓ) ((l, jc) :: jenv) := by
  intro l' cj' hl
  rw [lookup_cons] at hl
  rw [lookup_cons]
  by_cases he : l' = l
  · rw [if_pos he] at hl
    rw [if_pos he]
    injection hl with hl
    subst hl
    exact ⟨jc, rfl, hcj⟩
  · rw [if_neg he] at hl
    rw [if_neg he]
    exact h l' cj' hl

/-! ## Application plumbing (`applyMany` composition, saturated calls) -/

private theorem applyMany_one (C : Eval.Ctx) (k : Nat) (f : Val) :
    Eval.applyMany C (k + 1) f [] E = .ok f := by
  rw [Eval.applyMany]
  rfl

/-- Dissect a successful `applyAll` (the evaluator's VecMap/VecGenerate
element loop) into pointwise single-argument applications, in the
`applyMany`-singleton form the soundness statement's application leg
consumes. -/
private theorem applyAll_ok_idx {C : Eval.Ctx} :
    ∀ {k : Nat} {f : Val} {xs ys : List Val}, Eval.applyAll C k f xs E = .ok ys →
      ys.length = xs.length ∧
      ∀ i (h1 : i < xs.length) (h2 : i < ys.length),
        ∃ K, Eval.applyMany C K f [xs[i]] E = .ok ys[i] := by
  intro k
  induction k with
  | zero =>
      intro f xs ys h
      rw [Eval.applyAll] at h
      exact error_ne_ok h
  | succ k ih =>
      intro f xs ys h
      match xs with
      | [] =>
          rw [Eval.applyAll, except_pure_def] at h
          injection h with h
          subst h
          exact ⟨rfl, fun i h1 _ => absurd h1 (by simp)⟩
      | x :: rest =>
          rw [Eval.applyAll] at h
          obtain ⟨y, hy, h⟩ := except_bind_eq_ok h
          obtain ⟨ys', hys', h⟩ := except_bind_eq_ok h
          rw [except_pure_def] at h
          injection h with h
          subst h
          obtain ⟨hlen, hpt⟩ := ih hys'
          refine ⟨by simpa using hlen, ?_⟩
          intro i h1 h2
          match i with
          | 0 =>
              refine ⟨k + 2, ?_⟩
              simp only [List.getElem_cons_zero]
              rw [Eval.applyMany, Eval.applyValCore_mono (Nat.le_succ k) hy,
                  except_bind_ok]
              exact applyMany_one C k y
          | i + 1 =>
              simp only [List.getElem_cons_succ]
              exact hpt i (by simpa using h1) (by simpa using h2)

/-- `zip` ignores a right-list suffix beyond the left list's length. -/
private theorem zip_append_left {α β : Type} :
    ∀ {l : List α} {v1 v2 : List β}, l.length ≤ v1.length →
      l.zip (v1 ++ v2) = l.zip v1 := by
  intro l
  induction l with
  | nil => intro v1 v2 _; rfl
  | cons a as ih =>
      intro v1 v2 h
      match v1 with
      | [] => exact absurd h (by simp)
      | b :: bs =>
          rw [List.cons_append, List.zip_cons_cons, List.zip_cons_cons,
              ih (by simpa using h)]

/-- `zip` sees only the right list's prefix at the left list's length. -/
private theorem zip_take_right {α β : Type} :
    ∀ {l : List α} {vs : List β}, l.zip vs = l.zip (vs.take l.length) := by
  intro l
  induction l with
  | nil => intro vs; rfl
  | cons a as ih =>
      intro vs
      match vs with
      | [] => rfl
      | b :: bs =>
          rw [List.zip_cons_cons, List.length_cons, List.take_succ_cons,
              List.zip_cons_cons, ← ih]

/-- Compose two application chains into one over the appended
argument list (fuel by existential). -/
private theorem applyMany_compose {C : Eval.Ctx} {bs : List Val} :
    ∀ {as : List Val} {k1 k2 : Nat} {w f v : Val},
      Eval.applyMany C k1 w as E = .ok f → Eval.applyMany C k2 f bs E = .ok v →
      ∃ K, Eval.applyMany C K w (as ++ bs) E = .ok v := by
  intro as
  induction as with
  | nil =>
      intro k1 k2 w f v h1 h2
      have hw : f = w := applyMany_nil_inv h1
      subst hw
      exact ⟨k2, by simpa using h2⟩
  | cons a as ih =>
      intro k1 k2 w f v h1 h2
      cases k1 with
      | zero => rw [Eval.applyMany] at h1; exact error_ne_ok h1
      | succ k1 =>
          rw [Eval.applyMany] at h1
          obtain ⟨v1, hv1, h1⟩ := except_bind_eq_ok h1
          obtain ⟨K, hK⟩ := ih h1 h2
          refine ⟨max k1 K + 1, ?_⟩
          rw [show (a :: as) ++ bs = a :: (as ++ bs) from rfl, Eval.applyMany,
              Eval.applyValCore_mono (Nat.le_max_left k1 K) hv1, except_bind_ok]
          exact Eval.applyMany_mono (Nat.le_max_right k1 K) hK

/-- Extend a (possibly partial) definition application by further
arguments: the closure chain re-resolves the definition by name, so
the definition map must be consistent at the definition's own name
unique. -/
private theorem callDefn_extend {C : Eval.Ctx} {d : Defn}
    (hcons : C.defns.get? d.name.uniq = some d) :
    ∀ {ws vs : List Val} {k1 k2 : Nat} {f v : Val},
      Eval.callDefn C k1 d vs E = .ok f → Eval.applyMany C k2 f ws E = .ok v →
      ∃ K, Eval.callDefn C K d (vs ++ ws) E = .ok v := by
  intro ws
  induction ws with
  | nil =>
      intro vs k1 k2 f v h1 h2
      have hw : v = f := applyMany_nil_inv h2
      subst hw
      exact ⟨k1, by simpa using h1⟩
  | cons w ws ih =>
      intro vs k1 k2 f v h1 h2
      cases k1 with
      | zero => rw [Eval.callDefn] at h1; exact error_ne_ok h1
      | succ k1 =>
          rw [Eval.callDefn] at h1
          by_cases hlt : vs.length < d.params.length
          · rw [if_pos hlt, except_pure_def] at h1
            injection h1 with h1
            subst h1
            -- peel one application off the chain and recurse
            cases k2 with
            | zero => rw [Eval.applyMany] at h2; exact error_ne_ok h2
            | succ k2 =>
                rw [Eval.applyMany] at h2
                obtain ⟨v1, hv1, h2⟩ := except_bind_eq_ok h2
                cases k2 with
                | zero => rw [Eval.applyValCore] at hv1; exact error_ne_ok hv1
                | succ k2 =>
                    rw [Eval.applyValCore] at hv1
                    rw [hcons] at hv1
                    obtain ⟨K, hK⟩ := ih hv1 h2
                    exact ⟨K, by simpa using hK⟩
          · rw [if_neg hlt] at h1
            obtain ⟨w0, hw0, h1⟩ := except_bind_eq_ok h1
            obtain ⟨K, hK⟩ := applyMany_compose h1 h2
            have hn : d.params.length ≤ vs.length := by omega
            refine ⟨max k1 K + 1, ?_⟩
            rw [Eval.callDefn,
                if_neg (show ¬ (vs ++ w :: ws).length < d.params.length by
                  rw [List.length_append]; omega)]
            rw [show (d.params.map (·.uniq)).zip (vs ++ w :: ws)
                  = (d.params.map (·.uniq)).zip vs from
                zip_append_left (by rw [List.length_map]; omega)]
            rw [show (vs ++ w :: ws).drop d.params.length = vs.drop d.params.length ++ w :: ws from
                  List.drop_append_of_le_length hn]
            dsimp only
            rw [Eval.evalCore_mono (Nat.le_max_left k1 K) hw0, except_bind_ok]
            exact Eval.applyMany_mono (Nat.le_max_right k1 K) hK

/-! ## The zero value: representation (all-zero bits) and canonicality

`rep_zeroVal` needs `sizeOf` to be anti-monotone in the visited set
(the constructor-width computation runs with the type pushed onto the
visited list, while the field representations size with the empty
one). Canonicality of the zero value is checked computably (`vtyB`) —
the tuple family's constructor lists are not pinned by `denvOk`, so
the compiler checks the value it actually needs rather than assuming
a shape for every declaration. -/

/-- Transport a successful `mapM` along a pointwise implication. -/
private theorem mapM_ok_of_pointwise {α β : Type} {g g' : α → Except String β}
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
      rw [hg a b hb, except_bind_ok, ih hbs, except_bind_ok]
      exact h

private theorem any_eq_cons {t u : Ty} {vis vis' : List Ty}
    (hsub : ∀ w, vis'.any (Ty.eq · w) = true → vis.any (Ty.eq · w) = true)
    (h : (t :: vis').any (Ty.eq · u) = true) : (t :: vis).any (Ty.eq · u) = true := by
  simp only [List.any_cons, Bool.or_eq_true] at h ⊢
  rcases h with h | h
  · exact .inl h
  · exact .inr (hsub u h)

private theorem sizeOf_ctorWidth_anti (Δ : DEnv) :
    ∀ k,
      (∀ vis vis' t n, (∀ u, vis'.any (Ty.eq · u) = true → vis.any (Ty.eq · u) = true) →
        Δ.sizeOf k vis t = .ok n → Δ.sizeOf k vis' t = .ok n) ∧
      (∀ vis vis' t c n, (∀ u, vis'.any (Ty.eq · u) = true → vis.any (Ty.eq · u) = true) →
        Δ.ctorWidth k vis t c = .ok n → Δ.ctorWidth k vis' t c = .ok n) := by
  intro k
  induction k with
  | zero =>
      refine ⟨fun vis vis' t n _ h => ?_, fun vis vis' t c n _ h => ?_⟩
      · rw [DEnv.sizeOf] at h; exact error_ne_ok h
      · rw [DEnv.ctorWidth] at h; exact error_ne_ok h
  | succ k ihk =>
      obtain ⟨ihS, ihC⟩ := ihk
      constructor
      · intro vis vis' t n hsub h
        rw [DEnv.sizeOf] at h ⊢
        split at h
        · -- Vec
          split at h
          · obtain ⟨w, hw, h⟩ := except_bind_eq_ok h
            rw [ihS _ _ _ _ hsub hw, except_bind_ok]
            exact h
          · exact error_ne_ok h
        · exact h
        · exact h
        · exact h
        · split at h <;> rename_i htup
          · rw [if_pos htup]
            obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
            rw [mapM_ok_of_pointwise (fun a b hab => ihS _ _ _ _ hsub hab) hws, except_bind_ok]
            exact h
          · rw [if_neg htup]
            split at h <;> rename_i hvis
            · exact error_ne_ok h
            · rw [if_neg (show ¬ vis'.any (Ty.eq · t) = true from fun hc => hvis (hsub t hc))]
              split at h
              · obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
                rw [mapM_ok_of_pointwise
                      (fun a b hab => ihC _ _ _ _ _ (fun u hu => any_eq_cons hsub hu) hab) hws,
                    except_bind_ok]
                exact h
              · exact error_ne_ok h
        · exact h
        · exact error_ne_ok h
      · intro vis vis' t c n hsub h
        rw [DEnv.ctorWidth] at h ⊢
        split at h
        · split at h
          obtain ⟨sub, hsub2, h⟩ := except_bind_eq_ok h
          obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
          rw [hsub2, except_bind_ok,
              mapM_ok_of_pointwise (fun a b hab => ihS _ _ _ _ hsub hab) hws, except_bind_ok]
          exact h
        · exact h

private theorem sizeOf_anti {Δ : DEnv} {k : Nat} {vis vis' : List Ty} {t : Ty} {n : Nat}
    (hsub : ∀ u, vis'.any (Ty.eq · u) = true → vis.any (Ty.eq · u) = true)
    (h : Δ.sizeOf k vis t = .ok n) : Δ.sizeOf k vis' t = .ok n :=
  (sizeOf_ctorWidth_anti Δ k).1 vis vis' t n hsub h

private theorem vtyB_sound {Δ : DEnv} :
    ∀ {fuel : Nat} {v : Val} {t : Ty}, vtyB Δ fuel v t = true → VTy Δ v t := by
  intro fuel
  induction fuel with
  | zero =>
      intro v t h
      rw [vtyB] at h
      exact absurd h (by simp)
  | succ fuel ih =>
      intro v t h
      cases v with
      | vec elems =>
          rw [vtyB] at h
          split at h
          rotate_left
          · exact absurd h (by simp)
          rename_i n te heq
          split at h
          rotate_left
          · exact absurd h (by simp)
          rename_i k hk
          simp only [Bool.and_eq_true, beq_iff_eq, List.all_eq_true] at h
          exact VTy.vec heq hk h.1 (fun e he => ih (h.2 e he))
      | integer x =>
          rw [vtyB] at h
          split at h
          rotate_left
          · exact absurd h (by simp)
          rename_i heq
          exact VTy.integer heq
      | finite b i =>
          rw [vtyB] at h
          split at h
          rotate_left
          · exact absurd h (by simp)
          rename_i n heq
          simp only [Bool.and_eq_true, beq_iff_eq, decide_eq_true_eq] at h
          exact VTy.finite heq h.1 h.2
      | proxy =>
          rw [vtyB] at h
          split at h
          rotate_left
          · exact absurd h (by simp)
          rename_i args heq
          exact VTy.proxy heq rfl
      | con ty c fields =>
          rw [vtyB] at h
          simp only [Bool.and_eq_true] at h
          obtain ⟨hteq, h⟩ := h
          have hty : ty = t := teq_eq hteq
          subst hty
          split at h
          rotate_left
          · exact absurd h (by simp)
          rename_i sig hsig
          split at h
          rotate_left
          · exact absurd h (by simp)
          rename_i sub hsub
          simp only [Bool.and_eq_true, beq_iff_eq, List.all_eq_true] at h
          exact VTy.con hsig hsub h.1.1 (ctorOfB_sound h.1.2)
            (fun p hp => ih (h.2 p hp))
      | str s => rw [vtyB] at h; exact absurd h (by simp)
      | closL x env body => rw [vtyB] at h; exact absurd h (by simp)
      | closD f pre => rw [vtyB] at h; exact absurd h (by simp)

/-- `DEnv.zeroVal` is fuel-deterministic on success (from
monotonicity). -/
private theorem zeroVal_det {Δ : DEnv} {k k' : Nat} {t : Ty} {a b : Val}
    (h : Δ.zeroVal k t = .ok a) (h' : Δ.zeroVal k' t = .ok b) : a = b :=
  Except.ok.inj ((Δ.zeroVal_mono (Nat.le_max_left k k') h).symm.trans
    (Δ.zeroVal_mono (Nat.le_max_right k k') h'))

/-! ## Static-accessor inversions and `nbits` bounds -/

private theorem finBoundT_inv {who : String} {t : Ty} {k : Nat}
    (h : finBoundT who t = .ok k) :
    ∃ n, Ty.flatten t = (.con "Finite", [n]) ∧ Ty.evalNat n = some k := by
  rw [finBoundT] at h
  split at h
  case h_1 =>
      rename_i n heq
      split at h
      · rename_i kk hkk
        injection h with h
        subst h
        exact ⟨n, heq, hkk⟩
      · exact error_ne_ok h
  case h_2 => exact error_ne_ok h

private theorem vecLenElem_inv {who : String} {t : Ty} {k : Nat} {te : Ty}
    (h : vecLenElem who t = .ok (k, te)) :
    ∃ n, Ty.flatten t = (.con "Vec", [n, te]) ∧ Ty.evalNat n = some k := by
  rw [vecLenElem] at h
  split at h
  case h_1 =>
      rename_i n te' heq
      split at h
      · rename_i kk hkk
        injection h with h
        injection h with h1 h2
        subst h1; subst h2
        exact ⟨n, heq, hkk⟩
      · exact error_ne_ok h
  case h_2 => exact error_ne_ok h

private theorem proxyNatT_inv {who : String} {t : Ty} {k : Nat}
    (h : proxyNatT who t = .ok k) :
    ∃ n, Ty.flatten t = (.con "Proxy", [n]) ∧ Ty.evalNat n = some k := by
  rw [proxyNatT] at h
  split at h
  case h_1 =>
      rename_i n heq
      split at h
      · rename_i kk hkk
        injection h with h
        subst h
        exact ⟨n, heq, hkk⟩
      · exact error_ne_ok h
  case h_2 => exact error_ne_ok h

private theorem finBound_inv {who : String} {t : Ty} {k : Nat}
    (h : Eval.finBound who t = .ok k) :
    ∃ n, Ty.flatten t = (.con "Finite", [n]) ∧ Ty.evalNat n = some k := by
  rw [Eval.finBound] at h
  split at h
  case h_1 =>
      rename_i n heq
      split at h
      · rename_i kk hkk
        injection h with h
        subst h
        exact ⟨n, heq, hkk⟩
      · exact error_ne_ok h
  case h_2 => exact error_ne_ok h

private theorem proxyNatOf_inv {who : String} {t : Ty} {k : Nat}
    (h : Eval.proxyNatOf who t = .ok k) :
    ∃ n, Ty.flatten t = (.con "Proxy", [n]) ∧ Ty.evalNat n = some k := by
  rw [Eval.proxyNatOf] at h
  split at h
  case h_1 =>
      rename_i n heq
      split at h
      · rename_i kk hkk
        injection h with h
        subst h
        exact ⟨n, heq, hkk⟩
      · exact error_ne_ok h
  case h_2 => exact error_ne_ok h

private theorem intVal_inv {who : String} {v : Val} {x : BitVec 128}
    (h : Eval.intVal who v = .ok x) : v = .integer x := by
  cases v <;> rw [Eval.intVal] at h <;>
    first
      | (injection h with h; rw [h])
      | exact error_ne_ok h

private theorem finVal_inv {who : String} {v : Val} {b i : Nat}
    (h : Eval.finVal who v = .ok (b, i)) : v = .finite b i := by
  cases v <;> rw [Eval.finVal] at h <;>
    first
      | (injection h with h; injection h with h1 h2; rw [h1, h2])
      | exact error_ne_ok h

private theorem vecVal_inv {who : String} {v : Val} {xs : List Val}
    (h : Eval.vecVal who v = .ok xs) : v = .vec xs := by
  cases v <;> rw [Eval.vecVal] at h <;>
    first
      | (injection h with h; rw [h])
      | exact error_ne_ok h

private theorem domTy_inv {who : String} {doms : List Ty} {k : Nat} {t : Ty}
    (h : Eval.domTy who doms k = .ok t) : doms[k]? = some t := by
  rw [Eval.domTy] at h
  split at h
  · rename_i t' ht'
    injection h with h
    subst h
    exact ht'
  · exact error_ne_ok h

private theorem domTyT_inv {who : String} {doms : List Ty} {k : Nat} {t : Ty}
    (h : domTyT who doms k = .ok t) : doms[k]? = some t := by
  rw [domTyT] at h
  split at h
  · rename_i t' ht'
    injection h with h
    subst h
    exact ht'
  · exact error_ne_ok h

/-- Bind inversion as an iff, for shadow-free `rw … at h` use (a
`subst` after an expression-sourced `obtain` can resurrect shadowed
copies under the accessible name). -/
private theorem bind_ok_iff {α β : Type} {x : Except String α} {f : α → Except String β}
    {b : β} : (x >>= f) = .ok b ↔ ∃ a, x = .ok a ∧ f a = .ok b :=
  ⟨except_bind_eq_ok, fun ⟨a, hx, hf⟩ => by rw [hx, except_bind_ok]; exact hf⟩

/-- `n ≤ 2^m` bounds the tag width: `nbits n ≤ m`. -/
private theorem nbits_le_of_le_pow {n m : Nat} (h : n ≤ 2 ^ m) : nbits n ≤ m := by
  rw [nbits]
  by_cases h1 : n ≤ 1
  · rw [if_pos h1]; omega
  · rw [if_neg h1]
    have hn1 : n - 1 ≠ 0 := by omega
    have := (Nat.log2_lt hn1).mpr (show n - 1 < 2 ^ m by
      have := Nat.one_le_two_pow (n := m)
      omega)
    omega

/-- `2^wa ≤ n` forces `wa ≤ nbits n`. -/
private theorem le_nbits_of_pow_le {wa n : Nat} (h : 2 ^ wa ≤ n) : wa ≤ nbits n := by
  have h2 : (2 : Nat) ^ wa ≤ 2 ^ nbits n := Nat.le_trans h (nbits_le n)
  exact (Nat.pow_le_pow_iff_right (by omega)).mp h2

/-! ## Bit-level kit for the new rows -/

/-- `resizeNF`'s denotation is `setWidth` (given the operand's width). -/
private theorem resizeNF_eval {σ : String → BV} {m wa : Nat} {a : NF}
    (hw : (a.eval σ E).width = wa) :
    (resizeNF m wa a).eval σ E = ⟨m, (a.eval σ E).bits.setWidth m⟩ := by
  rw [resizeNF]
  by_cases h1 : m = wa
  · rw [if_pos h1]
    refine bv_ext (by dsimp only; omega) ?_
    intro i
    show (a.eval σ E).bits.getLsbD i = ((a.eval σ E).bits.setWidth m).getLsbD i
    rw [BitVec.getLsbD_setWidth]
    by_cases hi : i < m
    · simp [hi]
    · rw [decide_eq_false hi, Bool.false_and, getLsbD_ge _ (by omega)]
  · rw [if_neg h1]
    by_cases h2 : wa < m
    · rw [if_pos h2]
      rfl
    · rw [if_neg h2]
      rfl

/-- `⟨w, setWidth⟩` of a canonical `ofNat` at a large enough width is
the `ofNat` at the target width. -/
private theorem setWidth_ofNat {w m v : Nat} (hv : v < 2 ^ w) (hvm : v < 2 ^ m) :
    (BitVec.ofNat w v).setWidth m = BitVec.ofNat m v := by
  apply BitVec.eq_of_toNat_eq
  rw [BitVec.toNat_setWidth, BitVec.toNat_ofNat, BitVec.toNat_ofNat,
      Nat.mod_eq_of_lt hv]

/-- The width-1 slice at LSB index `i` is the §7.5.1 bit reading. -/
private theorem extract_one_b1 (x : BV) (i : Nat) :
    (⟨1, x.bits.extractLsb' i 1⟩ : BV) = Rwv.Hyle.Sem.b1 (x.bits.getLsbD i) := by
  cases hb : x.bits.getLsbD i with
  | false =>
      show (⟨1, x.bits.extractLsb' i 1⟩ : BV) = ⟨1, 0⟩
      congr 1
      apply BitVec.eq_of_getLsbD_eq
      intro j hj
      have hj0 : j = 0 := by omega
      subst hj0
      rw [BitVec.getLsbD_extractLsb']
      simpa using hb
  | true =>
      show (⟨1, x.bits.extractLsb' i 1⟩ : BV) = ⟨1, 1⟩
      congr 1
      apply BitVec.eq_of_getLsbD_eq
      intro j hj
      have hj0 : j = 0 := by omega
      subst hj0
      rw [BitVec.getLsbD_extractLsb']
      simpa using hb

/-- `catAll` of a replicated piece is the replicated bit vector. -/
private theorem catAll_replicate (r : BV) :
    ∀ k, catAll (List.replicate k r) = ⟨r.width * k, r.bits.replicate k⟩ := by
  intro k
  induction k with
  | zero =>
      refine bv_ext (by simp [catAll_nil, BV.nil, BV.ofNat]) ?_
      intro i
      rw [show List.replicate 0 r = [] from rfl, catAll_nil]
      show (BV.nil).bits.getLsbD i = _
      rw [BitVec.getLsbD_replicate]
      simp [BV.nil, BV.ofNat]
  | succ k ih =>
      rw [List.replicate_succ, catAll_cons, ih]
      refine bv_ext (by
        show r.width + r.width * k = r.width * (k + 1)
        rw [Nat.mul_succ]
        omega) ?_
      intro i
      rw [bvCat_getLsbD]
      show (if i < r.width * k then (BitVec.replicate k r.bits).getLsbD i
            else r.bits.getLsbD (i - r.width * k))
          = (BitVec.replicate (k + 1) r.bits).getLsbD i
      rw [BitVec.getLsbD_replicate, BitVec.getLsbD_replicate]
      by_cases hi : i < r.width * k
      · rw [if_pos hi, decide_eq_true hi, Bool.true_and,
            decide_eq_true (show i < r.width * (k + 1) by rw [Nat.mul_succ]; omega),
            Bool.true_and]
      · rw [if_neg hi]
        by_cases hik : i < r.width * (k + 1)
        · rw [decide_eq_true hik, Bool.true_and]
          have hlt : i - r.width * k < r.width := by
            rw [Nat.mul_succ] at hik
            omega
          have hmod : i % r.width = i - r.width * k := by
            have h2 : (i - r.width * k) + k * r.width = i := by
              rw [Nat.mul_comm k r.width]
              omega
            calc i % r.width
                = ((i - r.width * k) + k * r.width) % r.width := by rw [h2]
              _ = (i - r.width * k) % r.width := by rw [Nat.add_mul_mod_self_right]
              _ = i - r.width * k := Nat.mod_eq_of_lt hlt
          rw [hmod]
        · rw [decide_eq_false hik, Bool.false_and]
          exact getLsbD_ge _ (by rw [Nat.mul_succ] at hik; omega)

/-- Extracting an aligned element range out of a concatenation of
equal-width pieces (MSB-first: element `i` counts from the head). -/
private theorem catAll_extract_range {rs : List BV} {we : Nat}
    (hw : ∀ x ∈ rs, x.width = we) {i m : Nat} (him : i + m ≤ rs.length) :
    sliceBV (catAll rs) ((rs.length - i - m) * we) (m * we)
      = catAll ((rs.drop i).take m) := by
  have hsplit : rs = rs.take i ++ ((rs.drop i).take m ++ (rs.drop i).drop m) := by
    rw [List.take_append_drop, List.take_append_drop]
  have hmid : ((rs.drop i).take m).length = m := by
    rw [List.length_take, List.length_drop]
    omega
  have hwidth : ∀ (l : List BV), (∀ x ∈ l, x.width = we) →
      (catAll l).width = l.length * we := by
    intro l hl
    rw [catAll_width, sum_const (by
      intro a ha
      obtain ⟨x, hx, hxa⟩ := List.mem_map.mp ha
      rw [← hxa]
      exact hl x hx), List.length_map]
  have hwmid : ∀ x ∈ (rs.drop i).take m, x.width = we := fun x hx =>
    hw x (List.mem_of_mem_drop (List.mem_of_mem_take hx))
  have hwpost : ∀ x ∈ (rs.drop i).drop m, x.width = we := fun x hx =>
    hw x (List.mem_of_mem_drop (List.mem_of_mem_drop hx))
  have hpostlen : ((rs.drop i).drop m).length = rs.length - i - m := by
    rw [List.length_drop, List.length_drop]
  have hcat : catAll rs = bvCat (catAll (rs.take i))
      (bvCat (catAll ((rs.drop i).take m)) (catAll ((rs.drop i).drop m))) := by
    conv => lhs; rw [hsplit]
    rw [catAll_append, catAll_append]
  rw [hcat]
  rw [sliceBV_cat_low (by
    rw [bvCat_width, hwidth _ hwmid, hwidth _ hwpost, hmid, hpostlen]
    have h2 : (rs.length - i - m) * we + m * we ≤ (m + (rs.length - i - m)) * we := by
      rw [Nat.add_mul]
      omega
    omega)]
  rw [sliceBV_cat_high (by rw [hwidth _ hwpost, hpostlen]; exact Nat.le_refl _)]
  rw [hwidth _ hwpost, hpostlen, Nat.sub_self]
  have hmw : m * we = (catAll ((rs.drop i).take m)).width := by
    rw [hwidth _ hwmid, hmid]
  rw [hmw]
  exact sliceBV_all _

/-- Append a successful `mapM` split. -/
private theorem mapM_append_ok {α β : Type} {g : α → Except String β}
    {xs ys : List α} {as bs : List β}
    (hx : xs.mapM g = .ok as) (hy : ys.mapM g = .ok bs) :
    (xs ++ ys).mapM g = .ok (as ++ bs) := by
  induction xs generalizing as with
  | nil =>
      rw [List.mapM_nil, except_pure_def] at hx
      injection hx with hx
      subst hx
      simpa using hy
  | cons a rest ih =>
      rw [List.mapM_cons] at hx
      obtain ⟨b, hb, hx⟩ := except_bind_eq_ok hx
      obtain ⟨bs', hbs, hx⟩ := except_bind_eq_ok hx
      rw [except_pure_def] at hx
      injection hx with hx
      subst hx
      rw [List.cons_append, List.mapM_cons, hb, except_bind_ok, ih hbs, except_bind_ok,
          except_pure_def, List.cons_append]

/-- A canonical `Vec` value is a vector of the right length with
canonical elements (`denvOk` rules out `Vec`-headed constructor
values). -/
private theorem vty_vec_inv {Δ : DEnv} (hΔ : denvOk Δ = true) {v : Val} {t n te : Ty}
    {k : Nat} (hfl : Ty.flatten t = (.con "Vec", [n, te])) (hn : Ty.evalNat n = some k)
    (hv : VTy Δ v t) :
    ∃ elems, v = .vec elems ∧ elems.length = k ∧ ∀ e ∈ elems, VTy Δ e te := by
  cases hv with
  | vec hfl' hn' hlen helems =>
      rename_i elems n' te' kk
      rw [hfl] at hfl'
      have hp : n = n' ∧ te = te' := by simpa using hfl'
      obtain ⟨h1, h2⟩ := hp
      subst h1; subst h2
      rw [hn] at hn'
      injection hn' with hn'
      subst hn'
      exact ⟨elems, rfl, hlen, helems⟩
  | integer hfl' => rw [hfl] at hfl'; simp at hfl'
  | finite hfl' _ => rw [hfl] at hfl'; simp at hfl'
  | proxy hfl' _ => rw [hfl] at hfl'; simp at hfl'
  | con hsig hmatch hlen hctor hfields =>
      exfalso
      rw [ctorOf, hfl] at hctor
      dsimp only at hctor
      rw [if_neg (by simp [vec_not_tuple hΔ])] at hctor
      obtain ⟨cs, hcs, hmem⟩ := hctor
      rw [vec_abstract hΔ hcs] at hmem
      exact absurd hmem (by simp)

/-- Dissect the representation of a vector value. -/
private theorem rep_vec_inv {Δ : DEnv} {elems : List Val} {k : Nat} {bv : BV}
    (h : Val.rep Δ k (.vec elems) = .ok bv) :
    ∃ k' rs, k = k' + 1 ∧ elems.mapM (Val.rep Δ k') = .ok rs ∧ bv = catAll rs := by
  cases k with
  | zero => rw [Val.rep] at h; exact error_ne_ok h
  | succ k =>
      rw [Val.rep] at h
      obtain ⟨rs, hrs, h⟩ := except_bind_eq_ok h
      rw [mapM_attach_erase] at hrs
      rw [except_pure_def] at h
      injection h with h
      subst h
      exact ⟨k, rs, rfl, hrs, bvConcat_eq rs⟩

/-- A canonical vector's element representations all have the element
size (given `sizeOf` of the element type). -/
private theorem elem_widths {Δ : DEnv} {elems : List Val} {te : Ty} {we : Nat} {kf ks : Nat}
    {rs : List BV} (helems : ∀ e ∈ elems, VTy Δ e te)
    (hrs : elems.mapM (Val.rep Δ kf) = .ok rs)
    (hse : Δ.sizeOf ks [] te = .ok we) :
    ∀ x ∈ rs, x.width = we := by
  obtain ⟨hlen, hpt⟩ := mapM_ok_idx hrs
  intro x hx
  obtain ⟨j, hj, hxj⟩ := List.getElem_of_mem hx
  obtain ⟨hj', hrepj⟩ := hpt j (by omega)
  subst hxj
  exact vty_rep_width (helems elems[j] (List.getElem_mem _)) hrepj hse


/-! ## `mapM` sublist transports -/

private theorem mapM_take_ok {α β : Type} {g : α → Except String β} :
    ∀ {xs : List α} {rs : List β} {j : Nat}, xs.mapM g = .ok rs →
      (xs.take j).mapM g = .ok (rs.take j) := by
  intro xs
  induction xs with
  | nil =>
      intro rs j h
      rw [List.mapM_nil, except_pure_def] at h
      injection h with h
      subst h
      simp only [List.take_nil, List.mapM_nil]
      rw [except_pure_def]
  | cons a as ih =>
      intro rs j h
      rw [List.mapM_cons] at h
      obtain ⟨b, hb, h⟩ := except_bind_eq_ok h
      obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
      rw [except_pure_def] at h
      injection h with h
      subst h
      match j with
      | 0 =>
          simp only [List.take_zero, List.mapM_nil]
          rw [except_pure_def]
      | j + 1 =>
          rw [List.take_succ_cons, List.take_succ_cons, List.mapM_cons, hb, except_bind_ok,
              ih hbs, except_bind_ok, except_pure_def]

private theorem mapM_drop_ok {α β : Type} {g : α → Except String β} :
    ∀ {xs : List α} {rs : List β} {j : Nat}, xs.mapM g = .ok rs →
      (xs.drop j).mapM g = .ok (rs.drop j) := by
  intro xs
  induction xs with
  | nil =>
      intro rs j h
      rw [List.mapM_nil, except_pure_def] at h
      injection h with h
      subst h
      simp only [List.drop_nil, List.mapM_nil]
      rw [except_pure_def]
  | cons a as ih =>
      intro rs j h
      rw [List.mapM_cons] at h
      obtain ⟨b, hb, h⟩ := except_bind_eq_ok h
      obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
      rw [except_pure_def] at h
      injection h with h
      subst h
      match j with
      | 0 =>
          rw [List.drop_zero, List.drop_zero, List.mapM_cons, hb, except_bind_ok,
              hbs, except_bind_ok, except_pure_def]
      | j + 1 =>
          rw [List.drop_succ_cons, List.drop_succ_cons]
          exact ih hbs

private theorem mapM_reverse_ok {α β : Type} {g : α → Except String β} :
    ∀ {xs : List α} {rs : List β}, xs.mapM g = .ok rs →
      xs.reverse.mapM g = .ok rs.reverse := by
  intro xs
  induction xs with
  | nil =>
      intro rs h
      rw [List.mapM_nil, except_pure_def] at h
      injection h with h
      subst h
      simp only [List.reverse_nil, List.mapM_nil]
      rw [except_pure_def]
  | cons a as ih =>
      intro rs h
      rw [List.mapM_cons] at h
      obtain ⟨b, hb, h⟩ := except_bind_eq_ok h
      obtain ⟨bs, hbs, h⟩ := except_bind_eq_ok h
      rw [except_pure_def] at h
      injection h with h
      subst h
      rw [List.reverse_cons, List.reverse_cons]
      exact mapM_append_ok (ih hbs)
        (by rw [List.mapM_cons, hb, except_bind_ok, List.mapM_nil, except_pure_def,
                except_bind_ok, except_pure_def])

/-- Any two zero-width bit vectors are equal. -/
private theorem bv_width0_eq {x y : BV} (hx : x.width = 0) (hy : y.width = 0) : x = y := by
  refine bv_ext (by omega) ?_
  intro i
  rw [getLsbD_ge x.bits (by omega), getLsbD_ge y.bits (by omega)]

/-- The element slice at a dynamic index, as the compiled shift
computes it: `sliceBV` of the concatenation. -/
private theorem slice_singleton {rs : List BV} {we : Nat}
    (hw : ∀ x ∈ rs, x.width = we) {i : Nat} (hi : i < rs.length) :
    sliceBV (catAll rs) ((rs.length - i - 1) * we) we = rs[i] := by
  have h := catAll_extract_range hw (i := i) (m := 1) (by omega)
  rw [Nat.one_mul] at h
  rw [h]
  rw [show (rs.drop i).take 1 = [rs[i]] by
    rw [← List.getElem_cons_drop hi]
    rfl]
  rw [catAll_cons, catAll_nil, bvCat_zero_right rfl]

/-- `(len − i − m) · we = len·we − i·we − m·we` (aligned offsets). -/
private theorem sub_mul_offsets {len i m we : Nat} (h : i + m ≤ len) :
    (len - i - m) * we = len * we - i * we - m * we := by
  have hsplit : len * we = (len - i - m) * we + i * we + m * we := by
    rw [← Nat.add_mul, ← Nat.add_mul]
    congr 1
    omega
  omega

set_option maxHeartbeats 6400000 in
/-- Soundness of the extended row table: the new Finite/Vec/NatVal
rows directly, everything else by delegation to `cprim_sound`. -/
private theorem cprimF_sound {Δ : DEnv} {dmap : HashMap Int Defn} (hΔ : denvOk Δ = true)
    {σ : String → BV} {szf : Nat} {pty : Ty} {b : Builtin} {pas : List (NF × Ty)}
    {nf : NF} {ty : Ty} {efuel : Nat} {vs : List Val} {v : Val}
    (hc : cprimF Δ szf pty b pas = .ok (nf, ty))
    (hev : Eval.evalBuiltin ⟨Δ, dmap⟩ efuel pty b vs E = .ok v)
    (hlen : vs.length = pas.length)
    (hargs : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
       VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E)) :
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ E) := by
  have hev0 := hev
  cases efuel with
  | zero => rw [Eval.evalBuiltin] at hev; exact error_ne_ok hev
  | succ efuel =>
  rcases hfa : Ty.flattenArrow pty with ⟨doms, res⟩
  rw [Eval.evalBuiltin] at hev
  rw [hfa] at hev
  cases b <;>
    first
    | (dsimp only [cprimF] at hc
       exact cprim_sound hΔ hc hev0 hlen hargs)
    | skip
  all_goals
    dsimp only [cprimF] at hc
    rw [hfa] at hc
    dsimp only at hc
  case finite =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowFinite] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      cases a with
      | var w0 x0 => exact error_ne_ok hc
      | prim1 op0 a0 => exact error_ne_ok hc
      | prim2 op0 a0 b0 => exact error_ne_ok hc
      | cat a0 b0 => exact error_ne_ok hc
      | slice i0 w0 e0 => exact error_ne_ok hc
      | ite c0 t0 e0 => exact error_ne_ok hc
      | xcall w0 x0 a0 => exact error_ne_ok hc
      | lit lv =>
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hvn
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨n', hn', hev⟩ := except_bind_eq_ok hev
      obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
      split at hev
      rotate_left
      · exact error_ne_ok hev
      rename_i hxn
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      -- link the two bound computations
      obtain ⟨nt, hflr, hnt⟩ := finBoundT_inv hn
      obtain ⟨nt', hflr', hnt'⟩ := finBound_inv hn'
      rw [hflr] at hflr'
      have hnn : n = n' := by
        have : nt = nt' := by simpa using hflr'
        subst this
        rw [hnt] at hnt'
        exact Option.some.inj hnt'
      subst hnn
      -- the literal value is the evaluator's numeral
      have hv1 : v1 = .integer x := intVal_inv hx
      obtain ⟨ka, hka⟩ := h0.2
      rw [hv1] at hka
      have hlv : lv = ⟨128, x⟩ := by
        have hxa : (NF.lit lv).eval σ E = ⟨128, x⟩ := by
          cases ka with
          | zero => rw [Val.rep] at hka; exact error_ne_ok hka
          | succ ka =>
              rw [Val.rep] at hka
              injection hka with hka
              rw [← hka]
        exact hxa
      constructor
      · exact VTy.finite hflr hnt hxn
      · refine ⟨1, ?_⟩
        rw [Val.rep]
        show Except.ok (⟨nbits n, BitVec.ofNat (nbits n) x.toNat⟩ : BV)
          = Except.ok ((NF.lit ⟨nbits n, BitVec.ofNat (nbits n) lv.nat⟩).eval σ E)
        rw [hlv]
        rfl
  case finiteMinBound =>
      rcases pas with _ | ⟨p1, r1⟩ <;>
        rw [rowFinBound] at hc <;> try exact error_ne_ok hc
      have hvs : vs = [] := List.length_eq_zero_iff.mp (by simpa using hlen)
      subst hvs
      dsimp only at hev
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hn1
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨n', hn', hev⟩ := except_bind_eq_ok hev
      split at hev
      rotate_left
      · exact error_ne_ok hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      obtain ⟨nt, hflr, hnt⟩ := finBoundT_inv hn
      obtain ⟨nt', hflr', hnt'⟩ := finBound_inv hn'
      rw [hflr] at hflr'
      have hnn : n = n' := by
        have : nt = nt' := by simpa using hflr'
        subst this
        rw [hnt] at hnt'
        exact Option.some.inj hnt'
      subst hnn
      exact ⟨VTy.finite hflr hnt (by omega), 1, by rw [Val.rep]; rfl⟩
  case finiteMaxBound =>
      rcases pas with _ | ⟨p1, r1⟩ <;>
        rw [rowFinBound] at hc <;> try exact error_ne_ok hc
      have hvs : vs = [] := List.length_eq_zero_iff.mp (by simpa using hlen)
      subst hvs
      dsimp only at hev
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hn1
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨n', hn', hev⟩ := except_bind_eq_ok hev
      split at hev
      rotate_left
      · exact error_ne_ok hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      obtain ⟨nt, hflr, hnt⟩ := finBoundT_inv hn
      obtain ⟨nt', hflr', hnt'⟩ := finBound_inv hn'
      rw [hflr] at hflr'
      have hnn : n = n' := by
        have : nt = nt' := by simpa using hflr'
        subst this
        rw [hnt] at hnt'
        exact Option.some.inj hnt'
      subst hnn
      exact ⟨VTy.finite hflr hnt (by omega), 1, by rw [Val.rep]; rfl⟩
  case toFinite =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowToFinite] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hpow
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨n', hn', hev⟩ := except_bind_eq_ok hev
      obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
      split at hev
      rotate_left
      · exact error_ne_ok hev
      rename_i hxw
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      obtain ⟨nt, hflr, hnt⟩ := finBoundT_inv hn
      obtain ⟨nt', hflr', hnt'⟩ := finBound_inv hn'
      rw [hflr] at hflr'
      have hnn : n = n' := by
        have : nt = nt' := by simpa using hflr'
        subst this
        rw [hnt] at hnt'
        exact Option.some.inj hnt'
      subst hnn
      rw [Eval.valToBits] at hx
      obtain ⟨ka, hka⟩ := h0.2
      have hxa : x = a.eval σ E := rep_det hx hka
      subst hxa
      obtain ⟨na, tea, hfla, hba, hna⟩ := vecBoolLen_inv hwa
      have hwaa : (a.eval σ E).width = wa :=
        vty_vecBool_rep_width hΔ hfla hna hba h0.1 hka
      have hle : wa ≤ nbits n := le_nbits_of_pow_le (by omega)
      have hnatlt : (a.eval σ E).nat < 2 ^ nbits n := by
        have h1 : (a.eval σ E).nat < 2 ^ wa := by
          show (a.eval σ E).bits.toNat < 2 ^ wa
          rw [← hwaa]
          exact (a.eval σ E).bits.isLt
        exact Nat.lt_of_lt_of_le h1 (Nat.pow_le_pow_right (by omega) hle)
      constructor
      · exact VTy.finite hflr hnt (Nat.lt_of_lt_of_le (a.eval σ E).bits.isLt hxw)
      · refine ⟨1, ?_⟩
        rw [Val.rep]
        rw [resizeNF_eval hwaa]
        show Except.ok (⟨nbits n, BitVec.ofNat (nbits n) (a.eval σ E).nat⟩ : BV) = _
        congr 2
        apply BitVec.eq_of_toNat_eq
        rw [BitVec.toNat_ofNat, BitVec.toNat_setWidth,
            Nat.mod_eq_of_lt hnatlt]
        exact (Nat.mod_eq_of_lt hnatlt).symm ▸ rfl
  case natVal =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowNatVal] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      dsimp only at hev
      obtain ⟨pt, hpt, hc⟩ := except_bind_eq_ok hc
      obtain ⟨k, hk, hc⟩ := except_bind_eq_ok hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hflr
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨pt', hpt', hev⟩ := except_bind_eq_ok hev
      obtain ⟨k', hk', hev⟩ := except_bind_eq_ok hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      -- link the Proxy index
      have hpteq : pt = pt' := by
        have := (domTyT_inv hpt).symm.trans (domTy_inv hpt')
        exact Option.some.inj this
      subst hpteq
      obtain ⟨n1, hfl1, hn1⟩ := proxyNatT_inv hk
      obtain ⟨n2, hfl2, hn2⟩ := proxyNatOf_inv hk'
      rw [hfl1] at hfl2
      have hkk : k = k' := by
        have : n1 = n2 := by simpa using hfl2
        subst this
        rw [hn1] at hn2
        exact Option.some.inj hn2
      subst hkk
      exact ⟨VTy.integer hflr, 1, by rw [Val.rep]; rfl⟩
  case toFiniteMod =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowToFiniteMod] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      obtain ⟨n', hn', hev⟩ := except_bind_eq_ok hev
      obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
      obtain ⟨nt, hflr, hnt⟩ := finBoundT_inv hn
      obtain ⟨nt', hflr', hnt'⟩ := finBound_inv hn'
      rw [hflr] at hflr'
      have hnn : n = n' := by
        have : nt = nt' := by simpa using hflr'
        subst this
        rw [hnt] at hnt'
        exact Option.some.inj hnt'
      subst hnn
      rw [Eval.valToBits] at hx
      obtain ⟨ka, hka⟩ := h0.2
      have hxa : x = a.eval σ E := rep_det hx hka
      subst hxa
      obtain ⟨na, tea, hfla, hba, hna⟩ := vecBoolLen_inv hwa
      have hwaa : (a.eval σ E).width = wa :=
        vty_vecBool_rep_width hΔ hfla hna hba h0.1 hka
      have hxwa : (a.eval σ E).nat < 2 ^ wa := by
        show (a.eval σ E).bits.toNat < 2 ^ wa
        rw [← hwaa]
        exact (a.eval σ E).bits.isLt
      split at hc
      · -- 2^wa ≤ n: pure resize, and the modulus is the identity
        rename_i hpow
        injection hc with hc
        injection hc with hnf hty
        subst hnf; subst hty
        split at hev
        · rename_i hn0
          exfalso
          rw [hn0] at hpow
          have : 0 < 2 ^ wa := Nat.two_pow_pos wa
          omega
        · rw [except_pure_def] at hev
          injection hev with hev
          subst hev
          have hxlt : (a.eval σ E).nat < n := Nat.lt_of_lt_of_le hxwa hpow
          have hnatlt : (a.eval σ E).nat < 2 ^ nbits n :=
            Nat.lt_of_lt_of_le hxlt (nbits_le n)
          constructor
          · exact VTy.finite hflr hnt
              (Nat.mod_lt _ (by have := Nat.two_pow_pos wa; omega))
          · refine ⟨1, ?_⟩
            rw [Val.rep, resizeNF_eval hwaa]
            show Except.ok (⟨nbits n, BitVec.ofNat (nbits n) ((a.eval σ E).nat % n)⟩ : BV) = _
            rw [Nat.mod_eq_of_lt hxlt]
            congr 2
            apply BitVec.eq_of_toNat_eq
            rw [BitVec.toNat_ofNat, BitVec.toNat_setWidth,
                Nat.mod_eq_of_lt hnatlt]
            exact (Nat.mod_eq_of_lt hnatlt).symm ▸ rfl
      · -- n < 2^wa: the explicit modulus chain
        rename_i hpow
        split at hc
        · exact error_ne_ok hc
        rename_i hn0c
        injection hc with hc
        injection hc with hnf hty
        subst hnf; subst hty
        have hnb : nbits n ≤ wa := nbits_le_of_le_pow (by omega)
        have hw : max wa (nbits n) = wa := Nat.max_eq_left hnb
        rw [hw]
        -- the inner resize is width-preserving; name the umod node's value
        have hinner : (resizeNF wa wa a).eval σ E = ⟨wa, (a.eval σ E).bits.setWidth wa⟩ :=
          resizeNF_eval hwaa
        have humod : (NF.prim2 .umod (resizeNF wa wa a) (.lit ⟨wa, BitVec.ofNat wa n⟩)).eval σ E
            = ⟨wa, ((a.eval σ E).bits.setWidth wa) % ((BitVec.ofNat wa n).setWidth wa)⟩ := by
          show (match Rwv.Hyle.Sem.evalOp .umod
                  [(resizeNF wa wa a).eval σ E, (⟨wa, BitVec.ofNat wa n⟩ : BV)] with
                | .ok v => v
                | .error _ => BV.nil) = _
          rw [hinner]
          rfl
        have houter : (resizeNF (nbits n) wa
              (NF.prim2 .umod (resizeNF wa wa a) (.lit ⟨wa, BitVec.ofNat wa n⟩))).eval σ E
            = ⟨nbits n, (((a.eval σ E).bits.setWidth wa) % ((BitVec.ofNat wa n).setWidth wa)).setWidth (nbits n)⟩ := by
          rw [resizeNF_eval (by rw [humod]), humod]
        split at hev
        · -- n = 0: rejected at compile time
          rename_i hn0
          exact absurd hn0 hn0c
        · rename_i hn0
          rw [except_pure_def] at hev
          injection hev with hev
          subst hev
          have hn1 : 0 < n := Nat.pos_of_ne_zero hn0
          have hmodlt : (a.eval σ E).nat % n < 2 ^ nbits n :=
            Nat.lt_of_lt_of_le (Nat.mod_lt _ hn1) (nbits_le n)
          constructor
          · exact VTy.finite hflr hnt (Nat.mod_lt _ hn1)
          · refine ⟨1, ?_⟩
            rw [Val.rep, houter]
            congr 2
            apply BitVec.eq_of_toNat_eq
            rw [BitVec.toNat_ofNat, BitVec.toNat_setWidth, BitVec.toNat_umod,
                BitVec.toNat_setWidth, BitVec.toNat_setWidth, BitVec.toNat_ofNat,
                Nat.mod_eq_of_lt hmodlt]
            have h1 : (a.eval σ E).bits.toNat % 2 ^ wa = (a.eval σ E).nat :=
              Nat.mod_eq_of_lt hxwa
            have h2 : n % 2 ^ wa % 2 ^ wa = n := by
              rw [Nat.mod_eq_of_lt (Nat.mod_lt _ (Nat.two_pow_pos wa)),
                  Nat.mod_eq_of_lt (by omega)]
            rw [h1, h2]
            exact (Nat.mod_eq_of_lt hmodlt).symm ▸ rfl
  case fromFinite =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowFromFinite] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      obtain ⟨m, hm, hc⟩ := except_bind_eq_ok hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hle
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      obtain ⟨m', hm', hev⟩ := except_bind_eq_ok hev
      obtain ⟨bi, hbi, hev⟩ := except_bind_eq_ok hev
      obtain ⟨bound, i⟩ := bi
      split at hev
      rotate_left
      · exact error_ne_ok hev
      rename_i hble
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      -- widths agree
      obtain ⟨lt, et, hflr, hbet, hmlt⟩ := vecBoolLen_inv hm
      obtain ⟨lt', et', hflr2, hm2⟩ := vecLen_inv hm'
      rw [hflr] at hflr2
      have hmm : m = m' := by
        have hp : lt = lt' ∧ et = et' := by simpa using hflr2
        obtain ⟨h1, _⟩ := hp
        subst h1
        rw [hmlt] at hm2
        exact Option.some.inj hm2
      subst hmm
      -- the argument is a canonical Finite at the declared bound
      have hv1 : v1 = .finite bound i := finVal_inv hbi
      obtain ⟨ntt, hflta, hnta⟩ := finBoundT_inv hn
      have hbound : bound = n := by
        have hvt := h0.1
        rw [hv1] at hvt
        cases hvt with
        | finite hfl2 hn2 _hlt2 =>
            rename_i nvar
            rw [hflta] at hfl2
            have hp : ntt = nvar := by simpa using hfl2
            rw [← hp] at hn2
            rw [hnta] at hn2
            exact (Option.some.inj hn2).symm
      subst hbound
      obtain ⟨ka, hka⟩ := h0.2
      rw [hv1] at hka
      have haev : a.eval σ E = ⟨nbits bound, BitVec.ofNat (nbits bound) i⟩ := by
        cases ka with
        | zero => rw [Val.rep] at hka; exact error_ne_ok hka
        | succ ka =>
            rw [Val.rep] at hka
            injection hka with hka
            rw [← hka]
      -- the tracked canonicality bound makes the resize value-preserving
      have hilt : i < bound := by
        have hvt := h0.1
        rw [hv1] at hvt
        cases hvt with
        | finite _ _ hlt2 => exact hlt2
      constructor
      · exact vty_bitsToVec hΔ hflr hmlt hbet rfl
      · refine ⟨3, ?_⟩
        rw [rep_bitsToVec hΔ (BV.ofNat m i) 0]
        rw [resizeNF_eval (by rw [haev]), haev]
        show Except.ok (BV.ofNat m i)
          = Except.ok (⟨m, (BitVec.ofNat (nbits bound) i).setWidth m⟩ : BV)
        have hi2 : i < 2 ^ nbits bound := Nat.lt_of_lt_of_le hilt (nbits_le bound)
        have him : i < 2 ^ m := Nat.lt_of_lt_of_le hilt hble
        show Except.ok (⟨m, BitVec.ofNat m i⟩ : BV) = _
        congr 2
        apply BitVec.eq_of_toNat_eq
        rw [BitVec.toNat_setWidth, BitVec.toNat_ofNat, BitVec.toNat_ofNat,
            Nat.mod_eq_of_lt hi2, Nat.mod_eq_of_lt him]
  case vecReplicate =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowVecReplicate] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      rw [bind_ok_iff] at hc
      obtain ⟨nte, hnte, hc⟩ := hc
      obtain ⟨n, te⟩ := nte
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hteq
      rw [bind_ok_iff] at hc
      obtain ⟨sz, hsz, hc⟩ := hc
      rw [bind_ok_iff] at hev
      obtain ⟨n', hn', hev⟩ := hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      obtain ⟨ntr, hflr, hntr⟩ := vecLenElem_inv hnte
      obtain ⟨ntr', ter', hflr2, hn2⟩ := vecLen_inv hn'
      rw [hflr] at hflr2
      have hnn : n = n' := by
        have hp : ntr = ntr' ∧ te = ter' := by simpa using hflr2
        obtain ⟨h1, _⟩ := hp
        subst h1
        rw [hntr] at hn2
        exact Option.some.inj hn2
      subst hnn
      have hta : ta = te := teq_eq hteq
      subst hta
      obtain ⟨ka, hka⟩ := h0.2
      -- the element size
      cases szf with
      | zero => rw [DEnv.sizeOf] at hsz; exact error_ne_ok hsz
      | succ szf =>
      obtain ⟨kk, we, hkk, hwe, hszw⟩ := sizeOf_inv_vec hflr hsz
      have hkkn : n = kk := by
        rw [hntr] at hkk
        exact Option.some.inj hkk
      subst hkkn
      have hrw : (a.eval σ E).width = we := vty_rep_width h0.1 hka hwe
      have hmap : (List.replicate n v1).mapM (Val.rep Δ ka)
          = .ok (List.replicate n (a.eval σ E)) := by
        rw [mapM_ok_of_forall (f := fun _ => a.eval σ E)
              (fun b hb => by rw [List.eq_of_mem_replicate hb]; exact hka),
            List.map_replicate]
      have hVTy : VTy Δ (Val.vec (List.replicate n v1)) res := by
        refine VTy.vec hflr hntr (by rw [List.length_replicate]) ?_
        intro e he
        rw [List.eq_of_mem_replicate he]
        exact h0.1
      split at hc
      · rename_i hz
        injection hc with hc
        injection hc with hnf hty
        subst hnf; subst hty
        refine ⟨hVTy, ka + 1, ?_⟩
        rw [Val.rep, mapM_attach_erase, hmap, except_bind_ok, except_pure_def]
        congr 1
        rw [bvConcat_eq, catAll_replicate]
        refine bv_width0_eq ?_ rfl
        show (a.eval σ E).width * n = 0
        rw [hrw]
        rcases hz with hz | hz
        · rw [hz, Nat.mul_zero]
        · rw [hz] at hszw
          rw [Nat.mul_comm]
          omega
      · rename_i hz
        injection hc with hc
        injection hc with hnf hty
        subst hnf; subst hty
        refine ⟨hVTy, ka + 1, ?_⟩
        rw [Val.rep, mapM_attach_erase, hmap, except_bind_ok, except_pure_def]
        congr 1
        rw [bvConcat_eq, catAll_replicate]
        rfl

  case vecConcat =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b', tb⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        rw [rowVecConcat] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, v2, rfl⟩ := list_len2 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      have h1 := hargs 1 (by simp) (by simp)
      simp only [List.getElem_cons_zero, List.getElem_cons_succ] at h0 h1
      dsimp only at hev
      rw [bind_ok_iff] at hc
      obtain ⟨ntr0, hntr0, hc⟩ := hc
      obtain ⟨nr, ter⟩ := ntr0
      dsimp only at hc
      rw [bind_ok_iff] at hc
      obtain ⟨nte1, hnte1, hc⟩ := hc
      obtain ⟨n1, te1⟩ := nte1
      dsimp only at hc
      rw [bind_ok_iff] at hc
      obtain ⟨nte2, hnte2, hc⟩ := hc
      obtain ⟨n2, te2⟩ := nte2
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hcond
      simp only [Bool.and_eq_true, beq_iff_eq] at hcond
      rw [bind_ok_iff] at hc
      obtain ⟨sa, hsa, hc⟩ := hc
      rw [bind_ok_iff] at hc
      obtain ⟨sb, hsb, hc⟩ := hc
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      rw [bind_ok_iff] at hev
      obtain ⟨xs, hxs, hev⟩ := hev
      rw [bind_ok_iff] at hev
      obtain ⟨ys, hys, hev⟩ := hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      have hv1 : v1 = .vec xs := vecVal_inv hxs
      have hv2 : v2 = .vec ys := vecVal_inv hys
      obtain ⟨ntrT, hflr, hntr⟩ := vecLenElem_inv hntr0
      obtain ⟨nt1, hfla, hna⟩ := vecLenElem_inv hnte1
      obtain ⟨nt2, hflb, hnb⟩ := vecLenElem_inv hnte2
      have hte1 : te1 = ter := teq_eq hcond.1.1
      have hte2 : te2 = ter := teq_eq hcond.1.2
      subst hte1; subst hte2
      -- canonicality of the two vectors
      obtain ⟨xs0, hxs0, hxlen, hxel⟩ := vty_vec_inv hΔ hfla hna h0.1
      rw [hv1] at hxs0
      injection hxs0 with hxs0
      subst hxs0
      obtain ⟨ys0, hys0, hylen, hyel⟩ := vty_vec_inv hΔ hflb hnb h1.1
      rw [hv2] at hys0
      injection hys0 with hys0
      subst hys0
      constructor
      · refine VTy.vec hflr hntr (by rw [List.length_append]; omega) ?_
        intro e he
        rcases List.mem_append.mp he with he | he
        · exact hxel e he
        · exact hyel e he
      · -- representations concatenate
        obtain ⟨ka, hka⟩ := h0.2
        obtain ⟨kb, hkb⟩ := h1.2
        rw [hv1] at hka
        rw [hv2] at hkb
        obtain ⟨ka', rs1, hka1, hmap1, haev⟩ := rep_vec_inv hka
        obtain ⟨kb', rs2, hkb1, hmap2, hbev⟩ := rep_vec_inv hkb
        refine ⟨max ka' kb' + 1, ?_⟩
        rw [Val.rep, mapM_attach_erase,
            mapM_append_ok (mapM_rep_mono (Nat.le_max_left ka' kb') hmap1)
              (mapM_rep_mono (Nat.le_max_right ka' kb') hmap2),
            except_bind_ok, except_pure_def]
        congr 1
        have hwa : (a.eval σ E).width = sa := vty_rep_width (hv1 ▸ h0.1) hka hsa
        have hwb : (b'.eval σ E).width = sb := vty_rep_width (hv2 ▸ h1.1) hkb hsb
        rw [catNF_eval σ _ (by
          intro p hp
          rcases List.mem_cons.mp hp with rfl | hp
          · exact hwa
          rcases List.mem_cons.mp hp with rfl | hp
          · exact hwb
          exact absurd hp (by simp))]
        rw [show ([(a, sa), (b', sb)] : List (NF × Nat)).map (fun p => p.1.eval σ E)
              = [a.eval σ E, b'.eval σ E] from rfl]
        rw [show catAll [a.eval σ E, b'.eval σ E] = bvCat (a.eval σ E) (b'.eval σ E) by
          rw [catAll_cons, catAll_cons, catAll_nil,
              show bvCat (b'.eval σ E) BV.nil = b'.eval σ E from bvCat_zero_right rfl]]
        rw [bvConcat_eq, catAll_append, haev, hbev]
  case vecReverse =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowVecReverse] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, rfl⟩ := list_len1 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      rw [bind_ok_iff] at hc
      obtain ⟨ntr0, hntr0, hc⟩ := hc
      obtain ⟨nr, ter⟩ := ntr0
      dsimp only at hc
      rw [bind_ok_iff] at hc
      obtain ⟨nte1, hnte1, hc⟩ := hc
      obtain ⟨n1, te1⟩ := nte1
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hcond
      simp only [Bool.and_eq_true, beq_iff_eq] at hcond
      rw [bind_ok_iff] at hc
      obtain ⟨se, hse, hc⟩ := hc
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      rw [bind_ok_iff] at hev
      obtain ⟨xs, hxs, hev⟩ := hev
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      have hv1 : v1 = .vec xs := vecVal_inv hxs
      obtain ⟨ntrT, hflr, hntr⟩ := vecLenElem_inv hntr0
      obtain ⟨nt1, hfla, hna⟩ := vecLenElem_inv hnte1
      have hte1 : te1 = ter := teq_eq hcond.1
      subst hte1
      have hnr : n1 = nr := hcond.2.symm
      subst hnr
      obtain ⟨xs0, hxs0, hxlen, hxel⟩ := vty_vec_inv hΔ hfla hna h0.1
      rw [hv1] at hxs0
      injection hxs0 with hxs0
      subst hxs0
      constructor
      · refine VTy.vec hflr hntr (by rw [List.length_reverse]; omega) ?_
        intro e he
        exact hxel e (List.mem_reverse.mp he)
      · obtain ⟨ka, hka⟩ := h0.2
        rw [hv1] at hka
        obtain ⟨ka', rs, hka1, hmap, haev⟩ := rep_vec_inv hka
        have hrslen : rs.length = n1 := by
          rw [(mapM_ok_idx hmap).1, hxlen]
        have hwidths : ∀ x ∈ rs, x.width = se := elem_widths hxel hmap hse
        refine ⟨ka' + 1, ?_⟩
        rw [Val.rep, mapM_attach_erase, mapM_reverse_ok hmap, except_bind_ok,
            except_pure_def]
        congr 1
        rw [bvConcat_eq]
        rw [catNF_eval σ _ (by
          intro p hp
          obtain ⟨j, hj, hpj⟩ := List.mem_map.mp hp
          rw [← hpj]
          dsimp only
          rw [sliceNF_eval]
          rfl)]
        rw [List.map_map]
        have hpiece : ∀ k (hk : k < n1),
            (sliceNF (k * se) se a).eval σ E = rs[n1 - 1 - k]'(by rw [hrslen]; omega) := by
          intro k hk
          rw [sliceNF_eval, haev]
          have h9 := slice_singleton hwidths (i := n1 - 1 - k) (by omega)
          rw [show (rs.length - (n1 - 1 - k) - 1) * se = k * se by
            rw [hrslen]
            congr 1
            omega] at h9
          exact h9
        have hlist : (List.range n1).map ((fun p : NF × Nat => p.1.eval σ E) ∘
              fun k => (sliceNF (k * se) se a, se))
            = rs.reverse.map id := by
          refine List.ext_getElem (by simp [hrslen]) ?_
          intro j hj1 hj2
          simp only [List.getElem_map, List.getElem_range, Function.comp_apply,
            List.getElem_reverse, id]
          rw [hpiece j (by simpa using hj1)]
          congr 1
          rw [hrslen]
        rw [hlist, List.map_id]
  case vecIndexProxy =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b', tb⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        rw [rowVecIndexProxy] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, v2, rfl⟩ := list_len2 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      simp only [List.getElem_cons_zero] at h0
      dsimp only at hev
      rw [bind_ok_iff] at hc
      obtain ⟨pt, hpt, hc⟩ := hc
      rw [bind_ok_iff] at hc
      obtain ⟨k, hk, hc⟩ := hc
      rw [bind_ok_iff] at hc
      obtain ⟨nte1, hnte1, hc⟩ := hc
      obtain ⟨len, tea⟩ := nte1
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hteq
      rw [bind_ok_iff] at hc
      obtain ⟨se, hse, hc⟩ := hc
      rw [bind_ok_iff] at hc
      obtain ⟨szA, hszA, hc⟩ := hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hklen
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      rw [bind_ok_iff] at hev
      obtain ⟨pt', hpt', hev⟩ := hev
      rw [bind_ok_iff] at hev
      obtain ⟨k', hk', hev⟩ := hev
      rw [bind_ok_iff] at hev
      obtain ⟨xs, hxs, hev⟩ := hev
      split at hev
      rotate_left
      · exact error_ne_ok hev
      rename_i x hgx
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      -- link the Proxy index
      have hpteq : pt = pt' := by
        have := (domTyT_inv hpt).symm.trans (domTy_inv hpt')
        exact Option.some.inj this
      subst hpteq
      obtain ⟨np1, hfl1, hn1⟩ := proxyNatT_inv hk
      obtain ⟨np2, hfl2, hn2⟩ := proxyNatOf_inv hk'
      rw [hfl1] at hfl2
      have hkk : k = k' := by
        have hp : np1 = np2 := by simpa using hfl2
        subst hp
        rw [hn1] at hn2
        exact Option.some.inj hn2
      subst hkk
      have hv1 : v1 = .vec xs := vecVal_inv hxs
      obtain ⟨nt1, hfla, hna⟩ := vecLenElem_inv hnte1
      have htea : tea = res := teq_eq hteq
      subst htea
      obtain ⟨xs0, hxs0, hxlen, hxel⟩ := vty_vec_inv hΔ hfla hna h0.1
      rw [hv1] at hxs0
      injection hxs0 with hxs0
      subst hxs0
      obtain ⟨hklt, hxk⟩ := List.getElem?_eq_some_iff.mp hgx
      constructor
      · rw [← hxk]
        exact hxel _ (List.getElem_mem _)
      · obtain ⟨ka, hka⟩ := h0.2
        rw [hv1] at hka
        obtain ⟨ka', rs, hka1, hmap, haev⟩ := rep_vec_inv hka
        obtain ⟨hrslen, hpt2⟩ := mapM_ok_idx hmap
        obtain ⟨hklt2, hrepk⟩ := hpt2 k (by omega)
        refine ⟨ka', ?_⟩
        rw [← hxk, hrepk]
        congr 1
        rw [sliceNF_eval, haev]
        have hwidths : ∀ x ∈ rs, x.width = se := elem_widths hxel hmap hse
        have h9 := slice_singleton hwidths (i := k) (by omega)
        rw [show (rs.length - k - 1) * se = szA - k * se - se by
          cases szf with
          | zero => rw [DEnv.sizeOf] at hszA; exact error_ne_ok hszA
          | succ szf =>
              obtain ⟨kk, we, hkk, hwe, hszw⟩ := sizeOf_inv_vec hfla hszA
              have hkl : len = kk := by
                rw [hna] at hkk
                exact Option.some.inj hkk
              subst hkl
              have hwese : se = we := sizeOf_det hse hwe
              subst hwese
              rw [hszw, hrslen, hxlen]
              have h10 := sub_mul_offsets (len := len) (i := k) (m := 1) (we := se) (by omega)
              rw [Nat.one_mul] at h10
              rw [← h10]] at h9
        exact h9.symm
  case vecSlice =>
      rcases pas with _ | ⟨⟨a0, ta0⟩, _ | ⟨⟨a, ta⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        rw [rowVecSlice] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, v2, rfl⟩ := list_len2 (by simpa using hlen)
      have h1 := hargs 1 (by simp) (by simp)
      simp only [List.getElem_cons_zero, List.getElem_cons_succ] at h1
      dsimp only at hev
      rw [bind_ok_iff] at hc
      obtain ⟨pt, hpt, hc⟩ := hc
      rw [bind_ok_iff] at hc
      obtain ⟨i, hi, hc⟩ := hc
      rw [bind_ok_iff] at hc
      obtain ⟨nte0, hnte0, hc⟩ := hc
      obtain ⟨m, ter⟩ := nte0
      dsimp only at hc
      rw [bind_ok_iff] at hc
      obtain ⟨nte1, hnte1, hc⟩ := hc
      obtain ⟨len, tea⟩ := nte1
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hteq
      rw [bind_ok_iff] at hc
      obtain ⟨se, hse, hc⟩ := hc
      rw [bind_ok_iff] at hc
      obtain ⟨szA, hszA, hc⟩ := hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i him
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      rw [bind_ok_iff] at hev
      obtain ⟨pt', hpt', hev⟩ := hev
      rw [bind_ok_iff] at hev
      obtain ⟨i', hi', hev⟩ := hev
      rw [bind_ok_iff] at hev
      obtain ⟨m', hm', hev⟩ := hev
      rw [bind_ok_iff] at hev
      obtain ⟨xs, hxs, hev⟩ := hev
      split at hev
      rotate_left
      · exact error_ne_ok hev
      rename_i hrange
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      -- link the static data
      have hpteq : pt = pt' := by
        have := (domTyT_inv hpt).symm.trans (domTy_inv hpt')
        exact Option.some.inj this
      subst hpteq
      obtain ⟨np1, hfl1, hn1⟩ := proxyNatT_inv hi
      obtain ⟨np2, hfl2, hn2⟩ := proxyNatOf_inv hi'
      rw [hfl1] at hfl2
      have hii : i = i' := by
        have hp : np1 = np2 := by simpa using hfl2
        subst hp
        rw [hn1] at hn2
        exact Option.some.inj hn2
      subst hii
      obtain ⟨ntr, hflr, hntr⟩ := vecLenElem_inv hnte0
      obtain ⟨ntr2, ter2, hflr2, hm2⟩ := vecLen_inv hm'
      rw [hflr] at hflr2
      have hmm : m = m' := by
        have hp : ntr = ntr2 ∧ ter = ter2 := by simpa using hflr2
        obtain ⟨hp1, _⟩ := hp
        subst hp1
        rw [hntr] at hm2
        exact Option.some.inj hm2
      subst hmm
      have hv1 : v2 = .vec xs := vecVal_inv hxs
      obtain ⟨nt1, hfla, hna⟩ := vecLenElem_inv hnte1
      have htea : tea = ter := teq_eq hteq
      subst htea
      obtain ⟨xs0, hxs0, hxlen, hxel⟩ := vty_vec_inv hΔ hfla hna h1.1
      rw [hv1] at hxs0
      injection hxs0 with hxs0
      subst hxs0
      constructor
      · refine VTy.vec hflr hntr (by
          rw [List.length_take, List.length_drop]
          omega) ?_
        intro e he
        exact hxel e (List.mem_of_mem_drop (List.mem_of_mem_take he))
      · obtain ⟨ka, hka⟩ := h1.2
        rw [hv1] at hka
        obtain ⟨ka', rs, hka1, hmap, haev⟩ := rep_vec_inv hka
        obtain ⟨hrslen, _⟩ := mapM_ok_idx hmap
        have hwidths : ∀ x ∈ rs, x.width = se := elem_widths hxel hmap hse
        refine ⟨ka' + 1, ?_⟩
        rw [Val.rep, mapM_attach_erase, mapM_take_ok (mapM_drop_ok hmap),
            except_bind_ok, except_pure_def]
        congr 1
        rw [bvConcat_eq, sliceNF_eval, haev]
        have h9 := catAll_extract_range hwidths (i := i) (m := m)
          (by rw [hrslen, hxlen]; omega)
        rw [show (rs.length - i - m) * se = szA - i * se - m * se by
          cases szf with
          | zero => rw [DEnv.sizeOf] at hszA; exact error_ne_ok hszA
          | succ szf =>
              obtain ⟨kk, we, hkk, hwe, hszw⟩ := sizeOf_inv_vec hfla hszA
              have hkl : len = kk := by
                rw [hna] at hkk
                exact Option.some.inj hkk
              subst hkl
              have hwese : se = we := sizeOf_det hse hwe
              subst hwese
              rw [hszw, hrslen, hxlen]
              exact sub_mul_offsets (by omega)] at h9
        exact h9.symm
  case vecRSlice =>
      rcases pas with _ | ⟨⟨a0, ta0⟩, _ | ⟨⟨a, ta⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        rw [rowVecRSlice] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, v2, rfl⟩ := list_len2 (by simpa using hlen)
      have h1 := hargs 1 (by simp) (by simp)
      simp only [List.getElem_cons_zero, List.getElem_cons_succ] at h1
      dsimp only at hev
      rw [bind_ok_iff] at hc
      obtain ⟨pt, hpt, hc⟩ := hc
      rw [bind_ok_iff] at hc
      obtain ⟨i, hi, hc⟩ := hc
      rw [bind_ok_iff] at hc
      obtain ⟨nte0, hnte0, hc⟩ := hc
      obtain ⟨m, ter⟩ := nte0
      dsimp only at hc
      rw [bind_ok_iff] at hc
      obtain ⟨nte1, hnte1, hc⟩ := hc
      obtain ⟨len, tea⟩ := nte1
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hteq
      rw [bind_ok_iff] at hc
      obtain ⟨se, hse, hc⟩ := hc
      rw [bind_ok_iff] at hc
      obtain ⟨szA, hszA, hc⟩ := hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i him
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      rw [bind_ok_iff] at hev
      obtain ⟨pt', hpt', hev⟩ := hev
      rw [bind_ok_iff] at hev
      obtain ⟨i', hi', hev⟩ := hev
      rw [bind_ok_iff] at hev
      obtain ⟨m', hm', hev⟩ := hev
      rw [bind_ok_iff] at hev
      obtain ⟨xs, hxs, hev⟩ := hev
      split at hev
      rotate_left
      · exact error_ne_ok hev
      rename_i hrange
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      have hpteq : pt = pt' := by
        have := (domTyT_inv hpt).symm.trans (domTy_inv hpt')
        exact Option.some.inj this
      subst hpteq
      obtain ⟨np1, hfl1, hn1⟩ := proxyNatT_inv hi
      obtain ⟨np2, hfl2, hn2⟩ := proxyNatOf_inv hi'
      rw [hfl1] at hfl2
      have hii : i = i' := by
        have hp : np1 = np2 := by simpa using hfl2
        subst hp
        rw [hn1] at hn2
        exact Option.some.inj hn2
      subst hii
      obtain ⟨ntr, hflr, hntr⟩ := vecLenElem_inv hnte0
      obtain ⟨ntr2, ter2, hflr2, hm2⟩ := vecLen_inv hm'
      rw [hflr] at hflr2
      have hmm : m = m' := by
        have hp : ntr = ntr2 ∧ ter = ter2 := by simpa using hflr2
        obtain ⟨hp1, _⟩ := hp
        subst hp1
        rw [hntr] at hm2
        exact Option.some.inj hm2
      subst hmm
      have hv1 : v2 = .vec xs := vecVal_inv hxs
      obtain ⟨nt1, hfla, hna⟩ := vecLenElem_inv hnte1
      have htea : tea = ter := teq_eq hteq
      subst htea
      obtain ⟨xs0, hxs0, hxlen, hxel⟩ := vty_vec_inv hΔ hfla hna h1.1
      rw [hv1] at hxs0
      injection hxs0 with hxs0
      subst hxs0
      constructor
      · refine VTy.vec hflr hntr (by
          rw [List.length_take, List.length_drop]
          omega) ?_
        intro e he
        exact hxel e (List.mem_of_mem_drop (List.mem_of_mem_take he))
      · obtain ⟨ka, hka⟩ := h1.2
        rw [hv1] at hka
        obtain ⟨ka', rs, hka1, hmap, haev⟩ := rep_vec_inv hka
        obtain ⟨hrslen, _⟩ := mapM_ok_idx hmap
        have hwidths : ∀ x ∈ rs, x.width = se := elem_widths hxel hmap hse
        refine ⟨ka' + 1, ?_⟩
        rw [Val.rep, mapM_attach_erase, mapM_take_ok (mapM_drop_ok hmap),
            except_bind_ok, except_pure_def]
        congr 1
        rw [bvConcat_eq, sliceNF_eval, haev]
        have h9 := catAll_extract_range hwidths (i := xs.length - i - m) (m := m)
          (by rw [hrslen]; omega)
        rw [show (rs.length - (xs.length - i - m) - m) * se = i * se by
          rw [hrslen, hxlen]
          congr 1
          omega] at h9
        exact h9.symm
  case vecIndex =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨iN, ti⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        rw [rowVecIndex] at hc <;> try exact error_ne_ok hc
      obtain ⟨v1, v2, rfl⟩ := list_len2 (by simpa using hlen)
      have h0 := hargs 0 (by simp) (by simp)
      have h1 := hargs 1 (by simp) (by simp)
      simp only [List.getElem_cons_zero, List.getElem_cons_succ] at h0 h1
      dsimp only at hev
      rw [bind_ok_iff] at hc
      obtain ⟨nte1, hnte1, hc⟩ := hc
      obtain ⟨len, te⟩ := nte1
      dsimp only at hc
      rw [bind_ok_iff] at hc
      obtain ⟨nb, hnb, hc⟩ := hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hcond
      simp only [Bool.and_eq_true, beq_iff_eq] at hcond
      obtain ⟨hnbl, hteq⟩ := hcond
      rw [bind_ok_iff] at hc
      obtain ⟨se, hse, hc⟩ := hc
      rw [bind_ok_iff] at hc
      obtain ⟨szA, hszA, hc⟩ := hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      rename_i hguard
      obtain ⟨hlen1, hlen128, hprod128⟩ := hguard
      injection hc with hc
      injection hc with hnf hty
      subst hnf; subst hty
      rw [bind_ok_iff] at hev
      obtain ⟨xs, hxs, hev⟩ := hev
      rw [bind_ok_iff] at hev
      obtain ⟨bi, hbi, hev⟩ := hev
      obtain ⟨bound, iv⟩ := bi
      dsimp only at hev
      split at hev
      rotate_left
      · exact error_ne_ok hev
      rename_i x hgx
      rw [except_pure_def] at hev
      injection hev with hev
      subst hev
      -- the vector and its canonicality
      have hv1 : v1 = .vec xs := vecVal_inv hxs
      obtain ⟨nt1, hfla, hna⟩ := vecLenElem_inv hnte1
      have hte : te = res := teq_eq hteq
      subst hte
      obtain ⟨xs0, hxs0, hxlen, hxel⟩ := vty_vec_inv hΔ hfla hna h0.1
      rw [hv1] at hxs0
      injection hxs0 with hxs0
      subst hxs0
      -- the index value and its canonicality
      have hv2 : v2 = .finite bound iv := finVal_inv hbi
      obtain ⟨ntb, hflb, hnbt⟩ := finBoundT_inv hnb
      have hbound : bound = nb := by
        have hvt := h1.1
        rw [hv2] at hvt
        cases hvt with
        | finite hfl2 hn2 _hlt2 =>
            rename_i nvar
            rw [hflb] at hfl2
            have hp : ntb = nvar := by simpa using hfl2
            rw [← hp] at hn2
            rw [hnbt] at hn2
            exact (Option.some.inj hn2).symm
      subst hbound
      obtain ⟨hivlt, hxk⟩ := List.getElem?_eq_some_iff.mp hgx
      have hivlen : iv < len := by omega
      have hivnb : iv < 2 ^ nbits bound := by
        have h2 : iv < bound := by omega
        exact Nat.lt_of_lt_of_le h2 (nbits_le bound)
      -- representations
      obtain ⟨ka, hka⟩ := h0.2
      rw [hv1] at hka
      obtain ⟨ka', rs, hka1, hmap, haev⟩ := rep_vec_inv hka
      obtain ⟨hrslen, hptw⟩ := mapM_ok_idx hmap
      have hwidths : ∀ x ∈ rs, x.width = se := elem_widths hxel hmap hse
      obtain ⟨kb, hkb⟩ := h1.2
      rw [hv2] at hkb
      have hiev : iN.eval σ E = ⟨nbits bound, BitVec.ofNat (nbits bound) iv⟩ := by
        cases kb with
        | zero => rw [Val.rep] at hkb; exact error_ne_ok hkb
        | succ kb =>
            rw [Val.rep] at hkb
            injection hkb with hkb
            rw [← hkb]
      constructor
      · rw [← hxk]
        exact hxel _ (List.getElem_mem _)
      · -- widths and static numbers
        have hnb128 : nbits bound ≤ 128 := nbits_le_of_le_pow (by rw [hnbl]; omega)
        have hmax : max (nbits bound) 128 = 128 := Nat.max_eq_right hnb128
        rw [hmax]
        have hi'' : (resizeNF 128 (nbits bound) iN).eval σ E
            = ⟨128, BitVec.ofNat 128 iv⟩ := by
          rw [resizeNF_eval (by rw [hiev])]
          rw [hiev]
          show (⟨128, (BitVec.ofNat (nbits bound) iv).setWidth 128⟩ : BV) = _
          rw [setWidth_ofNat hivnb (by omega)]
        -- the shift amount
        have hsub1 : (NF.prim2 .sub (.lit ⟨128, BitVec.ofNat 128 len⟩)
              (resizeNF 128 (nbits bound) iN)).eval σ E
            = ⟨128, BitVec.ofNat 128 len - BitVec.ofNat 128 iv⟩ := by
          show (match Rwv.Hyle.Sem.evalOp .sub
                  [(NF.lit ⟨128, BitVec.ofNat 128 len⟩).eval σ E,
                   (resizeNF 128 (nbits bound) iN).eval σ E] with
                | .ok v => v
                | .error _ => BV.nil) = _
          rw [hi'']
          show (⟨128, BitVec.ofNat 128 len - (BitVec.ofNat 128 iv).setWidth 128⟩ : BV) = _
          rw [BitVec.setWidth_eq]
        have hlenN : (BitVec.ofNat 128 len).toNat = len :=
          Nat.mod_eq_of_lt (by omega)
        have hivN : (BitVec.ofNat 128 iv).toNat = iv :=
          Nat.mod_eq_of_lt (by omega)
        have hsub1N : (BitVec.ofNat 128 len - BitVec.ofNat 128 iv).toNat = len - iv := by
          rw [BitVec.toNat_sub_of_le (by
            rw [BitVec.le_def, hlenN, hivN]
            omega), hlenN, hivN]
        have hsub2 : (NF.prim2 .sub
              (NF.prim2 .sub (.lit ⟨128, BitVec.ofNat 128 len⟩)
                (resizeNF 128 (nbits bound) iN))
              (.lit ⟨128, (1 : BitVec 128)⟩)).eval σ E
            = ⟨128, (BitVec.ofNat 128 len - BitVec.ofNat 128 iv) - 1⟩ := by
          show (match Rwv.Hyle.Sem.evalOp .sub
                  [(NF.prim2 .sub (.lit ⟨128, BitVec.ofNat 128 len⟩)
                      (resizeNF 128 (nbits bound) iN)).eval σ E,
                   (NF.lit ⟨128, (1 : BitVec 128)⟩).eval σ E] with
                | .ok v => v
                | .error _ => BV.nil) = _
          rw [hsub1]
          show (⟨128, (BitVec.ofNat 128 len - BitVec.ofNat 128 iv)
                  - (1 : BitVec 128).setWidth 128⟩ : BV) = _
          rw [BitVec.setWidth_eq]
        have hsub2N : ((BitVec.ofNat 128 len - BitVec.ofNat 128 iv) - 1).toNat
            = len - iv - 1 := by
          rw [BitVec.toNat_sub_of_le (by
            rw [BitVec.le_def, hsub1N]
            have h1t : (1 : BitVec 128).toNat = 1 := rfl
            rw [h1t]
            omega), hsub1N]
          rfl
        have hamt : (NF.prim2 .mul
              (NF.prim2 .sub
                (NF.prim2 .sub (.lit ⟨128, BitVec.ofNat 128 len⟩)
                  (resizeNF 128 (nbits bound) iN))
                (.lit ⟨128, (1 : BitVec 128)⟩))
              (.lit ⟨128, BitVec.ofNat 128 se⟩)).eval σ E
            = ⟨128, ((BitVec.ofNat 128 len - BitVec.ofNat 128 iv) - 1)
                  * BitVec.ofNat 128 se⟩ := by
          show (match Rwv.Hyle.Sem.evalOp .mul
                  [(NF.prim2 .sub
                      (NF.prim2 .sub (.lit ⟨128, BitVec.ofNat 128 len⟩)
                        (resizeNF 128 (nbits bound) iN))
                      (.lit ⟨128, (1 : BitVec 128)⟩)).eval σ E,
                   (NF.lit ⟨128, BitVec.ofNat 128 se⟩).eval σ E] with
                | .ok v => v
                | .error _ => BV.nil) = _
          rw [hsub2]
          show (⟨128, ((BitVec.ofNat 128 len - BitVec.ofNat 128 iv) - 1)
                  * (BitVec.ofNat 128 se).setWidth 128⟩ : BV) = _
          rw [BitVec.setWidth_eq]
        have hseN : (BitVec.ofNat 128 se).toNat = se := by
          apply Nat.mod_eq_of_lt
          have h2 : 1 * se ≤ len * se := Nat.mul_le_mul_right se (by omega)
          rw [Nat.one_mul] at h2
          omega
        have hamtN : (((BitVec.ofNat 128 len - BitVec.ofNat 128 iv) - 1)
              * BitVec.ofNat 128 se).toNat = (len - iv - 1) * se := by
          rw [BitVec.toNat_mul, hsub2N, hseN]
          apply Nat.mod_eq_of_lt
          have h2 : (len - iv - 1) * se ≤ len * se :=
            Nat.mul_le_mul_right se (by omega)
          omega
        -- the shifted, truncated read is the element slice
        obtain ⟨hivlt2, hrepk⟩ := hptw iv (by omega)
        refine ⟨ka', ?_⟩
        rw [← hxk, hrepk]
        congr 1
        have hawidth : (a.eval σ E).width = szA := vty_rep_width (hv1 ▸ h0.1) hka hszA
        have hshift : (NF.prim2 .lshr a (NF.prim2 .mul
              (NF.prim2 .sub
                (NF.prim2 .sub (.lit ⟨128, BitVec.ofNat 128 len⟩)
                  (resizeNF 128 (nbits bound) iN))
                (.lit ⟨128, (1 : BitVec 128)⟩))
              (.lit ⟨128, BitVec.ofNat 128 se⟩))).eval σ E
            = ⟨(a.eval σ E).width, (a.eval σ E).bits >>> ((len - iv - 1) * se)⟩ := by
          show (match Rwv.Hyle.Sem.evalOp .lshr
                  [a.eval σ E, (NF.prim2 .mul
                    (NF.prim2 .sub
                      (NF.prim2 .sub (.lit ⟨128, BitVec.ofNat 128 len⟩)
                        (resizeNF 128 (nbits bound) iN))
                      (.lit ⟨128, (1 : BitVec 128)⟩))
                    (.lit ⟨128, BitVec.ofNat 128 se⟩)).eval σ E] with
                | .ok v => v
                | .error _ => BV.nil) = _
          rw [hamt]
          show (⟨(a.eval σ E).width,
              (a.eval σ E).bits >>> (((BitVec.ofNat 128 len - BitVec.ofNat 128 iv) - 1)
                * BitVec.ofNat 128 se).toNat⟩ : BV) = _
          rw [hamtN]
        rw [resizeNF_eval (by rw [hshift]; exact hawidth), hshift]
        refine (bv_ext ?_ ?_).symm
        · show se = (rs[iv]'(by omega)).width
          exact (hwidths _ (List.getElem_mem _)).symm
        · intro j
          show (((a.eval σ E).bits >>> ((len - iv - 1) * se)).setWidth se).getLsbD j
              = (rs[iv]'(by omega)).bits.getLsbD j
          rw [BitVec.getLsbD_setWidth, BitVec.getLsbD_ushiftRight]
          have h9 := slice_singleton hwidths (i := iv) (by omega)
          rw [show (rs.length - iv - 1) * se = (len - iv - 1) * se by
            rw [hrslen, hxlen]] at h9
          have h10 := congrArg (fun z => z.bits.getLsbD j) h9
          rw [sliceBV_getLsbD] at h10
          rw [← h10, haev]

/-! ## The full compiler's soundness (Phase 4b)

The statement generalizes `SoundAt` to "compile `e` applied to the
pending arguments": whenever the evaluator produces `e`'s value `f`
and applying `f` to values corresponding to the pending arguments
produces `v`, the compiled normal form denotes `v`'s representation. -/

private abbrev SoundAtJ (Δ : DEnv) (dmap : HashMap Int Defn) (σ : String → BV)
    (fuel : Nat) : Prop :=
  ∀ (Γ : HashMap Int (NF × Ty)) (jΓ : CJEnv) (e : Exp) (pend : List (NF × Ty))
    (nf : NF) (ty : Ty) (efuel afuel : Nat) (env : Eval.Env) (jenv : Eval.JEnv)
    (f : Val) (vsp : List Val) (v : Val),
    cexpJ Δ dmap fuel Γ jΓ e pend = .ok (nf, ty) →
    Eval.evalCore ⟨Δ, dmap⟩ efuel env jenv e E = .ok f →
    Eval.applyMany ⟨Δ, dmap⟩ afuel f vsp E = .ok v →
    EnvC (E := E) Δ σ Γ env →
    JEnvC (E := E) Δ σ jΓ jenv →
    vsp.length = pend.length →
    (∀ i (h1 : i < pend.length) (h2 : i < vsp.length),
       VTy Δ vsp[i] pend[i].2 ∧ ∃ k, Val.rep Δ k vsp[i] = .ok (pend[i].1.eval σ E)) →
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ E)

/-- Pointwise argument facts over an append. -/
private theorem pointwise_append {Δ : DEnv} {σ : String → BV}
    {pas pend : List (NF × Ty)} {vs vsp : List Val}
    (h1 : vs.length = pas.length) (h2 : vsp.length = pend.length)
    (f1 : ∀ i (ha : i < pas.length) (hb : i < vs.length),
       VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E))
    (f2 : ∀ i (ha : i < pend.length) (hb : i < vsp.length),
       VTy Δ vsp[i] pend[i].2 ∧ ∃ k, Val.rep Δ k vsp[i] = .ok (pend[i].1.eval σ E)) :
    ∀ i (ha : i < (pas ++ pend).length) (hb : i < (vs ++ vsp).length),
      VTy Δ (vs ++ vsp)[i] ((pas ++ pend)[i]).2 ∧
        ∃ k, Val.rep Δ k (vs ++ vsp)[i] = .ok (((pas ++ pend)[i]).1.eval σ E) := by
  intro i ha hb
  rw [List.length_append] at ha hb
  by_cases hi : i < pas.length
  · rw [List.getElem_append_left (by omega), List.getElem_append_left hi]
    exact f1 i hi (by omega)
  · rw [List.getElem_append_right (by omega), List.getElem_append_right (by omega)]
    have hgv : vsp[i - vs.length]'(by omega) = vsp[i - pas.length]'(by omega) := by
      congr 1
      omega
    rw [hgv]
    exact f2 (i - pas.length) (by omega) (by omega)

/-- Pointwise argument facts through a shared `drop`. -/
private theorem pointwise_drop {Δ : DEnv} {σ : String → BV}
    {pall : List (NF × Ty)} {vall : List Val} {n : Nat}
    (f : ∀ i (ha : i < pall.length) (hb : i < vall.length),
       VTy Δ vall[i] pall[i].2 ∧ ∃ k, Val.rep Δ k vall[i] = .ok (pall[i].1.eval σ E)) :
    ∀ i (ha : i < (pall.drop n).length) (hb : i < (vall.drop n).length),
      VTy Δ (vall.drop n)[i] ((pall.drop n)[i]).2 ∧
        ∃ k, Val.rep Δ k (vall.drop n)[i] = .ok (((pall.drop n)[i]).1.eval σ E) := by
  intro i ha hb
  rw [List.length_drop] at ha hb
  rw [List.getElem_drop, List.getElem_drop]
  exact f (n + i) (by omega) (by omega)

/-- Pointwise argument facts through a shared `take`. -/
private theorem pointwise_take {Δ : DEnv} {σ : String → BV}
    {pall : List (NF × Ty)} {vall : List Val} {n : Nat}
    (f : ∀ i (ha : i < pall.length) (hb : i < vall.length),
       VTy Δ vall[i] pall[i].2 ∧ ∃ k, Val.rep Δ k vall[i] = .ok (pall[i].1.eval σ E)) :
    ∀ i (ha : i < (pall.take n).length) (hb : i < (vall.take n).length),
      VTy Δ (vall.take n)[i] ((pall.take n)[i]).2 ∧
        ∃ k, Val.rep Δ k (vall.take n)[i] = .ok (((pall.take n)[i]).1.eval σ E) := by
  intro i ha hb
  rw [List.length_take] at ha hb
  rw [List.getElem_take, List.getElem_take]
  exact f i (by omega) (by omega)

set_option maxHeartbeats 8000000 in
private theorem cchainJ_sound {Δ : DEnv} {dmap : HashMap Int Defn} {σ : String → BV}
    (hΔ : denvOk Δ = true) {fuel : Nat} (IH : SoundAtJ (E := E) Δ dmap σ fuel)
    {Γ' : HashMap Int (NF × Ty)} {jΓ : CJEnv} {env : Eval.Env} {jenv : Eval.JEnv}
    {binder : Id} {dn : NF} {dty : Ty} {szT : Nat} {resTy : Ty} {sv : Val}
    (hΓ' : EnvC (E := E) Δ σ Γ' ((binder.uniq, sv) :: env))
    (hJ : JEnvC (E := E) Δ σ jΓ jenv)
    (hsz : Δ.sizeOf (fuel + 1) [] dty = .ok szT)
    (hvty : VTy Δ sv dty)
    {ks : Nat} (hks : Val.rep Δ ks sv = .ok (dn.eval σ E))
    {pend : List (NF × Ty)} {vsp : List Val}
    (hvl : vsp.length = pend.length)
    (hvp : ∀ i (h1 : i < pend.length) (h2 : i < vsp.length),
       VTy Δ vsp[i] pend[i].2 ∧ ∃ k, Val.rep Δ k vsp[i] = .ok (pend[i].1.eval σ E)) :
    ∀ (rest : List Alt) (macc : Option NF) (dflt : Option Alt) (out : NF × Ty)
      (ef2 afuel : Nat) (vout v : Val),
      cchainJ Δ dmap fuel Γ' jΓ dty szT dn resTy pend rest macc = .ok out →
      Eval.tryAlts ⟨Δ, dmap⟩ ef2 env jenv binder sv rest dflt E = .ok vout →
      Eval.applyMany ⟨Δ, dmap⟩ afuel vout vsp E = .ok v →
      ((macc = none ∧ dflt = none) ∨
       (∃ els c bs dbody dbt, macc = some els ∧ dflt = some (Alt.mk c bs dbody) ∧
          cexpJ Δ dmap fuel Γ' jΓ dbody pend = .ok (els, dbt) ∧
          teqN Δ dbt resTy = true)) →
      out.2 = resTy ∧ VTy Δ v resTy ∧ ∃ k, Val.rep Δ k v = .ok (out.1.eval σ E) := by
  -- The per-alternative step.
  have hstep : ∀ (con : AltCon) (xs : List Id) (body : Exp) (macc : Option NF) (bnf : NF)
      (restE : List Alt) (dflt : Option Alt) (ef3 afuel : Nat) (vout v : Val),
      cAltJ Δ dmap fuel Γ' jΓ dty szT dn resTy pend (.mk con xs body) macc = .ok bnf →
      Eval.tryAlts ⟨Δ, dmap⟩ (ef3 + 1) env jenv binder sv (.mk con xs body :: restE) dflt E
        = .ok vout →
      Eval.applyMany ⟨Δ, dmap⟩ afuel vout vsp E = .ok v →
      (macc = none → restE = [] ∧ dflt = none) →
      (∀ acc, macc = some acc →
         Eval.tryAlts ⟨Δ, dmap⟩ ef3 env jenv binder sv restE dflt E = .ok vout →
         VTy Δ v resTy ∧ ∃ k, Val.rep Δ k v = .ok (acc.eval σ E)) →
      VTy Δ v resTy ∧ ∃ k, Val.rep Δ k v = .ok (bnf.eval σ E) := by
    intro con xs body macc bnf restE dflt ef3 afuel vout v hca hev happ hnone hcont
    cases con with
    | default => rw [cAltJ] at hca; exact error_ne_ok hca
    | litAlt i =>
        rw [cAltJ] at hca
        obtain ⟨bt, hbt, hca⟩ := except_bind_eq_ok hca
        obtain ⟨bnf', bty⟩ := bt
        dsimp only at hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        rename_i hteq
        rw [Eval.tryAlts] at hev
        try dsimp only at hev
        obtain ⟨bm, hbm, hev⟩ := except_bind_eq_ok hev
        rw [Eval.litMatches] at hbm
        obtain ⟨x, hx, hbm⟩ := except_bind_eq_ok hbm
        rw [except_pure_def] at hbm
        injection hbm with hbm
        have hxd : x = dn.eval σ E := rep_det hx hks
        subst hxd
        subst hbm
        have hw : (dn.eval σ E).width = szT := vty_rep_width hvty hks hsz
        have hcondv : (NF.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT i⟩)).eval σ E
            = Rwv.Hyle.Sem.b1 ((dn.eval σ E).bits == BitVec.ofInt (dn.eval σ E).width i) := by
          rw [show (NF.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT i⟩)).eval σ E
                = Rwv.Hyle.Sem.b1 ((dn.eval σ E).bits ==
                    (BitVec.ofInt szT i).setWidth (dn.eval σ E).width) from rfl]
          rw [show (BitVec.ofInt szT i).setWidth (dn.eval σ E).width
                = BitVec.ofInt (dn.eval σ E).width i by
              rw [hw]
              exact BitVec.setWidth_eq _]
        cases hbm2 : ((dn.eval σ E).bits == BitVec.ofInt (dn.eval σ E).width i) with
        | true =>
            rw [hbm2] at hev
            try dsimp only at hev
            have hmain := IH Γ' jΓ body pend bnf' bty ef3 afuel ((binder.uniq, sv) :: env)
              jenv vout vsp v hbt hev happ hΓ' hJ hvl hvp
            refine ⟨vty_teqN hmain.1 hteq, ?_⟩
            obtain ⟨k, hk⟩ := hmain.2
            refine ⟨k, ?_⟩
            rw [hk]
            congr 1
            cases macc with
            | none =>
                try dsimp only at hca
                injection hca with hca
                rw [← hca]
            | some acc =>
                try dsimp only at hca
                injection hca with hca
                rw [← hca, ite_eval_of_cond (C := true) (by rw [hcondv, hbm2])]
                rw [if_pos rfl]
        | false =>
            rw [hbm2] at hev
            try dsimp only at hev
            cases macc with
            | none =>
                obtain ⟨hre, hdf⟩ := hnone rfl
                subst hre; subst hdf
                cases ef3 with
                | zero => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
                | succ ef4 => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
            | some acc =>
                obtain ⟨hvt, k, hk⟩ := hcont acc rfl hev
                refine ⟨hvt, k, ?_⟩
                rw [hk]
                congr 1
                try dsimp only at hca
                injection hca with hca
                rw [← hca, ite_eval_of_cond (C := false) (by rw [hcondv, hbm2])]
                rw [if_neg (by simp)]
    | dataAlt cn =>
        rw [cAltJ] at hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        rename_i hctb
        obtain ⟨tg, htagn, hca⟩ := except_bind_eq_ok hca
        obtain ⟨tag, w⟩ := tg
        cases hcs2 : Δ.ctorSig.get? cn with
        | none => rw [hcs2] at hca; dsimp only at hca; exact error_ne_ok hca
        | some sig2 =>
        rw [hcs2] at hca
        dsimp only at hca
        obtain ⟨sub2, hsub2, hca⟩ := except_bind_eq_ok hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        rename_i hxlen
        obtain ⟨szXs, hszXs, hca⟩ := except_bind_eq_ok hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        rename_i hwle
        obtain ⟨bt, hbt, hca⟩ := except_bind_eq_ok hca
        obtain ⟨bnf', bty⟩ := bt
        dsimp only at hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        rename_i hteq
        -- the evaluator's step: the scrutinee must be a constructor value
        rw [Eval.tryAlts.eq_def] at hev
        try dsimp only at hev
        split at hev
        rotate_left
        all_goals try exact error_ne_ok hev
        rename_i svty cv fields
        -- canonicality pins the carried type and the field facts
        cases hvty with
        | con hsigv hmatchv hlenv hctorv hfieldsv =>
        rename_i sigv subv
        -- the representation, dissected
        obtain ⟨ks', whole, tagv, tagWv, bsR, hks1, hwhole, htagv, hbs, hguard, hbv⟩ :=
          rep_con_inv hks
        have hwhsz : szT = whole := (sizeOf_det hwhole hsz).symm
        subst hwhsz
        -- the two constructors live in the same head datatype
        obtain ⟨tc, argsT, hflT, hdisj⟩ := ctorTag_inv htagn
        obtain ⟨tc', argsT', hflT', hdisj'⟩ := ctorTag_inv htagv
        rw [hflT] at hflT'
        have htcc : tc = tc' := by
          have := congrArg Prod.fst hflT'
          simpa using this
        subst htcc
        -- names from ctorOf
        have hcnOf : ctorOf Δ dty cn := ctorOfB_sound hctb
        rw [ctorOf, hflT] at hcnOf hctorv
        dsimp only at hcnOf hctorv
        -- tag widths agree, and w = 0 forces the same constructor
        have hkey : tagWv = w ∧ (cn = cv ↔ tagv = tag) := by
          rcases hdisj with ⟨htup, htag0, hw0⟩ | ⟨htup, cs, hcs, hidxn, hwn⟩
          · rcases hdisj' with ⟨_, htagv0, hwv0⟩ | ⟨htup', _, _, _⟩
            · subst htag0; subst hw0; subst htagv0; subst hwv0
              rw [if_pos htup] at hcnOf hctorv
              exact ⟨rfl, ⟨fun _ => rfl, fun _ => hcnOf.trans hctorv.symm⟩⟩
            · exact absurd htup (by simp [htup'])
          · rcases hdisj' with ⟨htup', _, _⟩ | ⟨_, cs', hcs', hidxv, hwv⟩
            · exact absurd htup' (by simp [htup])
            · rw [hcs] at hcs'
              injection hcs' with hcs'
              subst hcs'
              subst hwn; subst hwv
              refine ⟨rfl, ?_, ?_⟩
              · intro hcc
                subst hcc
                rw [hidxn] at hidxv
                injection hidxv with h
                exact h.symm
              · intro htt
                exact (idxOf?_inj hidxv (htt ▸ hidxn)).symm
        obtain ⟨hww, hcniff⟩ := hkey
        subst hww
        -- the width of the whole representation
        have hwsz : (dn.eval σ E).width = szT := vty_rep_width (VTy.con hsigv hmatchv hlenv
          (by rw [ctorOf, hflT]; exact hctorv) hfieldsv) hks hsz
        -- the tag slice of the representation
        have hFw : (bvCat (⟨szT - tagWv - (Val.bvConcat bsR).width, 0⟩ : BV)
            (Val.bvConcat bsR)).width = szT - tagWv := by
          rw [bvCat_width]
          show szT - tagWv - (Val.bvConcat bsR).width + (Val.bvConcat bsR).width = szT - tagWv
          omega
        have hslice : sliceBV (dn.eval σ E) (szT - tagWv) tagWv
            = (⟨tagWv, BitVec.ofNat tagWv tagv⟩ : BV) := by
          rw [hbv, sliceBV_cat_high (Nat.le_of_eq hFw), hFw, Nat.sub_self]
          exact sliceBV_all _
        -- the compiled test's value
        have hcondv : ∀ (w0 : Nat), tagWv = w0 + 1 →
            (NF.prim2 .eq (sliceNF (szT - tagWv) tagWv dn)
              (.lit ⟨tagWv, BitVec.ofNat tagWv tag⟩)).eval σ E
            = Rwv.Hyle.Sem.b1 (BitVec.ofNat tagWv tagv == BitVec.ofNat tagWv tag) := by
          intro w0 hw0
          have h1 : (NF.prim2 .eq (sliceNF (szT - tagWv) tagWv dn)
                (.lit ⟨tagWv, BitVec.ofNat tagWv tag⟩)).eval σ E
              = Rwv.Hyle.Sem.b1
                  (((sliceNF (szT - tagWv) tagWv dn).eval σ E).bits ==
                    BitVec.setWidth ((sliceNF (szT - tagWv) tagWv dn).eval σ E).width
                      (BitVec.ofNat tagWv tag)) := rfl
          rw [h1, sliceNF_eval, hslice]
          rw [show BitVec.setWidth (⟨tagWv, BitVec.ofNat tagWv tagv⟩ : BV).width
                (BitVec.ofNat tagWv tag) = BitVec.ofNat tagWv tag from BitVec.setWidth_eq _]
        cases hcv : (cn == cv) with
        | true =>
            have hcneq : cn = cv := beq_iff_eq.mp hcv
            subst hcneq
            -- same constructor: the signatures and substitutions coincide
            rw [hcs2] at hsigv
            injection hsigv with hsigv
            subst hsigv
            rw [hsub2] at hmatchv
            injection hmatchv with hmatchv
            subst hmatchv
            rw [hcv] at hev
            try dsimp only at hev
            by_cases hblen' : xs.length = fields.length
            case neg =>
                rw [if_neg (show ¬ (xs.length == fields.length) = true by
                      simpa using hblen')] at hev
                exact error_ne_ok hev
            rw [if_pos (show (xs.length == fields.length) = true by
                  simpa using hblen')] at hev
            -- lengths
            obtain ⟨hszlen, hszpt⟩ := mapM_ok_idx hszXs
            rw [List.length_map] at hszlen
            obtain ⟨hbslen, hbspt⟩ := mapM_ok_idx hbs
            have hxleni : xs.length = ((Ty.flattenArrow sig2.ty).1.map (DEnv.substTv sub2)).length := hxlen
            rw [List.length_map] at hxleni
            -- the piece widths are the instantiated field sizes
            have hlistw : szXs = bsR.map (·.width) := by
              refine List.ext_getElem (by simp only [List.length_map]; omega) ?_
              intro i h1 h2
              obtain ⟨hi1, hszi⟩ := hszpt i (by rw [List.length_map]; omega)
              obtain ⟨hi2, hrepi⟩ := hbspt i (by omega)
              have hvf := hfieldsv (((Ty.flattenArrow sig2.ty).1.zip fields)[i]'(by
                simp only [List.length_zip]; omega)) (List.getElem_mem _)
              rw [List.getElem_zip] at hvf
              rw [List.getElem_map] at hszi
              simp only [List.getElem_map]
              exact (vty_rep_width hvf hrepi hszi).symm
            have hsumw : (Val.bvConcat bsR).width = szXs.sum := by
              rw [bvConcat_eq, catAll_width, hlistw]
            -- the branch environment corresponds
            have henv'' : EnvC (E := E) Δ σ
                (bindFieldsΓ xs
                  ((((szXs.zip ((List.range szXs.length).map fun i => (szXs.drop (i + 1)).sum)).map
                      fun p => sliceNF p.2 p.1 dn)).zip
                    ((Ty.flattenArrow sig2.ty).1.map (DEnv.substTv sub2))) Γ')
                (((xs.map (·.uniq)).zip fields) ++ (binder.uniq, Val.con dty cn fields) :: env) := by
              refine envC_bind xs _ fields hΓ' ?_ (by omega) ?_
              · simp only [List.length_zip, List.length_map, List.length_range]
                omega
              · intro i h1 h2 h3
                have hzl : i < ((szXs.zip ((List.range szXs.length).map fun i =>
                    (szXs.drop (i + 1)).sum)).map fun p => sliceNF p.2 p.1 dn).length := by
                  simp only [List.length_map, List.length_zip, List.length_range]
                  omega
                rw [List.getElem_zip]
                constructor
                · -- canonicality at the instantiated field type
                  have hvf := hfieldsv (((Ty.flattenArrow sig2.ty).1.zip fields)[i]'(by
                    simp only [List.length_zip]; omega)) (List.getElem_mem _)
                  rw [List.getElem_zip] at hvf
                  dsimp only
                  rw [List.getElem_map]
                  exact hvf
                · -- representation: the field slice of the scrutinee
                  obtain ⟨hi2, hrepi⟩ := hbspt i (by omega)
                  refine ⟨ks', ?_⟩
                  rw [hrepi]
                  congr 1
                  dsimp only
                  rw [List.getElem_map, List.getElem_zip, List.getElem_map, List.getElem_range,
                      sliceNF_eval]
                  -- extract piece i
                  have hoffw : (szXs.drop (i + 1)).sum
                      = ((bsR.drop (i + 1)).map (·.width)).sum := by
                    rw [hlistw]
                    congr 1
                    first
                      | rw [List.map_drop]
                      | rw [← List.map_drop]
                      | rw [List.drop_map]
                      | rw [← List.drop_map]
                  have hszw : szXs[i]'(by omega) = (bsR[i]'(by omega)).width := by
                    have h5 : szXs[i]'(by omega) = (bsR.map (·.width))[i]'(by
                        simp only [List.length_map]; omega) :=
                      List.getElem_of_eq hlistw _
                    rw [h5, List.getElem_map]
                  rw [hbv]
                  rw [sliceBV_cat_low (by
                    rw [bvCat_width, hsumw]
                    have := drop_sum_le (l := szXs) (i := i) (by omega)
                    omega)]
                  rw [sliceBV_cat_low (by
                    rw [hsumw]
                    have := drop_sum_le (l := szXs) (i := i) (by omega)
                    omega)]
                  dsimp only
                  rw [hoffw, hszw, bvConcat_eq]
                  rw [congrArg catAll (show bsR = bsR.take i ++ bsR[i]'(by omega) :: bsR.drop (i + 1) by
                    rw [List.getElem_cons_drop, List.take_append_drop])]
                  exact (catAll_extract (bsR.take i) (bsR.drop (i + 1)) (bsR[i]'(by omega))).symm
            have hmain := IH _ jΓ body pend bnf' bty ef3 afuel _ jenv vout vsp v hbt hev
              happ henv'' hJ hvl hvp
            refine ⟨vty_teqN hmain.1 hteq, ?_⟩
            obtain ⟨k, hk⟩ := hmain.2
            refine ⟨k, ?_⟩
            rw [hk]
            congr 1
            -- the compiled alternative takes its own branch
            cases macc with
            | none =>
                try dsimp only at hca
                injection hca with hca
                rw [← hca]
            | some acc =>
                cases htw : tagWv with
                | zero =>
                    rw [htw] at hca
                    try dsimp only at hca
                    injection hca with hca
                    rw [← hca]
                | succ w0 =>
                    rw [htw] at hca
                    try dsimp only at hca
                    injection hca with hca
                    rw [← hca]
                    have hcv2 := hcondv w0 htw
                    rw [htw] at hcv2
                    rw [ite_eval_of_cond (C := true) (by
                      rw [hcv2]
                      congr 1
                      rw [(hcniff.mp rfl), beq_self_eq_true])]
                    rw [if_pos rfl]
        | false =>
            have hcnne : cn ≠ cv := by
              intro hcc
              rw [hcc] at hcv
              rw [beq_self_eq_true] at hcv
              cases hcv
            rw [hcv] at hev
            try dsimp only at hev
            cases macc with
            | none =>
                obtain ⟨hre, hdf⟩ := hnone rfl
                subst hre; subst hdf
                cases ef3 with
                | zero => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
                | succ ef4 => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
            | some acc =>
                cases htw : tagWv with
                | zero =>
                    -- a zero-width tag forces a single constructor
                    exfalso
                    rcases hdisj with ⟨htup, _, _⟩ | ⟨htup, cs, hcs, hidxn, hwn⟩
                    · rcases hdisj' with ⟨_, _, _⟩ | ⟨htup', _, _, _⟩
                      · rw [if_pos htup] at hcnOf hctorv
                        exact hcnne (hcnOf.trans hctorv.symm)
                      · exact absurd htup (by simp [htup'])
                    · rcases hdisj' with ⟨htup', _, _⟩ | ⟨_, cs', hcs', hidxv, hwv⟩
                      · exact absurd htup' (by simp [htup])
                      · rw [hcs] at hcs'
                        injection hcs' with hcs'
                        subst hcs'
                        have hlen1 : cs.length ≤ 1 := nbits_zero (by omega)
                        have h1 := idxOf?_lt hidxn
                        have h2 := idxOf?_lt hidxv
                        have htt : tagv = tag := by omega
                        exact hcnne (hcniff.mpr htt)
                | succ w0 =>
                    rw [htw] at hca
                    try dsimp only at hca
                    injection hca with hca
                    obtain ⟨hvt, k, hk⟩ := hcont acc rfl hev
                    refine ⟨hvt, k, ?_⟩
                    rw [hk]
                    congr 1
                    rw [← hca]
                    have htagne : tagv ≠ tag := fun htt => hcnne (hcniff.mpr htt)
                    have hbounds : tagv < 2 ^ tagWv ∧ tag < 2 ^ tagWv := by
                      rcases hdisj with ⟨_, htag0, hw0⟩ | ⟨htup, cs, hcs, hidxn, hwn⟩
                      · omega
                      · rcases hdisj' with ⟨htup', _, _⟩ | ⟨_, cs', hcs', hidxv, hwv⟩
                        · exact absurd htup' (by simp [htup])
                        · rw [hcs] at hcs'
                          injection hcs' with hcs'
                          subst hcs'
                          constructor
                          · rw [hwv]
                            exact Nat.lt_of_lt_of_le (idxOf?_lt hidxv) (nbits_le _)
                          · rw [hwn]
                            exact Nat.lt_of_lt_of_le (idxOf?_lt hidxn) (nbits_le _)
                    have hcv2 := hcondv w0 htw
                    rw [htw] at hcv2
                    rw [ite_eval_of_cond (C := false) (by
                      rw [hcv2]
                      congr 1
                      have hb2 := hbounds
                      rw [htw] at hb2
                      exact ofNat_beq_false htagne hb2.1 hb2.2)]
                    rw [if_neg (by simp)]
  -- The chain induction.
  intro rest
  induction rest with
  | nil =>
      intro macc dflt out ef2 afuel vout v hcc hev happ hrel
      rcases hrel with ⟨hm, hd⟩ | ⟨els, c0, bs0, dbody, dbt, hm, hd, hdb, hdteq⟩
      · subst hm
        rw [cchainJ] at hcc
        exact error_ne_ok hcc
      · subst hm; subst hd
        rw [cchainJ] at hcc
        injection hcc with hcc
        cases ef2 with
        | zero => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
        | succ ef3 =>
            rw [Eval.tryAlts] at hev
            try dsimp only at hev
            have hmain := IH Γ' jΓ dbody pend els dbt ef3 afuel ((binder.uniq, sv) :: env)
              jenv vout vsp v hdb hev happ hΓ' hJ hvl hvp
            refine ⟨by rw [← hcc], vty_teqN hmain.1 hdteq, ?_⟩
            obtain ⟨k, hk⟩ := hmain.2
            exact ⟨k, by rw [← hcc, hk]⟩
  | cons alt restT ihR =>
      intro macc dflt out ef2 afuel vout v hcc hev happ hrel
      obtain ⟨con, xs, body⟩ := alt
      cases ef2 with
      | zero => rw [Eval.tryAlts] at hev; exact error_ne_ok hev
      | succ ef3 =>
      rcases restT with _ | ⟨r2, rt⟩
      · rcases macc with _ | els
        · -- the unconditional last alternative
          rw [cchainJ] at hcc
          obtain ⟨bnf, hbnf, hcc⟩ := except_bind_eq_ok hcc
          injection hcc with hcc
          have hd0 : dflt = none := by
            rcases hrel with ⟨_, hd⟩ | ⟨_, _, _, _, _, hm, _, _, _⟩
            · exact hd
            · exact absurd hm (by simp)
          subst hd0
          have hstepped := hstep con xs body none bnf [] none ef3 afuel vout v hbnf hev happ
            (fun _ => ⟨rfl, rfl⟩) (fun acc hacc => absurd hacc (by simp))
          exact ⟨by rw [← hcc], hstepped.1, by rw [← hcc]; exact hstepped.2⟩
        · -- last conditional alternative, default fallback
          rw [cchainJ] at hcc
          obtain ⟨accp, haccp, hcc⟩ := except_bind_eq_ok hcc
          rw [cchainJ] at haccp
          injection haccp with haccp
          obtain ⟨accnf, accty⟩ := accp
          have haccnf : accnf = els := by
            have h6 := congrArg Prod.fst haccp
            simpa using h6.symm
          try dsimp only at hcc
          obtain ⟨bnf, hbnf, hcc⟩ := except_bind_eq_ok hcc
          injection hcc with hcc
          rw [haccnf] at hbnf
          rcases hrel with ⟨hm, _⟩ | ⟨els2, c0, bs0, dbody, dbt, hm, hd, hdb, hdteq⟩
          · exact absurd hm (by simp)
          have hm2 : els = els2 := Option.some.inj hm
          subst hm2
          subst hd
          have hstepped := hstep con xs body (some els) bnf [] (some (Alt.mk c0 bs0 dbody))
            ef3 afuel vout v hbnf hev happ (fun h => absurd h (by simp))
            (fun acc hacc hev' => by
              injection hacc with hacc
              subst hacc
              cases ef3 with
              | zero => rw [Eval.tryAlts] at hev'; exact error_ne_ok hev'
              | succ ef4 =>
                  rw [Eval.tryAlts] at hev'
                  try dsimp only at hev'
                  have hm3 := IH Γ' jΓ dbody pend els dbt ef4 afuel ((binder.uniq, sv) :: env)
                    jenv vout vsp v hdb hev' happ hΓ' hJ hvl hvp
                  exact ⟨vty_teqN hm3.1 hdteq, hm3.2⟩)
          exact ⟨by rw [← hcc], hstepped.1, by rw [← hcc]; exact hstepped.2⟩
          all_goals (intros; simp_all)
      · -- an interior alternative
        rw [cchainJ] at hcc
        obtain ⟨accp, haccp, hcc⟩ := except_bind_eq_ok hcc
        obtain ⟨accnf, accty⟩ := accp
        try dsimp only at hcc
        obtain ⟨bnf, hbnf, hcc⟩ := except_bind_eq_ok hcc
        injection hcc with hcc
        have hstepped := hstep con xs body (some accnf) bnf (r2 :: rt) dflt ef3 afuel vout v
          hbnf hev happ
          (fun h => absurd h (by simp))
          (fun acc hacc hev' => by
            injection hacc with hacc
            subst hacc
            have := ihR macc dflt (accnf, accty) ef3 afuel vout v haccp hev' happ hrel
            exact ⟨this.2.1, this.2.2⟩)
        exact ⟨by rw [← hcc], hstepped.1, by rw [← hcc]; exact hstepped.2⟩
        all_goals (intro h1 h2; exact absurd h2 (by simp))


/-! ## Syntactic Finite literals evaluate to their numeral -/

private theorem litIntVal_int_shape {ty : Ty} {n : Int} {x : BitVec 128}
    (h : Eval.litIntVal ty n = .ok (.integer x)) : x = BitVec.ofInt 128 n := by
  rw [Eval.litIntVal] at h
  split at h
  · rw [except_pure_def] at h
    injection h with h
    injection h with h
    exact h.symm
  · split at h
    · split at h
      · rw [except_pure_def] at h
        injection h with h
        cases h
      · exact error_ne_ok h
    · exact error_ne_ok h
  · split at h
    · split at h
      · rw [except_pure_def] at h
        injection h with h
        cases h
      · exact error_ne_ok h
    · exact error_ne_ok h
  · exact error_ne_ok h

/-- A syntactic Finite literal (ToHyle's `finLit`) evaluates to a
`Finite` value carrying exactly the numeral `finLitE` computes. -/
private theorem finLit_val {iE : Exp} {idx : Nat} (hfin : finLitE iE = some idx)
    {C : Eval.Ctx} :
    ∀ {k : Nat} {env : Eval.Env} {jenv : Eval.JEnv} {vi : Val},
      Eval.evalCore C k env jenv iE E = .ok vi → ∃ b, vi = .finite b idx := by
  rw [finLitE] at hfin
  split at hfin
  rotate_left
  · exact absurd hfin (by simp)
  rename_i fty ity nl heq
  injection hfin with hfin
  intro k env jenv vi hev
  cases k with
  | zero => rw [Eval.evalCore] at hev; exact error_ne_ok hev
  | succ k =>
      rw [Eval.evalCore] at hev
      rw [heq] at hev
      dsimp only at hev
      obtain ⟨vs0, hvs0, hev⟩ := except_bind_eq_ok hev
      cases k with
      | zero => rw [Eval.evalList] at hvs0; exact error_ne_ok hvs0
      | succ k =>
          rw [Eval.evalList] at hvs0
          obtain ⟨v0, hv0, hvs0⟩ := except_bind_eq_ok hvs0
          obtain ⟨vrest, hvrest, hvs0⟩ := except_bind_eq_ok hvs0
          cases k with
          | zero => rw [Eval.evalList] at hvrest; exact error_ne_ok hvrest
          | succ k =>
              rw [Eval.evalList] at hvrest
              rw [except_pure_def] at hvrest
              injection hvrest with hvrest
              subst hvrest
              rw [except_pure_def] at hvs0
              injection hvs0 with hvs0
              subst hvs0
              -- the literal's value
              cases hkl : k + 1 with
              | zero => cases hkl
              | succ k2 =>
                  rw [Eval.evalCore] at hv0
                  rw [show Eval.flattenApp (.litInt ity nl) = (.litInt ity nl, [])
                        from rfl] at hv0
                  dsimp only at hv0
                  -- the builtin row
                  rw [Eval.evalBuiltin] at hev
                  dsimp only at hev
                  obtain ⟨nb, hnb, hev⟩ := except_bind_eq_ok hev
                  obtain ⟨xi, hxi, hev⟩ := except_bind_eq_ok hev
                  split at hev
                  rotate_left
                  · exact error_ne_ok hev
                  rw [except_pure_def] at hev
                  injection hev with hev
                  subst hev
                  have hv02 : v0 = .integer xi := intVal_inv hxi
                  rw [hv02] at hv0
                  have hxin : xi = BitVec.ofInt 128 nl := litIntVal_int_shape hv0
                  refine ⟨nb, ?_⟩
                  rw [hxin, hfin]

set_option maxHeartbeats 12000000 in
/-- THE full-compiler soundness theorem (rep-correspondence with
pending application): a successful `cexpJ` compilation is faithful —
whenever the evaluator produces the expression's value and applying it
to values corresponding to the pending arguments succeeds, the result
is canonical at the synthesized type and its representation is the
compiled normal form's denotation. -/
theorem cexpJ_sound {Δ : DEnv} {dmap : HashMap Int Defn} {σ : String → BV}
    {X : Rwv.Hyle.Sem.XEnv} {F : Rwv.Hyle.Sem.FEnv}
    (hΔ : denvOk Δ = true) (hFor : ForeignC Δ X F) :
    ∀ (fuel : Nat), SoundAtJ (E := E) Δ dmap σ fuel := by
  intro fuel
  induction fuel with
  | zero =>
      intro Γ jΓ e pend nf ty efuel afuel env jenv f vsp v hc
      rw [cexpJ] at hc
      exact error_ne_ok hc
  | succ fuel ih =>
      intro Γ jΓ e pend nf ty efuel afuel env jenv f vsp v hc hev happ hΓ hJ hvl hvp
      cases efuel with
      | zero => rw [Eval.evalCore] at hev; exact error_ne_ok hev
      | succ efuel =>
      rw [cexpJ] at hc
      rw [Eval.evalCore] at hev
      rcases hfl : Eval.flattenApp e with ⟨hd, args⟩
      rw [hfl] at hc hev
      clear hfl
      cases hd with
      | var x =>
          dsimp only at hc hev
          obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
          cases hΓx : Γ.get? x.uniq with
          | some nt =>
              rw [hΓx] at hc
              cases args with
              | cons a as => cases pend <;> exact error_ne_ok hc
              | nil =>
                  cases pend with
                  | cons p ps => exact error_ne_ok hc
                  | nil =>
                      injection hc with hc
                      have hvsp : vsp = [] := List.length_eq_zero_iff.mp (by simpa using hvl)
                      subst hvsp
                      have hvf : v = f := applyMany_nil_inv happ
                      subst hvf
                      obtain ⟨w, hlook, hwty, k, hwrep⟩ := hΓ.fwd x.uniq nt hΓx
                      have hvs0 : vs = [] := by
                        obtain ⟨hlen, _⟩ := evalList_ok_idx hvs
                        exact List.length_eq_zero_iff.mp (by simpa using hlen)
                      subst hvs0
                      cases hL : List.lookup x.uniq env with
                      | none => rw [hlook] at hL; exact absurd hL (by simp)
                      | some w2 =>
                          rw [hL] at hev
                          dsimp only at hev
                          have hw2 : w2 = w := Option.some.inj (hL.symm.trans hlook)
                          subst hw2
                          have hv : v = w2 := applyMany_nil_inv hev
                          subst hv
                          subst hc
                          exact ⟨hwty, k, hwrep⟩
          | none =>
              rw [hΓx] at hc
              cases hL : List.lookup x.uniq env with
              | some w0 =>
                  rw [hΓ.miss x.uniq hΓx] at hL
                  exact absurd hL (by simp)
              | none =>
              rw [hL] at hev
              dsimp only at hev
              cases hdm : dmap.get? x.uniq with
              | none => rw [hdm] at hc; exact error_ne_ok hc
              | some d =>
                  rw [hdm] at hc
                  dsimp only at hc
                  rw [bind_ok_iff] at hc
                  obtain ⟨pas, hpas, hc⟩ := hc
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i hnle
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i hteq
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i hor
                  rw [hdm] at hev
                  dsimp only at hev
                  obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
                  obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
                  have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
                      VTy Δ vs[i] pas[i].2 ∧
                        ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
                    intro i h1 h2
                    obtain ⟨hia, hci⟩ := hpt i (by omega)
                    obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
                    exact ih Γ jΓ args[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i] [] vs[i]
                      hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
                      (fun j hj1 _ => absurd hj1 (by simp))
                  have hall := pointwise_append (by omega) hvl hptw hvp
                  have hlall : (vs ++ vsp).length = (pas ++ pend).length := by
                    rw [List.length_append, List.length_append]
                    omega
                  -- reduce both branches to the saturated call over vs ++ vsp
                  have hsat : ∃ K w, Eval.evalCore ⟨Δ, dmap⟩ K
                        ((d.params.map (·.uniq)).zip
                          ((vs ++ vsp).take d.params.length)) [] d.body E = .ok w ∧
                      ∃ K2, Eval.applyMany ⟨Δ, dmap⟩ K2 w
                        ((vs ++ vsp).drop d.params.length) E = .ok v := by
                    have hor2 : d.params.length ≤ pas.length ∨ x.uniq = d.name.uniq := by
                      simpa using hor
                    rcases hor2 with hA | hB
                    · -- the spine alone saturates
                      have hA' : d.params.length ≤ pas.length := hA
                      have hnvs : d.params.length ≤ vs.length := by omega
                      cases efuel with
                      | zero => rw [Eval.callDefn.eq_def] at hev; exact error_ne_ok hev
                      | succ ef2 =>
                          rw [Eval.callDefn.eq_def] at hev
                          dsimp only at hev
                          rw [if_neg (by omega)] at hev
                          obtain ⟨w, hbody, hev⟩ := except_bind_eq_ok hev
                          obtain ⟨K2, hK2⟩ := applyMany_compose hev happ
                          refine ⟨ef2, w, ?_, K2, ?_⟩
                          · rw [show (vs ++ vsp).take d.params.length
                                  = vs.take d.params.length from
                                List.take_append_of_le_length hnvs]
                            have hzz : (d.params.map (·.uniq)).zip vs
                                = (d.params.map (·.uniq)).zip (vs.take d.params.length) := by
                              rw [zip_take_right (l := d.params.map (·.uniq)) (vs := vs),
                                  List.length_map]
                            rw [← hzz]
                            exact hbody
                          · rw [show (vs ++ vsp).drop d.params.length
                                  = vs.drop d.params.length ++ vsp from
                                  List.drop_append_of_le_length hnvs]
                            exact hK2
                    · -- extend the partial application through the closure chain
                      have hB' : x.uniq = d.name.uniq := hB
                      have hconsM : (⟨Δ, dmap⟩ : Eval.Ctx).defns.get? d.name.uniq = some d := by
                        show dmap.get? d.name.uniq = some d
                        rw [← hB']
                        exact hdm
                      obtain ⟨K, hK⟩ := callDefn_extend hconsM hev happ
                      have hnle2 : d.params.length ≤ pas.length + pend.length := by
                        have h1 := hnle
                        rwa [List.length_append] at h1
                      cases K with
                      | zero => rw [Eval.callDefn] at hK; exact error_ne_ok hK
                      | succ K =>
                          rw [Eval.callDefn] at hK
                          rw [if_neg (by
                            rw [List.length_append]
                            omega)] at hK
                          obtain ⟨w, hbody, hK⟩ := except_bind_eq_ok hK
                          refine ⟨K, w, ?_, K, hK⟩
                          have hzz : (d.params.map (·.uniq)).zip (vs ++ vsp)
                              = (d.params.map (·.uniq)).zip
                                  ((vs ++ vsp).take d.params.length) := by
                            rw [zip_take_right (l := d.params.map (·.uniq)) (vs := vs ++ vsp),
                                List.length_map]
                          rw [hzz] at hbody
                          exact hbody
                  obtain ⟨K, w, hbody, K2, happ2⟩ := hsat
                  have hnlen : d.params.length ≤ (pas ++ pend).length := by
                    simpa using hnle
                  refine ih (mkGamma d.params ((pas ++ pend).take d.params.length)) []
                    d.body ((pas ++ pend).drop d.params.length) nf ty K K2 _ []
                    w ((vs ++ vsp).drop d.params.length) v hc hbody happ2 ?_ jenvC_nil
                    ?_ (pointwise_drop hall)
                  · have := envC_bind (Δ := Δ) (σ := σ) d.params
                      ((pas ++ pend).take d.params.length)
                      ((vs ++ vsp).take d.params.length)
                      (Γ₀ := ∅) (env₀ := []) envC_empty
                      (by rw [List.length_take]; omega)
                      (by rw [List.length_take]; omega)
                      (fun i h1 h2 h3 => pointwise_take hall i h2 h3)
                    simpa [mkGamma] using this
                  · rw [List.length_drop, List.length_drop]
                    omega
      | con cty c =>
          cases pend with
          | cons p ps =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
          have hvsp : vsp = [] := List.length_eq_zero_iff.mp (by simpa using hvl)
          subst hvsp
          have hvf : v = f := applyMany_nil_inv happ
          subst hvf
          dsimp only at hc hev
          rcases hfac : Ty.flattenArrow cty with ⟨dts, resTy⟩
          have hfac1 : (Ty.flattenArrow cty).1 = dts := by rw [hfac]
          have hfac2 : (Ty.flattenArrow cty).2 = resTy := by rw [hfac]
          rw [hfac1, hfac2] at hc
          rw [hfac] at hev
          dsimp only at hev
          obtain ⟨pas, hpas, hc⟩ := except_bind_eq_ok hc
          obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
          obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
          obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
          have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
              VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
            intro i h1 h2
            obtain ⟨hia, hci⟩ := hpt i (by omega)
            obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
            exact ih Γ jΓ args[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i] [] vs[i]
              hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
              (fun j hj1 _ => absurd hj1 (by simp))
          split at hc
          rotate_left
          · exact error_ne_ok hc
          rename_i hsat
          cases hcs : Δ.ctorSig.get? c with
          | none => rw [hcs] at hc; exact error_ne_ok hc
          | some sig =>
              rw [hcs] at hc
              dsimp only at hc
              obtain ⟨sub, hsub, hc⟩ := except_bind_eq_ok hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hteq
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hctb
              obtain ⟨whole, hwhole, hc⟩ := except_bind_eq_ok hc
              obtain ⟨tg, htag, hc⟩ := except_bind_eq_ok hc
              obtain ⟨tag, w⟩ := tg
              dsimp only at hc
              obtain ⟨ws, hws, hc⟩ := except_bind_eq_ok hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hle
              injection hc with hc
              injection hc with hnf hty
              subst hnf; subst hty
              split at hev
              rotate_left
              · exact error_ne_ok hev
              rename_i hsat2
              rw [except_pure_def] at hev
              injection hev with hev
              subst hev
              have hlent : pas.length = (Ty.flattenArrow sig.ty).1.length := by
                have h1 := teqAllN_length hteq
                simpa using h1
              have hvpas : vs.length = pas.length := by
                have h2 : vs.length = dts.length := by simpa using hsat2
                omega
              have hpasty : ∀ i (h : i < pas.length),
                  teqN Δ pas[i].2
                    (DEnv.substTv sub ((Ty.flattenArrow sig.ty).1[i]'(by omega))) = true := by
                intro i h
                have h1 := teqAllN_get hteq i h (by rw [List.length_map]; omega)
                simpa using h1
              obtain ⟨hwlen, hwpt⟩ := mapM_ok_idx hws
              rw [List.length_map] at hwlen
              have hwlen' : ws.length = pas.length := by omega
              have hwidths : ∀ i (h : i < pas.length),
                  (pas[i].1.eval σ E).width = ws[i]'(by omega) := by
                intro i h
                obtain ⟨hwi, hszi⟩ := hwpt i (by rw [List.length_map]; omega)
                obtain ⟨hv1, hex⟩ := hptw i h (by omega)
                obtain ⟨k1, hrep1⟩ := hex
                have hv1' := vty_teqN hv1 (hpasty i h)
                have hszi' : Δ.sizeOf (fuel + 1) []
                    (DEnv.substTv sub ((Ty.flattenArrow sig.ty).1[i]'(by omega))) = .ok (ws[i]'(by omega)) := by
                  have h2 := hszi
                  rw [List.getElem_map] at h2
                  exact h2
                exact vty_rep_width hv1' hrep1 hszi'
              have hlistw : (pas.map (fun p => p.1.eval σ E)).map (·.width) = ws := by
                refine List.ext_getElem (by simpa using by omega) ?_
                intro i h1 h2
                simp only [List.getElem_map]
                exact hwidths i (by simpa using h1)
              have hsum : (Val.bvConcat (pas.map fun p => p.1.eval σ E)).width = ws.sum := by
                rw [bvConcat_eq, catAll_width, hlistw]
              obtain ⟨K, hK⟩ := mapM_rep_exists (Δ := Δ) (vs := vs)
                (bs := pas.map (fun p => p.1.eval σ E)) (by simpa using hvpas)
                (fun i h1 h2 => by
                  obtain ⟨_, hex⟩ := hptw i (by omega) h1
                  obtain ⟨k1, hrep1⟩ := hex
                  exact ⟨k1, by simpa using hrep1⟩)
              constructor
              · refine VTy.con (sub := sub) hcs hsub (by omega) (ctorOfB_sound hctb) ?_
                intro p hp
                obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                have hj2 : j < pas.length := by
                  simp only [List.length_zip] at hj
                  omega
                rw [← hpj, List.getElem_zip]
                have hvj := (hptw j hj2 (by omega)).1
                exact vty_teqN hvj (hpasty j hj2)
              · refine ⟨max (fuel + 1) K + 1, ?_⟩
                have hle2 : w + (Val.bvConcat (pas.map fun p => p.1.eval σ E)).width ≤ whole := by
                  rw [hsum]
                  exact hle
                have hcon := rep_con_intro (K := max (fuel + 1) K)
                  (Δ.sizeOf_mono (by omega) hwhole) htag
                  (mapM_rep_mono (Nat.le_max_right _ _) hK) hle2
                refine hcon.trans ?_
                congr 1
                have hpwidths : ∀ p ∈ ((NF.lit ⟨w, BitVec.ofNat w tag⟩, w)
                    :: (NF.lit ⟨whole - w - ws.sum, 0⟩, whole - w - ws.sum)
                    :: (pas.map (·.1)).zip ws), (p.1.eval σ E).width = p.2 := by
                  intro p hp
                  rcases hp with _ | ⟨_, hp⟩
                  · rfl
                  rcases hp with _ | ⟨_, hp⟩
                  · rfl
                  obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                  have hj2 : j < pas.length := by
                    simp only [List.length_zip, List.length_map] at hj
                    omega
                  rw [← hpj, List.getElem_zip]
                  simp only [List.getElem_map]
                  exact hwidths j hj2
                rw [catNF_eval σ _ hpwidths]
                have hmaps : ((NF.lit (⟨w, BitVec.ofNat w tag⟩ : BV), w)
                    :: (NF.lit (⟨whole - w - ws.sum, 0⟩ : BV), whole - w - ws.sum)
                    :: (pas.map (·.1)).zip ws).map (fun p => p.1.eval σ E)
                    = (⟨w, BitVec.ofNat w tag⟩ : BV) :: (⟨whole - w - ws.sum, 0⟩ : BV)
                      :: (pas.map fun p => p.1.eval σ E) := by
                  rw [List.map_cons, List.map_cons,
                      map_zip_fst (NF.eval σ E) (pas.map (·.1)) ws
                        (by simp only [List.length_map]; omega),
                      List.map_map]
                  rfl
                rw [hmaps, catAll_cons, catAll_cons, ← bvConcat_eq, hsum]
      | prim pty b =>
          cases pend with
          | cons p ps =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
          have hvsp : vsp = [] := List.length_eq_zero_iff.mp (by simpa using hvl)
          subst hvsp
          have hvf : v = f := applyMany_nil_inv happ
          subst hvf
          dsimp only at hc hev
          by_cases hbc : (b == .cryptol) = true
          · -- THE CRYPTOL FOREIGN ROW (stage A): the compiled form is
            -- the spliced entry definition's symbolic inlining; its
            -- denotation is the evaluator's foreign result through
            -- `ForeignC`, `symExp_sound`, and the decode round trip.
            rw [if_pos hbc] at hev
            rw [beq_iff_eq] at hbc
            subst hbc
            dsimp only at hc hev
            split at hc
            all_goals try exact error_ne_ok hc
            rename_i f' n' impl rest
            dsimp only at hev
            cases efuel with
            | zero => rw [Eval.evalCry] at hev; exact error_ne_ok hev
            | succ ef2 =>
            rw [Eval.evalCry] at hev
            obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
            obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
            obtain ⟨ityE, hityE, hev⟩ := except_bind_eq_ok hev
            obtain ⟨ityC, hityC, hc⟩ := except_bind_eq_ok hc
            rw [hityC] at hityE
            injection hityE with hity
            subst hity
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hlenC
            rw [if_pos hlenC] at hev
            cases hcryD : Δ.cryD f' n' ityC with
            | none => rw [hcryD] at hc; exact error_ne_ok hc
            | some g =>
            rw [hcryD] at hc
            dsimp only at hc
            cases hgd : Δ.hyleDefs.get? g with
            | none => rw [hgd] at hc; exact error_ne_ok hc
            | some hd =>
            rw [hgd] at hc
            dsimp only at hc
            obtain ⟨pas, hpas, hc⟩ := except_bind_eq_ok hc
            obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i harity
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hargsFree
            obtain ⟨nfS, hsym, hc⟩ := except_bind_eq_ok hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hnfFree
            injection hc with hc
            injection hc with hnf hty
            subst hnf
            subst hty
            obtain ⟨den, hcryF, hden⟩ := hFor.cry f' n' ityC g hcryD
            rw [hcryF] at hev
            dsimp only at hev
            obtain ⟨reps, hreps, hev⟩ := except_bind_eq_ok hev
            obtain ⟨hrlen, hrpt⟩ := mapM_ok_idx hreps
            obtain ⟨bv, hbv, hev⟩ := except_bind_eq_ok hev
            have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
                VTy Δ vs[i] pas[i].2 ∧
                  ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
              intro i h1 h2
              obtain ⟨hia, hci⟩ := hpt i (by omega)
              obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
              exact ih Γ jΓ rest[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i] [] vs[i]
                hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
                (fun j hj1 _ => absurd hj1 (by simp))
            have hre : reps = (pas.map (·.1)).map (NF.eval σ E) := by
              refine List.ext_getElem (by rw [List.length_map, List.length_map]; omega) ?_
              intro i hi1 hi2
              obtain ⟨hia, hri⟩ := hrpt i (by omega)
              obtain ⟨hvtyi, ki, hrepi⟩ := hptw i (by omega) (by omega)
              have hri' : Val.rep Δ ef2 vs[i] = .ok reps[i] := hri
              rw [List.getElem_map, List.getElem_map]
              exact rep_det hri' hrepi
            obtain ⟨fn, hFg, hfn⟩ := hFor.impl g hd hgd
            have hlenR : reps.length = hd.params.length := by omega
            have himpl' : Rwv.Hyle.Bridge.FImplements Δ.hyleDefs Δ.hyleX F := by
              have h0 := hFor.impl
              rw [hFor.hX] at h0
              exact h0
            have hbv' : Rwv.Hyle.evalExp F Δ.hyleX (HashMap.ofList (hd.params.zip reps))
                hd.body = .ok bv := by
              have h1 := hden reps
              rw [h1, callF, hFg] at hbv
              dsimp only at hbv
              rw [hfn reps] at hbv
              simp only [Rwv.Hyle.Bridge.mkFn] at hbv
              rw [if_pos hlenR] at hbv
              rw [← hFor.hX]
              exact hbv
            -- The spliced denotation is pinned at the EMPTY extern
            -- environment; the compiled arguments and result are
            -- extern-free by the compiler's gates, so their
            -- denotations cannot tell the environments apart.
            have hre0 : reps = (pas.map (·.1)).map (NF.eval σ Rwv.Hyle.Sem.eEmpty) := by
              rw [hre]
              refine List.map_congr_left ?_
              intro n hn
              exact Rwv.Hyle.Bridge.NF.xcallFree_eval (List.all_eq_true.mp hargsFree n hn)
            have hcorr : Rwv.Hyle.Bridge.EnvCorr σ
                (HashMap.ofList (hd.params.zip (pas.map (·.1))))
                (HashMap.ofList (hd.params.zip reps)) := by
              rw [hre0]
              exact envCorr_zip_eval (E := Rwv.Hyle.Sem.eEmpty) hd.params (pas.map (·.1))
            have hsem := Rwv.Hyle.Bridge.symExp_sound himpl' Δ.hyleFuel hd.body
              (HashMap.ofList (hd.params.zip (pas.map (·.1))))
              (HashMap.ofList (hd.params.zip reps)) nfS hcorr hsym
            have hbvnf : bv = nfS.eval σ Rwv.Hyle.Sem.eEmpty := by
              rw [hsem] at hbv'
              injection hbv' with h'
              exact h'.symm
            have hlift : nfS.eval σ Rwv.Hyle.Sem.eEmpty = nfS.eval σ E :=
              Rwv.Hyle.Bridge.NF.xcallFree_eval (E := Rwv.Hyle.Sem.eEmpty) (E' := E) hnfFree
            exact ⟨decode_vty hev, ef2, (hlift ▸ hbvnf) ▸ decode_rep hev⟩
          have hbcF : (b == .cryptol) = false := by
            revert hbc
            cases b == .cryptol <;> simp
          by_cases hbx : (b == .«extern») = true
          · -- THE MODEL-LESS EXTERN ROW (stage B, the η tier): the
            -- compiled form is the uninterpreted-call node over the
            -- packed arguments. Both sides read the SAME bit-level
            -- interpretation E on the SAME bits (the argument IH),
            -- and the evaluator's decode gate pins the result
            -- canonical, with `decode_rep` closing the representation
            -- leg and the clamp made inert by `vty_rep_width`.
            rw [if_neg (by simp [hbcF]), if_pos hbx] at hev
            rw [beq_iff_eq] at hbx
            subst hbx
            dsimp only at hc hev
            split at hc
            all_goals try exact error_ne_ok hc
            rename_i _ps _clk _rst _as _rs s _impl _inst rest
            dsimp only at hev
            cases efuel with
            | zero => rw [Eval.evalExt] at hev; exact error_ne_ok hev
            | succ ef2 =>
            rw [Eval.evalExt] at hev
            obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
            obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
            obtain ⟨ityE, hityE, hev⟩ := except_bind_eq_ok hev
            obtain ⟨ityC, hityC, hc⟩ := except_bind_eq_ok hc
            rw [hityC] at hityE
            injection hityE with hity
            subst hity
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hlenC
            rw [if_pos hlenC] at hev
            cases hhx : Δ.hyleX.get? s with
            | some g =>
              -- THE MODEL-CARRYING EXTERN ROW (stage C): the Cryptol
              -- case's argument verbatim, keyed by the extern table —
              -- the compiled form is the model definition's symbolic
              -- inlining; its denotation is the evaluator's `Δ.xtF`
              -- result through `ForeignC.xt`, `symExp_sound`, and the
              -- decode round trip, with the extern-free gates pinning
              -- the pinned-at-eEmpty denotations.
              rw [hhx] at hc
              dsimp only at hc
              cases hgd : Δ.hyleDefs.get? g with
              | none => rw [hgd] at hc; exact error_ne_ok hc
              | some hd =>
              rw [hgd] at hc
              dsimp only at hc
              obtain ⟨pas, hpas, hc⟩ := except_bind_eq_ok hc
              obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i harity
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hargsFree
              obtain ⟨nfS, hsym, hc⟩ := except_bind_eq_ok hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hnfFree
              injection hc with hc
              injection hc with hnf hty
              subst hnf
              subst hty
              obtain ⟨den, hxtF, hden⟩ := hFor.xt s g hhx
              rw [hxtF] at hev
              dsimp only at hev
              obtain ⟨reps, hreps, hev⟩ := except_bind_eq_ok hev
              obtain ⟨hrlen, hrpt⟩ := mapM_ok_idx hreps
              obtain ⟨bv, hbv, hev⟩ := except_bind_eq_ok hev
              have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
                  VTy Δ vs[i] pas[i].2 ∧
                    ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
                intro i h1 h2
                obtain ⟨hia, hci⟩ := hpt i (by omega)
                obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
                exact ih Γ jΓ rest[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i] [] vs[i]
                  hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
                  (fun j hj1 _ => absurd hj1 (by simp))
              have hre : reps = (pas.map (·.1)).map (NF.eval σ E) := by
                refine List.ext_getElem (by rw [List.length_map, List.length_map]; omega) ?_
                intro i hi1 hi2
                obtain ⟨hia, hri⟩ := hrpt i (by omega)
                obtain ⟨hvtyi, ki, hrepi⟩ := hptw i (by omega) (by omega)
                have hri' : Val.rep Δ ef2 vs[i] = .ok reps[i] := hri
                rw [List.getElem_map, List.getElem_map]
                exact rep_det hri' hrepi
              obtain ⟨fn, hFg, hfn⟩ := hFor.impl g hd hgd
              have hlenR : reps.length = hd.params.length := by omega
              have himpl' : Rwv.Hyle.Bridge.FImplements Δ.hyleDefs Δ.hyleX F := by
                have h0 := hFor.impl
                rw [hFor.hX] at h0
                exact h0
              have hbv' : Rwv.Hyle.evalExp F Δ.hyleX (HashMap.ofList (hd.params.zip reps))
                  hd.body = .ok bv := by
                have h1 := hden reps
                rw [h1, callF, hFg] at hbv
                dsimp only at hbv
                rw [hfn reps] at hbv
                simp only [Rwv.Hyle.Bridge.mkFn] at hbv
                rw [if_pos hlenR] at hbv
                rw [← hFor.hX]
                exact hbv
              have hre0 : reps = (pas.map (·.1)).map (NF.eval σ Rwv.Hyle.Sem.eEmpty) := by
                rw [hre]
                refine List.map_congr_left ?_
                intro n hn
                exact Rwv.Hyle.Bridge.NF.xcallFree_eval (List.all_eq_true.mp hargsFree n hn)
              have hcorr : Rwv.Hyle.Bridge.EnvCorr σ
                  (HashMap.ofList (hd.params.zip (pas.map (·.1))))
                  (HashMap.ofList (hd.params.zip reps)) := by
                rw [hre0]
                exact envCorr_zip_eval (E := Rwv.Hyle.Sem.eEmpty) hd.params (pas.map (·.1))
              have hsem := Rwv.Hyle.Bridge.symExp_sound himpl' Δ.hyleFuel hd.body
                (HashMap.ofList (hd.params.zip (pas.map (·.1))))
                (HashMap.ofList (hd.params.zip reps)) nfS hcorr hsym
              have hbvnf : bv = nfS.eval σ Rwv.Hyle.Sem.eEmpty := by
                rw [hsem] at hbv'
                injection hbv' with h'
                exact h'.symm
              have hlift : nfS.eval σ Rwv.Hyle.Sem.eEmpty = nfS.eval σ E :=
                Rwv.Hyle.Bridge.NF.xcallFree_eval (E := Rwv.Hyle.Sem.eEmpty) (E' := E) hnfFree
              exact ⟨decode_vty hev, ef2, (hlift ▸ hbvnf) ▸ decode_rep hev⟩
            | none =>
            rw [hhx] at hc
            dsimp only at hc
            obtain ⟨pas, hpas, hc⟩ := except_bind_eq_ok hc
            obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
            obtain ⟨w, hw, hc⟩ := except_bind_eq_ok hc
            injection hc with hc
            injection hc with hnf hty
            subst hnf
            subst hty
            have hxtF : Δ.xtF s = none := hFor.ext s hhx
            rw [hxtF] at hev
            cases hEs : E s with
            | none => rw [hEs] at hev; exact error_ne_ok hev
            | some fx =>
            rw [hEs] at hev
            obtain ⟨reps, hreps, hev⟩ := except_bind_eq_ok hev
            obtain ⟨hrlen, hrpt⟩ := mapM_ok_idx hreps
            obtain ⟨bv, hbv, hev⟩ := except_bind_eq_ok hev
            have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
                VTy Δ vs[i] pas[i].2 ∧
                  ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
              intro i h1 h2
              obtain ⟨hia, hci⟩ := hpt i (by omega)
              obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
              exact ih Γ jΓ rest[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i] [] vs[i]
                hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
                (fun j hj1 _ => absurd hj1 (by simp))
            have hre : reps = (pas.map (·.1)).map (NF.eval σ E) := by
              refine List.ext_getElem (by rw [List.length_map, List.length_map]; omega) ?_
              intro i hi1 hi2
              obtain ⟨hia, hri⟩ := hrpt i (by omega)
              obtain ⟨hvtyi, ki, hrepi⟩ := hptw i (by omega) (by omega)
              have hri' : Val.rep Δ ef2 vs[i] = .ok reps[i] := hri
              rw [List.getElem_map, List.getElem_map]
              exact rep_det hri' hrepi
            have hwidth : bv.width = w :=
              vty_rep_width (decode_vty hev) (decode_rep hev) hw
            refine ⟨decode_vty hev, ef2, ?_⟩
            have h1 : (NF.xcall w s (.xpack (pas.map (·.1)))).eval σ E
                = Rwv.Hyle.Sem.xapply E s w (Rwv.Hyle.Sem.bvcat reps) := by
              show Rwv.Hyle.Sem.xapply E s w ((NF.xpack (pas.map (·.1))).eval σ E) = _
              rw [Rwv.Hyle.Bridge.NF.xpack_eval, ← hre]
            have heval : (NF.xcall w s (.xpack (pas.map (·.1)))).eval σ E = bv := by
              rw [h1]
              simp only [Rwv.Hyle.Sem.xapply, hEs, hbv]
              rw [if_pos hwidth]
            rw [heval]
            exact decode_rep hev
          have hbxF : (b == .«extern») = false := by
            revert hbx
            cases b == .«extern» <;> simp
          rw [if_neg (by simp [hbcF]), if_neg (by simp [hbxF])] at hev
          obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
          obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
          split at hc
          · -- rwPrimError: the zero value of the applied type
            rw [bind_ok_iff] at hc
            obtain ⟨sz, hsz, hc⟩ := hc
            rw [bind_ok_iff] at hc
            obtain ⟨zv, hzv, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hvtyB
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i bv hrepzv
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hbv
            injection hc with hc
            injection hc with hnf hty
            subst hnf; subst hty
            cases efuel with
            | zero =>
                rw [Eval.evalBuiltin.eq_def] at hev
                exact error_ne_ok hev
            | succ ef2 =>
                rw [Eval.evalBuiltin.eq_def] at hev
                dsimp only at hev
                rw [hvlen] at hev
                have hfzv : v = zv := zeroVal_det hev hzv
                subst hfzv
                refine ⟨vtyB_sound hvtyB, fuel + 1, ?_⟩
                rw [hrepzv]
                congr 1
                exact beq_iff_eq.mp hbv
          · -- rwPrimVecFromList over a list literal
            cases args with
            | nil => exact error_ne_ok hc
            | cons a1 rest1 =>
            cases rest1 with
            | cons a2 rest2 => cases a1 <;> exact error_ne_ok hc
            | nil =>
            cases a1 with
            | var xv => exact error_ne_ok hc
            | con cty2 c2 => exact error_ne_ok hc
            | prim pty2 b2 => exact error_ne_ok hc
            | litInt ty2 n2 => exact error_ne_ok hc
            | litStr s2 => exact error_ne_ok hc
            | litVec ty2 es2 => exact error_ne_ok hc
            | app e2 a2 => exact error_ne_ok hc
            | lam x2 e2 => exact error_ne_ok hc
            | letE b2 e2 => exact error_ne_ok hc
            | jump l2 es2 => exact error_ne_ok hc
            | cases ty2 sc2 b2 alts2 => exact error_ne_ok hc
            | litList lty els =>
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨pas, hpas, hc⟩ := hc
            have hvs1 : vs.length = 1 := by simpa using hvlen
            obtain ⟨v1, hvs1'⟩ := list_len1 hvs1
            subst hvs1'
            obtain ⟨_, k0, hev0⟩ := hpt2 0 (by simp)
            simp only [List.getElem_cons_zero] at hev0
            cases k0 with
            | zero => rw [Eval.evalCore] at hev0; exact error_ne_ok hev0
            | succ k0 =>
                rw [Eval.evalCore] at hev0
                rw [show Eval.flattenApp (.litList lty els) = (.litList lty els, [])
                      from rfl] at hev0
                dsimp only at hev0
                obtain ⟨ws, hws, hev0⟩ := except_bind_eq_ok hev0
                rw [except_pure_def] at hev0
                injection hev0 with hev0
                cases efuel with
                | zero =>
                    rw [Eval.evalBuiltin.eq_def] at hev
                    exact error_ne_ok hev
                | succ ef2 =>
                rw [Eval.evalBuiltin.eq_def] at hev
                dsimp only at hev
                obtain ⟨xs, hxs, hev⟩ := except_bind_eq_ok hev
                obtain ⟨n', hn', hev⟩ := except_bind_eq_ok hev
                split at hev
                rotate_left
                · exact error_ne_ok hev
                rename_i hlen'
                rw [except_pure_def] at hev
                injection hev with hev
                subst hev
                have hxs' : v1 = .vec xs := vecVal_inv hxs
                rw [← hev0] at hxs'
                injection hxs' with hxs'
                have hxsws : xs = ws := hxs'.symm
                subst hxsws
                obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
                obtain ⟨hwlen, hptI⟩ := evalList_ok_idx hws
                have hptw : ∀ i (h1 : i < pas.length) (h2 : i < xs.length),
                    VTy Δ xs[i] pas[i].2 ∧
                      ∃ k, Val.rep Δ k xs[i] = .ok (pas[i].1.eval σ E) := by
                  intro i h1 h2
                  obtain ⟨hia, hci⟩ := hpt i (by omega)
                  obtain ⟨hia2, ki, hei⟩ := hptI i (by omega)
                  exact ih Γ jΓ els[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv xs[i] [] xs[i]
                    hci hei (applyMany_one ⟨Δ, dmap⟩ 0 xs[i]) hΓ hJ rfl
                    (fun j hj1 _ => absurd hj1 (by simp))
                split at hc
                rotate_left
                · exact error_ne_ok hc
                rename_i nt te heq
                split at hc
                rotate_left
                · exact error_ne_ok hc
                rename_i k hk
                split at hc
                rotate_left
                · exact error_ne_ok hc
                rename_i hlenk
                split at hc
                rotate_left
                · exact error_ne_ok hc
                rename_i hteq
                rw [bind_ok_iff] at hc
                obtain ⟨se, hse, hc⟩ := hc
                injection hc with hc
                injection hc with hnf hty
                subst hnf; subst hty
                obtain ⟨nt2, te2, hflr2, hn2⟩ := vecLen_inv hn'
                rw [heq] at hflr2
                have hnk : n' = k := by
                  have hp : nt = nt2 ∧ te = te2 := by simpa using hflr2
                  obtain ⟨hp1, _⟩ := hp
                  subst hp1
                  rw [hk] at hn2
                  exact (Option.some.inj hn2).symm
                have hxslen : xs.length = k := by
                  have h2 : xs.length = n' := by simpa using hlen'
                  omega
                have htys := teqAll_types hteq
                have hpaste : ∀ i (h : i < pas.length), pas[i].2 = te := by
                  intro i h
                  have h1 : (pas.map (·.2))[i]'(by simpa using h)
                      = (List.replicate pas.length te)[i]'(by simpa using h) :=
                    List.getElem_of_eq htys _
                  simpa using h1
                constructor
                · refine VTy.vec heq hk hxslen ?_
                  intro e' he'
                  obtain ⟨j, hj, hej⟩ := List.getElem_of_mem he'
                  rw [← hej]
                  have := (hptw j (by omega) hj).1
                  rw [hpaste j (by omega)] at this
                  exact this
                · obtain ⟨K, hK⟩ := mapM_rep_exists (Δ := Δ) (vs := xs)
                    (bs := pas.map (fun p => p.1.eval σ E)) (by simp only [List.length_map]; omega)
                    (fun i h1 h2 => by
                      obtain ⟨_, hex⟩ := hptw i (by omega) h1
                      obtain ⟨k1, hrep1⟩ := hex
                      exact ⟨k1, by simpa using hrep1⟩)
                  refine ⟨K + 1, ?_⟩
                  rw [Val.rep, mapM_attach_erase, hK, except_bind_ok, except_pure_def]
                  congr 1
                  have hpwidths : ∀ p ∈ (pas.map (·.1)).map (·, se),
                      (p.1.eval σ E).width = p.2 := by
                    intro p hp
                    obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                    have hj2 : j < pas.length := by
                      simp only [List.length_map] at hj
                      omega
                    rw [← hpj]
                    simp only [List.getElem_map]
                    obtain ⟨hv1, hex⟩ := hptw j hj2 (by omega)
                    obtain ⟨k1, hrep1⟩ := hex
                    rw [hpaste j hj2] at hv1
                    exact vty_rep_width hv1 hrep1 hse
                  rw [catNF_eval σ _ hpwidths, List.map_map, List.map_map, bvConcat_eq]
                  rfl
          · -- rwPrimBitIndex at a syntactic Finite literal
            cases args with
            | nil => exact error_ne_ok hc
            | cons argE rest1 =>
            cases rest1 with
            | nil => exact error_ne_ok hc
            | cons iE rest2 =>
            cases rest2 with
            | cons a3 rest3 => exact error_ne_ok hc
            | nil =>
            dsimp only at hc
            cases hfin : finLitE iE with
            | none => rw [hfin] at hc; exact error_ne_ok hc
            | some iidx =>
            rw [hfin] at hc
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨ant, harg, hc⟩ := hc
            obtain ⟨a, ta⟩ := ant
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨wa, hwa, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hres
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hiwa
            injection hc with hc
            injection hc with hnf hty
            subst hnf; subst hty
            have hvs2 : vs.length = 2 := by simpa using hvlen
            obtain ⟨va, vi, hvs2'⟩ := list_len2 hvs2
            subst hvs2'
            obtain ⟨_, ka0, heva⟩ := hpt2 0 (by simp)
            obtain ⟨_, ki0, hevi⟩ := hpt2 1 (by simp)
            simp only [List.getElem_cons_zero, List.getElem_cons_succ] at heva hevi
            obtain ⟨hvta, kra, hrepa⟩ := ih Γ jΓ argE [] a ta ka0 1 env jenv va [] va
              harg heva (applyMany_one ⟨Δ, dmap⟩ 0 va) hΓ hJ rfl
              (fun j hj1 _ => absurd hj1 (by simp))
            obtain ⟨ib, hfinv⟩ := finLit_val hfin hevi
            cases efuel with
            | zero =>
                rw [Eval.evalBuiltin.eq_def] at hev
                exact error_ne_ok hev
            | succ ef2 =>
                rw [Eval.evalBuiltin.eq_def] at hev
                dsimp only at hev
                obtain ⟨bi, hbi, hev⟩ := except_bind_eq_ok hev
                obtain ⟨b0, ival⟩ := bi
                dsimp only at hev
                obtain ⟨x, hx, hev⟩ := except_bind_eq_ok hev
                rw [except_pure_def] at hev
                injection hev with hev
                subst hev
                have hvi2 : vi = .finite b0 ival := finVal_inv hbi
                rw [hvi2] at hfinv
                injection hfinv with _ hival
                rw [Eval.valToBits] at hx
                have hxa : x = a.eval σ E := rep_det hx hrepa
                subst hxa
                constructor
                · exact vty_bool_at hΔ (by simpa [isBoolT] using hres) _
                · refine ⟨2, ?_⟩
                  rw [rep_boolVal hΔ _ 0]
                  congr 1
                  rw [sliceNF_eval]
                  rw [show sliceBV (a.eval σ E) iidx 1
                        = (⟨1, (a.eval σ E).bits.extractLsb' iidx 1⟩ : BV) from rfl]
                  rw [extract_one_b1]
                  rw [hival]
          · -- rwPrimBitSlice at syntactic Finite literals
            cases args with
            | nil => exact error_ne_ok hc
            | cons argE rest1 =>
            cases rest1 with
            | nil => exact error_ne_ok hc
            | cons jE rest2 =>
            cases rest2 with
            | nil => exact error_ne_ok hc
            | cons iE rest3 =>
            cases rest3 with
            | cons a4 rest4 => exact error_ne_ok hc
            | nil =>
            dsimp only at hc
            cases hfinj : finLitE jE with
            | none => rw [hfinj] at hc; exact error_ne_ok hc
            | some jidx =>
            rw [hfinj] at hc
            cases hfini : finLitE iE with
            | none => rw [hfini] at hc; dsimp only at hc; exact error_ne_ok hc
            | some iidx =>
            rw [hfini] at hc
            dsimp only at hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hji
            rw [bind_ok_iff] at hc
            obtain ⟨ant, harg, hc⟩ := hc
            obtain ⟨a, ta⟩ := ant
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨wa, hwa, hc⟩ := hc
            rw [bind_ok_iff] at hc
            obtain ⟨mr, hmr, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hguard
            obtain ⟨hmrm, hrange⟩ := hguard
            injection hc with hc
            injection hc with hnf hty
            subst hnf; subst hty
            have hvs3 : vs.length = 3 := by simpa using hvlen
            obtain ⟨va, vj, vi, hvs3'⟩ := (by
              match vs, hvs3 with
              | [va, vj, vi], _ => exact ⟨va, vj, vi, rfl⟩ :
              ∃ va vj vi, vs = [va, vj, vi])
            subst hvs3'
            obtain ⟨_, ka0, heva⟩ := hpt2 0 (by simp)
            obtain ⟨_, kj0, hevj⟩ := hpt2 1 (by simp)
            obtain ⟨_, ki0, hevi⟩ := hpt2 2 (by simp)
            simp only [List.getElem_cons_zero, List.getElem_cons_succ] at heva hevj hevi
            obtain ⟨hvta, kra, hrepa⟩ := ih Γ jΓ argE [] a ta ka0 1 env jenv va [] va
              harg heva (applyMany_one ⟨Δ, dmap⟩ 0 va) hΓ hJ rfl
              (fun j hj1 _ => absurd hj1 (by simp))
            obtain ⟨jb0, hfinvj⟩ := finLit_val hfinj hevj
            obtain ⟨ib0, hfinvi⟩ := finLit_val hfini hevi
            cases efuel with
            | zero =>
                rw [Eval.evalBuiltin.eq_def] at hev
                exact error_ne_ok hev
            | succ ef2 =>
                rw [Eval.evalBuiltin.eq_def] at hev
                dsimp only at hev
                obtain ⟨bj, hbj, hev⟩ := except_bind_eq_ok hev
                obtain ⟨bj0, jval⟩ := bj
                dsimp only at hev
                obtain ⟨bi, hbi, hev⟩ := except_bind_eq_ok hev
                obtain ⟨bi0, ival⟩ := bi
                dsimp only at hev
                split at hev
                · exact error_ne_ok hev
                rename_i hji2
                rw [bind_ok_iff] at hev
                obtain ⟨x, hx, hev⟩ := hev
                rw [except_pure_def] at hev
                injection hev with hev
                subst hev
                have hvj2 : vj = .finite bj0 jval := finVal_inv hbj
                have hvi2 : vi = .finite bi0 ival := finVal_inv hbi
                rw [hvj2] at hfinvj
                rw [hvi2] at hfinvi
                injection hfinvj with _ hjval
                injection hfinvi with _ hival
                rw [Eval.valToBits] at hx
                have hxa : x = a.eval σ E := rep_det hx hrepa
                subst hxa
                obtain ⟨ltr, etr, hflr, hbetr, hmltr⟩ := vecBoolLen_inv hmr
                constructor
                · exact vty_bitsToVec hΔ hflr hmltr hbetr (by
                    show jval + 1 - ival = mr
                    rw [hjval, hival, hmrm])
                · refine ⟨3, ?_⟩
                  rw [rep_bitsToVec hΔ _ 0]
                  congr 1
                  rw [sliceNF_eval]
                  rw [show sliceBV (a.eval σ E) iidx (jidx + 1 - iidx)
                        = (⟨jidx + 1 - iidx,
                            (a.eval σ E).bits.extractLsb' iidx (jidx + 1 - iidx)⟩ : BV)
                      from rfl]
                  rw [hival, hjval]
          · -- rwPrimVecMap: unrolled per element, the function argument
            -- applied through the pending discipline; each element
            -- relates to the evaluator's `applyAll` loop through the
            -- `applyMany` leg.
            cases args with
            | nil => exact error_ne_ok hc
            | cons fE rest1 =>
            cases rest1 with
            | nil => exact error_ne_ok hc
            | cons argE rest2 =>
            cases rest2 with
            | cons a3 rest3 => exact error_ne_ok hc
            | nil =>
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨nto, hres, hc⟩ := hc
            obtain ⟨nElems, teO⟩ := nto
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨seO, hseO, hc⟩ := hc
            rw [bind_ok_iff] at hc
            obtain ⟨ant, harg, hc⟩ := hc
            obtain ⟨a, ta⟩ := ant
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨nti, hlenI, hc⟩ := hc
            obtain ⟨len, teI⟩ := nti
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨seI, hseI, hc⟩ := hc
            rw [bind_ok_iff] at hc
            obtain ⟨szV, hszV, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hlenN
            rw [bind_ok_iff] at hc
            obtain ⟨ps, hps, hc⟩ := hc
            injection hc with hc
            injection hc with hnf hty
            subst hnf; subst hty
            -- the evaluator's two arguments
            have hvs2 : vs.length = 2 := by simpa using hvlen
            obtain ⟨fv, av, hvs2'⟩ := list_len2 hvs2
            subst hvs2'
            obtain ⟨_, kf0, hevf⟩ := hpt2 0 (by simp)
            obtain ⟨_, ka0, heva⟩ := hpt2 1 (by simp)
            simp only [List.getElem_cons_zero, List.getElem_cons_succ] at hevf heva
            obtain ⟨hvta, kra, hrepa⟩ := ih Γ jΓ argE [] a ta ka0 1 env jenv av [] av
              harg heva (applyMany_one ⟨Δ, dmap⟩ 0 av) hΓ hJ rfl
              (fun j hj1 _ => absurd hj1 (by simp))
            -- the evaluator's row
            cases efuel with
            | zero =>
                rw [Eval.evalBuiltin.eq_def] at hev
                exact error_ne_ok hev
            | succ ef2 =>
            rw [Eval.evalBuiltin.eq_def] at hev
            dsimp only at hev
            obtain ⟨xs, hxs, hev⟩ := except_bind_eq_ok hev
            obtain ⟨ys, hys, hev⟩ := except_bind_eq_ok hev
            rw [except_pure_def] at hev
            injection hev with hev
            subst hev
            -- the argument value is a canonical vector
            have hav : av = .vec xs := vecVal_inv hxs
            obtain ⟨ntI, hflta, hnta⟩ := vecLenElem_inv hlenI
            obtain ⟨elems, hav2, hellen, helems⟩ := vty_vec_inv hΔ hflta hnta hvta
            rw [hav] at hav2
            injection hav2 with hav2
            subst hav2
            -- its representation, elementwise
            rw [hav] at hrepa
            obtain ⟨kr, rs, _, hrs, hacat⟩ := rep_vec_inv hrepa
            obtain ⟨hrslen, hrspt⟩ := mapM_ok_idx hrs
            have hrw : ∀ x ∈ rs, x.width = seI := elem_widths helems hrs hseI
            -- the vector width is the element count times the element width
            obtain ⟨kk, we, hkk, hwe, hszVe⟩ := sizeOf_inv_vec hflta hszV
            have hkk2 : len = kk := by
              rw [hnta] at hkk
              exact Option.some.inj hkk
            subst hkk2
            have hwe2 : seI = we := (sizeOf_det hwe hseI).symm
            subst hwe2
            -- the compiled elements, pointwise
            obtain ⟨hpslen, hpspt⟩ := mapM_ok_idx hps
            have hpslen' : ps.length = nElems := by
              rw [hpslen, List.length_range]
            obtain ⟨hyslen, hyspt⟩ := applyAll_ok_idx hys
            have hxslen : xs.length = len := hellen
            -- per-element soundness through the `applyMany` leg
            have helem : ∀ i (hi : i < nElems),
                VTy Δ (ys[i]'(by omega)) teO ∧
                (∃ k, Val.rep Δ k (ys[i]'(by omega))
                    = .ok ((ps[i]'(by omega)).1.eval σ E)) ∧
                (ps[i]'(by omega)).2 = seO := by
              intro i hi
              obtain ⟨hi2, hci⟩ := hpspt i (by rw [List.length_range]; omega)
              rw [List.getElem_range] at hci
              obtain ⟨eti, hcei, hifi⟩ := except_bind_eq_ok hci
              obtain ⟨ei, ti⟩ := eti
              dsimp only at hifi
              split at hifi
              rotate_left
              · exact error_ne_ok hifi
              rename_i hteqi
              injection hifi with hifi
              obtain ⟨K, hKi⟩ := hyspt i (by omega) (by omega)
              have hsl : (sliceNF (szV - (i + 1) * seI) seI a).eval σ E
                  = rs[i]'(by omega) := by
                rw [sliceNF_eval, hacat]
                have hoff : szV - (i + 1) * seI = (rs.length - i - 1) * seI := by
                  rw [hszVe, hrslen, hxslen,
                      sub_mul_offsets (show i + 1 ≤ len by omega), Nat.add_mul,
                      Nat.sub_sub]
                rw [hoff]
                exact slice_singleton hrw (by omega)
              obtain ⟨hvy, ky, hky⟩ := ih Γ jΓ fE
                [(sliceNF (szV - (i + 1) * seI) seI a, teI)] ei ti kf0 K env jenv
                fv [xs[i]'(by omega)] (ys[i]'(by omega)) hcei hevf hKi hΓ hJ rfl
                (by
                  intro j hj1 hj2
                  match j with
                  | 0 =>
                      simp only [List.getElem_cons_zero]
                      refine ⟨helems _ (List.getElem_mem _), kr, ?_⟩
                      show Val.rep Δ kr (xs[i]'(by omega))
                        = .ok ((sliceNF (szV - (i + 1) * seI) seI a).eval σ E)
                      rw [hsl]
                      exact (hrspt i (by omega)).2
                  | j + 1 => exact absurd hj1 (by simp))
              rw [← hifi]
              exact ⟨teq_eq hteqi ▸ hvy, ⟨ky, hky⟩, rfl⟩
            -- assemble the vector value
            obtain ⟨ntO, hflo, hno⟩ := vecLenElem_inv hres
            have hyslen2 : ys.length = nElems := by omega
            constructor
            · refine VTy.vec hflo hno hyslen2 ?_
              intro e' he'
              obtain ⟨j, hj, hej⟩ := List.getElem_of_mem he'
              rw [← hej]
              exact (helem j (by omega)).1
            · obtain ⟨K, hK⟩ := mapM_rep_exists (Δ := Δ) (vs := ys)
                (bs := ps.map (fun p => p.1.eval σ E))
                (by rw [List.length_map]; omega)
                (fun i h1 h2 => by
                  obtain ⟨_, ⟨k1, hrep1⟩, _⟩ := helem i (by omega)
                  exact ⟨k1, by simpa using hrep1⟩)
              refine ⟨K + 1, ?_⟩
              rw [Val.rep, mapM_attach_erase, hK, except_bind_ok, except_pure_def]
              congr 1
              have hpwidths : ∀ p ∈ ps, (p.1.eval σ E).width = p.2 := by
                intro p hp
                obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                obtain ⟨hvyj, ⟨kyj, hkyj⟩, hsnd⟩ := helem j (by omega)
                rw [← hpj, hsnd]
                exact vty_rep_width hvyj hkyj hseO
              rw [catNF_eval σ _ hpwidths]
              exact bvConcat_eq _
          · -- rwPrimVecGenerate: index literals at width `nbits nElems`,
            -- applied through the pending discipline.
            cases args with
            | nil => exact error_ne_ok hc
            | cons fE rest1 =>
            cases rest1 with
            | cons a2 rest2 => exact error_ne_ok hc
            | nil =>
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨nto, hres, hc⟩ := hc
            obtain ⟨nElems, teO⟩ := nto
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨seO, hseO, hc⟩ := hc
            rw [bind_ok_iff] at hc
            obtain ⟨ft, hft, hc⟩ := hc
            cases ft with
            | con c => exact error_ne_ok hc
            | app t1 t2 => exact error_ne_ok hc
            | var xv => exact error_ne_ok hc
            | nat nn => exact error_ne_ok hc
            | arrow tFin tR =>
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨nb, hnb, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hnbe
            rw [bind_ok_iff] at hc
            obtain ⟨ps, hps, hc⟩ := hc
            injection hc with hc
            injection hc with hnf hty
            subst hnf; subst hty
            -- the evaluator's single argument
            have hvs1 : vs.length = 1 := by simpa using hvlen
            obtain ⟨fv, hvs1'⟩ := list_len1 hvs1
            subst hvs1'
            obtain ⟨_, kf0, hevf⟩ := hpt2 0 (by simp)
            simp only [List.getElem_cons_zero] at hevf
            -- the evaluator's row
            cases efuel with
            | zero =>
                rw [Eval.evalBuiltin.eq_def] at hev
                exact error_ne_ok hev
            | succ ef2 =>
            rw [Eval.evalBuiltin.eq_def] at hev
            dsimp only at hev
            obtain ⟨n', hn', hev⟩ := except_bind_eq_ok hev
            obtain ⟨ys, hys, hev⟩ := except_bind_eq_ok hev
            rw [except_pure_def] at hev
            injection hev with hev
            subst hev
            -- the two length readings agree
            obtain ⟨ntO, hflo, hno⟩ := vecLenElem_inv hres
            obtain ⟨nt2, te2, hflr2, hn2⟩ := vecLen_inv hn'
            rw [hflo] at hflr2
            have hn'e : nElems = n' := by
              have hp : ntO = nt2 ∧ teO = te2 := by simpa using hflr2
              obtain ⟨hp1, _⟩ := hp
              subst hp1
              rw [hno] at hn2
              exact Option.some.inj hn2
            subst hn'e
            -- the Finite domain type
            obtain ⟨btF, hflF, hbtF⟩ := finBoundT_inv hnb
            have hnbe' : nElems = nb := hnbe.symm
            subst hnbe'
            -- the compiled elements, pointwise
            obtain ⟨hpslen, hpspt⟩ := mapM_ok_idx hps
            have hpslen' : ps.length = nElems := by
              rw [hpslen, List.length_range]
            obtain ⟨hyslen, hyspt⟩ := applyAll_ok_idx hys
            rw [List.length_map, List.length_range] at hyslen
            -- per-element soundness through the `applyMany` leg
            have helem : ∀ i (hi : i < nElems),
                VTy Δ (ys[i]'(by omega)) teO ∧
                (∃ k, Val.rep Δ k (ys[i]'(by omega))
                    = .ok ((ps[i]'(by omega)).1.eval σ E)) ∧
                (ps[i]'(by omega)).2 = seO := by
              intro i hi
              obtain ⟨hi2, hci⟩ := hpspt i (by rw [List.length_range]; omega)
              rw [List.getElem_range] at hci
              obtain ⟨eti, hcei, hifi⟩ := except_bind_eq_ok hci
              obtain ⟨ei, ti⟩ := eti
              dsimp only at hifi
              split at hifi
              rotate_left
              · exact error_ne_ok hifi
              rename_i hteqi
              injection hifi with hifi
              obtain ⟨K, hKi⟩ := hyspt i
                (by simp only [List.length_map, List.length_range]; omega) (by omega)
              simp only [List.getElem_map, List.getElem_range] at hKi
              obtain ⟨hvy, ky, hky⟩ := ih Γ jΓ fE
                [(.lit ⟨nbits nElems, BitVec.ofNat (nbits nElems) i⟩, tFin)] ei ti kf0 K
                env jenv fv [Val.finite nElems i] (ys[i]'(by omega)) hcei hevf hKi hΓ hJ
                rfl
                (by
                  intro j hj1 hj2
                  match j with
                  | 0 =>
                      simp only [List.getElem_cons_zero]
                      refine ⟨VTy.finite hflF hbtF (by omega), 1, ?_⟩
                      rw [Val.rep, except_pure_def]
                      rfl
                  | j + 1 => exact absurd hj1 (by simp))
              rw [← hifi]
              exact ⟨teq_eq hteqi ▸ hvy, ⟨ky, hky⟩, rfl⟩
            -- assemble the vector value
            constructor
            · refine VTy.vec hflo hno (by omega) ?_
              intro e' he'
              obtain ⟨j, hj, hej⟩ := List.getElem_of_mem he'
              rw [← hej]
              exact (helem j (by omega)).1
            · obtain ⟨K, hK⟩ := mapM_rep_exists (Δ := Δ) (vs := ys)
                (bs := ps.map (fun p => p.1.eval σ E))
                (by rw [List.length_map]; omega)
                (fun i h1 h2 => by
                  obtain ⟨_, ⟨k1, hrep1⟩, _⟩ := helem i (by omega)
                  exact ⟨k1, by simpa using hrep1⟩)
              refine ⟨K + 1, ?_⟩
              rw [Val.rep, mapM_attach_erase, hK, except_bind_ok, except_pure_def]
              congr 1
              have hpwidths : ∀ p ∈ ps, (p.1.eval σ E).width = p.2 := by
                intro p hp
                obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                obtain ⟨hvyj, ⟨kyj, hkyj⟩, hsnd⟩ := helem j (by omega)
                rw [← hpj, hsnd]
                exact vty_rep_width hvyj hkyj hseO
              rw [catNF_eval σ _ hpwidths]
              exact bvConcat_eq _
          · -- the (excluded) cryptol arm: contradicts the dispatch guard
            simp at hbcF
          · -- the (excluded) extern arm: contradicts the dispatch guard
            simp at hbxF
          · -- the extended row table
            rw [bind_ok_iff] at hc
            obtain ⟨pas, hpas, hc⟩ := hc
            obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
            have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
                VTy Δ vs[i] pas[i].2 ∧
                  ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
              intro i h1 h2
              obtain ⟨hia, hci⟩ := hpt i (by omega)
              obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
              exact ih Γ jΓ args[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i] [] vs[i]
                hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
                (fun j hj1 _ => absurd hj1 (by simp))
            exact cprimF_sound hΔ hc hev (by omega) hptw
      | litInt tyL n =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil =>
          cases pend with
          | cons p ps =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
          have hvsp : vsp = [] := List.length_eq_zero_iff.mp (by simpa using hvl)
          subst hvsp
          have hvf : v = f := applyMany_nil_inv happ
          subst hvf
          dsimp only at hc hev
          obtain ⟨hrty, hshape⟩ := clitInt_inv hc
          subst hrty
          rcases hshape with ⟨hfl2, hnf⟩ | ⟨bt, k, hfl2, hk, hnf⟩ | ⟨lt, et, w, hfl2, het, hlt, hnf⟩
          · subst hnf
            have hv := litIntVal_inv_integer hfl2 hev
            subst hv
            exact ⟨VTy.integer hfl2, 1, by rw [Val.rep]; rfl⟩
          · subst hnf
            obtain ⟨h0, hk2, hv⟩ := litIntVal_inv_finite hfl2 hk hev
            subst hv
            refine ⟨VTy.finite hfl2 hk (by omega), 1, ?_⟩
            rw [Val.rep]
            rw [show (NF.lit (⟨nbits k, BitVec.ofInt (nbits k) n⟩ : BV)).eval σ E
                  = (⟨nbits k, BitVec.ofInt (nbits k) n⟩ : BV) from rfl]
            rw [ofInt_nonneg h0]
            rfl
          · subst hnf
            have hv := litIntVal_inv_vec hfl2 hlt hev
            subst hv
            exact ⟨vty_bitsToVec hΔ hfl2 hlt het rfl, 3, rep_bitsToVec hΔ _ 0⟩
      | litVec vty es =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil =>
          cases pend with
          | cons p ps =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
          have hvsp : vsp = [] := List.length_eq_zero_iff.mp (by simpa using hvl)
          subst hvsp
          have hvf : v = f := applyMany_nil_inv happ
          subst hvf
          dsimp only at hc hev
          obtain ⟨pas, hpas, hc⟩ := except_bind_eq_ok hc
          obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
          rw [except_pure_def] at hev
          injection hev with hev
          subst hev
          obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
          obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
          have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
              VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
            intro i h1 h2
            obtain ⟨hia, hci⟩ := hpt i (by omega)
            obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
            exact ih Γ jΓ es[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i] [] vs[i]
              hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
              (fun j hj1 _ => absurd hj1 (by simp))
          split at hc
          rotate_left
          · exact error_ne_ok hc
          rename_i nt te heq
          split at hc
          rotate_left
          · exact error_ne_ok hc
          rename_i k hk
          split at hc
          rotate_left
          · exact error_ne_ok hc
          rename_i hlenk
          split at hc
          rotate_left
          · exact error_ne_ok hc
          rename_i hteq
          obtain ⟨se, hse, hc⟩ := except_bind_eq_ok hc
          injection hc with hc
          injection hc with hnf hty
          subst hnf; subst hty
          have htys := teqAll_types hteq
          have hpaste : ∀ i (h : i < pas.length), pas[i].2 = te := by
            intro i h
            have h1 : (pas.map (·.2))[i]'(by simpa using h)
                = (List.replicate pas.length te)[i]'(by simpa using h) :=
              List.getElem_of_eq htys _
            simpa using h1
          have hvtys : ∀ i (h : i < vs.length), VTy Δ vs[i] te := by
            intro i h
            have := (hptw i (by omega) h).1
            rw [hpaste i (by omega)] at this
            exact this
          constructor
          · refine VTy.vec heq hk (by omega) ?_
            intro e' he'
            obtain ⟨j, hj, hej⟩ := List.getElem_of_mem he'
            rw [← hej]
            exact hvtys j hj
          · obtain ⟨K, hK⟩ := mapM_rep_exists (Δ := Δ) (vs := vs)
              (bs := pas.map (fun p => p.1.eval σ E)) (by simpa using by omega)
              (fun i h1 h2 => by
                obtain ⟨_, hex⟩ := hptw i (by omega) h1
                obtain ⟨k1, hrep1⟩ := hex
                exact ⟨k1, by simpa using hrep1⟩)
            refine ⟨K + 1, ?_⟩
            rw [Val.rep, mapM_attach_erase, hK, except_bind_ok, except_pure_def]
            congr 1
            have hpwidths : ∀ p ∈ (pas.map (·.1)).map (·, se), (p.1.eval σ E).width = p.2 := by
              intro p hp
              obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
              have hj2 : j < pas.length := by
                simp only [List.length_map] at hj
                omega
              rw [← hpj]
              simp only [List.getElem_map]
              obtain ⟨hv1, hex⟩ := hptw j hj2 (by omega)
              obtain ⟨k1, hrep1⟩ := hex
              rw [hpaste j hj2] at hv1
              exact vty_rep_width hv1 hrep1 hse
            rw [catNF_eval σ _ hpwidths, List.map_map, List.map_map, bvConcat_eq]
            rfl
      | lam x b =>
          dsimp only at hc hev
          rw [bind_ok_iff] at hc
          obtain ⟨pas, hpas, hc⟩ := hc
          obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
          obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
          obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
          have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
              VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
            intro i h1 h2
            obtain ⟨hia, hci⟩ := hpt i (by omega)
            obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
            exact ih Γ jΓ args[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i] [] vs[i]
              hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
              (fun j hj1 _ => absurd hj1 (by simp))
          have hall := pointwise_append (by omega) hvl hptw hvp
          have hlall : (vs ++ vsp).length = (pas ++ pend).length := by
            rw [List.length_append, List.length_append]
            omega
          obtain ⟨K, hKall⟩ := applyMany_compose hev happ
          cases hpp : pas ++ pend with
          | nil => rw [hpp] at hc; exact error_ne_ok hc
          | cons nt rest =>
              rw [hpp] at hc
              cases hvv : vs ++ vsp with
              | nil =>
                  exfalso
                  rw [hvv, hpp] at hlall
                  simp at hlall
              | cons va vrest =>
                  rw [hvv] at hKall
                  cases K with
                  | zero => rw [Eval.applyMany] at hKall; exact error_ne_ok hKall
                  | succ K =>
                      rw [Eval.applyMany] at hKall
                      obtain ⟨v1, hv1, hKall⟩ := except_bind_eq_ok hKall
                      cases K with
                      | zero => rw [Eval.applyValCore] at hv1; exact error_ne_ok hv1
                      | succ K =>
                          rw [Eval.applyValCore] at hv1
                          rw [hpp, hvv] at hall
                          have hhead := hall 0 (by simp) (by simp)
                          simp only [List.getElem_cons_zero] at hhead
                          refine ih (Γ.insert x.uniq nt) [] b rest nf ty K (K + 1)
                            ((x.uniq, va) :: env) [] v1 vrest v hc hv1 hKall
                            (envC_cons hΓ hhead.1 hhead.2) jenvC_nil
                            ?_ ?_
                          · rw [hpp, hvv] at hlall
                            simpa using hlall
                          · intro i h1 h2
                            have := hall (i + 1) (by simpa using h1) (by simpa using h2)
                            simpa using this
      | letE bnd body =>
          cases bnd with
          | nonRec x rhs =>
              dsimp only at hc hev
              rw [bind_ok_iff] at hc
              obtain ⟨nt, hrhs, hc⟩ := hc
              obtain ⟨ntn, ntt⟩ := nt
              rw [bind_ok_iff] at hc
              obtain ⟨pas, hpas, hc⟩ := hc
              obtain ⟨v0, hv0, hev⟩ := except_bind_eq_ok hev
              obtain ⟨rv, hrv, hbody⟩ := except_bind_eq_ok hv0
              obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
              obtain ⟨K, hKall⟩ := applyMany_compose hev happ
              obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
              obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
              have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
                  VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
                intro i h1 h2
                obtain ⟨hia, hci⟩ := hpt i (by omega)
                obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
                exact ih Γ jΓ args[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i] [] vs[i]
                  hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
                  (fun j hj1 _ => absurd hj1 (by simp))
              have hall := pointwise_append (by omega) hvl hptw hvp
              obtain ⟨hvty1, k1, hrep1⟩ := ih Γ jΓ rhs [] ntn ntt efuel 1 env jenv rv [] rv
                hrhs hrv (applyMany_one ⟨Δ, dmap⟩ 0 rv) hΓ hJ rfl
                (fun j hj1 _ => absurd hj1 (by simp))
              exact ih (Γ.insert x.uniq (ntn, ntt)) jΓ body (pas ++ pend) nf ty efuel K
                ((x.uniq, rv) :: env) jenv v0 (vs ++ vsp) v hc hbody hKall
                (envC_cons hΓ hvty1 ⟨k1, hrep1⟩) hJ
                (by rw [List.length_append, List.length_append]; omega) hall
          | recB bs =>
              dsimp only at hc
              exact error_ne_ok hc
          | join l ps jb =>
              dsimp only at hc hev
              rw [bind_ok_iff] at hc
              obtain ⟨pas, hpas, hc⟩ := hc
              obtain ⟨v0, hv0, hev⟩ := except_bind_eq_ok hev
              obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
              obtain ⟨K, hKall⟩ := applyMany_compose hev happ
              obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
              obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
              have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
                  VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
                intro i h1 h2
                obtain ⟨hia, hci⟩ := hpt i (by omega)
                obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
                exact ih Γ jΓ args[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i] [] vs[i]
                  hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
                  (fun j hj1 _ => absurd hj1 (by simp))
              have hall := pointwise_append (by omega) hvl hptw hvp
              exact ih Γ ((l.uniq, CJoin.mk ps Γ jΓ jb) :: jΓ) body (pas ++ pend) nf ty
                efuel K env ((l.uniq, Eval.JoinClos.mk ps env jenv jb) :: jenv) v0
                (vs ++ vsp) v hc hv0 hKall hΓ
                (jenvC_cons hJ (jc_intro hΓ hJ))
                (by rw [List.length_append, List.length_append]; omega) hall
      | jump l es =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil =>
              dsimp only at hc hev
              rw [bind_ok_iff] at hc
              obtain ⟨pas, hpas, hc⟩ := hc
              obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
              cases hjx : jΓ.lookup l.uniq with
              | none => rw [hjx] at hc; exact error_ne_ok hc
              | some cj =>
                  rw [hjx] at hc
                  obtain ⟨ps, Γc, cjs, jb⟩ := cj
                  dsimp only at hc
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i hteq
                  obtain ⟨jc, hjc, hJC⟩ := hJ l.uniq _ hjx
                  cases hJC with
                  | mk hEc hcov hpt3 =>
                      rename_i envc cjenv
                      rw [hjc] at hev
                      dsimp only at hev
                      split at hev
                      rotate_left
                      · exact error_ne_ok hev
                      rename_i harr
                      obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
                      obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
                      have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
                          VTy Δ vs[i] pas[i].2 ∧
                            ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
                        intro i h1 h2
                        obtain ⟨hia, hci⟩ := hpt i (by omega)
                        obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
                        exact ih Γ jΓ es[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i]
                          [] vs[i] hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
                          (fun j hj1 _ => absurd hj1 (by simp))
                      have hlen1 : pas.length = ps.length := by
                        have := teqAll_length hteq
                        simpa using this
                      have hlen2 : vs.length = ps.length := by
                        have : (vs.length == ps.length) = true := harr
                        simpa using this
                      refine ih (bindFieldsΓ ps pas Γc) cjs jb pend nf ty efuel afuel
                        _ cjenv f vsp v hc hev happ ?_ ?_ hvl hvp
                      · exact envC_bind ps pas vs hEc hlen1 hlen2
                          (fun i h1 h2 h3 => hptw i h2 h3)
                      · intro l' cj' hl'
                        have hs := hcov l' cj' hl'
                        cases hjc' : cjenv.lookup l' with
                        | none => rw [hjc'] at hs; exact absurd hs (by simp)
                        | some jc' => exact ⟨jc', rfl, hpt3 l' cj' jc' hl' hjc'⟩
      | cases resTy scrut binder alts =>
          dsimp only at hc hev
          rw [bind_ok_iff] at hc
          obtain ⟨dnt, hdn, hc⟩ := hc
          obtain ⟨dn, dty⟩ := dnt
          dsimp only at hc
          rw [bind_ok_iff] at hc
          obtain ⟨szT, hsz, hc⟩ := hc
          rw [bind_ok_iff] at hc
          obtain ⟨pas, hpas, hc⟩ := hc
          rw [bind_ok_iff] at hc
          obtain ⟨resTy', hpeel, hc⟩ := hc
          obtain ⟨sv, hsv, hev⟩ := except_bind_eq_ok hev
          obtain ⟨v', hv', hev⟩ := except_bind_eq_ok hev
          obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
          obtain ⟨K, hKall⟩ := applyMany_compose hev happ
          obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
          obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
          have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
              VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ E) := by
            intro i h1 h2
            obtain ⟨hia, hci⟩ := hpt i (by omega)
            obtain ⟨hia2, ki, hei⟩ := hpt2 i (by omega)
            exact ih Γ jΓ args[i] [] (pas[i].1) (pas[i].2) ki 1 env jenv vs[i] [] vs[i]
              hci hei (applyMany_one ⟨Δ, dmap⟩ 0 vs[i]) hΓ hJ rfl
              (fun j hj1 _ => absurd hj1 (by simp))
          have hall := pointwise_append (by omega) hvl hptw hvp
          have hlall : (vs ++ vsp).length = (pas ++ pend).length := by
            rw [List.length_append, List.length_append]
            omega
          obtain ⟨hvty, ks, hks⟩ := ih Γ jΓ scrut [] dn dty efuel 1 env jenv sv [] sv
            hdn hsv (applyMany_one ⟨Δ, dmap⟩ 0 sv) hΓ hJ rfl
            (fun j hj1 _ => absurd hj1 (by simp))
          have hΓ' : EnvC (E := E) Δ σ (Γ.insert binder.uniq (dn, dty)) ((binder.uniq, sv) :: env) :=
            envC_cons hΓ hvty ⟨ks, hks⟩
          rcases alts with _ | ⟨⟨con0, bs0, dbody⟩, rest⟩
          · dsimp only at hc
            rw [cchainJ] at hc
            exact error_ne_ok hc
          · cases con0 with
            | default =>
                dsimp only at hc
                rw [bind_ok_iff] at hc
                obtain ⟨dnt2, hdb, hc⟩ := hc
                obtain ⟨dnf, dbt⟩ := dnt2
                dsimp only at hc
                split at hc
                rotate_left
                · exact error_ne_ok hc
                rename_i hteq2
                cases efuel with
                | zero => rw [Eval.tryAlts] at hv'; exact error_ne_ok hv'
                | succ ef2 =>
                    rw [Eval.tryAlts] at hv'
                    try dsimp only at hv'
                    obtain ⟨hty2, hvt, hr⟩ := cchainJ_sound hΔ ih hΓ' hJ hsz hvty hks
                      hlall hall rest (some dnf) (some (Alt.mk .default bs0 dbody))
                      (nf, ty) ef2 K v' v hc hv' hKall
                      (.inr ⟨dnf, .default, bs0, dbody, dbt, rfl, rfl, hdb, hteq2⟩)
                    exact ⟨by rw [show ty = resTy' from hty2]; exact hvt, hr⟩
            | dataAlt cn =>
                try dsimp only at hc
                obtain ⟨hty2, hvt, hr⟩ := cchainJ_sound hΔ ih hΓ' hJ hsz hvty hks
                  hlall hall (Alt.mk (AltCon.dataAlt cn) bs0 dbody :: rest) none none
                  (nf, ty) efuel K v' v hc hv' hKall (.inl ⟨rfl, rfl⟩)
                exact ⟨by rw [show ty = resTy' from hty2]; exact hvt, hr⟩
            | litAlt i =>
                try dsimp only at hc
                obtain ⟨hty2, hvt, hr⟩ := cchainJ_sound hΔ ih hΓ' hJ hsz hvty hks
                  hlall hall (Alt.mk (AltCon.litAlt i) bs0 dbody :: rest) none none
                  (nf, ty) efuel K v' v hc hv' hKall (.inl ⟨rfl, rfl⟩)
                exact ⟨by rw [show ty = resTy' from hty2]; exact hvt, hr⟩
      | litStr sl =>
          dsimp only at hc
          exact error_ne_ok hc
      | litList tyl es =>
          dsimp only at hc
          exact error_ne_ok hc
      | app f' a' =>
          dsimp only at hc
          exact error_ne_ok hc

/-- Soundness at the top level (`cexpFull`): the `cexp_sound`-shaped
statement on the full compiler — the evaluator's join environment is
arbitrary (the compile-time one is empty), exactly as in Phase 4a. -/
theorem cexpFull_sound {Δ : DEnv} {dmap : HashMap Int Defn} {σ : String → BV}
    {X : Rwv.Hyle.Sem.XEnv} {F : Rwv.Hyle.Sem.FEnv}
    (hΔ : denvOk Δ = true) (hFor : ForeignC Δ X F)
    (fuel : Nat) (Γ : HashMap Int (NF × Ty)) (e : Exp)
    (nf : NF) (ty : Ty) (efuel : Nat) (env : Eval.Env) (jenv : Eval.JEnv) (v : Val)
    (hc : cexpFull Δ dmap fuel Γ e = .ok (nf, ty))
    (hev : Eval.evalCore ⟨Δ, dmap⟩ efuel env jenv e E = .ok v)
    (hΓ : EnvC (E := E) Δ σ Γ env) :
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ E) :=
  cexpJ_sound hΔ hFor fuel Γ [] e [] nf ty efuel 1 env jenv v [] v hc hev
    (applyMany_one ⟨Δ, dmap⟩ 0 v) hΓ jenvC_nil rfl
    (fun j hj _ => absurd hj (by simp))

/-! ## The variable-width discipline of compiled forms

`cexpJ`'s output draws its variables exclusively from the recorded
environments (Γ, the join closures, and the pending arguments), so
any width predicate holding there holds of the output — the `VarsWF`
invariant the bridge's width-aware normalizer (`cfoldW3`) needs. -/

/-- Every normal form recorded in Γ satisfies `VarsWF P`. -/
def GammaWF (P : String → Nat → Prop) (Γ : HashMap Int (NF × Ty)) : Prop :=
  ∀ u nt, Γ.get? u = some nt → NF.VarsWF P nt.1

/-- The discipline through a compile-time join closure. -/
inductive CJWF (P : String → Nat → Prop) : CJoin → Prop where
  | mk {ps : List Id} {Γc : HashMap Int (NF × Ty)} {cjs : CJEnv} {body : Exp} :
      GammaWF P Γc →
      (∀ l cj, cjs.lookup l = some cj → CJWF P cj) →
      CJWF P (.mk ps Γc cjs body)

def JGammaWF (P : String → Nat → Prop) (jΓ : CJEnv) : Prop :=
  ∀ l cj, jΓ.lookup l = some cj → CJWF P cj

private theorem gammaWF_empty {P : String → Nat → Prop} :
    GammaWF P (∅ : HashMap Int (NF × Ty)) := by
  intro u nt h
  rw [HashMap.get?_eq_getElem?] at h
  simp at h

private theorem gammaWF_insert {P : String → Nat → Prop} {Γ : HashMap Int (NF × Ty)}
    (h : GammaWF P Γ) {u : Int} {nt : NF × Ty} (hnt : NF.VarsWF P nt.1) :
    GammaWF P (Γ.insert u nt) := by
  intro u' nt' h'
  rw [get?_insert] at h'
  by_cases he : u' = u
  · rw [if_pos he] at h'
    injection h' with h'
    subst h'
    exact hnt
  · rw [if_neg he] at h'
    exact h u' nt' h'

private theorem gammaWF_bindFields {P : String → Nat → Prop} :
    ∀ (xs : List Id) (nts : List (NF × Ty)) {Γ : HashMap Int (NF × Ty)},
      GammaWF P Γ → (∀ nt ∈ nts, NF.VarsWF P nt.1) →
      GammaWF P (bindFieldsΓ xs nts Γ) := by
  intro xs
  induction xs with
  | nil =>
      intro nts Γ hΓ _
      simpa [bindFieldsΓ] using hΓ
  | cons x xs ih =>
      intro nts Γ hΓ hnts
      match nts with
      | [] => simpa [bindFieldsΓ] using hΓ
      | nt :: nts' =>
          have step : bindFieldsΓ (x :: xs) (nt :: nts') Γ
              = (bindFieldsΓ xs nts' Γ).insert x.uniq nt := by
            simp only [bindFieldsΓ, List.zip_cons_cons, List.foldr_cons]
          rw [step]
          exact gammaWF_insert
            (ih nts' hΓ (fun a ha => hnts a (List.mem_cons_of_mem _ ha)))
            (hnts nt List.mem_cons_self)

private theorem jgammaWF_nil {P : String → Nat → Prop} : JGammaWF P [] := by
  intro l cj h
  simp [List.lookup] at h

private theorem jgammaWF_cons {P : String → Nat → Prop} {jΓ : CJEnv}
    (h : JGammaWF P jΓ) {l : Int} {cj : CJoin} (hcj : CJWF P cj) :
    JGammaWF P ((l, cj) :: jΓ) := by
  intro l' cj' h'
  rw [lookup_cons] at h'
  by_cases he : l' = l
  · rw [if_pos he] at h'
    injection h' with h'
    subst h'
    exact hcj
  · rw [if_neg he] at h'
    exact h l' cj' h'

private theorem catList_varsWF {P : String → Nat → Prop} :
    ∀ {xs : List NF}, (∀ x ∈ xs, NF.VarsWF P x) → NF.VarsWF P (catList xs) := by
  intro xs
  match xs with
  | [] => intro _; trivial
  | [x] => intro h; exact h x List.mem_cons_self
  | x :: y :: rest =>
      intro h
      exact ⟨h x List.mem_cons_self,
        catList_varsWF (fun a ha => h a (List.mem_cons_of_mem _ ha))⟩

private theorem catNF_varsWF {P : String → Nat → Prop} {xs : List (NF × Nat)}
    (h : ∀ p ∈ xs, NF.VarsWF P p.1) : NF.VarsWF P (catNF xs) := by
  rw [catNF]
  refine catList_varsWF ?_
  intro x hx
  obtain ⟨p, hp, hpx⟩ := List.mem_map.mp hx
  rw [← hpx]
  exact h p (List.mem_filter.mp hp).1

private theorem sliceNF_varsWF {P : String → Nat → Prop} {off w : Nat} {e : NF}
    (h : NF.VarsWF P e) : NF.VarsWF P (sliceNF off w e) := by
  rw [sliceNF]
  split
  · trivial
  · exact h

private theorem resizeNF_varsWF {P : String → Nat → Prop} {m wa : Nat} {a : NF}
    (h : NF.VarsWF P a) : NF.VarsWF P (resizeNF m wa a) := by
  rw [resizeNF]
  split
  · exact h
  split
  · exact h
  · exact h

/-- The 4a row table preserves the discipline. -/
private theorem cprim_varsWF {P : String → Nat → Prop} {pty : Ty} {b : Builtin}
    {pas : List (NF × Ty)} {nf : NF} {ty : Ty}
    (hc : cprim pty b pas = .ok (nf, ty)) (h : ∀ p ∈ pas, NF.VarsWF P p.1) :
    NF.VarsWF P nf := by
  have harith : ∀ {op res ta a b'}, arithRow op res ta a b' = .ok (nf, ty) →
      NF.VarsWF P a → NF.VarsWF P b' → NF.VarsWF P nf := by
    intro op res ta a b' hr ha hb
    rw [arithRow] at hr
    obtain ⟨m, hm, hr⟩ := except_bind_eq_ok hr
    obtain ⟨wa, hwa, hr⟩ := except_bind_eq_ok hr
    split at hr
    · rw [except_pure_def] at hr
      injection hr with hr
      injection hr with h1 _
      subst h1
      exact ⟨ha, hb⟩
    · exact error_ne_ok hr
  have hcmp : ∀ {op res a b'}, cmpRow op res a b' = .ok (nf, ty) →
      NF.VarsWF P a → NF.VarsWF P b' → NF.VarsWF P nf := by
    intro op res a b' hr ha hb
    rw [cmpRow] at hr
    split at hr
    · rw [except_pure_def] at hr
      injection hr with hr
      injection hr with h1 _
      subst h1
      exact ⟨ha, hb⟩
    · exact error_ne_ok hr
  have hred : ∀ {op neg res a}, redRow op neg res a = .ok (nf, ty) →
      NF.VarsWF P a → NF.VarsWF P nf := by
    intro op neg res a hr ha
    rw [redRow] at hr
    split at hr
    · rw [except_pure_def] at hr
      injection hr with hr
      cases neg with
      | true =>
          have h2 := congrArg Prod.fst hr
          dsimp only at h2
          rw [← h2]
          exact ha
      | false =>
          have h2 := congrArg Prod.fst hr
          dsimp only at h2
          rw [← h2]
          exact ha
    · exact error_ne_ok hr
  cases b <;> rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b', tb⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
    dsimp only [cprim] at hc <;>
    first
    | exact error_ne_ok hc
    | (exact harith hc (h _ List.mem_cons_self)
        (h _ (List.mem_cons_of_mem _ List.mem_cons_self)))
    | (exact hcmp hc (h _ List.mem_cons_self)
        (h _ (List.mem_cons_of_mem _ List.mem_cons_self)))
    | (exact hred hc (h _ List.mem_cons_self))
    | skip
  case bits =>
      obtain ⟨k, hk, hc⟩ := except_bind_eq_ok hc
      split at hc
      · rw [except_pure_def] at hc
        injection hc with hc
        injection hc with h1 _
        subst h1
        exact h _ List.mem_cons_self
      · exact error_ne_ok hc
  case resize =>
      obtain ⟨m, hm, hc⟩ := except_bind_eq_ok hc
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      have ha := h _ List.mem_cons_self
      split at hc
      · rw [except_pure_def] at hc
        injection hc with hc
        injection hc with h1 _
        subst h1
        exact ha
      split at hc
      · rw [except_pure_def] at hc
        injection hc with hc
        injection hc with h1 _
        subst h1
        exact ha
      · rw [except_pure_def] at hc
        injection hc with hc
        injection hc with h1 _
        subst h1
        exact ha
  case xnor =>
      obtain ⟨m, hm, hc⟩ := except_bind_eq_ok hc
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      split at hc
      · rw [except_pure_def] at hc
        injection hc with hc
        injection hc with h1 _
        subst h1
        exact ⟨h _ List.mem_cons_self, h _ (List.mem_cons_of_mem _ List.mem_cons_self)⟩
      · exact error_ne_ok hc
  case not =>
      obtain ⟨m, hm, hc⟩ := except_bind_eq_ok hc
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      split at hc
      · rw [except_pure_def] at hc
        injection hc with hc
        injection hc with h1 _
        subst h1
        exact h _ List.mem_cons_self
      · exact error_ne_ok hc
  case msBit =>
      split at hc
      rotate_left
      · exact error_ne_ok hc
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      split at hc
      · rw [except_pure_def] at hc
        injection hc with hc
        injection hc with h1 _
        subst h1
        exact h _ List.mem_cons_self
      · exact error_ne_ok hc

/-- The extended row table preserves the discipline. -/
private theorem cprimF_varsWF {P : String → Nat → Prop} {Δ : DEnv} {szf : Nat}
    {pty : Ty} {b : Builtin} {pas : List (NF × Ty)} {nf : NF} {ty : Ty}
    (hc : cprimF Δ szf pty b pas = .ok (nf, ty)) (h : ∀ p ∈ pas, NF.VarsWF P p.1) :
    NF.VarsWF P nf := by
  cases b <;>
    first
    | (dsimp only [cprimF] at hc
       exact cprim_varsWF hc h)
    | skip
  all_goals dsimp only [cprimF] at hc
  case finite =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowFinite] at hc <;> try exact error_ne_ok hc
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      cases a <;> try exact error_ne_ok hc
      dsimp only at hc
      split at hc
      · injection hc with hc
        injection hc with h1 _
        subst h1
        trivial
      · exact error_ne_ok hc
  case finiteMinBound =>
      rcases pas with _ | ⟨p1, r1⟩ <;>
        rw [rowFinBound] at hc <;> try exact error_ne_ok hc
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      split at hc
      · injection hc with hc
        injection hc with h1 _
        subst h1
        trivial
      · exact error_ne_ok hc
  case finiteMaxBound =>
      rcases pas with _ | ⟨p1, r1⟩ <;>
        rw [rowFinBound] at hc <;> try exact error_ne_ok hc
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      split at hc
      · injection hc with hc
        injection hc with h1 _
        subst h1
        trivial
      · exact error_ne_ok hc
  case toFinite =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowToFinite] at hc <;> try exact error_ne_ok hc
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      split at hc
      · injection hc with hc
        injection hc with h1 _
        subst h1
        exact resizeNF_varsWF (h _ List.mem_cons_self)
      · exact error_ne_ok hc
  case toFiniteMod =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowToFiniteMod] at hc <;> try exact error_ne_ok hc
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      obtain ⟨wa, hwa, hc⟩ := except_bind_eq_ok hc
      split at hc
      · injection hc with hc
        injection hc with h1 _
        subst h1
        exact resizeNF_varsWF (h _ List.mem_cons_self)
      · split at hc
        · exact error_ne_ok hc
        · injection hc with hc
          injection hc with h1 _
          subst h1
          exact resizeNF_varsWF ⟨resizeNF_varsWF (h _ List.mem_cons_self), trivial⟩
  case fromFinite =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowFromFinite] at hc <;> try exact error_ne_ok hc
      obtain ⟨n, hn, hc⟩ := except_bind_eq_ok hc
      obtain ⟨m, hm, hc⟩ := except_bind_eq_ok hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      injection hc with hc
      injection hc with h1 _
      subst h1
      exact resizeNF_varsWF (h _ List.mem_cons_self)
  case vecReplicate =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowVecReplicate] at hc <;> try exact error_ne_ok hc
      obtain ⟨nte, hnte, hc⟩ := except_bind_eq_ok hc
      obtain ⟨n, te⟩ := nte
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      obtain ⟨sz, hsz, hc⟩ := except_bind_eq_ok hc
      split at hc
      · injection hc with hc
        injection hc with h1 _
        subst h1
        trivial
      · injection hc with hc
        injection hc with h1 _
        subst h1
        exact h _ List.mem_cons_self
  case vecConcat =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b', tb⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        rw [rowVecConcat] at hc <;> try exact error_ne_ok hc
      obtain ⟨nte0, h0, hc⟩ := except_bind_eq_ok hc
      obtain ⟨nr, ter⟩ := nte0
      dsimp only at hc
      obtain ⟨nte1, h1, hc⟩ := except_bind_eq_ok hc
      obtain ⟨n1, te1⟩ := nte1
      dsimp only at hc
      obtain ⟨nte2, h2, hc⟩ := except_bind_eq_ok hc
      obtain ⟨n2, te2⟩ := nte2
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      obtain ⟨sa, hsa, hc⟩ := except_bind_eq_ok hc
      obtain ⟨sb, hsb, hc⟩ := except_bind_eq_ok hc
      injection hc with hc
      injection hc with h1 _
      subst h1
      refine catNF_varsWF ?_
      intro p hp
      rcases List.mem_cons.mp hp with rfl | hp
      · exact h _ List.mem_cons_self
      rcases List.mem_cons.mp hp with rfl | hp
      · exact h _ (List.mem_cons_of_mem _ List.mem_cons_self)
      exact absurd hp (by simp)
  case vecReverse =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowVecReverse] at hc <;> try exact error_ne_ok hc
      obtain ⟨nte0, h0, hc⟩ := except_bind_eq_ok hc
      obtain ⟨nr, ter⟩ := nte0
      dsimp only at hc
      obtain ⟨nte1, h1, hc⟩ := except_bind_eq_ok hc
      obtain ⟨n1, te1⟩ := nte1
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      obtain ⟨se, hse, hc⟩ := except_bind_eq_ok hc
      injection hc with hc
      injection hc with h1 _
      subst h1
      refine catNF_varsWF ?_
      intro p hp
      obtain ⟨j, hj, hpj⟩ := List.mem_map.mp hp
      rw [← hpj]
      exact sliceNF_varsWF (h _ List.mem_cons_self)
  case vecSlice =>
      rcases pas with _ | ⟨⟨a0, ta0⟩, _ | ⟨⟨a, ta⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        rw [rowVecSlice] at hc <;> try exact error_ne_ok hc
      obtain ⟨pt, hpt, hc⟩ := except_bind_eq_ok hc
      obtain ⟨i, hi, hc⟩ := except_bind_eq_ok hc
      obtain ⟨nte0, h0, hc⟩ := except_bind_eq_ok hc
      obtain ⟨m, ter⟩ := nte0
      dsimp only at hc
      obtain ⟨nte1, h1, hc⟩ := except_bind_eq_ok hc
      obtain ⟨len, tea⟩ := nte1
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      obtain ⟨se, hse, hc⟩ := except_bind_eq_ok hc
      obtain ⟨szA, hszA, hc⟩ := except_bind_eq_ok hc
      split at hc
      · injection hc with hc
        injection hc with h1 _
        subst h1
        exact sliceNF_varsWF (h _ (List.mem_cons_of_mem _ List.mem_cons_self))
      · exact error_ne_ok hc
  case vecRSlice =>
      rcases pas with _ | ⟨⟨a0, ta0⟩, _ | ⟨⟨a, ta⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        rw [rowVecRSlice] at hc <;> try exact error_ne_ok hc
      obtain ⟨pt, hpt, hc⟩ := except_bind_eq_ok hc
      obtain ⟨i, hi, hc⟩ := except_bind_eq_ok hc
      obtain ⟨nte0, h0, hc⟩ := except_bind_eq_ok hc
      obtain ⟨m, ter⟩ := nte0
      dsimp only at hc
      obtain ⟨nte1, h1, hc⟩ := except_bind_eq_ok hc
      obtain ⟨len, tea⟩ := nte1
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      obtain ⟨se, hse, hc⟩ := except_bind_eq_ok hc
      obtain ⟨szA, hszA, hc⟩ := except_bind_eq_ok hc
      split at hc
      · injection hc with hc
        injection hc with h1 _
        subst h1
        exact sliceNF_varsWF (h _ (List.mem_cons_of_mem _ List.mem_cons_self))
      · exact error_ne_ok hc
  case vecIndexProxy =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b', tb⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        rw [rowVecIndexProxy] at hc <;> try exact error_ne_ok hc
      obtain ⟨pt, hpt, hc⟩ := except_bind_eq_ok hc
      obtain ⟨k, hk, hc⟩ := except_bind_eq_ok hc
      obtain ⟨nte1, h1, hc⟩ := except_bind_eq_ok hc
      obtain ⟨len, tea⟩ := nte1
      dsimp only at hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      obtain ⟨se, hse, hc⟩ := except_bind_eq_ok hc
      obtain ⟨szA, hszA, hc⟩ := except_bind_eq_ok hc
      split at hc
      · injection hc with hc
        injection hc with h1 _
        subst h1
        exact sliceNF_varsWF (h _ List.mem_cons_self)
      · exact error_ne_ok hc
  case vecIndex =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨iN, ti⟩, _ | ⟨p3, r3⟩⟩⟩ <;>
        rw [rowVecIndex] at hc <;> try exact error_ne_ok hc
      obtain ⟨nte1, h1, hc⟩ := except_bind_eq_ok hc
      obtain ⟨len, te⟩ := nte1
      dsimp only at hc
      obtain ⟨nb, hnb, hc⟩ := except_bind_eq_ok hc
      split at hc
      rotate_left
      · exact error_ne_ok hc
      obtain ⟨se, hse, hc⟩ := except_bind_eq_ok hc
      obtain ⟨szA, hszA, hc⟩ := except_bind_eq_ok hc
      split at hc
      · injection hc with hc
        injection hc with h1 _
        subst h1
        refine resizeNF_varsWF ?_
        refine ⟨h _ List.mem_cons_self, ?_, trivial⟩
        exact ⟨⟨trivial, resizeNF_varsWF
          (h _ (List.mem_cons_of_mem _ List.mem_cons_self))⟩, trivial⟩
      · exact error_ne_ok hc
  case natVal =>
      rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨p2, r2⟩⟩ <;>
        rw [rowNatVal] at hc <;> try exact error_ne_ok hc
      obtain ⟨pt, hpt, hc⟩ := except_bind_eq_ok hc
      obtain ⟨k, hk, hc⟩ := except_bind_eq_ok hc
      split at hc
      · injection hc with hc
        injection hc with h1 _
        subst h1
        trivial
      · exact error_ne_ok hc

private abbrev WFAtJ (Δ : DEnv) (dmap : HashMap Int Defn) (P : String → Nat → Prop)
    (fuel : Nat) : Prop :=
  ∀ (Γ : HashMap Int (NF × Ty)) (jΓ : CJEnv) (e : Exp) (pend : List (NF × Ty))
    (nf : NF) (ty : Ty),
    GammaWF P Γ → JGammaWF P jΓ → (∀ p ∈ pend, NF.VarsWF P p.1) →
    cexpJ Δ dmap fuel Γ jΓ e pend = .ok (nf, ty) → NF.VarsWF P nf

private theorem cchainJ_varsWF {Δ : DEnv} {dmap : HashMap Int Defn}
    {P : String → Nat → Prop} {fuel : Nat} (IH : WFAtJ Δ dmap P fuel)
    {Γ' : HashMap Int (NF × Ty)} {jΓ : CJEnv} {dty : Ty} {szT : Nat} {dn : NF}
    {resTy : Ty} {pend : List (NF × Ty)}
    (hΓ' : GammaWF P Γ') (hJ : JGammaWF P jΓ) (hdn : NF.VarsWF P dn)
    (hpend : ∀ p ∈ pend, NF.VarsWF P p.1) :
    ∀ (rest : List Alt) (macc : Option NF) (out : NF × Ty),
      (∀ acc, macc = some acc → NF.VarsWF P acc) →
      cchainJ Δ dmap fuel Γ' jΓ dty szT dn resTy pend rest macc = .ok out →
      NF.VarsWF P out.1 := by
  have hstep : ∀ (alt : Alt) (macc : Option NF) (bnf : NF),
      (∀ acc, macc = some acc → NF.VarsWF P acc) →
      cAltJ Δ dmap fuel Γ' jΓ dty szT dn resTy pend alt macc = .ok bnf →
      NF.VarsWF P bnf := by
    intro alt macc bnf hacc hca
    obtain ⟨con, xs, body⟩ := alt
    cases con with
    | default => rw [cAltJ] at hca; exact error_ne_ok hca
    | dataAlt cn =>
        rw [cAltJ] at hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        obtain ⟨tg, htg, hca⟩ := except_bind_eq_ok hca
        obtain ⟨tag, w⟩ := tg
        cases hcs : Δ.ctorSig.get? cn with
        | none => rw [hcs] at hca; dsimp only at hca; exact error_ne_ok hca
        | some sig =>
        rw [hcs] at hca
        dsimp only at hca
        obtain ⟨sub, hsub, hca⟩ := except_bind_eq_ok hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        obtain ⟨szXs, hszXs, hca⟩ := except_bind_eq_ok hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        obtain ⟨bt, hbt, hca⟩ := except_bind_eq_ok hca
        obtain ⟨bnf', bty⟩ := bt
        dsimp only at hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        have hbody : NF.VarsWF P bnf' := by
          refine IH _ jΓ body pend bnf' bty ?_ hJ hpend hbt
          refine gammaWF_bindFields _ _ hΓ' ?_
          intro nt hnt
          have h1 := (List.of_mem_zip hnt).1
          obtain ⟨pr, hpr, hprnt⟩ := List.mem_map.mp h1
          rw [← hprnt]
          exact sliceNF_varsWF hdn
        split at hca
        · injection hca with hca
          subst hca
          exact ⟨⟨sliceNF_varsWF hdn, trivial⟩, hbody, hacc _ rfl⟩
        all_goals
          injection hca with hca
          subst hca
          exact hbody
    | litAlt i =>
        rw [cAltJ] at hca
        obtain ⟨bt, hbt, hca⟩ := except_bind_eq_ok hca
        obtain ⟨bnf', bty⟩ := bt
        dsimp only at hca
        split at hca
        rotate_left
        · exact error_ne_ok hca
        have hbody : NF.VarsWF P bnf' :=
          IH _ jΓ body pend bnf' bty hΓ' hJ hpend hbt
        split at hca
        · injection hca with hca
          subst hca
          exact ⟨⟨hdn, trivial⟩, hbody, hacc _ rfl⟩
        all_goals
          injection hca with hca
          subst hca
          exact hbody
  intro rest
  induction rest with
  | nil =>
      intro macc out hacc hcc
      cases macc with
      | some els =>
          rw [cchainJ] at hcc
          injection hcc with hcc
          have h1 := congrArg Prod.fst hcc
          dsimp only at h1
          rw [← h1]
          exact hacc els rfl
      | none =>
          rw [cchainJ] at hcc
          exact error_ne_ok hcc
  | cons alt restT ihR =>
      intro macc out hacc hcc
      rcases restT with _ | ⟨r2, rt⟩
      · rcases macc with _ | els
        · rw [cchainJ] at hcc
          obtain ⟨bnf, hbnf, hcc⟩ := except_bind_eq_ok hcc
          injection hcc with hcc
          have h1 := congrArg Prod.fst hcc
          dsimp only at h1
          rw [← h1]
          exact hstep alt none bnf (fun acc h => absurd h (by simp)) hbnf
        · rw [cchainJ] at hcc
          obtain ⟨accp, haccp, hcc⟩ := except_bind_eq_ok hcc
          obtain ⟨accnf, accty⟩ := accp
          dsimp only at hcc
          obtain ⟨bnf, hbnf, hcc⟩ := except_bind_eq_ok hcc
          injection hcc with hcc
          have h1 := congrArg Prod.fst hcc
          dsimp only at h1
          rw [← h1]
          have haccWF : NF.VarsWF P accnf := by
            have := ihR (some els) (accnf, accty) hacc haccp
            simpa using this
          exact hstep alt (some accnf) bnf
            (fun acc h => by injection h with h; subst h; exact haccWF) hbnf
          all_goals (intros; simp_all)
      · rw [cchainJ] at hcc
        obtain ⟨accp, haccp, hcc⟩ := except_bind_eq_ok hcc
        obtain ⟨accnf, accty⟩ := accp
        dsimp only at hcc
        obtain ⟨bnf, hbnf, hcc⟩ := except_bind_eq_ok hcc
        injection hcc with hcc
        have h1 := congrArg Prod.fst hcc
        dsimp only at h1
        rw [← h1]
        have haccWF : NF.VarsWF P accnf := by
          have := ihR macc (accnf, accty) hacc haccp
          simpa using this
        exact hstep alt (some accnf) bnf
          (fun acc h => by injection h with h; subst h; exact haccWF) hbnf
        all_goals (intro h1 h2; exact absurd h2 (by simp))

/-- Packing preserves the width discipline (Bridge's identical lemma
is private there). -/
private theorem xpack_varsWF' {P : String → Nat → Prop} {ns : List NF}
    (h : ∀ n ∈ ns, n.VarsWF P) : (NF.xpack ns).VarsWF P := by
  suffices hgen : ∀ (acc : NF) (ns : List NF), acc.VarsWF P → (∀ n ∈ ns, n.VarsWF P) →
      (ns.foldl NF.cat acc).VarsWF P from hgen (.lit Rwv.Hyle.BV.nil) ns trivial h
  intro acc ns
  induction ns generalizing acc with
  | nil => intro hacc _; exact hacc
  | cons n ns ih =>
      intro hacc hns
      rw [List.foldl_cons]
      exact ih (.cat acc n) ⟨hacc, hns n List.mem_cons_self⟩
        (fun m hm => hns m (List.mem_cons_of_mem _ hm))

set_option maxHeartbeats 8000000 in
/-- The compiled output's variables all satisfy the environments'
width discipline (the `VarsWF` invariant needed by the width-aware
normalizer). -/
theorem cexpJ_varsWF {Δ : DEnv} {dmap : HashMap Int Defn} {P : String → Nat → Prop} :
    ∀ (fuel : Nat), WFAtJ Δ dmap P fuel := by
  intro fuel
  induction fuel with
  | zero =>
      intro Γ jΓ e pend nf ty _ _ _ hc
      rw [cexpJ] at hc
      exact error_ne_ok hc
  | succ fuel ih =>
      intro Γ jΓ e pend nf ty hΓ hJ hpend hc
      rw [cexpJ] at hc
      rcases hfl : Eval.flattenApp e with ⟨hd, args⟩
      rw [hfl] at hc
      clear hfl
      cases hd with
      | var x =>
          dsimp only at hc
          cases hΓx : Γ.get? x.uniq with
          | some nt =>
              rw [hΓx] at hc
              cases args with
              | cons a as => cases pend <;> exact error_ne_ok hc
              | nil =>
                  cases pend with
                  | cons p ps => exact error_ne_ok hc
                  | nil =>
                      injection hc with hc
                      have h1 := congrArg Prod.fst hc
                      dsimp only at h1
                      rw [← h1]
                      exact hΓ _ _ hΓx
          | none =>
              rw [hΓx] at hc
              cases hdm : dmap.get? x.uniq with
              | none => rw [hdm] at hc; exact error_ne_ok hc
              | some d =>
                  rw [hdm] at hc
                  dsimp only at hc
                  rw [bind_ok_iff] at hc
                  obtain ⟨pas, hpas, hc⟩ := hc
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
                  have hpasWF : ∀ p ∈ pas ++ pend, NF.VarsWF P p.1 := by
                    intro p hp
                    rcases List.mem_append.mp hp with hp | hp
                    · obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                      obtain ⟨hj2, hcj⟩ := hpt j (by omega)
                      rw [← hpj]
                      exact ih Γ jΓ args[j] [] (pas[j].1) (pas[j].2) hΓ hJ
                        (fun q hq => absurd hq (by simp)) hcj
                    · exact hpend p hp
                  refine ih _ [] d.body _ nf ty ?_ jgammaWF_nil ?_ hc
                  · refine gammaWF_bindFields _ _ gammaWF_empty ?_
                    intro nt hnt
                    exact hpasWF nt (List.mem_of_mem_take hnt)
                  · intro p hp
                    exact hpasWF p (List.mem_of_mem_drop hp)
      | con cty c =>
          cases pend with
          | cons p ps =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
          dsimp only at hc
          rw [bind_ok_iff] at hc
          obtain ⟨pas, hpas, hc⟩ := hc
          obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
          have hpasWF : ∀ p ∈ pas, NF.VarsWF P p.1 := by
            intro p hp
            obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
            obtain ⟨hj2, hcj⟩ := hpt j (by omega)
            rw [← hpj]
            exact ih Γ jΓ args[j] [] (pas[j].1) (pas[j].2) hΓ hJ
              (fun q hq => absurd hq (by simp)) hcj
          split at hc
          rotate_left
          · exact error_ne_ok hc
          cases hcs : Δ.ctorSig.get? c with
          | none => rw [hcs] at hc; exact error_ne_ok hc
          | some sig =>
              rw [hcs] at hc
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨sub, hsub, hc⟩ := hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rw [bind_ok_iff] at hc
              obtain ⟨whole, hwhole, hc⟩ := hc
              rw [bind_ok_iff] at hc
              obtain ⟨tg, htg, hc⟩ := hc
              obtain ⟨tag, w⟩ := tg
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨ws, hws, hc⟩ := hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              injection hc with hc
              have h1 := congrArg Prod.fst hc
              dsimp only at h1
              rw [← h1]
              refine catNF_varsWF ?_
              intro p hp
              rcases List.mem_cons.mp hp with rfl | hp
              · trivial
              rcases List.mem_cons.mp hp with rfl | hp
              · trivial
              have h2 := (List.of_mem_zip hp).1
              obtain ⟨q, hq, hqp⟩ := List.mem_map.mp h2
              rw [← hqp]
              exact hpasWF q hq
      | prim pty b =>
          cases pend with
          | cons p ps =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
          dsimp only at hc
          split at hc
          · -- error row: a literal
            rw [bind_ok_iff] at hc
            obtain ⟨sz, hsz, hc⟩ := hc
            rw [bind_ok_iff] at hc
            obtain ⟨zv, hzv, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            injection hc with hc
            have h1 := congrArg Prod.fst hc
            dsimp only at h1
            rw [← h1]
            trivial
          · -- vecFromList
            cases args with
            | nil => exact error_ne_ok hc
            | cons a1 rest1 =>
            cases rest1 with
            | cons a2 rest2 => cases a1 <;> exact error_ne_ok hc
            | nil =>
            cases a1 <;> try exact error_ne_ok hc
            rename_i lty els
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨pas, hpas, hc⟩ := hc
            obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
            have hpasWF : ∀ p ∈ pas, NF.VarsWF P p.1 := by
              intro p hp
              obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
              obtain ⟨hj2, hcj⟩ := hpt j (by omega)
              rw [← hpj]
              exact ih Γ jΓ els[j] [] (pas[j].1) (pas[j].2) hΓ hJ
                (fun q hq => absurd hq (by simp)) hcj
            split at hc
            rotate_left
            · exact error_ne_ok hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rw [bind_ok_iff] at hc
            obtain ⟨se, hse, hc⟩ := hc
            injection hc with hc
            have h1 := congrArg Prod.fst hc
            dsimp only at h1
            rw [← h1]
            refine catNF_varsWF ?_
            intro p hp
            obtain ⟨q, hq, hqp⟩ := List.mem_map.mp hp
            rw [← hqp]
            obtain ⟨r, hr, hrq⟩ := List.mem_map.mp hq
            dsimp only
            rw [← hrq]
            exact hpasWF r hr
          · -- bitIndex
            cases args with
            | nil => exact error_ne_ok hc
            | cons argE rest1 =>
            cases rest1 with
            | nil => exact error_ne_ok hc
            | cons iE rest2 =>
            cases rest2 with
            | cons a3 rest3 => exact error_ne_ok hc
            | nil =>
            dsimp only at hc
            cases hfin : finLitE iE with
            | none => rw [hfin] at hc; exact error_ne_ok hc
            | some iidx =>
            rw [hfin] at hc
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨ant, harg, hc⟩ := hc
            obtain ⟨a, ta⟩ := ant
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨wa, hwa, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            injection hc with hc
            have h1 := congrArg Prod.fst hc
            dsimp only at h1
            rw [← h1]
            exact sliceNF_varsWF (ih Γ jΓ argE [] a ta hΓ hJ
              (fun q hq => absurd hq (by simp)) harg)
          · -- bitSlice
            cases args with
            | nil => exact error_ne_ok hc
            | cons argE rest1 =>
            cases rest1 with
            | nil => exact error_ne_ok hc
            | cons jE rest2 =>
            cases rest2 with
            | nil => exact error_ne_ok hc
            | cons iE rest3 =>
            cases rest3 with
            | cons a4 rest4 => exact error_ne_ok hc
            | nil =>
            dsimp only at hc
            cases hfinj : finLitE jE with
            | none => rw [hfinj] at hc; exact error_ne_ok hc
            | some jidx =>
            rw [hfinj] at hc
            cases hfini : finLitE iE with
            | none => rw [hfini] at hc; dsimp only at hc; exact error_ne_ok hc
            | some iidx =>
            rw [hfini] at hc
            dsimp only at hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rw [bind_ok_iff] at hc
            obtain ⟨ant, harg, hc⟩ := hc
            obtain ⟨a, ta⟩ := ant
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨wa, hwa, hc⟩ := hc
            rw [bind_ok_iff] at hc
            obtain ⟨mr, hmr, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            injection hc with hc
            have h1 := congrArg Prod.fst hc
            dsimp only at h1
            rw [← h1]
            exact sliceNF_varsWF (ih Γ jΓ argE [] a ta hΓ hJ
              (fun q hq => absurd hq (by simp)) harg)
          · -- vecMap
            cases args with
            | nil => exact error_ne_ok hc
            | cons fE rest1 =>
            cases rest1 with
            | nil => exact error_ne_ok hc
            | cons argE rest2 =>
            cases rest2 with
            | cons a3 rest3 => exact error_ne_ok hc
            | nil =>
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨nto, hres, hc⟩ := hc
            obtain ⟨nElems, teO⟩ := nto
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨seO, hseO, hc⟩ := hc
            rw [bind_ok_iff] at hc
            obtain ⟨ant, harg, hc⟩ := hc
            obtain ⟨a, ta⟩ := ant
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨nti, hlenI, hc⟩ := hc
            obtain ⟨len, teI⟩ := nti
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨seI, hseI, hc⟩ := hc
            rw [bind_ok_iff] at hc
            obtain ⟨szV, hszV, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rw [bind_ok_iff] at hc
            obtain ⟨ps, hps, hc⟩ := hc
            injection hc with hc
            have h1 := congrArg Prod.fst hc
            dsimp only at h1
            rw [← h1]
            have haWF : NF.VarsWF P a :=
              ih Γ jΓ argE [] a ta hΓ hJ (fun q hq => absurd hq (by simp)) harg
            obtain ⟨hpslen, hpspt⟩ := mapM_ok_idx hps
            refine catNF_varsWF ?_
            intro p hp
            obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
            obtain ⟨hj2, hcj⟩ := hpspt j (by omega)
            obtain ⟨etj, hcej, hifj⟩ := except_bind_eq_ok hcj
            obtain ⟨ej, tj⟩ := etj
            dsimp only at hifj
            split at hifj
            rotate_left
            · exact error_ne_ok hifj
            injection hifj with hifj
            rw [← hpj, ← hifj]
            exact ih Γ jΓ fE _ ej tj hΓ hJ
              (by
                intro q hq
                obtain rfl := List.mem_singleton.mp hq
                exact sliceNF_varsWF haWF) hcej
          · -- vecGenerate
            cases args with
            | nil => exact error_ne_ok hc
            | cons fE rest1 =>
            cases rest1 with
            | cons a2 rest2 => exact error_ne_ok hc
            | nil =>
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨nto, hres, hc⟩ := hc
            obtain ⟨nElems, teO⟩ := nto
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨seO, hseO, hc⟩ := hc
            rw [bind_ok_iff] at hc
            obtain ⟨ft, hft, hc⟩ := hc
            cases ft with
            | con c => exact error_ne_ok hc
            | app t1 t2 => exact error_ne_ok hc
            | var xv => exact error_ne_ok hc
            | nat nn => exact error_ne_ok hc
            | arrow tFin tR =>
            dsimp only at hc
            rw [bind_ok_iff] at hc
            obtain ⟨nb, hnb, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rw [bind_ok_iff] at hc
            obtain ⟨ps, hps, hc⟩ := hc
            injection hc with hc
            have h1 := congrArg Prod.fst hc
            dsimp only at h1
            rw [← h1]
            obtain ⟨hpslen, hpspt⟩ := mapM_ok_idx hps
            refine catNF_varsWF ?_
            intro p hp
            obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
            obtain ⟨hj2, hcj⟩ := hpspt j (by omega)
            obtain ⟨etj, hcej, hifj⟩ := except_bind_eq_ok hcj
            obtain ⟨ej, tj⟩ := etj
            dsimp only at hifj
            split at hifj
            rotate_left
            · exact error_ne_ok hifj
            injection hifj with hifj
            rw [← hpj, ← hifj]
            exact ih Γ jΓ fE _ ej tj hΓ hJ
              (by
                intro q hq
                obtain rfl := List.mem_singleton.mp hq
                trivial) hcej
          · -- the Cryptol foreign row: the spliced inlining's
            -- variables are the compiled arguments'
            split at hc
            all_goals try exact error_ne_ok hc
            rename_i f' n' impl rest
            rw [bind_ok_iff] at hc
            obtain ⟨ity, _hity, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            split at hc
            all_goals try exact error_ne_ok hc
            rename_i g _hcryD
            split at hc
            all_goals try exact error_ne_ok hc
            rename_i hd _hgd
            rw [bind_ok_iff] at hc
            obtain ⟨pas, hpas, hc⟩ := hc
            obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
            have hpasWF : ∀ p ∈ pas, NF.VarsWF P p.1 := by
              intro p hp
              obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
              obtain ⟨hj2, hcj⟩ := hpt j (by omega)
              rw [← hpj]
              exact ih Γ jΓ rest[j] [] (pas[j].1) (pas[j].2) hΓ hJ
                (fun q hq => absurd hq (by simp)) hcj
            split at hc
            rotate_left
            · exact error_ne_ok hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rw [bind_ok_iff] at hc
            obtain ⟨nfS, hsym, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            injection hc with hc
            have h1 := congrArg Prod.fst hc
            dsimp only at h1
            rw [← h1]
            refine Rwv.Hyle.Bridge.symExp_varsWF Δ.hyleFuel hd.body _ nfS ?_ hsym
            intro x nx hnx
            have hmem := ofList_get?_some' hnx
            have hx2 : nx ∈ pas.map (·.1) := (List.of_mem_zip hmem).2
            obtain ⟨q, hq, hqn⟩ := List.mem_map.mp hx2
            rw [← hqn]
            exact hpasWF q hq
          · -- the extern row: model-carrying occurrences inline the
            -- model (the Cryptol case's argument); model-less ones
            -- pack the compiled arguments' variables
            split at hc
            all_goals try exact error_ne_ok hc
            rename_i _ps _clk _rst _as _rs sx _impl _inst rest
            rw [bind_ok_iff] at hc
            obtain ⟨ity, _hity, hc⟩ := hc
            split at hc
            rotate_left
            · exact error_ne_ok hc
            split at hc
            · -- model-carrying: the model inlining's variables are
              -- the compiled arguments'
              rename_i g _hhx
              split at hc
              all_goals try exact error_ne_ok hc
              rename_i hd _hgd
              rw [bind_ok_iff] at hc
              obtain ⟨pas, hpas, hc⟩ := hc
              obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
              have hpasWF : ∀ p ∈ pas, NF.VarsWF P p.1 := by
                intro p hp
                obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                obtain ⟨hj2, hcj⟩ := hpt j (by omega)
                rw [← hpj]
                exact ih Γ jΓ rest[j] [] (pas[j].1) (pas[j].2) hΓ hJ
                  (fun q hq => absurd hq (by simp)) hcj
              split at hc
              rotate_left
              · exact error_ne_ok hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rw [bind_ok_iff] at hc
              obtain ⟨nfS, hsym, hc⟩ := hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              injection hc with hc
              have h1 := congrArg Prod.fst hc
              dsimp only at h1
              rw [← h1]
              refine Rwv.Hyle.Bridge.symExp_varsWF Δ.hyleFuel hd.body _ nfS ?_ hsym
              intro x nx hnx
              have hmem := ofList_get?_some' hnx
              have hx2 : nx ∈ pas.map (·.1) := (List.of_mem_zip hmem).2
              obtain ⟨q, hq, hqn⟩ := List.mem_map.mp hx2
              rw [← hqn]
              exact hpasWF q hq
            · -- model-less: the packed arguments' variables are the
              -- compiled arguments'
              rw [bind_ok_iff] at hc
              obtain ⟨pas, hpas, hc⟩ := hc
              obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
              have hpasWF : ∀ p ∈ pas, NF.VarsWF P p.1 := by
                intro p hp
                obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                obtain ⟨hj2, hcj⟩ := hpt j (by omega)
                rw [← hpj]
                exact ih Γ jΓ rest[j] [] (pas[j].1) (pas[j].2) hΓ hJ
                  (fun q hq => absurd hq (by simp)) hcj
              rw [bind_ok_iff] at hc
              obtain ⟨w, _hw, hc⟩ := hc
              injection hc with hc
              have h1 := congrArg Prod.fst hc
              dsimp only at h1
              rw [← h1]
              show ((NF.xpack (pas.map (·.1))).VarsWF P)
              refine xpack_varsWF' ?_
              intro n hn
              obtain ⟨q, hq, hqn⟩ := List.mem_map.mp hn
              rw [← hqn]
              exact hpasWF q hq
          · -- extended rows
            rw [bind_ok_iff] at hc
            obtain ⟨pas, hpas, hc⟩ := hc
            obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
            have hpasWF : ∀ p ∈ pas, NF.VarsWF P p.1 := by
              intro p hp
              obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
              obtain ⟨hj2, hcj⟩ := hpt j (by omega)
              rw [← hpj]
              exact ih Γ jΓ args[j] [] (pas[j].1) (pas[j].2) hΓ hJ
                (fun q hq => absurd hq (by simp)) hcj
            exact cprimF_varsWF hc hpasWF
      | litInt tyL n =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil =>
          cases pend with
          | cons p ps =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
          dsimp only at hc
          obtain ⟨hrty, hshape⟩ := clitInt_inv hc
          rcases hshape with ⟨_, hnf⟩ | ⟨_, _, _, _, hnf⟩ | ⟨_, _, _, _, _, _, hnf⟩ <;>
            (subst hnf; trivial)
      | litVec vty es =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil =>
          cases pend with
          | cons p ps =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
          dsimp only at hc
          rw [bind_ok_iff] at hc
          obtain ⟨pas, hpas, hc⟩ := hc
          obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
          have hpasWF : ∀ p ∈ pas, NF.VarsWF P p.1 := by
            intro p hp
            obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
            obtain ⟨hj2, hcj⟩ := hpt j (by omega)
            rw [← hpj]
            exact ih Γ jΓ es[j] [] (pas[j].1) (pas[j].2) hΓ hJ
              (fun q hq => absurd hq (by simp)) hcj
          split at hc
          rotate_left
          · exact error_ne_ok hc
          split at hc
          rotate_left
          · exact error_ne_ok hc
          split at hc
          rotate_left
          · exact error_ne_ok hc
          split at hc
          rotate_left
          · exact error_ne_ok hc
          rw [bind_ok_iff] at hc
          obtain ⟨se, hse, hc⟩ := hc
          injection hc with hc
          have h1 := congrArg Prod.fst hc
          dsimp only at h1
          rw [← h1]
          refine catNF_varsWF ?_
          intro p hp
          obtain ⟨q, hq, hqp⟩ := List.mem_map.mp hp
          rw [← hqp]
          obtain ⟨r, hr, hrq⟩ := List.mem_map.mp hq
          dsimp only
          rw [← hrq]
          exact hpasWF r hr
      | lam x b =>
          dsimp only at hc
          rw [bind_ok_iff] at hc
          obtain ⟨pas, hpas, hc⟩ := hc
          obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
          have hpasWF : ∀ p ∈ pas ++ pend, NF.VarsWF P p.1 := by
            intro p hp
            rcases List.mem_append.mp hp with hp | hp
            · obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
              obtain ⟨hj2, hcj⟩ := hpt j (by omega)
              rw [← hpj]
              exact ih Γ jΓ args[j] [] (pas[j].1) (pas[j].2) hΓ hJ
                (fun q hq => absurd hq (by simp)) hcj
            · exact hpend p hp
          cases hpp : pas ++ pend with
          | nil => rw [hpp] at hc; exact error_ne_ok hc
          | cons nt rest =>
              rw [hpp] at hc
              refine ih (Γ.insert x.uniq nt) [] b rest nf ty ?_ jgammaWF_nil ?_ hc
              · exact gammaWF_insert hΓ (by
                  have := hpasWF nt (by rw [hpp]; exact List.mem_cons_self)
                  exact this)
              · intro p hp
                exact hpasWF p (by rw [hpp]; exact List.mem_cons_of_mem _ hp)
      | letE bnd body =>
          cases bnd with
          | nonRec x rhs =>
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨nt, hrhs, hc⟩ := hc
              obtain ⟨ntn, ntt⟩ := nt
              rw [bind_ok_iff] at hc
              obtain ⟨pas, hpas, hc⟩ := hc
              obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
              have hntWF : NF.VarsWF P ntn :=
                ih Γ jΓ rhs [] ntn ntt hΓ hJ (fun q hq => absurd hq (by simp)) hrhs
              refine ih (Γ.insert x.uniq (ntn, ntt)) jΓ body (pas ++ pend) nf ty
                (gammaWF_insert hΓ hntWF) hJ ?_ hc
              intro p hp
              rcases List.mem_append.mp hp with hp | hp
              · obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                obtain ⟨hj2, hcj⟩ := hpt j (by omega)
                rw [← hpj]
                exact ih Γ jΓ args[j] [] (pas[j].1) (pas[j].2) hΓ hJ
                  (fun q hq => absurd hq (by simp)) hcj
              · exact hpend p hp
          | recB bs =>
              dsimp only at hc
              exact error_ne_ok hc
          | join l ps jb =>
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨pas, hpas, hc⟩ := hc
              obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
              refine ih Γ ((l.uniq, CJoin.mk ps Γ jΓ jb) :: jΓ) body (pas ++ pend) nf ty
                hΓ (jgammaWF_cons hJ (CJWF.mk hΓ hJ)) ?_ hc
              intro p hp
              rcases List.mem_append.mp hp with hp | hp
              · obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                obtain ⟨hj2, hcj⟩ := hpt j (by omega)
                rw [← hpj]
                exact ih Γ jΓ args[j] [] (pas[j].1) (pas[j].2) hΓ hJ
                  (fun q hq => absurd hq (by simp)) hcj
              · exact hpend p hp
      | jump l es =>
          cases args with
          | cons a as => dsimp only at hc; exact error_ne_ok hc
          | nil =>
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨pas, hpas, hc⟩ := hc
              obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
              have hpasWF : ∀ p ∈ pas, NF.VarsWF P p.1 := by
                intro p hp
                obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
                obtain ⟨hj2, hcj⟩ := hpt j (by omega)
                rw [← hpj]
                exact ih Γ jΓ es[j] [] (pas[j].1) (pas[j].2) hΓ hJ
                  (fun q hq => absurd hq (by simp)) hcj
              cases hjx : jΓ.lookup l.uniq with
              | none => rw [hjx] at hc; exact error_ne_ok hc
              | some cj =>
                  rw [hjx] at hc
                  obtain ⟨ps, Γc, cjs, jb⟩ := cj
                  dsimp only at hc
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  have hcw := hJ l.uniq _ hjx
                  cases hcw with
                  | mk hΓc hcjs =>
                      refine ih (bindFieldsΓ ps pas Γc) cjs jb pend nf ty ?_ hcjs hpend hc
                      exact gammaWF_bindFields _ _ hΓc hpasWF
      | cases resTy scrut binder alts =>
          dsimp only at hc
          rw [bind_ok_iff] at hc
          obtain ⟨dnt, hdn, hc⟩ := hc
          obtain ⟨dn, dty⟩ := dnt
          dsimp only at hc
          rw [bind_ok_iff] at hc
          obtain ⟨szT, hsz, hc⟩ := hc
          rw [bind_ok_iff] at hc
          obtain ⟨pas, hpas, hc⟩ := hc
          rw [bind_ok_iff] at hc
          obtain ⟨resTy', hpeel, hc⟩ := hc
          obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
          have hdnWF : NF.VarsWF P dn :=
            ih Γ jΓ scrut [] dn dty hΓ hJ (fun q hq => absurd hq (by simp)) hdn
          have hΓ' : GammaWF P (Γ.insert binder.uniq (dn, dty)) :=
            gammaWF_insert hΓ hdnWF
          have hpallWF : ∀ p ∈ pas ++ pend, NF.VarsWF P p.1 := by
            intro p hp
            rcases List.mem_append.mp hp with hp | hp
            · obtain ⟨j, hj, hpj⟩ := List.getElem_of_mem hp
              obtain ⟨hj2, hcj⟩ := hpt j (by omega)
              rw [← hpj]
              exact ih Γ jΓ args[j] [] (pas[j].1) (pas[j].2) hΓ hJ
                (fun q hq => absurd hq (by simp)) hcj
            · exact hpend p hp
          rcases alts with _ | ⟨⟨con0, bs0, dbody⟩, rest⟩
          · dsimp only at hc
            rw [cchainJ] at hc
            exact error_ne_ok hc
          · cases con0 with
            | default =>
                dsimp only at hc
                rw [bind_ok_iff] at hc
                obtain ⟨dnt2, hdb, hc⟩ := hc
                obtain ⟨dnf, dbt⟩ := dnt2
                dsimp only at hc
                split at hc
                rotate_left
                · exact error_ne_ok hc
                have hdnfWF : NF.VarsWF P dnf :=
                  ih _ jΓ dbody (pas ++ pend) dnf dbt hΓ' hJ hpallWF hdb
                exact cchainJ_varsWF ih hΓ' hJ hdnWF hpallWF rest (some dnf) (nf, ty)
                  (fun acc h => by injection h with h; subst h; exact hdnfWF) hc
            | dataAlt cn =>
                dsimp only at hc
                exact cchainJ_varsWF ih hΓ' hJ hdnWF hpallWF _ none (nf, ty)
                  (fun acc h => absurd h (by simp)) hc
            | litAlt i =>
                dsimp only at hc
                exact cchainJ_varsWF ih hΓ' hJ hdnWF hpallWF _ none (nf, ty)
                  (fun acc h => absurd h (by simp)) hc
      | litStr sl =>
          dsimp only at hc
          exact error_ne_ok hc
      | litList tyl es =>
          dsimp only at hc
          exact error_ne_ok hc
      | app f' a' =>
          dsimp only at hc
          exact error_ne_ok hc

/-- `VarsWF` at the top level. -/
theorem cexpFull_varsWF {Δ : DEnv} {dmap : HashMap Int Defn} {P : String → Nat → Prop}
    {fuel : Nat} {Γ : HashMap Int (NF × Ty)} {e : Exp} {nf : NF} {ty : Ty}
    (hΓ : GammaWF P Γ) (hc : cexpFull Δ dmap fuel Γ e = .ok (nf, ty)) :
    NF.VarsWF P nf :=
  cexpJ_varsWF fuel Γ [] e [] nf ty hΓ jgammaWF_nil
    (fun p hp => absurd hp (by simp)) hc


/-! # The DAG mirror of the full compiler (Phase 4d)

`cexpJD` compiles the same fragment as `cexpJ` into
`Rwv.Hyle.BridgeDag`'s hash-consed node store, raw node for raw node
(the way `symExpDag` mirrors `symExp`), so the per-definition and
per-label validators can compare by store index where the tree normal
forms blow up. The mirror's guards are exactly `cexpJ`'s (plus
width-coherence checks that keep the store invariant and can only
make the mirror fail MORE often, never differently). THE soundness
statement is the simulation `cexpJD_sim`/`cexpFullD_sim`: a
successful DAG compilation certifies a successful tree compilation
whose normal form is the reading of the returned index
(`symExpDag_sim`'s pattern, lifted through the join environment and
the pending-argument discipline); beneath it sit the environment/join
simulation relations, every constructor helper
(`catListD`/`catNFD`/`sliceNFD`/`resizeNFD`/`slicesWD`/`cpendVecD`/
`cpendGenD`/`pushVars`), the integer-literal row, the complete row
tables (`cprimD_sim`, `cprimFD_sim` — split per row family), and the
case-chain legs (`cAltJD_sim`/`cchainJD_sim`). On top of the
simulation, `checkDefnPairDag` is the per-definition DAG verdict —
both sides compiled into ONE shared store over pushed parameter
variables, compared by index after three `renorm` sweeps (the
store-level `cfoldW3`) — and `checkDefnPairDag_sound` reduces its
`true` verdict to the tree legs, which is how `checkDefnPair` (the
DAG leg ordered first) keeps `checkDefnPair_sound` intact on pairs
whose tree normal forms blow up. -/

section DagMirror

open Rwv.Hyle.BridgeDag (Dag DNode mIdx)

/-- The index-based symbolic environment (`cexpJ`'s Γ with normal
forms as store indices). -/
abbrev DGamma := HashMap Int (Nat × Ty)

/-- Index-based compile-time join closures (mirror `Cexp.CJoin`). -/
inductive CJoinD where
  | mk (params : List Id) (Γ : DGamma) (js : List (Int × CJoinD)) (body : Exp)

abbrev CJEnvD := List (Int × CJoinD)

/-- `Cexp.teqAll` on index pairs (only the types are consulted). -/
def teqAllD : List (Nat × Ty) → List Ty → Bool
  | [], [] => true
  | (_, t) :: ps, u :: us => teq t u && teqAllD ps us
  | _, _ => false

/-- `Cexp.teqAllN` on index pairs (only the types are consulted). -/
def teqAllND (Δ : DEnv) : List (Nat × Ty) → List Ty → Bool
  | [], [] => true
  | (_, t) :: ps, u :: us => teqN Δ t u && teqAllND Δ ps us
  | _, _ => false

/-- `Cexp.catList` on indices. -/
def catListD (d : Dag) : List Nat → Dag × Nat
  | [] => d.mkLit Rwv.Hyle.BV.nil
  | [x] => (d, x)
  | x :: y :: xs =>
      let (d₁, r) := catListD d (y :: xs)
      d₁.rawCat x r

/-- `Cexp.catNF` on width-annotated indices. -/
def catNFD (d : Dag) (xs : List (Nat × Nat)) : Dag × Nat :=
  catListD d ((xs.filter fun p => p.2 != 0).map (·.1))

/-- `Cexp.sliceNF` on indices. -/
def sliceNFD (d : Dag) (off w e : Nat) : Dag × Nat :=
  if w = 0 then d.mkLit Rwv.Hyle.BV.nil else d.rawSlice off w e

/-- `Cexp.resizeNF` on indices. -/
def resizeNFD (d : Dag) (m wa : Nat) (a : Nat) : Dag × Nat :=
  if m = wa then (d, a)
  else if wa < m then d.rawPrim1 (.zext m) a
  else d.rawPrim1 (.trunc m) a

/-- `Cexp.clitInt` on the store. -/
def clitIntD (d : Dag) (ty : Ty) (n : Int) : Except String (Dag × Nat × Ty) := do
  let (nf, rty) ← clitInt ty n
  match nf with
  | .lit v =>
      let dr := d.mkLit v
      .ok (dr.1, dr.2, rty)
  | _ => .error "cexpD: non-literal integer-literal image"

/-- The 4a row table on indices (`Cexp.cprim`, row for row; the rows
build only `prim1`/`prim2`/`slice` nodes over the argument indices). -/
def cprimD (d : Dag) (pty : Ty) (b : Builtin) (pas : List (Nat × Ty)) :
    Except String (Dag × Nat × Ty) :=
  let res := (Ty.flattenArrow pty).2
  let arith := fun (op : Op) (ta : Ty) (a b' : Nat) => do
    let m ← vecBoolLen "arith row" res
    let wa ← vecBoolLen "arith row" ta
    if wa = m then
      let (d₁, r) := d.rawPrim2 op a b'
      .ok (d₁, r, res)
    else .error "cexp: arith row width mismatch"
  let cmp := fun (op : Op) (a b' : Nat) =>
    if isBoolT res then
      let (d₁, r) := d.rawPrim2 op a b'
      (.ok (d₁, r, res) : Except String (Dag × Nat × Ty))
    else .error "cexp: comparison row at a non-Bool result type"
  let red := fun (negated : Bool) (op : Op) (a : Nat) =>
    if isBoolT res then
      if negated then
        let (d₁, r) := d.rawPrim1 op a
        let (d₂, r₂) := d₁.rawPrim1 .not r
        (.ok (d₂, r₂, res) : Except String (Dag × Nat × Ty))
      else
        let (d₁, r) := d.rawPrim1 op a
        .ok (d₁, r, res)
    else .error "cexp: reduction row at a non-Bool result type"
  match b, pas with
  | .bits, [(a, _)] => do
      let k ← vecBoolLen "rwPrimBits" res
      if k = 128 then .ok (d, a, res) else .error "rwPrimBits: result is not Vec 128 Bool"
  | .resize, [(a, ta)] => do
      let m ← vecBoolLen "rwPrimResize" res
      let wa ← vecBoolLen "rwPrimResize" ta
      if m = wa then .ok (d, a, res)
      else if wa < m then
        let (d₁, r) := d.rawPrim1 (.zext m) a
        .ok (d₁, r, res)
      else
        let (d₁, r) := d.rawPrim1 (.trunc m) a
        .ok (d₁, r, res)
  | .add, [(a, ta), (b', _)] => arith .add  ta a b'
  | .sub, [(a, ta), (b', _)] => arith .sub  ta a b'
  | .mul, [(a, ta), (b', _)] => arith .mul  ta a b'
  | .div, [(a, ta), (b', _)] => arith .udiv ta a b'
  | .mod, [(a, ta), (b', _)] => arith .umod ta a b'
  | .pow, [(a, ta), (b', _)] => arith .pow  ta a b'
  | .and, [(a, ta), (b', _)] => arith .and  ta a b'
  | .or,  [(a, ta), (b', _)] => arith .or   ta a b'
  | .xor, [(a, ta), (b', _)] => arith .xor  ta a b'
  | .xnor, [(a, ta), (b', _)] => do
      let m ← vecBoolLen "rwPrimXNor" res
      let wa ← vecBoolLen "rwPrimXNor" ta
      if wa = m then
        let (d₁, r) := d.rawPrim2 .xor a b'
        let (d₂, r₂) := d₁.rawPrim1 .not r
        .ok (d₂, r₂, res)
      else .error "rwPrimXNor: width mismatch"
  | .not, [(a, ta)] => do
      let m ← vecBoolLen "rwPrimNot" res
      let wa ← vecBoolLen "rwPrimNot" ta
      if wa = m then
        let (d₁, r) := d.rawPrim1 .not a
        .ok (d₁, r, res)
      else .error "rwPrimNot: width mismatch"
  | .lShift,      [(a, ta), (b', _)] => arith .shl  ta a b'
  | .rShift,      [(a, ta), (b', _)] => arith .lshr ta a b'
  | .rShiftArith, [(a, ta), (b', _)] => arith .ashr ta a b'
  | .eq,   [(a, _), (b', _)] => cmp .eq  a b'
  | .gt,   [(a, _), (b', _)] => cmp .ugt a b'
  | .gtEq, [(a, _), (b', _)] => cmp .uge a b'
  | .lt,   [(a, _), (b', _)] => cmp .ult a b'
  | .ltEq, [(a, _), (b', _)] => cmp .ule a b'
  | .lAnd, [(a, _), (b', _)] =>
      if isBoolT res then
        let (d₁, ra) := d.rawPrim1 .redor a
        let (d₂, rb) := d₁.rawPrim1 .redor b'
        let (d₃, r) := d₂.rawPrim2 .and ra rb
        .ok (d₃, r, res)
      else .error "cexp: comparison row at a non-Bool result type"
  | .lOr, [(a, _), (b', _)] =>
      if isBoolT res then
        let (d₁, ra) := d.rawPrim1 .redor a
        let (d₂, rb) := d₁.rawPrim1 .redor b'
        let (d₃, r) := d₂.rawPrim2 .or ra rb
        .ok (d₃, r, res)
      else .error "cexp: comparison row at a non-Bool result type"
  | .lNot, [(a, _)] => red true .redor a
  | .rAnd,  [(a, _)] => red false .redand a
  | .rNAnd, [(a, _)] => red true  .redand a
  | .rOr,   [(a, _)] => red false .redor  a
  | .rNor,  [(a, _)] => red true  .redor  a
  | .rXOr,  [(a, _)] => red false .redxor a
  | .rXNor, [(a, _)] => red true  .redxor a
  | .msBit, [(a, ta)] => do
      if isBoolT res then do
        let wa ← vecBoolLen "rwPrimMSBit" ta
        if wa ≥ 1 then
          let (d₁, r) := d.rawSlice (wa - 1) 1 a
          .ok (d₁, r, res)
        else .error "rwPrimMSBit: zero-width argument"
      else .error "rwPrimMSBit: non-Bool result type"
  | _, _ => .error s!"cexp: unsupported builtin {b.name} (outside the Phase 4a fragment)"

/-- Push a list of (offset, width) slices of a base index, keeping the
widths. -/
def slicesWD (d : Dag) (a : Nat) : List (Nat × Nat) → Dag × List (Nat × Nat)
  | [] => (d, [])
  | (off, w) :: rest =>
      let (d₁, s) := sliceNFD d off w a
      let (d₂, rs) := slicesWD d₁ a rest
      (d₂, (s, w) :: rs)

/-- The extended row table on indices (`Cexp.cprimF`, row for row). -/
def cprimFD (d : Dag) (Δ : DEnv) (szf : Nat) (pty : Ty) (b : Builtin)
    (pas : List (Nat × Ty)) : Except String (Dag × Nat × Ty) :=
  match b with
  | .finite =>
      (match pas with
      | [(a, _)] => do
          let n ← finBoundT "rwPrimFinite" (Ty.flattenArrow pty).2
          match d.nodes[a]? with
          | some (.lit v) =>
              if v.nat < n then
                let dr := d.mkLit ⟨nbits n, BitVec.ofNat (nbits n) v.nat⟩
                .ok (dr.1, dr.2, (Ty.flattenArrow pty).2)
              else .error s!"rwPrimFinite: {v.nat} is not representable in Finite {n}"
          | _ => .error "rwPrimFinite: can't determine the argument value at compile time"
      | _ => .error "rwPrimFinite: arity mismatch")
  | .natVal =>
      (match pas with
      | [(_, _)] => do
          let pt ← domTyT "rwPrimNatVal" (Ty.flattenArrow pty).1 0
          let k ← proxyNatT "rwPrimNatVal" pt
          match Ty.flatten (Ty.flattenArrow pty).2 with
          | (.con "Integer", []) =>
              let dr := d.mkLit ⟨128, BitVec.ofNat 128 k⟩
              .ok (dr.1, dr.2, (Ty.flattenArrow pty).2)
          | _ => .error "rwPrimNatVal: non-Integer result type"
      | _ => .error "rwPrimNatVal: arity mismatch")
  | .vecIndexProxy =>
      (match pas with
      | [(a, ta), (_, _)] => do
          let pt ← domTyT "rwPrimVecIndexProxy" (Ty.flattenArrow pty).1 1
          let k ← proxyNatT "rwPrimVecIndexProxy" pt
          let (len, tea) ← vecLenElem "rwPrimVecIndexProxy" ta
          if teq tea (Ty.flattenArrow pty).2 then do
            let se ← Δ.sizeOf szf [] (Ty.flattenArrow pty).2
            let szA ← Δ.sizeOf szf [] ta
            if k < len then
              let (d₁, r) := sliceNFD d (szA - k * se - se) se a
              .ok (d₁, r, (Ty.flattenArrow pty).2)
            else .error "rwPrimVecIndexProxy: index out of range"
          else .error "rwPrimVecIndexProxy: element-type mismatch"
      | _ => .error "rwPrimVecIndexProxy: arity mismatch")
  | .finiteMinBound =>
      (match pas with
      | [] => do
          let n ← finBoundT "rwPrimFiniteBound" (Ty.flattenArrow pty).2
          if 1 ≤ n then
            let dr := d.mkLit ⟨nbits n, BitVec.ofNat (nbits n) 0⟩
            .ok (dr.1, dr.2, (Ty.flattenArrow pty).2)
          else .error "rwPrimFiniteBound: Finite 0 is uninhabited"
      | _ => .error "rwPrimFiniteBound: arity mismatch")
  | .finiteMaxBound =>
      (match pas with
      | [] => do
          let n ← finBoundT "rwPrimFiniteBound" (Ty.flattenArrow pty).2
          if 1 ≤ n then
            let dr := d.mkLit ⟨nbits n, BitVec.ofNat (nbits n) (n - 1)⟩
            .ok (dr.1, dr.2, (Ty.flattenArrow pty).2)
          else .error "rwPrimFiniteBound: Finite 0 is uninhabited"
      | _ => .error "rwPrimFiniteBound: arity mismatch")
  | .toFinite =>
      (match pas with
      | [(a, ta)] => do
          let n ← finBoundT "rwPrimToFinite" (Ty.flattenArrow pty).2
          let wa ← vecBoolLen "rwPrimToFinite" ta
          if 2 ^ wa ≤ n then
            let dr := resizeNFD d (nbits n) wa a
            .ok (dr.1, dr.2, (Ty.flattenArrow pty).2)
          else .error "rwPrimToFinite: bit vector not representable in the Finite bound"
      | _ => .error "rwPrimToFinite: arity mismatch")
  | .toFiniteMod =>
      (match pas with
      | [(a, ta)] => do
          let n ← finBoundT "rwPrimToFiniteMod" (Ty.flattenArrow pty).2
          let wa ← vecBoolLen "rwPrimToFiniteMod" ta
          if 2 ^ wa ≤ n then
            let dr := resizeNFD d (nbits n) wa a
            .ok (dr.1, dr.2, (Ty.flattenArrow pty).2)
          else if n = 0 then
            .error "rwPrimToFiniteMod: Finite 0 result (uninhabited)"
          else
            let w := max wa (nbits n)
            let (d₁, r₁) := resizeNFD d w wa a
            let (d₂, l₂) := d₁.mkLit ⟨w, BitVec.ofNat w n⟩
            let (d₃, r₃) := d₂.rawPrim2 .umod r₁ l₂
            let (d₄, r₄) := resizeNFD d₃ (nbits n) w r₃
            .ok (d₄, r₄, (Ty.flattenArrow pty).2)
      | _ => .error "rwPrimToFiniteMod: arity mismatch")
  | .fromFinite =>
      (match pas with
      | [(a, ta)] => do
          let n ← finBoundT "rwPrimFromFinite" ta
          let m ← vecBoolLen "rwPrimFromFinite" (Ty.flattenArrow pty).2
          if n ≤ 2 ^ m then
            let dr := resizeNFD d m (nbits n) a
            .ok (dr.1, dr.2, (Ty.flattenArrow pty).2)
          else .error "rwPrimFromFinite: Finite bound not representable"
      | _ => .error "rwPrimFromFinite: arity mismatch")
  | .vecReplicate =>
      (match pas with
      | [(a, ta)] => do
          let (n, te) ← vecLenElem "rwPrimVecReplicate" (Ty.flattenArrow pty).2
          if teq ta te then do
            let sz ← Δ.sizeOf szf [] (Ty.flattenArrow pty).2
            if n = 0 ∨ sz = 0 then
              let dr := d.mkLit Rwv.Hyle.BV.nil
              .ok (dr.1, dr.2, (Ty.flattenArrow pty).2)
            else
              let dr := d.rawPrim1 (.rep n) a
              .ok (dr.1, dr.2, (Ty.flattenArrow pty).2)
          else .error "rwPrimVecReplicate: element-type mismatch"
      | _ => .error "rwPrimVecReplicate: arity mismatch")
  | .vecConcat =>
      (match pas with
      | [(a, ta), (b', tb)] => do
          let (nr, ter) ← vecLenElem "rwPrimVecConcat" (Ty.flattenArrow pty).2
          let (n1, te1) ← vecLenElem "rwPrimVecConcat" ta
          let (n2, te2) ← vecLenElem "rwPrimVecConcat" tb
          if teq te1 ter && teq te2 ter && nr == n1 + n2 then do
            let sa ← Δ.sizeOf szf [] ta
            let sb ← Δ.sizeOf szf [] tb
            let (d₁, r) := catNFD d [(a, sa), (b', sb)]
            .ok (d₁, r, (Ty.flattenArrow pty).2)
          else .error "rwPrimVecConcat: type mismatch"
      | _ => .error "rwPrimVecConcat: arity mismatch")
  | .vecReverse =>
      (match pas with
      | [(a, ta)] => do
          let (nr, ter) ← vecLenElem "rwPrimVecReverse" (Ty.flattenArrow pty).2
          let (n1, te1) ← vecLenElem "rwPrimVecReverse" ta
          if teq te1 ter && nr == n1 then do
            let se ← Δ.sizeOf szf [] ter
            let (d₁, ps) := slicesWD d a ((List.range n1).map fun k => (k * se, se))
            let (d₂, r) := catNFD d₁ ps
            .ok (d₂, r, (Ty.flattenArrow pty).2)
          else .error "rwPrimVecReverse: type mismatch"
      | _ => .error "rwPrimVecReverse: arity mismatch")
  | .vecSlice =>
      (match pas with
      | [(_, _), (a, ta)] => do
          let pt ← domTyT "rwPrimVecSlice" (Ty.flattenArrow pty).1 0
          let i ← proxyNatT "rwPrimVecSlice" pt
          let (m, ter) ← vecLenElem "rwPrimVecSlice" (Ty.flattenArrow pty).2
          let (len, tea) ← vecLenElem "rwPrimVecSlice" ta
          if teq tea ter then do
            let se ← Δ.sizeOf szf [] ter
            let szA ← Δ.sizeOf szf [] ta
            if i + m ≤ len then
              let (d₁, r) := sliceNFD d (szA - i * se - m * se) (m * se) a
              .ok (d₁, r, (Ty.flattenArrow pty).2)
            else .error "rwPrimVecSlice: slice out of range"
          else .error "rwPrimVecSlice: element-type mismatch"
      | _ => .error "rwPrimVecSlice: arity mismatch")
  | .vecRSlice =>
      (match pas with
      | [(_, _), (a, ta)] => do
          let pt ← domTyT "rwPrimVecRSlice" (Ty.flattenArrow pty).1 0
          let i ← proxyNatT "rwPrimVecRSlice" pt
          let (m, ter) ← vecLenElem "rwPrimVecRSlice" (Ty.flattenArrow pty).2
          let (len, tea) ← vecLenElem "rwPrimVecRSlice" ta
          if teq tea ter then do
            let se ← Δ.sizeOf szf [] ter
            let _szA ← Δ.sizeOf szf [] ta
            if i + m ≤ len then
              let (d₁, r) := sliceNFD d (i * se) (m * se) a
              .ok (d₁, r, (Ty.flattenArrow pty).2)
            else .error "rwPrimVecRSlice: slice out of range"
          else .error "rwPrimVecRSlice: element-type mismatch"
      | _ => .error "rwPrimVecRSlice: arity mismatch")
  | .vecIndex =>
      (match pas with
      | [(a, ta), (i, ti)] => do
          let (len, te) ← vecLenElem "rwPrimVecIndex" ta
          let nb ← finBoundT "rwPrimVecIndex" ti
          if nb == len && teq te (Ty.flattenArrow pty).2 then do
            let se ← Δ.sizeOf szf [] (Ty.flattenArrow pty).2
            let szA ← Δ.sizeOf szf [] ta
            if 1 ≤ len ∧ len < 2 ^ 128 ∧ len * se < 2 ^ 128 then
              let w := max (nbits nb) 128
              let (d₁, i') := resizeNFD d w (nbits nb) i
              let (d₂, lLen) := d₁.mkLit ⟨w, BitVec.ofNat w len⟩
              let (d₃, s₁) := d₂.rawPrim2 .sub lLen i'
              let (d₄, l1) := d₃.mkLit ⟨w, 1⟩
              let (d₅, s₂) := d₄.rawPrim2 .sub s₁ l1
              let (d₆, lSe) := d₅.mkLit ⟨w, BitVec.ofNat w se⟩
              let (d₇, amt) := d₆.rawPrim2 .mul s₂ lSe
              let (d₈, sh) := d₇.rawPrim2 .lshr a amt
              let (d₉, r) := resizeNFD d₈ se szA sh
              .ok (d₉, r, (Ty.flattenArrow pty).2)
            else .error "rwPrimVecIndex: width guard failed"
          else .error "rwPrimVecIndex: type mismatch"
      | _ => .error "rwPrimVecIndex: arity mismatch")
  | _ => cprimD d pty b pas

/-- The DAG mirror has no Cryptol row (the tree fallback carries the
foreign tier); its 4a delegation errors on the foreign builtins. -/
private theorem cprimD_cry_err {d : Dag} {pty : Ty} {pas : List (Nat × Ty)}
    {r : Dag × Nat × Ty} (h : cprimD d pty .cryptol pas = .ok r) : False := by
  rcases pas with _ | ⟨p, _ | ⟨q, _ | ⟨w, ws⟩⟩⟩ <;> exact error_ne_ok h

private theorem cprimFD_cry_err {d : Dag} {Δ : DEnv} {szf : Nat} {pty : Ty}
    {pas : List (Nat × Ty)} {r : Dag × Nat × Ty}
    (h : cprimFD d Δ szf pty .cryptol pas = .ok r) : False :=
  cprimD_cry_err (show cprimD d pty .cryptol pas = .ok r from h)

/-- Bind binders over a `DGamma` (`Cexp.bindFieldsΓ`). -/
def bindFieldsΓD (xs : List Id) (nts : List (Nat × Ty)) (Γ : DGamma) : DGamma :=
  (xs.zip nts).foldr (fun (x, nt) m => m.insert x.uniq nt) Γ

/-- `Cexp.mkGamma` on indices. -/
def mkGammaD (params : List Id) (pas : List (Nat × Ty)) : DGamma :=
  bindFieldsΓD params pas ∅

/-- Per-element pend heads for the VecMap row: MSB-first input-element
slices of the compiled vector argument. -/
def cpendVecD (d : Dag) (szV seI a : Nat) : List Nat → Dag × List Nat
  | [] => (d, [])
  | i :: is =>
      let (d₁, s) := sliceNFD d (szV - (i + 1) * seI) seI a
      let (d₂, rest) := cpendVecD d₁ szV seI a is
      (d₂, s :: rest)

/-- Per-element pend heads for the VecGenerate row: the index literals
at width `nbits nElems`. -/
def cpendGenD (d : Dag) (nElems : Nat) : List Nat → Dag × List Nat
  | [] => (d, [])
  | i :: is =>
      let (d₁, s) := d.mkLit ⟨nbits nElems, BitVec.ofNat (nbits nElems) i⟩
      let (d₂, rest) := cpendGenD d₁ nElems is
      (d₂, s :: rest)

mutual

/-- `Cexp.cAltJ`, mirrored on the store. -/
def cAltJD (Δ : DEnv) (dmap : HashMap Int Defn) (fuel : Nat) (Γ' : DGamma)
    (jΓ : CJEnvD) (dty : Ty) (szT : Nat) (dn : Nat) (resTy : Ty) (pend : List (Nat × Ty)) :
    Alt → Option Nat → Dag → Except String (Dag × Nat)
  | .mk .default _ _, _, _ => .error "cexp: default alternative not first"
  | .mk (.dataAlt cn) xs body, macc, d => do
      if ctorOfB Δ dty cn then do
        let (tag, w) ← Δ.ctorTag dty cn
        match Δ.ctorSig.get? cn with
        | none => .error s!"cexp: unknown constructor {cn}"
        | some sig => do
            let sub ← DEnv.matchTy (Ty.flattenArrow sig.ty).2 dty
            let instTys := (Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)
            if xs.length = instTys.length then do
              let szXs ← instTys.mapM (Δ.sizeOf (fuel + 1) [])
              if w + szXs.sum ≤ szT then do
                let offs := (List.range szXs.length).map fun i =>
                  (szXs.drop (i + 1)).sum
                let (d₁, slices) := (szXs.zip offs).foldr
                  (fun (sz, off) (dacc, acc) =>
                    let (d', r) := sliceNFD dacc off sz dn
                    (d', r :: acc))
                  (d, ([] : List Nat))
                let Γ'' := bindFieldsΓD xs (slices.zip instTys) Γ'
                let (d₂, bnf, bty) ← cexpJD Δ dmap fuel Γ'' jΓ body pend d₁
                if teqN Δ bty resTy then
                  match macc, w with
                  | some acc, _ + 1 =>
                      let (d₃, sl) := sliceNFD d₂ (szT - w) w dn
                      let (d₄, tl) := d₃.mkLit ⟨w, BitVec.ofNat w tag⟩
                      let (d₅, cnd) := d₄.rawPrim2 .eq sl tl
                      if d₅.widthOf bnf = d₅.widthOf acc then
                        .ok (d₅.rawIte cnd bnf acc)
                      else .error "cexpD: alternative arm width mismatch"
                  | _, _ => .ok (d₂, bnf)
                else .error "cexp: case alternative result-type mismatch"
              else .error s!"cexp: constructor {cn} wider than the discriminant"
            else .error s!"cexp: constructor {cn} binder arity mismatch"
      else .error s!"cexp: constructor {cn} does not belong to the discriminant type"
  | .mk (.litAlt i) _ body, macc, d => do
      let (d₁, bnf, bty) ← cexpJD Δ dmap fuel Γ' jΓ body pend d
      if teqN Δ bty resTy then
        match macc with
        | some acc =>
            let (d₂, tl) := d₁.mkLit ⟨szT, BitVec.ofInt szT i⟩
            let (d₃, cnd) := d₂.rawPrim2 .eq dn tl
            if d₃.widthOf bnf = d₃.widthOf acc then
              .ok (d₃.rawIte cnd bnf acc)
            else .error "cexpD: alternative arm width mismatch"
        | none => .ok (d₁, bnf)
      else .error "cexp: case alternative result-type mismatch"
termination_by alt _ _ => (fuel, 1, 0)

/-- `Cexp.cchainJ`, mirrored on the store. -/
def cchainJD (Δ : DEnv) (dmap : HashMap Int Defn) (fuel : Nat) (Γ' : DGamma)
    (jΓ : CJEnvD) (dty : Ty) (szT : Nat) (dn : Nat) (resTy : Ty) (pend : List (Nat × Ty)) :
    List Alt → Option Nat → Dag → Except String (Dag × Nat × Ty)
  | [], some els, d => .ok (d, els, resTy)
  | [], none, _ => .error "cexp: empty case"
  | [alt], none, d => do
      let (d₁, bnf) ← cAltJD Δ dmap fuel Γ' jΓ dty szT dn resTy pend alt none d
      .ok (d₁, bnf, resTy)
  | alt :: rest, macc, d => do
      let (d₁, accnf, _) ← cchainJD Δ dmap fuel Γ' jΓ dty szT dn resTy pend rest macc d
      let (d₂, bnf) ← cAltJD Δ dmap fuel Γ' jΓ dty szT dn resTy pend alt (some accnf) d₁
      .ok (d₂, bnf, resTy)
termination_by rest _ _ => (fuel, 2, rest.length)

/-- Compile a spine's arguments left to right, threading the store
(`args.mapM (cexpJ …)`, state-passing). -/
def cargsD (Δ : DEnv) (dmap : HashMap Int Defn) (fuel : Nat) (Γ : DGamma)
    (jΓ : CJEnvD) : List Exp → Dag → Except String (Dag × List (Nat × Ty))
  | [], d => .ok (d, [])
  | e :: es, d => do
      let (d₁, r, t) ← cexpJD Δ dmap fuel Γ jΓ e [] d
      let (d₂, rs) ← cargsD Δ dmap fuel Γ jΓ es d₁
      .ok (d₂, (r, t) :: rs)
termination_by es _ => (fuel, 3, es.length)

/-- Apply the function argument to each pre-built pend head, checking
the element result type (the VecMap/VecGenerate element loop). -/
def celemsD (Δ : DEnv) (dmap : HashMap Int Defn) (fuel : Nat) (Γ : DGamma)
    (jΓ : CJEnvD) (fE : Exp) (teI teO : Ty) (seO : Nat) :
    List Nat → Dag → Except String (Dag × List (Nat × Nat))
  | [], d => .ok (d, [])
  | p :: ps', d => do
      let (d₁, ei, ti) ← cexpJD Δ dmap fuel Γ jΓ fE [(p, teI)] d
      if teq ti teO then do
        let (d₂, rest) ← celemsD Δ dmap fuel Γ jΓ fE teI teO seO ps' d₁
        .ok (d₂, (ei, seO) :: rest)
      else .error "cexpD: element result-type mismatch"
termination_by ps' _ => (fuel, 3, ps'.length)

/-- `Cexp.cexpJ`, mirrored on the store (the corpus fragment: the
higher-order Vec rows and `vecFromList`'s list literals stay in the
tree leg). -/
def cexpJD (Δ : DEnv) (dmap : HashMap Int Defn) :
    Nat → DGamma → CJEnvD → Exp → List (Nat × Ty) → Dag →
    Except String (Dag × Nat × Ty)
  | 0, _, _, _, _, _ => .error "cexp: out of fuel"
  | fuel + 1, Γ, jΓ, e, pend, d =>
    match Eval.flattenApp e with
    | (.var x, args) =>
        match Γ.get? x.uniq with
        | some nt =>
            match args, pend with
            | [], [] => .ok (d, nt.1, nt.2)
            | _, _ => .error s!"cexp: unsupported application of a local variable: {x.occ}"
        | none =>
            match dmap.get? x.uniq with
            | some df => do
                let (d₁, pas) ← cargsD Δ dmap fuel Γ jΓ args d
                let pall := pas ++ pend
                if df.params.length ≤ pall.length then
                  if teqAllD (pall.take df.params.length) (df.params.map (·.sig.ty)) then
                    if df.params.length ≤ pas.length || x.uniq == df.name.uniq then
                      cexpJD Δ dmap fuel (mkGammaD df.params (pall.take df.params.length)) []
                        df.body (pall.drop df.params.length) d₁
                    else .error s!"cexp: call to {x.occ}: partial application through an inconsistent map"
                  else .error s!"cexp: call to {x.occ}: argument-type mismatch"
                else .error s!"cexp: call to {x.occ}: unsaturated ({pall.length} of {df.params.length} arguments)"
            | none => .error s!"cexp: unknown definition {x.occ}#{x.uniq}"
    | (.con cty c, args) =>
        match pend with
        | _ :: _ => .error s!"cexp: over-applied constructor {c}"
        | [] => do
            let (d₁, pas) ← cargsD Δ dmap fuel Γ jΓ args d
            let resTy := (Ty.flattenArrow cty).2
            if pas.length = (Ty.flattenArrow cty).1.length then
              match Δ.ctorSig.get? c with
              | some sig => do
                  let sub ← DEnv.matchTy (Ty.flattenArrow sig.ty).2 resTy
                  if teqAllND Δ pas ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)) then
                    if ctorOfB Δ resTy c then do
                      let whole ← Δ.sizeOf (fuel + 1) [] resTy
                      let (tag, w) ← Δ.ctorTag resTy c
                      let ws ← ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)).mapM
                        (Δ.sizeOf (fuel + 1) [])
                      if w + ws.sum ≤ whole then
                        let (d₂, tl) := d₁.mkLit ⟨w, BitVec.ofNat w tag⟩
                        let (d₃, pl) := d₂.mkLit ⟨whole - w - ws.sum, 0⟩
                        let (d₄, r) := catNFD d₃ ((tl, w)
                                    :: (pl, whole - w - ws.sum)
                                    :: (pas.map (·.1)).zip ws)
                        .ok (d₄, r, resTy)
                      else .error s!"cexp: constructor {c} wider than its type"
                    else .error s!"cexp: constructor {c} does not belong to its result type"
                  else .error s!"cexp: constructor {c}: field-type mismatch"
              | none => .error s!"cexp: unknown constructor {c}"
            else .error s!"cexp: unsaturated constructor {c}"
    | (.prim pty b, args) =>
        match pend with
        | _ :: _ => .error "cexp: over-applied primitive"
        | [] =>
            match b with
            | .error => do
                let rty := Eval.dropArrows args.length pty
                let sz ← Δ.sizeOf (fuel + 1) [] rty
                let zv ← Δ.zeroVal (fuel + 1) rty
                if vtyB Δ (fuel + 1) zv rty then
                  match Val.rep Δ (fuel + 1) zv with
                  | .ok bv =>
                      if bv == (⟨sz, 0⟩ : BV) then
                        let dr := d.mkLit ⟨sz, 0⟩
                        .ok (dr.1, dr.2, rty)
                      else .error "rwPrimError: zero value is not all-zeros at the result width"
                  | .error _ => .error "rwPrimError: zero value has no representation"
                else .error "rwPrimError: zero value not canonical at the result type"
            | .vecFromList =>
                (match args with
                | [.litList _ els] => do
                    let (d₁, pas) ← cargsD Δ dmap fuel Γ jΓ els d
                    let res := (Ty.flattenArrow pty).2
                    match Ty.flatten res with
                    | (.con "Vec", [nt, te]) =>
                        match Ty.evalNat nt with
                        | some k =>
                            if pas.length = k then
                              if teqAllD pas (List.replicate pas.length te) then do
                                let se ← Δ.sizeOf (fuel + 1) [] te
                                let (d₂, r) := catNFD d₁ ((pas.map (·.1)).map (·, se))
                                .ok (d₂, r, res)
                              else .error "rwPrimVecFromList: element-type mismatch"
                            else .error "rwPrimVecFromList: length mismatch"
                        | none => .error "rwPrimVecFromList: open Vec length"
                    | _ => .error "rwPrimVecFromList: non-Vec result type"
                | _ => .error "rwPrimVecFromList: argument must be a list literal")
            | .bitIndex =>
                (match args with
                | [argE, iE] =>
                    (match finLitE iE with
                    | some i => do
                        let (d₁, a, ta) ← cexpJD Δ dmap fuel Γ jΓ argE [] d
                        let wa ← vecBoolLen "rwPrimBitIndex" ta
                        if isBoolT (Ty.flattenArrow pty).2 then
                          if i < wa then
                            let (d₂, r) := sliceNFD d₁ i 1 a
                            .ok (d₂, r, (Ty.flattenArrow pty).2)
                          else .error "rwPrimBitIndex: index out of range"
                        else .error "rwPrimBitIndex: non-Bool result type"
                    | none => .error "rwPrimBitIndex: index must be a Finite literal")
                | _ => .error "rwPrimBitIndex: arity mismatch")
            | .bitSlice =>
                (match args with
                | [argE, jE, iE] =>
                    (match finLitE jE, finLitE iE with
                    | some j, some i =>
                        if i ≤ j + 1 then do
                          let (d₁, a, ta) ← cexpJD Δ dmap fuel Γ jΓ argE [] d
                          let wa ← vecBoolLen "rwPrimBitSlice" ta
                          let mr ← vecBoolLen "rwPrimBitSlice" (Ty.flattenArrow pty).2
                          if mr = j + 1 - i ∧ i + (j + 1 - i) ≤ wa then
                            let (d₂, r) := sliceNFD d₁ i (j + 1 - i) a
                            .ok (d₂, r, (Ty.flattenArrow pty).2)
                          else .error "rwPrimBitSlice: slice out of range"
                        else .error s!"rwPrimBitSlice: invalid slice (j: {j}, i: {i})"
                    | _, _ => .error "rwPrimBitSlice: indices must be Finite literals")
                | _ => .error "rwPrimBitSlice: arity mismatch")
            | .vecMap =>
                (match args with
                | [fE, argE] => do
                    let (nElems, teO) ← vecLenElem "rwPrimVecMap" (Ty.flattenArrow pty).2
                    let seO ← Δ.sizeOf (fuel + 1) [] teO
                    let (d₁, a, ta) ← cexpJD Δ dmap fuel Γ jΓ argE [] d
                    let (len, teI) ← vecLenElem "rwPrimVecMap" ta
                    let seI ← Δ.sizeOf (fuel + 1) [] teI
                    let szV ← Δ.sizeOf (fuel + 1) [] ta
                    if len = nElems then do
                      let (d₂, pends) := cpendVecD d₁ szV seI a (List.range nElems)
                      let (d₃, ps) ← celemsD Δ dmap fuel Γ jΓ fE teI teO seO pends d₂
                      let (d₄, r) := catNFD d₃ ps
                      .ok (d₄, r, (Ty.flattenArrow pty).2)
                    else .error "rwPrimVecMap: length mismatch"
                | _ => .error "rwPrimVecMap: arity mismatch")
            | .vecGenerate =>
                (match args with
                | [fE] => do
                    let (nElems, teO) ← vecLenElem "rwPrimVecGenerate" (Ty.flattenArrow pty).2
                    let seO ← Δ.sizeOf (fuel + 1) [] teO
                    let ft ← domTyT "rwPrimVecGenerate" (Ty.flattenArrow pty).1 0
                    match ft with
                    | .arrow tFin _ => do
                        let nb ← finBoundT "rwPrimVecGenerate" tFin
                        if nb = nElems then do
                          let (d₁, pends) := cpendGenD d nElems (List.range nElems)
                          let (d₂, ps) ← celemsD Δ dmap fuel Γ jΓ fE tFin teO seO pends d₁
                          let (d₃, r) := catNFD d₂ ps
                          .ok (d₃, r, (Ty.flattenArrow pty).2)
                        else .error "rwPrimVecGenerate: generator domain-bound mismatch"
                    | _ => .error "rwPrimVecGenerate: non-arrow function-argument type"
                | _ => .error "rwPrimVecGenerate: arity mismatch")
            | .cryptol =>
                -- The foreign row's DAG mirror (stage B): the spliced
                -- entry inlined by `symExpDag` over the shared store,
                -- gated extern-free exactly like the tree arm.
                (match args with
                | .litStr f :: .litStr n :: _impl :: rest => do
                    let ity ← Eval.domTy "rwPrimCryptol" (Ty.flattenArrow pty).1 2
                    if rest.length = (Ty.flattenArrow ity).1.length then
                      match Δ.cryD f n ity with
                      | some g =>
                          (match Δ.hyleDefs.get? g with
                          | some hd => do
                              let (d₁, pas) ← cargsD Δ dmap fuel Γ jΓ rest d
                              if pas.length = hd.params.length then
                                if pas.all (fun p => d₁.xcallFreeM p.1) then do
                                  let (d₂, i) ← Rwv.Hyle.BridgeDag.symExpDag Δ.hyleDefs Δ.hyleX
                                    Δ.hyleFuel (HashMap.ofList (hd.params.zip (pas.map (·.1))))
                                    hd.body d₁
                                  if d₂.xcallFreeM i then
                                    .ok (d₂, i, (Ty.flattenArrow ity).2)
                                  else .error "rwPrimCryptol: spliced definition reaches an extern (out of scope)"
                                else .error "rwPrimCryptol: extern-derived argument (out of scope)"
                              else .error "rwPrimCryptol: spliced definition arity mismatch"
                          | none => .error s!"rwPrimCryptol: unknown spliced definition {g}")
                      | none => .error "rwPrimCryptol: no entry mapping for this instantiation"
                    else .error "rwPrimCryptol: unsaturated foreign application"
                | _ => .error "rwPrimCryptol: malformed foreign application")
            | .«extern» =>
                -- The model-less extern row's DAG mirror (stage B).
                (match args with
                | _ps :: _clk :: _rst :: _as :: _rs :: .litStr s :: _impl :: _inst :: rest => do
                    let ity ← Eval.domTy "rwPrimExtern" (Ty.flattenArrow pty).1 6
                    if rest.length = (Ty.flattenArrow ity).1.length then
                      match Δ.hyleX.get? s with
                      | some _ => .error s!"rwPrimExtern {s}: model-carrying extern (validator row out of scope)"
                      | none => do
                          let (d₁, pas) ← cargsD Δ dmap fuel Γ jΓ rest d
                          let res := (Ty.flattenArrow ity).2
                          let w ← Δ.sizeOf (fuel + 1) [] res
                          let (d₂, ip) := d₁.xpackD (pas.map (·.1))
                          let (d₃, r) := d₂.mkXcallD w s ip
                          .ok (d₃, r, res)
                    else .error "rwPrimExtern: unsaturated foreign application"
                | _ => .error "rwPrimExtern: malformed foreign application")
            | _ => do
                let (d₁, pas) ← cargsD Δ dmap fuel Γ jΓ args d
                cprimFD d₁ Δ (fuel + 1) pty b pas
    | (.litInt ty n, []) =>
        match pend with
        | [] => clitIntD d ty n
        | _ :: _ => .error "cexp: applied integer literal"
    | (.litVec vty es, []) =>
        match pend with
        | _ :: _ => .error "cexp: applied vector literal"
        | [] => do
            let (d₁, pas) ← cargsD Δ dmap fuel Γ jΓ es d
            match Ty.flatten vty with
            | (.con "Vec", [nt, te]) =>
                match Ty.evalNat nt with
                | some k =>
                    if pas.length = k then
                      if teqAllD pas (List.replicate pas.length te) then do
                        let se ← Δ.sizeOf (fuel + 1) [] te
                        let (d₂, r) := catNFD d₁ ((pas.map (·.1)).map (·, se))
                        .ok (d₂, r, vty)
                      else .error "cexp: vector literal element-type mismatch"
                    else .error "cexp: vector literal length mismatch"
                | none => .error "cexp: vector literal at an open Vec length"
            | _ => .error "cexp: vector literal at a non-Vec type"
    | (.lam x b, args) => do
        let (d₁, pas) ← cargsD Δ dmap fuel Γ jΓ args d
        match pas ++ pend with
        | [] => .error "cexp: unsupported lambda expression"
        | nt :: rest => cexpJD Δ dmap fuel (Γ.insert x.uniq nt) [] b rest d₁
    | (.letE (.nonRec x rhs) body, args) => do
        let (d₁, r, t) ← cexpJD Δ dmap fuel Γ jΓ rhs [] d
        let (d₂, pas) ← cargsD Δ dmap fuel Γ jΓ args d₁
        cexpJD Δ dmap fuel (Γ.insert x.uniq (r, t)) jΓ body (pas ++ pend) d₂
    | (.letE (.join l ps jb) body, args) => do
        let (d₁, pas) ← cargsD Δ dmap fuel Γ jΓ args d
        cexpJD Δ dmap fuel Γ ((l.uniq, .mk ps Γ jΓ jb) :: jΓ) body (pas ++ pend) d₁
    | (.jump l es, []) => do
        let (d₁, pas) ← cargsD Δ dmap fuel Γ jΓ es d
        match jΓ.lookup l.uniq with
        | some (.mk ps Γc jc jb) =>
            if teqAllD pas (ps.map (·.sig.ty)) then
              cexpJD Δ dmap fuel (bindFieldsΓD ps pas Γc) jc jb pend d₁
            else .error s!"cexp: jump to {l.occ}: arity or argument-type mismatch"
        | none => .error s!"cexp: jump to an unbound join point {l.occ}#{l.uniq}"
    | (.cases resTy scrut binder alts, args) => do
        let (d₁, dn, dty) ← cexpJD Δ dmap fuel Γ jΓ scrut [] d
        let szT ← Δ.sizeOf (fuel + 1) [] dty
        let (d₂, pas) ← cargsD Δ dmap fuel Γ jΓ args d₁
        let pall := pas ++ pend
        let resTy' ← Ty.peel pall.length resTy
        let Γ' := Γ.insert binder.uniq (dn, dty)
        match alts with
        | .mk .default _ dbody :: rest => do
            let (d₃, dnf, dbt) ← cexpJD Δ dmap fuel Γ' jΓ dbody pall d₂
            if teqN Δ dbt resTy' then
              cchainJD Δ dmap fuel Γ' jΓ dty szT dn resTy' pall rest (some dnf) d₃
            else .error "cexp: default alternative result-type mismatch"
        | rest => cchainJD Δ dmap fuel Γ' jΓ dty szT dn resTy' pall rest none d₂
    | (_, _) => .error "cexp: unsupported expression (outside the Phase 4b fragment)"
termination_by fuel _ _ _ _ _ => (fuel, 0, 0)

end

/-- `cexpFull` on the store. -/
def cexpFullD (Δ : DEnv) (dmap : HashMap Int Defn) (fuel : Nat) (Γ : DGamma)
    (e : Exp) (d : Dag) : Except String (Dag × Nat × Ty) :=
  cexpJD Δ dmap fuel Γ [] e [] d


/-! ## The simulation: a successful DAG compilation certifies a
successful tree compilation whose result is the reading of the
returned index (`symExpDag_sim`'s pattern, lifted to `cexpJ`). -/

open Rwv.Hyle.BridgeDag (Mk)
open Rwv.Hyle.BridgeDag.Dag (mkLit_spec mkVar_spec rawCat_spec rawSlice_spec
  rawPrim1_spec rawPrim2_spec rawIte_spec self_mk read_ext node_of_lt lt_of_node
  read_eq widthOf_eq)

theorem mk_out {d d' : Dag} {r : Nat} {res : Dag × Nat} {nf : NF}
    (h : Mk d res nf) (heq : res = (d', r)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧ d'.read r = nf := by
  rw [heq] at h
  exact ⟨h.wf, h.ext, h.lt, h.read⟩

theorem mk_trans {d d₁ : Dag} {res : Dag × Nat} {nf : NF}
    (hext : d.Ext d₁) (h : Mk d₁ res nf) : Mk d res nf :=
  ⟨h.wf, hext.trans h.ext, h.lt, h.read⟩

/-- The environment simulation: pointwise readings plus range
(`BridgeDag.EnvSim`, keyed by uniques and carrying the types). -/
structure GSim (d : Dag) (ΓD : DGamma) (ΓS : HashMap Int (NF × Ty)) : Prop where
  reads : ∀ u, ΓS.get? u = (ΓD.get? u).map fun p => (d.read p.1, p.2)
  inRange : ∀ u p, ΓD.get? u = some p → p.1 < d.size

theorem GSim.mono {d d' : Dag} {ΓD : DGamma} {ΓS : HashMap Int (NF × Ty)}
    (h : GSim d ΓD ΓS) (hext : d.Ext d') : GSim d' ΓD ΓS := by
  refine ⟨fun u => ?_, fun u p hp => Nat.lt_of_lt_of_le (h.inRange u p hp) hext.size_le⟩
  rw [h.reads u]
  cases hu : ΓD.get? u with
  | none => rfl
  | some p =>
      simp only [Option.map_some]
      rw [read_ext hext p.1 (h.inRange u p hu)]

theorem GSim.insert {d : Dag} {ΓD : DGamma} {ΓS : HashMap Int (NF × Ty)}
    (h : GSim d ΓD ΓS) (u : Int) {i : Nat} {t : Ty} (hi : i < d.size) :
    GSim d (ΓD.insert u (i, t)) (ΓS.insert u (d.read i, t)) := by
  refine ⟨fun y => ?_, fun y p hp => ?_⟩
  · rw [get?_insert, get?_insert]
    by_cases hy : y = u
    · rw [if_pos hy, if_pos hy]
      rfl
    · rw [if_neg hy, if_neg hy]
      exact h.reads y
  · rw [get?_insert] at hp
    by_cases hy : y = u
    · rw [if_pos hy] at hp
      injection hp with hp
      subst hp
      exact hi
    · rw [if_neg hy] at hp
      exact h.inRange y p hp

theorem gsim_empty (d : Dag) : GSim d (∅ : DGamma) (∅ : HashMap Int (NF × Ty)) := by
  refine ⟨fun u => ?_, fun u p hp => ?_⟩
  · rw [HashMap.get?_eq_getElem?, HashMap.get?_eq_getElem?]
    simp
  · rw [HashMap.get?_eq_getElem?] at hp
    simp at hp

theorem gsim_bindFields {d : Dag} :
    ∀ (xs : List Id) {ntsD : List (Nat × Ty)} {ΓD : DGamma} {ΓS : HashMap Int (NF × Ty)},
      GSim d ΓD ΓS → (∀ p ∈ ntsD, p.1 < d.size) →
      GSim d (bindFieldsΓD xs ntsD ΓD)
        (bindFieldsΓ xs (ntsD.map fun p => (d.read p.1, p.2)) ΓS) := by
  intro xs
  induction xs with
  | nil =>
      intro ntsD ΓD ΓS h _
      simpa [bindFieldsΓD, bindFieldsΓ] using h
  | cons x xs ih =>
      intro ntsD ΓD ΓS h hin
      match ntsD with
      | [] => simpa [bindFieldsΓD, bindFieldsΓ] using h
      | nt :: nts =>
          have step : bindFieldsΓD (x :: xs) (nt :: nts) ΓD
              = (bindFieldsΓD xs nts ΓD).insert x.uniq nt := by
            simp only [bindFieldsΓD, List.zip_cons_cons, List.foldr_cons]
          have stepS : bindFieldsΓ (x :: xs) ((nt :: nts).map fun p => (d.read p.1, p.2)) ΓS
              = (bindFieldsΓ xs (nts.map fun p => (d.read p.1, p.2)) ΓS).insert x.uniq
                  (d.read nt.1, nt.2) := by
            simp only [List.map_cons, bindFieldsΓ, List.zip_cons_cons, List.foldr_cons]
          rw [step, stepS]
          exact GSim.insert (ih h (fun p hp => hin p (List.mem_cons_of_mem _ hp))) x.uniq
            (hin nt List.mem_cons_self)

/-- The join-closure simulation (lookup form, mirroring `JC`). -/
inductive JSimC (d : Dag) : CJoinD → CJoin → Prop where
  | mk {ps : List Id} {ΓD : DGamma} {ΓS : HashMap Int (NF × Ty)}
      {jsD : CJEnvD} {jsS : CJEnv} {body : Exp} :
      GSim d ΓD ΓS →
      (∀ l cj, jsD.lookup l = some cj → (jsS.lookup l).isSome) →
      (∀ l cj cjS, jsD.lookup l = some cj → jsS.lookup l = some cjS → JSimC d cj cjS) →
      JSimC d (.mk ps ΓD jsD body) (.mk ps ΓS jsS body)

def JSimEnv (d : Dag) (jΓD : CJEnvD) (jΓS : CJEnv) : Prop :=
  ∀ l cj, jΓD.lookup l = some cj → ∃ cjS, jΓS.lookup l = some cjS ∧ JSimC d cj cjS

theorem JSimC.mono {d d' : Dag} (hext : d.Ext d') {cj : CJoinD} {cjS : CJoin}
    (h : JSimC d cj cjS) : JSimC d' cj cjS := by
  induction h with
  | mk hG hcov hpt ih => exact JSimC.mk (hG.mono hext) hcov ih

theorem jsimEnv_mono {d d' : Dag} {jΓD : CJEnvD} {jΓS : CJEnv}
    (h : JSimEnv d jΓD jΓS) (hext : d.Ext d') : JSimEnv d' jΓD jΓS := by
  intro l cj hl
  obtain ⟨cjS, hlS, hc⟩ := h l cj hl
  exact ⟨cjS, hlS, hc.mono hext⟩

theorem jsimEnv_cons {d : Dag} {jΓD : CJEnvD} {jΓS : CJEnv}
    (h : JSimEnv d jΓD jΓS) {l : Int} {cj : CJoinD} {cjS : CJoin}
    (hcj : JSimC d cj cjS) : JSimEnv d ((l, cj) :: jΓD) ((l, cjS) :: jΓS) := by
  intro l' cj' hl
  rw [lookup_cons] at hl
  rw [lookup_cons]
  by_cases he : l' = l
  · rw [if_pos he] at hl
    rw [if_pos he]
    injection hl with hl
    subst hl
    exact ⟨cjS, rfl, hcj⟩
  · rw [if_neg he] at hl
    rw [if_neg he]
    exact h l' cj' hl

theorem jsimC_intro {d : Dag} {ps : List Id} {ΓD : DGamma}
    {ΓS : HashMap Int (NF × Ty)} {jsD : CJEnvD} {jsS : CJEnv} {body : Exp}
    (hG : GSim d ΓD ΓS) (hJ : JSimEnv d jsD jsS) :
    JSimC d (.mk ps ΓD jsD body) (.mk ps ΓS jsS body) := by
  refine JSimC.mk hG ?_ ?_
  · intro l cj h
    obtain ⟨cjS, hlS, _⟩ := hJ l cj h
    rw [hlS]
    rfl
  · intro l cj cjS h1 h2
    obtain ⟨cjS', hlS', hc⟩ := hJ l cj h1
    rw [hlS'] at h2
    injection h2 with h2
    subst h2
    exact hc

theorem jsimEnv_of_jsimC {d : Dag} {ps ps' : List Id} {ΓD : DGamma}
    {ΓS : HashMap Int (NF × Ty)} {jsD : CJEnvD} {jsS : CJEnv} {body body' : Exp}
    (h : JSimC d (.mk ps ΓD jsD body) (.mk ps' ΓS jsS body')) : JSimEnv d jsD jsS := by
  cases h with
  | mk hG hcov hpt =>
      intro l cj hl
      cases hjS : jsS.lookup l with
      | none =>
          have := hcov l cj hl
          rw [hjS] at this
          exact absurd this (by simp)
      | some cjS => exact ⟨cjS, rfl, hpt l cj cjS hl hjS⟩

theorem gsim_of_jsimC {d : Dag} {ps ps' : List Id} {ΓD : DGamma}
    {ΓS : HashMap Int (NF × Ty)} {jsD : CJEnvD} {jsS : CJEnv} {body body' : Exp}
    (h : JSimC d (.mk ps ΓD jsD body) (.mk ps' ΓS jsS body')) : GSim d ΓD ΓS := by
  cases h with
  | mk hG _ _ => exact hG

theorem jsimEnv_nil {d : Dag} {jΓS : CJEnv} : JSimEnv d [] jΓS := by
  intro l cj h
  simp [List.lookup] at h

/-! ### Constructor-spec lemmas for the mirrored helpers -/

theorem catListD_spec :
    ∀ (xs : List Nat) (d : Dag), d.WF → (∀ x ∈ xs, x < d.size) →
      Mk d (catListD d xs) (catList (xs.map d.read)) := by
  intro xs
  match xs with
  | [] =>
      intro d hwf _
      exact (mkLit_spec hwf _).cast rfl
  | [x] =>
      intro d hwf h
      exact (self_mk hwf (h x List.mem_cons_self)).cast rfl
  | x :: y :: rest =>
      intro d hwf h
      have hrest := catListD_spec (y :: rest) d hwf
        (fun z hz => h z (List.mem_cons_of_mem _ hz))
      rcases hres : catListD d (y :: rest) with ⟨d₁, r⟩
      obtain ⟨W₁, E₁, L₁, R₁⟩ := mk_out hrest hres
      have hx1 : x < d₁.size := Nat.lt_of_lt_of_le (h x List.mem_cons_self) E₁.size_le
      have hcat := rawCat_spec W₁ hx1 L₁
      have hgoal : catListD d (x :: y :: rest) = d₁.rawCat x r := by
        rw [catListD, hres]
      rw [hgoal]
      refine (mk_trans E₁ hcat).cast ?_
      rw [read_ext E₁ x (h x List.mem_cons_self), R₁]
      rfl

theorem filter_map_snd {d : Dag} (xs : List (Nat × Nat)) :
    ((xs.map fun p => ((d.read p.1 : NF), p.2)).filter fun p => p.2 != 0).map (·.1)
      = ((xs.filter fun p => p.2 != 0).map (·.1)).map d.read := by
  induction xs with
  | nil => rfl
  | cons x rest ih =>
      by_cases hx : (x.2 != 0) = true
      · simp only [List.map_cons, List.filter_cons, hx, if_pos, List.map_cons, ih]
      · simp only [List.map_cons, List.filter_cons]
        rw [if_neg (by simpa using hx), if_neg (by simpa using hx)]
        exact ih

theorem catNFD_spec {d : Dag} (hwf : d.WF) {xs : List (Nat × Nat)}
    (h : ∀ p ∈ xs, p.1 < d.size) :
    Mk d (catNFD d xs) (catNF (xs.map fun p => (d.read p.1, p.2))) := by
  rw [catNF, filter_map_snd]
  refine (catListD_spec _ d hwf ?_).cast rfl
  intro x hx
  obtain ⟨p, hp, hpx⟩ := List.mem_map.mp hx
  rw [← hpx]
  exact h p (List.mem_filter.mp hp).1

theorem sliceNFD_spec {d : Dag} (hwf : d.WF) {e : Nat} (off w : Nat)
    (he : e < d.size) : Mk d (sliceNFD d off w e) (sliceNF off w (d.read e)) := by
  rw [sliceNFD, sliceNF]
  by_cases hw : w = 0
  · rw [if_pos hw, if_pos hw]
    exact (mkLit_spec hwf _).cast rfl
  · rw [if_neg hw, if_neg hw]
    exact rawSlice_spec hwf off w he

theorem resizeNFD_spec {d : Dag} (hwf : d.WF) {m wa a : Nat} (ha : a < d.size) :
    Mk d (resizeNFD d m wa a) (resizeNF m wa (d.read a)) := by
  rw [resizeNFD, resizeNF]
  by_cases h1 : m = wa
  · rw [if_pos h1, if_pos h1]
    exact self_mk hwf ha
  · rw [if_neg h1, if_neg h1]
    by_cases h2 : wa < m
    · rw [if_pos h2, if_pos h2]
      exact rawPrim1_spec hwf ha rfl
    · rw [if_neg h2, if_neg h2]
      exact rawPrim1_spec hwf ha rfl

/-- The list-result contract for the walkers (readings paired with
kept annotations). -/
def MkLW (d : Dag) (res : Dag × List (Nat × Nat)) (nfs : List (NF × Nat)) : Prop :=
  res.1.WF ∧ d.Ext res.1 ∧ (∀ q ∈ res.2, q.1 < res.1.size) ∧
    res.2.map (fun q => ((res.1.read q.1 : NF), q.2)) = nfs

theorem slicesWD_spec {a : Nat} :
    ∀ (ps : List (Nat × Nat)) (d : Dag), d.WF → a < d.size →
      MkLW d (slicesWD d a ps) (ps.map fun q => (sliceNF q.1 q.2 (d.read a), q.2)) := by
  intro ps
  induction ps with
  | nil =>
      intro d hwf _
      exact ⟨hwf, Rwv.Hyle.BridgeDag.Dag.Ext.refl d, by simp [slicesWD], by simp [slicesWD]⟩
  | cons q rest ih =>
      intro d hwf ha
      show MkLW d (slicesWD d a (q :: rest)) _
      rw [MkLW, slicesWD]
      rcases hs : sliceNFD d q.1 q.2 a with ⟨d₁, s⟩
      obtain ⟨W₁, E₁, L₁, R₁⟩ := mk_out (sliceNFD_spec hwf q.1 q.2 ha) hs
      obtain ⟨W₂, E₂, L₂, R₂⟩ := ih d₁ W₁ (Nat.lt_of_lt_of_le ha E₁.size_le)
      rcases hr : slicesWD d₁ a rest with ⟨d₂, rs⟩
      rw [hr] at W₂ E₂ L₂ R₂
      simp only [hs, hr]
      refine ⟨W₂, E₁.trans E₂, ?_, ?_⟩
      · intro p hp
        rcases List.mem_cons.mp hp with hp | hp
        · subst hp
          exact Nat.lt_of_lt_of_le L₁ E₂.size_le
        · exact L₂ p hp
      · rw [List.map_cons, List.map_cons, R₂,
            read_ext E₂ s L₁, R₁,
            read_ext E₁ a ha]

theorem cpendVecD_spec {szV seI a : Nat} :
    ∀ (is : List Nat) (d : Dag), d.WF → a < d.size →
      (cpendVecD d szV seI a is).1.WF ∧ d.Ext (cpendVecD d szV seI a is).1 ∧
      (∀ q ∈ (cpendVecD d szV seI a is).2, q < (cpendVecD d szV seI a is).1.size) ∧
      (cpendVecD d szV seI a is).2.map (cpendVecD d szV seI a is).1.read
        = is.map fun i => sliceNF (szV - (i + 1) * seI) seI (d.read a) := by
  intro is
  induction is with
  | nil =>
      intro d hwf _
      exact ⟨hwf, Rwv.Hyle.BridgeDag.Dag.Ext.refl d, by simp [cpendVecD], by simp [cpendVecD]⟩
  | cons i rest ih =>
      intro d hwf ha
      show _ ∧ _ ∧ _ ∧ _
      rw [cpendVecD]
      rcases hs : sliceNFD d (szV - (i + 1) * seI) seI a with ⟨d₁, s⟩
      obtain ⟨W₁, E₁, L₁, R₁⟩ := mk_out (sliceNFD_spec hwf _ _ ha) hs
      obtain ⟨W₂, E₂, L₂, R₂⟩ := ih d₁ W₁ (Nat.lt_of_lt_of_le ha E₁.size_le)
      rcases hr : cpendVecD d₁ szV seI a rest with ⟨d₂, rs⟩
      rw [hr] at W₂ E₂ L₂ R₂
      simp only [hs, hr]
      refine ⟨W₂, E₁.trans E₂, ?_, ?_⟩
      · intro q hq
        rcases List.mem_cons.mp hq with hq | hq
        · subst hq
          exact Nat.lt_of_lt_of_le L₁ E₂.size_le
        · exact L₂ q hq
      · rw [List.map_cons, List.map_cons, R₂,
            read_ext E₂ s L₁, R₁,
            read_ext E₁ a ha]

theorem cpendGenD_spec {nElems : Nat} :
    ∀ (is : List Nat) (d : Dag), d.WF →
      (cpendGenD d nElems is).1.WF ∧ d.Ext (cpendGenD d nElems is).1 ∧
      (∀ q ∈ (cpendGenD d nElems is).2, q < (cpendGenD d nElems is).1.size) ∧
      (cpendGenD d nElems is).2.map (cpendGenD d nElems is).1.read
        = is.map fun i => (.lit ⟨nbits nElems, BitVec.ofNat (nbits nElems) i⟩ : NF) := by
  intro is
  induction is with
  | nil =>
      intro d hwf
      exact ⟨hwf, Rwv.Hyle.BridgeDag.Dag.Ext.refl d, by simp [cpendGenD], by simp [cpendGenD]⟩
  | cons i rest ih =>
      intro d hwf
      show _ ∧ _ ∧ _ ∧ _
      rw [cpendGenD]
      rcases hs : d.mkLit ⟨nbits nElems, BitVec.ofNat (nbits nElems) i⟩ with ⟨d₁, s⟩
      obtain ⟨W₁, E₁, L₁, R₁⟩ := mk_out (mkLit_spec hwf _) hs
      obtain ⟨W₂, E₂, L₂, R₂⟩ := ih d₁ W₁
      rcases hr : cpendGenD d₁ nElems rest with ⟨d₂, rs⟩
      rw [hr] at W₂ E₂ L₂ R₂
      simp only [hs, hr]
      refine ⟨W₂, E₁.trans E₂, ?_, ?_⟩
      · intro q hq
        rcases List.mem_cons.mp hq with hq | hq
        · subst hq
          exact Nat.lt_of_lt_of_le L₁ E₂.size_le
        · exact L₂ q hq
      · rw [List.map_cons, List.map_cons, R₂,
            read_ext E₂ s L₁, R₁]

theorem triple_eq {d₁ d₂ : Dag} {r₁ r₂ : Nat} {t₁ t₂ : Ty}
    (h : (Except.ok (d₁, r₁, t₁) : Except String (Dag × Nat × Ty)) = .ok (d₂, r₂, t₂)) :
    d₁ = d₂ ∧ r₁ = r₂ ∧ t₁ = t₂ := by
  injection h with h
  injection h with h1 h2
  injection h2 with h2 h3
  exact ⟨h1, h2, h3⟩

set_option maxHeartbeats 3200000 in
/-- The 4a row table's simulation: a successful `cprimD` run certifies
the `cprim` row over the read-back arguments. -/
theorem cprimD_sim {d : Dag} (hwf : d.WF) {pty : Ty} {b : Builtin}
    {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimD d pty b pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprim pty b (pas.map fun p => ((d.read p.1 : NF), p.2)) = .ok (d'.read r, ty) := by
  have harith : ∀ (op : Op), Rwv.Hyle.Bridge.opArity op = 2 →
      ∀ {a ta b' tb}, pas = [(a, ta), (b', tb)] →
      (do
        let m ← vecBoolLen "arith row" (Ty.flattenArrow pty).2
        let wa ← vecBoolLen "arith row" ta
        if wa = m then
          let (d₁, r) := d.rawPrim2 op a b'
          (.ok (d₁, r, (Ty.flattenArrow pty).2) : Except String (Dag × Nat × Ty))
        else .error "cexp: arith row width mismatch") = .ok (d', r, ty) →
      d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
        arithRow op (Ty.flattenArrow pty).2 ta (d.read a) (d.read b')
          = .ok (d'.read r, ty) := by
    clear h
    intro op hop a ta b' tb hps h
    subst hps
    obtain ⟨m, hm, h⟩ := except_bind_eq_ok h
    obtain ⟨wa, hwa, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hcond
    rcases hp : d.rawPrim2 op a b' with ⟨d₁, r₁⟩
    rw [hp] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out (rawPrim2_spec hwf (show a < d.size from hpas (a, ta) (by simp)) (show b' < d.size from hpas (b', tb) (by simp)) hop) hp
    refine ⟨W, E, L, ?_⟩
    rw [arithRow, hm, except_bind_ok, hwa, except_bind_ok, if_pos hcond,
        except_pure_def, R]
  have hcmp : ∀ (op : Op), Rwv.Hyle.Bridge.opArity op = 2 →
      ∀ {a ta b' tb}, pas = [(a, ta), (b', tb)] →
      (if isBoolT (Ty.flattenArrow pty).2 then
        let (d₁, r) := d.rawPrim2 op a b'
        (.ok (d₁, r, (Ty.flattenArrow pty).2) : Except String (Dag × Nat × Ty))
      else .error "cexp: comparison row at a non-Bool result type") = .ok (d', r, ty) →
      d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
        cmpRow op (Ty.flattenArrow pty).2 (d.read a) (d.read b') = .ok (d'.read r, ty) := by
    clear h
    intro op hop a ta b' tb hps h
    subst hps
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hcond
    rcases hp : d.rawPrim2 op a b' with ⟨d₁, r₁⟩
    rw [hp] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out (rawPrim2_spec hwf (show a < d.size from hpas (a, ta) (by simp)) (show b' < d.size from hpas (b', tb) (by simp)) hop) hp
    refine ⟨W, E, L, ?_⟩
    rw [cmpRow, if_pos hcond, except_pure_def, R]
  have hred : ∀ (op : Op) (neg : Bool), Rwv.Hyle.Bridge.opArity op = 1 →
      ∀ {a ta}, pas = [(a, ta)] →
      (if isBoolT (Ty.flattenArrow pty).2 then
        if neg then
          let (d₁, r) := d.rawPrim1 op a
          let (d₂, r₂) := d₁.rawPrim1 .not r
          (.ok (d₂, r₂, (Ty.flattenArrow pty).2) : Except String (Dag × Nat × Ty))
        else
          let (d₁, r) := d.rawPrim1 op a
          .ok (d₁, r, (Ty.flattenArrow pty).2)
      else .error "cexp: reduction row at a non-Bool result type") = .ok (d', r, ty) →
      d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
        redRow op neg (Ty.flattenArrow pty).2 (d.read a) = .ok (d'.read r, ty) := by
    clear h
    intro op neg hop a ta hps h
    subst hps
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hcond
    cases neg with
    | false =>
        simp only [Bool.false_eq_true, if_false] at h
        rcases hp : d.rawPrim1 op a with ⟨d₁, r₁⟩
        rw [hp] at h
        obtain ⟨hd, hr, hty⟩ := triple_eq h
        subst hd; subst hr; subst hty
        obtain ⟨W, E, L, R⟩ := mk_out (rawPrim1_spec hwf (show a < d.size from hpas (a, ta) (by simp)) hop) hp
        refine ⟨W, E, L, ?_⟩
        rw [redRow, if_pos hcond]
        show Except.ok ((NF.prim1 op (d.read a) : NF), (Ty.flattenArrow pty).2) = _
        rw [R]
    | true =>
        simp only [if_true] at h
        rcases hp : d.rawPrim1 op a with ⟨d₁, r₁⟩
        rw [hp] at h
        rcases hp2 : d₁.rawPrim1 .not r₁ with ⟨d₂, r₂⟩
        rw [hp2] at h
        obtain ⟨hd, hr, hty⟩ := triple_eq h
        subst hd; subst hr; subst hty
        obtain ⟨W₁, E₁, L₁, R₁⟩ := mk_out (rawPrim1_spec hwf (show a < d.size from hpas (a, ta) (by simp)) hop) hp
        obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (rawPrim1_spec W₁ L₁ rfl) hp2
        refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
        rw [redRow, if_pos hcond]
        show Except.ok ((NF.prim1 .not (NF.prim1 op (d.read a)) : NF), (Ty.flattenArrow pty).2) = _
        rw [R₂, R₁]
  simp only [cprimD] at h
  split at h
  · -- .bits
    rename_i a ta
    obtain ⟨k, hk, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hcond
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    refine ⟨hwf, Rwv.Hyle.BridgeDag.Dag.Ext.refl d, hpas (a, ta) (by simp), ?_⟩
    show cprim pty .bits [((d.read a : NF), ta)] = _
    rw [cprim, hk, except_bind_ok, if_pos hcond, except_pure_def]
  · -- .resize
    rename_i a ta
    obtain ⟨m, hm, h⟩ := except_bind_eq_ok h
    obtain ⟨wa, hwa, h⟩ := except_bind_eq_ok h
    have htree : ∀ (out : NF),
        d'.read r = out →
        (if m = wa then (Except.ok ((d.read a : NF), (Ty.flattenArrow pty).2) :
              Except String (NF × Ty))
         else if wa < m then .ok (.prim1 (.zext m) (d.read a), (Ty.flattenArrow pty).2)
         else .ok (.prim1 (.trunc m) (d.read a), (Ty.flattenArrow pty).2)) = .ok (out, ty) →
        cprim pty .resize [((d.read a : NF), ta)] = .ok (d'.read r, ty) := by
      intro out hout htr
      show (do
        let m ← vecBoolLen "rwPrimResize" (Ty.flattenArrow pty).2
        let wa ← vecBoolLen "rwPrimResize" ta
        if m = wa then pure ((d.read a : NF), (Ty.flattenArrow pty).2)
        else if wa < m then pure (.prim1 (.zext m) (d.read a), (Ty.flattenArrow pty).2)
        else pure (.prim1 (.trunc m) (d.read a), (Ty.flattenArrow pty).2)) = _
      rw [hm, except_bind_ok, hwa, except_bind_ok]
      rw [hout]
      exact htr
    split at h
    · -- m = wa: identity
      rename_i hcond
      obtain ⟨hd, hr, hty⟩ := triple_eq h
      subst hd; subst hr; subst hty
      refine ⟨hwf, Rwv.Hyle.BridgeDag.Dag.Ext.refl d, hpas (a, ta) (by simp), ?_⟩
      exact htree _ rfl (by rw [if_pos hcond])
    · split at h
      · -- zext
        rename_i hne hlt
        rcases hp : d.rawPrim1 (.zext m) a with ⟨d₁, r₁⟩
        rw [hp] at h
        obtain ⟨hd, hr, hty⟩ := triple_eq h
        subst hd; subst hr; subst hty
        obtain ⟨W, E, L, R⟩ := mk_out (rawPrim1_spec hwf (show a < d.size from hpas (a, ta) (by simp)) rfl) hp
        refine ⟨W, E, L, ?_⟩
        exact htree _ R (by rw [if_neg hne, if_pos hlt])
      · -- trunc
        rename_i hne hnlt
        rcases hp : d.rawPrim1 (.trunc m) a with ⟨d₁, r₁⟩
        rw [hp] at h
        obtain ⟨hd, hr, hty⟩ := triple_eq h
        subst hd; subst hr; subst hty
        obtain ⟨W, E, L, R⟩ := mk_out (rawPrim1_spec hwf (show a < d.size from hpas (a, ta) (by simp)) rfl) hp
        refine ⟨W, E, L, ?_⟩
        exact htree _ R (by rw [if_neg hne, if_neg hnlt])
  · exact harith .add  rfl rfl h
  · exact harith .sub  rfl rfl h
  · exact harith .mul  rfl rfl h
  · exact harith .udiv rfl rfl h
  · exact harith .umod rfl rfl h
  · exact harith .pow  rfl rfl h
  · exact harith .and  rfl rfl h
  · exact harith .or   rfl rfl h
  · exact harith .xor  rfl rfl h
  · -- .xnor
    rename_i a ta b' tb
    obtain ⟨m, hm, h⟩ := except_bind_eq_ok h
    obtain ⟨wa, hwa, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hcond
    rcases hp : d.rawPrim2 .xor a b' with ⟨d₁, r₁⟩
    rw [hp] at h
    rcases hp2 : d₁.rawPrim1 .not r₁ with ⟨d₂, r₂⟩
    rw [hp2] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W₁, E₁, L₁, R₁⟩ := mk_out (rawPrim2_spec hwf (show a < d.size from hpas (a, ta) (by simp)) (show b' < d.size from hpas (b', tb) (by simp)) rfl) hp
    obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (rawPrim1_spec W₁ L₁ rfl) hp2
    refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
    show (do
      let m ← vecBoolLen "rwPrimXNor" (Ty.flattenArrow pty).2
      let wa ← vecBoolLen "rwPrimXNor" ta
      if wa = m then
        pure ((.prim1 .not (.prim2 .xor (d.read a) (d.read b')) : NF), (Ty.flattenArrow pty).2)
      else .error "rwPrimXNor: width mismatch") = _
    rw [hm, except_bind_ok, hwa, except_bind_ok, if_pos hcond, except_pure_def, R₂, R₁]
  · -- .not
    rename_i a ta
    obtain ⟨m, hm, h⟩ := except_bind_eq_ok h
    obtain ⟨wa, hwa, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hcond
    rcases hp : d.rawPrim1 .not a with ⟨d₁, r₁⟩
    rw [hp] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out (rawPrim1_spec hwf (show a < d.size from hpas (a, ta) (by simp)) rfl) hp
    refine ⟨W, E, L, ?_⟩
    show (do
      let m ← vecBoolLen "rwPrimNot" (Ty.flattenArrow pty).2
      let wa ← vecBoolLen "rwPrimNot" ta
      if wa = m then pure ((.prim1 .not (d.read a) : NF), (Ty.flattenArrow pty).2)
      else .error "rwPrimNot: width mismatch") = _
    rw [hm, except_bind_ok, hwa, except_bind_ok, if_pos hcond, except_pure_def, R]
  · exact harith .shl  rfl rfl h
  · exact harith .lshr rfl rfl h
  · exact harith .ashr rfl rfl h
  · exact hcmp .eq  rfl rfl h
  · exact hcmp .ugt rfl rfl h
  · exact hcmp .uge rfl rfl h
  · exact hcmp .ult rfl rfl h
  · exact hcmp .ule rfl rfl h
  · -- .lAnd
    rename_i a ta b' tb
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hcond
    rcases hp1 : d.rawPrim1 .redor a with ⟨d₁, ra⟩
    rw [hp1] at h
    rcases hp2 : d₁.rawPrim1 .redor b' with ⟨d₂, rb⟩
    rw [hp2] at h
    rcases hp3 : d₂.rawPrim2 .and ra rb with ⟨d₃, r₃⟩
    rw [hp3] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W₁, E₁, L₁, R₁⟩ := mk_out (rawPrim1_spec hwf (show a < d.size from hpas (a, ta) (by simp)) rfl) hp1
    obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (rawPrim1_spec W₁
      (Nat.lt_of_lt_of_le (show b' < d.size from hpas (b', tb) (by simp)) E₁.size_le) rfl) hp2
    obtain ⟨W₃, E₃, L₃, R₃⟩ := mk_out (rawPrim2_spec W₂
      (Nat.lt_of_lt_of_le L₁ E₂.size_le) L₂ rfl) hp3
    refine ⟨W₃, (E₁.trans E₂).trans E₃, L₃, ?_⟩
    show cmpRow .and (Ty.flattenArrow pty).2
      (.prim1 .redor (d.read a)) (.prim1 .redor (d.read b')) = _
    rw [cmpRow, if_pos hcond, except_pure_def, R₃,
        read_ext E₂ ra L₁, R₁, R₂,
        read_ext E₁ b' (show b' < d.size from hpas (b', tb) (by simp))]
  · -- .lOr
    rename_i a ta b' tb
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hcond
    rcases hp1 : d.rawPrim1 .redor a with ⟨d₁, ra⟩
    rw [hp1] at h
    rcases hp2 : d₁.rawPrim1 .redor b' with ⟨d₂, rb⟩
    rw [hp2] at h
    rcases hp3 : d₂.rawPrim2 .or ra rb with ⟨d₃, r₃⟩
    rw [hp3] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W₁, E₁, L₁, R₁⟩ := mk_out (rawPrim1_spec hwf (show a < d.size from hpas (a, ta) (by simp)) rfl) hp1
    obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (rawPrim1_spec W₁
      (Nat.lt_of_lt_of_le (show b' < d.size from hpas (b', tb) (by simp)) E₁.size_le) rfl) hp2
    obtain ⟨W₃, E₃, L₃, R₃⟩ := mk_out (rawPrim2_spec W₂
      (Nat.lt_of_lt_of_le L₁ E₂.size_le) L₂ rfl) hp3
    refine ⟨W₃, (E₁.trans E₂).trans E₃, L₃, ?_⟩
    show cmpRow .or (Ty.flattenArrow pty).2
      (.prim1 .redor (d.read a)) (.prim1 .redor (d.read b')) = _
    rw [cmpRow, if_pos hcond, except_pure_def, R₃,
        read_ext E₂ ra L₁, R₁, R₂,
        read_ext E₁ b' (show b' < d.size from hpas (b', tb) (by simp))]
  · exact hred .redor  true  rfl rfl h
  · exact hred .redand false rfl rfl h
  · exact hred .redand true  rfl rfl h
  · exact hred .redor  false rfl rfl h
  · exact hred .redor  true  rfl rfl h
  · exact hred .redxor false rfl rfl h
  · exact hred .redxor true  rfl rfl h
  · -- .msBit
    rename_i a ta
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hcond
    obtain ⟨wa, hwa, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hge
    rcases hp : d.rawSlice (wa - 1) 1 a with ⟨d₁, r₁⟩
    rw [hp] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out (rawSlice_spec hwf (wa - 1) 1 (show a < d.size from hpas (a, ta) (by simp))) hp
    refine ⟨W, E, L, ?_⟩
    show (do
      if isBoolT (Ty.flattenArrow pty).2 then do
        let wa ← vecBoolLen "rwPrimMSBit" ta
        if wa ≥ 1 then pure ((.slice (wa - 1) 1 (d.read a) : NF), (Ty.flattenArrow pty).2)
        else .error "rwPrimMSBit: zero-width argument"
      else .error "rwPrimMSBit: non-Bool result type") = _
    rw [if_pos hcond, hwa, except_bind_ok, if_pos hge, except_pure_def, R]
  · -- catch-all
    exact error_ne_ok h

theorem clitIntD_sim {d : Dag} (hwf : d.WF) {ty : Ty} {n : Int}
    {d' : Dag} {r : Nat} {rty : Ty}
    (h : clitIntD d ty n = .ok (d', r, rty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧ clitInt ty n = .ok (d'.read r, rty) := by
  rw [clitIntD] at h
  obtain ⟨nt, hnt, h⟩ := except_bind_eq_ok h
  obtain ⟨nf, rty'⟩ := nt
  dsimp only at h
  split at h
  rotate_left
  · exact error_ne_ok h
  rename_i v
  injection h with h
  injection h with h1 h2
  injection h2 with h2 h3
  rcases hlit : d.mkLit v with ⟨dl, rl⟩
  rw [hlit] at h1 h2
  subst h1
  subst h2
  subst h3
  obtain ⟨W, E, L, R⟩ := mk_out (mkLit_spec hwf v) hlit
  refine ⟨W, E, L, ?_⟩
  rw [hnt, R]


/-! ### The cexpJD simulation groundwork (Phase 4d, the landing) -/

/-- Readings of in-range pend pairs are extension-stable. -/
theorem pend_read_ext {d d' : Dag} (hext : d.Ext d') {ps : List (Nat × Ty)}
    (h : ∀ p ∈ ps, p.1 < d.size) :
    (ps.map fun p => ((d'.read p.1 : NF), p.2)) = ps.map fun p => ((d.read p.1 : NF), p.2) :=
  List.map_congr_left fun p hp => by rw [read_ext hext p.1 (h p hp)]

/-- `teqAllD` is `teqAll` of the read-back pairs (only types are
consulted). -/
theorem teqAllD_eq (d : Dag) :
    ∀ (ps : List (Nat × Ty)) (us : List Ty),
      teqAllD ps us = teqAll (ps.map fun p => ((d.read p.1 : NF), p.2)) us := by
  intro ps
  induction ps with
  | nil => intro us; cases us <;> rfl
  | cons p rest ih =>
      intro us
      cases us with
      | nil => rfl
      | cons u urest =>
          obtain ⟨i, t⟩ := p
          show (teq t u && teqAllD rest urest) = (teq t u && teqAll _ urest)
          rw [ih urest]

/-- `teqAllND` is `teqAllN` of the read-back pairs (only types are
consulted). -/
theorem teqAllND_eq {Δ : DEnv} (d : Dag) :
    ∀ (ps : List (Nat × Ty)) (us : List Ty),
      teqAllND Δ ps us = teqAllN Δ (ps.map fun p => ((d.read p.1 : NF), p.2)) us := by
  intro ps
  induction ps with
  | nil => intro us; cases us <;> rfl
  | cons p rest ih =>
      intro us
      cases us with
      | nil => rfl
      | cons u urest =>
          obtain ⟨i, t⟩ := p
          show (teqN Δ t u && teqAllND Δ rest urest) = (teqN Δ t u && teqAllN Δ _ urest)
          rw [ih urest]

/-- The simulation contract at one fuel (the induction hypothesis's
shape): a successful `cexpJD` run certifies a successful `cexpJ` run
whose normal form is the reading of the returned index, at read-back
environments and pending arguments. -/
abbrev DSimAt (Δ : DEnv) (dmap : HashMap Int Defn) (fuel : Nat) : Prop :=
  ∀ (ΓD : DGamma) (jΓD : CJEnvD) (e : Exp) (pendD : List (Nat × Ty)) (d d' : Dag)
    (r : Nat) (ty : Ty) (ΓS : HashMap Int (NF × Ty)) (jΓS : CJEnv),
    d.WF → GSim d ΓD ΓS → JSimEnv d jΓD jΓS → (∀ p ∈ pendD, p.1 < d.size) →
    cexpJD Δ dmap fuel ΓD jΓD e pendD d = .ok (d', r, ty) →
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cexpJ Δ dmap fuel ΓS jΓS e (pendD.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty)

/-- The argument-spine leg. -/
theorem cargsD_sim {Δ : DEnv} {dmap : HashMap Int Defn} {fuel : Nat}
    {ΓD : DGamma} {jΓD : CJEnvD} {ΓS : HashMap Int (NF × Ty)} {jΓS : CJEnv}
    (IH : DSimAt Δ dmap fuel) :
    ∀ (es : List Exp) (d d₁ : Dag) (pas : List (Nat × Ty)),
      d.WF → GSim d ΓD ΓS → JSimEnv d jΓD jΓS →
      cargsD Δ dmap fuel ΓD jΓD es d = .ok (d₁, pas) →
      d₁.WF ∧ d.Ext d₁ ∧ (∀ p ∈ pas, p.1 < d₁.size) ∧
        es.mapM (fun e => cexpJ Δ dmap fuel ΓS jΓS e [])
          = .ok (pas.map fun p => ((d₁.read p.1 : NF), p.2)) := by
  intro es
  induction es with
  | nil =>
      intro d d₁ pas hwf hG hJ h
      rw [cargsD] at h
      injection h with h
      injection h with h1 h2
      subst h1; subst h2
      exact ⟨hwf, Rwv.Hyle.BridgeDag.Dag.Ext.refl d, by simp, rfl⟩
  | cons e es ihe =>
      intro d d₁ pas hwf hG hJ h
      rw [cargsD] at h
      obtain ⟨drt, hd1, h⟩ := except_bind_eq_ok h
      obtain ⟨da, ra, ta⟩ := drt
      dsimp only at h
      obtain ⟨drs, hd2, h⟩ := except_bind_eq_ok h
      obtain ⟨db, rs⟩ := drs
      dsimp only at h
      injection h with h
      injection h with h1 h2
      subst h1; subst h2
      obtain ⟨W₁, E₁, L₁, S₁⟩ := IH ΓD jΓD e [] d da ra ta ΓS jΓS hwf hG hJ
        (by simp) hd1
      simp only [List.map_nil] at S₁
      obtain ⟨W₂, E₂, L₂, S₂⟩ := ihe da db rs W₁ (hG.mono E₁) (jsimEnv_mono hJ E₁) hd2
      refine ⟨W₂, E₁.trans E₂, ?_, ?_⟩
      · intro p hp
        rcases List.mem_cons.mp hp with hp | hp
        · subst hp
          exact Nat.lt_of_lt_of_le L₁ E₂.size_le
        · exact L₂ p hp
      · rw [List.mapM_cons, S₁, except_bind_ok, S₂, except_bind_ok, except_pure_def, List.map_cons,
            read_ext E₂ ra L₁]


/-- The per-element application leg (VecMap/VecGenerate): the tree
side's `List.range` mapM, from pre-built pend heads whose readings are
the per-index tree pends. -/
theorem celemsD_sim {Δ : DEnv} {dmap : HashMap Int Defn} {fuel : Nat}
    {ΓD : DGamma} {jΓD : CJEnvD} {ΓS : HashMap Int (NF × Ty)} {jΓS : CJEnv}
    {fE : Exp} {teI teO : Ty} {seO : Nat} {pf : Nat → NF} {emsg : String}
    (IH : DSimAt Δ dmap fuel) :
    ∀ (is : List Nat) (pends : List Nat) (d d₁ : Dag) (ps : List (Nat × Nat)),
      d.WF → GSim d ΓD ΓS → JSimEnv d jΓD jΓS → (∀ q ∈ pends, q < d.size) →
      pends.length = is.length →
      (∀ j (h1 : j < pends.length) (h2 : j < is.length),
        d.read (pends[j]'h1) = pf (is[j]'h2)) →
      celemsD Δ dmap fuel ΓD jΓD fE teI teO seO pends d = .ok (d₁, ps) →
      d₁.WF ∧ d.Ext d₁ ∧ (∀ q ∈ ps, q.1 < d₁.size) ∧
        is.mapM (fun i => do
            let (ei, ti) ← cexpJ Δ dmap fuel ΓS jΓS fE [(pf i, teI)]
            if teq ti teO then (.ok (ei, seO) : Except String (NF × Nat))
            else .error emsg)
          = .ok (ps.map fun q => ((d₁.read q.1 : NF), q.2)) := by
  intro is
  induction is with
  | nil =>
      intro pends d d₁ ps hwf hG hJ hrange hlen hreads h
      have hp : pends = [] := List.length_eq_zero_iff.mp (by simpa using hlen)
      subst hp
      rw [celemsD] at h
      injection h with h
      injection h with h1 h2
      subst h1; subst h2
      exact ⟨hwf, Rwv.Hyle.BridgeDag.Dag.Ext.refl d, by simp, rfl⟩
  | cons i is ihe =>
      intro pends d d₁ ps hwf hG hJ hrange hlen hreads h
      match pends, hlen with
      | p :: prest, hlen =>
      rw [celemsD] at h
      obtain ⟨det, hde, h⟩ := except_bind_eq_ok h
      obtain ⟨da, ei, ti⟩ := det
      dsimp only at h
      split at h
      rotate_left
      · exact error_ne_ok h
      rename_i hteq
      obtain ⟨drs, hdr, h⟩ := except_bind_eq_ok h
      obtain ⟨db, rest⟩ := drs
      dsimp only at h
      injection h with h
      injection h with h1 h2
      subst h1; subst h2
      obtain ⟨W₁, E₁, L₁, S₁⟩ := IH ΓD jΓD fE [(p, teI)] d da ei ti ΓS jΓS hwf hG hJ
        (by
          intro q hq
          obtain rfl := List.mem_singleton.mp hq
          exact hrange p List.mem_cons_self) hde
      obtain ⟨W₂, E₂, L₂, S₂⟩ := ihe prest da db rest W₁ (hG.mono E₁) (jsimEnv_mono hJ E₁)
        (fun q hq => Nat.lt_of_lt_of_le (hrange q (List.mem_cons_of_mem _ hq)) E₁.size_le)
        (by simpa using hlen)
        (by
          intro j h1 h2
          rw [read_ext E₁ _ (hrange _ (List.mem_cons_of_mem _ (List.getElem_mem _)))]
          have := hreads (j + 1) (by simpa using h1) (by simpa using h2)
          simpa using this)
        hdr
      have hp0 : d.read p = pf i := by
        have := hreads 0 (by simp) (by simp)
        simpa using this
      refine ⟨W₂, E₁.trans E₂, ?_, ?_⟩
      · intro q hq
        rcases List.mem_cons.mp hq with hq | hq
        · subst hq
          exact Nat.lt_of_lt_of_le L₁ E₂.size_le
        · exact L₂ q hq
      · rw [List.mapM_cons]
        rw [show [((pf i : NF), teI)] = [(p, teI)].map fun p => ((d.read p.1 : NF), p.2) by
          simp [hp0]]
        rw [S₁, except_bind_ok]
        dsimp only
        rw [if_pos hteq, except_bind_ok, S₂, except_bind_ok, except_pure_def, List.map_cons,
            read_ext E₂ ei L₁]

/-- The record slices of one terminator alternative (the `foldr` in
`cAltJD`/`goAlt1D`). -/
theorem altSlices_spec {dn : Nat} :
    ∀ (ps : List (Nat × Nat)) (d : Dag), d.WF → dn < d.size →
      (ps.foldr (fun (q : Nat × Nat) (acc : Dag × List Nat) =>
          let (d', r) := sliceNFD acc.1 q.2 q.1 dn
          (d', r :: acc.2)) (d, ([] : List Nat))).1.WF ∧
      d.Ext (ps.foldr (fun q acc =>
          let (d', r) := sliceNFD acc.1 q.2 q.1 dn
          (d', r :: acc.2)) (d, ([] : List Nat))).1 ∧
      (∀ q ∈ (ps.foldr (fun q acc =>
          let (d', r) := sliceNFD acc.1 q.2 q.1 dn
          (d', r :: acc.2)) (d, ([] : List Nat))).2,
        q < (ps.foldr (fun q acc =>
          let (d', r) := sliceNFD acc.1 q.2 q.1 dn
          (d', r :: acc.2)) (d, ([] : List Nat))).1.size) ∧
      (ps.foldr (fun q acc =>
          let (d', r) := sliceNFD acc.1 q.2 q.1 dn
          (d', r :: acc.2)) (d, ([] : List Nat))).2.map
        (ps.foldr (fun q acc =>
          let (d', r) := sliceNFD acc.1 q.2 q.1 dn
          (d', r :: acc.2)) (d, ([] : List Nat))).1.read
        = ps.map fun q => sliceNF q.2 q.1 (d.read dn) := by
  intro ps
  induction ps with
  | nil =>
      intro d hwf hdn
      exact ⟨hwf, Rwv.Hyle.BridgeDag.Dag.Ext.refl d, by simp, by simp⟩
  | cons q rest ih =>
      intro d hwf hdn
      obtain ⟨W₁, E₁, L₁, R₁⟩ := ih d hwf hdn
      rcases hfold : rest.foldr (fun q acc =>
          let (d', r) := sliceNFD acc.1 q.2 q.1 dn
          (d', r :: acc.2)) (d, ([] : List Nat)) with ⟨d₁, acc⟩
      rw [hfold] at W₁ E₁ L₁ R₁
      dsimp only at W₁ E₁ L₁ R₁
      rcases hs : sliceNFD d₁ q.2 q.1 dn with ⟨d₂, s⟩
      obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out
        (sliceNFD_spec W₁ q.2 q.1 (Nat.lt_of_lt_of_le hdn E₁.size_le)) hs
      have hstep : (q :: rest).foldr (fun q acc =>
          let (d', r) := sliceNFD acc.1 q.2 q.1 dn
          (d', r :: acc.2)) (d, ([] : List Nat)) = (d₂, s :: acc) := by
        rw [List.foldr_cons, hfold]
        dsimp only
        rw [hs]
      rw [hstep]
      dsimp only
      refine ⟨W₂, E₁.trans E₂, ?_, ?_⟩
      · intro x hx
        rcases List.mem_cons.mp hx with hx | hx
        · subst hx
          exact L₂
        · exact Nat.lt_of_lt_of_le (L₁ x hx) E₂.size_le
      · rw [List.map_cons, R₂, List.map_cons]
        congr 1
        · rw [read_ext E₁ dn hdn]
        · rw [← R₁]
          exact List.map_congr_left fun x hx => read_ext E₂ x (L₁ x hx)

/-- One alternative's simulation (`cAltJ`'s mirror), parameterized by
the master induction hypothesis. All relations are stated at the entry
store `d`. -/
theorem cAltJD_sim {Δ : DEnv} {dmap : HashMap Int Defn} {fuel : Nat}
    (IH : DSimAt Δ dmap fuel) {Γ'D : DGamma} {jΓD : CJEnvD}
    {Γ'S : HashMap Int (NF × Ty)} {jΓS : CJEnv} {dty : Ty} {szT : Nat} {dnD : Nat}
    {resTy : Ty} {pendD : List (Nat × Ty)} :
    ∀ (alt : Alt) (maccD : Option Nat) (maccS : Option NF) (d d' : Dag) (bnf : Nat),
      d.WF → GSim d Γ'D Γ'S → JSimEnv d jΓD jΓS → (∀ p ∈ pendD, p.1 < d.size) →
      dnD < d.size →
      (match maccD, maccS with
       | some aD, some aS => aD < d.size ∧ d.read aD = aS
       | none, none => True
       | _, _ => False) →
      cAltJD Δ dmap fuel Γ'D jΓD dty szT dnD resTy pendD alt maccD d = .ok (d', bnf) →
      d'.WF ∧ d.Ext d' ∧ bnf < d'.size ∧
        cAltJ Δ dmap fuel Γ'S jΓS dty szT (d.read dnD) resTy
            (pendD.map fun p => ((d.read p.1 : NF), p.2)) alt maccS
          = .ok (d'.read bnf) := by
  intro alt maccD maccS d d' bnf hwf hG hJ hpend hdn hmacc h
  obtain ⟨con, xs, body⟩ := alt
  cases con with
  | default => rw [cAltJD] at h; exact error_ne_ok h
  | dataAlt cn =>
      rw [cAltJD] at h
      split at h
      rotate_left
      · exact error_ne_ok h
      rename_i hctb
      obtain ⟨tg, htag, h⟩ := except_bind_eq_ok h
      obtain ⟨tag, w⟩ := tg
      cases hcs : Δ.ctorSig.get? cn with
      | none => rw [hcs] at h; dsimp only at h; exact error_ne_ok h
      | some sig =>
      rw [hcs] at h
      dsimp only at h
      obtain ⟨sub, hsub, h⟩ := except_bind_eq_ok h
      split at h
      rotate_left
      · exact error_ne_ok h
      rename_i hxlen
      obtain ⟨szXs, hszXs, h⟩ := except_bind_eq_ok h
      split at h
      rotate_left
      · exact error_ne_ok h
      rename_i hwle
      -- the field slices
      rcases hfold : (szXs.zip ((List.range szXs.length).map fun i =>
          (szXs.drop (i + 1)).sum)).foldr
          (fun (q : Nat × Nat) (acc : Dag × List Nat) =>
            let (d', r) := sliceNFD acc.1 q.2 q.1 dnD
            (d', r :: acc.2)) (d, ([] : List Nat)) with ⟨d₁, slices⟩
      rw [hfold] at h
      obtain ⟨W₁, E₁, L₁, R₁⟩ := altSlices_spec _ d hwf hdn
      rw [hfold] at W₁ E₁ L₁ R₁
      obtain ⟨det, hbody, h⟩ := except_bind_eq_ok h
      obtain ⟨d₂, bnf', bty⟩ := det
      dsimp only at h
      split at h
      rotate_left
      · exact error_ne_ok h
      rename_i hbteq
      -- the compiled body through the IH
      have hGb : GSim d₁ (bindFieldsΓD xs ((slices.zip ((Ty.flattenArrow sig.ty).1.map
          (DEnv.substTv sub))) ) Γ'D)
          (bindFieldsΓ xs (((slices.zip ((Ty.flattenArrow sig.ty).1.map
            (DEnv.substTv sub))).map fun p => ((d₁.read p.1 : NF), p.2))) Γ'S) :=
        gsim_bindFields xs (hG.mono E₁) (by
          intro p hp
          exact L₁ p.1 (List.of_mem_zip hp).1)
      obtain ⟨W₂, E₂, L₂, S₂⟩ := IH _ jΓD body pendD d₁ d₂ bnf' bty _ jΓS
        W₁ hGb (jsimEnv_mono hJ E₁)
        (fun p hp => Nat.lt_of_lt_of_le (hpend p hp) E₁.size_le) hbody
      -- the tree-side slice pairs coincide
      have hzipS : ((slices.zip ((Ty.flattenArrow sig.ty).1.map
          (DEnv.substTv sub))).map fun p => ((d₁.read p.1 : NF), p.2))
          = ((szXs.zip ((List.range szXs.length).map fun i =>
              (szXs.drop (i + 1)).sum)).map fun q => sliceNF q.2 q.1 (d.read dnD)).zip
            ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)) := by
        rw [← R₁]
        rw [List.zip_map_left]
        rfl
      -- assemble per macc
      cases maccD with
      | none =>
          cases maccS with
          | some aS => exact absurd hmacc (by simp)
          | none =>
              cases hwcase : w with
              | zero =>
                  subst hwcase
                  injection h with h
                  injection h with h1 h2
                  subst h1; subst h2
                  refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
                  rw [cAltJ]
                  rw [if_pos hctb, htag, except_bind_ok, hcs]
                  dsimp only
                  rw [hsub, except_bind_ok, if_pos hxlen, hszXs, except_bind_ok, if_pos hwle]
                  rw [show (pendD.map fun p => ((d.read p.1 : NF), p.2))
                        = pendD.map fun p => ((d₁.read p.1 : NF), p.2) from
                      (pend_read_ext E₁ hpend).symm]
                  rw [← hzipS, S₂, except_bind_ok]
                  dsimp only
                  rw [if_pos hbteq]
              | succ w0 =>
                  subst hwcase
                  injection h with h
                  injection h with h1 h2
                  subst h1; subst h2
                  refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
                  rw [cAltJ]
                  rw [if_pos hctb, htag, except_bind_ok, hcs]
                  dsimp only
                  rw [hsub, except_bind_ok, if_pos hxlen, hszXs, except_bind_ok, if_pos hwle]
                  rw [show (pendD.map fun p => ((d.read p.1 : NF), p.2))
                        = pendD.map fun p => ((d₁.read p.1 : NF), p.2) from
                      (pend_read_ext E₁ hpend).symm]
                  rw [← hzipS, S₂, except_bind_ok]
                  dsimp only
                  rw [if_pos hbteq]
      | some aD =>
          cases maccS with
          | none => exact absurd hmacc (by simp)
          | some aS =>
              obtain ⟨haD, haR⟩ := hmacc
              cases hwcase : w with
              | zero =>
                  subst hwcase
                  injection h with h
                  injection h with h1 h2
                  subst h1; subst h2
                  refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
                  rw [cAltJ]
                  rw [if_pos hctb, htag, except_bind_ok, hcs]
                  dsimp only
                  rw [hsub, except_bind_ok, if_pos hxlen, hszXs, except_bind_ok, if_pos hwle]
                  rw [show (pendD.map fun p => ((d.read p.1 : NF), p.2))
                        = pendD.map fun p => ((d₁.read p.1 : NF), p.2) from
                      (pend_read_ext E₁ hpend).symm]
                  rw [← hzipS, S₂, except_bind_ok]
                  dsimp only
                  rw [if_pos hbteq]
              | succ w0 =>
                  subst hwcase
                  -- the guarded if-chain node
                  rcases hsl : sliceNFD d₂ (szT - (w0 + 1)) (w0 + 1) dnD with ⟨d₃, sl⟩
                  rw [hsl] at h
                  rcases hlt : d₃.mkLit ⟨w0 + 1, BitVec.ofNat (w0 + 1) tag⟩ with ⟨d₄, tl⟩
                  rw [hlt] at h
                  rcases hpe : d₄.rawPrim2 .eq sl tl with ⟨d₅, cnd⟩
                  rw [hpe] at h
                  dsimp only at h
                  split at h
                  rotate_left
                  · exact error_ne_ok h
                  rename_i harm
                  rcases hit : d₅.rawIte cnd bnf' aD with ⟨d₆, itr⟩
                  rw [hit] at h
                  injection h with h
                  injection h with h1 h2
                  subst h1; subst h2
                  have hdnd2 : dnD < d₂.size :=
                    Nat.lt_of_lt_of_le hdn (E₁.trans E₂).size_le
                  obtain ⟨W₃, E₃, L₃, R₃⟩ := mk_out (sliceNFD_spec W₂ _ _ hdnd2) hsl
                  obtain ⟨W₄, E₄, L₄, R₄⟩ := mk_out (mkLit_spec W₃ _) hlt
                  obtain ⟨W₅, E₅, L₅, R₅⟩ := mk_out
                    (rawPrim2_spec W₄ (Nat.lt_of_lt_of_le L₃ E₄.size_le) L₄ rfl) hpe
                  have haD5 : aD < d₅.size := Nat.lt_of_lt_of_le haD
                    ((((E₁.trans E₂).trans E₃).trans E₄).trans E₅).size_le
                  have hbnf5 : bnf' < d₅.size :=
                    Nat.lt_of_lt_of_le L₂ ((E₃.trans E₄).trans E₅).size_le
                  obtain ⟨W₆, E₆, L₆, R₆⟩ := mk_out (rawIte_spec W₅ L₅ hbnf5 haD5 harm) hit
                  refine ⟨W₆, ((((E₁.trans E₂).trans E₃).trans E₄).trans E₅).trans E₆,
                    L₆, ?_⟩
                  rw [cAltJ]
                  rw [if_pos hctb, htag, except_bind_ok, hcs]
                  dsimp only
                  rw [hsub, except_bind_ok, if_pos hxlen, hszXs, except_bind_ok, if_pos hwle]
                  rw [show (pendD.map fun p => ((d.read p.1 : NF), p.2))
                        = pendD.map fun p => ((d₁.read p.1 : NF), p.2) from
                      (pend_read_ext E₁ hpend).symm]
                  rw [← hzipS, S₂, except_bind_ok]
                  dsimp only
                  rw [if_pos hbteq]
                  rw [R₆, R₅, read_ext E₄ sl L₃, R₃, R₄,
                      read_ext ((E₃.trans E₄).trans E₅) bnf' L₂,
                      read_ext ((((E₁.trans E₂).trans E₃).trans E₄).trans E₅) aD haD,
                      read_ext (E₁.trans E₂) dnD hdn, haR]
  | litAlt i =>
      rw [cAltJD] at h
      obtain ⟨det, hbody, h⟩ := except_bind_eq_ok h
      obtain ⟨d₁, bnf', bty⟩ := det
      dsimp only at h
      split at h
      rotate_left
      · exact error_ne_ok h
      rename_i hbteq
      obtain ⟨W₁, E₁, L₁, S₁⟩ := IH Γ'D jΓD body pendD d d₁ bnf' bty Γ'S jΓS
        hwf hG hJ hpend hbody
      cases maccD with
      | none =>
          cases maccS with
          | some aS => exact absurd hmacc (by simp)
          | none =>
              injection h with h
              injection h with h1 h2
              subst h1; subst h2
              refine ⟨W₁, E₁, L₁, ?_⟩
              rw [cAltJ, S₁, except_bind_ok]
              dsimp only
              rw [if_pos hbteq]
      | some aD =>
          cases maccS with
          | none => exact absurd hmacc (by simp)
          | some aS =>
              obtain ⟨haD, haR⟩ := hmacc
              rcases hlt : d₁.mkLit ⟨szT, BitVec.ofInt szT i⟩ with ⟨d₂, tl⟩
              rw [hlt] at h
              rcases hpe : d₂.rawPrim2 .eq dnD tl with ⟨d₃, cnd⟩
              rw [hpe] at h
              dsimp only at h
              split at h
              rotate_left
              · exact error_ne_ok h
              rename_i harm
              rcases hit : d₃.rawIte cnd bnf' aD with ⟨d₄, itr⟩
              rw [hit] at h
              injection h with h
              injection h with h1 h2
              subst h1; subst h2
              obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (mkLit_spec W₁ _) hlt
              have hdn2 : dnD < d₂.size :=
                Nat.lt_of_lt_of_le hdn (E₁.trans E₂).size_le
              obtain ⟨W₃, E₃, L₃, R₃⟩ := mk_out (rawPrim2_spec W₂ hdn2 L₂ rfl) hpe
              have haD3 : aD < d₃.size :=
                Nat.lt_of_lt_of_le haD ((E₁.trans E₂).trans E₃).size_le
              have hbnf3 : bnf' < d₃.size :=
                Nat.lt_of_lt_of_le L₁ (E₂.trans E₃).size_le
              obtain ⟨W₄, E₄, L₄, R₄⟩ := mk_out (rawIte_spec W₃ L₃ hbnf3 haD3 harm) hit
              refine ⟨W₄, ((E₁.trans E₂).trans E₃).trans E₄, L₄, ?_⟩
              rw [cAltJ, S₁, except_bind_ok]
              dsimp only
              rw [if_pos hbteq]
              rw [R₄, R₃, read_ext (E₁.trans E₂) dnD hdn, R₂,
                  read_ext (E₂.trans E₃) bnf' L₁,
                  read_ext ((E₁.trans E₂).trans E₃) aD haD, haR]

/-- The if-chain simulation (`cchainJ`'s mirror). -/
theorem cchainJD_sim {Δ : DEnv} {dmap : HashMap Int Defn} {fuel : Nat}
    (IH : DSimAt Δ dmap fuel) {Γ'D : DGamma} {jΓD : CJEnvD}
    {Γ'S : HashMap Int (NF × Ty)} {jΓS : CJEnv} {dty : Ty} {szT : Nat} {dnD : Nat}
    {resTy : Ty} {pendD : List (Nat × Ty)} :
    ∀ (rest : List Alt) (maccD : Option Nat) (maccS : Option NF) (d d' : Dag)
      (out : Nat) (oty : Ty),
      d.WF → GSim d Γ'D Γ'S → JSimEnv d jΓD jΓS → (∀ p ∈ pendD, p.1 < d.size) →
      dnD < d.size →
      (match maccD, maccS with
       | some aD, some aS => aD < d.size ∧ d.read aD = aS
       | none, none => True
       | _, _ => False) →
      cchainJD Δ dmap fuel Γ'D jΓD dty szT dnD resTy pendD rest maccD d
        = .ok (d', out, oty) →
      d'.WF ∧ d.Ext d' ∧ out < d'.size ∧
        cchainJ Δ dmap fuel Γ'S jΓS dty szT (d.read dnD) resTy
            (pendD.map fun p => ((d.read p.1 : NF), p.2)) rest maccS
          = .ok (d'.read out, oty) := by
  intro rest
  induction rest with
  | nil =>
      intro maccD maccS d d' out oty hwf hG hJ hpend hdn hmacc h
      cases maccD with
      | none => rw [cchainJD] at h; exact error_ne_ok h
      | some els =>
          cases maccS with
          | none => exact absurd hmacc (by simp)
          | some elsS =>
              obtain ⟨hlt, hread⟩ := hmacc
              rw [cchainJD] at h
              obtain ⟨hd, hr, hty⟩ := triple_eq h
              subst hd; subst hr; subst hty
              refine ⟨hwf, Rwv.Hyle.BridgeDag.Dag.Ext.refl d, hlt, ?_⟩
              rw [cchainJ, hread]
  | cons alt restT ihR =>
      intro maccD maccS d d' out oty hwf hG hJ hpend hdn hmacc h
      rcases restT with _ | ⟨r2, rt⟩
      · cases maccD with
        | none =>
            cases maccS with
            | some aS => exact absurd hmacc (by simp)
            | none =>
                rw [cchainJD] at h
                obtain ⟨db, hb, h⟩ := except_bind_eq_ok h
                obtain ⟨d₁, bnf⟩ := db
                dsimp only at h
                obtain ⟨hd, hr, hty⟩ := triple_eq h
                subst hd; subst hr; subst hty
                obtain ⟨W, E, L, S⟩ := cAltJD_sim IH alt none none d d₁ bnf
                  hwf hG hJ hpend hdn trivial hb
                refine ⟨W, E, L, ?_⟩
                rw [cchainJ, S, except_bind_ok]
        | some aD =>
            cases maccS with
            | none => exact absurd hmacc (by simp)
            | some aS =>
                rw [cchainJD] at h
                obtain ⟨dacc, hacc, h⟩ := except_bind_eq_ok h
                obtain ⟨d₁, accnf, accty⟩ := dacc
                dsimp only at h
                obtain ⟨db, hb, h⟩ := except_bind_eq_ok h
                obtain ⟨d₂, bnf'⟩ := db
                dsimp only at h
                obtain ⟨hd, hr, hty⟩ := triple_eq h
                subst hd; subst hr; subst hty
                obtain ⟨W₁, E₁, L₁, S₁⟩ := ihR (some aD) (some aS) d d₁ accnf accty
                  hwf hG hJ hpend hdn hmacc hacc
                obtain ⟨W₂, E₂, L₂, S₂⟩ := cAltJD_sim IH alt (some accnf)
                  (some (d₁.read accnf)) d₁ d₂ bnf' W₁ (hG.mono E₁)
                  (jsimEnv_mono hJ E₁)
                  (fun p hp => Nat.lt_of_lt_of_le (hpend p hp) E₁.size_le)
                  (Nat.lt_of_lt_of_le hdn E₁.size_le) ⟨L₁, rfl⟩ hb
                refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
                rw [cchainJ, S₁, except_bind_ok]
                dsimp only
                rw [show ((pendD.map fun p => ((d.read p.1 : NF), p.2)))
                      = pendD.map fun p => ((d₁.read p.1 : NF), p.2) from
                    (pend_read_ext E₁ hpend).symm,
                    show d.read dnD = d₁.read dnD from
                      (read_ext E₁ dnD hdn).symm, S₂, except_bind_ok]
                all_goals intro h' _
                all_goals cases h'
      · rw [cchainJD] at h
        obtain ⟨dacc, hacc, h⟩ := except_bind_eq_ok h
        obtain ⟨d₁, accnf, accty⟩ := dacc
        dsimp only at h
        obtain ⟨db, hb, h⟩ := except_bind_eq_ok h
        obtain ⟨d₂, bnf'⟩ := db
        dsimp only at h
        obtain ⟨hd, hr, hty⟩ := triple_eq h
        subst hd; subst hr; subst hty
        have hmaccS : maccS.isSome = maccD.isSome := by
          cases maccD <;> cases maccS <;> simp_all
        obtain ⟨W₁, E₁, L₁, S₁⟩ := ihR maccD maccS d d₁ accnf accty
          hwf hG hJ hpend hdn hmacc hacc
        obtain ⟨W₂, E₂, L₂, S₂⟩ := cAltJD_sim IH alt (some accnf)
          (some (d₁.read accnf)) d₁ d₂ bnf' W₁ (hG.mono E₁)
          (jsimEnv_mono hJ E₁)
          (fun p hp => Nat.lt_of_lt_of_le (hpend p hp) E₁.size_le)
          (Nat.lt_of_lt_of_le hdn E₁.size_le) ⟨L₁, rfl⟩ hb
        refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
        rw [cchainJ, S₁, except_bind_ok]
        dsimp only
        rw [show ((pendD.map fun p => ((d.read p.1 : NF), p.2)))
              = pendD.map fun p => ((d₁.read p.1 : NF), p.2) from
            (pend_read_ext E₁ hpend).symm,
            show d.read dnD = d₁.read dnD from
              (read_ext E₁ dnD hdn).symm, S₂, except_bind_ok]
        all_goals intro _ h'
        all_goals cases h'

private theorem cprimFD_sim_finite {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .finite pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .finite (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, ps⟩⟩
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨n, hn, h⟩ := except_bind_eq_ok h
    cases hnd : d.nodes[a]? with
    | none => rw [hnd] at h; exact error_ne_ok h
    | some node =>
        rw [hnd] at h
        cases node <;> try exact error_ne_ok h
        rename_i v
        dsimp only at h
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hlt
        rcases hml : d.mkLit ⟨nbits n, BitVec.ofNat (nbits n) v.nat⟩ with ⟨d₁, r₁⟩
        rw [hml] at h
        obtain ⟨hd, hr, hty⟩ := triple_eq h
        subst hd; subst hr; subst hty
        obtain ⟨W, E, L, R⟩ := mk_out (mkLit_spec hwf _) hml
        have hread : d.read a = .lit v :=
          read_eq (fun j hj => absurd hj (by simp [DNode.children])) hnd
        refine ⟨W, E, L, ?_⟩
        show rowFinite (Ty.flattenArrow pty).2 [((d.read a : NF), ta)] = _
        rw [hread, rowFinite, hn, except_bind_ok]
        dsimp only
        rw [if_pos hlt, R]

private theorem cprimFD_sim_natVal {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .natVal pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .natVal (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, ps⟩⟩
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨pt, hpt, h⟩ := except_bind_eq_ok h
    obtain ⟨k, hk, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hfl
    rcases hml : d.mkLit ⟨128, BitVec.ofNat 128 k⟩ with ⟨d₁, r₁⟩
    rw [hml] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out (mkLit_spec hwf _) hml
    refine ⟨W, E, L, ?_⟩
    show rowNatVal (Ty.flattenArrow pty).1 (Ty.flattenArrow pty).2
      [((d.read a : NF), ta)] = _
    rw [rowNatVal, hpt, except_bind_ok, hk, except_bind_ok, hfl]
    split
    · rw [R]
    · simp_all

private theorem cprimFD_sim_vecIndexProxy {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .vecIndexProxy pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .vecIndexProxy (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, _ | ⟨p3, ps⟩⟩⟩
    · exact error_ne_ok h
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨pt, hpt, h⟩ := except_bind_eq_ok h
    obtain ⟨k, hk, h⟩ := except_bind_eq_ok h
    obtain ⟨lt', hlt', h⟩ := except_bind_eq_ok h
    obtain ⟨len, tea⟩ := lt'
    dsimp only at h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hteq
    obtain ⟨se, hse, h⟩ := except_bind_eq_ok h
    obtain ⟨szA, hszA, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hklen
    rcases hsl : sliceNFD d (szA - k * se - se) se a with ⟨d₁, r₁⟩
    rw [hsl] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out
      (sliceNFD_spec hwf _ _ (show a < d.size from hpas (a, ta) (by simp))) hsl
    refine ⟨W, E, L, ?_⟩
    show rowVecIndexProxy Δ szf (Ty.flattenArrow pty).1 (Ty.flattenArrow pty).2
      [((d.read a : NF), ta), ((d.read b2 : NF), tb2)] = _
    rw [rowVecIndexProxy, hpt, except_bind_ok, hk, except_bind_ok, hlt', except_bind_ok]
    dsimp only
    rw [if_pos hteq, hse, except_bind_ok, hszA, except_bind_ok, if_pos hklen, R]

private theorem cprimFD_sim_finiteMinBound {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .finiteMinBound pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .finiteMinBound (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨p1, ps⟩
    rotate_left
    · exact error_ne_ok h
    obtain ⟨n, hn, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hge
    rcases hml : d.mkLit ⟨nbits n, BitVec.ofNat (nbits n) 0⟩ with ⟨d₁, r₁⟩
    rw [hml] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out (mkLit_spec hwf _) hml
    refine ⟨W, E, L, ?_⟩
    show rowFinBound false (Ty.flattenArrow pty).2 [] = _
    rw [rowFinBound, hn, except_bind_ok, if_pos hge, R]
    rfl

private theorem cprimFD_sim_finiteMaxBound {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .finiteMaxBound pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .finiteMaxBound (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨p1, ps⟩
    rotate_left
    · exact error_ne_ok h
    obtain ⟨n, hn, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hge
    rcases hml : d.mkLit ⟨nbits n, BitVec.ofNat (nbits n) (n - 1)⟩ with ⟨d₁, r₁⟩
    rw [hml] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out (mkLit_spec hwf _) hml
    refine ⟨W, E, L, ?_⟩
    show rowFinBound true (Ty.flattenArrow pty).2 [] = _
    rw [rowFinBound, hn, except_bind_ok, if_pos hge, R]
    rfl

private theorem cprimFD_sim_toFinite {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .toFinite pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .toFinite (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, ps⟩⟩
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨n, hn, h⟩ := except_bind_eq_ok h
    obtain ⟨wa, hwa, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hle
    rcases hrz : resizeNFD d (nbits n) wa a with ⟨d₁, r₁⟩
    rw [hrz] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out
      (resizeNFD_spec hwf (show a < d.size from hpas (a, ta) (by simp))) hrz
    refine ⟨W, E, L, ?_⟩
    show rowToFinite (Ty.flattenArrow pty).2 [((d.read a : NF), ta)] = _
    rw [rowToFinite, hn, except_bind_ok, hwa, except_bind_ok, if_pos hle, R]

private theorem cprimFD_sim_toFiniteMod {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .toFiniteMod pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .toFiniteMod (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, ps⟩⟩
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨n, hn, h⟩ := except_bind_eq_ok h
    obtain ⟨wa, hwa, h⟩ := except_bind_eq_ok h
    split at h
    · rename_i hle
      rcases hrz : resizeNFD d (nbits n) wa a with ⟨d₁, r₁⟩
      rw [hrz] at h
      obtain ⟨hd, hr, hty⟩ := triple_eq h
      subst hd; subst hr; subst hty
      obtain ⟨W, E, L, R⟩ := mk_out
        (resizeNFD_spec hwf (show a < d.size from hpas (a, ta) (by simp))) hrz
      refine ⟨W, E, L, ?_⟩
      show rowToFiniteMod (Ty.flattenArrow pty).2 [((d.read a : NF), ta)] = _
      rw [rowToFiniteMod, hn, except_bind_ok, hwa, except_bind_ok, if_pos hle, R]
    · rename_i hnle
      split at h
      · exact error_ne_ok h
      rename_i hn0c
      rcases hr1 : resizeNFD d (max wa (nbits n)) wa a with ⟨d₁, r₁⟩
      rw [hr1] at h
      rcases hl2 : d₁.mkLit ⟨max wa (nbits n), BitVec.ofNat (max wa (nbits n)) n⟩
        with ⟨d₂, l₂⟩
      rw [hl2] at h
      rcases hp3 : d₂.rawPrim2 .umod r₁ l₂ with ⟨d₃, r₃⟩
      rw [hp3] at h
      rcases hr4 : resizeNFD d₃ (nbits n) (max wa (nbits n)) r₃ with ⟨d₄, r₄⟩
      rw [hr4] at h
      obtain ⟨hd, hr, hty⟩ := triple_eq h
      subst hd; subst hr; subst hty
      obtain ⟨W₁, E₁, L₁, R₁⟩ := mk_out
        (resizeNFD_spec hwf (show a < d.size from hpas (a, ta) (by simp))) hr1
      obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (mkLit_spec W₁ _) hl2
      obtain ⟨W₃, E₃, L₃, R₃⟩ := mk_out
        (rawPrim2_spec W₂ (Nat.lt_of_lt_of_le L₁ E₂.size_le) L₂ rfl) hp3
      obtain ⟨W₄, E₄, L₄, R₄⟩ := mk_out (resizeNFD_spec W₃ L₃) hr4
      refine ⟨W₄, ((E₁.trans E₂).trans E₃).trans E₄, L₄, ?_⟩
      show rowToFiniteMod (Ty.flattenArrow pty).2 [((d.read a : NF), ta)] = _
      rw [rowToFiniteMod, hn, except_bind_ok, hwa, except_bind_ok, if_neg hnle,
          if_neg hn0c, R₄, R₃, read_ext E₂ r₁ L₁, R₁, R₂]

private theorem cprimFD_sim_fromFinite {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .fromFinite pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .fromFinite (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, ps⟩⟩
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨n, hn, h⟩ := except_bind_eq_ok h
    obtain ⟨m, hm, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hle
    rcases hrz : resizeNFD d m (nbits n) a with ⟨d₁, r₁⟩
    rw [hrz] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out
      (resizeNFD_spec hwf (show a < d.size from hpas (a, ta) (by simp))) hrz
    refine ⟨W, E, L, ?_⟩
    show rowFromFinite (Ty.flattenArrow pty).2 [((d.read a : NF), ta)] = _
    rw [rowFromFinite, hn, except_bind_ok, hm, except_bind_ok, if_pos hle, R]

private theorem cprimFD_sim_vecReplicate {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .vecReplicate pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .vecReplicate (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, ps⟩⟩
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨nte, hnte, h⟩ := except_bind_eq_ok h
    obtain ⟨n, te⟩ := nte
    dsimp only at h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hteq
    obtain ⟨sz, hsz, h⟩ := except_bind_eq_ok h
    split at h
    · rename_i hz
      rcases hml : d.mkLit Rwv.Hyle.BV.nil with ⟨d₁, r₁⟩
      rw [hml] at h
      obtain ⟨hd, hr, hty⟩ := triple_eq h
      subst hd; subst hr; subst hty
      obtain ⟨W, E, L, R⟩ := mk_out (mkLit_spec hwf _) hml
      refine ⟨W, E, L, ?_⟩
      show rowVecReplicate Δ szf (Ty.flattenArrow pty).2 [((d.read a : NF), ta)] = _
      rw [rowVecReplicate, hnte, except_bind_ok]
      dsimp only
      rw [if_pos hteq, hsz, except_bind_ok, if_pos hz, R]
    · rename_i hnz
      rcases hp : d.rawPrim1 (.rep n) a with ⟨d₁, r₁⟩
      rw [hp] at h
      obtain ⟨hd, hr, hty⟩ := triple_eq h
      subst hd; subst hr; subst hty
      obtain ⟨W, E, L, R⟩ := mk_out
        (rawPrim1_spec hwf (show a < d.size from hpas (a, ta) (by simp)) rfl) hp
      refine ⟨W, E, L, ?_⟩
      show rowVecReplicate Δ szf (Ty.flattenArrow pty).2 [((d.read a : NF), ta)] = _
      rw [rowVecReplicate, hnte, except_bind_ok]
      dsimp only
      rw [if_pos hteq, hsz, except_bind_ok, if_neg hnz, R]

private theorem cprimFD_sim_vecConcat {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .vecConcat pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .vecConcat (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, _ | ⟨p3, ps⟩⟩⟩
    · exact error_ne_ok h
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨nte, hnte, h⟩ := except_bind_eq_ok h
    obtain ⟨nr, ter⟩ := nte
    dsimp only at h
    obtain ⟨nte1, hnte1, h⟩ := except_bind_eq_ok h
    obtain ⟨n1, te1⟩ := nte1
    dsimp only at h
    obtain ⟨nte2, hnte2, h⟩ := except_bind_eq_ok h
    obtain ⟨n2, te2⟩ := nte2
    dsimp only at h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hteq
    obtain ⟨sa, hsa, h⟩ := except_bind_eq_ok h
    obtain ⟨sb, hsb, h⟩ := except_bind_eq_ok h
    rcases hcat : catNFD d [(a, sa), (b2, sb)] with ⟨d₁, r₁⟩
    rw [hcat] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out (catNFD_spec hwf (by
      intro p hp
      rcases List.mem_cons.mp hp with hp | hp
      · subst hp; exact hpas (a, ta) (by simp)
      rcases List.mem_cons.mp hp with hp | hp
      · subst hp; exact hpas (b2, tb2) (by simp)
      · exact absurd hp (by simp))) hcat
    refine ⟨W, E, L, ?_⟩
    show rowVecConcat Δ szf (Ty.flattenArrow pty).2
      [((d.read a : NF), ta), ((d.read b2 : NF), tb2)] = _
    rw [rowVecConcat, hnte, except_bind_ok]
    dsimp only
    rw [hnte1, except_bind_ok]
    dsimp only
    rw [hnte2, except_bind_ok]
    dsimp only
    rw [if_pos hteq, hsa, except_bind_ok, hsb, except_bind_ok, R]
    rfl

private theorem cprimFD_sim_vecReverse {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .vecReverse pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .vecReverse (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, ps⟩⟩
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨nte, hnte, h⟩ := except_bind_eq_ok h
    obtain ⟨nr, ter⟩ := nte
    dsimp only at h
    obtain ⟨nte1, hnte1, h⟩ := except_bind_eq_ok h
    obtain ⟨n1, te1⟩ := nte1
    dsimp only at h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hteq
    obtain ⟨se, hse, h⟩ := except_bind_eq_ok h
    rcases hsw : slicesWD d a ((List.range n1).map fun k => (k * se, se)) with ⟨d₁, ps₁⟩
    rw [hsw] at h
    rcases hcat : catNFD d₁ ps₁ with ⟨d₂, r₂⟩
    rw [hcat] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    have hspec := slicesWD_spec (a := a) ((List.range n1).map fun k => (k * se, se)) d hwf
      (show a < d.size from hpas (a, ta) (by simp))
    rw [hsw, MkLW] at hspec
    obtain ⟨W₁, E₁, L₁, R₁⟩ := hspec
    obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (catNFD_spec W₁ L₁) hcat
    refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
    show rowVecReverse Δ szf (Ty.flattenArrow pty).2 [((d.read a : NF), ta)] = _
    rw [rowVecReverse, hnte, except_bind_ok]
    dsimp only
    rw [hnte1, except_bind_ok]
    dsimp only
    rw [if_pos hteq, hse, except_bind_ok, R₂, R₁]
    congr 1
    rw [List.map_map]
    rfl

private theorem cprimFD_sim_vecSlice {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .vecSlice pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .vecSlice (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, _ | ⟨p3, ps⟩⟩⟩
    · exact error_ne_ok h
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨pt, hpt, h⟩ := except_bind_eq_ok h
    obtain ⟨i, hi, h⟩ := except_bind_eq_ok h
    obtain ⟨nte, hnte, h⟩ := except_bind_eq_ok h
    obtain ⟨m, ter⟩ := nte
    dsimp only at h
    obtain ⟨nte1, hnte1, h⟩ := except_bind_eq_ok h
    obtain ⟨len, tea⟩ := nte1
    dsimp only at h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hteq
    obtain ⟨se, hse, h⟩ := except_bind_eq_ok h
    obtain ⟨szA, hszA, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hrange
    rcases hsl : sliceNFD d (szA - i * se - m * se) (m * se) b2 with ⟨d₁, r₁⟩
    rw [hsl] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out
      (sliceNFD_spec hwf _ _ (show b2 < d.size from hpas (b2, tb2) (by simp))) hsl
    refine ⟨W, E, L, ?_⟩
    show rowVecSlice Δ szf (Ty.flattenArrow pty).1 (Ty.flattenArrow pty).2
      [((d.read a : NF), ta), ((d.read b2 : NF), tb2)] = _
    rw [rowVecSlice, hpt, except_bind_ok, hi, except_bind_ok, hnte, except_bind_ok]
    dsimp only
    rw [hnte1, except_bind_ok]
    dsimp only
    rw [if_pos hteq, hse, except_bind_ok, hszA, except_bind_ok, if_pos hrange, R]

private theorem cprimFD_sim_vecRSlice {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .vecRSlice pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .vecRSlice (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    simp only [cprimFD] at h
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, _ | ⟨p3, ps⟩⟩⟩
    · exact error_ne_ok h
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨pt, hpt, h⟩ := except_bind_eq_ok h
    obtain ⟨i, hi, h⟩ := except_bind_eq_ok h
    obtain ⟨nte, hnte, h⟩ := except_bind_eq_ok h
    obtain ⟨m, ter⟩ := nte
    dsimp only at h
    obtain ⟨nte1, hnte1, h⟩ := except_bind_eq_ok h
    obtain ⟨len, tea⟩ := nte1
    dsimp only at h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hteq
    obtain ⟨se, hse, h⟩ := except_bind_eq_ok h
    obtain ⟨szA, hszA, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hrange
    rcases hsl : sliceNFD d (i * se) (m * se) b2 with ⟨d₁, r₁⟩
    rw [hsl] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W, E, L, R⟩ := mk_out
      (sliceNFD_spec hwf _ _ (show b2 < d.size from hpas (b2, tb2) (by simp))) hsl
    refine ⟨W, E, L, ?_⟩
    show rowVecRSlice Δ szf (Ty.flattenArrow pty).1 (Ty.flattenArrow pty).2
      [((d.read a : NF), ta), ((d.read b2 : NF), tb2)] = _
    rw [rowVecRSlice, hpt, except_bind_ok, hi, except_bind_ok, hnte, except_bind_ok]
    dsimp only
    rw [hnte1, except_bind_ok]
    dsimp only
    rw [if_pos hteq, hse, except_bind_ok, hszA, except_bind_ok, if_pos hrange, R]

set_option maxHeartbeats 1600000 in
private theorem cprimFD_sim_vecIndex {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat}
    {pty : Ty} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty .vecIndex pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty .vecIndex (pas.map fun p => ((d.read p.1 : NF), p.2))
        = .ok (d'.read r, ty) := by
    rcases pas with _ | ⟨⟨a, ta⟩, _ | ⟨⟨b2, tb2⟩, _ | ⟨p3, ps⟩⟩⟩ <;>
      rw [cprimFD.eq_def] at h <;> dsimp only at h
    · exact error_ne_ok h
    · exact error_ne_ok h
    rotate_left
    · exact error_ne_ok h
    obtain ⟨nte, hnte, h⟩ := except_bind_eq_ok h
    obtain ⟨len, te⟩ := nte
    dsimp only at h
    obtain ⟨nb, hnb, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hteq
    obtain ⟨se, hse, h⟩ := except_bind_eq_ok h
    obtain ⟨szA, hszA, h⟩ := except_bind_eq_ok h
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i hguard
    rcases hr1 : resizeNFD d (max (nbits nb) 128) (nbits nb) b2 with ⟨d₁, i'⟩
    rw [hr1] at h
    rcases hl2 : d₁.mkLit ⟨max (nbits nb) 128, BitVec.ofNat (max (nbits nb) 128) len⟩
      with ⟨d₂, lLen⟩
    rw [hl2] at h
    rcases hp3 : d₂.rawPrim2 .sub lLen i' with ⟨d₃, s₁⟩
    rw [hp3] at h
    rcases hl4 : d₃.mkLit ⟨max (nbits nb) 128, 1⟩ with ⟨d₄, l1⟩
    rw [hl4] at h
    rcases hp5 : d₄.rawPrim2 .sub s₁ l1 with ⟨d₅, s₂⟩
    rw [hp5] at h
    rcases hl6 : d₅.mkLit ⟨max (nbits nb) 128, BitVec.ofNat (max (nbits nb) 128) se⟩
      with ⟨d₆, lSe⟩
    rw [hl6] at h
    rcases hp7 : d₆.rawPrim2 .mul s₂ lSe with ⟨d₇, amt⟩
    rw [hp7] at h
    rcases hp8 : d₇.rawPrim2 .lshr a amt with ⟨d₈, sh⟩
    rw [hp8] at h
    rcases hr9 : resizeNFD d₈ se szA sh with ⟨d₉, r₉⟩
    rw [hr9] at h
    obtain ⟨hd, hr, hty⟩ := triple_eq h
    subst hd; subst hr; subst hty
    obtain ⟨W₁, E₁, L₁, R₁⟩ := mk_out
      (resizeNFD_spec hwf (show b2 < d.size from hpas (b2, tb2) (by simp))) hr1
    obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (mkLit_spec W₁ _) hl2
    obtain ⟨W₃, E₃, L₃, R₃⟩ := mk_out
      (rawPrim2_spec W₂ L₂ (Nat.lt_of_lt_of_le L₁ E₂.size_le) rfl) hp3
    obtain ⟨W₄, E₄, L₄, R₄⟩ := mk_out (mkLit_spec W₃ _) hl4
    obtain ⟨W₅, E₅, L₅, R₅⟩ := mk_out
      (rawPrim2_spec W₄ (Nat.lt_of_lt_of_le L₃ E₄.size_le) L₄ rfl) hp5
    obtain ⟨W₆, E₆, L₆, R₆⟩ := mk_out (mkLit_spec W₅ _) hl6
    obtain ⟨W₇, E₇, L₇, R₇⟩ := mk_out
      (rawPrim2_spec W₆ (Nat.lt_of_lt_of_le L₅ E₆.size_le) L₆ rfl) hp7
    have haRange : a < d₇.size :=
      Nat.lt_of_lt_of_le (show a < d.size from hpas (a, ta) (by simp))
        ((((((E₁.trans E₂).trans E₃).trans E₄).trans E₅).trans E₆).trans E₇).size_le
    obtain ⟨W₈, E₈, L₈, R₈⟩ := mk_out (rawPrim2_spec W₇ haRange L₇ rfl) hp8
    obtain ⟨W₉, E₉, L₉, R₉⟩ := mk_out (resizeNFD_spec W₈ L₈) hr9
    refine ⟨W₉, (((((((E₁.trans E₂).trans E₃).trans E₄).trans E₅).trans E₆).trans
      E₇).trans E₈).trans E₉, L₉, ?_⟩
    show rowVecIndex Δ szf (Ty.flattenArrow pty).2
      [((d.read a : NF), ta), ((d.read b2 : NF), tb2)] = _
    rw [rowVecIndex, hnte, except_bind_ok]
    dsimp only
    rw [hnb, except_bind_ok, if_pos hteq, hse, except_bind_ok, hszA, except_bind_ok,
        if_pos hguard, R₉, R₈,
        read_ext ((((((E₁.trans E₂).trans E₃).trans E₄).trans E₅).trans E₆).trans E₇) a
          (show a < d.size from hpas (a, ta) (by simp)),
        R₇, read_ext E₆ s₂ L₅, R₅,
        read_ext E₄ s₁ L₃, R₃, R₂,
        read_ext E₂ i' L₁, R₁, R₄, R₆]

set_option maxHeartbeats 1600000 in
/-- The extended row table's simulation: a successful `cprimFD` run
certifies the `cprimF` row over the read-back arguments (split per row
family; non-extended rows dispatch to `cprimD_sim`). -/
theorem cprimFD_sim {d : Dag} (hwf : d.WF) {Δ : DEnv} {szf : Nat} {pty : Ty}
    {b : Builtin} {pas : List (Nat × Ty)} {d' : Dag} {r : Nat} {ty : Ty}
    (hpas : ∀ p ∈ pas, p.1 < d.size)
    (h : cprimFD d Δ szf pty b pas = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cprimF Δ szf pty b (pas.map fun p => ((d.read p.1 : NF), p.2)) = .ok (d'.read r, ty) := by
  cases b
  case finite => exact cprimFD_sim_finite hwf hpas h
  case natVal => exact cprimFD_sim_natVal hwf hpas h
  case vecIndexProxy => exact cprimFD_sim_vecIndexProxy hwf hpas h
  case finiteMinBound => exact cprimFD_sim_finiteMinBound hwf hpas h
  case finiteMaxBound => exact cprimFD_sim_finiteMaxBound hwf hpas h
  case toFinite => exact cprimFD_sim_toFinite hwf hpas h
  case toFiniteMod => exact cprimFD_sim_toFiniteMod hwf hpas h
  case fromFinite => exact cprimFD_sim_fromFinite hwf hpas h
  case vecReplicate => exact cprimFD_sim_vecReplicate hwf hpas h
  case vecConcat => exact cprimFD_sim_vecConcat hwf hpas h
  case vecReverse => exact cprimFD_sim_vecReverse hwf hpas h
  case vecSlice => exact cprimFD_sim_vecSlice hwf hpas h
  case vecRSlice => exact cprimFD_sim_vecRSlice hwf hpas h
  case vecIndex => exact cprimFD_sim_vecIndex hwf hpas h
  all_goals
    simp only [cprimFD] at h
    obtain ⟨W, E, L, R⟩ := cprimD_sim hwf hpas h
    simp only [cprimF]
    exact ⟨W, E, L, R⟩

/-! ### The master simulation: a successful `cexpJD` run certifies the
`cexpJ` run whose result is the reading of the returned index. -/

/-- Zip-with-widths reads transport (the constructor-field pattern):
reading the zipped indices at an extended store is zipping the
read-back pairs' firsts. -/
private theorem zip_reads {d d' : Dag} (hext : d.Ext d') :
    ∀ (pas : List (Nat × Ty)) (ws : List Nat), (∀ p ∈ pas, p.1 < d.size) →
      ((pas.map (·.1)).zip ws).map (fun q => ((d'.read q.1 : NF), q.2))
        = ((pas.map fun p => ((d.read p.1 : NF), p.2)).map (·.1)).zip ws := by
  intro pas
  induction pas with
  | nil => intro ws _; rfl
  | cons p rest ih =>
      intro ws h
      cases ws with
      | nil => rfl
      | cons w wrest =>
          simp only [List.map_cons, List.zip_cons_cons]
          rw [read_ext hext p.1 (h p List.mem_cons_self)]
          exact congrArg _ (ih wrest fun q hq => h q (List.mem_cons_of_mem _ hq))

/-- Same-store variant for the per-element `(·, se)` annotation (the
vector-literal pattern). -/
private theorem map_se_reads {d : Dag} (se : Nat) (pas : List (Nat × Ty)) :
    (((pas.map (·.1)).map (·, se)).map fun q => ((d.read q.1 : NF), q.2))
      = ((pas.map fun p => ((d.read p.1 : NF), p.2)).map (·.1)).map (·, se) := by
  simp only [List.map_map]
  rfl

set_option maxHeartbeats 3200000 in
/-- THE simulation theorem for the DAG mirror (Phase 4d): a successful
`cexpJD` compilation certifies a successful `cexpJ` compilation whose
normal form is the reading of the returned index, at read-back
environments, join closures, and pending arguments. -/
theorem cexpJD_sim {Δ : DEnv} {dmap : HashMap Int Defn} :
    ∀ (fuel : Nat), DSimAt Δ dmap fuel := by
  intro fuel
  induction fuel with
  | zero =>
      intro ΓD jΓD e pendD d d' r ty ΓS jΓS _ _ _ _ hc
      rw [cexpJD] at hc
      exact error_ne_ok hc
  | succ fuel ih =>
      intro ΓD jΓD e pendD d d' r ty ΓS jΓS hwf hG hJ hpend hc
      rw [cexpJD] at hc
      rw [cexpJ]
      rcases hfl : Eval.flattenApp e with ⟨hd, args⟩
      rw [hfl] at hc
      clear hfl
      cases hd with
      | var x =>
          dsimp only at hc ⊢
          cases hΓx : ΓD.get? x.uniq with
          | some nt =>
              rw [hΓx] at hc
              have hsx : ΓS.get? x.uniq = some (d.read nt.1, nt.2) := by
                rw [hG.reads x.uniq, hΓx]
                rfl
              cases args with
              | cons a as =>
                  cases pendD <;> exact error_ne_ok hc
              | nil =>
                  cases pendD with
                  | cons p ps => exact error_ne_ok hc
                  | nil =>
                      obtain ⟨hd', hr, hty⟩ := triple_eq hc
                      subst hd'; subst hr; subst hty
                      refine ⟨hwf, Rwv.Hyle.BridgeDag.Dag.Ext.refl d,
                        hG.inRange _ _ hΓx, ?_⟩
                      rw [hsx]
                      simp only [List.map_nil]
          | none =>
              rw [hΓx] at hc
              have hsx : ΓS.get? x.uniq = none := by
                rw [hG.reads x.uniq, hΓx]
                rfl
              rw [hsx]
              cases hdm : dmap.get? x.uniq with
              | none => rw [hdm] at hc; exact error_ne_ok hc
              | some df =>
                  rw [hdm] at hc
                  dsimp only at hc ⊢
                  rw [bind_ok_iff] at hc
                  obtain ⟨dp, hargs, hc⟩ := hc
                  obtain ⟨d₁, pas⟩ := dp
                  dsimp only at hc
                  obtain ⟨W₁, E₁, L₁, S₁⟩ := cargsD_sim ih args d d₁ pas hwf hG hJ hargs
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i hlen
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i hteqs
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i hsat
                  have hallrange : ∀ p ∈ pas ++ pendD, p.1 < d₁.size := by
                    intro p hp
                    rcases List.mem_append.mp hp with hp | hp
                    · exact L₁ p hp
                    · exact Nat.lt_of_lt_of_le (hpend p hp) E₁.size_le
                  have hGm : GSim d₁
                      (mkGammaD df.params ((pas ++ pendD).take df.params.length))
                      (mkGamma df.params
                        (((pas ++ pendD).take df.params.length).map
                          fun p => ((d₁.read p.1 : NF), p.2))) :=
                    gsim_bindFields df.params (gsim_empty d₁)
                      (fun p hp => hallrange p (List.mem_of_mem_take hp))
                  obtain ⟨W₂, E₂, L₂, S₂⟩ := ih _ [] df.body
                    ((pas ++ pendD).drop df.params.length) d₁ d' r ty _ []
                    W₁ hGm jsimEnv_nil
                    (fun p hp => hallrange p (List.mem_of_mem_drop hp)) hc
                  refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
                  rw [S₁, except_bind_ok]
                  rw [show (pendD.map fun p => ((d.read p.1 : NF), p.2))
                        = pendD.map fun p => ((d₁.read p.1 : NF), p.2) from
                      (pend_read_ext E₁ hpend).symm,
                      ← List.map_append]
                  simp only [List.length_map, ← List.map_take, ← List.map_drop]
                  rw [if_pos hlen]
                  rw [teqAllD_eq d₁] at hteqs
                  rw [if_pos hteqs, if_pos hsat]
                  exact S₂
      | con cty c =>
          cases pendD with
          | cons p ps =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
          simp only [List.map_nil]
          dsimp only at hc ⊢
          rw [bind_ok_iff] at hc
          obtain ⟨dp, hargs, hc⟩ := hc
          obtain ⟨d₁, pas⟩ := dp
          dsimp only at hc
          obtain ⟨W₁, E₁, L₁, S₁⟩ := cargsD_sim ih args d d₁ pas hwf hG hJ hargs
          split at hc
          rotate_left
          · exact error_ne_ok hc
          rename_i hplen
          cases hcs : Δ.ctorSig.get? c with
          | none => rw [hcs] at hc; exact error_ne_ok hc
          | some sig =>
          rw [hcs] at hc
          dsimp only at hc
          rw [bind_ok_iff] at hc
          obtain ⟨sub, hsub, hc⟩ := hc
          split at hc
          rotate_left
          · exact error_ne_ok hc
          rename_i hteqs
          split at hc
          rotate_left
          · exact error_ne_ok hc
          rename_i hcob
          rw [bind_ok_iff] at hc
          obtain ⟨whole, hwhole, hc⟩ := hc
          rw [bind_ok_iff] at hc
          obtain ⟨tg, htg, hc⟩ := hc
          obtain ⟨tag, w⟩ := tg
          dsimp only at hc
          rw [bind_ok_iff] at hc
          obtain ⟨ws, hws, hc⟩ := hc
          split at hc
          rotate_left
          · exact error_ne_ok hc
          rename_i hwsum
          rcases hl1 : d₁.mkLit ⟨w, BitVec.ofNat w tag⟩ with ⟨d₂, tl⟩
          rw [hl1] at hc
          rcases hl2 : d₂.mkLit ⟨whole - w - ws.sum, 0⟩ with ⟨d₃, pl⟩
          rw [hl2] at hc
          dsimp only at hc
          rcases hcat : catNFD d₃
              ((tl, w) :: (pl, whole - w - ws.sum) :: (pas.map (·.1)).zip ws)
            with ⟨d₄, rr⟩
          rw [hcat] at hc
          obtain ⟨hd4, hrr, hty⟩ := triple_eq hc
          subst hd4; subst hrr; subst hty
          obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (mkLit_spec W₁ _) hl1
          obtain ⟨W₃, E₃, L₃, R₃⟩ := mk_out (mkLit_spec W₂ _) hl2
          obtain ⟨W₄, E₄, L₄, R₄⟩ := mk_out (catNFD_spec W₃ (by
            intro q hq
            rcases List.mem_cons.mp hq with rfl | hq
            · exact Nat.lt_of_lt_of_le L₂ E₃.size_le
            rcases List.mem_cons.mp hq with rfl | hq
            · exact L₃
            · obtain ⟨q1, q2⟩ := q
              obtain ⟨p, hp, hpq⟩ := List.mem_map.mp (List.of_mem_zip hq).1
              dsimp only
              rw [← hpq]
              exact Nat.lt_of_lt_of_le (L₁ p hp) (E₂.trans E₃).size_le)) hcat
          refine ⟨W₄, ((E₁.trans E₂).trans E₃).trans E₄, L₄, ?_⟩
          rw [S₁, except_bind_ok]
          dsimp only
          rw [if_pos (show (pas.map fun p => ((d₁.read p.1 : NF), p.2)).length
                = (Ty.flattenArrow cty).1.length by simpa using hplen)]
          rw [hsub, except_bind_ok]
          rw [teqAllND_eq d₁] at hteqs
          rw [if_pos hteqs, if_pos hcob, hwhole, except_bind_ok, htg, except_bind_ok]
          dsimp only
          rw [hws, except_bind_ok, if_pos hwsum, R₄, List.map_cons, List.map_cons]
          dsimp only
          rw [read_ext E₃ tl L₂, R₂, R₃, zip_reads (E₂.trans E₃) pas ws L₁]
      | prim pty b =>
          cases pendD with
          | cons p ps =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
          simp only [List.map_nil]
          dsimp only at hc ⊢
          cases b
          case error =>
              dsimp only at hc ⊢
              rw [bind_ok_iff] at hc
              obtain ⟨sz, hszk, hc⟩ := hc
              rw [bind_ok_iff] at hc
              obtain ⟨zv, hzv, hc⟩ := hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hvty
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i bv hrep
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hbv
              rcases hml : d.mkLit ⟨sz, 0⟩ with ⟨d₁, r₁⟩
              rw [hml] at hc
              dsimp only at hc
              obtain ⟨hd1, hr1, hty1⟩ := triple_eq hc
              subst hd1; subst hr1; subst hty1
              obtain ⟨W, E, L, R⟩ := mk_out (mkLit_spec hwf _) hml
              refine ⟨W, E, L, ?_⟩
              rw [hszk, except_bind_ok, hzv, except_bind_ok, if_pos hvty, hrep]
              dsimp only
              rw [if_pos hbv, R]
          case vecFromList =>
              rcases args with _ | ⟨a1, _ | ⟨a2, rest⟩⟩ <;> dsimp only at hc
              · exact error_ne_ok hc
              rotate_left
              · cases a1 <;> dsimp only at hc <;> exact error_ne_ok hc
              cases a1 <;> dsimp only at hc <;> try exact error_ne_ok hc
              rename_i tyL els
              dsimp only
              rw [bind_ok_iff] at hc
              obtain ⟨dp, hargs, hc⟩ := hc
              obtain ⟨d₁, pas⟩ := dp
              dsimp only at hc
              obtain ⟨W₁, E₁, L₁, S₁⟩ := cargsD_sim ih els d d₁ pas hwf hG hJ hargs
              rw [S₁, except_bind_ok]
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i nt te hflat
              cases hev : Ty.evalNat nt with
              | none => rw [hev] at hc; exact error_ne_ok hc
              | some k =>
              rw [hev] at hc
              dsimp only at hc ⊢
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hlen
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hteqs
              rw [bind_ok_iff] at hc
              obtain ⟨se, hse, hc⟩ := hc
              rcases hcat : catNFD d₁ ((pas.map (·.1)).map (·, se)) with ⟨d₂, rr⟩
              rw [hcat] at hc
              obtain ⟨hd2, hrr, hty⟩ := triple_eq hc
              subst hd2; subst hrr; subst hty
              obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (catNFD_spec W₁ (by
                intro q hq
                obtain ⟨a, ha, hqa⟩ := List.mem_map.mp hq
                obtain ⟨p, hp, hap⟩ := List.mem_map.mp ha
                rw [← hqa]
                dsimp only
                rw [← hap]
                exact L₁ p hp)) hcat
              refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
              rw [if_pos (by simpa using hlen)]
              rw [teqAllD_eq d₁] at hteqs
              rw [if_pos (by
                    rw [List.length_map]
                    exact hteqs),
                  hse, except_bind_ok, R₂, map_se_reads]
          case bitIndex =>
              rcases args with _ | ⟨argE, _ | ⟨iE, _ | ⟨a3, rest⟩⟩⟩ <;> dsimp only at hc
              · exact error_ne_ok hc
              · exact error_ne_ok hc
              rotate_left
              · exact error_ne_ok hc
              dsimp only
              cases hfin : finLitE iE with
              | none => rw [hfin] at hc; exact error_ne_ok hc
              | some i =>
              rw [hfin] at hc
              dsimp only at hc ⊢
              rw [bind_ok_iff] at hc
              obtain ⟨dat, hargE, hc⟩ := hc
              obtain ⟨d₁, a, ta⟩ := dat
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨wa, hwa, hc⟩ := hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hbool
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hlt
              rcases hsl : sliceNFD d₁ i 1 a with ⟨d₂, r₂⟩
              rw [hsl] at hc
              obtain ⟨hd2, hr2, hty2⟩ := triple_eq hc
              subst hd2; subst hr2; subst hty2
              obtain ⟨W₁, E₁, L₁, S₁⟩ := ih ΓD jΓD argE [] d d₁ a ta ΓS jΓS
                hwf hG hJ (by simp) hargE
              simp only [List.map_nil] at S₁
              obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (sliceNFD_spec W₁ i 1 L₁) hsl
              refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
              rw [S₁, except_bind_ok]
              dsimp only
              rw [hwa, except_bind_ok, if_pos hbool, if_pos hlt, R₂]
          case bitSlice =>
              rcases args with _ | ⟨argE, _ | ⟨jE, _ | ⟨iE, _ | ⟨a4, rest⟩⟩⟩⟩ <;>
                dsimp only at hc
              · exact error_ne_ok hc
              · exact error_ne_ok hc
              · exact error_ne_ok hc
              rotate_left
              · exact error_ne_ok hc
              dsimp only
              cases hfj : finLitE jE with
              | none => rw [hfj] at hc; exact error_ne_ok hc
              | some j =>
              rw [hfj] at hc
              cases hfi : finLitE iE with
              | none => rw [hfi] at hc; exact error_ne_ok hc
              | some i =>
              rw [hfi] at hc
              dsimp only at hc ⊢
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hij
              rw [bind_ok_iff] at hc
              obtain ⟨dat, hargE, hc⟩ := hc
              obtain ⟨d₁, a, ta⟩ := dat
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨wa, hwa, hc⟩ := hc
              rw [bind_ok_iff] at hc
              obtain ⟨mr, hmr, hc⟩ := hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hguard
              rcases hsl : sliceNFD d₁ i (j + 1 - i) a with ⟨d₂, r₂⟩
              rw [hsl] at hc
              obtain ⟨hd2, hr2, hty2⟩ := triple_eq hc
              subst hd2; subst hr2; subst hty2
              obtain ⟨W₁, E₁, L₁, S₁⟩ := ih ΓD jΓD argE [] d d₁ a ta ΓS jΓS
                hwf hG hJ (by simp) hargE
              simp only [List.map_nil] at S₁
              obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (sliceNFD_spec W₁ i (j + 1 - i) L₁) hsl
              refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
              rw [if_pos hij, S₁, except_bind_ok]
              dsimp only
              rw [hwa, except_bind_ok, hmr, except_bind_ok, if_pos hguard, R₂]
          case vecMap =>
              rcases args with _ | ⟨fE, _ | ⟨argE, _ | ⟨a3, rest⟩⟩⟩ <;> dsimp only at hc
              · exact error_ne_ok hc
              · exact error_ne_ok hc
              rotate_left
              · exact error_ne_ok hc
              dsimp only
              rw [bind_ok_iff] at hc
              obtain ⟨nte, hvle, hc⟩ := hc
              obtain ⟨nElems, teO⟩ := nte
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨seO, hseO, hc⟩ := hc
              rw [bind_ok_iff] at hc
              obtain ⟨dat, hargE, hc⟩ := hc
              obtain ⟨d₁, a, ta⟩ := dat
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨lte, hvle2, hc⟩ := hc
              obtain ⟨len, teI⟩ := lte
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨seI, hseI, hc⟩ := hc
              rw [bind_ok_iff] at hc
              obtain ⟨szV, hszV, hc⟩ := hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hlenE
              obtain ⟨W₁, E₁, L₁, S₁⟩ := ih ΓD jΓD argE [] d d₁ a ta ΓS jΓS
                hwf hG hJ (by simp) hargE
              simp only [List.map_nil] at S₁
              rcases hpends : cpendVecD d₁ szV seI a (List.range nElems) with ⟨d₂, pends⟩
              rw [hpends] at hc
              dsimp only at hc
              have hcp := cpendVecD_spec (szV := szV) (seI := seI) (a := a)
                (List.range nElems) d₁ W₁ L₁
              rw [hpends] at hcp
              dsimp only at hcp
              obtain ⟨W₂, E₂, Lp, Rp⟩ := hcp
              rw [bind_ok_iff] at hc
              obtain ⟨dps, hel, hc⟩ := hc
              obtain ⟨d₃, ps⟩ := dps
              dsimp only at hc
              have hplen : pends.length = (List.range nElems).length := by
                have := congrArg List.length Rp
                simpa using this
              have hreads : ∀ j (h1 : j < pends.length)
                  (h2 : j < (List.range nElems).length),
                  d₂.read (pends[j]'h1)
                    = (fun i => (sliceNF (szV - (i + 1) * seI) seI (d₁.read a) : NF))
                        ((List.range nElems)[j]'h2) := by
                intro j h1 h2
                have hj := List.getElem_of_eq Rp (by simpa using h1)
                simpa using hj
              obtain ⟨W₃, E₃, L₃, S₃⟩ := celemsD_sim (fE := fE) (teI := teI) (teO := teO)
                (seO := seO) (pf := fun i => sliceNF (szV - (i + 1) * seI) seI (d₁.read a))
                (emsg := "rwPrimVecMap: element result-type mismatch")
                (ΓS := ΓS) (jΓS := jΓS) ih
                (List.range nElems) pends d₂ d₃ ps W₂ (hG.mono (E₁.trans E₂))
                (jsimEnv_mono hJ (E₁.trans E₂)) Lp hplen hreads hel
              rcases hcat : catNFD d₃ ps with ⟨d₄, rr⟩
              rw [hcat] at hc
              obtain ⟨hd4, hrr, hty4⟩ := triple_eq hc
              subst hd4; subst hrr; subst hty4
              obtain ⟨W₄, E₄, L₄, R₄⟩ := mk_out (catNFD_spec W₃ L₃) hcat
              refine ⟨W₄, ((E₁.trans E₂).trans E₃).trans E₄, L₄, ?_⟩
              rw [hvle, except_bind_ok]
              dsimp only
              rw [hseO, except_bind_ok, S₁, except_bind_ok]
              dsimp only
              rw [hvle2, except_bind_ok]
              dsimp only
              rw [hseI, except_bind_ok, hszV, except_bind_ok, if_pos hlenE,
                  S₃, except_bind_ok, R₄]
          case vecGenerate =>
              rcases args with _ | ⟨fE, _ | ⟨a2, rest⟩⟩ <;> dsimp only at hc
              · exact error_ne_ok hc
              rotate_left
              · exact error_ne_ok hc
              dsimp only
              rw [bind_ok_iff] at hc
              obtain ⟨nte, hvle, hc⟩ := hc
              obtain ⟨nElems, teO⟩ := nte
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨seO, hseO, hc⟩ := hc
              rw [bind_ok_iff] at hc
              obtain ⟨ft, hft, hc⟩ := hc
              cases ft <;> dsimp only at hc <;> try exact error_ne_ok hc
              rename_i tFin tRes
              rw [bind_ok_iff] at hc
              obtain ⟨nb, hnb, hc⟩ := hc
              split at hc
              rotate_left
              · exact error_ne_ok hc
              rename_i hnbE
              rcases hpends : cpendGenD d nElems (List.range nElems) with ⟨d₁, pends⟩
              rw [hpends] at hc
              dsimp only at hc
              have hcp := cpendGenD_spec (nElems := nElems) (List.range nElems) d hwf
              rw [hpends] at hcp
              dsimp only at hcp
              obtain ⟨W₁, E₁, Lp, Rp⟩ := hcp
              rw [bind_ok_iff] at hc
              obtain ⟨dps, hel, hc⟩ := hc
              obtain ⟨d₂, ps⟩ := dps
              dsimp only at hc
              have hplen : pends.length = (List.range nElems).length := by
                have := congrArg List.length Rp
                simpa using this
              have hreads : ∀ j (h1 : j < pends.length)
                  (h2 : j < (List.range nElems).length),
                  d₁.read (pends[j]'h1)
                    = (fun i => (NF.lit ⟨nbits nElems, BitVec.ofNat (nbits nElems) i⟩ : NF))
                        ((List.range nElems)[j]'h2) := by
                intro j h1 h2
                have hj := List.getElem_of_eq Rp (by simpa using h1)
                simpa using hj
              obtain ⟨W₂, E₂, L₂, S₂⟩ := celemsD_sim (fE := fE) (teI := tFin) (teO := teO)
                (seO := seO)
                (pf := fun i => (.lit ⟨nbits nElems, BitVec.ofNat (nbits nElems) i⟩ : NF))
                (emsg := "rwPrimVecGenerate: element result-type mismatch")
                (ΓS := ΓS) (jΓS := jΓS) ih
                (List.range nElems) pends d₁ d₂ ps W₁ (hG.mono E₁)
                (jsimEnv_mono hJ E₁) Lp hplen hreads hel
              rcases hcat : catNFD d₂ ps with ⟨d₃, rr⟩
              rw [hcat] at hc
              obtain ⟨hd3, hrr, hty3⟩ := triple_eq hc
              subst hd3; subst hrr; subst hty3
              obtain ⟨W₃, E₃, L₃, R₃⟩ := mk_out (catNFD_spec W₂ L₂) hcat
              refine ⟨W₃, (E₁.trans E₂).trans E₃, L₃, ?_⟩
              rw [hvle, except_bind_ok]
              dsimp only
              rw [hseO, except_bind_ok, hft, except_bind_ok]
              dsimp only
              rw [hnb, except_bind_ok, if_pos hnbE, S₂, except_bind_ok, R₃]
          case cryptol =>
            -- The foreign row's DAG mirror (stage B): `symExpDag` over
            -- the shared store certifies the tree arm's `symExp` run
            -- (`symExpDag_sim` through the zip environment simulation),
            -- and the store-level extern-freedom gates transport to
            -- the tree gates by `xcallFreeIdx_read`.
            dsimp only at hc ⊢
            split at hc
            all_goals try exact error_ne_ok hc
            rename_i f' n' impl rest
            try dsimp only at hc ⊢
            rw [bind_ok_iff] at hc
            obtain ⟨ity, hity, hc⟩ := hc
            rw [hity, except_bind_ok]
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hlen
            rw [if_pos hlen]
            cases hcry : Δ.cryD f' n' ity with
            | none => rw [hcry] at hc; exact error_ne_ok hc
            | some g =>
            rw [hcry] at hc
            try dsimp only at hc ⊢
            cases hgd : Δ.hyleDefs.get? g with
            | none => rw [hgd] at hc; exact error_ne_ok hc
            | some hd =>
            rw [hgd] at hc
            try dsimp only at hc ⊢
            rw [bind_ok_iff] at hc
            obtain ⟨dp, hargs, hc⟩ := hc
            obtain ⟨d₁, pas⟩ := dp
            try dsimp only at hc
            obtain ⟨W₁, E₁, L₁, S₁⟩ := cargsD_sim ih rest d d₁ pas hwf hG hJ hargs
            rw [S₁, except_bind_ok]
            try dsimp only
            have hmm : (pas.map fun p => ((d₁.read p.1 : NF), p.2)).map (·.1)
                = (pas.map (·.1)).map d₁.read := by
              rw [List.map_map, List.map_map]
              rfl
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hplen
            rw [if_pos (by
              rw [List.length_map]
              simpa using hplen)]
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hargsFree
            simp only [Rwv.Hyle.BridgeDag.Dag.xcallFreeM_eq] at hargsFree
            rw [if_pos (by
              rw [hmm]
              rw [List.all_eq_true]
              intro n hn
              obtain ⟨q, hq, hqn⟩ := List.mem_map.mp hn
              obtain ⟨pq, hpq, hpqn⟩ := List.mem_map.mp hq
              rw [← hqn, ← hpqn]
              exact Rwv.Hyle.BridgeDag.Dag.xcallFreeIdx_read _
                (List.all_eq_true.mp hargsFree pq hpq))]
            rw [bind_ok_iff] at hc
            obtain ⟨dpi, hsymD, hc⟩ := hc
            obtain ⟨d₂, i⟩ := dpi
            dsimp only at hc
            have henvsim : Rwv.Hyle.BridgeDag.EnvSim d₁
                (HashMap.ofList (hd.params.zip (pas.map (·.1))))
                (HashMap.ofList (hd.params.zip ((pas.map (·.1)).map d₁.read))) :=
              Rwv.Hyle.BridgeDag.EnvSim.zip hd.params (pas.map (·.1))
                (fun n hn => by
                  obtain ⟨q, hq, hqn⟩ := List.mem_map.mp hn
                  rw [← hqn]
                  exact L₁ q hq)
            obtain ⟨W₂, E₂, L₂, S₂⟩ := Rwv.Hyle.BridgeDag.symExpDag_sim Δ.hyleFuel hd.body
              _ _ d₁ d₂ i W₁ henvsim hsymD
            rw [hmm, S₂, except_bind_ok]
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hfreeD
            rw [Rwv.Hyle.BridgeDag.Dag.xcallFreeM_eq] at hfreeD
            rw [if_pos (Rwv.Hyle.BridgeDag.Dag.xcallFreeIdx_read _ hfreeD)]
            obtain ⟨hd2, hri, hty2⟩ := triple_eq hc
            subst hd2; subst hri; subst hty2
            exact ⟨W₂, E₁.trans E₂, L₂, rfl⟩
          case «extern» =>
            -- The model-less extern row's DAG mirror (stage B): the
            -- packed-argument node is built by the hash-consed
            -- constructors, whose specs read back to exactly the tree
            -- arm's normal form.
            dsimp only at hc ⊢
            split at hc
            all_goals try exact error_ne_ok hc
            rename_i _ps _clk _rst _as _rs sx _impl _inst rest
            try dsimp only at hc ⊢
            rw [bind_ok_iff] at hc
            obtain ⟨ity, hity, hc⟩ := hc
            rw [hity, except_bind_ok]
            split at hc
            rotate_left
            · exact error_ne_ok hc
            rename_i hlen
            rw [if_pos hlen]
            cases hhx : Δ.hyleX.get? sx with
            | some m => rw [hhx] at hc; exact error_ne_ok hc
            | none =>
            rw [hhx] at hc
            try dsimp only at hc ⊢
            rw [bind_ok_iff] at hc
            obtain ⟨dp, hargs, hc⟩ := hc
            obtain ⟨d₁, pas⟩ := dp
            try dsimp only at hc
            obtain ⟨W₁, E₁, L₁, S₁⟩ := cargsD_sim ih rest d d₁ pas hwf hG hJ hargs
            rw [S₁, except_bind_ok]
            try dsimp only
            rw [bind_ok_iff] at hc
            obtain ⟨w, hw, hc⟩ := hc
            rw [hw, except_bind_ok]
            rcases hxp : d₁.xpackD (pas.map (·.1)) with ⟨d₂, ip⟩
            rw [hxp] at hc
            dsimp only at hc
            rcases hxc : d₂.mkXcallD w sx ip with ⟨d₃, rr⟩
            rw [hxc] at hc
            dsimp only at hc
            obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (Rwv.Hyle.BridgeDag.Dag.xpackD_spec W₁
              (fun n hn => by
                obtain ⟨q, hq, hqn⟩ := List.mem_map.mp hn
                rw [← hqn]
                exact L₁ q hq)) hxp
            obtain ⟨W₃, E₃, L₃, R₃⟩ := mk_out (Rwv.Hyle.BridgeDag.Dag.mkXcallD_spec W₂ L₂) hxc
            obtain ⟨hd3, hrr, hty3⟩ := triple_eq hc
            subst hd3; subst hrr; subst hty3
            refine ⟨W₃, (E₁.trans E₂).trans E₃, L₃, ?_⟩
            rw [R₃, R₂]
            have hmm : (pas.map fun p => ((d₁.read p.1 : NF), p.2)).map (·.1)
                = (pas.map (·.1)).map d₁.read := by
              rw [List.map_map, List.map_map]
              rfl
            rw [hmm]
          all_goals
            dsimp only at hc ⊢
            rw [bind_ok_iff] at hc
            obtain ⟨dp, hargs, hc⟩ := hc
            obtain ⟨d₁, pas⟩ := dp
            dsimp only at hc
            obtain ⟨W₁, E₁, L₁, S₁⟩ := cargsD_sim ih args d d₁ pas hwf hG hJ hargs
            obtain ⟨W₂, E₂, L₂, S₂⟩ := cprimFD_sim W₁ L₁ hc
            exact ⟨W₂, E₁.trans E₂, L₂, by rw [S₁, except_bind_ok]; exact S₂⟩
      | litInt ty0 n =>
          cases args with
          | cons a as =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
              cases pendD with
              | cons p ps =>
                  dsimp only at hc
                  exact error_ne_ok hc
              | nil =>
                  dsimp only at hc ⊢
                  simp only [List.map_nil]
                  obtain ⟨W, E, L, R⟩ := clitIntD_sim hwf hc
                  exact ⟨W, E, L, R⟩
      | litStr s =>
          dsimp only at hc
          exact error_ne_ok hc
      | litList tyl es =>
          dsimp only at hc
          exact error_ne_ok hc
      | litVec vty es =>
          cases args with
          | cons a as =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
              cases pendD with
              | cons p ps =>
                  dsimp only at hc
                  exact error_ne_ok hc
              | nil =>
                  dsimp only at hc ⊢
                  simp only [List.map_nil]
                  rw [bind_ok_iff] at hc
                  obtain ⟨dp, hargs, hc⟩ := hc
                  obtain ⟨d₁, pas⟩ := dp
                  dsimp only at hc
                  obtain ⟨W₁, E₁, L₁, S₁⟩ := cargsD_sim ih es d d₁ pas hwf hG hJ hargs
                  rw [S₁, except_bind_ok]
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i nt te hflat
                  cases hev : Ty.evalNat nt with
                  | none => rw [hev] at hc; exact error_ne_ok hc
                  | some k =>
                  rw [hev] at hc
                  dsimp only at hc ⊢
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i hlen
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i hteqs
                  rw [bind_ok_iff] at hc
                  obtain ⟨se, hse, hc⟩ := hc
                  rcases hcat : catNFD d₁ ((pas.map (·.1)).map (·, se)) with ⟨d₂, rr⟩
                  rw [hcat] at hc
                  obtain ⟨hd2, hrr, hty⟩ := triple_eq hc
                  subst hd2; subst hrr; subst hty
                  obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (catNFD_spec W₁ (by
                    intro q hq
                    obtain ⟨a, ha, hqa⟩ := List.mem_map.mp hq
                    obtain ⟨p, hp, hap⟩ := List.mem_map.mp ha
                    rw [← hqa]
                    dsimp only
                    rw [← hap]
                    exact L₁ p hp)) hcat
                  refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
                  rw [if_pos (by simpa using hlen)]
                  rw [teqAllD_eq d₁] at hteqs
                  rw [if_pos (by
                        rw [List.length_map]
                        exact hteqs),
                      hse, except_bind_ok, R₂, map_se_reads]
      | lam x b =>
          dsimp only at hc ⊢
          rw [bind_ok_iff] at hc
          obtain ⟨dp, hargs, hc⟩ := hc
          obtain ⟨d₁, pas⟩ := dp
          dsimp only at hc
          obtain ⟨W₁, E₁, L₁, S₁⟩ := cargsD_sim ih args d d₁ pas hwf hG hJ hargs
          rw [S₁, except_bind_ok]
          rw [show (pendD.map fun p => ((d.read p.1 : NF), p.2))
                = pendD.map fun p => ((d₁.read p.1 : NF), p.2) from
              (pend_read_ext E₁ hpend).symm,
              ← List.map_append]
          cases hpp : pas ++ pendD with
          | nil =>
              rw [hpp] at hc
              exact error_ne_ok hc
          | cons nt rest =>
              rw [hpp] at hc
              obtain ⟨ni, t0⟩ := nt
              have hrange : ∀ p ∈ (ni, t0) :: rest, p.1 < d₁.size := by
                rw [← hpp]
                intro p hp
                rcases List.mem_append.mp hp with hp | hp
                · exact L₁ p hp
                · exact Nat.lt_of_lt_of_le (hpend p hp) E₁.size_le
              have hni : ni < d₁.size := hrange (ni, t0) List.mem_cons_self
              obtain ⟨W₂, E₂, L₂, S₂⟩ := ih (ΓD.insert x.uniq (ni, t0)) [] b rest d₁ d' r ty
                (ΓS.insert x.uniq (d₁.read ni, t0)) []
                W₁ ((hG.mono E₁).insert x.uniq hni) jsimEnv_nil
                (fun p hp => hrange p (List.mem_cons_of_mem _ hp)) hc
              refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
              rw [List.map_cons]
              exact S₂
      | letE bnd body =>
          cases bnd with
          | recB bs =>
              dsimp only at hc
              exact error_ne_ok hc
          | nonRec x rhs =>
              dsimp only at hc ⊢
              rw [bind_ok_iff] at hc
              obtain ⟨dpr, hrhs, hc⟩ := hc
              obtain ⟨d₁, r0, t0⟩ := dpr
              dsimp only at hc
              rw [bind_ok_iff] at hc
              obtain ⟨dp, hargs, hc⟩ := hc
              obtain ⟨d₂, pas⟩ := dp
              dsimp only at hc
              obtain ⟨W₁, E₁, L₁, S₁⟩ := ih ΓD jΓD rhs [] d d₁ r0 t0 ΓS jΓS
                hwf hG hJ (by simp) hrhs
              simp only [List.map_nil] at S₁
              obtain ⟨W₂, E₂, L₂, S₂⟩ := cargsD_sim ih args d₁ d₂ pas W₁
                (hG.mono E₁) (jsimEnv_mono hJ E₁) hargs
              have hr0 : r0 < d₂.size := Nat.lt_of_lt_of_le L₁ E₂.size_le
              have hrange : ∀ p ∈ pas ++ pendD, p.1 < d₂.size := by
                intro p hp
                rcases List.mem_append.mp hp with hp | hp
                · exact L₂ p hp
                · exact Nat.lt_of_lt_of_le (hpend p hp) (E₁.trans E₂).size_le
              obtain ⟨W₃, E₃, L₃, S₃⟩ := ih (ΓD.insert x.uniq (r0, t0)) jΓD body
                (pas ++ pendD) d₂ d' r ty
                (ΓS.insert x.uniq (d₂.read r0, t0)) jΓS
                W₂ ((hG.mono (E₁.trans E₂)).insert x.uniq hr0)
                (jsimEnv_mono hJ (E₁.trans E₂)) hrange hc
              refine ⟨W₃, (E₁.trans E₂).trans E₃, L₃, ?_⟩
              rw [S₁, except_bind_ok, S₂, except_bind_ok]
              rw [show (pendD.map fun p => ((d.read p.1 : NF), p.2))
                    = pendD.map fun p => ((d₂.read p.1 : NF), p.2) from
                  (pend_read_ext (E₁.trans E₂) hpend).symm,
                  ← List.map_append,
                  show d₁.read r0 = d₂.read r0 from (read_ext E₂ r0 L₁).symm]
              exact S₃
          | join l ps jb =>
              dsimp only at hc ⊢
              rw [bind_ok_iff] at hc
              obtain ⟨dp, hargs, hc⟩ := hc
              obtain ⟨d₁, pas⟩ := dp
              dsimp only at hc
              obtain ⟨W₁, E₁, L₁, S₁⟩ := cargsD_sim ih args d d₁ pas hwf hG hJ hargs
              have hrange : ∀ p ∈ pas ++ pendD, p.1 < d₁.size := by
                intro p hp
                rcases List.mem_append.mp hp with hp | hp
                · exact L₁ p hp
                · exact Nat.lt_of_lt_of_le (hpend p hp) E₁.size_le
              obtain ⟨W₂, E₂, L₂, S₂⟩ := ih ΓD ((l.uniq, .mk ps ΓD jΓD jb) :: jΓD) body
                (pas ++ pendD) d₁ d' r ty ΓS ((l.uniq, .mk ps ΓS jΓS jb) :: jΓS)
                W₁ (hG.mono E₁)
                (jsimEnv_cons (jsimEnv_mono hJ E₁)
                  (jsimC_intro (hG.mono E₁) (jsimEnv_mono hJ E₁)))
                hrange hc
              refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
              rw [S₁, except_bind_ok]
              rw [show (pendD.map fun p => ((d.read p.1 : NF), p.2))
                    = pendD.map fun p => ((d₁.read p.1 : NF), p.2) from
                  (pend_read_ext E₁ hpend).symm,
                  ← List.map_append]
              exact S₂
      | jump l es =>
          cases args with
          | cons a as =>
              dsimp only at hc
              exact error_ne_ok hc
          | nil =>
              dsimp only at hc ⊢
              rw [bind_ok_iff] at hc
              obtain ⟨dp, hargs, hc⟩ := hc
              obtain ⟨d₁, pas⟩ := dp
              dsimp only at hc
              obtain ⟨W₁, E₁, L₁, S₁⟩ := cargsD_sim ih es d d₁ pas hwf hG hJ hargs
              rw [S₁, except_bind_ok]
              cases hjx : jΓD.lookup l.uniq with
              | none => rw [hjx] at hc; exact error_ne_ok hc
              | some cj =>
                  rw [hjx] at hc
                  obtain ⟨cjS, hjS, hsim⟩ := hJ l.uniq cj hjx
                  rw [hjS]
                  obtain ⟨ps, ΓcD, jcD, jb⟩ := cj
                  dsimp only at hc
                  split at hc
                  rotate_left
                  · exact error_ne_ok hc
                  rename_i hteqs
                  cases hsim with
                  | mk hGc hcov hpt =>
                  dsimp only
                  rw [teqAllD_eq d₁] at hteqs
                  rw [if_pos hteqs]
                  have hJc : JSimEnv d₁ jcD _ :=
                    jsimEnv_mono (jsimEnv_of_jsimC
                      (JSimC.mk (ps := ps) (body := jb) hGc hcov hpt)) E₁
                  obtain ⟨W₂, E₂, L₂, S₂⟩ := ih (bindFieldsΓD ps pas ΓcD) jcD jb
                    pendD d₁ d' r ty _ _
                    W₁ (gsim_bindFields ps (hGc.mono E₁) L₁) hJc
                    (fun p hp => Nat.lt_of_lt_of_le (hpend p hp) E₁.size_le) hc
                  refine ⟨W₂, E₁.trans E₂, L₂, ?_⟩
                  rw [show (pendD.map fun p => ((d.read p.1 : NF), p.2))
                        = pendD.map fun p => ((d₁.read p.1 : NF), p.2) from
                      (pend_read_ext E₁ hpend).symm]
                  exact S₂
      | cases resTy scrut binder alts =>
          dsimp only at hc ⊢
          rw [bind_ok_iff] at hc
          obtain ⟨dnt, hscrut, hc⟩ := hc
          obtain ⟨d₁, dn, dty⟩ := dnt
          dsimp only at hc
          rw [bind_ok_iff] at hc
          obtain ⟨szT, hsz, hc⟩ := hc
          rw [bind_ok_iff] at hc
          obtain ⟨dp, hargs, hc⟩ := hc
          obtain ⟨d₂, pas⟩ := dp
          dsimp only at hc
          rw [bind_ok_iff] at hc
          obtain ⟨resTy', hpeel, hc⟩ := hc
          obtain ⟨W₁, E₁, L₁, S₁⟩ := ih ΓD jΓD scrut [] d d₁ dn dty ΓS jΓS
            hwf hG hJ (by simp) hscrut
          simp only [List.map_nil] at S₁
          obtain ⟨W₂, E₂, L₂, S₂⟩ := cargsD_sim ih args d₁ d₂ pas W₁
            (hG.mono E₁) (jsimEnv_mono hJ E₁) hargs
          have hdn2 : dn < d₂.size := Nat.lt_of_lt_of_le L₁ E₂.size_le
          have hrange : ∀ p ∈ pas ++ pendD, p.1 < d₂.size := by
            intro p hp
            rcases List.mem_append.mp hp with hp | hp
            · exact L₂ p hp
            · exact Nat.lt_of_lt_of_le (hpend p hp) (E₁.trans E₂).size_le
          rw [S₁, except_bind_ok]
          dsimp only
          rw [hsz, except_bind_ok, S₂, except_bind_ok]
          rw [show d₁.read dn = d₂.read dn from (read_ext E₂ dn L₁).symm,
              show (pendD.map fun p => ((d.read p.1 : NF), p.2))
                = pendD.map fun p => ((d₂.read p.1 : NF), p.2) from
              (pend_read_ext (E₁.trans E₂) hpend).symm,
              ← List.map_append, List.length_map, hpeel, except_bind_ok]
          have hGins : GSim d₂ (ΓD.insert binder.uniq (dn, dty))
              (ΓS.insert binder.uniq (d₂.read dn, dty)) :=
            (hG.mono (E₁.trans E₂)).insert binder.uniq hdn2
          rcases alts with _ | ⟨⟨con0, bs0, dbody⟩, rest⟩
          · dsimp only at hc
            rw [cchainJD] at hc
            exact error_ne_ok hc
          · cases con0 with
            | default =>
                dsimp only at hc ⊢
                rw [bind_ok_iff] at hc
                obtain ⟨dnt2, hdb, hc⟩ := hc
                obtain ⟨d₃, dnf, dbt⟩ := dnt2
                dsimp only at hc
                split at hc
                rotate_left
                · exact error_ne_ok hc
                rename_i hdteq
                obtain ⟨W₃, E₃, L₃, S₃⟩ := ih (ΓD.insert binder.uniq (dn, dty)) jΓD
                  dbody (pas ++ pendD) d₂ d₃ dnf dbt _ jΓS
                  W₂ hGins (jsimEnv_mono hJ (E₁.trans E₂)) hrange hdb
                obtain ⟨W₄, E₄, L₄, S₄⟩ := cchainJD_sim ih rest (some dnf)
                  (some (d₃.read dnf)) d₃ d' r ty
                  W₃ (hGins.mono E₃) (jsimEnv_mono hJ ((E₁.trans E₂).trans E₃))
                  (fun p hp => Nat.lt_of_lt_of_le (hrange p hp) E₃.size_le)
                  (Nat.lt_of_lt_of_le hdn2 E₃.size_le) ⟨L₃, rfl⟩ hc
                refine ⟨W₄, ((E₁.trans E₂).trans E₃).trans E₄, L₄, ?_⟩
                rw [S₃, except_bind_ok]
                dsimp only
                rw [if_pos hdteq]
                rw [read_ext E₃ dn hdn2] at S₄
                rw [show ((pas ++ pendD).map fun p => ((d₂.read p.1 : NF), p.2))
                      = (pas ++ pendD).map fun p => ((d₃.read p.1 : NF), p.2) from
                    (pend_read_ext E₃ hrange).symm]
                exact S₄
            | dataAlt cn =>
                dsimp only at hc ⊢
                obtain ⟨W₄, E₄, L₄, S₄⟩ := cchainJD_sim ih
                  (Alt.mk (.dataAlt cn) bs0 dbody :: rest) none none d₂ d' r ty
                  W₂ hGins (jsimEnv_mono hJ (E₁.trans E₂)) hrange hdn2 trivial hc
                refine ⟨W₄, (E₁.trans E₂).trans E₄, L₄, ?_⟩
                exact S₄
            | litAlt i =>
                dsimp only at hc ⊢
                obtain ⟨W₄, E₄, L₄, S₄⟩ := cchainJD_sim ih
                  (Alt.mk (.litAlt i) bs0 dbody :: rest) none none d₂ d' r ty
                  W₂ hGins (jsimEnv_mono hJ (E₁.trans E₂)) hrange hdn2 trivial hc
                refine ⟨W₄, (E₁.trans E₂).trans E₄, L₄, ?_⟩
                exact S₄
      | app f' a' =>
          dsimp only at hc
          exact error_ne_ok hc

/-- `cexpFullD`'s simulation, at the top level (empty join
environment, no pending arguments): a successful DAG compilation
certifies the tree compilation whose normal form is the reading of
the returned index. -/
theorem cexpFullD_sim {Δ : DEnv} {dmap : HashMap Int Defn} {fuel : Nat}
    {ΓD : DGamma} {ΓS : HashMap Int (NF × Ty)} {e : Exp} {d d' : Dag} {r : Nat}
    {ty : Ty} (hwf : d.WF) (hG : GSim d ΓD ΓS)
    (hc : cexpFullD Δ dmap fuel ΓD e d = .ok (d', r, ty)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧
      cexpFull Δ dmap fuel ΓS e = .ok (d'.read r, ty) := by
  obtain ⟨W, E, L, S⟩ := cexpJD_sim fuel ΓD [] e [] d d' r ty ΓS []
    hwf hG jsimEnv_nil (by simp) hc
  simp only [List.map_nil] at S
  exact ⟨W, E, L, S⟩

/-! ### The per-definition DAG leg (Phase 4d): compile both sides into
one shared store over pushed parameter variables and compare by index
after three `renorm` sweeps (the store-level `cfoldW3`). Soundness is
a reduction to the tree legs: a `true` verdict certifies both tree
compilations and the `cfoldW3` equality of their normal forms, so
`checkDefnPair_sound`'s width-aware argument applies unchanged. -/

/-- Push a run of variable nodes (name/width pairs), keeping the
returned indices in order. -/
def pushVars (d : Dag) : List (String × Nat) → Dag × List Nat
  | [] => (d, [])
  | (x, w) :: rest =>
      let (d₁, i) := d.mkVar w x
      let (d₂, is) := pushVars d₁ rest
      (d₂, i :: is)

private theorem pushVars_spec :
    ∀ (prs : List (String × Nat)) (d : Dag), d.WF →
      (pushVars d prs).1.WF ∧ d.Ext (pushVars d prs).1 ∧
      (∀ i ∈ (pushVars d prs).2, i < (pushVars d prs).1.size) ∧
      (pushVars d prs).2.map (pushVars d prs).1.read
        = prs.map (fun p => (.var p.2 p.1 : NF)) := by
  intro prs
  induction prs with
  | nil =>
      intro d hwf
      exact ⟨hwf, Rwv.Hyle.BridgeDag.Dag.Ext.refl d, by simp [pushVars], by simp [pushVars]⟩
  | cons p rest ih =>
      intro d hwf
      obtain ⟨x, w⟩ := p
      show _ ∧ _ ∧ _ ∧ _
      rw [pushVars]
      rcases hv : d.mkVar w x with ⟨d₁, i⟩
      obtain ⟨W₁, E₁, L₁, R₁⟩ := mk_out (mkVar_spec hwf w x) hv
      obtain ⟨W₂, E₂, L₂, R₂⟩ := ih d₁ W₁
      rcases hr : pushVars d₁ rest with ⟨d₂, is⟩
      rw [hr] at W₂ E₂ L₂ R₂
      simp only [hv, hr]
      refine ⟨W₂, E₁.trans E₂, ?_, ?_⟩
      · intro q hq
        rcases List.mem_cons.mp hq with hq | hq
        · subst hq
          exact Nat.lt_of_lt_of_le L₁ E₂.size_le
        · exact L₂ q hq
      · rw [List.map_cons, List.map_cons, R₂, read_ext E₂ i L₁, R₁]

/-- The parameter environment's simulation (`mkParamGamma`'s fold with
indices whose readings are the parameter variables). -/
private theorem gsim_paramFold {d : Dag} :
    ∀ (eps : List Id) (is : List Nat) (nfs : List NF),
      (∀ i ∈ is, i < d.size) →
      is.map d.read = nfs →
      GSim d ((eps.zip is).foldr (fun pr m => m.insert pr.1.uniq (pr.2, pr.1.sig.ty)) ∅)
        ((eps.zip nfs).foldr (fun pr m => m.insert pr.1.uniq (pr.2, pr.1.sig.ty)) ∅) := by
  intro eps
  induction eps with
  | nil =>
      intro is nfs _ _
      simpa using gsim_empty d
  | cons e eps ih =>
      intro is nfs hlt hmap
      cases is with
      | nil =>
          rw [← hmap]
          simpa using gsim_empty d
      | cons i irest =>
          cases nfs with
          | nil => exact absurd hmap (by simp)
          | cons nf nrest =>
              rw [List.map_cons] at hmap
              injection hmap with h1 h2
              simp only [List.zip_cons_cons, List.foldr_cons]
              rw [← h1]
              exact GSim.insert
                (ih irest nrest (fun q hq => hlt q (List.mem_cons_of_mem _ hq)) h2)
                e.uniq (hlt i List.mem_cons_self)

/-- The Hyle-side parameter environment's simulation (`mkParamRho`'s
fold with indices whose readings are the parameter variables). -/
private theorem envsim_paramFold {d : Dag} :
    ∀ (prs : List (String × Nat)) (is : List Nat)
      (mD : HashMap String Nat) (mS : HashMap String NF),
      Rwv.Hyle.BridgeDag.EnvSim d mD mS →
      (∀ i ∈ is, i < d.size) →
      is.map d.read = prs.map (fun p => (.var p.2 p.1 : NF)) →
      Rwv.Hyle.BridgeDag.EnvSim d
        ((prs.zip is).foldl (fun m pr => m.insert pr.1.1 pr.2) mD)
        (prs.foldl (fun m pr => m.insert pr.1 (.var pr.2 pr.1)) mS) := by
  intro prs
  induction prs with
  | nil =>
      intro is mD mS hsim _ _
      simpa using hsim
  | cons p rest ih =>
      intro is mD mS hsim hlt hmap
      cases is with
      | nil => exact absurd hmap (by simp)
      | cons i irest =>
          simp only [List.map_cons] at hmap
          injection hmap with h1 h2
          simp only [List.zip_cons_cons, List.foldl_cons]
          refine ih irest _ _ ?_ (fun q hq => hlt q (List.mem_cons_of_mem _ hq)) h2
          have := hsim.insert p.1 (hlt i List.mem_cons_self)
          rw [h1] at this
          exact this

/-- The DAG comparison leg of the per-definition validator. -/
def checkDefnPairDag (Δ : DEnv) (edm : HashMap Int Defn)
    (hdm : HashMap String Rwv.Hyle.Defn) (hX : Rwv.Hyle.Sem.XEnv) (fuelE fuelH : Nat)
    (ed : Defn) (hd : Rwv.Hyle.Defn) : Bool :=
  let prs := hd.params.zip hd.sig.params
  let (d₀, is) := pushVars Rwv.Hyle.BridgeDag.Dag.empty prs
  let ΓD : DGamma :=
    (ed.params.zip is).foldr (fun pr m => m.insert pr.1.uniq (pr.2, pr.1.sig.ty)) ∅
  let ρD : HashMap String Nat :=
    (prs.zip is).foldl (fun m pr => m.insert pr.1.1 pr.2) ∅
  match Rwv.Hyle.BridgeDag.symExpDag hdm hX fuelH ρD hd.body d₀ with
  | .error _ => false
  | .ok (d₁, iH) =>
      match cexpFullD Δ edm fuelE ΓD ed.body d₁ with
      | .error _ => false
      | .ok (d₂, iE, _) =>
          match Rwv.Hyle.BridgeDag.renorm d₂ with
          | .error _ => false
          | .ok (e₁, m₁) =>
              match Rwv.Hyle.BridgeDag.renorm e₁ with
              | .error _ => false
              | .ok (e₂, m₂) =>
                  match Rwv.Hyle.BridgeDag.renorm e₂ with
                  | .error _ => false
                  | .ok (_, m₃) =>
                      mIdx m₃ (mIdx m₂ (mIdx m₁ iE))
                        == mIdx m₃ (mIdx m₂ (mIdx m₁ iH))

/-- Soundness of the DAG leg, by reduction: a `true` verdict certifies
both tree compilations and the `cfoldW3` equality of their normal
forms (`renorm_spec` three times = the store-level `cfoldW3`). -/
theorem checkDefnPairDag_sound {Δ : DEnv} {edm : HashMap Int Defn}
    {hdm : HashMap String Rwv.Hyle.Defn} {hX : Rwv.Hyle.Sem.XEnv} {fuelE fuelH : Nat}
    {ed : Defn} {hd : Rwv.Hyle.Defn}
    (h : checkDefnPairDag Δ edm hdm hX fuelE fuelH ed hd = true) :
    ∃ ne tyE nh,
      cexpFull Δ edm fuelE (mkParamGamma ed.params hd.params hd.sig.params) ed.body
        = .ok (ne, tyE)
      ∧ Rwv.Hyle.Bridge.symExp hdm hX fuelH (mkParamRho hd.params hd.sig.params) hd.body
        = .ok nh
      ∧ Rwv.Hyle.Bridge.cfoldW3 ne = Rwv.Hyle.Bridge.cfoldW3 nh := by
  rw [checkDefnPairDag] at h
  rcases hpv : pushVars Rwv.Hyle.BridgeDag.Dag.empty (hd.params.zip hd.sig.params)
    with ⟨d₀, is⟩
  rw [hpv] at h
  dsimp only at h
  have hps := pushVars_spec (hd.params.zip hd.sig.params) Rwv.Hyle.BridgeDag.Dag.empty
    Rwv.Hyle.BridgeDag.Dag.WF.empty
  rw [hpv] at hps
  dsimp only at hps
  obtain ⟨W₀, _E₀, L₀, R₀⟩ := hps
  cases hsE : Rwv.Hyle.BridgeDag.symExpDag hdm hX fuelH
      ((((hd.params.zip hd.sig.params)).zip is).foldl
        (fun m pr => m.insert pr.1.1 pr.2) ∅) hd.body d₀ with
  | error e => rw [hsE] at h; exact absurd h (by simp)
  | ok res =>
  obtain ⟨d₁, iH⟩ := res
  rw [hsE] at h
  dsimp only at h
  cases hcE : cexpFullD Δ edm fuelE
      ((ed.params.zip is).foldr (fun pr m => m.insert pr.1.uniq (pr.2, pr.1.sig.ty)) ∅)
      ed.body d₁ with
  | error e => rw [hcE] at h; exact absurd h (by simp)
  | ok res =>
  obtain ⟨d₂, iE, tyE⟩ := res
  rw [hcE] at h
  dsimp only at h
  cases hr₁ : Rwv.Hyle.BridgeDag.renorm d₂ with
  | error e => rw [hr₁] at h; exact absurd h (by simp)
  | ok res =>
  obtain ⟨e₁, m₁⟩ := res
  rw [hr₁] at h
  dsimp only at h
  cases hr₂ : Rwv.Hyle.BridgeDag.renorm e₁ with
  | error e => rw [hr₂] at h; exact absurd h (by simp)
  | ok res =>
  obtain ⟨e₂, m₂⟩ := res
  rw [hr₂] at h
  dsimp only at h
  cases hr₃ : Rwv.Hyle.BridgeDag.renorm e₂ with
  | error e => rw [hr₃] at h; exact absurd h (by simp)
  | ok res =>
  obtain ⟨e₃, m₃⟩ := res
  rw [hr₃] at h
  dsimp only at h
  -- the two simulations
  have hEnvSim : Rwv.Hyle.BridgeDag.EnvSim d₀
      (((hd.params.zip hd.sig.params).zip is).foldl
        (fun m pr => m.insert pr.1.1 pr.2) ∅)
      (mkParamRho hd.params hd.sig.params) :=
    envsim_paramFold (hd.params.zip hd.sig.params) is ∅ ∅
      (Rwv.Hyle.BridgeDag.EnvSim.empty d₀) L₀ R₀
  obtain ⟨W₁, E₁, LH, SH⟩ :=
    Rwv.Hyle.BridgeDag.symExpDag_sim fuelH hd.body _ _ d₀ d₁ iH W₀ hEnvSim hsE
  have hGSim : GSim d₀
      ((ed.params.zip is).foldr (fun pr m => m.insert pr.1.uniq (pr.2, pr.1.sig.ty)) ∅)
      (mkParamGamma ed.params hd.params hd.sig.params) :=
    gsim_paramFold ed.params is _ L₀ R₀
  obtain ⟨W₂, E₂, LE, SE⟩ := cexpFullD_sim W₁ (hGSim.mono E₁) hcE
  -- the three renormalization sweeps
  obtain ⟨WR₁, ER₁, hsz₁, hinv₁⟩ := Rwv.Hyle.BridgeDag.renorm_spec W₂ hr₁
  obtain ⟨WR₂, ER₂, hsz₂, hinv₂⟩ := Rwv.Hyle.BridgeDag.renorm_spec WR₁ hr₂
  obtain ⟨WR₃, ER₃, hsz₃, hinv₃⟩ := Rwv.Hyle.BridgeDag.renorm_spec WR₂ hr₃
  have hchain : ∀ i, i < d₂.size →
      mIdx m₃ (mIdx m₂ (mIdx m₁ i)) < e₃.size ∧
      e₃.read (mIdx m₃ (mIdx m₂ (mIdx m₁ i))) = Rwv.Hyle.Bridge.cfoldW3 (d₂.read i) := by
    intro i hi
    obtain ⟨h1lt, h1eq⟩ := hinv₁ i hi
    obtain ⟨h2lt, h2eq⟩ := hinv₂ (mIdx m₁ i) h1lt
    obtain ⟨h3lt, h3eq⟩ := hinv₃ (mIdx m₂ (mIdx m₁ i)) h2lt
    refine ⟨h3lt, ?_⟩
    rw [h3eq, h2eq, h1eq]
    rfl
  have hIdx : mIdx m₃ (mIdx m₂ (mIdx m₁ iE)) = mIdx m₃ (mIdx m₂ (mIdx m₁ iH)) :=
    beq_iff_eq.mp h
  have hiH2 : iH < d₂.size := Nat.lt_of_lt_of_le LH E₂.size_le
  refine ⟨d₂.read iE, tyE, d₂.read iH, SE, ?_, ?_⟩
  · rw [SH, read_ext E₂ iH LH]
  · rw [← (hchain iE LE).2, ← (hchain iH hiH2).2, hIdx]

end DagMirror

/-! ## The per-definition validator, upgraded (Phase 4b)

`checkDefnPair` now compiles with the FULL compiler and accepts
either the unconditional `cfold` leg or the width-aware `cfoldW3` leg
— the latter guarded by the parameter-width discipline `paramsOkW`
(matching telescopes, distinct Eidos parameter uniques, and every
Eidos parameter's type sized to its declared Hyle width), which is
what lets the soundness proof discharge `VarsWF (WP σ)` from `EnvC`
alone. -/

def nodupIntB : List Int → Bool
  | [] => true
  | x :: xs => !(xs.contains x) && nodupIntB xs

/-- The parameter-width discipline for the width-aware leg. -/
def paramsOkW (Δ : DEnv) (szf : Nat) (eps : List Id) (hps : List String)
    (hws : List Nat) : Bool :=
  (eps.length == hps.length) && (hps.length == hws.length)
    && nodupIntB (eps.map (·.uniq))
    && ((eps.zip hws).all fun pr =>
          match Δ.sizeOf szf [] pr.1.sig.ty with
          | .ok w => w == pr.2
          | .error _ => false)

/-- The per-definition validator. `true` requires: the datatype
environment passes `denvOk`, both compilations succeed, and the
normal forms coincide after `cfold` — or after `cfoldW3` under the
parameter-width discipline. -/
def checkDefnPair (Δ : DEnv) (edm : HashMap Int Defn)
    (hdm : HashMap String Rwv.Hyle.Defn) (hX : Rwv.Hyle.Sem.XEnv) (fuelE fuelH : Nat)
    (ed : Defn) (hd : Rwv.Hyle.Defn) : Bool :=
  denvOk Δ &&
    -- The DAG leg first: on the pairs whose tree normal forms blow up,
    -- a passing store-level comparison short-circuits the tree legs.
    if paramsOkW Δ fuelE ed.params hd.params hd.sig.params
        && checkDefnPairDag Δ edm hdm hX fuelE fuelH ed hd then true
    else
      match cexpFull Δ edm fuelE (mkParamGamma ed.params hd.params hd.sig.params) ed.body,
            Rwv.Hyle.Bridge.symExp hdm hX fuelH (mkParamRho hd.params hd.sig.params) hd.body with
      | .ok (ne, _), .ok nh =>
          ne.cfold == nh.cfold
            || (paramsOkW Δ fuelE ed.params hd.params hd.sig.params
                && (Rwv.Hyle.Bridge.cfoldW3 ne == Rwv.Hyle.Bridge.cfoldW3 nh))
      | _, _ => false

/-- `HashMap.get?` through a `String`-keyed insert. -/
private theorem get?_insert_str {β : Type} {m : HashMap String β} {k k' : String} {v : β} :
    (m.insert k v).get? k' = if k' = k then some v else m.get? k' := by
  rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert, HashMap.get?_eq_getElem?]
  by_cases h : k' = k
  · simp [h]
  · simp [h, Ne.symm h]

/-- Every binding of `mkParamRho` is a variable at a declared pair. -/
private theorem mkParamRho_mem {hps : List String} {hws : List Nat} {x : String} {n : NF}
    (h : (mkParamRho hps hws).get? x = some n) :
    ∃ w, n = .var w x ∧ (x, w) ∈ hps.zip hws := by
  rw [mkParamRho] at h
  suffices hgen : ∀ (l : List (String × Nat)) (m₀ : HashMap String NF),
      (∀ pr ∈ l, pr ∈ hps.zip hws) →
      (∀ y m, m₀.get? y = some m → ∃ w, m = .var w y ∧ (y, w) ∈ hps.zip hws) →
      ∀ y m, (l.foldl (fun acc pr => acc.insert pr.1 (.var pr.2 pr.1)) m₀).get? y = some m →
        ∃ w, m = .var w y ∧ (y, w) ∈ hps.zip hws by
    refine hgen (hps.zip hws) ∅ (fun pr hpr => hpr) ?_ x n h
    intro y m hm
    rw [HashMap.get?_eq_getElem?] at hm
    simp at hm
  intro l
  induction l with
  | nil =>
      intro m₀ _ hbase y m hm
      exact hbase y m hm
  | cons pr l ih =>
      intro m₀ hsub hbase y m hm
      rw [List.foldl_cons] at hm
      refine ih _ (fun q hq => hsub q (List.mem_cons_of_mem _ hq)) ?_ y m hm
      intro z mz hz
      rw [get?_insert_str] at hz
      by_cases hzp : z = pr.1
      · rw [if_pos hzp] at hz
        injection hz with hz
        subst hz
        subst hzp
        exact ⟨pr.2, rfl, hsub pr List.mem_cons_self⟩
      · rw [if_neg hzp] at hz
        exact hbase z mz hz

/-- Every binding of `mkParamGamma` records a variable at a declared
pair. -/
private theorem mkParamGamma_mem {eps : List Id} {hps : List String} {hws : List Nat}
    {u : Int} {nt : NF × Ty}
    (h : (mkParamGamma eps hps hws).get? u = some nt) :
    ∃ x w, nt.1 = .var w x ∧ (x, w) ∈ hps.zip hws := by
  rw [mkParamGamma] at h
  suffices hgen : ∀ (l : List (Id × NF)),
      (∀ pr ∈ l, ∃ x w, pr.2 = NF.var w x ∧ (x, w) ∈ hps.zip hws) →
      ∀ u nt, (l.foldr (fun pr m => m.insert pr.1.uniq (pr.2, pr.1.sig.ty))
          (∅ : HashMap Int (NF × Ty))).get? u = some nt →
        ∃ x w, nt.1 = .var w x ∧ (x, w) ∈ hps.zip hws by
    refine hgen _ ?_ u nt h
    intro pr hpr
    have h2 := (List.of_mem_zip hpr).2
    obtain ⟨q, hq, hqpr⟩ := List.mem_map.mp h2
    exact ⟨q.1, q.2, hqpr.symm, hq⟩
  intro l
  induction l with
  | nil =>
      intro _ u' nt' h'
      rw [HashMap.get?_eq_getElem?] at h'
      simp at h'
  | cons pr l ih =>
      intro hsub u' nt' h'
      rw [List.foldr_cons, get?_insert] at h'
      by_cases hup : u' = pr.1.uniq
      · rw [if_pos hup] at h'
        injection h' with h'
        subst h'
        obtain ⟨x, w, hx, hmem⟩ := hsub pr List.mem_cons_self
        exact ⟨x, w, hx, hmem⟩
      · rw [if_neg hup] at h'
        exact ih (fun q hq => hsub q (List.mem_cons_of_mem _ hq)) u' nt' h'

/-- With distinct parameter uniques, `mkParamGamma` records exactly
the i-th telescope entry at the i-th parameter's unique. -/
private theorem mkParamGamma_get :
    ∀ {eps : List Id} {hps : List String} {hws : List Nat},
      nodupIntB (eps.map (·.uniq)) = true →
      ∀ {i : Nat} (h1 : i < eps.length) (h2 : i < hps.length) (h3 : i < hws.length),
        (mkParamGamma eps hps hws).get? (eps[i].uniq)
          = some (.var hws[i] hps[i], eps[i].sig.ty) := by
  intro eps
  induction eps with
  | nil =>
      intro hps hws _ i h1 _ _
      exact absurd h1 (by simp)
  | cons e eps ih =>
      intro hps hws hnd i h1 h2 h3
      match hps, hws with
      | h :: hps', w :: hws' =>
          simp only [List.map_cons, nodupIntB, Bool.and_eq_true,
            Bool.not_eq_eq_eq_not, Bool.not_true] at hnd
          have hstep : mkParamGamma (e :: eps) (h :: hps') (w :: hws')
              = (mkParamGamma eps hps' hws').insert e.uniq (.var w h, e.sig.ty) := by
            simp only [mkParamGamma, List.zip_cons_cons, List.map_cons, List.foldr_cons]
          rw [hstep, get?_insert]
          match i, h1, h2, h3 with
          | 0, _, _, _ =>
              simp only [List.getElem_cons_zero]
              simp
          | i + 1, h1, h2, h3 =>
              simp only [List.getElem_cons_succ]
              have hne : (eps[i]'(by simpa using h1)).uniq ≠ e.uniq := by
                intro heq
                have hmem : e.uniq ∈ eps.map (·.uniq) := by
                  rw [← heq]
                  exact List.mem_map.mpr ⟨eps[i]'(by simpa using h1), List.getElem_mem _, rfl⟩
                have hcon : (eps.map (·.uniq)).contains e.uniq = true :=
                  List.elem_eq_true_of_mem hmem
                rw [hnd.1] at hcon
                cases hcon
              rw [if_neg hne]
              exact ih hnd.2 (by simpa using h1) (by simpa using h2) (by simpa using h3)

/-- Soundness of the per-definition verdict, composing the full
compiler's soundness with the bridge's `symExp_sound` and either
`cfold_eval` (unconditional) or `cfoldW3_eval` (via the `VarsWF`
invariants both compilers guarantee, and the parameter widths the
`paramsOkW` guard pins down). -/
theorem checkDefnPair_sound {Δ : DEnv} {edm : HashMap Int Defn}
    {hdm : HashMap String Rwv.Hyle.Defn} {X : Rwv.Hyle.Sem.XEnv} {fuelE fuelH : Nat}
    {ed : Defn} {hd : Rwv.Hyle.Defn}
    (hck : checkDefnPair Δ edm hdm X fuelE fuelH ed hd = true)
    {σ : String → BV} {F : Rwv.Hyle.Sem.FEnv}
    {Xf : Rwv.Hyle.Sem.XEnv} {Ff : Rwv.Hyle.Sem.FEnv}
    (hFor : ForeignC Δ Xf Ff)
    (hImpl : Rwv.Hyle.Bridge.FImplements hdm X F E)
    {env : Eval.Env}
    (hEnv : EnvC (E := E) Δ σ (mkParamGamma ed.params hd.params hd.sig.params) env)
    {ρ' : HashMap String BV}
    (hRho : Rwv.Hyle.Bridge.EnvCorr σ (mkParamRho hd.params hd.sig.params) ρ' E)
    {efuel : Nat} {v : Val}
    (hev : Eval.evalCore ⟨Δ, edm⟩ efuel env [] ed.body E = .ok v) :
    ∃ k bv, Val.rep Δ k v = .ok bv ∧ Rwv.Hyle.evalExp F X ρ' hd.body E = .ok bv := by
  rw [checkDefnPair, Bool.and_eq_true] at hck
  obtain ⟨hΔ, hck⟩ := hck
  -- both verdict routes produce the tree compilations plus a leg
  have hlegs : ∃ ne tyE nh,
      cexpFull Δ edm fuelE (mkParamGamma ed.params hd.params hd.sig.params) ed.body
        = .ok (ne, tyE)
      ∧ Rwv.Hyle.Bridge.symExp hdm X fuelH (mkParamRho hd.params hd.sig.params) hd.body
        = .ok nh
      ∧ (ne.cfold = nh.cfold
         ∨ (paramsOkW Δ fuelE ed.params hd.params hd.sig.params = true
            ∧ Rwv.Hyle.Bridge.cfoldW3 ne = Rwv.Hyle.Bridge.cfoldW3 nh)) := by
    split at hck
    · rename_i hdag
      rw [Bool.and_eq_true] at hdag
      obtain ⟨ne, tyE, nh, hcE, hcH, hW3⟩ := checkDefnPairDag_sound hdag.2
      exact ⟨ne, tyE, nh, hcE, hcH, Or.inr ⟨hdag.1, hW3⟩⟩
    · rcases hcE : cexpFull Δ edm fuelE (mkParamGamma ed.params hd.params hd.sig.params)
          ed.body with _ | ⟨ne, tyE⟩
      · rw [hcE] at hck; exact absurd hck (by simp)
      rcases hcH : Rwv.Hyle.Bridge.symExp hdm X fuelH (mkParamRho hd.params hd.sig.params)
          hd.body with _ | nh
      · rw [hcE, hcH] at hck; exact absurd hck (by simp)
      rw [hcE, hcH] at hck
      exact ⟨ne, tyE, nh, rfl, rfl, by simpa using hck⟩
  obtain ⟨ne, tyE, nh, hcE, hcH, hck2⟩ := hlegs
  obtain ⟨_hvty, k, hrep⟩ :=
    cexpFull_sound hΔ hFor fuelE (mkParamGamma ed.params hd.params hd.sig.params)
      ed.body ne tyE efuel env [] v hcE hev hEnv
  have hH := Rwv.Hyle.Bridge.symExp_sound hImpl fuelH hd.body
    (mkParamRho hd.params hd.sig.params) ρ' nh hRho hcH
  refine ⟨k, ne.eval σ E, hrep, ?_⟩
  rw [hH]
  rcases hck2 with hfold' | ⟨hpw, hW3'⟩
  · congr 1
    calc nh.eval σ E = nh.cfold.eval σ E := (Rwv.Hyle.Bridge.cfold_eval σ E nh).symm
      _ = ne.cfold.eval σ E := by rw [hfold']
      _ = ne.eval σ E := Rwv.Hyle.Bridge.cfold_eval σ E ne
  · simp only [paramsOkW, Bool.and_eq_true, beq_iff_eq] at hpw
    obtain ⟨⟨⟨hl1, hl2⟩, hnd⟩, hall⟩ := hpw
    -- the σ-width discipline of every declared parameter pair
    have hwidths : ∀ x w, (x, w) ∈ hd.params.zip hd.sig.params → (σ x).width = w := by
      intro x w hmem
      obtain ⟨i, hi, hxw⟩ := List.getElem_of_mem hmem
      rw [List.length_zip] at hi
      have hip : i < hd.params.length := Nat.lt_of_lt_of_le hi (Nat.min_le_left _ _)
      have hiw : i < hd.sig.params.length := Nat.lt_of_lt_of_le hi (Nat.min_le_right _ _)
      have hi1 : i < ed.params.length := by omega
      rw [List.getElem_zip] at hxw
      have hx : hd.params[i]'hip = x := congrArg Prod.fst hxw
      have hw : hd.sig.params[i]'hiw = w := congrArg Prod.snd hxw
      have hG := mkParamGamma_get (by simpa using hnd) hi1 hip hiw
      obtain ⟨val, hlook, hvty2, k2, hrep2⟩ := hEnv.fwd _ _ hG
      -- the declared width is the parameter type's size
      have hz := List.all_eq_true.mp hall ((ed.params.zip hd.sig.params)[i]'(by
          rw [List.length_zip]
          omega)) (List.getElem_mem _)
      rw [List.getElem_zip] at hz
      dsimp only at hz
      have hsz : Δ.sizeOf fuelE [] ((ed.params[i]'hi1).sig.ty)
          = .ok (hd.sig.params[i]'hiw) := by
        revert hz
        cases hsz0 : Δ.sizeOf fuelE [] ((ed.params[i]'hi1).sig.ty) with
        | error e => intro hz; exact absurd hz (by simp)
        | ok w0 =>
            intro hz
            have hzz : w0 = hd.sig.params[i]'hiw := by simpa using hz
            rw [hzz]
      have hwv := vty_rep_width hvty2 hrep2 hsz
      rw [show (NF.var (hd.sig.params[i]'hiw) (hd.params[i]'hip)).eval σ E
          = σ (hd.params[i]'hip) from rfl] at hwv
      rw [← hx, ← hw]
      exact hwv
    -- both compiled forms respect the discipline at σ
    have hP : ∀ x w, ((x, w) ∈ hd.params.zip hd.sig.params) → Rwv.Hyle.Bridge.WP σ x w :=
      hwidths
    have hWFe : ne.VarsWF (Rwv.Hyle.Bridge.WP σ) := by
      refine Rwv.Hyle.Bridge.NF.VarsWF.mono hP ?_
      refine cexpFull_varsWF ?_ hcE
      intro u nt hnt
      obtain ⟨x, w, hx, hmem⟩ := mkParamGamma_mem hnt
      rw [hx]
      exact hmem
    have hWFh : nh.VarsWF (Rwv.Hyle.Bridge.WP σ) := by
      refine Rwv.Hyle.Bridge.NF.VarsWF.mono hP ?_
      refine Rwv.Hyle.Bridge.symExp_varsWF fuelH hd.body _ nh ?_ hcH
      intro x n hn
      obtain ⟨w, hnv, hmem⟩ := mkParamRho_mem hn
      rw [hnv]
      exact hmem
    congr 1
    calc nh.eval σ E = (Rwv.Hyle.Bridge.cfoldW3 nh).eval σ E :=
          (Rwv.Hyle.Bridge.cfoldW3_eval hWFh).symm
      _ = (Rwv.Hyle.Bridge.cfoldW3 ne).eval σ E := by rw [hW3']
      _ = ne.eval σ E := Rwv.Hyle.Bridge.cfoldW3_eval hWFe

/-! ## Axiom audit -/

#print axioms cexp
#print axioms vty_rep_width
#print axioms rep_bitsToVec
#print axioms cprim_sound
#print axioms cexp_sound
#print axioms cexpJ
#print axioms cprimF_sound
#print axioms cchainJ_sound
#print axioms cexpJ_sound
#print axioms cexpFull_sound
#print axioms cexpJ_varsWF
#print axioms cexpFull_varsWF
#print axioms checkDefnPair_sound
#print axioms cprimFD_sim
#print axioms checkDefnPairDag_sound
#print axioms cexpJD_sim
#print axioms cexpFullD_sim

end Rwv.Eidos.Cexp

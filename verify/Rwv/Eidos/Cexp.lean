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

Everything else — joins/jumps, lambdas and higher-order arguments,
the remaining Vec/Finite/conversion builtins, `error`, externs —
is rejected (staged for Phase 4b).

THE soundness theorem (`cexp_sound`): a successful compilation is
rep-correspondent — if the committed evaluator produces a value, the
compiled normal form's denotation is that value's bit representation:

    cexp Δ dmap fuel Γ e = .ok (nf, ty) →
    evalCore ⟨Δ, dmap⟩ efuel env jenv e = .ok v →
    EnvC Δ σ Γ env →
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ)

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
import Std.Data.HashMap

namespace Rwv.Eidos.Cexp

open Std (HashMap)
open Rwv.Hyle (BV Op)
open Rwv.Hyle.Bridge (NF annWidth WP)

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
makes the compiler's and the predicate's substitutions coincide). -/

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
      Eval.evalList C ef env jenv es = .ok vs →
      vs.length = es.length ∧
      ∀ i (hi : i < es.length), ∃ (hv : i < vs.length), ∃ k,
        Eval.evalCore C k env jenv es[i] = .ok vs[i] := by
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
      ∃ k, Val.rep Δ k v = .ok (nt.1.eval σ)
  miss : ∀ x, Γ.get? x = none → env.lookup x = none

private theorem envC_empty {Δ : DEnv} {σ : String → BV} :
    EnvC Δ σ (∅ : HashMap Int (NF × Ty)) ([] : Eval.Env) := by
  constructor
  · intro x nt h
    rw [HashMap.get?_eq_getElem?] at h
    simp at h
  · intro x _
    rfl

private theorem envC_cons {Δ : DEnv} {σ : String → BV} {Γ : HashMap Int (NF × Ty)}
    {env : Eval.Env} (h : EnvC Δ σ Γ env) {u : Int} {n : NF} {t : Ty} {v : Val}
    (hv : VTy Δ v t) (hrep : ∃ k, Val.rep Δ k v = .ok (n.eval σ)) :
    EnvC Δ σ (Γ.insert u (n, t)) ((u, v) :: env) := by
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
      EnvC Δ σ Γ₀ env₀ →
      pas.length = params.length → vs.length = params.length →
      (∀ i (h1 : i < params.length) (h2 : i < pas.length) (h3 : i < vs.length),
        VTy Δ vs[i] (pas[i].2) ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ)) →
      EnvC Δ σ (bindFieldsΓ params pas Γ₀) (((params.map (·.uniq)).zip vs) ++ env₀) := by
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
    (hrepa : ∃ k, Val.rep Δ k va = .ok (a.eval σ))
    (hrepb : ∃ k, Val.rep Δ k vb = .ok (b.eval σ))
    {efuel : Nat} {v : Val} (hev : Eval.bvBinArith Δ efuel op va vb = .ok v) :
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ) := by
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
    have hxa : x = a.eval σ := rep_det hx hka
    have hyb : y = b.eval σ := rep_det hy hkb
    subst hxa; subst hyb
    obtain ⟨n', te', hfa, hba, hna⟩ := vecBoolLen_inv hwa
    obtain ⟨n'', te'', hfr, hbr, hnr⟩ := vecBoolLen_inv hm
    obtain ⟨bits, hr'⟩ := hop (a.eval σ) (b.eval σ)
    rw [hr'] at hr
    injection hr with hr
    subst hr
    have hxw : (a.eval σ).width = wa :=
      vty_vecBool_rep_width hΔ hfa hna hba hva hka
    constructor
    · exact vty_bitsToVec hΔ hfr hnr hbr (by simp only [BV.width]; omega)
    · refine ⟨3, ?_⟩
      have hnf : (NF.prim2 op a b).eval σ = ⟨(a.eval σ).width, bits⟩ := by
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
    (hrepa : ∃ k, Val.rep Δ k va = .ok (a.eval σ))
    (hrepb : ∃ k, Val.rep Δ k vb = .ok (b.eval σ))
    {efuel : Nat} {v : Val} (hev : Eval.bvBinCmp Δ efuel op va vb = .ok v) :
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ) := by
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
    have hxa : x = a.eval σ := rep_det hx hka
    have hyb : y = b.eval σ := rep_det hy hkb
    subst hxa; subst hyb
    obtain ⟨c, hc'⟩ := hop (a.eval σ) (b.eval σ)
    rw [hc'] at hr
    injection hr with hr
    subst hr
    rw [decide_b1_ne c]
    constructor
    · exact vty_bool_at hΔ hres c
    · refine ⟨2, ?_⟩
      have hnf : (NF.prim2 op a b).eval σ = Rwv.Hyle.Sem.b1 c := by
        simp only [NF.eval, hc']
      rw [hnf]
      exact rep_boolVal hΔ c 0
  · exact error_ne_ok hc

private theorem redRow_sound {Δ : DEnv} (hΔ : denvOk Δ = true) {σ : String → BV}
    {op : Op} {negated : Bool} {res : Ty} {a : NF} {nf : NF} {ty : Ty}
    (hop : ∀ x : BV, ∃ c, Rwv.Hyle.Sem.evalOp op [x] = .ok (Rwv.Hyle.Sem.b1 c))
    (hc : redRow op negated res a = .ok (nf, ty))
    {va : Val} (hrepa : ∃ k, Val.rep Δ k va = .ok (a.eval σ))
    {efuel : Nat} {v : Val} (hev : Eval.bvRed Δ efuel op negated va = .ok v) :
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ) := by
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
    have hxa : x = a.eval σ := rep_det hx hka
    subst hxa
    obtain ⟨c, hc'⟩ := hop (a.eval σ)
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
          have hnf : (NF.prim1 op a).eval σ = Rwv.Hyle.Sem.b1 c := by
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
          have hnf : (NF.prim1 .not (NF.prim1 op a)).eval σ = Rwv.Hyle.Sem.b1 (!c) := by
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
    (hev : Eval.evalBuiltin ⟨Δ, dmap⟩ efuel pty b vs = .ok v)
    (hlen : vs.length = pas.length)
    (hargs : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
       VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ)) :
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ) := by
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
      have haev : a.eval σ = ⟨128, x⟩ := by
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
      have hxa : x = a.eval σ := rep_det hx hka
      have hyb : y = b'.eval σ := rep_det hy hkb
      subst hxa; subst hyb
      have hrxor : Rwv.Hyle.Sem.evalOp .xor [a.eval σ, b'.eval σ]
          = .ok ⟨(a.eval σ).width,
                 (a.eval σ).bits ^^^ ((b'.eval σ).bits.setWidth (a.eval σ).width)⟩ := rfl
      rw [hrxor] at hr
      injection hr with hr
      subst hr
      obtain ⟨n', te', hfda, hba, hna⟩ := vecBoolLen_inv hwa
      obtain ⟨n'', te'', hfr, hbr, hnr⟩ := vecBoolLen_inv hm
      have hxw : (a.eval σ).width = wa :=
        vty_vecBool_rep_width hΔ hfda hna hba h0.1 hka
      constructor
      · exact vty_bitsToVec hΔ hfr hnr hbr (by simp only [BV.width]; omega)
      · refine ⟨3, ?_⟩
        have hnf : (NF.prim1 .not (NF.prim2 .xor a b')).eval σ
            = (⟨(a.eval σ).width,
                ~~~((a.eval σ).bits ^^^ ((b'.eval σ).bits.setWidth (a.eval σ).width))⟩ : BV) := by
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
      have hxa : x = a.eval σ := rep_det hx hka
      subst hxa
      obtain ⟨n', te', hfda, hba, hna⟩ := vecBoolLen_inv hwa
      obtain ⟨n'', te'', hfr, hbr, hnr⟩ := vecBoolLen_inv hm
      have hxw : (a.eval σ).width = wa :=
        vty_vecBool_rep_width hΔ hfda hna hba h0.1 hka
      constructor
      · exact vty_bitsToVec hΔ hfr hnr hbr (by simp only [BV.width]; omega)
      · refine ⟨3, ?_⟩
        have hnf : (NF.prim1 .not a).eval σ
            = (⟨(a.eval σ).width, ~~~(a.eval σ).bits⟩ : BV) := by
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
      have hxa : x = a.eval σ := rep_det hx hka
      have hyb : y = b'.eval σ := rep_det hy hkb
      subst hxa; subst hyb
      have hval : Eval.boolVal (decide ((a.eval σ).nat ≠ 0 ∧ (b'.eval σ).nat ≠ 0))
          = Eval.boolVal (((a.eval σ).bits != 0) && ((b'.eval σ).bits != 0)) := by
        rw [decide_and', bits_ne_nat, bits_ne_nat]
      rw [hval]
      constructor
      · exact vty_bool_at hΔ hres _
      · refine ⟨2, ?_⟩
        have hnf : (NF.prim2 .and (NF.prim1 .redor a) (NF.prim1 .redor b')).eval σ
            = Rwv.Hyle.Sem.b1 (((a.eval σ).bits != 0) && ((b'.eval σ).bits != 0)) := by
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
      have hxa : x = a.eval σ := rep_det hx hka
      have hyb : y = b'.eval σ := rep_det hy hkb
      subst hxa; subst hyb
      have hval : Eval.boolVal (decide ((a.eval σ).nat ≠ 0 ∨ (b'.eval σ).nat ≠ 0))
          = Eval.boolVal (((a.eval σ).bits != 0) || ((b'.eval σ).bits != 0)) := by
        rw [decide_or', bits_ne_nat, bits_ne_nat]
      rw [hval]
      constructor
      · exact vty_bool_at hΔ hres _
      · refine ⟨2, ?_⟩
        have hnf : (NF.prim2 .or (NF.prim1 .redor a) (NF.prim1 .redor b')).eval σ
            = Rwv.Hyle.Sem.b1 (((a.eval σ).bits != 0) || ((b'.eval σ).bits != 0)) := by
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
      have hxa : x = a.eval σ := rep_det hx hka
      subst hxa
      have hval : Eval.boolVal (decide ((a.eval σ).nat = 0))
          = Eval.boolVal (!((a.eval σ).bits != 0)) := by
        congr 1
        rw [bits_ne_nat]
        by_cases h : (a.eval σ).nat = 0 <;> simp [h]
      rw [hval]
      constructor
      · exact vty_bool_at hΔ hres _
      · refine ⟨2, ?_⟩
        have hnf : (NF.prim1 .not (NF.prim1 .redor a)).eval σ
            = Rwv.Hyle.Sem.b1 (!((a.eval σ).bits != 0)) := by
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
      have hxa : x = a.eval σ := rep_det hx hka
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
      have hxw : (a.eval σ).width = wa :=
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
          have hbv : (⟨m', (a.eval σ).bits.setWidth m'⟩ : BV) = a.eval σ := by
            refine bv_ext (show m' = (a.eval σ).width by omega) ?_
            intro i
            show ((a.eval σ).bits.setWidth m').getLsbD i = _
            rw [BitVec.getLsbD_setWidth]
            by_cases hi : i < m'
            · simp [hi]
            · rw [decide_eq_false hi, Bool.false_and,
                  getLsbD_ge (a.eval σ).bits (by omega)]
          rw [hbv]
          exact rep_bitsToVec hΔ (a.eval σ) 0
      · split at hc
        · rename_i hne hlt
          rw [except_pure_def] at hc
          injection hc with hc
          injection hc with hnf hty
          subst hnf; subst hty
          constructor
          · exact vty_bitsToVec hΔ hfr hnr hbr rfl
          · refine ⟨3, ?_⟩
            have hnf : (NF.prim1 (.zext m') a).eval σ
                = (⟨m', (a.eval σ).bits.setWidth m'⟩ : BV) := by
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
            have hnf : (NF.prim1 (.trunc m') a).eval σ
                = (⟨m', (a.eval σ).bits.setWidth m'⟩ : BV) := by
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
              have hslice : sliceBV (a.eval σ) (wa - 1) 1 = bh := by
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
                have hnf : (NF.slice (wa - 1) 1 a).eval σ = sliceBV (a.eval σ) (wa - 1) 1 := rfl
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
    ∀ (xs : List NF), (catList xs).eval σ = catAll (xs.map (NF.eval σ)) := by
  intro xs
  match xs with
  | [] => rfl
  | [x] =>
      show x.eval σ = catAll [x.eval σ]
      rw [catAll_cons, catAll_nil, bvCat_zero_right rfl]
  | x :: y :: rest =>
      show bvCat (x.eval σ) ((catList (y :: rest)).eval σ) = _
      rw [catList_eval σ (y :: rest)]
      rw [show (x :: y :: rest).map (NF.eval σ) = x.eval σ :: (y :: rest).map (NF.eval σ)
            from rfl, catAll_cons]

private theorem catNF_eval (σ : String → BV)
    (xs : List (NF × Nat)) (hw : ∀ p ∈ xs, (p.1.eval σ).width = p.2) :
    (catNF xs).eval σ = catAll (xs.map (fun p => p.1.eval σ)) := by
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
        show (x.1.eval σ).width = 0
        rw [hw x List.mem_cons_self]
        simpa using hx

private theorem sliceNF_eval (σ : String → BV) (off w : Nat) (e : NF) :
    (sliceNF off w e).eval σ = sliceBV (e.eval σ) off w := by
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


/-! ## The soundness theorem -/

/-- The main induction hypothesis, packaged: soundness of `cexp` at a
fixed fuel. -/
private abbrev SoundAt (Δ : DEnv) (dmap : HashMap Int Defn) (σ : String → BV) (fuel : Nat) : Prop :=
  ∀ (Γ : HashMap Int (NF × Ty)) (e : Exp) (nf : NF) (ty : Ty) (efuel : Nat)
    (env : Eval.Env) (jenv : Eval.JEnv) (v : Val),
    cexp Δ dmap fuel Γ e = .ok (nf, ty) →
    Eval.evalCore ⟨Δ, dmap⟩ efuel env jenv e = .ok v →
    EnvC Δ σ Γ env →
    VTy Δ v ty ∧ ∃ k, Val.rep Δ k v = .ok (nf.eval σ)

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
    (hc : c.eval σ = Rwv.Hyle.Sem.b1 C) :
    (NF.ite c t e).eval σ = if C then t.eval σ else e.eval σ := by
  show (if (c.eval σ).nat ≠ 0 then t.eval σ else e.eval σ) = _
  rw [hc]
  cases C with
  | true => rw [if_pos (by decide), if_pos rfl]
  | false => rw [if_neg (by decide), if_neg (by simp)]

set_option maxHeartbeats 8000000 in
/-- The chain lemma: with the scrutinee facts in hand, the compiled
if-chain agrees with `tryAlts`' selection. -/
private theorem cchain_sound {Δ : DEnv} {dmap : HashMap Int Defn} {σ : String → BV}
    (hΔ : denvOk Δ = true) {fuel : Nat} (IH : SoundAt Δ dmap σ fuel)
    {Γ' : HashMap Int (NF × Ty)} {env : Eval.Env} {jenv : Eval.JEnv}
    {binder : Id} {dn : NF} {dty : Ty} {szT : Nat} {resTy : Ty} {sv : Val}
    (hΓ' : EnvC Δ σ Γ' ((binder.uniq, sv) :: env))
    (hsz : Δ.sizeOf (fuel + 1) [] dty = .ok szT)
    (hvty : VTy Δ sv dty)
    {ks : Nat} (hks : Val.rep Δ ks sv = .ok (dn.eval σ)) :
    ∀ (rest : List Alt) (macc : Option NF) (dflt : Option Alt) (out : NF × Ty)
      (ef2 : Nat) (vout : Val),
      cchain Δ dmap fuel Γ' dty szT dn resTy rest macc = .ok out →
      Eval.tryAlts ⟨Δ, dmap⟩ ef2 env jenv binder sv rest dflt = .ok vout →
      ((macc = none ∧ dflt = none) ∨
       (∃ els c bs dbody, macc = some els ∧ dflt = some (Alt.mk c bs dbody) ∧
          cexp Δ dmap fuel Γ' dbody = .ok (els, resTy))) →
      out.2 = resTy ∧ VTy Δ vout resTy ∧ ∃ k, Val.rep Δ k vout = .ok (out.1.eval σ) := by
  -- The per-alternative step.
  have hstep : ∀ (con : AltCon) (xs : List Id) (body : Exp) (macc : Option NF) (bnf : NF)
      (restE : List Alt) (dflt : Option Alt) (ef3 : Nat) (vout : Val),
      cAlt Δ dmap fuel Γ' dty szT dn resTy (.mk con xs body) macc = .ok bnf →
      Eval.tryAlts ⟨Δ, dmap⟩ (ef3 + 1) env jenv binder sv (.mk con xs body :: restE) dflt
        = .ok vout →
      (macc = none → restE = [] ∧ dflt = none) →
      (∀ acc, macc = some acc →
         Eval.tryAlts ⟨Δ, dmap⟩ ef3 env jenv binder sv restE dflt = .ok vout →
         VTy Δ vout resTy ∧ ∃ k, Val.rep Δ k vout = .ok (acc.eval σ)) →
      VTy Δ vout resTy ∧ ∃ k, Val.rep Δ k vout = .ok (bnf.eval σ) := by
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
        have hxd : x = dn.eval σ := rep_det hx hks
        subst hxd
        subst hbm
        have hw : (dn.eval σ).width = szT := vty_rep_width hvty hks hsz
        have hcondv : (NF.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT i⟩)).eval σ
            = Rwv.Hyle.Sem.b1 ((dn.eval σ).bits == BitVec.ofInt (dn.eval σ).width i) := by
          rw [show (NF.prim2 .eq dn (.lit ⟨szT, BitVec.ofInt szT i⟩)).eval σ
                = Rwv.Hyle.Sem.b1 ((dn.eval σ).bits ==
                    (BitVec.ofInt szT i).setWidth (dn.eval σ).width) from rfl]
          rw [show (BitVec.ofInt szT i).setWidth (dn.eval σ).width
                = BitVec.ofInt (dn.eval σ).width i by
              rw [hw]
              exact BitVec.setWidth_eq _]
        cases hbm2 : ((dn.eval σ).bits == BitVec.ofInt (dn.eval σ).width i) with
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
        have hwsz : (dn.eval σ).width = szT := vty_rep_width (VTy.con hsigv hmatchv hlenv
          (by rw [ctorOf, hflT]; exact hctorv) hfieldsv) hks hsz
        -- the tag slice of the representation
        have hFw : (bvCat (⟨szT - tagWv - (Val.bvConcat bsR).width, 0⟩ : BV)
            (Val.bvConcat bsR)).width = szT - tagWv := by
          rw [bvCat_width]
          show szT - tagWv - (Val.bvConcat bsR).width + (Val.bvConcat bsR).width = szT - tagWv
          omega
        have hslice : sliceBV (dn.eval σ) (szT - tagWv) tagWv
            = (⟨tagWv, BitVec.ofNat tagWv tagv⟩ : BV) := by
          rw [hbv, sliceBV_cat_high (Nat.le_of_eq hFw), hFw, Nat.sub_self]
          exact sliceBV_all _
        -- the compiled test's value
        have hcondv : ∀ (w0 : Nat), tagWv = w0 + 1 →
            (NF.prim2 .eq (sliceNF (szT - tagWv) tagWv dn)
              (.lit ⟨tagWv, BitVec.ofNat tagWv tag⟩)).eval σ
            = Rwv.Hyle.Sem.b1 (BitVec.ofNat tagWv tagv == BitVec.ofNat tagWv tag) := by
          intro w0 hw0
          have h1 : (NF.prim2 .eq (sliceNF (szT - tagWv) tagWv dn)
                (.lit ⟨tagWv, BitVec.ofNat tagWv tag⟩)).eval σ
              = Rwv.Hyle.Sem.b1
                  (((sliceNF (szT - tagWv) tagWv dn).eval σ).bits ==
                    BitVec.setWidth ((sliceNF (szT - tagWv) tagWv dn).eval σ).width
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
            have henv'' : EnvC Δ σ
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
    (h : Eval.applyMany C k f [] = .ok v) : v = f := by
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
    (hΔ : denvOk Δ = true) : ∀ (fuel : Nat), SoundAt Δ dmap σ fuel := by
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
                      VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ) := by
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
              VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ) := by
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
                  (pas[i].1.eval σ).width = ws[i]'(by omega) := by
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
              have hlistw : (pas.map (fun p => p.1.eval σ)).map (·.width) = ws := by
                refine List.ext_getElem (by simpa using by omega) ?_
                intro i h1 h2
                simp only [List.getElem_map]
                exact hwidths i (by simpa using h1)
              have hsum : (Val.bvConcat (pas.map fun p => p.1.eval σ)).width = ws.sum := by
                rw [bvConcat_eq, catAll_width, hlistw]
              obtain ⟨K, hK⟩ := mapM_rep_exists (Δ := Δ) (vs := vs)
                (bs := pas.map (fun p => p.1.eval σ)) (by simpa using hvpas)
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
                have hle2 : w + (Val.bvConcat (pas.map fun p => p.1.eval σ)).width ≤ whole := by
                  rw [hsum]
                  exact hle
                have hcon := rep_con_intro (K := max (fuel + 1) K)
                  (Δ.sizeOf_mono (by omega) hwhole) htag
                  (mapM_rep_mono (Nat.le_max_right _ _) hK) hle2
                refine hcon.trans ?_
                congr 1
                have hpwidths : ∀ p ∈ ((NF.lit ⟨w, BitVec.ofNat w tag⟩, w)
                    :: (NF.lit ⟨whole - w - ws.sum, 0⟩, whole - w - ws.sum)
                    :: (pas.map (·.1)).zip ws), (p.1.eval σ).width = p.2 := by
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
                    :: (pas.map (·.1)).zip ws).map (fun p => p.1.eval σ)
                    = (⟨w, BitVec.ofNat w tag⟩ : BV) :: (⟨whole - w - ws.sum, 0⟩ : BV)
                      :: (pas.map fun p => p.1.eval σ) := by
                  rw [List.map_cons, List.map_cons,
                      map_zip_fst (NF.eval σ) (pas.map (·.1)) ws
                        (by simp only [List.length_map]; omega),
                      List.map_map]
                  rfl
                rw [hmaps, catAll_cons, catAll_cons, ← bvConcat_eq, hsum]
      | prim pty b =>
          dsimp only at hc hev
          obtain ⟨pas, hpas, hc⟩ := except_bind_eq_ok hc
          obtain ⟨vs, hvs, hev⟩ := except_bind_eq_ok hev
          obtain ⟨hplen, hpt⟩ := mapM_ok_idx hpas
          obtain ⟨hvlen, hpt2⟩ := evalList_ok_idx hvs
          have hptw : ∀ i (h1 : i < pas.length) (h2 : i < vs.length),
              VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ) := by
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
                refine ⟨VTy.finite hfl2 hk, 1, ?_⟩
                rw [Val.rep]
                rw [show (NF.lit (⟨nbits k, BitVec.ofInt (nbits k) n⟩ : BV)).eval σ
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
                  VTy Δ vs[i] pas[i].2 ∧ ∃ k, Val.rep Δ k vs[i] = .ok (pas[i].1.eval σ) := by
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
                  (bs := pas.map (fun p => p.1.eval σ)) (by simpa using by omega)
                  (fun i h1 h2 => by
                    obtain ⟨_, hex⟩ := hptw i (by omega) h1
                    obtain ⟨k1, hrep1⟩ := hex
                    exact ⟨k1, by simpa using hrep1⟩)
                refine ⟨K + 1, ?_⟩
                rw [Val.rep, mapM_attach_erase, hK, except_bind_ok, except_pure_def]
                congr 1
                have hpwidths : ∀ p ∈ (pas.map (·.1)).map (·, se), (p.1.eval σ).width = p.2 := by
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
              have hΓ' : EnvC Δ σ (Γ.insert binder.uniq (dn, dty)) ((binder.uniq, sv) :: env) :=
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

/-- The per-definition validator. `true` requires: the datatype
environment passes `denvOk`, both compilations succeed, and the
`cfold` normal forms coincide syntactically. -/
def checkDefnPair (Δ : DEnv) (edm : HashMap Int Defn)
    (hdm : HashMap String Rwv.Hyle.Defn) (fuelE fuelH : Nat)
    (ed : Defn) (hd : Rwv.Hyle.Defn) : Bool :=
  denvOk Δ &&
    match cexp Δ edm fuelE (mkParamGamma ed.params hd.params hd.sig.params) ed.body,
          Rwv.Hyle.Bridge.symExp hdm fuelH (mkParamRho hd.params hd.sig.params) hd.body with
    | .ok (ne, _), .ok nh => ne.cfold == nh.cfold
    | _, _ => false

/-- Soundness of the per-definition verdict, composing `cexp_sound`
with the bridge's `symExp_sound` and `cfold_eval`: against ANY
argument valuation under which the two compile-time environments
correspond to the runtime ones (`EnvC` on the Eidos side,
`Bridge.EnvCorr` on the Hyle side — the caller instantiates σ with
the representations of the actual arguments), whenever the Eidos body
evaluates to a value, the Hyle body evaluates to exactly that value's
bit representation. -/
theorem checkDefnPair_sound {Δ : DEnv} {edm : HashMap Int Defn}
    {hdm : HashMap String Rwv.Hyle.Defn} {fuelE fuelH : Nat} {ed : Defn}
    {hd : Rwv.Hyle.Defn}
    (hck : checkDefnPair Δ edm hdm fuelE fuelH ed hd = true)
    {σ : String → BV} {X : Rwv.Hyle.Sem.XEnv} {F : Rwv.Hyle.Sem.FEnv}
    (hImpl : Rwv.Hyle.Bridge.FImplements hdm X F)
    {env : Eval.Env}
    (hEnv : EnvC Δ σ (mkParamGamma ed.params hd.params hd.sig.params) env)
    {ρ' : HashMap String BV}
    (hRho : Rwv.Hyle.Bridge.EnvCorr σ (mkParamRho hd.params hd.sig.params) ρ')
    {efuel : Nat} {v : Val}
    (hev : Eval.evalCore ⟨Δ, edm⟩ efuel env [] ed.body = .ok v) :
    ∃ k bv, Val.rep Δ k v = .ok bv ∧ Rwv.Hyle.evalExp F X ρ' hd.body = .ok bv := by
  rw [checkDefnPair, Bool.and_eq_true] at hck
  obtain ⟨hΔ, hck⟩ := hck
  rcases hcE : cexp Δ edm fuelE (mkParamGamma ed.params hd.params hd.sig.params) ed.body with
    _ | ⟨ne, tyE⟩
  · rw [hcE] at hck; exact absurd hck (by simp)
  rcases hcH : Rwv.Hyle.Bridge.symExp hdm fuelH (mkParamRho hd.params hd.sig.params) hd.body with
    _ | nh
  · rw [hcE, hcH] at hck; exact absurd hck (by simp)
  rw [hcE, hcH] at hck
  have hfold : ne.cfold = nh.cfold := beq_iff_eq.mp hck
  obtain ⟨_hvty, k, hrep⟩ :=
    cexp_sound hΔ fuelE (mkParamGamma ed.params hd.params hd.sig.params)
      ed.body ne tyE efuel env [] v hcE hev hEnv
  have hH := Rwv.Hyle.Bridge.symExp_sound hImpl fuelH hd.body
    (mkParamRho hd.params hd.sig.params) ρ' nh hRho hcH
  refine ⟨k, ne.eval σ, hrep, ?_⟩
  rw [hH]
  congr 1
  calc nh.eval σ = nh.cfold.eval σ := (Rwv.Hyle.Bridge.cfold_eval σ nh).symm
    _ = ne.cfold.eval σ := by rw [hfold]
    _ = ne.eval σ := Rwv.Hyle.Bridge.cfold_eval σ ne

/-! ## Axiom audit -/

#print axioms cexp
#print axioms vty_rep_width
#print axioms rep_bitsToVec
#print axioms cprim_sound
#print axioms cexp_sound
#print axioms checkDefnPair_sound

end Rwv.Eidos.Cexp

/-
The Synolon semantic value domain (doc/synolon.md §5.1) and the
data-to-bits representation (the `rep` of the validation architecture,
transcribing ToHyle's sizeOf/ctorTag/ctorRep and the detupleSizes port
convention).

Values are algebraic: vectors as tuples, `Finite` and `Integer` as
numbers, ADT values (including Bool, unit, and the tuple family, which
are just prim-basis datatypes) as constructor applications carrying
their instance type — the type is what `rep` needs to lay out
tag | pad | fields. Function values (higher-order builtin arguments,
doc/synolon.md §5.2) are defunctionalized closures: a lambda with its captured
environment, or a partially applied definition.
-/
import Rwv.Eidos.Types
import Rwv.Hyle.Syntax
import Std.Data.HashMap
import Std.Data.HashSet

namespace Rwv.Eidos

open Std (HashMap HashSet)

/-- Semantic values (doc/synolon.md §5.1). -/
inductive Val where
  | vec     (elems : List Val)
  | integer (v : BitVec 128)
  | finite  (bound : Nat) (val : Nat)
  | str     (s : String)
  | proxy
  | con     (ty : Ty) (c : String) (fields : List Val)
  | closL   (x : Id) (env : List (Int × Val)) (body : Exp)
  | closD   (f : Id) (pre : List Val)
deriving Repr, Inhabited

/-- ⌈log₂ n⌉, the width of a tag or `Finite` value
(ReWire.BitVector.nbits, an equivalent exact integer recursion; this
is the closed log2 form, `nbits 0 = nbits 1 = 0`). -/
def nbits (n : Nat) : Nat :=
  if n ≤ 1 then 0 else Nat.log2 (n - 1) + 1

/-- The static environment of the machine semantics: the datatype
environment — constructor lists in declaration order and constructor
signatures, from the program's (prim-basis-extended) data declarations
— extended (the foreign tier) with the model-carrying foreign
interpretations and the syntactic data a validator compiles them
through. The foreign fields all default to "absent", so a bare
`DEnv.ofDatas` behaves exactly as before the extension:

  * `cryF f n τ` — the semantic denotation of the Cryptol foreign
    function `(module file f, function n)` at the impl monotype `τ`
    (per doc/synolon.md §5.5, with η for a Cryptol splice DEFINED as
    the Hyle-side denotation of the `cry$…` definitions rwcry
    emitted — the model-carrying trust boundary). The drivers build
    it from the compiled program's own definition environment
    (`Rwv.Hyle.Sem.mkFEnv`).
  * `cryD f n τ` — the (untrusted, checked) Hyle entry-definition
    name for the same key: what the verified expression compiler
    inlines through. `hyleDefs`/`hyleFuel` are the compiled program's
    definition map and a symbolic-evaluation fuel for that inlining.
    The soundness theorems consume these only through an explicit
    premise tying `cryF` to `hyleDefs`' denotations
    (`Rwv.Eidos.Cexp.ForeignC`); a wrong map fails validation, never
    soundness.
  * `hyleX` — the compiled program's syntactic extern→model table
    (`Rwv.Hyle.Sem.xenv` of the foreign program, structurally): the
    table at which the compiler's Cryptol row inlines spliced
    definitions through the bridge.

(Model-carrying externs need no hook here: an extern occurrence's
source-side meaning is its own implementation argument — see
`Eval.externModelless` — so it is evaluated and compiled as an
ordinary expression, independent of any target program.) -/
structure DEnv where
  ctors    : HashMap String (List String)
  ctorSig  : HashMap String Sig
  cryF     : String → String → Ty → Option (List Rwv.Hyle.BV → Except String Rwv.Hyle.BV) :=
    fun _ _ _ => none
  cryD     : String → String → Ty → Option String := fun _ _ _ => none
  hyleDefs : HashMap String Rwv.Hyle.Defn := ∅
  hyleFuel : Nat := 0
  hyleX    : HashMap String String := ∅

def DEnv.ofDatas (datas : List DataDefn) : DEnv where
  ctors   := HashMap.ofList (datas.map fun d => (d.name, d.cons.map (·.name)))
  ctorSig := HashMap.ofList (datas.flatMap fun d => d.cons.map fun c => (c.name, c.sig))

namespace DEnv

/-- First-order matching of a constructor's result type against a
concrete type (ToHyle.matchTy, transcribed *leniently*: mismatched
heads yield the empty substitution, and inconsistent bindings fail). -/
def matchTy : Ty → Ty → Except String (HashMap TyVar Ty)
  | .app t₁ t₂, .app t₁' t₂' => do
      let s₁ ← matchTy t₁ t₁'
      let s₂ ← matchTy t₂ t₂'
      let mut merged := s₁
      for (v, t) in s₂ do
        match merged.get? v with
        | some t' =>
            if (Ty.natNorm t == Ty.natNorm t') then pure () else
              throw "matchTy: inconsistent assignment of a type variable"
        | none => merged := merged.insert v t
      pure merged
  | .var v, t => pure (HashMap.ofList [(v, t)])
  | _, _ => pure ∅

/-- Substitute type variables. -/
def substTv (sub : HashMap TyVar Ty) : Ty → Ty
  | .var v => match sub.get? v with
      | some t => t
      | none   => .var v
  | .app t₁ t₂   => .app (substTv sub t₁) (substTv sub t₂)
  | .arrow t₁ t₂ => .arrow (substTv sub t₁) (substTv sub t₂)
  | t => t

mutual

/-- The bit width of a representable type (ToHyle.sizeOf): `Vec n τ` is
n · |τ|, `Finite n` is nbits n, `Integer` is 128, `Proxy` and type
variables are 0, tuples are the sum of their components, a datatype is
nbits(#ctors) + the widest constructor payload; recursive datatypes
are rejected via the visited set. The fuel bounds datatype unfolding
(the visited set is the semantic bound; fuel makes it structural). -/
def sizeOf (Δ : DEnv) (fuel : Nat) (visited : List Ty) (t : Ty) : Except String Nat :=
  match fuel with
  | 0 => throw "sizeOf: fuel exhausted"
  | fuel + 1 =>
    match Ty.flatten t with
    | (.con "Vec", [n, te]) =>
        match Ty.evalNat n with
        | some k => do pure (k * (← sizeOf Δ fuel visited te))
        | none   => throw "sizeOf: can't determine the size of a Vec"
    | (.con "Finite", [n]) =>
        match Ty.evalNat n with
        | some k => pure (nbits k)
        | none   => throw "sizeOf: can't determine the size of a Finite"
    | (.con "Integer", []) => pure 128
    | (.con "Proxy", _)    => pure 0
    | (.con c, args) =>
        if Ty.isTupleCon c then do
          let ws ← args.mapM (sizeOf Δ fuel visited)
          pure ws.sum
        else if visited.any (Ty.eq · t) then
          throw s!"sizeOf: can't determine the size of a recursive datatype: {c}"
        else
          match Δ.ctors.get? c with
          | some cs => do
              let ws ← cs.mapM (ctorWidth Δ fuel (t :: visited) t)
              pure (nbits cs.length + (ws.foldl max 0))
          | none => throw s!"sizeOf: couldn't calculate the size of a type ({c})"
    | (.var _, _) => pure 0
    | _ => throw "sizeOf: couldn't calculate the size of a type"

/-- The payload width of a constructor at a concrete instance type
(ToHyle.ctorWidth): match the result type, substitute, sum the field
sizes. -/
def ctorWidth (Δ : DEnv) (fuel : Nat) (visited : List Ty) (t : Ty) (c : String) :
    Except String Nat :=
  match fuel with
  | 0 => throw "ctorWidth: fuel exhausted"
  | fuel + 1 =>
    match Δ.ctorSig.get? c with
    | some sig => do
        let (targs, tres) := Ty.flattenArrow sig.ty
        let sub ← matchTy tres t
        let ws ← targs.mapM (fun ta => sizeOf Δ fuel visited (substTv sub ta))
        pure ws.sum
    | none => pure 0

end

/-- The tag value and width of a constructor of the given concrete type
(ToHyle.ctorTag): declaration index, at width nbits(#ctors); tuples
are tagless. -/
def ctorTag (Δ : DEnv) (t : Ty) (c : String) : Except String (Nat × Nat) :=
  match Ty.flatten t with
  | (.con tc, _) =>
      if Ty.isTupleCon tc then pure (0, 0)
      else match Δ.ctors.get? tc with
        | some cs =>
            match cs.idxOf? c with
            | some idx => pure (idx, nbits cs.length)
            | none => throw s!"ctorTag: unknown ctor: {c} of type {tc}"
        | none => throw s!"ctorTag: unknown type: {tc}"
  | _ => throw "ctorTag: unexpected type"

/-- The zero value of a representable type (doc/synolon.md §5.1):
zero vectors and numbers, and the first constructor applied to zero
fields — whose representation is the all-zeros bit pattern. -/
def zeroVal (Δ : DEnv) (fuel : Nat) (t : Ty) : Except String Val :=
  match fuel with
  | 0 => throw "zeroVal: fuel exhausted"
  | fuel + 1 =>
    match Ty.flatten t with
    | (.con "Vec", [n, te]) =>
        match Ty.evalNat n with
        | some k => do pure (.vec (List.replicate k (← zeroVal Δ fuel te)))
        | none   => throw "zeroVal: open Vec length"
    | (.con "Finite", [nt]) =>
        -- `Finite 0` is uninhabited (matching Data.Finite): an empty
        -- type has no zero value, and an open bound has no width.
        (match Ty.evalNat nt with
        | some (k + 1) => pure (.finite (k + 1) 0)
        | some 0       => throw "zeroVal: Finite 0 is uninhabited"
        | none         => throw "zeroVal: open Finite bound")
    | (.con "Integer", []) => pure (.integer 0)
    | (.con "Proxy", [_])  => pure .proxy
    | (.con c, _) =>
        match Δ.ctors.get? c with
        | some (c₀ :: _) => do
            match Δ.ctorSig.get? c₀ with
            | some sig => do
                let (targs, tres) := Ty.flattenArrow sig.ty
                let sub ← matchTy tres t
                let fields ← targs.mapM (fun ta => zeroVal Δ fuel (substTv sub ta))
                pure (.con t c₀ fields)
            | none => throw s!"zeroVal: unknown ctor sig: {c₀}"
        | some [] => throw s!"zeroVal: uninhabited datatype: {c}"
        | none => throw s!"zeroVal: unknown type: {c}"
    | _ => throw "zeroVal: unrepresentable type"

end DEnv

namespace Val

open Rwv.Hyle (BV)

/-- Concatenate bit vectors, left = most significant. -/
def bvConcat (xs : List BV) : BV :=
  xs.foldl (fun acc x => ⟨_, acc.bits ++ x.bits⟩) Rwv.Hyle.BV.nil

/-- The data-to-bits representation `rep` (doc/synolon.md §5.1's bit
readings extended to ADTs per the translation's encoding, ToHyle
ctorRep): vectors concatenate MSB-first from the head; a constructor
value is tag | zero pad | fields. -/
def rep (Δ : DEnv) (fuel : Nat) (v : Val) : Except String BV :=
  match fuel with
  | 0 => throw "rep: fuel exhausted"
  | fuel + 1 =>
    match v with
    | .vec elems => do
        let bs ← elems.attach.mapM fun ⟨e, _⟩ => rep Δ fuel e
        pure (bvConcat bs)
    | .integer n => pure ⟨128, n⟩
    | .finite bound val => pure ⟨nbits bound, BitVec.ofNat _ val⟩
    | .str _ => throw "rep: a string has no bit representation"
    | .proxy => pure Rwv.Hyle.BV.nil
    | .con ty c fields => do
        let whole ← Δ.sizeOf (fuel + 1) [] ty
        let (tag, tagW) ← Δ.ctorTag ty c
        let bs ← fields.attach.mapM fun ⟨f, _⟩ => rep Δ fuel f
        let fieldsBV := bvConcat bs
        if tagW + fieldsBV.width ≤ whole then
          let padW := whole - tagW - fieldsBV.width
          pure (bvConcat [⟨tagW, BitVec.ofNat _ tag⟩, ⟨padW, 0⟩, fieldsBV])
        else
          throw s!"rep: constructor {c} wider than its type"
    | .closL .. | .closD .. => throw "rep: a function value has no bit representation"

/-- The device port convention (ToHyle.detupleSizes): one level of
type-application splitting — the bare head is its own component
(sizing the constructor tag; zero and dropped for tuples), `Vec` and
`Finite` stay whole — with a leading residual component and zero
widths dropped. -/
def detupleSizes (Δ : DEnv) (fuel : Nat) (t : Ty) : Except String (List Nat) := do
  let whole ← Δ.sizeOf fuel [] t
  let parts := match Ty.flatten t with
    | (.con "Vec", _) | (.con "Finite", _) => [t]
    | (h, args) => h :: args
  let sizes ← parts.mapM (Δ.sizeOf fuel [])
  pure (((whole - sizes.sum) :: sizes).filter (· > 0))

/-- Split a value of a port type into per-port bit vectors: MSB-first
consecutive slices of `rep` at the detupleSizes widths. -/
def portSplit (Δ : DEnv) (fuel : Nat) (t : Ty) (v : Val) : Except String (List BV) := do
  let bv ← rep Δ fuel v
  let sizes ← detupleSizes Δ fuel t
  if sizes.sum ≠ bv.width then
    throw s!"portSplit: port widths {sizes} do not cover the representation ({bv.width})"
  let (_, out) := sizes.foldl (init := (bv.width, ([] : List BV)))
    fun (hi, acc) w => (hi - w, ⟨w, bv.bits.extractLsb' (hi - w) w⟩ :: acc)
  pure out.reverse

end Val

end Rwv.Eidos

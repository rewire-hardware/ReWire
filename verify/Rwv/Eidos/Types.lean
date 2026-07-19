/-
Type utilities for the Eidos deep embedding: application/arrow
flattening, evaluation of nat-closed type arithmetic, normalization,
equality modulo normalization, substitution/instantiation, and the
trusting type synthesizer `typeOf` — transcribing the corresponding
parts of ReWire.Eidos.Types (doc/eidos.md §3.1, §5). `typeOf` is
shared by the `.eir` elaborator (Rwv.Eidos.Parse, which reconstructs
the types the concrete syntax leaves implicit) — the machine-mode
checker (Rwv.Eidos.Check) is its located, checking twin, exactly as
ReWire.Eidos.Lint.checkExp is the monadic twin of
ReWire.Eidos.Types.typeOf in the reference.
-/
import Rwv.Eidos.Syntax
import Std.Data.HashMap

namespace Rwv.Eidos

namespace Ty

/-- Flatten an application spine: `T a b` ↦ `(T, [a, b])`. -/
def flatten : Ty → Ty × List Ty
  | .app t₁ t₂ =>
      let (h, args) := flatten t₁
      (h, args ++ [t₂])
  | t => (t, [])

/-- Flatten an arrow spine: `a → b → c` ↦ `([a, b], c)`. -/
def flattenArrow : Ty → List Ty × Ty
  | .arrow t₁ t₂ =>
      let (doms, res) := flattenArrow t₂
      (t₁ :: doms, res)
  | t => ([], t)

/-- Evaluate a nat-closed type to its numeric value (doc/eidos.md
§3.1): literals, and the built-in `+`/`-`/`*` constructors applied to
nat-closed operands (as binary application trees). Subtraction is
truncated (as on `Natural`); well-formed types do not underflow. -/
def evalNat : Ty → Option Nat
  | .nat n => some n
  | .app (.app (.con op) t₁) t₂ => do
      let a ← evalNat t₁
      let b ← evalNat t₂
      match op with
      | "+" => some (a + b)
      | "-" => some (a - b)
      | "*" => some (a * b)
      | _   => none
  | _ => none

/-- Normalize by folding every nat-closed subterm to a literal. -/
def natNorm (t : Ty) : Ty :=
  match evalNat t with
  | some n => .nat n
  | none =>
      match t with
      | .app t₁ t₂   => .app (natNorm t₁) (natNorm t₂)
      | .arrow t₁ t₂ => .arrow (natNorm t₁) (natNorm t₂)
      | t => t

/-- Type equality: structural modulo `natNorm` (annotations are already
absent from the embedding). -/
def eq (t₁ t₂ : Ty) : Bool := natNorm t₁ == natNorm t₂

def isTupleCon (c : String) : Bool :=
  c.startsWith "(" && c.endsWith ")" && (c.toList.drop 1).dropLast.all (· = ',')

/-- Substitution of types for type variables (by unique). No renaming
is ever needed: types contain no binders. -/
def substUniq (s : Std.HashMap Int Ty) : Ty → Ty
  | .var a     => s.getD a.uniq (.var a)
  | .app t u   => .app (substUniq s t) (substUniq s u)
  | .arrow t u => .arrow (substUniq s t) (substUniq s u)
  | t          => t

/-- Peel `n` arrows off a type (the result of applying `n` term
arguments). -/
def peel : Nat → Ty → Except String Ty
  | 0,     t          => .ok t
  | n + 1, .arrow _ u => peel n u
  | _ + 1, _          => .error "term argument applied to a non-arrow type"

end Ty

/-- Instantiate a signature at type arguments (the caller checks
saturation). -/
def Sig.instantiate (sig : Sig) (ts : List Ty) : Ty :=
  Ty.substUniq (Std.HashMap.ofList ((sig.tvs.zip ts).map fun (v, t) => (v.uniq, t))) sig.ty

/-- A term application spine: head and arguments, outermost last. -/
def Exp.flattenApp : Exp → Exp × List Arg := go []
where
  go (acc : List Arg) : Exp → Exp × List Arg
    | .app e a => go (a :: acc) e
    | e        => (e, acc)

def Arg.isTArg : Arg → Bool
  | .tArg _ => true
  | _       => false

/-- The type of an instantiated head: a bare (argument-less) reference
to a polymorphic name yields its open signature type. -/
def headTy (sig : Sig) (tys : List Ty) : Except String Ty :=
  if tys.isEmpty then .ok sig.ty
  else if sig.tvs.length == tys.length then .ok (sig.instantiate tys)
  else .error "unsaturated type application"

/-- Synthesize the type of an (elaborated) expression — the trusting
transcription of ReWire.Eidos.Types.typeOf, total on lint-clean
programs: binders carry types; `Con`/`Prim`/literal occurrences carry
instantiated types; spines instantiate signatures by substitution.
`joins` maps join-point label uniques to their reconstructed types
(arrows from parameter types to body type — in the reference the type
lives on the label's own signature, which this embedding's `JoinId`
does not carry). Grossly ill-typed input is rejected; everything
subtler is the machine-mode checker's job (Rwv.Eidos.Check, whose
`checkExp` is this function's located, checking twin). -/
partial def typeOf (joins : Std.HashMap Int Ty) : Exp → Except String Ty
  | .var x        => .ok x.sig.ty
  | .con t _      => .ok t
  | .prim t _     => .ok t
  | .litInt t _   => .ok t
  | .litStr _     => .ok (.con "String")
  | .litList t _  => .ok t
  | .litVec t _   => .ok t
  | .lam x b      => Ty.arrow x.sig.ty <$> typeOf joins b
  | .letE (.join j xs e) body => do
      let bt ← typeOf joins e
      let jt := xs.foldr (fun x acc => Ty.arrow x.sig.ty acc) bt
      typeOf (joins.insert j.uniq jt) body
  | .letE _ body  => typeOf joins body
  | .jump l args  =>
      match joins[l.uniq]? with
      | some t => Ty.peel args.length t
      | none   => .error s!"unbound join point: {l.occ}#{l.uniq}"
  | .cases t _ _ _ => .ok t
  | e@(.app ..)   => do
      let (h, args) := Exp.flattenApp e
      match h with
      | .var x => do
          let (tas, eas) := args.span Arg.isTArg
          let tys := tas.filterMap fun | .tArg t => some t | _ => none
          if eas.any Arg.isTArg then
            .error "type arguments must form a prefix of the application spine"
          else do
            Ty.peel eas.length (← headTy x.sig tys)
      | _ =>
          if args.any Arg.isTArg then .error "type argument applied to a non-variable head"
          else do Ty.peel args.length (← typeOf joins h)

end Rwv.Eidos

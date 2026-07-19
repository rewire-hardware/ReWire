/-
Type utilities for the Eidos deep embedding: application/arrow
flattening, evaluation of nat-closed type arithmetic, normalization,
and equality modulo normalization — transcribing the corresponding
parts of ReWire.Eidos.Types (doc/eidos.md §3.1, §5).
-/
import Rwv.Eidos.Syntax

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

end Ty

end Rwv.Eidos

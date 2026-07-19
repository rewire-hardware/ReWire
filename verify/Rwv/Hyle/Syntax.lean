/-
The Hyle bit-level IR, deep-embedded: a transcription of the abstract
syntax of doc/hyle.md §3 (implementation: rewire-backend
ReWire.Hyle.Syntax). Every expression node caches its width, exactly as
the Haskell AST does; the checker re-verifies the cache. Source
annotations are omitted (they are semantically inert and excluded from
every equality on the Haskell side).
-/

namespace Rwv.Hyle

/-- A bit vector of runtime-determined width: the value domain BV(n) of
doc/hyle.md §2, packaged with its width. `BV 0` is the unit ⟨⟩. -/
structure BV where
  width : Nat
  bits  : BitVec width
deriving DecidableEq, Repr

namespace BV

def mk' {n : Nat} (b : BitVec n) : BV := ⟨n, b⟩

def ofNat (w v : Nat) : BV := ⟨w, BitVec.ofNat w v⟩

def nil : BV := ofNat 0 0

def zero (w : Nat) : BV := ofNat w 0

/-- The natural-number reading (unsigned; §2). -/
def nat (x : BV) : Nat := x.bits.toNat

instance : Inhabited BV := ⟨nil⟩

end BV

/-- The 30 primitive operations (doc/hyle.md §3.3, §5.2). Static
parameters (target widths, repetition count) live in the constructor,
as in the Haskell `Op`. -/
inductive Op where
  | add | sub | mul | udiv | umod | pow
  | and | or | xor | not
  | shl | lshr | ashr
  | eq | ne | ult | ule | ugt | uge | slt | sle | sgt | sge
  | redand | redor | redxor
  | zext (m : Nat) | sext (m : Nat) | trunc (m : Nat)
  | rep (k : Nat)
deriving DecidableEq, Repr

/-- Expressions (doc/hyle.md §3.2): 10 constructors, each caching its
width. `slice` is the LSB-indexed static slice e[i +: w]. -/
inductive Exp where
  | lit    (v : BV)
  | undef  (w : Nat)
  | var    (w : Nat) (x : String)
  | cat    (e₁ e₂ : Exp)
  | slice  (i : Nat) (w : Nat) (e : Exp)
  | prim   (w : Nat) (op : Op) (args : List Exp)
  | call   (w : Nat) (f : String) (args : List Exp)
  | xcall  (w : Nat) (ext : String) (generics : List Nat) (args : List Exp)
  | ite    (w : Nat) (c t e : Exp)
  | letE   (w : Nat) (x : String) (rhs body : Exp)
deriving Repr, Inhabited

namespace Exp

/-- The cached width of an expression (`sizeOf` on the Haskell side —
for `cat` the sum of the operands', for `slice` the slice width). -/
def width : Exp → Nat
  | .lit v         => v.width
  | .undef w       => w
  | .var w _       => w
  | .cat e₁ e₂     => e₁.width + e₂.width
  | .slice _ w _   => w
  | .prim w _ _    => w
  | .call w _ _    => w
  | .xcall w _ _ _ => w
  | .ite w _ _ _   => w
  | .letE w _ _ _  => w

end Exp

/-- A definition signature: parameter widths and result width. -/
structure Sig where
  params : List Nat
  result : Nat
deriving DecidableEq, Repr

/-- A definition (doc/hyle.md §3.4): first-order, saturated calls,
non-recursive (checked). `noInline` and doc text are compilation
pragmas/metadata with no semantic content; only the name, signature,
parameters, and body are modeled. -/
structure Defn where
  name   : String
  sig    : Sig
  params : List String
  body   : Exp
deriving Repr

/-- Extern kinds (doc/hyle.md §3.5): combinational, or sequential with
optional clock/reset port names. -/
inductive ExternKind where
  | comb
  | seq (clock reset : Option String)
deriving DecidableEq, Repr

/-- An extern declaration: generics, kind, ports, and the optional model
definition that pins its interpretation (doc/hyle.md §6.1). -/
structure Extern where
  name     : String
  generics : List String
  kind     : ExternKind
  ins      : List (String × Nat)
  outs     : List (String × Nat)
  model    : Option String
deriving Repr

/-- A device register with its initial value (doc/hyle.md §3.6). -/
structure Register where
  name : String
  width : Nat
  init : BV
deriving Repr

/-- A device instance of a sequential extern. -/
structure Instance where
  name     : String
  ext      : String
  generics : List Nat
deriving Repr

/-- Device body statements (doc/hyle.md §3.6): local wire, output
assignment, register next-state, instance input. -/
inductive Stmt where
  | sLet    (x : String) (e : Exp)
  | sOutput (o : String) (e : Exp)
  | sNext   (r : String) (e : Exp)
  | sInstIn (inst port : String) (e : Exp)
deriving Repr

/-- The device: ports, registers, instances, and a body of statements
processed in order (doc/hyle.md §6.3). -/
structure Device where
  name      : String
  inputs    : List (String × Nat)
  outputs   : List (String × Nat)
  registers : List Register
  instances : List Instance
  body      : List Stmt
deriving Repr

/-- A program: externs, definitions, and exactly one device. -/
structure Program where
  externs : List Extern
  defns   : List Defn
  device  : Device
deriving Repr

end Rwv.Hyle

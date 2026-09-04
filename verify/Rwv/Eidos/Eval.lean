/-
The Synolon pure evaluator: call-by-value big-step evaluation of the
pure fragment of Synolon (doc/synolon.md §5.2) with the full builtin
denotation table of doc/synolon.md §6, over the committed value domain
(Rwv.Eidos.Value). The bit-level rows ("as hyle op") reuse the Hyle
primitive denotations (Rwv.Hyle.Semantics.Sem.evalOp) through the bv
readings, so SMT-LIB division/modulus, shift, and reduction conventions
are shared with the Hyle side by construction.

API (see also the exported wrappers at the bottom):
- `Eval.Env`   — binder unique ↦ value (innermost first; `Val.closL`
                 closures store exactly this representation).
- `eval Δ defns fuel env e` — evaluate `e` (join environment empty).
- `applyVal Δ defns fuel f a` — apply a function value (closL/closD).
- `mkDefnMap`  — key a program's definitions by name unique.

Fuel discipline: every evaluator function consumes one unit of fuel on
entry, so fuel bounds total evaluation *work* (steps), not merely the
call depth. Exhaustion is an error (a well-formed Synolon program
has an acyclic pure call graph, so exhaustion at generous fuel means
ill-formed input — e.g. recursion — or too little fuel). Pass generous
fuel (10^6 is plenty for the test corpus).

Decisions where the spec leaves latitude (candidates for doc folding):
1. Joins live in a separate lexical environment component (`JEnv`),
   not in `Val` — join continuations are not first-class. Lambda
   closures do not capture the join environment: a jump out of a
   closure body to an enclosing join is rejected as unbound (jumps are
   tail transfers, so Synolon programs cannot exercise this).
2. Literal alternatives match via `rep` at the scrutinee's
   representation width (integer: 128-bit residue; Finite: nbits-width
   residue; Vec n Bool: width-n residue) — exactly the translated
   if-chain's comparison. For lint-passing programs (literals fit)
   this coincides with exact numeric comparison.
3. An integer literal at `Finite n` is fit-checked (0 ≤ v < n; error
   otherwise) rather than wrapped to the nbits-width residue as the
   compiled code would — divergence only on lint-rejected programs.
   At `Vec n Bool` it denotes the width-n residue (matching the
   compiled `bitVec` truncation, including negative literals); at
   `Integer` the 128-bit residue.
4. Partial application is supported for definitions (`Val.closD`) and
   lambdas (`Val.closL`); constructor and builtin occurrences must be
   saturated (mirroring ToHyle's applyFn, which accepts only
   global-definition heads and lambdas as function-valued arguments).
5. The default alternative is accepted anywhere in the alternative
   list (the syntactically first one is used); non-default
   alternatives are tried in syntactic order first (doc/synolon.md §5.2).
6. `litList` evaluates to `Val.vec`, like `litVec` — a list literal is
   only legal as `rwPrimVecFromList`'s static argument (which passes
   it through) and in extern/cryptol argument positions.
7. doc/synolon.md §6 static-argument requirements mostly vanish at evaluation time
   (arguments are already values); their side conditions are checked
   dynamically: `rwPrimFinite` range, `rwPrimToFinite` 2^m ≤ n,
   `rwPrimFromFinite` n ≤ 2^m, `rwPrimBitSlice` j+1 ≥ i,
   `rwPrimVecFromList` length = n, min/maxBound n ≥ 1.
8. Eliminated (`bind`/`ret`/…) and reserved (`usingExtern`/
   `vecFoldR`/`vecFoldL`) builtins evaluate to errors naming the
   builtin — the machine fragment never reaches them.
9. FOREIGN rows (doc/synolon.md §5.5): a SATURATED
   `rwPrimCryptol f n impl ā` (`f`/`n` string literals, as ToHyle
   requires after inlining) evaluates the value arguments ā, reps
   them, applies the foreign denotation `Δ.cryF f n τ_impl` — which
   the drivers may instantiate from a foreign artifact (the
   model-carrying trust boundary for Cryptol; with the default empty
   hook the row errors, naming the builtin) — and `decode`s the
   result at the row's result type (both types read off the
   occurrence's carried instantiated type, τ_impl being its third
   argument type). The canonicality-checked `decode` makes a
   non-canonical foreign result a loud error, never a junk value.
   `rwPrimExtern` dispatches on its OWN implementation argument (the
   occurrence's seventh argument, classified by `externModelless`):
   a MODEL-LESS occurrence (the neutered-placeholder idiom) reads
   through the bit-level extern environment η (`evalExt`); a
   MODEL-CARRYING occurrence means exactly its implementation
   argument applied to the value arguments — an ordinary source-side
   application, so the extern's semantics comes from the source
   artifact, never from a target program. Under-application of the
   model-less row is an error — the drivers eta-saturate to signature
   arity first, exactly as rwc's own normalization (ToHyle's
   etaExpand) does.
-/
import Rwv.Eidos.Value
import Rwv.Eidos.Decode
import Rwv.Hyle.Semantics
import Std.Data.HashMap

namespace Rwv.Eidos

open Std (HashMap)
open Rwv.Hyle (BV)

namespace Eval

/-- The evaluation environment: binder unique ↦ value, innermost
binding first (shadowing by consing). `Val.closL` closures store
exactly this representation. -/
abbrev Env := List (Int × Val)

/-- A join-point continuation (doc/synolon.md §5.2): parameters, the captured value
and join environments (lexical — a join body may jump to joins that
enclose its own binding), and the body. -/
inductive JoinClos where
  | mk (params : List Id) (env : Env) (joins : List (Int × JoinClos)) (body : Exp)

/-- The join environment: join-label unique ↦ continuation. Separate
from `Env` because join continuations are not first-class values. -/
abbrev JEnv := List (Int × JoinClos)

/-- The static evaluation context: the datatype environment and the
global definitions keyed by name unique. -/
structure Ctx where
  Δ     : DEnv
  defns : HashMap Int Defn

def fuelErr : String := "eval: fuel exhausted (recursion in the pure fragment, or too little fuel)"

/-! ## Value and type helpers -/

/-- The prim-basis `Bool` type (bare names — prim-basis names are not
qualified). -/
def boolTy : Ty := .con "Bool"

/-- Booleans are ADT values of the prim-basis `Bool` (doc/synolon.md §5.1). -/
def boolVal (b : Bool) : Val := .con boolTy (if b then "True" else "False") []

/-- The bv reading (doc/synolon.md §5.1) of a value as a Hyle bit vector, via the
data-to-bits representation `Val.rep` (on `Vec n Bool` values this is
exactly the MSB-first bit reading). -/
def valToBits (Δ : DEnv) (fuel : Nat) (v : Val) : Except String BV :=
  Val.rep Δ fuel v

/-- bv⁻¹: a Hyle bit vector as a `Vec n Bool` value, head = MSB. -/
def bitsToVec (x : BV) : Val :=
  .vec ((List.range x.width).map fun i => boolVal (x.bits.getMsbD i))

def vecVal (who : String) : Val → Except String (List Val)
  | .vec xs => pure xs
  | _       => throw s!"{who}: expected a vector value"

def finVal (who : String) : Val → Except String (Nat × Nat)
  | .finite bound i => pure (bound, i)
  | _               => throw s!"{who}: expected a Finite value"

def intVal (who : String) : Val → Except String (BitVec 128)
  | .integer v => pure v
  | _          => throw s!"{who}: expected an Integer value"

/-- The length of a `Vec n τ` type (n must be nat-closed). -/
def vecLen (who : String) (t : Ty) : Except String Nat :=
  match Ty.flatten t with
  | (.con "Vec", [n, _]) =>
      match Ty.evalNat n with
      | some k => pure k
      | none   => throw s!"{who}: open Vec length"
  | _ => throw s!"{who}: expected a Vec type"

/-- The bound of a `Finite n` type. -/
def finBound (who : String) (t : Ty) : Except String Nat :=
  match Ty.flatten t with
  | (.con "Finite", [n]) =>
      match Ty.evalNat n with
      | some k => pure k
      | none   => throw s!"{who}: open Finite bound"
  | _ => throw s!"{who}: expected a Finite type"

/-- The index of a `Proxy n` type (the doc/synolon.md §6 rows whose static data
rides on a Proxy argument's type, read off the builtin occurrence's
instantiated signature). -/
def proxyNatOf (who : String) (t : Ty) : Except String Nat :=
  match Ty.flatten t with
  | (.con "Proxy", [n]) =>
      match Ty.evalNat n with
      | some k => pure k
      | none   => throw s!"{who}: open Proxy index"
  | _ => throw s!"{who}: expected a Proxy type"

def isBoolTy (t : Ty) : Bool :=
  match Ty.flatten t with
  | (.con "Bool", []) => true
  | _ => false

/-- Peel k arrows off a type (the result type after applying k
arguments). -/
def dropArrows : Nat → Ty → Ty
  | 0, t => t
  | k + 1, .arrow _ t₂ => dropArrows k t₂
  | _ + 1, t => t

def domTy (who : String) (doms : List Ty) (k : Nat) : Except String Ty :=
  match doms[k]? with
  | some t => pure t
  | none   => throw s!"{who}: missing argument type in the instantiated builtin type"

/-- Flatten an application spine to its head and term arguments; type
arguments are erased (they may appear in poly-mode fixtures; doc/synolon.md §5.2
evaluates them away). -/
def flattenApp (e : Exp) : Exp × List Exp := go [] e
where go (acc : List Exp) : Exp → Exp × List Exp
  | .app f (.eArg a) => go (a :: acc) f
  | .app f (.tArg _) => go acc f
  | e => (e, acc)

/-- The neutered implementation-argument literal marking a MODEL-LESS
extern occurrence (what `ReWire.Eidos.Externs` leaves when the user
supplied no model). -/
def externPlaceholder : String := "Extern expression placeholder"

/-- Strip leading lambda binders (the eta-saturation wrapper shape). -/
def stripLams : Exp → Exp
  | .lam _ e => stripLams e
  | e => e

/-- Whether an extern occurrence is MODEL-LESS, decided from the
SOURCE artifact alone: its implementation argument is the neutered
`rwPrimError "Extern expression placeholder"` application (possibly
eta-wrapped). This one classifier is shared by the evaluator, the
verified compiler, and the drivers, so the source and target readings
always dispatch together — and it is UNTRUSTED: a misclassification
merely selects the other reading for the occurrence, which the
symbolic comparison then rejects; it can never produce a false
acceptance. -/
def externModelless (impl : Exp) : Bool :=
  match flattenApp (stripLams impl) with
  | (.prim _ .error, .litStr s :: _) => s == externPlaceholder
  | _ => false

/-- The generic parameters of an extern occurrence, read from the
descriptor list literal (the occurrence's FIRST argument) exactly as
the fold reads it (ToHyle `applyExtern.generics`): each element is a
`(,) "name" (value :: Integer)` pair, and an empty name defaults to
`g<i>` — mirroring the fold, so the emitted declaration's generic
names match the scan's cross-check. The fold fabricates a zero value
for elements outside this shape; a validator must refuse rather than
agree with a fabricated value, so those extract to `none` (outside
the certified fragment), as do negative values. Shared by the
evaluator, the verified compiler, and the drivers: both sides key the
η interpretation by the SAME extracted values, and the target call's
values meet them through the uninterpreted-symbol identity. -/
def externGenerics : Exp → Option (List (String × Nat))
  | .litList _ es => go 0 es
  | _ => none
where go (i : Nat) : List Exp → Option (List (String × Nat))
  | [] => some []
  | e :: rest =>
      match flattenApp e with
      | (.con _ "(,)", [.litStr p, .litInt _ v]) =>
          if h : 0 ≤ v then
            match go (i + 1) rest with
            | some ps => some ((if p = "" then s!"g{i}" else p, v.toNat) :: ps)
            | none => none
          else none
      | _ => none

/-- The denotation of an integer literal at its carried type (doc/synolon.md §5.1):
the 128-bit residue at `Integer`; the (fit-checked) value at
`Finite n`; the MSB-first width-n residue at `Vec n Bool`. -/
def litIntVal (ty : Ty) (n : Int) : Except String Val :=
  match Ty.flatten ty with
  | (.con "Integer", []) => pure (.integer (BitVec.ofInt 128 n))
  | (.con "Finite", [bt]) =>
      match Ty.evalNat bt with
      | some k =>
          if 0 ≤ n ∧ n < (k : Int) then pure (.finite k n.toNat)
          else throw s!"integer literal {n} does not fit Finite {k}"
      | none => throw "integer literal at an open Finite bound"
  | (.con "Vec", [lt, et]) =>
      if isBoolTy et then
        match Ty.evalNat lt with
        | some w => pure (bitsToVec ⟨w, BitVec.ofInt w n⟩)
        | none   => throw "integer literal at an open Vec length"
      else throw "integer literal at a non-Bool Vec type"
  | _ => throw s!"integer literal {n} at an unsupported type"

/-- Literal-alternative matching (doc/synolon.md §5.2): the scrutinee equals the
literal's denotation at the scrutinee's type — uniformly through
`rep`, mirroring the translated if-chain's comparison at the
scrutinee's representation width. -/
def litMatches (Δ : DEnv) (fuel : Nat) (v : Val) (n : Int) : Except String Bool := do
  let x ← valToBits Δ fuel v
  pure (x.bits == BitVec.ofInt x.width n)

/-! ## Bit-level builtin rows, through the Hyle op denotations -/

/-- A binary `Vec n Bool → Vec n Bool → Vec n Bool` row ("as hyle
op"): read both operands, apply the Hyle denotation, read back. -/
def bvBinArith (Δ : DEnv) (fuel : Nat) (op : Rwv.Hyle.Op) (v w : Val) : Except String Val := do
  let x ← valToBits Δ fuel v
  let y ← valToBits Δ fuel w
  let r ← Rwv.Hyle.Sem.evalOp op [x, y]
  pure (bitsToVec r)

/-- A binary `Vec n Bool → Vec n Bool → Bool` comparison row. -/
def bvBinCmp (Δ : DEnv) (fuel : Nat) (op : Rwv.Hyle.Op) (v w : Val) : Except String Val := do
  let x ← valToBits Δ fuel v
  let y ← valToBits Δ fuel w
  let r ← Rwv.Hyle.Sem.evalOp op [x, y]
  pure (boolVal (r.nat ≠ 0))

/-- A reduction row (`RAnd`/`ROr`/`RXOr` and their negations). -/
def bvRed (Δ : DEnv) (fuel : Nat) (op : Rwv.Hyle.Op) (negated : Bool) (v : Val) : Except String Val := do
  let x ← valToBits Δ fuel v
  let r ← Rwv.Hyle.Sem.evalOp op [x]
  pure (boolVal (if negated then r.nat = 0 else r.nat ≠ 0))

/-! ## The evaluator -/

mutual

/-- E⟦e⟧ρ (doc/synolon.md §5.2): call-by-value big-step evaluation. Application
spines are flattened (type arguments erased); heads dispatch to
environment/definition lookup, constructor values, or the builtin
table. -/
def evalCore (C : Ctx) (fuel : Nat) (env : Env) (jenv : JEnv) (e : Exp)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String Val :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match flattenApp e with
    | (.var x, args) => do
        let vs ← evalList C fuel env jenv args E
        match env.lookup x.uniq with
        | some v => applyMany C fuel v vs E
        | none =>
            match C.defns.get? x.uniq with
            | some d => callDefn C fuel d vs E
            | none   => throw s!"unbound variable {x.occ}#{x.uniq}"
    | (.con ty c, args) => do
        -- The Con node carries the full instantiated function type;
        -- the value's type is the result type after the arrows.
        let vs ← evalList C fuel env jenv args E
        let (dts, resTy) := Ty.flattenArrow ty
        if vs.length == dts.length then pure (.con resTy c vs)
        else throw s!"unsaturated constructor {c} ({vs.length} of {dts.length} arguments)"
    | (.prim ty b, args) =>
        if b == .cryptol then
          match args with
          | .litStr f :: .litStr n :: _impl :: rest => evalCry C fuel env jenv ty f n rest E
          | _ => throw "rwPrimCryptol: malformed foreign application"
        else if b == .«extern» then
          match args with
          | ps :: _clk :: _rst :: _as :: _rs :: .litStr s :: impl :: _inst :: rest =>
              if externModelless impl then
                match externGenerics ps with
                | some gps => evalExt C fuel env jenv ty s (gps.map (·.2)) rest E
                | none => throw "rwPrimExtern: non-literal extern parameter (outside the certified fragment)"
              else do
                -- The MODEL-CARRYING extern row: the occurrence means
                -- exactly its own implementation argument applied to
                -- the value arguments — the source-side semantics
                -- comes from the source artifact, never from the
                -- target being checked.
                let vs ← evalList C fuel env jenv rest E
                let fv ← evalCore C fuel env jenv impl E
                applyMany C fuel fv vs E
          | _ => throw "rwPrimExtern: malformed foreign application"
        else do
          let vs ← evalList C fuel env jenv args E
          evalBuiltin C fuel ty b vs E
    | (.lam x body, args) => do
        let vs ← evalList C fuel env jenv args E
        applyMany C fuel (.closL x env body) vs E
    | (.litInt ty n, []) => litIntVal ty n
    | (.litStr s, [])    => pure (.str s)
    | (.litVec _ es, []) => do pure (.vec (← evalList C fuel env jenv es E))
    | (.litList _ es, []) => do pure (.vec (← evalList C fuel env jenv es E))
    | (.letE bnd body, args) => do
        let v ← (match bnd with
          | .nonRec x rhs => do
              let rv ← evalCore C fuel env jenv rhs E
              evalCore C fuel ((x.uniq, rv) :: env) jenv body E
          | .recB _ => throw "recursive let binding (outside the machine fragment)"
          | .join l ps jbody =>
              evalCore C fuel env ((l.uniq, JoinClos.mk ps env jenv jbody) :: jenv) body E)
        let vs ← evalList C fuel env jenv args E
        applyMany C fuel v vs E
    | (.jump l es, []) => do
        let vs ← evalList C fuel env jenv es E
        match jenv.lookup l.uniq with
        | some (.mk ps cenv cjenv body) =>
            if vs.length == ps.length then
              evalCore C fuel (((ps.map (·.uniq)).zip vs) ++ cenv) cjenv body E
            else throw s!"jump to {l.occ}: arity mismatch ({vs.length} of {ps.length} arguments)"
        | none => throw s!"jump to an unbound join point {l.occ}#{l.uniq}"
    | (.cases _ scrut binder alts, args) => do
        let sv ← evalCore C fuel env jenv scrut E
        let v  ← tryAlts C fuel env jenv binder sv alts none E
        let vs ← evalList C fuel env jenv args E
        applyMany C fuel v vs E
    | (_, _) => throw "ill-formed expression (application of a literal or jump)"
termination_by fuel

/-- The Cryptol foreign row (decision note 9), after `evalCore`'s
shape dispatch: evaluate the value arguments `rest`, rep them, apply
the foreign denotation keyed by the module file `f`, the function
name `n`, and the impl monotype (the occurrence type's third argument
type), and decode the result at the impl type's result type. -/
def evalCry (C : Ctx) (fuel : Nat) (env : Env) (jenv : JEnv) (pty : Ty)
    (f n : String) (rest : List Exp)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String Val :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 => do
      let vs ← evalList C fuel env jenv rest E
      let ity ← domTy "rwPrimCryptol" (Ty.flattenArrow pty).1 2
      if rest.length = (Ty.flattenArrow ity).1.length then
        match C.Δ.cryF f n ity with
        | some den => do
            let reps ← vs.mapM (fun v => valToBits C.Δ fuel v)
            let bv ← den reps
            decode C.Δ fuel (Ty.flattenArrow ity).2 bv
        | none => throw "rwPrimCryptol: no denotation for this instantiation (no foreign environment?)"
      else throw "rwPrimCryptol: unsaturated foreign application"
termination_by fuel

/-- The MODEL-LESS extern row (decision note 9, the η tier), after
`evalCore`'s shape dispatch: the extern reads through the bit-level
extern environment — rep the arguments at the current fuel, apply the
interpretation to their concatenation (errors are loud), and decode
the result at the row's result type (the impl monotype is the
occurrence type's seventh argument type). The canonicality-checked
decode is the gate that confines the bit-level quantification to
representation images. (Model-carrying occurrences never reach here:
`evalCore` dispatches them to their own implementation argument.) -/
def evalExt (C : Ctx) (fuel : Nat) (env : Env) (jenv : JEnv) (pty : Ty)
    (s : String) (gs : List Nat) (rest : List Exp)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String Val :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 => do
      let vs ← evalList C fuel env jenv rest E
      let ity ← domTy "rwPrimExtern" (Ty.flattenArrow pty).1 6
      if rest.length = (Ty.flattenArrow ity).1.length then
        match E s gs with
        | some f => do
            let reps ← vs.mapM (fun v => valToBits C.Δ fuel v)
            let bv ← f (Rwv.Hyle.Sem.bvcat reps)
            decode C.Δ fuel (Ty.flattenArrow ity).2 bv
        | none => throw s!"rwPrimExtern: no model denotation for extern {s}"
      else throw "rwPrimExtern: unsaturated foreign application"
termination_by fuel

/-- Evaluate a list of expressions left to right. -/
def evalList (C : Ctx) (fuel : Nat) (env : Env) (jenv : JEnv) (es : List Exp)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String (List Val) :=
  match fuel, es with
  | 0, _ => throw fuelErr
  | _ + 1, [] => pure []
  | fuel + 1, e :: rest => do
      let v  ← evalCore C fuel env jenv e E
      let vs ← evalList C fuel env jenv rest E
      pure (v :: vs)
termination_by fuel

/-- A call to a global definition (doc/synolon.md §5.2): under-application yields a
`closD` partial application; saturation evaluates the body with the
parameters bound in a fresh environment (definitions are closed);
over-application applies the result to the leftover arguments. -/
def callDefn (C : Ctx) (fuel : Nat) (d : Defn) (vs : List Val)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String Val :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
      if vs.length < d.params.length then pure (.closD d.name vs)
      else do
        let env' := (d.params.map (·.uniq)).zip vs
        let v ← evalCore C fuel env' [] d.body E
        applyMany C fuel v (vs.drop d.params.length) E
termination_by fuel

/-- Apply a function value — a lambda closure or a (possibly partial)
definition application — to one argument. -/
def applyValCore (C : Ctx) (fuel : Nat) (f a : Val)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String Val :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
      match f with
      | .closL x cenv body => evalCore C fuel ((x.uniq, a) :: cenv) [] body E
      | .closD g pre =>
          match C.defns.get? g.uniq with
          | some d => callDefn C fuel d (pre ++ [a]) E
          | none   => throw s!"closure over an unknown definition {g.occ}#{g.uniq}"
      | _ => throw "cannot apply a non-function value"
termination_by fuel

/-- Apply a function value to arguments left to right. -/
def applyMany (C : Ctx) (fuel : Nat) (f : Val) (as' : List Val)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String Val :=
  match fuel, as' with
  | 0, _ => throw fuelErr
  | _ + 1, [] => pure f
  | fuel + 1, a :: rest => do
      let v ← applyValCore C fuel f a E
      applyMany C fuel v rest E
termination_by fuel

/-- Map a function value over argument values (`VecMap`,
`VecGenerate`: "applied semantically, element by element"). -/
def applyAll (C : Ctx) (fuel : Nat) (f : Val) (xs : List Val)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String (List Val) :=
  match fuel, xs with
  | 0, _ => throw fuelErr
  | _ + 1, [] => pure []
  | fuel + 1, x :: rest => do
      let y  ← applyValCore C fuel f x E
      let ys ← applyAll C fuel f rest E
      pure (y :: ys)
termination_by fuel

/-- Case selection (doc/synolon.md §5.2): the first matching non-default
alternative fires, binding the case binder to the scrutinee's value
and the field binders to its components; the default (syntactically
first when present) fires only when no other alternative matches,
binding only the case binder. -/
def tryAlts (C : Ctx) (fuel : Nat) (env : Env) (jenv : JEnv) (binder : Id) (v : Val)
    (alts : List Alt) (dflt : Option Alt)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String Val :=
  match fuel, alts with
  | 0, _ => throw fuelErr
  | fuel + 1, [] =>
      match dflt with
      | some (.mk _ _ body) => evalCore C fuel ((binder.uniq, v) :: env) jenv body E
      | none => throw "case: no matching alternative (and no default)"
  | fuel + 1, alt :: rest =>
      match alt with
      | .mk .default _ _ =>
          tryAlts C fuel env jenv binder v rest (dflt.orElse fun _ => some alt) E
      | .mk (.dataAlt cn) bs body =>
          match v with
          | .con _ cv fields =>
              if cn == cv then
                if bs.length == fields.length then
                  evalCore C fuel (((bs.map (·.uniq)).zip fields) ++ (binder.uniq, v) :: env) jenv body E
                else throw s!"case: constructor {cn} arity mismatch ({bs.length} binders, {fields.length} fields)"
              else tryAlts C fuel env jenv binder v rest dflt E
          | _ => throw "case: constructor pattern against a non-constructor value"
      | .mk (.litAlt n) _ body => do
          if (← litMatches C.Δ fuel v n) then
            evalCore C fuel ((binder.uniq, v) :: env) jenv body E
          else tryAlts C fuel env jenv binder v rest dflt E
termination_by fuel

/-- The builtin denotation table of doc/synolon.md §6, complete (all 60 rows),
plus error arms for the four retired enum entries. The occurrence's
carried instantiated type `pty` supplies the static data: result
widths and bounds from the result type, Proxy indices from the
argument types. -/
def evalBuiltin (C : Ctx) (fuel : Nat) (pty : Ty) (b : Builtin) (vs : List Val)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String Val :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    let Δ := C.Δ
    let (doms, res) := Ty.flattenArrow pty
    match b, vs with
    -- Conversions, Finite, and miscellany.
    | .error, _ => Δ.zeroVal fuel (dropArrows vs.length pty)
    | .bits, [v] => do
        let x ← intVal "rwPrimBits" v
        pure (bitsToVec ⟨128, x⟩)
    | .resize, [v] => do
        let m ← vecLen "rwPrimResize" res
        let x ← valToBits Δ fuel v
        pure (bitsToVec ⟨m, x.bits.setWidth m⟩)
    | .natVal, [_] => do
        let pt ← domTy "rwPrimNatVal" doms 0
        let n  ← proxyNatOf "rwPrimNatVal" pt
        pure (.integer (BitVec.ofNat 128 n))
    | .bitSlice, [v, jv, iv] => do
        let (_, j) ← finVal "rwPrimBitSlice" jv
        let (_, i) ← finVal "rwPrimBitSlice" iv
        if j + 1 < i then throw s!"rwPrimBitSlice: invalid slice (j: {j}, i: {i})"
        else do
          let m := j + 1 - i
          let x ← valToBits Δ fuel v
          pure (bitsToVec ⟨m, x.bits.extractLsb' i m⟩)
    | .bitIndex, [v, iv] => do
        let (_, i) ← finVal "rwPrimBitIndex" iv
        let x ← valToBits Δ fuel v
        pure (boolVal (x.bits.getLsbD i))
    | .finite, [v] => do
        let n ← finBound "rwPrimFinite" res
        let i ← intVal "rwPrimFinite" v
        if i.toNat < n then pure (.finite n i.toNat)
        else throw s!"rwPrimFinite: {i.toNat} is not representable in Finite {n}"
    | .finiteMinBound, [] => do
        let n ← finBound "rwPrimFiniteMinBound" res
        if n ≥ 1 then pure (.finite n 0)
        else throw "rwPrimFiniteMinBound: Finite 0 is uninhabited"
    | .finiteMaxBound, [] => do
        let n ← finBound "rwPrimFiniteMaxBound" res
        if n ≥ 1 then pure (.finite n (n - 1))
        else throw "rwPrimFiniteMaxBound: Finite 0 is uninhabited"
    | .toFinite, [v] => do
        let n ← finBound "rwPrimToFinite" res
        let x ← valToBits Δ fuel v
        if 2 ^ x.width ≤ n then pure (.finite n x.nat)
        else throw s!"rwPrimToFinite: a bit vector of width {x.width} is not representable in Finite {n}"
    | .toFiniteMod, [v] => do
        let n ← finBound "rwPrimToFiniteMod" res
        let x ← valToBits Δ fuel v
        if n ≥ 1 then pure (.finite n (x.nat % n))
        else throw "rwPrimToFiniteMod: Finite 0 is uninhabited"
    | .fromFinite, [v] => do
        let m ← vecLen "rwPrimFromFinite" res
        let (bound, i) ← finVal "rwPrimFromFinite" v
        if bound ≤ 2 ^ m then pure (bitsToVec (BV.ofNat m i))
        else throw s!"rwPrimFromFinite: Finite {bound} is not representable in {m} bits"
    -- Vectors.
    | .vecFromList, [v] => do
        let xs ← vecVal "rwPrimVecFromList" v
        let n  ← vecLen "rwPrimVecFromList" res
        if xs.length == n then pure (.vec xs)
        else throw s!"rwPrimVecFromList: list of length {xs.length} at Vec {n}"
    | .vecReplicate, [v] => do
        let n ← vecLen "rwPrimVecReplicate" res
        pure (.vec (List.replicate n v))
    | .vecReverse, [v] => do
        let xs ← vecVal "rwPrimVecReverse" v
        pure (.vec xs.reverse)
    | .vecSlice, [_, v] => do
        let pt ← domTy "rwPrimVecSlice" doms 0
        let i  ← proxyNatOf "rwPrimVecSlice" pt
        let n  ← vecLen "rwPrimVecSlice" res
        let xs ← vecVal "rwPrimVecSlice" v
        if i + n ≤ xs.length then pure (.vec ((xs.drop i).take n))
        else throw s!"rwPrimVecSlice: slice (i: {i}, n: {n}) out of range for Vec {xs.length}"
    | .vecRSlice, [_, v] => do
        let pt ← domTy "rwPrimVecRSlice" doms 0
        let i  ← proxyNatOf "rwPrimVecRSlice" pt
        let n  ← vecLen "rwPrimVecRSlice" res
        let xs ← vecVal "rwPrimVecRSlice" v
        if i + n ≤ xs.length then pure (.vec ((xs.drop (xs.length - i - n)).take n))
        else throw s!"rwPrimVecRSlice: slice (i: {i}, n: {n}) out of range for Vec {xs.length}"
    | .vecIndex, [v, iv] => do
        let xs ← vecVal "rwPrimVecIndex" v
        let (_, i) ← finVal "rwPrimVecIndex" iv
        match xs[i]? with
        | some x => pure x
        | none   => throw s!"rwPrimVecIndex: index {i} out of range for Vec {xs.length}"
    | .vecIndexProxy, [v, _] => do
        let pt ← domTy "rwPrimVecIndexProxy" doms 1
        let n  ← proxyNatOf "rwPrimVecIndexProxy" pt
        let xs ← vecVal "rwPrimVecIndexProxy" v
        match xs[n]? with
        | some x => pure x
        | none   => throw s!"rwPrimVecIndexProxy: index {n} out of range for Vec {xs.length}"
    | .vecConcat, [v, w] => do
        let xs ← vecVal "rwPrimVecConcat" v
        let ys ← vecVal "rwPrimVecConcat" w
        pure (.vec (xs ++ ys))
    | .vecMap, [f, v] => do
        let xs ← vecVal "rwPrimVecMap" v
        let ys ← applyAll C fuel f xs E
        pure (.vec ys)
    | .vecGenerate, [f] => do
        let n  ← vecLen "rwPrimVecGenerate" res
        let ys ← applyAll C fuel f ((List.range n).map fun i => .finite n i) E
        pure (.vec ys)
    -- Bit-vector operations (through the Hyle op denotations —
    -- SMT-LIB division/modulus by zero, zero-filling shifts).
    | .add, [v, w] => bvBinArith Δ fuel .add  v w
    | .sub, [v, w] => bvBinArith Δ fuel .sub  v w
    | .mul, [v, w] => bvBinArith Δ fuel .mul  v w
    | .div, [v, w] => bvBinArith Δ fuel .udiv v w
    | .mod, [v, w] => bvBinArith Δ fuel .umod v w
    | .pow, [v, w] => bvBinArith Δ fuel .pow  v w
    | .and, [v, w] => bvBinArith Δ fuel .and  v w
    | .or,  [v, w] => bvBinArith Δ fuel .or   v w
    | .xor, [v, w] => bvBinArith Δ fuel .xor  v w
    | .xnor, [v, w] => do
        let x ← valToBits Δ fuel v
        let y ← valToBits Δ fuel w
        let r ← Rwv.Hyle.Sem.evalOp .xor [x, y]
        pure (bitsToVec ⟨r.width, ~~~ r.bits⟩)
    | .not, [v] => do
        let x ← valToBits Δ fuel v
        pure (bitsToVec ⟨x.width, ~~~ x.bits⟩)
    | .lShift,      [v, w] => bvBinArith Δ fuel .shl  v w
    | .rShift,      [v, w] => bvBinArith Δ fuel .lshr v w
    | .rShiftArith, [v, w] => bvBinArith Δ fuel .ashr v w
    | .eq,   [v, w] => bvBinCmp Δ fuel .eq  v w
    | .gt,   [v, w] => bvBinCmp Δ fuel .ugt v w
    | .gtEq, [v, w] => bvBinCmp Δ fuel .uge v w
    | .lt,   [v, w] => bvBinCmp Δ fuel .ult v w
    | .ltEq, [v, w] => bvBinCmp Δ fuel .ule v w
    | .lAnd, [v, w] => do
        let x ← valToBits Δ fuel v
        let y ← valToBits Δ fuel w
        pure (boolVal (x.nat ≠ 0 ∧ y.nat ≠ 0))
    | .lOr, [v, w] => do
        let x ← valToBits Δ fuel v
        let y ← valToBits Δ fuel w
        pure (boolVal (x.nat ≠ 0 ∨ y.nat ≠ 0))
    | .lNot, [v] => do
        let x ← valToBits Δ fuel v
        pure (boolVal (x.nat = 0))
    | .rAnd,  [v] => bvRed Δ fuel .redand false v
    | .rNAnd, [v] => bvRed Δ fuel .redand true  v
    | .rOr,   [v] => bvRed Δ fuel .redor  false v
    | .rNor,  [v] => bvRed Δ fuel .redor  true  v
    | .rXOr,  [v] => bvRed Δ fuel .redxor false v
    | .rXNor, [v] => bvRed Δ fuel .redxor true  v
    | .msBit, [v] =>
        (match v with
          | .vec (h :: _) => pure h
          | .vec []       => throw "rwPrimMSBit: zero-width argument"
          | _             => throw "rwPrimMSBit: expected a Vec value")
    -- Eliminated by procification (doc/eidos.md §7), which consumes
    -- these; none may be reachable from a process.
    | .bind, _ | .ret, _ | .put, _ | .get, _ | .signal, _ | .lift, _ | .extrude, _ | .unfold, _ =>
        throw s!"{b.name}: eliminated before the machine level (must not appear in an evaluated program)"
    -- Foreign mechanisms: denoted by η (doc/synolon.md §5.5), not by this table —
    -- `evalCore` routes their occurrences to the foreign rows above,
    -- so this arm is unreachable from it.
    | .«extern», _ | .cryptol, _ =>
        throw s!"{b.name}: foreign builtin (not evaluable by the pure evaluator)"
    -- Reserved enum entries: no denotation.
    | .usingExtern, _ | .vecFoldR, _ | .vecFoldL, _ =>
        throw s!"{b.name}: reserved builtin (no denotation)"
    | _, _ => throw s!"{b.name}: arity or argument mismatch ({vs.length} argument(s))"
termination_by fuel

end

end Eval

/-- Key a program's definitions by name unique (from `Program.defns`;
the evaluator's definition environment). -/
def mkDefnMap (defns : List Defn) : HashMap Int Defn :=
  HashMap.ofList (defns.map fun d => (d.name.uniq, d))

/-- E⟦e⟧ρ (doc/synolon.md §5.2): evaluate an expression against a
datatype environment, the global definitions (keyed by name unique —
see `mkDefnMap`), and a value environment; the join environment starts
empty. Fuel is consumed on every evaluator step (it bounds total work);
pass a generous amount (e.g. 10^6). -/
def eval (Δ : DEnv) (defns : HashMap Int Defn) (fuel : Nat) (env : Eval.Env) (e : Exp)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String Val :=
  Eval.evalCore ⟨Δ, defns⟩ fuel env [] e E

/-- Apply a function value (`Val.closL`/`Val.closD`) to an argument
(doc/synolon.md §5.2's semantic application, needed by the machine semantics and
used internally by `VecMap`/`VecGenerate`). -/
def applyVal (Δ : DEnv) (defns : HashMap Int Defn) (fuel : Nat) (f a : Val)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String Val :=
  Eval.applyValCore ⟨Δ, defns⟩ fuel f a E

/- Conversion into `Finite 0` fails — the codomain is uninhabited
(matching Data.Finite, and the GHC model's `mod` by zero error). -/
example {C : Eval.Ctx} {E : Rwv.Hyle.Sem.EEnv} :
    Eval.evalBuiltin C 2
      (.arrow (.app (.app (.con "Vec") (.nat 0)) (.con "Bool"))
        (.app (.con "Finite") (.nat 0)))
      .toFiniteMod [.vec []] E
      = .error "rwPrimToFiniteMod: Finite 0 is uninhabited" := by
  rw [Eval.evalBuiltin]
  rfl

end Rwv.Eidos

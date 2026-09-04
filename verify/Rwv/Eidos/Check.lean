/-
The Eidos machine-mode well-formedness judgment (doc/eidos.md §4.1,
§4.4, §7.4, §7.6): the whole-program pre-ToHyle check, and the
hypothesis set for the Eidos↔Hyle simulation metatheorem.

This is a transcription of the machine-relevant subset of
rewire-frontend ReWire.Eidos.Lint — i.e. what the pipeline's pass-8
discipline guarantees (`lintProc` per process, plus the mono/ANF-era
whole-program rules its input already satisfies), structured as the
Haskell is (an `Env`, `checkExp` as the located checking twin of the
trusting `Rwv.Eidos.typeOf`, per-construct rules inline). There is no
mode parameter: this judgment is exactly the machine-stage discipline —
the poly rules plus the mono rules (closed nat-normalized types,
monomorphic signatures, literal fit) plus the mono+ANF first-order
value-binder rule plus the §7.4 machine rules, with the reactive types
out of the type grammar.

Also here: the §7.6 builtin signature table (`builtinSig`, `matchesSig`
— a port of ReWire.Eidos.BuiltinSigs, same schemes, same negative
table-tyvar uniques, same one-way matching with deferred nat-arithmetic
equations), checked at every `Prim` occurrence; and two rules the
Haskell linter does not enforce yet but §7.4 states normatively:

* **pure-acyclicity** — the call graph of the pure definitions
  reachable from a process (block bodies, cell initials, and
  transitively) is acyclic (enforced today downstream by the Hyle
  checker's recursion rule; a fueled DFS in the style of
  Rwv.Hyle.Check.checkRecursion). A `rec` binding *reachable from a
  process* is rejected by the same rule (local recursion breaks the
  same well-foundedness; the reference's evaluator-side counterpart is
  Rwv.Eidos.Eval's rejection of `recB`) — an unreachable `rec` is legal
  Eidos-P and is checked structurally like any binding;
* **representability** — every value binder, block parameter, and
  state cell has a fixed bit width (`DEnv.sizeOf`, shared with the
  translation's sizing in Rwv.Eidos.Value) — so `Integer` = 128 bits,
  recursive datatypes are rejected, open widths are rejected.

Carrier-definition tolerance (doc/eidos.md §4.1): a Synolon program
(the machine-level `.syn` dump) carries only the definitions the fold
lowers, but a legacy Eidos machine-level `.eir` dump is a superset of
that fragment, and hand-written input may be too. Definitions that are
builtin-named (`rwPrim*` signature carriers), have polymorphic
signatures, or have reactive (`ReacT`/`StateT`/`Identity`-mentioning)
types are therefore *skipped* — the fold's `emit` filters them out, so
they are not part of the checked fragment. "Skipped" is not "trusted",
though: a builtin-named carrier must be the *canonical intrinsic stub*
— its body an application headed by the `error` builtin, the shape the
bridge emits — and a reference from checked code (a process, or a
checked definition) to any *other* skipped definition is rejected, so
nothing reachable from the machine ever has an unchecked body.
Skipped definitions still contribute their names to scope and *all*
their binding sites to the global-uniqueness rule, exactly as in the
reference. Relatedly, the global-uniqueness rule covers the
datatype-and-definition fragment only: procify splices one definition
per continuation and passes one binder along goto chains, so a unique
is legitimately bound by several blocks (see the uniqueness section).

Shape notes (differences forced by the embedding, none semantic):

* Annotations are absent, so diagnostics are located by name
  (`occ#uniq`) rather than source span; message texts otherwise follow
  the Haskell.
* `JoinId` carries only an arity, not the label's signature (the `.eir`
  format prints none), so the join rules that compare against the
  declared label signature (`checkLocalBinder` on the label,
  signature-arrow-count vs arity, parameter-type-vs-signature) become:
  arity-vs-parameter-count, and the join's result type is
  *reconstructed* from its body (as the elaborator reconstructs it) and
  compared against the scope's type. The `JoinDef` recorded while
  checking a join's own body carries a placeholder result type — never
  consulted, because a jump to the join from inside its own body always
  fails the tail rule first (joins are not recursive).
* Block labels *do* carry (parser-reconstructed) signatures; the
  occurrence-signature rule applies to them as in the reference.
* Every function is total: recursion over expressions/terminators is
  fueled (fuel bounds *depth*, generously; exhaustion is an "rwv bug?"
  error, as in Rwv.Hyle.Check), everything else is structural.

Three historical Haskell-linter laxities are deliberately *not*
transcribed (this judgment is strictly stronger than the reference
there): terminator-case constructor alternatives compare field-binder
types against the constructor's instantiated field types, exactly as
expression-level `case` does; terminator-case literal alternatives
are distinctness-checked, like expression-level ones; and `checkTop`'s
device-type rule (the root has type `ReacT i o Identity a`) holds
whenever a `top` is present, with or without processes — in a legacy
dump with processes, `top` names the reactive machine-root definition
procify consumed, which retains exactly that type as skipped residue.
A Synolon program carries no `top` (its processes are its roots) and
the rule does not apply.
-/
import Rwv.Eidos.Types
import Rwv.Eidos.Value
import Rwv.Eidos.Parse
import Std.Data.HashMap
import Std.Data.HashSet

namespace Rwv.Eidos

open Std (HashMap HashSet)

/-! ## Rendering (diagnostics; the concrete-syntax spellings) -/

private def renderKind : Kind → String
  | .star     => "*"
  | .nat      => "Nat"
  | .fn k₁ k₂ => s!"({renderKind k₁} -> {renderKind k₂})"

private def renderTy (prec : Nat) : Ty → String
  | .con c   => c
  | .var v   => s!"{v.occ}#{v.uniq}"
  | .nat n   => toString n
  | .arrow t₁ t₂ =>
      let s := s!"{renderTy 1 t₁} -> {renderTy 0 t₂}"
      if prec > 0 then s!"({s})" else s
  | .app t₁ t₂ =>
      let s := s!"{renderTy 1 t₁} {renderTy 2 t₂}"
      if prec > 1 then s!"({s})" else s

def Ty.render (t : Ty) : String := renderTy 0 t

def Sig.render (sig : Sig) : String :=
  if sig.tvs.isEmpty then sig.ty.render
  else
    let vs := " ".intercalate (sig.tvs.map fun v => s!"({v.occ}#{v.uniq} :: {renderKind v.kind})")
    s!"forall {vs}. {sig.ty.render}"

def Id.render (x : Id) : String := s!"{x.occ}#{x.uniq}"

/-! ## Type predicates (ReWire.Eidos.Types) -/

/-- Mentions a reactive-monad-stack constructor. -/
def Ty.reacOrStateT : Ty → Bool
  | .con "ReacT"    => true
  | .con "StateT"   => true
  | .con "Identity" => true
  | .arrow t₁ t₂    => t₁.reacOrStateT || t₂.reacOrStateT
  | .app t₁ t₂      => t₁.reacOrStateT || t₂.reacOrStateT
  | _               => false

/-- Mentions an arrow anywhere (the first-order value-binder rule). -/
def Ty.hasArrowDeep : Ty → Bool
  | .arrow ..  => true
  | .app t₁ t₂ => t₁.hasArrowDeep || t₂.hasArrowDeep
  | _          => false

/-! ## Builtin signatures (doc/eidos.md §7.6; ReWire.Eidos.BuiltinSigs)

The signature scheme every `Prim` occurrence must instantiate, and the
one-way matcher the judgment checks occurrences with. Matching is
first-order and unification-free: scheme variables bind to the
occurrence type's subterms, bindings must agree (up to `natNorm`), and
everything else compares structurally. Type-level arithmetic on the
scheme side (`Vec ((i + n) + m) a` and the like) cannot be inverted by
matching, so arithmetic subterms become deferred equations, checked
only when substitution makes both sides nat-closed and skipped
otherwise — deliberately partial there (sound: it never rejects a
correct instance).

A builtin with no recorded signature (`none`) has its occurrence types
trusted: `extern` (legacy-shaped parameter-list type) and the four
retired enum entries (`unfold`, `vecFoldR`, `vecFoldL`,
`usingExtern`). -/

namespace BuiltinSigs

/-- Table type variables: negative uniques, per the primitive basis
convention (these signatures are only matched against, never inserted
into programs) — the same uniques as the Haskell table's. -/
private def kmonad : Kind := .fn .star .star

private def nN : TyVar := ⟨"n", -9001, .nat⟩
private def mN : TyVar := ⟨"m", -9002, .nat⟩
private def iN : TyVar := ⟨"i", -9003, .nat⟩
private def aS : TyVar := ⟨"a", -9004, .star⟩
private def bS : TyVar := ⟨"b", -9005, .star⟩
private def sS : TyVar := ⟨"s", -9006, .star⟩
private def oS : TyVar := ⟨"o", -9007, .star⟩
private def iS : TyVar := ⟨"i", -9008, .star⟩
private def mM : TyVar := ⟨"m", -9009, kmonad⟩
private def tT : TyVar := ⟨"t", -9010, .fn kmonad kmonad⟩

private def vn  : Ty := .var nN
private def vm' : Ty := .var mN
private def vi' : Ty := .var iN
private def va  : Ty := .var aS
private def vb  : Ty := .var bS
private def vs  : Ty := .var sS
private def vo  : Ty := .var oS
private def vi  : Ty := .var iS
private def vm  : Ty := .var mM
private def vt  : Ty := .var tT

private def bool    : Ty := .con "Bool"
private def unit    : Ty := .con "()"
private def string  : Ty := .con "String"
private def integer : Ty := .con "Integer"

private def vec (n t : Ty) : Ty := .app (.app (.con "Vec") n) t
private def bVec (n : Ty) : Ty := vec n bool
private def list (t : Ty) : Ty := .app (.con "[_]") t
private def proxy (t : Ty) : Ty := .app (.con "Proxy") t
private def finite (t : Ty) : Ty := .app (.con "Finite") t
private def plus (t₁ t₂ : Ty) : Ty := .app (.app (.con "+") t₁) t₂

private def stateT (s m t : Ty) : Ty := .app (.app (.app (.con "StateT") s) m) t
private def reacT (i o m t : Ty) : Ty := .app (.app (.app (.app (.con "ReacT") i) o) m) t

/-- Vec n Bool -> Vec n Bool -> Vec n Bool -/
private def binOp : Option Sig := some ⟨[nN], .arrow (bVec vn) (.arrow (bVec vn) (bVec vn))⟩
/-- Vec n Bool -> Vec n Bool -> Bool -/
private def cmpOp : Option Sig := some ⟨[nN], .arrow (bVec vn) (.arrow (bVec vn) bool)⟩
/-- Vec (1 + n) Bool -> Bool -/
private def redOp : Option Sig := some ⟨[nN], .arrow (bVec (plus (.nat 1) vn)) bool⟩

end BuiltinSigs

open BuiltinSigs in
/-- The signature scheme of each builtin (doc/eidos.md §7.6). -/
def builtinSig : Builtin → Option Sig
  | .error          => some ⟨[aS], .arrow string va⟩
  | .«extern»       => none
  | .cryptol        => some ⟨[aS], .arrow string (.arrow string (.arrow va va))⟩
  | .bind           => some ⟨[mM, aS, bS],
      .arrow (.app vm va) (.arrow (.arrow va (.app vm vb)) (.app vm vb))⟩
  | .ret            => some ⟨[mM, aS], .arrow va (.app vm va)⟩
  | .put            => some ⟨[sS, mM], .arrow vs (stateT vs vm unit)⟩
  | .get            => some ⟨[sS, mM], stateT vs vm vs⟩
  | .signal         => some ⟨[oS, iS, mM], .arrow vo (reacT vi vo vm vi)⟩
  | .lift           => some ⟨[tT, mM, aS], .arrow (.app vm va) (.app (.app vt vm) va)⟩
  | .extrude        => some ⟨[iS, oS, sS, mM, aS],
      .arrow (reacT vi vo (.app (.app (.con "StateT") vs) vm) va) (.arrow vs (reacT vi vo vm va))⟩
  | .unfold         => none
  | .vecFromList    => some ⟨[nN, aS], .arrow (list va) (vec vn va)⟩
  | .vecReplicate   => some ⟨[nN, aS], .arrow va (vec vn va)⟩
  | .vecReverse     => some ⟨[nN, aS], .arrow (vec vn va) (vec vn va)⟩
  | .vecSlice       => some ⟨[iN, nN, mN, aS],
      .arrow (proxy vi') (.arrow (vec (plus (plus vi' vn) vm') va) (vec vn va))⟩
  | .vecRSlice      => some ⟨[iN, nN, mN, aS],
      .arrow (proxy vi') (.arrow (vec (plus (plus vi' vn) vm') va) (vec vn va))⟩
  | .vecIndex       => some ⟨[nN, aS], .arrow (vec vn va) (.arrow (finite vn) va)⟩
  | .vecIndexProxy  => some ⟨[nN, mN, aS],
      .arrow (vec (plus (plus vn vm') (.nat 1)) va) (.arrow (proxy vn) va)⟩
  | .vecConcat      => some ⟨[nN, mN, aS],
      .arrow (vec vn va) (.arrow (vec vm' va) (vec (plus vn vm') va))⟩
  | .vecMap         => some ⟨[nN, aS, bS],
      .arrow (.arrow va vb) (.arrow (vec vn va) (vec vn vb))⟩
  | .vecFoldR       => none
  | .vecFoldL       => none
  | .vecGenerate    => some ⟨[nN, aS], .arrow (.arrow (finite vn) va) (vec vn va)⟩
  | .finite         => some ⟨[nN], .arrow integer (finite vn)⟩
  | .finiteMinBound => some ⟨[nN], finite vn⟩
  | .finiteMaxBound => some ⟨[nN], finite vn⟩
  | .toFinite       => some ⟨[mN, nN], .arrow (bVec vm') (finite vn)⟩
  | .toFiniteMod    => some ⟨[mN, nN], .arrow (bVec vm') (finite vn)⟩
  | .fromFinite     => some ⟨[nN, mN], .arrow (finite vn) (bVec vm')⟩
  | .natVal         => some ⟨[nN], .arrow (proxy vn) integer⟩
  | .bits           => some ⟨[], .arrow integer (bVec (.nat 128))⟩
  | .resize         => some ⟨[mN, nN], .arrow (bVec vn) (bVec vm')⟩
  | .bitSlice       => some ⟨[mN, nN],
      .arrow (bVec vn) (.arrow (finite vn) (.arrow (finite vn) (bVec vm')))⟩
  | .bitIndex       => some ⟨[nN], .arrow (bVec vn) (.arrow (finite vn) bool)⟩
  | .add | .sub | .mul | .div | .mod | .pow => binOp
  | .lAnd | .lOr    => cmpOp
  | .and | .or | .xor | .xnor => binOp
  | .lShift | .rShift | .rShiftArith => binOp
  | .eq | .gt | .gtEq | .lt | .ltEq => cmpOp
  | .lNot           => some ⟨[nN], .arrow (bVec vn) bool⟩
  | .not            => some ⟨[nN], .arrow (bVec vn) (bVec vn)⟩
  | .rAnd | .rOr    => some ⟨[nN], .arrow (bVec vn) bool⟩
  | .rNAnd | .rNor | .rXOr | .rXNor | .msBit => redOp
  | .usingExtern    => none

namespace BuiltinSigs

/-- An application of the built-in nat arithmetic constructors
(doc/eidos.md §3.1) — deferred rather than decomposed. -/
private def isNatArith (ty : Ty) : Bool :=
  match Ty.flatten ty with
  | (.con c, _ :: _) => c == "+" || c == "-" || c == "*"
  | _                => false

private abbrev Bnds := HashMap TyVar Ty

/-- One-way first-order matching of a scheme subterm against a
(normalized) target subterm, accumulating bindings and deferred
nat-arithmetic equations. -/
private def matchGo (tvs : HashSet TyVar) (s tgt : Ty) (st : Bnds × List (Ty × Ty)) :
    Option (Bnds × List (Ty × Ty)) :=
  match s with
  | .var v =>
      if tvs.contains v then
        match st.1.get? v with
        | none    => some (st.1.insert v tgt, st.2)
        | some t₀ => if Ty.eq t₀ tgt then some st else none
      else none
  | _ =>
    if isNatArith s then some (st.1, (s, tgt) :: st.2)
    else
      match s, tgt with
      | .arrow s₁ s₂, .arrow t₁ t₂ => matchGo tvs s₁ t₁ st >>= matchGo tvs s₂ t₂
      | .app s₁ s₂,   .app t₁ t₂   => matchGo tvs s₁ t₁ st >>= matchGo tvs s₂ t₂
      | .con c,       .con c'      => if c == c' then some st else none
      | .nat n,       .nat n'      => if n == n' then some st else none
      | _,            _            => none

/-- A deferred equation holds unless both sides are nat-closed after
substitution and disagree. -/
private def checkDeferred (bnds : Bnds) (eqn : Ty × Ty) : Bool :=
  match Ty.evalNat (DEnv.substTv bnds eqn.1), Ty.evalNat eqn.2 with
  | some n, some n' => n == n'
  | _,      _       => true -- open on either side: unchecked.

end BuiltinSigs

/-- Does the occurrence type instantiate the signature scheme? -/
def matchesSig (sig : Sig) (t : Ty) : Bool :=
  match BuiltinSigs.matchGo (HashSet.ofList sig.tvs) sig.ty (Ty.natNorm t) (∅, []) with
  | none            => false
  | some (bnds, defs) => defs.all (BuiltinSigs.checkDeferred bnds)

namespace Check

/-! ## Fuel

Fuel bounds recursion *depth* (each fueled function consumes one unit
on entry and passes the remainder down), so exhaustion at these bounds
means structurally deeper input than any program the compiler can emit
— reported as a bug, as in Rwv.Hyle.Check. -/

def expFuel : Nat := 1000000

def fuelErr : String := "checkMachine: fuel exhausted (rwv bug?)"

/-! ## Environments (Lint.hs `Env`) -/

/-- A join point visible in the current scope: its declared label, its
parameter types, and its result type (the type of its scope). -/
structure JoinDef where
  j    : JoinId
  ptys : List Ty
  res  : Ty

structure Env where
  /-- The datatype environment (for representability via `sizeOf`). -/
  Δ     : DEnv
  /-- Data constructors, by name: datatype name and signature. -/
  cons  : HashMap String (String × Sig)
  /-- Term binders in scope, by unique. -/
  scope : HashMap Int Id
  /-- Signature type variables in scope (empty everywhere in machine
  mode except inside constructor-signature scoping). -/
  tvs   : HashMap Int TyVar
  /-- Join points lexically visible. -/
  joins : HashMap Int JoinDef
  /-- Joins jumpable from here (tail position of their scopes). -/
  tail  : HashSet Int
  /-- Names of skipped non-intrinsic (polymorphic/reactive carrier)
  definitions: their bodies are unchecked, so references from checked
  code are rejected (the module header's carrier rule). -/
  unchecked : HashSet Int

/-- A builtin-named signature carrier (the canonical-intrinsic class of
the skip rule; its body must be the canonical error stub). -/
def intrinsicDefn (d : Defn) : Bool :=
  (Parse.lookupBuiltin d.name.occ).isSome

def mkEnv (p : Program) : Env where
  Δ     := DEnv.ofDatas p.datas
  cons  := HashMap.ofList (p.datas.flatMap fun d => d.cons.map fun c => (c.name, (d.name, c.sig)))
  scope := HashMap.ofList (p.defns.map fun d => (d.name.uniq, d.name))
  tvs   := ∅
  joins := ∅
  tail  := ∅
  unchecked := HashSet.ofList
    ((p.defns.filter fun d =>
        !intrinsicDefn d && (!d.name.sig.tvs.isEmpty || d.name.sig.ty.reacOrStateT)).map
      (·.name.uniq))

def bindVar (x : Id) (env : Env) : Env :=
  { env with scope := env.scope.insert x.uniq x }

/-- Make a join point lexically visible without making it jumpable
(used for its own body: join points are not recursive). -/
def scopeJoin (j : JoinId) (jd : JoinDef) (env : Env) : Env :=
  { env with joins := env.joins.insert j.uniq jd }

/-- Make a join point visible and jumpable (used for its scope: the
let body, whose tail is the join's tail). -/
def bindJoin (j : JoinId) (jd : JoinDef) (env : Env) : Env :=
  { scopeJoin j jd env with tail := env.tail.insert j.uniq }

/-- Entering a non-tail position: no join is jumpable from here (they
stay visible, for diagnostics). -/
def nonTail (env : Env) : Env := { env with tail := ∅ }

/-! ## Distinctness -/

/-- Fold a list of keyed sites, reporting the first duplicate with
both descriptions. -/
def checkDistinct [BEq κ] [Hashable κ] (sites : List (κ × String)) : Except String Unit := do
  let mut seen : HashMap κ String := ∅
  for (k, what) in sites do
    match seen.get? k with
    | some what' => throw s!"duplicate {what} (first introduced: {what'})"
    | none       => seen := seen.insert k what

/-! ## Global binder uniqueness (doc/eidos.md §2, §4.4)

Every binding site of the P fragment, in deterministic order:
definition names, signature type variables, parameters, all local
binders, and datatype parameters. Occurrences (which share their
binder's unique) contribute nothing. Skipped (carrier) definitions
contribute their sites like any other: the datatype-and-definition
fragment is untouched by procify, and the pass-6 whole-program lint
guaranteed its global uniqueness.

Process binding sites are deliberately NOT part of the rule: procify
splices one definition per continuation and passes the same binder
along goto chains, so one unique is legitimately bound by several
blocks (and, in a legacy dump that still carries the consumed reactive
definitions as skipped residue, by those definitions too). Scoping
inside blocks is by innermost binding, which occurrence-signature
checking still validates per occurrence. Block *labels* are checked
distinct per-proc below (the label table must be well-defined). -/

abbrev Site := Int × String

private def idSite (what : String) (x : Id) : Site :=
  (x.uniq, s!"binding unique #{x.uniq} ({what} {x.occ}#{x.uniq})")

private def tvSite (what : String) (v : TyVar) : Site :=
  (v.uniq, s!"binding unique #{v.uniq} ({what} {v.occ}#{v.uniq})")

mutual

def expSites (fuel : Nat) (acc : Array Site) (e : Exp) : Except String (Array Site) :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match e with
    | .var _ | .con .. | .prim .. | .litInt .. | .litStr _ => pure acc
    | .litList _ es => es.foldlM (expSites fuel) acc
    | .litVec _ es  => es.foldlM (expSites fuel) acc
    | .app e a      => do argSites fuel (← expSites fuel acc e) a
    | .lam x e      => expSites fuel (acc.push (idSite "lambda parameter" x)) e
    | .letE b e     => do expSites fuel (← bindSites fuel acc b) e
    | .jump _ es    => es.foldlM (expSites fuel) acc
    | .cases _ e x alts => do
        let acc ← expSites fuel acc e
        alts.foldlM (altSites fuel) (acc.push (idSite "case binder" x))
termination_by fuel

def argSites (fuel : Nat) (acc : Array Site) (a : Arg) : Except String (Array Site) :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match a with
    | .eArg e => expSites fuel acc e
    | .tArg _ => pure acc
termination_by fuel

def bindSites (fuel : Nat) (acc : Array Site) (b : Bind) : Except String (Array Site) :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match b with
    | .nonRec x e => expSites fuel (acc.push (idSite "let binder" x)) e
    | .recB bs    =>
        bs.foldlM (fun acc (x, e) =>
          expSites fuel (acc.push (idSite "recursive let binder" x)) e) acc
    | .join j xs e =>
        let acc := acc.push (j.uniq, s!"binding unique #{j.uniq} (join point {j.occ}#{j.uniq})")
        expSites fuel (xs.foldl (fun acc x => acc.push (idSite "join point parameter" x)) acc) e
termination_by fuel

def altSites (fuel : Nat) (acc : Array Site) (a : Alt) : Except String (Array Site) :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match a with
    | .mk _ xs e => expSites fuel (xs.foldl (fun acc x => acc.push (idSite "pattern binder" x)) acc) e
termination_by fuel

end

def uniqSites (p : Program) : Except String (List Site) := do
  let mut acc : Array Site := #[]
  -- Constructors of one datatype share the datatype's parameter
  -- uniques (checkDataDefn enforces that their quantifier lists
  -- coincide), so only the first constructor's list contributes
  -- binding sites.
  for d in p.datas do
    match d.cons with
    | c :: _ => acc := c.sig.tvs.foldl (fun acc v => acc.push (tvSite "datatype parameter" v)) acc
    | []     => pure ()
  for d in p.defns do
    acc := acc.push (idSite "definition name" d.name)
    acc := d.name.sig.tvs.foldl (fun acc v => acc.push (tvSite "signature type variable" v)) acc
    acc := d.params.foldl (fun acc x => acc.push (idSite "parameter" x)) acc
    acc ← expSites expFuel acc d.body
  pure acc.toList

/-! ## Types (Lint.hs checkTy/checkTyScope/checkClosed) -/

/-- Scoping: every type variable bound, at its binder's kind. -/
def checkTyScope (env : Env) : Ty → Except String Unit
  | .var v =>
      match env.tvs.get? v.uniq with
      | some v' =>
          if v'.kind == v.kind then pure ()
          else throw s!"type variable {v.occ}#{v.uniq}: occurrence kind does not match its binder's"
      | none => throw s!"unbound type variable: {v.occ}#{v.uniq}"
  | .app t₁ t₂   => do checkTyScope env t₁; checkTyScope env t₂
  | .arrow t₁ t₂ => do checkTyScope env t₁; checkTyScope env t₂
  | _            => pure ()

/-- On a `natNorm`-normalized type: no type variables, and no residual
type-level arithmetic (every nat-closed subterm has already been
folded to a literal, so any surviving arithmetic constructor is
open). -/
def checkClosed : Ty → Except String Unit
  | .var v =>
      throw s!"type variable {v.occ}#{v.uniq} in mono mode (types must be closed)"
  | .con c =>
      if c == "+" || c == "-" || c == "*" then
        throw "type-level arithmetic does not evaluate to a literal (types must be nat-closed in mono mode)"
      else pure ()
  | .app t₁ t₂   => do checkClosed t₁; checkClosed t₂
  | .arrow t₁ t₂ => do checkClosed t₁; checkClosed t₂
  | _            => pure ()

/-- Scoping plus closedness plus the machine-mode type-grammar rule:
the reactive types are out of the grammar entirely (doc/eidos.md
§4.1). -/
def checkTy (env : Env) (t : Ty) : Except String Unit := do
  checkTyScope env t
  checkClosed (Ty.natNorm t)
  if t.reacOrStateT then
    throw s!"reactive type {t.render} in Synolon (procification has retired ReacT/StateT/Identity)"

/-! ## Occurrences and binders -/

/-- An occurrence's signature equals its binder's: the same quantified
variables (uniques and kinds) over `natNorm`-structurally equal
types. -/
def checkOccSig (occ bnd : Id) : Except String Unit := do
  unless occ.sig.tvs == bnd.sig.tvs
      && occ.sig.tvs.map (·.kind) == bnd.sig.tvs.map (·.kind)
      && Ty.eq occ.sig.ty bnd.sig.ty do
    throw s!"occurrence of {occ.render} carries signature {occ.sig.render} but its binder carries {bnd.sig.render}"

/-- A `Var` occurrence: bound in scope, with the binder's signature
(§4.4). Returns the binder. -/
def lookupVar (env : Env) (x : Id) : Except String Id :=
  match env.scope.get? x.uniq with
  | some xB => do
      if env.unchecked.contains x.uniq then
        throw s!"reference to {x.render}, a polymorphic or reactive carrier definition whose body is outside the checked machine fragment"
      checkOccSig x xB
      pure xB
  | none =>
      if env.joins.contains x.uniq then
        throw s!"join point {x.render} used as a value (labels may only be jump targets)"
      else throw s!"unbound variable: {x.render}"

/-- Rules common to every local binder: a monomorphic signature (§3.2)
over a well-scoped, closed, non-reactive type. -/
def checkLocalBinder (env : Env) (what : String) (x : Id) : Except String Unit := do
  unless x.sig.tvs.isEmpty do
    throw s!"{what} {x.render} has a polymorphic signature (local binders are monomorphic)"
  checkTy env x.sig.ty

/-- Fuel for `DEnv.sizeOf` (bounds datatype-unfolding depth; the
visited set is the semantic bound). -/
def szFuel : Nat := 100000

/-- A local *value* binder (parameter, lambda/let/case/pattern/command
binder, block parameter): additionally first-order (mono+ANF rule) and
representable at a fixed bit width (§7.4; `DEnv.sizeOf` is the
translation's own sizing). Join point labels are exempt — a label's
signature is its continuation's function type, and a label is not a
value. -/
def checkValueBinder (env : Env) (what : String) (x : Id) : Except String Unit := do
  checkLocalBinder env what x
  if x.sig.ty.hasArrowDeep then
    throw s!"{what} {x.render} has a function type (higher-order binders are not representable past the ANF stage)"
  match env.Δ.sizeOf szFuel [] x.sig.ty with
  | .ok _    => pure ()
  | .error e => throw s!"{what} {x.render} is not representable at a fixed bit width ({e})"

/-! ## Constructors (Lint.hs lookupCon/dconFieldTys/checkCon) -/

def lookupCon (env : Env) (c : String) : Except String (String × Sig) :=
  match env.cons.get? c with
  | some r => pure r
  | none   => throw s!"unknown data constructor: {c}"

/-- The instantiated field types of a constructor at a fully-applied
datatype type `T ts` (the scrutinee's type, or a `Con` occurrence's
result type). -/
def dconFieldTys (c tcon : String) (sig : Sig) (t : Ty) : Except String (List Ty) :=
  match Ty.flatten (Ty.natNorm t) with
  | (.con t', args) =>
      if t' == tcon then
        if args.length == sig.tvs.length then
          let sub : HashMap TyVar Ty := HashMap.ofList (sig.tvs.zip args)
          pure ((Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub))
        else throw s!"constructor {c}: datatype {tcon} applied to {args.length} arguments (expected {sig.tvs.length})"
      else throw s!"constructor {c} of datatype {tcon} used at incompatible type {t.render}"
  | _ => throw s!"constructor {c} of datatype {tcon} used at incompatible type {t.render}"

/-- A `Con` occurrence's carried type instantiates its signature: the
instantiation is read off the carried type's result by first-order
matching against `T as` (§3.6), then the whole type must agree. -/
def checkCon (env : Env) (t : Ty) (c : String) : Except String Unit := do
  let (tcon, sig) ← lookupCon env c
  let res := (Ty.flattenArrow t).2
  let fields ← dconFieldTys c tcon sig res
  unless Ty.eq t (fields.foldr .arrow res) do
    throw s!"constructor {c} at type {t.render} does not instantiate its signature {sig.render}"

/-! ## Integer literals (doc/eidos.md §4.2): representability -/

inductive LitRep where
  | integer
  | bits (w : Nat)
  | finiteR (w : Nat)
  | openR
  | bad

def litRep (t : Ty) : LitRep :=
  match Ty.flatten (Ty.natNorm t) with
  | (.con "Integer", [])           => .integer
  | (.con "Vec", [w, .con "Bool"]) =>
      match Ty.evalNat w with
      | some k => .bits k
      | none   => .openR
  | (.con "Finite", [w]) =>
      match Ty.evalNat w with
      | some k => .finiteR k
      | none   => .openR
  | _ => .bad

def fitsRep (rep : LitRep) (n : Int) : Bool :=
  match rep with
  | .integer   => true
  | .openR     => true -- unreachable here (types are nat-closed in machine mode).
  | .bits w    =>
      if n ≥ 0 then n < (2 : Int) ^ w
      else w > 0 && n ≥ -((2 : Int) ^ (w - 1))
  | .finiteR w => 0 ≤ n && n < (w : Int)
  | .bad       => false

def checkLitInt (t : Ty) (n : Int) : Except String Unit :=
  match litRep t with
  | .bad =>
      throw s!"integer literal at unrepresentable type {t.render} (must be Integer, a bit vector, or Finite)"
  | rep =>
      unless fitsRep rep n do
        throw s!"literal {n} is not representable at type {t.render}"

/-- The list type constructor applied to an element type (both the
bridge's spelling `[_]` and the source spelling `[]`). -/
def dstListTy : Ty → Option Ty
  | .app (.con c) et => if c == "[_]" || c == "[]" then some et else none
  | _                => none

def isDefaultAlt : AltCon → Bool
  | .default => true
  | _        => false

/-! ## Expressions (doc/eidos.md §4.2): synthesis with all checks
inline — the located, checking twin of `Rwv.Eidos.typeOf`
(transcribing Lint.hs checkExp and friends). -/

mutual

/-- Check an expression and return its type. -/
def checkExp (env : Env) (fuel : Nat) (e : Exp) : Except String Ty :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match e with
    | .app ..   => checkSpine env fuel e
    | .var x    => do
        let xB ← lookupVar env x
        unless xB.sig.tvs.isEmpty do
          throw s!"unsaturated reference to polymorphic {x.render} (type arguments must saturate the quantifier list)"
        pure xB.sig.ty
    | .con t c  => do
        checkTy env t
        checkCon env t c
        pure t
    | .prim t b => do
        checkTy env t
        match builtinSig b with
        | some sig =>
            unless matchesSig sig t do
              throw s!"primitive {b.name} at a type that does not instantiate its signature (expected an instance of: {sig.render}; but the occurrence has: {t.render})"
        | none => pure ()
        pure t
    | .litInt t n => do
        checkTy env t
        checkLitInt t n
        pure t
    | .litStr _ => pure (.con "String")
    | .litList t es => do
        checkTy env t
        match dstListTy t with
        | some et => es.forM fun e => checkAgainst (nonTail env) fuel e et
        | none    => throw s!"list literal at a non-list type: {t.render}"
        pure t
    | .litVec t es => do
        checkTy env t
        match Ty.flatten (Ty.natNorm t) with
        | (.con "Vec", [n, et]) => do
            es.forM fun e => checkAgainst (nonTail env) fuel e et
            match Ty.evalNat n with
            | some k =>
                unless k == es.length do
                  throw s!"vector literal has {es.length} elements but type {t.render}"
            | none => pure ()
        | _ => throw s!"vector literal at a non-Vec type: {t.render}"
        pure t
    | .lam x e => do
        checkValueBinder env "lambda parameter" x
        let te ← checkExp (bindVar x (nonTail env)) fuel e
        pure (.arrow x.sig.ty te)
    | .letE b e   => checkLet env fuel b e
    | .jump j es  => checkJump env fuel j es
    | .cases t e x alts => checkCase env fuel t e x alts
termination_by fuel

/-- Check an expression against an expected type: synthesize and
compare after `natNorm`. -/
def checkAgainst (env : Env) (fuel : Nat) (e : Exp) (t : Ty) : Except String Unit :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 => do
    let t' ← checkExp env fuel e
    unless Ty.eq t t' do
      throw s!"expression has type {t'.render} but {t.render} is expected"
termination_by fuel

/-- The spine discipline (§4.2): type arguments only on `Var` heads,
in prefix position, saturating the head's quantifier list; then one
arrow peeled per term argument, each argument checking against its
domain. -/
def checkSpine (env : Env) (fuel : Nat) (e : Exp) : Except String Ty :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 => do
    let (h, args) := Exp.flattenApp e
    let (tas, eas) := args.span Arg.isTArg
    let tys := tas.filterMap fun | .tArg t => some t | _ => none
    if eas.any Arg.isTArg then
      throw "type arguments must form a prefix of the application spine"
    tys.forM (checkTy env)
    let ht ←
      match h, tys with
      | .var x, _ :: _ => do
          let xB ← lookupVar env x
          unless xB.sig.tvs.length == tys.length do
            throw s!"{x.render} expects {xB.sig.tvs.length} type arguments, applied to {tys.length}"
          pure (xB.sig.instantiate tys)
      | _, _ :: _ => throw "type argument applied to a non-variable head"
      | _, []     => checkExp (nonTail env) fuel h
    checkSpineApply env fuel ht eas
termination_by fuel

/-- Apply term arguments to a (synthesized) head type, one arrow
each. -/
def checkSpineApply (env : Env) (fuel : Nat) (t : Ty) (args : List Arg) : Except String Ty :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match args with
    | [] => pure t
    | .tArg _ :: _ => throw "type arguments must form a prefix of the application spine"
    | .eArg a :: rest => do
        let ta ← checkExp (nonTail env) fuel a
        match t with
        | .arrow dom cod => do
            unless Ty.eq dom ta do
              throw s!"argument has type {ta.render} but the function expects {dom.render}"
            checkSpineApply env fuel cod rest
        | _ => throw s!"term argument applied to a non-arrow (head type {t.render})"
termination_by fuel

/-- Bindings and the join point discipline (doc/eidos.md §3.4,
§4.2). -/
def checkLet (env : Env) (fuel : Nat) (b : Bind) (body : Exp) : Except String Ty :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match b with
    | .nonRec x e => do
        checkValueBinder env "let binder" x
        checkAgainst (nonTail env) fuel e x.sig.ty
        checkExp (bindVar x env) fuel body
    | .recB bs => do
        bs.forM fun (x, _) => checkValueBinder env "recursive let binder" x
        let env' := bs.foldr (fun (x, _) acc => bindVar x acc) env
        bs.forM fun (x, e) => checkAgainst (nonTail env') fuel e x.sig.ty
        checkExp env' fuel body
    | .join j xs e => do
        -- The label carries no signature in this embedding (see the
        -- module header): arity vs parameters, then the result type is
        -- reconstructed from the body and compared against the scope.
        unless xs.length == j.arity do
          throw s!"join point {j.occ}#{j.uniq} declares arity {j.arity} but binds {xs.length} parameters"
        xs.forM (checkValueBinder env "join point parameter")
        let ptys := xs.map (·.sig.ty)
        -- The join body and the scope check against the same type (the
        -- join's result is the scope's result). Outer joins remain
        -- jumpable from the body's tail (it is transitively a tail of
        -- their scopes); the join itself is not (no recursion) — its
        -- placeholder result type below is never consulted, because a
        -- self-jump fails the tail rule first.
        let envBody := xs.foldr bindVar (scopeJoin j ⟨j, ptys, .con "()"⟩ env)
        let resTy ← checkExp envBody fuel e
        let tb ← checkExp (bindJoin j ⟨j, ptys, resTy⟩ env) fuel body
        unless Ty.eq tb resTy do
          throw s!"the scope of join point {j.occ}#{j.uniq} has type {tb.render} but the join point returns {resTy.render}"
        pure tb
termination_by fuel

/-- A jump: to a join point bound in an enclosing let, from tail
position of that join's scope, saturating its arity, each argument
checking against the corresponding parameter type. -/
def checkJump (env : Env) (fuel : Nat) (j : JoinId) (args : List Exp) : Except String Ty :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match env.joins.get? j.uniq with
    | some jd => do
        unless env.tail.contains j.uniq do
          throw s!"jump to join point {j.occ}#{j.uniq} outside the tail of its scope (jumps are tail-only, and join points are not recursive)"
        unless j.arity == jd.j.arity do
          throw s!"jump to {j.occ}#{j.uniq} carries arity {j.arity} but the join point declares {jd.j.arity}"
        unless args.length == jd.j.arity do
          throw s!"jump to {j.occ}#{j.uniq} supplies {args.length} arguments (arity {jd.j.arity})"
        (args.zip jd.ptys).forM fun (a, t) => checkAgainst (nonTail env) fuel a t
        pure jd.res
    | none =>
        if env.scope.contains j.uniq then
          throw s!"jump target {j.occ}#{j.uniq} is a value binder, not a join point"
        else
          throw s!"jump to unbound join point {j.occ}#{j.uniq} (labels do not escape their scope)"
termination_by fuel

/-- Case expressions (doc/eidos.md §4.2): non-empty alternatives, the
default (if present) first, disjoint constructor and literal
alternatives, the case binder at the scrutinee's type. -/
def checkCase (env : Env) (fuel : Nat) (t : Ty) (scrut : Exp) (x : Id) (alts : List Alt) :
    Except String Ty :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 => do
    checkTy env t
    let ts ← checkExp (nonTail env) fuel scrut
    checkValueBinder env "case binder" x
    unless Ty.eq x.sig.ty ts do
      throw s!"case binder {x.render} has type {x.sig.ty.render} but the scrutinee has type {ts.render}"
    if alts.isEmpty then
      throw "case expression with no alternatives"
    if (alts.drop 1).any (fun (.mk c _ _) => isDefaultAlt c) then
      throw "the default case alternative must come first"
    checkDistinct (alts.filterMap fun (.mk c _ _) =>
      match c with
      | .dataAlt c' => some (c', s!"case alternative for constructor {c'}")
      | _           => none)
    checkDistinct (alts.filterMap fun (.mk c _ _) =>
      match c with
      | .litAlt n => some (n, s!"case alternative for literal {n}")
      | _         => none)
    alts.forM (checkAlt (bindVar x env) fuel t ts)
    pure t
termination_by fuel

/-- One alternative: fields bound at the constructor's instantiated
field types; the body (a tail position) checks against the carried
result type. -/
def checkAlt (env : Env) (fuel : Nat) (t ts : Ty) (alt : Alt) : Except String Unit :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match alt with
    | .mk .default xs body => do
        unless xs.isEmpty do throw "default case alternative binds fields"
        checkAgainst env fuel body t
    | .mk (.litAlt n) xs body => do
        unless xs.isEmpty do throw "literal case alternative binds fields"
        match litRep ts with
        | .bad =>
            throw s!"literal case alternative on a scrutinee of type {ts.render} (must be Integer, a bit vector, or Finite)"
        | rep =>
            unless fitsRep rep n do
              throw s!"literal {n} is not representable at the scrutinee type {ts.render}"
        checkAgainst env fuel body t
    | .mk (.dataAlt c) xs body => do
        let (tcon, sig) ← lookupCon env c
        let fields ← dconFieldTys c tcon sig ts
        unless xs.length == fields.length do
          throw s!"case alternative for {c} binds {xs.length} fields (the constructor has {fields.length})"
        xs.forM (checkValueBinder env "pattern binder")
        (xs.zip fields).forM fun (p, ft) =>
          unless Ty.eq p.sig.ty ft do
            throw s!"pattern binder {p.render}: type does not match the constructor's field type {ft.render}"
        checkAgainst (xs.foldr bindVar env) fuel body t
termination_by fuel

end

/-! ## Datatypes (doc/eidos.md §3.6, §4.3) -/

def kindSpine : Kind → List Kind × Kind
  | .fn k₁ k₂ => let (ks, r) := kindSpine k₂; (k₁ :: ks, r)
  | k         => ([], k)

/-- The datatype's kind constructs `*`; every constructor quantifies
exactly the datatype's parameters (the same type variables, in the
same order, across all constructors) and constructs exactly the
datatype applied to them. Datatypes stay parametric through
specialization, so constructor signatures see only the scoping rules
(no closedness). -/
def checkDataDefn (env : Env) (d : DataDefn) : Except String Unit := do
  let (doms, kres) := kindSpine d.kind
  unless kres == Kind.star do
    throw s!"datatype {d.name}: kind must construct *"
  match d.cons with
  | []      => pure ()
  | c₀ :: _ => d.cons.forM (checkCtor doms c₀.sig.tvs)
where
  checkCtor (doms : List Kind) (tvs₀ : List TyVar) (c : DataCon) : Except String Unit := do
    unless c.sig.tvs == tvs₀ do
      throw s!"constructor {c.name}: quantified type variables differ across constructors of {d.name}"
    unless c.sig.tvs.length == doms.length do
      throw s!"constructor {c.name}: quantifies {c.sig.tvs.length} type variables but the kind of {d.name} has {doms.length} parameters"
    (c.sig.tvs.zip doms).forM fun (v, kd) =>
      unless v.kind == kd do
        throw s!"constructor {c.name}: the kind of type variable {v.occ}#{v.uniq} does not match the corresponding parameter of the kind of {d.name}"
    checkTyScope { env with tvs := HashMap.ofList (c.sig.tvs.map fun v => (v.uniq, v)) } c.sig.ty
    match Ty.flatten (Ty.flattenArrow c.sig.ty).2 with
    | (.con t', args) =>
        unless t' == d.name && args == c.sig.tvs.map Ty.var do
          throw s!"constructor {c.name} must construct {d.name} applied to exactly its quantified type variables"
    | _ => throw s!"constructor {c.name} must construct {d.name} applied to exactly its quantified type variables"

/-! ## Definitions (doc/eidos.md §3.5, §4.3) -/

/-- The carrier-definition skip predicate (module header): builtin-
named signature carriers (exact builtin-name match, as the Haskell's
`primNames` membership), polymorphic signatures, and reactive types —
the definitions the fold's `emit` filters out of the lowered
fragment. Skipping a body does not mean trusting it: intrinsic
carriers must be the canonical error stub (`checkIntrinsicStub`), and
references from checked code to the other skipped definitions are
rejected (`lookupVar`, `enterPure`). -/
def skipDefn (d : Defn) : Bool :=
  intrinsicDefn d
    || !d.name.sig.tvs.isEmpty
    || d.name.sig.ty.reacOrStateT

/-- A builtin-named carrier is exactly the shape the bridge emits: an
application spine headed by the `error` builtin (the placeholder the
fold never lowers). Anything else under a builtin name is a
declaration masquerading as an intrinsic, not residue. -/
def checkIntrinsicStub (d : Defn) : Except String Unit :=
  match (Exp.flattenApp d.body).1 with
  | .prim _ .error => pure ()
  | _ => throw s!"builtin-named definition {d.name.render} is not the canonical error stub (its body would be trusted without being checked)"

/-- Parameters match a prefix of the signature's arrow spine; the body
checks against the remainder. (Non-skipped definitions are
monomorphic by the skip predicate, so the mono-mode signature rule
holds by construction.) -/
def checkDefn (env : Env) (d : Defn) : Except String Unit := do
  checkTy env d.name.sig.ty
  let (doms, res) := Ty.flattenArrow d.name.sig.ty
  if d.params.length > doms.length then
    throw s!"definition {d.name.render} has more parameters than its signature has arrows"
  (d.params.zip doms).forM fun (p, dom) => do
    checkValueBinder env "parameter" p
    unless Ty.eq p.sig.ty dom do
      throw s!"parameter {p.render} of {d.name.render}: type does not match the signature's arrow prefix (expected {dom.render})"
  let rest := (doms.drop d.params.length).foldr Ty.arrow res
  checkAgainst (d.params.foldr bindVar env) expFuel d.body rest

/-- When a program carries a `top` (a legacy Eidos machine-level dump
or hand-written input; a Synolon program has none): it resolves to a
definition, its occurrence signature matches the binder's, and it has
the device-root type `ReacT i o Identity a` — the reactive machine-root
definition procify consumed, retained as skipped residue (module
header). -/
def checkTop (defns : List Defn) (top : Id) : Except String Unit := do
  match defns.find? (fun d => d.name.uniq == top.uniq) with
  | none => throw s!"top: designated device root {top.render} does not name a definition"
  | some d => do
      checkOccSig top d.name
      match Ty.flatten (Ty.natNorm d.name.sig.ty) with
      | (.con "ReacT", [_, _, .con "Identity", _]) => pure ()
      | _ => throw s!"the top definition must have type ReacT i o Identity a, not {d.name.sig.ty.render}"

/-! ## Processes (doc/eidos.md §7.4): the machine rules, per-proc
(Lint.hs checkProc) -/

/-- Fuel for terminator-tree recursion (terminator-case nesting
depth). -/
def termFuel : Nat := 100000

/-- The per-process context: label and cell tables, the process name,
and the input/output types. -/
structure PEnv where
  name  : String
  inTy  : Ty
  outTy : Ty
  ltab  : HashMap Int (Id × Block)
  ctab  : HashMap String Ty

def PEnv.ofProc (pr : Proc) : PEnv where
  name  := pr.name
  inTy  := pr.inTy
  outTy := pr.outTy
  ltab  := HashMap.ofList (pr.blocks.map fun (l, b) => (l.uniq, (l, b)))
  ctab  := HashMap.ofList (pr.cells.map fun c => (c.name, c.ty))

def cellTy (P : PEnv) (s : String) : Except String Ty :=
  match P.ctab.get? s with
  | some t => pure t
  | none   => throw s!"unknown state cell: {s} (process {P.name})"

def target (P : PEnv) (l : Id) : Except String (Id × Block) :=
  match P.ltab.get? l.uniq with
  | some lb => do checkOccSig l lb.1; pure lb
  | none    => throw s!"terminator targets an undeclared block label: {l.render}"

/-- Cell initials are closed (checked in the top-level-only
environment: no locals are in scope) and cell-typed; the cell's type
is representable (§7.4). -/
def checkCell (env : Env) (procName : String) (c : Cell) : Except String Unit := do
  checkTy env c.ty
  match env.Δ.sizeOf szFuel [] c.ty with
  | .ok _    => pure ()
  | .error e =>
      throw s!"state cell {c.name} of process {procName} is not representable at a fixed bit width ({e})"
  match c.init with
  | none   => pure ()
  | some e => do
      let t' ← checkExp (nonTail env) expFuel e
      unless Ty.eq c.ty t' do
        throw s!"the initial value of state cell {c.name} has type {t'.render}, not the cell's type {c.ty.render}"

def checkCmd (P : PEnv) (env : Env) (c : Cmd) : Except String Env :=
  match c with
  | .bind x rhs => do
      checkValueBinder env "command binder" x
      checkAgainst (nonTail env) expFuel rhs x.sig.ty
      pure (bindVar x env)
  | .get x s => do
      checkValueBinder env "command binder" x
      let t ← cellTy P s
      unless Ty.eq x.sig.ty t do
        throw s!"get: binder {x.render} has type {x.sig.ty.render}, not the type of state cell {s} ({t.render})"
      pure (bindVar x env)
  | .put s a => do
      let t ← cellTy P s
      checkAgainst (nonTail env) expFuel a t
      pure env

mutual

def checkTerm (env : Env) (P : PEnv) (fuel : Nat) (t : Term) : Except String Unit :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match t with
    | .pause out l args => do
        checkAgainst (nonTail env) expFuel out P.outTy
        let (lB, b) ← target P l
        if b.params.isEmpty then
          throw s!"pause target {lB.render} has no parameters (the last is the resumed input)"
        unless args.length == b.params.length - 1 do
          throw s!"pause to {lB.render} supplies {args.length} arguments (its target takes {b.params.length - 1} plus the resumed input)"
        (args.zip b.params).forM fun (a, p) => checkAgainst (nonTail env) expFuel a p.sig.ty
    | .goto l args => do
        let (lB, b) ← target P l
        unless args.length == b.params.length do
          throw s!"goto {lB.render} supplies {args.length} arguments (its target takes {b.params.length})"
        (args.zip b.params).forM fun (a, p) => checkAgainst (nonTail env) expFuel a p.sig.ty
    | .halt a => do
        -- The halt answer becomes a slice of the machine-step record
        -- (tagged per distinct answer type), so it needs a fixed
        -- width; its type is otherwise unconstrained.
        let ta ← checkExp (nonTail env) expFuel a
        match env.Δ.sizeOf szFuel [] ta with
        | .ok _    => pure ()
        | .error e =>
            throw s!"a halt answer of process {P.name} is not representable at a fixed bit width ({e})"
    | .cases a alts => do
        let ts ← checkExp (nonTail env) expFuel a
        if (alts.drop 1).any (fun (.mk c _ _) => isDefaultAlt c) then
          throw "the default terminator alternative must come first"
        checkDistinct (alts.filterMap fun (.mk c _ _) =>
          match c with
          | .dataAlt c' => some (c', s!"terminator alternative for constructor {c'}")
          | _           => none)
        checkDistinct (alts.filterMap fun (.mk c _ _) =>
          match c with
          | .litAlt n => some (n, s!"terminator alternative for literal {n}")
          | _         => none)
        if alts.isEmpty then
          throw "terminator case with no alternatives"
        alts.forM (checkTAlt env P fuel ts)
termination_by fuel

def checkTAlt (env : Env) (P : PEnv) (fuel : Nat) (ts : Ty) (a : TAlt) : Except String Unit :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match a with
    | .mk .default xs t => do
        unless xs.isEmpty do throw "default terminator alternative binds fields"
        checkTerm env P fuel t
    | .mk (.litAlt n) xs t => do
        unless xs.isEmpty do throw "literal terminator alternative binds fields"
        match litRep ts with
        | .bad => throw s!"literal terminator alternative on a scrutinee of type {ts.render}"
        | rep =>
            unless fitsRep rep n do
              throw s!"literal {n} is not representable at the scrutinee type {ts.render}"
        checkTerm env P fuel t
    | .mk (.dataAlt c) xs t => do
        let (tcon, sig) ← lookupCon env c
        let fields ← dconFieldTys c tcon sig ts
        unless xs.length == fields.length do
          throw s!"terminator alternative for {c} binds {xs.length} fields (the constructor has {fields.length})"
        xs.forM (checkValueBinder env "pattern binder")
        (xs.zip fields).forM fun (p, ft) =>
          unless Ty.eq p.sig.ty ft do
            throw s!"pattern binder {p.render}: type does not match the constructor's field type {ft.render}"
        checkTerm (xs.foldr bindVar env) P fuel t
termination_by fuel

end

def checkBlock (env : Env) (P : PEnv) (b : Block) : Except String Unit := do
  b.params.forM (checkValueBinder env "block parameter")
  let env' ← b.cmds.foldlM (checkCmd P) (b.params.foldr bindVar env)
  checkTerm env' P termFuel b.term

/-! ### Terminator-tree collectors (fueled, like the checks) -/

mutual

def termHasPause (fuel : Nat) (t : Term) : Except String Bool :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match t with
    | .pause ..     => pure true
    | .cases _ alts => alts.anyM fun (.mk _ _ t') => termHasPause fuel t'
    | _             => pure false
termination_by fuel

end

mutual

def termGotos (fuel : Nat) (acc : Array Id) (t : Term) : Except String (Array Id) :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match t with
    | .goto l _     => pure (acc.push l)
    | .cases _ alts => alts.foldlM (fun acc (.mk _ _ t') => termGotos fuel acc t') acc
    | _             => pure acc
termination_by fuel

end

mutual

def termPauseTargets (fuel : Nat) (acc : Array Int) (t : Term) : Except String (Array Int) :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match t with
    | .pause _ l _  => pure (acc.push l.uniq)
    | .cases _ alts => alts.foldlM (fun acc (.mk _ _ t') => termPauseTargets fuel acc t') acc
    | _             => pure acc
termination_by fuel

end

/-- Pause targets: their last parameter is the resumed input, typed by
the process input type. -/
def checkInput (P : PEnv) (pauseTgts : HashSet Int) (l : Id) (b : Block) :
    Except String Unit := do
  if pauseTgts.contains l.uniq then
    match b.params.getLast? with
    | some p =>
        unless Ty.eq p.sig.ty P.inTy do
          throw s!"the last parameter of pause target {l.render} (the resumed input) has type {p.sig.ty.render}, not the process input type {P.inTy.render}"
    | none => pure ()

/-- The DFS of the signal-guardedness rule: follow goto edges, failing
on a repeated node. Fuel bounds the stack depth, which acyclicity (or
the failure) caps at the block count. -/
def visitGoto (edges : HashMap Int (List Int)) (msg : String) (fuel : Nat)
    (stack : HashSet Int) (u : Int) : Except String Unit :=
  match fuel with
  | 0 => throw "goto subgraph deeper than the block count (rwv bug?)"
  | fuel + 1 =>
    if stack.contains u then throw msg
    else (edges.getD u []).forM (visitGoto edges msg fuel (stack.insert u))
termination_by fuel

/-- Signal-guardedness (§7.4): the goto-only subgraph of the block
graph is acyclic — every cycle crosses a pause. -/
def checkGuarded (pr : Proc) : Except String Unit := do
  let entryGotos ← termGotos termFuel #[] pr.entry.term
  let edges : HashMap Int (List Int) ← pr.blocks.foldlM (init := ∅) fun m (l, b) => do
    pure (m.insert l.uniq ((← termGotos termFuel #[] b.term).toList.map (·.uniq)))
  let msg := s!"process {pr.name}: a cycle of gotos crosses no pause (is recursion guarded by signal?)"
  let fuel := pr.blocks.length + 2
  for l in entryGotos do
    visitGoto edges msg fuel ∅ l.uniq
  for (l, _) in pr.blocks do
    visitGoto edges msg fuel ∅ l.uniq

def checkProc (env : Env) (pr : Proc) : Except String Unit := do
  checkTy env pr.inTy
  checkTy env pr.outTy
  -- The ports are the machine's layout boundary: fixed widths, like
  -- cells and block parameters (§7.4).
  match env.Δ.sizeOf szFuel [] pr.inTy with
  | .ok _    => pure ()
  | .error e =>
      throw s!"the input type of process {pr.name} is not representable at a fixed bit width ({e})"
  match env.Δ.sizeOf szFuel [] pr.outTy with
  | .ok _    => pure ()
  | .error e =>
      throw s!"the output type of process {pr.name} is not representable at a fixed bit width ({e})"
  -- Label distinctness (an addition over Lint.hs checkProc, whose
  -- whole-program mode covered it via uniqSites): the per-proc label
  -- table must be well-defined for target resolution and the fold.
  checkDistinct (pr.blocks.map fun (l, _) =>
    (l.uniq, s!"block label {l.render} of process {pr.name}"))
  checkDistinct (pr.cells.map fun c => (c.name, s!"state cell {c.name} of process {pr.name}"))
  pr.cells.forM (checkCell env pr.name)
  let P := PEnv.ofProc pr
  checkBlock env P pr.entry
  pr.blocks.forM fun (_, b) => checkBlock env P b
  let pauseTgts ← (pr.entry :: pr.blocks.map (·.2)).foldlM
    (fun acc b => termPauseTargets termFuel acc b.term) #[]
  let pauseSet : HashSet Int := HashSet.ofArray pauseTgts
  pr.blocks.forM fun (l, b) => checkInput P pauseSet l b
  let anyPause ← (pr.entry :: pr.blocks.map (·.2)).anyM
    fun b => termHasPause termFuel b.term
  unless anyPause do
    throw s!"process {pr.name} never pauses (no machine to generate)"
  checkGuarded pr

/-! ## Pure-acyclicity (doc/eidos.md §7.4, normative; not yet enforced
by the Haskell linter — downstream today via the Hyle checker's
recursion rule): the call graph of the pure definitions reachable from
a process is acyclic, and no `rec` binding is reachable. A fueled DFS
in the style of Rwv.Hyle.Check.checkRecursion. Intrinsic carriers are
leaves (their bodies are the validated error stubs, edge-free); a
reference into any other skipped definition is an error here as it is
at `lookupVar` — recursion must not hide inside an unchecked body. -/

mutual

/-- Collect the definition-reference candidates (all `Var` uniques) of
an expression, rejecting `rec` bindings (reachable local recursion
breaks the same well-foundedness the rule protects). -/
def expRefs (fuel : Nat) (acc : Array Int) (e : Exp) : Except String (Array Int) :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match e with
    | .var x        => pure (acc.push x.uniq)
    | .con .. | .prim .. | .litInt .. | .litStr _ => pure acc
    | .litList _ es => es.foldlM (expRefs fuel) acc
    | .litVec _ es  => es.foldlM (expRefs fuel) acc
    | .app e a      => do
        let acc ← expRefs fuel acc e
        match a with
        | .eArg a' => expRefs fuel acc a'
        | .tArg _  => pure acc
    | .lam _ e      => expRefs fuel acc e
    | .letE b e     => do
        let acc ← match b with
          | .nonRec _ rhs => expRefs fuel acc rhs
          | .recB _       =>
              throw "a rec binding is reachable from a process (the pure call graph must be acyclic, doc/eidos.md §7.4)"
          | .join _ _ jb  => expRefs fuel acc jb
        expRefs fuel acc e
    | .jump _ es    => es.foldlM (expRefs fuel) acc
    | .cases _ scrut _ alts => do
        let acc ← expRefs fuel acc scrut
        alts.foldlM (fun acc (.mk _ _ b) => expRefs fuel acc b) acc
termination_by fuel

end

/-- Follow an edge into a (non-skipped) definition, pushing it on the
DFS stack; leaves (local binders, intrinsic carriers) contribute
nothing, and an edge into an unchecked (polymorphic/reactive) skipped
definition is an error. -/
def enterPure (defns : HashMap Int Defn) (unchecked : HashMap Int Defn) (fuel : Nat)
    (stack done : HashSet Int) (u : Int) : Except String (HashSet Int) :=
  match fuel with
  | 0 => throw "pure call graph deeper than the number of definitions (rwv bug?)"
  | fuel + 1 =>
    match defns.get? u with
    | none   =>
        match unchecked.get? u with
        | some d =>
            throw s!"{d.name.render}, a polymorphic or reactive carrier definition, is reachable from a process (its body is outside the checked machine fragment)"
        | none => pure done
    | some d =>
        if stack.contains u then
          throw s!"recursion among pure definitions reachable from a process: {d.name.render} (the pure call graph must be acyclic, doc/eidos.md §7.4)"
        else if done.contains u then pure done
        else do
          let refs ← expRefs expFuel #[] d.body
          let stack' := stack.insert u
          let done ← refs.foldlM (fun dn v => enterPure defns unchecked fuel stack' dn v) done
          pure (done.insert u)
termination_by fuel

/-- The reference roots of one process: every expression in its cells
and blocks. -/
def procRefs (pr : Proc) : Except String (Array Int) := do
  let mut acc : Array Int := #[]
  for c in pr.cells do
    if let some e := c.init then acc ← expRefs expFuel acc e
  for b in pr.entry :: pr.blocks.map (·.2) do
    for c in b.cmds do
      match c with
      | .bind _ rhs => acc ← expRefs expFuel acc rhs
      | .put _ a    => acc ← expRefs expFuel acc a
      | .get _ _    => pure ()
    acc ← termExpRefs termFuel acc b.term
  pure acc
where
  termExpRefs (fuel : Nat) (acc : Array Int) (t : Term) : Except String (Array Int) :=
    match fuel with
    | 0 => throw fuelErr
    | fuel + 1 =>
      match t with
      | .pause o _ args => do args.foldlM (expRefs expFuel) (← expRefs expFuel acc o)
      | .goto _ args    => args.foldlM (expRefs expFuel) acc
      | .halt e         => expRefs expFuel acc e
      | .cases s alts   => do
          alts.foldlM (fun acc (.mk _ _ t') => termExpRefs fuel acc t') (← expRefs expFuel acc s)
  termination_by fuel

def checkPureAcyclic (p : Program) : Except String Unit := do
  let checked : HashMap Int Defn :=
    HashMap.ofList ((p.defns.filter (fun d => !skipDefn d)).map fun d => (d.name.uniq, d))
  let unchecked : HashMap Int Defn :=
    HashMap.ofList ((p.defns.filter (fun d => skipDefn d && !intrinsicDefn d)).map
      fun d => (d.name.uniq, d))
  let mut done : HashSet Int := ∅
  for pr in p.procs do
    let roots ← procRefs pr
    for u in roots do
      done ← enterPure checked unchecked (p.defns.length + 1) ∅ done u

end Check

/-! ## The whole-program judgment -/

open Check in
/-- The machine-mode well-formedness judgment (module header): the
whole-program pre-ToHyle check — global binder uniqueness and name
distinctness, datatype well-formedness, the definition rules on the
non-carrier definitions, the §7.4 machine rules per process,
pure-acyclicity, and the `top` rule — succeeding exactly when every
rule holds, with the first violation reported. -/
def Program.checkMachine (p : Program) : Except String Unit := do
  let env := mkEnv p
  checkDistinct (← uniqSites p)
  checkDistinct (p.datas.map fun d => (d.name, s!"datatype name {d.name}"))
  checkDistinct (p.datas.flatMap fun d => d.cons.map fun c => (c.name, s!"data constructor name {c.name}"))
  checkDistinct (p.procs.map fun pr => (pr.name, s!"process name {pr.name}"))
  p.datas.forM (checkDataDefn env)
  p.defns.forM fun d =>
    if intrinsicDefn d then checkIntrinsicStub d
    else if skipDefn d then pure ()
    else checkDefn env d
  p.procs.forM (checkProc env)
  checkPureAcyclic p
  match p.top with
  | some t => checkTop p.defns t
  | none   => pure ()

end Rwv.Eidos

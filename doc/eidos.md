# Eidos: the ReWire typed intermediate representation

This document specifies the syntax and static discipline of *Eidos*, the
typed IR between GHC Core and Hyle, implemented in `ReWire.Eidos.*`.

Eidos is the IR of the compiler's front half; the pipeline is

    GHC Core  →  Eidos-P  →  (specialization, normalization)  →  Eidos-M  →  Hyle

*Eidos* (εἶδος, form) pairs with *Hyle* (ὕλη, matter): the front half
determines the form of the machine — its states, transitions, and data
path — and the back half realizes that form in bits. The producer
(`ReWire.GHC.ToEidos`) and the consumers of each level are outside the scope
of this document: everything below treats Eidos programs as given, whether
produced by the compiler or written by hand (`.eir` files, §9).

Eidos is one language with two *levels*, distinguished by which productions
and static rules are in force:

- **Eidos-P** (§3–§5): a System-F-lite mirror of GHC Core — polymorphic
  definitions, recursive lets, join points, n-ary case. The bridge targets
  it; specialization, dictionary elimination, and normalization happen here.
- **Eidos-M** (§7): the machine level — the monomorphic ANF fragment of
  Eidos-P plus a first-class process calculus (state cells, labeled blocks,
  `pause`/`goto`/`halt` terminators). Purification consumes the mono+ANF
  restriction of P (§6) and produces M; the translation to Hyle is a fold
  over M.

## 1. Design goals

- **G1 — One machine story.** The temporal structure of a program — where
  it pauses, what it saves, what it steps — is expressed by a dedicated
  calculus with its own checker, not by conventions among passes over a
  functional IR. A resumption state is a *label*; label identity is
  structural. Duplicate states arising from lost sharing are unrepresentable;
  duplicate states arising from inlining are mergeable by construction.
- **G2 — Globally unique binders.** Every binding site carries a distinct
  unique. Substitution never captures because the invariant is maintained,
  not because the representation prevents it; a linter checks the invariant
  mechanically. Alpha-equivalence is an explicit operation, never accidental
  structural equality.
- **G3 — Types are plain data.** No binders inside types; quantification
  only on signatures, as a plain variable list. Exactly one mandatory type
  per binder; everything else is synthesized by a total `typeOf`. There is
  no inference anywhere: monomorphization is substitution driven by the
  explicit type arguments GHC already provides, and checking is a linear
  bidirectional lint.
- **G4 — Sharing is structural.** Lets and join points from GHC Core are
  preserved, not lowered; a pass that duplicates must go through one audited
  refreshing clone. Preserve function hierarchy and preserve sharing are
  standing obligations of every pass (lost sharing is duplicated hardware).
- **G5 — Every stage is checkable.** The linter has modes (poly, mono,
  mono+ANF, machine) corresponding to the pipeline's invariant stages, so
  "pass X runs after pass Y" contracts are grammar- and lint-enforced,
  not pass-order-enforced.
- **G6 — A textual format from day one.** Eidos has a concrete syntax
  (`.eir`, §9) with a parser and a round-trip property, enabling golden
  tests at the front half's boundaries and hand-written test inputs, exactly
  as Hyle's `.rwc` does for the back half.

Non-goals at this writing: multi-clock semantics (procs carry an optional
clock-domain annotation, default the single implicit domain); multiple
processes and process composition operators (the program grammar admits a proc
list and a `top` designation; the singleton restriction is only a property of
what procify mints, not a grammar, parser, or lint rule); memory primitives.

## 2. Metavariables and annotations

Metavariables: `x`, `y` range over term names; `a` over type variables;
`L` over join point and block labels; `T` over type constructor names;
`C` over data constructor names; `f` over definition names; `s` over state
cells; `u` over uniques (naturals); `n`, `k` over type-level naturals;
`τ`, `σ` over types; `κ` over kinds; `e` over expressions.

Term names, type variables, and labels are pairs `occ#u` of display text
and unique (a machine integer in the implementation); equality and hashing
are *by unique only*. A bare `_` is not a name (it would collide with the
default case alternative); `_`-prefixed occurrences with uniques are fine. Type and data
constructor names are stable dotted text with no uniques (they are global
and never shadowed). Every AST node carries an annotation (`Annote`,
source location); annotations are semantically inert, ignored by every
equality, and not printed in the concrete syntax except as comments.

**The uniqueness discipline.** In a well-formed program, every binding
site — definition names, parameters, lambda/let/join/case binders, type
variables in signatures, block labels — has a unique distinct from every
other binding site's. Occurrences share the unique (and signature) of
their binder. The bridge establishes the invariant; passes preserve it;
any operation that duplicates a term refreshes the copies' binders through
one shared primitive. The linter's uniqueness rule re-checks it globally.

## 3. Eidos-P abstract syntax

### 3.1 Kinds and types

    κ ::= *  |  Nat  |  κ₁ → κ₂

    τ ::= T                                type constructor
        | τ₁ τ₂                            application
        | a                                type variable (occ#u, kinded)
        | n                                type-level natural literal
        | τ₁ → τ₂                          arrow

Types contain no binders and no quantifiers. Type-level arithmetic appears
as applications of the built-in constructors `+`, `-`, `*` (kind
`Nat → Nat → Nat`); a type is *nat-closed* when every such application
evaluates to a literal, and `natNorm` folds all nat-closed subterms. Type
equality throughout the compiler is structural equality after `natNorm`
(annotations ignored).

### 3.2 Signatures

    sig ::= ∀ a₁ … a_g . τ                 g ≥ 0; τ arrow- and app-structured, forall-free

Quantification exists only here. Every binder carries a signature; local
binders (parameters, lambda/let/case/join binders) always carry `g = 0`
(monomorphic) signatures — only top-level definition names, data
constructors, and primitive references are polymorphic, and only until
specialization.

### 3.3 Expressions

    e ::= x                                variable occurrence
        | C :: τ                           data constructor at instantiated type τ
        | p :: τ                           primitive (builtin) at instantiated type τ
        | lit :: τ                         integer literal at type τ
        | "…"                              string literal
        | list [e₁, …] :: τ                list literal
        | vec [e₁, …] :: τ                 vector literal
        | e (e' | @τ)                      application (term or type argument)
        | λ (x :: τ) → e                   abstraction
        | let b in e                       local binding (§3.4)
        | jump L (e₁, …, e_k)              saturated jump to a join point
        | case e of x { alt; … } :: τ      scrutinee, case binder x, result type τ

    alt ::= _ → e                          default (first, if present)
          | C (x₁ :: τ₁) … (x_k :: τ_k) → e
          | lit → e

Constructor, primitive, and integer-literal occurrences carry their full
*instantiated* type; variables read their type from their binder's
signature; everything else synthesizes. Type arguments (`@τ`) may be
applied only to variable heads, must precede all term arguments in a
spine, and must saturate the head's quantifier list — so the specializer
reads instantiations directly off spines and `typeOf` is total (§5).

The case binder `x` names the scrutinee's value in every alternative
(the Core convention); the default alternative, when present, comes first.

### 3.4 Bindings

    b ::= x :: τ = e                       non-recursive let
        | rec { x₁ :: τ₁ = e₁; … }         recursive group
        | join L (x₁ :: τ₁, …, x_k :: τ_k) = e     join point of arity k

A join point is a labeled continuation: it may be reached only by `jump`,
every `jump L (…)` supplies exactly `k` arguments, and jumps occur only in
*tail position* of the join's scope (the let body, or transitively the tail
of other join bodies bound in it). Join points are the IR-level residue of
GHC's pattern-match decision trees and of continuation sharing; passes must
preserve them (G4) — a join point that survives to purification becomes a
shared block, i.e. one resumption state instead of one per reference.

### 3.5 Definitions

    defn ::= f :: sig ; attrs? f (x₁ :: τ₁) … (x_k :: τ_k) = e
    attrs ::= (inline | noinline)? (from f₀ (τ̄) | baked f₀)?

A definition's parameter telescope matches a prefix of its signature's
arrow spine (arity is structural). `inline`/`noinline` carry over GHC
pragmas; `from f₀ (τ̄)` and `baked f₀` are *provenance* — the origin
definition of a compiler-minted clone, with the type instantiation for a
specializer clone and nothing further for a partial-evaluator clone whose
baked arguments are terms — carried for dumps, error messages, and stable
generated-HDL naming.

### 3.6 Datatypes

    data ::= data T κ { C₁ :: sig₁; … }

Constructor signatures quantify the datatype's parameters:
`C :: ∀ ā. τ₁ → … → τ_k → T ā`. Datatypes stay parametric through
specialization (only definitions are cloned); consumers size constructor
applications at use sites by first-order matching.

### 3.7 Programs

    prog ::= data* defn* proc* top f

A program designates one *device root* with `top`. The `proc` productions
belong to the M level (§7); at the P level the list is empty. The restriction
that the reactive root set is the singleton `{top}` is a property of the
current pipeline — procify mints exactly one proc, and neither the grammar, the
parser, nor the linter imposes the restriction, but process composition is
future work.

## 4. Static semantics: Eidos-P

### 4.1 Contexts and modes

The linter checks a program in one of four cumulative modes:

- **poly** (post-bridge): the rules of §4.2–§4.4.
- **mono** (post-specialization): additionally, every *definition* signature
  has `g = 0` (constructor signatures stay parametric, §3.6, and are
  covered at occurrences, which carry instantiated types) and every type is
  nat-closed. Value binders may still be higher-order and the type grammar
  is still open here: specialization eliminates polymorphism, not functions
  — first-orderization is the partial evaluator's job, which runs after it.
  Builtin-named definitions (`rwPrim*`) are exempt: they are the builtins'
  type assumptions riding to the Eidos-to-Hyle fold as polymorphic signature
  carriers (error-stub bodies, never referenced as variables — references
  become `Prim` occurrences at the bridge), and they check in poly mode.
  (`Prim` occurrences themselves are checked against the builtin signature
  table of §7.6, in every mode.)
- **mono+ANF** (procify's input contract): additionally, value binders are
  first-order and the type grammar is restricted to the *representable
  closure*: `Vec n τ`, `Finite n`, `Bool`, `()`, tuples, monomorphic ADTs,
  `Integer`, `Proxy n`, `String` (literal positions only), with arrows only
  in definition signatures — plus the reactive types (`ReacT`, `StateT`,
  `Identity`), which are permitted *only* until purification — plus the ANF
  restriction of §6.
- **machine** (post-procify): the rules of §7.4; reactive types are out of
  the grammar entirely.

### 4.2 Expression typing

Typing is bidirectional and syntax-directed; there is no unification. The
interesting rules (all others are structural):

- **Spines.** For an application spine `x @τ₁ … @τ_g e₁ … e_k` where
  `x :: ∀ a₁ … a_g . σ`: the type arguments must saturate the quantifier
  list; the spine's type is `σ[ā ↦ τ̄]` after peeling `k` arrows, each
  peeled argument checking against its domain (after `natNorm`). A type
  argument on a non-variable head, out of prefix position, or in excess of
  the quantifier list is ill-formed.
- **Case.** The scrutinee synthesizes `τ_s`; the case binder gets `τ_s`;
  each `DataAlt C` requires `C`'s instantiated result to match `τ_s` and
  binds its fields at the instantiated field types; every alternative's
  body checks against the carried result type. Alternative lists are
  non-empty; a default, if present, is first; constructor alternatives are
  disjoint.
- **Join points.** `join L (x̄) = e_L in e`: jumps to `L` appear only in
  tail position of `e` (and of sibling join bodies); every jump saturates
  the arity; `e_L` and `e` check against the same type (the join's result
  is the scope's result). A join label never escapes its scope (it is not
  a value; `jump` is not an expression head that can be abstracted).
- **Literals.** An integer literal's carried type must be `Integer` or a
  bit-vector/`Finite` type wide enough for the value (checked in mono
  mode, deferred in poly mode where widths may be open).

### 4.3 Definition and program rules

Parameters match the signature's arrow prefix; the body checks against the
remainder. `top` names a definition of type `ReacT τ_i τ_o Identity τ`
(the device signature; checked in mono mode — the result type `τ` is
unconstrained, since a non-halting device never produces it; the halt
policy is the machine level's concern, §7.3). Data constructor signatures
quantify exactly the parameters of their datatype and construct exactly it.

### 4.4 Uniqueness and scoping

Every binding unique is globally distinct (§2); every occurrence's unique
is bound in scope; a variable occurrence's signature equals its binder's.
The uniqueness rule is what licenses environment-map substitution
everywhere in the compiler.

## 5. `typeOf`, evaluation, and equality

**`typeOf`** (in `ReWire.Eidos.Types`) is total on lint-clean programs and
synthesizing: binders carry types; `Con`/`Prim`/literal occurrences carry
instantiated types; spines instantiate signatures by substitution. It
follows the Core-Lint convention: on ill-formed input it fails loudly, and
the linter exists to reject such input with a located diagnostic first.

**Nat evaluation.** `evalNat` evaluates nat-closed types; `natNorm`
normalizes by folding. No solving exists anywhere: the GHC typechecker (and
its typelits plugins) already discharged all constraints; what remains is
arithmetic on literals.

**Equality.** `Ty` equality is structural modulo annotations (and callers
normalize with `natNorm` first). *There is no `Eq` on expressions*:
alpha-equivalence (canonical renumbering of binders, annotations ignored)
is an explicit operation used at specialization memo-keys and
block-merging; hash-based or structural term equality is deliberately not
offered (the predecessor IR's hash-equality instance was a live collision
unsoundness).

**Dynamic semantics of the P level.** Eidos-P is a pure, total fragment
(general recursion is confined to the reactive layer, where productivity is
later enforced by guardedness, §7.4): its programs mean what the same
programs mean in GHC Core under call-by-value evaluation of the reachable
first-order fragment; the observable semantics of a whole program is defined
by the M level it compiles to (§7.5) and, transitively, by Hyle's
denotational semantics (doc/hyle.md §6). Eidos-P is a *transformation*
level; no interpreter for it is provided or needed — behavioral oracles run
at the Hyle level.

## 6. The mono+ANF restriction (procify's input)

The ANF productions are shared with the full P grammar (they are a
restriction, not a new syntax): in mono+ANF mode every definition body is

    e ::= let x :: τ = r in e  |  ret a  |  jump L (ā)
    r ::= a  |  x a₁ … a_k  |  C ā  |  p ā  |  case a of x { alt; … } :: τ
    a ::= x  |  lit :: τ                  atoms

Join points survive into this form (their bodies are ANF like any other;
jumps are tails with atom arguments); `ret a` marks a tail. The
normalization from full P to mono+ANF is a small ordered ruleset
(eta-expansion to signature arity with parameters in the telescope,
argument- and subject-naming, alternative flattening, let-flattening).

The *reactive fragment is exempt from naming* — it is procify's input
skeleton, and its structure must survive: a spine whose type mentions the
reactive stack (`rwPrimBind`, `rwPrimSignal`, `rwPrimGet`, …) stays a
spine, keeping its lambda (continuation) arguments in place with
A-normalized bodies and its reactive arguments in place (a pure let may
wrap them), while its pure non-atom arguments are named; and a case with
a reactive result type stays in tail position (scrutinee named,
alternatives A-normalized) — procify turns it into a terminator case. A
case whose alternatives jump (the scope of a join point) likewise stays
in tail position, since jumps are tail-only. Any other pure-resulted
case or call is named like any other computation.

## 7. Eidos-M: the process calculus

*Normative target of the `procify` pass. This section fixes the grammar,
static rules, and machine-step contract.*

### 7.1 Processes

    proc ::= proc P : τ_I ~> τ_O clock? {
                 state s₁ : τ₁ := e₀₁ ;  …          cells: one per state layer
                 entry { cmds ; term }               the reset block (implicitly labeled)
                 block L₁ (x₁:τ, …, inp:τ_I) { cmds ; term }
                 …
             }

    cmd  ::= x :: τ ← r                    pure computation (ANF rhs)
           | x :: τ ← get s                cell read
           | put s a                       cell write
    term ::= pause a → L (a₁, …)           emit a : τ_O; resume at L next cycle
           | goto L (a₁, …)                intra-cycle transfer (saturated)
           | halt a                        terminate with answer a
           | case a of { _ → term; C x̄ → term; …; lit → term; … }
                                           (default first, as at the P level)

A block's *last* parameter is the resumed input (type `τ_I`); a
`pause a → L(ā)` supplies all of `L`'s parameters *except* that one, which
the machine supplies on resumption. A block may be the target of both
`pause` and `goto` (a `goto` supplies all parameters). The `entry` block is
parameterless, implicitly labeled (so `goto entry` expresses restart), and
holds the reset prefix; cells it writes before the first pause become
register initials (§7.3).

Cells are named and per-proc: `get`/`put` target a cell by name — there is
no state-stack indexing and no cross-proc state. Cell initials `e₀` are
closed pure expressions, compile-time evaluated (consulting combinational
extern models; a model-less extern in an initial is a located error), or
`undef` for a cell first written before any read on every path from entry.
An `undef` initial *denotes* the zero value of the cell's type (`zero_τ`,
§7.5.1) — the write-before-read convention makes the choice unobservable
in intended use, and pinning it keeps the semantics total and matches the
compiled machine bit-for-bit.

Externs: combinational extern calls (`xcall`) are ordinary ANF right-hand
sides; *sequential* (clocked) extern calls are legal only as commands, and
each syntactic occurrence denotes one device instance.

Labels are a distinct namespace, per-proc; all generated names (label
enum, step record, cells) are qualified by the proc name (the qualifier
renders empty while programs have one proc).

### 7.2 Degenerate forms

Zero-cell, single-label procs (combinational devices), zero-input procs,
and zero-width cells and inputs are all legal; consumers handle them as
Hyle handles zero-width values (doc/hyle.md §8.6).

### 7.3 The machine-step contract

Purification proper is a fold over a proc's label table producing:

- a `Label` enumeration, one constructor per **pause-target** block
  (goto-only blocks are intra-cycle and get no state);
- a step record, laid out `out | next | args | cells'` — output value,
  next label, the next label's saved parameters (a per-label zero-padded
  union field, LSB-aligned, sized by the widest parameter list), and the
  updated cells;
- one n-ary dispatch over labels;
- register initials and the initial label, by compile-time evaluation of
  `entry` to its first pause (total by §7.4 guardedness);
- a `halted` flag bit if `halt` is reachable, occupying the record's
  most-significant bit (post-halt outputs are unspecified; the strict
  mode `--no-halt` rejects reachable halt).

Tag width is `nbits(#labels)`; label constructor names derive from
`(proc name, source continuation name)` under one shared, deterministic
naming and ordering function, which fixes dispatch order and tag values.

### 7.4 Static semantics: the machine rules

- **Signal-guardedness**: the goto-only subgraph of the block graph is
  acyclic — every cycle crosses a `pause`. This one rule yields: divergent
  (pause-free) loops are rejected with a located error; entry evaluation
  terminates without fuel; blocks lower to acyclic Hyle definitions.
- **Constness** of cell initials; **representability** (fixed bit width)
  of every binder, parameter, and cell; full-arity jumps and gotos; the
  input parameter typed `τ_I`; "root proc never pauses" (a proc with no
  pause target has no machine) — all with located diagnostics.
- **Pure-acyclicity**: the call graph of the pure definitions reachable
  from the process (block bodies, cell initials, and transitively) is
  acyclic. Together with guardedness this makes the machine semantics
  (§7.5) a well-founded definition and block lowering total. (Enforced
  today downstream, by the Hyle checker's recursion rule and the
  translation's entry-evaluation check; Eidos-level lint enforcement is
  pending.)
- Command right-hand sides are the *pure* ANF forms of §6 — jumps, join
  bindings, and reactive spines are excluded from blocks (§7.1); joins
  survive only in pure definition bodies. (Holds by construction —
  procify's ANF input names only simple right-hand sides, and
  jump-containing cases stay in tail position; the implementation's
  command AST is a general expression, the machine lint enforces the
  jump/tail discipline, and the full r-shape check is pending, like the
  representable-closure permit-list.)
- **Scope of the uniqueness invariant at this stage**: procify builds
  block parameters and command binders from the reactive fragment it
  consumes, and the consumed definitions remain in the program (as
  unreferenced carriers) still carrying those uniques — one binder can
  also ride a goto chain into several blocks. Whole-program binder
  uniqueness (§2, §4.4) therefore holds of the datatype and definition
  fragment, per-proc block labels are distinct, and in-block binding is
  validated by scoping and occurrence-signature agreement rather than
  global uniqueness. (Pass 8 runs the per-proc machine lint only, so
  the implementation never re-checks global uniqueness here.)
- All rules are stated per-proc.

### 7.5 Machine semantics

This section defines the meaning of a well-formed (machine-mode) process
as a stream function, independent of the translation to Hyle. The
translation realizes exactly this semantics (§7.5.6); the four-way
cosimulation in `rwc-test` tests the correspondence per test.

#### 7.5.1 Semantic values

For each representable type τ (§4.1), the set `V_τ` of values:

- `V_(Vec n τ)` = length-n tuples over `V_τ`. Index 0 is the *head*
  (printed leftmost); for `Vec n Bool` the head is the most significant
  bit.
- `V_Bool = {False, True}`; `V_() = {()}`; `V_(Proxy n) = {Proxy}`.
- `V_(Finite n) = {0, …, n−1}` for n ≥ 1. `Finite 0` is uninhabited,
  matching `Data.Finite`: it is representable at zero width, but no
  value of the type exists — zero/undef initialization at `Finite 0`
  and conversions into it fail rather than producing a degenerate
  value.
- `V_Integer = {0, …, 2¹²⁸ − 1}`: `Integer` compiles at fixed width 128
  (§4.1); its machine meaning is the 128-bit residue. Integer literals
  at `Finite n` and `Vec n Bool` are lint-checked to fit; at `Integer`
  every literal is accepted and denotes its 128-bit residue (wrapping
  silently), as does runtime arithmetic.
- For a monomorphic instance `T τ̄` of a declared datatype: the disjoint
  union, over its constructors `C :: ∀ā. τ₁ → … → τ_k → T ā`, of tuples
  `C(v₁, …, v_k)` with `vᵢ ∈ V_(τᵢ[ā ↦ τ̄])`. (Recursive datatypes are
  not representable, so this induction is well-founded.)

**Bit readings.** `bv : V_(Vec n Bool) → {0, …, 2ⁿ−1}` reads a bit
vector MSB-first: `bv(v) = Σᵢ vᵢ · 2^(n−1−i)`; `⟨x⟩ₙ = x mod 2ⁿ` is the
width-n residue and `bv⁻¹ₙ` its inverse reading. `Finite` and `Integer`
values are already numbers. These canonical readings are all the builtin
denotations (§7.6) need; the general data-to-bits encoding of ADTs
belongs to the translation (doc/hyle.md), not to this semantics.

**Zero values.** `zero_τ ∈ V_τ`, used by cell initials (7.5.4) and the
`error` builtin (§7.6): `zero_(Vec n τ)` = n copies of `zero_τ`;
`zero_Bool = False`; `zero_() = ()`; `zero_(Finite n) = 0`;
`zero_Integer = 0`; `zero_(T τ̄) = C₀(zero_τ₁', …)` where `C₀` is T's
first constructor in declaration order. Well-defined by the same
induction.

#### 7.5.2 Pure evaluation

Let ρ map term binders to values and η interpret externs (7.5.5).
`E⟦e⟧ρ` is standard call-by-value big-step evaluation of the machine-mode
pure fragment:

- **Atoms**: variables look up ρ; an integer literal at type `Integer`,
  `Finite n`, or `Vec n Bool` denotes its residue/value at that type
  (7.5.1); `vec [e₁,…]` denotes the tuple of its elements' values.
- **Constructors**: a saturated constructor spine denotes `C(v̄)`.
- **Definition calls**: a saturated call to a pure definition evaluates
  the definition's body with parameters bound to argument values —
  well-founded because the pure call graph is acyclic (§7.4,
  *pure-acyclicity*). A definition may be eta-reduced (fewer declared
  parameters than its signature has arrows — including a bare builtin
  body); it means its eta-expansion, exactly as the translation
  eta-expands before lowering. The same reading covers a builtin
  applied below its signature arity in an argument position (a
  partially applied `error` at function type denotes the function
  constantly `zero` at the result).
- **Builtins**: per the signature and denotation table of §7.6.
  Higher-order builtin arguments (`rwPrimVecMap`'s function argument: a
  lambda, a possibly-partially-applied reference to a definition, or a
  possibly-partially-applied operator builtin) are applied semantically,
  element by element.
- **Case**: evaluate the scrutinee; select the first *matching*
  constructor or literal alternative, binding the case binder to the
  scrutinee's value and field binders to its components; the default
  alternative (syntactically first, when present) fires only when no
  other alternative matches. Machine-mode case analyses are total
  (a non-matching value with no default is ill-formed input; the
  translation compiles the last alternative as unconditional).
- **Joins**: `let join L(x̄) = e_L in e` evaluates `e` with `L` bound to
  its continuation; `jump L(ā)` evaluates `e_L` under `x̄ ↦ ā-values`.
  Jumps are tail transfers to lexically enclosing joins; the scoping
  discipline (§3.4) admits no recursion among joins, so this is
  structural.

Evaluation is total on machine-mode programs (no exceptions, no
divergence): every partiality in the source discipline — literal fit,
static-argument requirements, `error` — is either rejected by lint/the
translation or given a total denotation (§7.6).

#### 7.5.3 Configurations and the machine step

A machine state is `(ℓ, w̄, σ)`: a pause-target label ℓ, saved values w̄
for ℓ's parameters *except* the last (the resumed input, §7.1), and a
cell store σ.

Block-body execution `X⟦cmds; term⟧(ρ, σ)` threads the cell store
through the commands and then runs the terminator:

    X⟦x ← r;  rest⟧(ρ, σ)   = X⟦rest⟧(ρ[x ↦ E⟦r⟧ρ], σ)
    X⟦x ← get s; rest⟧(ρ, σ) = X⟦rest⟧(ρ[x ↦ σ(s)], σ)
    X⟦put s a; rest⟧(ρ, σ)  = X⟦rest⟧(ρ, σ[s ↦ E⟦a⟧ρ])

    X⟦pause a → L(ā)⟧(ρ, σ) = Step(E⟦a⟧ρ, L, E⟦ā⟧ρ, σ)
    X⟦goto L(ā)⟧(ρ, σ)      = X⟦body(L)⟧(params(L) ↦ E⟦ā⟧ρ, σ)
    X⟦halt a⟧(ρ, σ)         = Halt(E⟦a⟧ρ, σ)
    X⟦case a of talts⟧(ρ, σ) = X⟦term of the selected alternative⟧(ρ', σ)
                               (selection and field binding as in 7.5.2)

The `goto` clause is well-founded by signal-guardedness (§7.4): the
goto-only subgraph is acyclic, so recursion decreases the block's rank
in any topological order of that subgraph. A `goto` supplies *all* of
the target's parameters, including its resumed-input slot.

The one-cycle step, for input value i:

    step(ℓ, w̄, σ, i) = X⟦body(ℓ)⟧(params(ℓ) ↦ w̄ ⧺ ⟨i⟩, σ)

yielding either `Step(o, ℓ′, w̄′, σ′)` — an emitted output and the next
state — or `Halt(a, σ′)`.

#### 7.5.4 Initialization, streams, and halt

The initial cell store σ₀ maps each cell to its declared initial's value
(a closed pure expression, evaluated with extern models per §7.1) or, for
`undef` initials, to `zero_τ` — the write-before-read convention of §7.1
makes this choice unobservable in intended use, and the compiled machine
realizes exactly `zero_τ`. (Declared initials are a calculus-level
generality: the pipeline's only producer emits `undef` for every cell,
entry-block writes are the implemented initialization path, and the
translation does not yet consult declared initials.)

The reset step runs the parameterless entry block: `X⟦entry⟧(∅, σ₀)`
(terminating, by guardedness). If it yields `Step(o₋, ℓ₀, w̄₀, σ₀′)`, the
initial machine state is `s(0) = (ℓ₀, w̄₀, σ₀′)` and **the entry's
emitted value o₋ is not observable** — it is the value "paused on"
during reset; the first observable output is produced at cycle 0. If the
entry halts, the process's observable trace is empty.

The process then denotes the stream function

    𝔐⟦P⟧η : (ℕ → V_I) → V_O-traces

defined by iterating: `step(s(t), i(t))` yields `(o(t), s(t+1))` while
it yields `Step`; if it yields `Halt(a, ·)` at cycle k, the trace is the
finite prefix `⟨o(0), …, o(k−1)⟩` and the process's *result* is a — the
halting cycle drives no *defined* output (the machine does not route the
answer to the output port; whatever halt-record bits happen to overlap
the output field are unspecified), and there is no post-halt behavior
(§7.3; the strict mode `--no-halt` rejects reachable halt, and halt-free
processes denote total streams).

**Causality.** `o(t)` depends on `i(0), …, i(t)`: the resumed block
reads the current cycle's input combinationally (a Mealy machine, as in
doc/hyle.md §6.4). It is the machine *state* — label, saved arguments,
cells — that is registered and depends only on strictly earlier inputs.

#### 7.5.5 Externs

Mirroring doc/hyle.md §6.1, algebraically: an interpretation η assigns
each model-less combinational extern a function between the `V`-domains
of its use-site monotype; an extern with a usable model is pinned to the
model definition's denotation. A process's denotation is a function of
η; processes whose externs all carry models (or that have none) denote
absolutely. Sequential (clocked) externs — legal only as commands, one
device instance per syntactic occurrence — are interpreted as strictly
causal stream functions, exactly as instances in doc/hyle.md §6.4.
Cryptol foreign functions (`rwPrimCryptol`) are model-carrying by
construction: their denotation is fixed by the translated Cryptol
fragment.

#### 7.5.6 Correspondence to Hyle

The translation (`ReWire.Eidos.ToHyle`) realizes this semantics as a
Hyle device: the machine state becomes the registers (label tag ⧺ saved
arguments ⧺ cells, in the translation's record layout), each block
becomes a definition, dispatch is a case on the label tag, and the
register initials come from compile-time evaluation of the entry block
(7.5.4). The device's stream function (doc/hyle.md §6.4) agrees with
`𝔐⟦P⟧η` through the data-to-bits encoding on the observable trace — up
to and excluding the halting cycle, exactly (bit-for-bit) for halt-free
processes. The four-way cosimulation check in `rwc-test` tests exactly
this correspondence, and `rwc --certify` checks it per compilation with
a machine-checked validator (doc/certify.md).

### 7.6 Builtin signatures and denotations

The normative signature scheme and machine-level denotation of every
builtin (`ReWire.Builtins`; occurrences print as `rwPrim<Name>`, §9).
Signatures are what `Prim` occurrences must instantiate: an occurrence's
carried type must be a substitution instance of its builtin's scheme
(this is the linter's builtin signature table). Quantified `n, m, i` have
kind `Nat`; `a, b` kind `*`; `m̂` ranges over the reactive stack. In
denotations, `x = bv(v)` and `y = bv(w)` are the bit readings of the
first and second `Vec _ Bool` arguments (§7.5.1), and `⟨·⟩ₙ` the width-n
residue; "as hyle `op`" means the denotation table of doc/hyle.md §5.2,
through `bv`. A **static** argument must be a compile-time literal after
inlining (a located error otherwise) — a partiality of the *translation*,
not of the denotation.

**Eliminated before the M level** (signatures only; these have no
machine denotation — purification consumes them, and none may appear in
a process or in any definition reachable from one):

| builtin | signature | eliminated by |
|---|---|---|
| `rwPrimBind` | `∀ m̂ a b. m̂ a → (a → m̂ b) → m̂ b` | procify |
| `rwPrimReturn` | `∀ m̂ a. a → m̂ a` | procify |
| `rwPrimGet` | `∀ s m̂. StateT s m̂ s` | procify |
| `rwPrimPut` | `∀ s m̂. s → StateT s m̂ ()` | procify |
| `rwPrimSignal` | `∀ i o m̂. o → ReacT i o m̂ i` | procify |
| `rwPrimLift` | `∀ t̂ m̂ a. m̂ a → t̂ m̂ a` | procify |
| `rwPrimExtrude` | `∀ i o s m̂ a. ReacT i o (StateT s m̂) a → s → ReacT i o m̂ a` | procify |

**Foreign mechanisms** (signatures; denotation per §7.5.5):

| builtin | signature | notes |
|---|---|---|
| `rwPrimExtern` | `∀ a. [(String, Integer)] → String → String → [(String, Integer)] → [(String, Integer)] → String → a → String → a` | params, clock, reset, ins, outs, module name, model, instance name (reserved; currently ignored) — all but the model static; becomes an `xcall` (combinational) or an instance (clocked) |
| `rwPrimCryptol` | `∀ a. String → String → a → a` | module file and function name static; model-carrying by construction (§7.5.5) |

**Bit-vector operations** (denotations through `bv`; the result width is
fixed by the type):

| builtin | signature | denotation |
|---|---|---|
| `rwPrimAdd` | `∀ n. Vec n Bool → Vec n Bool → Vec n Bool` | as hyle `add`: `⟨x + y⟩ₙ` |
| `rwPrimSub` | ″ | as hyle `sub` (modular) |
| `rwPrimMul` | ″ | as hyle `mul` |
| `rwPrimDiv` | ″ | as hyle `udiv`: y = 0 ⇒ 2ⁿ−1 (SMT-LIB) |
| `rwPrimMod` | ″ | as hyle `umod`: y = 0 ⇒ x (SMT-LIB) |
| `rwPrimPow` | ″ | as hyle `pow`: `⟨x^y⟩ₙ`, 0⁰ = 1 |
| `rwPrimAnd`, `rwPrimOr`, `rwPrimXOr` | ″ | bitwise |
| `rwPrimXNor` | ″ | `⟨2ⁿ−1⟩ − (x ⊕ y)` (bitwise complement of xor) |
| `rwPrimNot` | `∀ n. Vec n Bool → Vec n Bool` | bitwise complement |
| `rwPrimLShift` | `∀ n. Vec n Bool → Vec n Bool → Vec n Bool` | as hyle `shl` (y ≥ n ⇒ 0) |
| `rwPrimRShift` | ″ | as hyle `lshr` (y ≥ n ⇒ 0) |
| `rwPrimRShiftArith` | ″ | as hyle `ashr` (sign-filling; y ≥ n ⇒ all-sign) |
| `rwPrimEq` | `∀ n. Vec n Bool → Vec n Bool → Bool` | x = y (n = 0 ⇒ True) |
| `rwPrimGt`, `rwPrimGtEq`, `rwPrimLt`, `rwPrimLtEq` | ″ | unsigned comparison |
| `rwPrimLAnd` | ″ | (x ≠ 0) ∧ (y ≠ 0) |
| `rwPrimLOr` | ″ | (x ≠ 0) ∨ (y ≠ 0) |
| `rwPrimLNot` | `∀ n. Vec n Bool → Bool` | x = 0 |
| `rwPrimRAnd` | `∀ n. Vec n Bool → Bool` | as hyle `redand`: x = 2ⁿ−1 (n = 0 ⇒ True) |
| `rwPrimROr` | ″ | as hyle `redor`: x ≠ 0 |
| `rwPrimRNAnd` | `∀ n. Vec (1 + n) Bool → Bool` | ¬ redand |
| `rwPrimRNor` | ″ | ¬ redor |
| `rwPrimRXOr` | ″ | as hyle `redxor`: parity of x |
| `rwPrimRXNor` | ″ | ¬ parity |
| `rwPrimMSBit` | ″ | the head element (bit n, LSB-numbered — the MSB) |

*(The GHC reference models of every row in this table — including the
SMT-LIB division-by-zero equations — are exact at every width.)*

**Conversions, `Finite`, and miscellany**:

| builtin | signature | denotation | static / well-formedness |
|---|---|---|---|
| `rwPrimError` | `∀ a. String → a` | `zero_a` (§7.5.1) | the message is not compiled (no hardware representation); the translation warns, quoting the message when it is a literal |
| `rwPrimBits` | `Integer → Vec 128 Bool` | `bv⁻¹₁₂₈(x)` (identity on residues) | |
| `rwPrimResize` | `∀ m n. Vec n Bool → Vec m Bool` | `bv⁻¹ₘ⟨x⟩ₘ` — truncate (keep low bits) or zero-extend | |
| `rwPrimNatVal` | `∀ n. Proxy n → Integer` | `⟨n⟩₁₂₈` | |
| `rwPrimBitSlice` | `∀ m n. Vec n Bool → Finite n → Finite n → Vec m Bool` | bits j…i of x, LSB-numbered (head = bit n−1) | j, i static (`finite` applications of integer literals); j + 1 ≥ i (j = i − 1 is the empty slice); m = j − i + 1, checked at the fold |
| `rwPrimBitIndex` | `∀ n. Vec n Bool → Finite n → Bool` | bit i of x, LSB-numbered | i static (a `finite` application of an integer literal) |
| `rwPrimFinite` | `∀ n. Integer → Finite n` | the value itself | static; 0 ≤ value < n |
| `rwPrimFiniteMinBound` | `∀ n. Finite n` | 0 | n ≥ 1 |
| `rwPrimFiniteMaxBound` | `∀ n. Finite n` | n − 1 | n ≥ 1 |
| `rwPrimToFinite` | `∀ m n. Vec m Bool → Finite n` | `bv(v)` | 2^m ≤ n |
| `rwPrimToFiniteMod` | `∀ m n. Vec m Bool → Finite n` | `bv(v) mod n` | n ≥ 1 |
| `rwPrimFromFinite` | `∀ n m. Finite n → Vec m Bool` | `bv⁻¹ₘ(i)` | n ≤ 2^m |

**Vectors** (element-polymorphic; denotations are algebraic on tuples,
positions 0-indexed from the head):

| builtin | signature | denotation | static |
|---|---|---|---|
| `rwPrimVecFromList` | `∀ n a. [a] → Vec n a` | the elements, in order | the list literal; its length must equal n (checked at the fold) |
| `rwPrimVecReplicate` | `∀ n a. a → Vec n a` | n copies | |
| `rwPrimVecReverse` | `∀ n a. Vec n a → Vec n a` | reversal | |
| `rwPrimVecSlice` | `∀ i n m a. Proxy i → Vec ((i + n) + m) a → Vec n a` | elements i … i+n−1 | |
| `rwPrimVecRSlice` | `∀ i n m a. Proxy i → Vec ((i + n) + m) a → Vec n a` | elements ℓ−i−n … ℓ−i−1, ℓ = i+n+m (counted from the end) | |
| `rwPrimVecIndex` | `∀ n a. Vec n a → Finite n → a` | element i (dynamic index) | |
| `rwPrimVecIndexProxy` | `∀ n m a. Vec ((n + m) + 1) a → Proxy n → a` | element n | |
| `rwPrimVecConcat` | `∀ n m a. Vec n a → Vec m a → Vec (n + m) a` | concatenation | |
| `rwPrimVecMap` | `∀ n a b. (a → b) → Vec n a → Vec n b` | elementwise application (7.5.2) | |
| `rwPrimVecGenerate` | `∀ n a. (Finite n → a) → Vec n a` | `⟨f(0), …, f(n−1)⟩` (function argument as in 7.5.2) | |

One user-facing function deserves a disclaimer here: `ReWire.Bits.toInteger`
is *not* a builtin but a plain GHC-side convenience for simulation and
testing — `Integer` is a compile-time-literal-only type in the fragment,
so rwc rejects programs that reach it. (`ReWire.Bits.sext`, by contrast,
is an ordinary derived definition — replicated-MSB concatenation — and
compiles like any user code.)

## 8. Pass discipline

Every pass over Eidos:

1. preserves binder uniqueness (duplication only through the refreshing
   clone primitive);
2. preserves lets and join points (sharing; G4) — dead-binding removal and
   the occurrence-driven simplifier are the sanctioned exceptions;
3. leaves the program in a lint mode at least as strong as its input's
   (the pipeline is monotone through poly → mono → mono+ANF → machine);
4. is annotation-transparent (annotations propagate; never compared).

`--debug-lint` fills the gaps the standing lints leave, so that every
front-half pass is followed by a lint in the mode of its pipeline
position; the machine-mode lint after purification runs unconditionally —
the predecessor pipeline's back half was uncheckable by construction.

**Specialization** (poly → mono) is a worklist over instantiation
requests. A request is a spine `f @τ̄ …` in a monomorphic body whose head
is a polymorphic definition; since local binders are monomorphic and type
arguments saturate quantifier lists (§4.2), every request's `τ̄` is closed.
Each new `(f, natNorm τ̄)` mints a clone by pure type substitution through
the refreshing clone primitive, named `f$<rendered type arguments>`
(e.g. `ReWire.get$StateTW8Identity`; `ReWire.Eidos.Naming.originTag`
sanitizes the rendering and hashes it when it is too long — so an
unrelated instantiation elsewhere never renames this one) with provenance
`from f (τ̄)`; requesting spines rewrite to the clone with the type
arguments erased; the clone's own body is scanned for further requests.
Polymorphic definitions are templates and are dropped. The worklist runs
in generations, bounded by the instantiation budget (`--depth`): only an
instantiation *chain* deeper than the budget — poly recursion — is
rejected; fan-out is unbounded.

**INLINE inlining** (mono; before further lowering) replaces every
occurrence of an `inline`-attributed definition with its body as a lambda
telescope over its parameters (application sites become beta redexes for
the partial evaluator), refreshing every inserted copy. Mutual recursion
among `inline` definitions is rejected. Inlining runs after
specialization: substituting under a type-argument spine would strand the
arguments on a non-variable head.

## 9. Concrete syntax (.eir)

One grammar spans both levels; the M-level productions (§7.1) parse today
and are populated by later stages. Line comments are `--`. The format
round-trips: `parse ∘ pretty` is the identity on programs modulo
annotations, and `pretty ∘ parse ∘ pretty ≡ pretty` is a tested fixpoint.

Names print with their uniques: `x#12`, `Main.loop#3`, type variables
`a#7`. Type/data constructor names print bare (`Main.CPUState`, `Vec`).
Primitives print by their builtin name (`rwPrimBind`); the `rwPrim` prefix
and absence of `#` distinguishes them lexically from constructors and
variables.

    prog  ::= data* defn* proc* 'top' var
    data  ::= 'data' T kind '{' (ctor (';' ctor)*)? '}'
    ctor  ::= C '::' sig
    kind  ::= '*' | 'Nat' | kind '->' kind | '(' kind ')'
    sig   ::= ('forall' tyvarb+ '.')? ty
    tyvarb::= '(' a '::' kind ')'
    ty    ::= ty '->' ty                  (right associative)
            | ty tyatom
            | natop tyatom tyatom+          (prefix arithmetic application;
                                             the printer's canonical form)
            | tyatom
    tyatom::= T | a | nat | '()' | '[]' | '[_]'
            | '(' ty ')' | '(' ty natop ty ')'
    natop ::= '+' | '-' | '*'            (the built-in arithmetic constructors,
                                          §3.1; the parenthesized infix form is
                                          input sugar for the prefix form, and an
                                          unapplied operator is rejected)

`()` (written tightly) is the unit type constructor (name `"()"`); tuple
type constructors are likewise the usual `"(,)"`-family names, and `[]`
and `[_]` the list type constructors, applied via `tyatom` spines.

    defn  ::= var '::' sig NL attrs? var param* '=' exp
    attrs ::= ('inline' | 'noinline')?
              ( 'from' name '(' (ty (',' ty)*)? ')'
              | 'baked' name )?
    param ::= '(' var '::' ty ')'

    exp   ::= '\\' param+ '->' exp
            | 'let' bind 'in' exp
            | 'case' exp 'of' var '{' alt (';' alt)* '}' '::' ty
            | 'jump' var '(' (exp (',' exp)*)? ')'
            | app
    app   ::= app arg | atom
    arg   ::= atom | '@' tyatom
    atom  ::= var | lit
            | '(' C '::' ty ')' | '(' p '::' ty ')' | '(' int '::' ty ')'
            | strlit
            | '(' 'list' '[' exps? ']' '::' ty ')'
            | '(' 'vec'  '[' exps? ']' '::' ty ')'
            | '(' exp ')'
    bind  ::= var '::' ty '=' exp
            | 'rec' '{' (var '::' ty '=' exp) (';' …)* '}'
            | 'join' var '(' param,* ')' '=' exp
    alt   ::= '_' '->' exp
            | C param* '->' exp
            | int '->' exp

    proc  ::= 'proc' P ':' ty '~>' ty clock? '{' state* entryb block* '}'
    state ::= 'state' s ':' ty ':=' ('undef' | exp) ';'
    entryb::= 'entry' '{' cmds term '}'
    block ::= 'block' var '(' param,* ')' '{' cmds term '}'
    cmd   ::= var '::' ty '<-' exp ';'
            | var '::' ty '<-' 'get' s ';'
            | 'put' s atom ';'
    term  ::= 'pause' atom '->' var '(' atom,* ')'
            | 'goto' var '(' atom,* ')'
            | 'halt' atom
            | 'case' atom 'of' '{' talt (';' talt)* '}'
    talt  ::= '_' '->' term
            | C param* '->' term
            | int '->' term
    clock ::= '@' 'clock' name

In machine positions, `atom` extends to any parenthesized expression (the
printer parenthesizes non-atom forms); labels print like variables
(`L#12`) and carry no signature. Terminator labels may reference blocks
declared later in the same process.

## 10. Correspondence to the implementation

| Spec section | Module |
|---|---|
| §3 (P syntax) | `ReWire.Eidos.Syntax` |
| §5 (`typeOf`, nats, spines) | `ReWire.Eidos.Types` |
| §4 (lint modes poly/mono) | `ReWire.Eidos.Lint` |
| §7.6 (builtin signatures) | `ReWire.Eidos.BuiltinSigs` |
| §2/§8 (uniqueness, the refreshing clone, substitution) | `ReWire.Eidos.Subst` |
| §8 (specialization) | `ReWire.Eidos.Spec` (types), `ReWire.Eidos.Simplify` (values) |
| §8 (INLINE inlining) | `ReWire.Eidos.Inline` |
| §8 (partial evaluation, LiftNonRep, purge) | `ReWire.Eidos.Simplify` |
| extern neutering | `ReWire.Eidos.Externs` |
| minted-name conventions | `ReWire.Eidos.Naming` |
| the bridge (GHC Core → P) | `ReWire.GHC.ToEidos` |
| §7.3 (the machine fold, M → Hyle) | `ReWire.Eidos.ToHyle` |
| §9 (`.eir`, both levels) | `ReWire.Eidos.Pretty`, `ReWire.Eidos.Parse` |
| §7.1 (proc syntax) | `ReWire.Eidos.Syntax` |
| §7.4 (machine rules, lint machine mode) | `ReWire.Eidos.Lint` |
| §6 (ANF, reactive fragment) | `ReWire.Eidos.ANF` |
| §7.3 (procify) | `ReWire.Eidos.Procify`; cleanups in `ReWire.Eidos.ProcOpt` |

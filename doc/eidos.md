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

  *Status:* the M level is normative for the `procify` pass and is
  implemented in a later stage of the Eidos migration than the P level; its
  productions are reserved in the concrete syntax from day one.

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

Non-goals at this writing (deliberately deferred, with their seams named):
multi-clock semantics (procs carry an optional clock-domain annotation,
default the single implicit domain); multiple processes and process
composition operators (the program grammar admits a proc list and a `top`
designation; the singleton restriction is a removable lint rule,
`single-top-proc`, not a grammar or parser rule); memory primitives.

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
reads instantiations directly off spines and `typeOf` is total (§5.2).

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
    attrs ::= (inline | noinline)? (from f₀ (τ̄))?

A definition's parameter telescope matches a prefix of its signature's
arrow spine (arity is structural). `inline`/`noinline` carry over GHC
pragmas; `from f₀ (τ̄)` is *provenance* — the origin definition and type
instantiation of a specializer-minted clone, carried for dumps, error
messages, and stable generated-HDL naming.

### 3.6 Datatypes

    data ::= data T κ { C₁ :: sig₁; … }

Constructor signatures quantify the datatype's parameters:
`C :: ∀ ā. τ₁ → … → τ_k → T ā`. Datatypes stay parametric through
specialization (only definitions are cloned); consumers size constructor
applications at use sites by first-order matching.

### 3.7 Programs

    prog ::= data* defn* proc* top f

A program designates one *device root* with `top`. The `proc` productions
belong to the M level (§7); at the P level the list is empty. The
restriction that the reactive root set is the singleton `{top}` is a
named, removable lint rule (`single-top-proc`) — enforced neither by the
grammar nor by the parser — reserving the seam for process composition.

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
  type assumptions riding to the retained pipeline as polymorphic signature
  carriers (error-stub bodies, never referenced as variables — references
  become `Prim` occurrences at the bridge), and they check in poly mode
  until an Eidos-level builtin signature table replaces them.
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

    e ::= let x :: τ = r in e  |  ret a
    r ::= a  |  x a₁ … a_k  |  C ā  |  p ā  |  case a of x { alt; … } :: τ
        | proj_i a  |  xcall …            (M-level forms, §7)
    a ::= x  |  lit :: τ                  atoms

with reactive primitives (`rwPrimBind`, `rwPrimSignal`, `rwPrimGet`, …)
still permitted in `r` — that is what procify consumes and eliminates.
Join points survive into this form (their bodies are ANF like any other);
`ret a` marks a tail. The normalization from full P to mono+ANF is a small
ordered ruleset (eta-expansion, argument- and subject-naming, alternative
flattening, let-flattening, top-level-lambda restoration).

## 7. Eidos-M: the process calculus

*Normative target of the `procify` pass; implemented in a later migration
stage. This section fixes the grammar, static rules, and machine-step
contract so that everything upstream (naming, `.eir`, lint modes) is
forward-compatible.*

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
           | case a of { C x̄ → term; …; _ → term }

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
- a `halted` flag bit if `halt` is reachable, occupying the position of
  the legacy `PuRe` tag (post-halt outputs are unspecified, as today; a
  strict lint mode rejects reachable halt).

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
- All rules are stated per-proc.

### 7.5 Machine semantics

A proc with cells `s̄ : τ̄`, pause-target labels `L̄`, and step fold
`step : Label × args × cells × τ_I → out × Label × args × cells` denotes
the causal stream function obtained by iterating `step` from the initial
label and cell values (§7.3), exactly the device semantics of Hyle
(doc/hyle.md §6.4): output at time t depends on inputs strictly before t.
The translation to a Hyle device (registers = cells ⧺ label ⧺ args;
equations = the step fold's ANF, mapped 1:1 onto Hyle lets and muxes) is
semantics-preserving by construction, and the four-way cosimulation check
in `rwc-test` tests exactly this.

## 8. Pass discipline

Every pass over Eidos:

1. preserves binder uniqueness (duplication only through the refreshing
   clone primitive);
2. preserves lets and join points (sharing; G4) — dead-binding removal and
   the occurrence-driven simplifier are the sanctioned exceptions;
3. leaves the program in a lint mode at least as strong as its input's
   (the pipeline is monotone through poly → mono → mono+ANF → machine);
4. is annotation-transparent (annotations propagate; never compared).

`--debug-lint` re-lints between passes in the mode of the pipeline
position, including after purification (machine mode) — the predecessor
pipeline's back half was uncheckable by construction.

**Specialization** (poly → mono) is a worklist over instantiation
requests. A request is a spine `f @τ̄ …` in a monomorphic body whose head
is a polymorphic definition; since local binders are monomorphic and type
arguments saturate quantifier lists (§4.2), every request's `τ̄` is closed.
Each new `(f, natNorm τ̄)` mints a clone by pure type substitution through
the refreshing clone primitive, named `f$i` (per-origin counter, discovery
order) with provenance `from f (τ̄)`; requesting spines rewrite to the
clone with the type arguments erased; the clone's own body is scanned for
further requests. Polymorphic definitions are templates and are dropped.
The worklist runs in generations, bounded by the instantiation budget
(`--depth`): only an instantiation *chain* deeper than the budget — poly
recursion — is rejected; fan-out is unbounded.

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
    attrs ::= ('inline' | 'noinline')? ('from' name '(' (ty (',' ty)*)? ')')?
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
    clock ::= '@' 'clock' name

## 10. Design rationale

**Why labels instead of named definitions for states.** The predecessor
(Crust) minted one resumption state per *definition name* satisfying an
invariant established by five passes in a fixed order; equal continuations
minted distinct states, repaired after the fact by alpha-merging. Labels
make state identity a structural property with a namespace, a checker, and
a deterministic naming scheme — and make FSM-level transformations
(block merging, epsilon elimination, eventually equivalent-state merging)
well-defined program transformations instead of heuristics over names.

**Why no inference.** GHC has already typechecked the program, solved all
class and type-nat constraints, and recorded every instantiation as a type
application. Re-inferring (as Crust did, with a 499-line HM engine whose
unifier hard-coded a whole-device single-ReacT assumption) recomputes
worse what the input already says. Substitution + lint keeps every type
fact local and every failure located.

**Why plain-data types and explicit uniques.** The predecessor's
binder-library representation made free-variable computation quadratic,
made structural equality unsound (hash-based), taxed every traversal with
annotation binders, and required a deep-force between passes. Uniques are
the representation GHC and Clash both converged on; the price — capture
discipline by invariant rather than by construction — is paid once, in one
clone primitive and one lint rule.

**Why the process calculus is worth a second level.** Purification is
ReWire's essential transformation; no comparable compiler has one (Clash
rejects recursion; FSM DSLs make states explicit in the source). Giving
its output a first-class calculus turns the compiler's hardest contract
(the purify input invariants) into a grammar, makes the whenchain class of
divergence a static error (guardedness), deletes the layout
reverse-engineering in the Hyle translation (the step record is declared,
not inferred), and reserves clean seams for multiple processes and
pipeline composition operators — the singular-ReacT restriction becomes
one removable lint rule instead of an axiom of the unifier.

## 11. Correspondence to the implementation

| Spec section | Module |
|---|---|
| §3 (P syntax) | `ReWire.Eidos.Syntax` |
| §5 (`typeOf`, nats, spines) | `ReWire.Eidos.Types` |
| §4 (lint modes poly/mono) | `ReWire.Eidos.Lint` |
| §2/§8 (uniqueness, the refreshing clone, substitution) | `ReWire.Eidos.Subst` |
| §8 (specialization) | `ReWire.Eidos.Spec` (types), `ReWire.Eidos.Simplify` (values) |
| §8 (INLINE inlining) | `ReWire.Eidos.Inline` |
| §8 (partial evaluation, LiftNonRep, purge) | `ReWire.Eidos.Simplify` |
| extern neutering | `ReWire.Eidos.Externs` |
| minted-name conventions | `ReWire.Eidos.Naming` |
| the bridge (GHC Core → P) | `ReWire.GHC.ToEidos` |
| the retained-pipeline shim (P → Crust; dies with stage 2) | `ReWire.Eidos.ToCrust` |
| §9 (`.eir`) | `ReWire.Eidos.Pretty`, `ReWire.Eidos.Parse` |
| §6 (ANF) | the P→M lowering (later stage) |
| §7 (procs, machine fold) | `procify` and successors (later stage) |

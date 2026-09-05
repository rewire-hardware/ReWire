# Eidos: the ReWire typed intermediate representation

This document specifies the syntax and static discipline of *Eidos*, the
typed functional IR between GHC Core and Synolon (doc/synolon.md, the
machine-level IR), implemented in `ReWire.Eidos.*`.

Eidos is the IR of the compiler's front-end passes; the pipeline is

    GHC Core  →  Eidos  →  (specialization, partial evaluation, ANF)  →  Synolon  →  Hyle

*Eidos* is the functional program the machine will compute, *Synolon* the
particular machine — states, transitions, data path — that program becomes, and
*Hyle* is that machine realized in bits. The producer (`ReWire.GHC.ToEidos`)
and the consumer (purification, `ReWire.Eidos.ToSynolon`, which consumes the
mono+ANF restriction of §6 and produces Synolon) are outside the scope of this
document: everything below treats Eidos programs as given, whether produced by
the compiler or written by hand (`.eir` files, §9).

Eidos is a System-F-lite mirror of GHC Core — polymorphic definitions,
recursive lets, join points, n-ary case. The bridge targets it; specialization,
dictionary elimination, and normalization happen here.  Class dictionaries
arrive from the bridge as ordinary data: a class's dictionary is a datatype
whose constructor fields are its superclass dictionaries and methods
(single-method classes have *newtype* dictionaries in GHC, and the bridge
unwraps their types to the method type, so no datatype exists for them here),
instance definitions are inline-annotated definitions of those values, and
method calls are single-alternative case projections. A dictionary-typed
binding is never shared: no dictionary is representable, so the simplifier
substitutes it at every occurrence regardless of use count (where a
representable multi-use binding keeps its let). The partial-evaluation fixpoint
*requires* dictionary-freedom — a program whose dictionaries cannot be resolved
statically fails compilation — so no class construct ever reaches Synolon or
Hyle.

The machine level — the process calculus of state cells, labeled blocks, and
`pause`/`goto`/`halt` terminators that purification produces from the mono+ANF
fragment — is Synolon's, specified in doc/synolon.md; it embeds Eidos's
expression, definition, and datatype syntax.

## 1. Design goals

- **G1 — One machine story.** The temporal structure of a program is
  expressed by a dedicated calculus with its own checker — Synolon
  (doc/synolon.md, G1) — not by conventions among passes over this
  functional IR; Eidos's job ends at the mono+ANF form purification
  consumes (§6).
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
  mono+ANF) corresponding to the pipeline's invariant stages, so "pass X
  runs after pass Y" contracts are grammar- and lint-enforced, not
  pass-order-enforced; the machine stage has its own lint (doc/synolon.md
  §4).
- **G6 — A textual format from day one.** Eidos has a concrete syntax
  (`.eir`, §9) with a parser and a round-trip property, enabling golden
  tests at the pass boundaries and hand-written test inputs, as Synolon's
  `.syn` and Hyle's `.rwc` do downstream.

Non-goals at this writing: memory primitives; the machine-level non-goals
(multi-clock semantics, process composition) are Synolon's.

## 2. Metavariables and annotations

Metavariables: `x`, `y` range over term names; `a` over type variables;
`L` over join point labels; `T` over type constructor names;
`C` over data constructor names; `f` over definition names;
`u` over uniques (naturals); `n`, `k` over type-level naturals;
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
variables in signatures — has a unique distinct from every
other binding site's. Occurrences share the unique (and signature) of
their binder. The bridge establishes the invariant; passes preserve it;
any operation that duplicates a term refreshes the copies' binders through
one shared primitive. The linter's uniqueness rule re-checks it globally.

## 3. Abstract syntax

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
shared Synolon block (doc/synolon.md §3.4), i.e. one resumption state
instead of one per reference.

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

    prog ::= data* defn* top f

A program designates one *device root* with `top`; purification consumes
it into a Synolon process (doc/synolon.md §3.6).

## 4. Static semantics

### 4.1 Contexts and modes

The linter checks a program in one of three cumulative modes:

- **poly** (post-bridge): the rules of §4.2–§4.4.
- **mono** (post-specialization): additionally, every *definition* signature
  has `g = 0` (constructor signatures stay parametric, §3.6, and are
  covered at occurrences, which carry instantiated types) and every type is
  nat-closed. Value binders may still be higher-order and the type grammar
  is still open here: specialization eliminates polymorphism, not functions
  — first-orderization is the partial evaluator's job, which runs after it.
  Builtin-named definitions (`rwPrim*`) are exempt: they are the builtins'
  type assumptions riding through the Eidos passes as polymorphic signature
  carriers (error-stub bodies, never referenced as variables — references
  become `Prim` occurrences at the bridge), and they check in poly mode.
  (`Prim` occurrences themselves are checked against the builtin signature
  table of §7, in every mode.)
- **mono+ANF** (purification's input contract): additionally, value
  binders are first-order and the type grammar is restricted to the
  *representable closure* of doc/synolon.md §3.1 — `Vec n τ`, `Finite n`,
  `Bool`, `()`, tuples, monomorphic ADTs, `Integer`, `Proxy n`, `String`
  (literal positions only), with arrows only in definition signatures —
  plus the reactive types (`ReacT`, `StateT`, `Identity`), which are
  permitted *only* until purification — plus the ANF restriction of §6.

The machine stage that follows has its own checker (doc/synolon.md §4),
under which the reactive types are out of the grammar entirely.

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
policy is the machine level's concern, doc/synolon.md §7). Data constructor signatures
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
is an explicit operation used at specialization memo-keys and at Synolon's
block merging (doc/synolon.md §8); hash-based or structural term equality is deliberately not
offered (the predecessor IR's hash-equality instance was a live collision
unsoundness).

**Dynamic semantics.** Eidos is a pure, total fragment (general recursion
is confined to the reactive layer, where productivity is later enforced by
guardedness, doc/synolon.md §4): its programs mean what the same programs
mean in GHC Core under call-by-value evaluation of the reachable
first-order fragment; the observable semantics of a whole program is
defined by the Synolon program it compiles to (doc/synolon.md §5) and,
transitively, by Hyle's denotational semantics (doc/hyle.md §6). Eidos is a
*transformation* level; no interpreter for it is provided or needed —
behavioral oracles run at the Hyle level.

## 6. The mono+ANF restriction (purification's input)

The ANF productions are shared with the full grammar (they are a
restriction, not a new syntax): in mono+ANF mode every *reactive*
definition body is (pure bodies are exempt — the fold lowers them in any
shape)

    e ::= let x :: τ = r in e  |  ret a  |  jump L (ā)
    r ::= a  |  x ᾱ  |  C ᾱ  |  p ᾱ  |  case a of x { alt; … } :: τ
    α ::= a  |  p ᾱ  |  λ (x :: τ) → e  |  f ᾱ  |  C  |  p
                                          arguments: atoms, primitive
                                          expressions, lambdas, function-typed
                                          partial applications, and bare
                                          constructor or operator references
    a ::= x  |  lit :: τ  |  C :: τ  |  p :: τ  |  "…"  |  list [ā] :: τ  |  vec [ā] :: τ
                                          atoms (variables, literals, and
                                          nullary constructor and primitive
                                          occurrences)

Join points survive into this form (their bodies are ANF like any other;
jumps are tails with atom arguments); `ret a` marks a tail. The
normalization from the full grammar to mono+ANF is a small ordered
ruleset (eta-expansion to signature arity with parameters in the
telescope, argument- and subject-naming, alternative flattening,
let-flattening, and head reduction: a residual beta-redex — a lambda head
applied to arguments, which the partial evaluator's single round can
leave — becomes a let). Primitive applications are *transparent*: a
primitive-headed argument is normalized in place rather than named, so
the pure data path stays one expression tree of primitives over atoms —
the fold lowers it inline (a literal idiom such as `resize (bits n)`
compositionally, folding downstream in Hyle) and matches its static
idioms (a bit-slice or bit-index position is a `finite` literal) where
they stand. Two more argument forms normalize in place: lambdas (a
higher-order builtin's function argument, its body a tail) and
function-typed partial applications, along with bare constructor and
operator references; any other function-typed argument (a function
chosen by a case) is rejected here, since no consumer can lower it.
doc/synolon.md §3.2 states the resulting block normal form, which the
Synolon lint requires of every block.

The *reactive fragment is exempt from naming* — it is purification's
input skeleton, and its structure must survive: a spine whose type
mentions the reactive stack (`rwPrimBind`, `rwPrimSignal`, `rwPrimGet`, …)
stays a spine, keeping its lambda (continuation) arguments in place with
A-normalized bodies and its reactive arguments in place (a pure let may
wrap them), while its pure non-atom arguments are named; and a case with
a reactive result type stays in tail position (scrutinee named,
alternatives A-normalized) — purification turns it into a terminator
case. A case whose alternatives jump (the scope of a join point) likewise
stays in tail position, since jumps are tail-only. Any other pure-resulted
case or call is named like any other computation.

## 7. Builtin signatures

The normative signature scheme of every builtin (`ReWire.Builtins`;
occurrences print as `rwPrim<Name>`, §9). Signatures are what `Prim`
occurrences must instantiate: an occurrence's carried type must be a
substitution instance of its builtin's scheme (this is the linter's builtin
signature table, `ReWire.Eidos.BuiltinSigs`, which the Synolon lint shares;
the `rwPrimExtern` row is the one exception — its occurrence types are
trusted, not checked). Quantified `n, m, i` have kind `Nat`; `a, b` kind
`*`; `m̂` ranges over the reactive stack.

The schemes of every builtin that survives purification are the signature
column of doc/synolon.md §6, which also gives their machine-level
denotations. The seven reactive operations do not survive — purification
consumes them, and none may appear in a process or in any definition
reachable from one:

| builtin | signature |
|---|---|
| `rwPrimBind` | `∀ m̂ a b. m̂ a → (a → m̂ b) → m̂ b` |
| `rwPrimReturn` | `∀ m̂ a. a → m̂ a` |
| `rwPrimGet` | `∀ s m̂. StateT s m̂ s` |
| `rwPrimPut` | `∀ s m̂. s → StateT s m̂ ()` |
| `rwPrimSignal` | `∀ i o m̂. o → ReacT i o m̂ i` |
| `rwPrimLift` | `∀ t̂ m̂ a. m̂ a → t̂ m̂ a` |
| `rwPrimExtrude` | `∀ i o s m̂ a. ReacT i o (StateT s m̂) a → s → ReacT i o m̂ a` |

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
   (the pipeline is monotone through poly → mono → mono+ANF, and then
   Synolon);
4. is annotation-transparent (annotations propagate; never compared).

`--debug-lint` fills the gaps the standing lints leave, so that every
Eidos pass is followed by a lint in the mode of its pipeline position
(and every Synolon pass by the Synolon lint, doc/synolon.md §8).

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

**Partial evaluation** (mono; the `--depth`-bounded fixpoint) alternates
value specialization, purge, and a let-preserving reduction until every
definition's signature is representable and no signature mentions a
dictionary datatype (one with a function-typed constructor field). The
reduction turns beta redexes into lets and inlines a let only when its
binder is dead, used once, or bound to an atom — with two policy
exceptions: a multi-use *function-typed* binding is lifted to a top-level
definition over its captured locals (LiftNonRep; the `$LL.` prefix), and
a *dictionary-typed* binding is substituted at every occurrence (it can
never be represented, so there is no sharing to preserve — this is what
lets known-constructor case selection consume multi-use superclass
projections). Known-constructor and known-literal cases select their
alternative; top-level references are never unfolded — higher-orderness
dies by *argument baking* instead: a call to a top-level definition with
closed arguments (free variables all top-level) bakes those arguments
into a memoized clone (provenance `baked f`), dropping the baked
parameters from its signature. Definitions unreachable from the device
root (plus the builtin signature carriers) are purged each round; a
program that fails to converge within `--depth` rounds is rejected,
listing the definitions that held up the fixpoint.

## 9. Concrete syntax (.eir)

Line comments are `--`. The lexer (`ReWire.Eidos.Lexer`) and its
reserved-word table are shared with Synolon's `.syn` (doc/synolon.md §9),
so the machine level's keywords (`proc`, `block`, `put`, …) are reserved
here too and print quoted when they occur as names. The format
round-trips: `parse ∘ pretty` is the identity on programs modulo
annotations, and `pretty ∘ parse ∘ pretty ≡ pretty` is a tested fixpoint.

Names print with their uniques: `x#12`, `Main.loop#3`, type variables
`a#7`. Type/data constructor names print bare (`Main.CPUState`, `Vec`).
Primitives print by their builtin name (`rwPrimBind`); the `rwPrim` prefix
and absence of `#` distinguishes them lexically from constructors and
variables. Occurrence text that does not lex as an identifier prints
backtick-quoted (`` `Main.C:Frob` ``, `` `GHC.Classes.&&`#5 ``) and the
parser accepts the quoted form anywhere a name is expected; string
literals use the `\\`, `\"`, `\n`, `\t`, `\r` escapes.

    prog  ::= data* defn* 'top' var
    data  ::= 'data' T kind '{' (ctor (';' ctor)*)? '}'
    ctor  ::= C '::' sig
    kind  ::= '*' | 'Nat' | kind '->' kind | '(' kind ')'
    sig   ::= ('forall' tyvarb+ '.')? ty
    tyvarb::= '(' a '::' kind ')'
    ty    ::= ty '->' ty                  (right associative)
            | ty tyatom
            | natop tyatom+                 (prefix arithmetic application;
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
    atom  ::= var
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

## 10. Correspondence to the implementation

| Spec section | Module |
|---|---|
| §3 (P syntax) | `ReWire.Eidos.Syntax` |
| §5 (`typeOf`, nats, spines) | `ReWire.Eidos.Types` |
| §4 (lint modes poly/mono/mono+ANF) | `ReWire.Eidos.Lint` |
| §7 (builtin signatures) | `ReWire.Eidos.BuiltinSigs` |
| §2/§8 (uniqueness, the refreshing clone, substitution) | `ReWire.Eidos.Subst` |
| §8 (specialization) | `ReWire.Eidos.Spec` (types), `ReWire.Eidos.Simplify` (values) |
| §8 (INLINE inlining) | `ReWire.Eidos.Inline` |
| §8 (partial evaluation, LiftNonRep, purge) | `ReWire.Eidos.Simplify` |
| extern neutering | `ReWire.Eidos.Externs` |
| minted-name conventions | `ReWire.Eidos.Naming` |
| the primitive datatype basis | `ReWire.Eidos.PrimBasis` |
| the bridge (GHC Core → Eidos) | `ReWire.GHC.ToEidos` |
| §9 (`.eir`) | `ReWire.Eidos.Pretty`, `ReWire.Eidos.Parse`, `ReWire.Eidos.Lexer` |
| §6 (ANF, reactive fragment) | `ReWire.Eidos.ANF` |
| purification (Eidos → Synolon; doc/synolon.md §8) | `ReWire.Eidos.ToSynolon` |

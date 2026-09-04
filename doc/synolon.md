# Synolon: the ReWire machine-level intermediate representation

This document specifies the syntax, static discipline, and machine
semantics of *Synolon*, the machine-level IR between Eidos and Hyle,
implemented in `ReWire.Synolon.*`. The pipeline is

    GHC Core  →  Eidos  →  Synolon  →  Hyle

*Synolon* (σύνολον, the composite whole) completes the pair of *Eidos*
(εἶδος, form) and *Hyle* (ὕλη, matter): it is Aristotle's name for the
concrete individual in which form has been taken up by matter. The name
marks the point at which the program stops being a description and becomes
a particular machine — its states, transitions, and data path fixed — that
Hyle then realizes in bits.

A Synolon program is produced by *purification* (`ReWire.Eidos.ToSynolon`)
from the mono+ANF restriction of an Eidos program (doc/eidos.md §6), and
consumed by the fold to Hyle (`ReWire.Synolon.ToHyle`, §7). Its expression
language is Eidos's: command right-hand sides, cell initials, and
terminator operands are Eidos expressions, and the definitions a machine
calls are Eidos definitions — the machine level restricts what may appear
(§3, §4), it does not introduce a second expression syntax. Producer and
consumer are outside the scope of this document: everything below treats
Synolon programs as given, whether produced by the compiler (`rwc
--synolon` writes the program after the block-graph cleanup as `<out>.syn`,
which is also the `--certify` source artifact) or written by hand (`.syn`
files, §9).

## 1. Design goals

- **G1 — One machine story.** The temporal structure of a program — where
  it pauses, what it saves, what it steps — is expressed by a dedicated
  calculus with its own checker, not by conventions among passes over a
  functional IR. A resumption state is a *label*; label identity is
  structural. Duplicate states arising from lost sharing are unrepresentable;
  duplicate states arising from inlining are mergeable by construction.
- **G2 — Scoped uniqueness.** The datatype and definition fragment keeps
  Eidos's global binder uniqueness (doc/eidos.md §1, G2); process binding
  sites are scoped — labels distinct per process, sites distinct per block
  — because purification legitimately binds one unique in several blocks
  (§4).
- **G3 — Types are plain data.** Inherited from Eidos: no binders inside
  types, one mandatory type per binder, a total `typeOf`; here further
  restricted to monomorphic, nat-closed, representable types (§3.1).
- **G4 — One unconditional checker.** The machine rules (§4) are checked
  by one lint that the pipeline runs on every program after the
  block-graph cleanup, before the dump `--certify` binds and before the
  fold.
- **G5 — A textual format.** Synolon has a concrete syntax (`.syn`, §9)
  with a parser and a round-trip property; `rwc-test` round-trips the
  machine dump of every golden program through it, and hand-written
  fixtures under `tests/synolon/` exercise the checker.

Non-goals at this writing: multi-clock semantics (procs carry an optional
clock-domain annotation, default the single implicit domain); multiple
processes and process composition operators (the program grammar admits a
proc list; the singleton restriction is only a property of what purify
mints, not a grammar, parser, or lint rule); memory primitives.

## 2. Metavariables and annotations

The metavariables, names, uniques, and annotations of doc/eidos.md §2
carry over: `x`, `y` range over term names, `a` over type variables, `T`
and `C` over type and data constructor names, `f` over definition names,
`u` over uniques, `n`, `k` over type-level naturals, `τ`, `σ` over types,
`e` over expressions. In addition, `P` ranges over process names and `s`
over state cells — both plain text in their own per-program and per-process
namespaces, without uniques — `L` over block labels (which are names in the
unique discipline: `occ#u`), and `ℓ` over the labels that are pause targets,
i.e. the machine states.

## 3. Abstract syntax

### 3.1 Types and signatures

Types and signatures are Eidos's (doc/eidos.md §3.1–§3.2), restricted:
every definition signature is monomorphic (`g = 0`), every type is
nat-closed, and every value binder, block parameter, and state cell has a
type in the *representable closure*:

- `Vec n τ` for representable `τ`; `Finite n` (`Finite 0` is uninhabited,
  §5.1); `Bool`; `()`; tuples; monomorphic instances of declared, non-
  recursive datatypes; `Integer` (at the fixed width 128); `Proxy n`;
  `String`, in literal positions only.
- Arrows occur only in definition signatures (a definition may take and
  return representable values); no binder, parameter, or cell has a
  function type.
- The reactive types (`ReacT`, `StateT`, `Identity`) are out of the type
  grammar entirely: purification retired them.

This closure is the type universe the machine semantics is defined over
(§5.1). The lint enforces it: the exclusion of the reactive types,
monomorphism, the first-order binder rule, and representability at a
fixed bit width — every binder, block parameter, cell, port, and halt
answer sizes (`ReWire.Synolon.Repr`, the one sizing the fold also lays
values out by, so the two agree on which types have a width; a type in a
position only the fold sizes — a definition's codomain, a primitive's
instantiation — gets the same diagnostic there). The mechanized checker
(`verify/Rwv/Synolon/Check.lean`) enforces the same rule.

### 3.2 Pure expressions

The expressions inside a Synolon program — command right-hand sides, cell
initials, terminator operands, and the bodies of the definitions the
machine calls — are Eidos expressions (doc/eidos.md §3.3–§3.4) without type
arguments (every head is monomorphic), without `rec` bindings reachable
from a process (§4, pure-acyclicity), and without reactive types. Lambdas
occur, with first-order binders, only as function arguments to the
higher-order builtins; join points and jumps occur only in definition
bodies, never in blocks.

What a block holds is in *block normal form*: command right-hand sides
and cell initials are simple computations `r`, and terminator operands
and put payloads are operands `o`:

    r ::= a  |  x ᾱ  |  C ᾱ  |  π  |  case a of x { alt; … } :: τ
    π ::= p ᾱ                                   primitive expression
    α ::= a  |  π  |  λ (x :: τ) → e  |  f ᾱ  |  C  |  p
                                                argument: an atom, a primitive
                                                expression, a lambda (its body
                                                a tail), a function-typed
                                                partial application, or a bare
                                                constructor or operator
    o ::= a  |  π                               operand
    a ::= x  |  lit :: τ  |  C :: τ  |  p :: τ  |  "…"  |  list [ā] :: τ  |  vec [ā] :: τ

Primitive expressions are *transparent*: a primitive applied to arguments
is not named but nests freely, so the pure data path of a block — bit
arithmetic, slices, resizes, the literal idioms `resize (bits n)` and
`finite n` — is one expression tree of primitives over atoms, which the
fold lowers inline (and whose static idioms, a bit-slice index being a
`finite` literal, it matches where they stand). Every other computation
is named, one command each: a definition call, a constructor application,
a case (over an atom, with alternatives that are let chains over `r`
ending in an atom). A lambda or a function-typed partial application
occurs only as an argument (a higher-order builtin's function argument),
kept in place with its body or arguments in this form; an application's
head is a definition, constructor, or primitive, never a lambda. This is
the shape the A-normalization of doc/eidos.md §6 establishes for the
reactive fragment; purification carries it into blocks, the cleanup
transforms preserve it (epsilon-block inlining substitutes operands for
block parameters, and takes a hop only where the result is still in the
form — a parameter may sit where only an atom may, under a lambda or in a
literal — otherwise the block stays), and the lint requires it (§4). The abstract syntax admits any pure expression in these
positions and the printer parenthesizes a non-atom operand (§9), so a
hand-written `.syn` file can violate the form — and be rejected.

### 3.3 Definitions and datatypes

Definitions and datatypes are Eidos's (doc/eidos.md §3.5–§3.6). A Synolon
program carries the definitions the machine calls — pure, monomorphic,
first-order (`ReWire.Eidos.Types.machineDefn`: dotted-named, no quantified
type variables, not reactive-typed), in the order purification found
them (Hyle names are assigned by position) — with their provenance
attributes (`from f₀ (τ̄)`, `baked f₀`) riding along for dumps and stable
HDL naming. The consumed reactive definitions and the builtin signature
carriers do not ride along.

Datatypes stay parametric (constructor signatures quantify the datatype's
parameters); consumers size constructor applications at use sites. A
program declares exactly the datatypes it mentions: those named by some
type in its definitions or processes (a constructor occurrence carries
its instantiated type, a case alternative names its constructor; a
definition's provenance attribute does not count), closed under the
field types of their constructors. The rest of an Eidos
program's declarations do not ride along — the reactive stack `ReacT`,
`StateT`, `Identity`, retired with the reactive fragment (no Synolon type
may mention them, §3.1), and whatever of the primitive basis and the
tuple family the program never uses. The certify validator re-adds the
absent basis declarations itself; its basis gate rejects only a basis
declaration that is present and differs from the canonical one.

### 3.4 Processes

    proc ::= proc P : τ_I ~> τ_O clock? {
                 state s₁ : τ₁ := e₀₁ ;  …          cells: one per state layer
                 entry { cmds ; term }               the reset block (unlabeled)
                 block L₁ (x₁:τ, …, inp:τ_I) { cmds ; term }
                 …
             }

    cmd  ::= x :: τ ← e                    pure computation (a pure expression, §3.2)
           | x :: τ ← get s                cell read
           | put s e                       cell write
    term ::= pause e → L (e₁, …)           emit e : τ_O; resume at L next cycle
           | goto L (e₁, …)                intra-cycle transfer (saturated)
           | halt e                        terminate with answer e
           | case e of { _ → term; C x̄ → term; …; lit → term; … }
                                           (default first, as in an expression case)

A block's *last* parameter is the resumed input (type `τ_I`); a
`pause a → L(ā)` supplies all of `L`'s parameters *except* that one, which
the machine supplies on resumption. A block may be the target of both
`pause` and `goto` (a `goto` supplies all parameters). The `entry` block is
parameterless and unlabeled (no `goto` can target it) and
holds the reset prefix; cells it writes before the first pause become
register initials (§7).

Cells are named and per-proc: `get`/`put` target a cell by name — there is
no state-stack indexing and no cross-proc state. Cell initials `e₀` are
closed pure expressions, compile-time evaluated (consulting combinational
extern models; a model-less extern in an initial is a located error), or
`undef` for a cell first written before any read on every path from entry.
An `undef` initial *denotes* the zero value of the cell's type (`zero_τ`,
§5.1) — the write-before-read convention makes the choice unobservable
in intended use, and pinning it keeps the semantics total and matches the
compiled machine bit-for-bit.

Externs: combinational extern calls (`xcall`) are ordinary command
right-hand sides; *sequential* (clocked) extern calls are legal only as
commands, and each syntactic occurrence denotes one device instance.

Labels are a distinct namespace, per-proc; all generated names (label
enum, step record, cells) are qualified by the proc name (purify names
the single proc `main`).

### 3.5 Degenerate forms

Zero-cell, single-label procs (combinational devices), zero-input procs,
and zero-width cells and inputs are all legal; consumers handle them as
Hyle handles zero-width values (doc/hyle.md §8.6).

### 3.6 Programs

    prog ::= data* defn* proc+

A Synolon program has no designated root: its processes are its roots.
There is no `top` — purification consumed the device root into the
process. The restriction that the reactive root set is a singleton is a
property of the current pipeline: purify mints exactly one proc (named
`main`), and the fold requires exactly one; the grammar, the parser, and
the lint admit a list, and a multi-process program has no machine meaning
yet (`--certify` reports it UNSUPPORTED). Process composition is future
work.

## 4. Static semantics

- **Signal-guardedness**: the goto-only subgraph of the block graph is
  acyclic — every cycle crosses a `pause`. This one rule yields: divergent
  (pause-free) loops are rejected with a located error; entry evaluation
  terminates without fuel; blocks lower to acyclic Hyle definitions.
- **Constness** of cell initials; **representability** (fixed bit width,
  §3.1) of every binder, block parameter, cell, port, and halt answer;
  full-arity pauses and gotos; the input parameter typed `τ_I`; "root
  proc never pauses" (a proc with no pause target has no machine) — all
  with located diagnostics.
- **Typing** of the embedded expressions is doc/eidos.md §4.2's, at
  mono+ANF strength (every definition monomorphic, every type nat-closed,
  every value binder first-order) with the reactive types out of the type
  grammar entirely; a `Prim` occurrence instantiates its scheme of §6, and
  the seven builtins purification eliminates (doc/eidos.md §7) may not
  occur.
- **Pure-acyclicity**: the call graph of the pure definitions reachable
  from the process (block bodies, cell initials, and transitively) is
  acyclic, and no `rec` binding is reachable. Together with guardedness
  this makes the machine semantics (§5) a well-founded definition and
  block lowering total. The lint walks the reachable definitions
  depth-first from every block expression and cell initial; the Hyle
  checker's recursion rule remains the check on the fold's output.
- **Block normal form** (§3.2): command right-hand sides and cell
  initials are simple computations, terminator operands and put payloads
  are atoms or primitive expressions, case scrutinees are atoms and case
  alternatives let chains ending in an atom, and an application's head is
  a definition, constructor, or primitive (never a lambda: a residual
  beta-redex is a let). Jumps, join bindings, and reactive spines are
  thereby excluded from blocks (doc/eidos.md §3.4); joins survive only in
  pure definition bodies.
- **Scope of the uniqueness invariant**: every binding site of the
  datatype and definition fragment is globally unique (doc/eidos.md §2,
  doc/eidos.md §4.4; checked whole-program); labels and cells are distinct per
  process; within a block every binding site is distinct and disjoint
  from the definition-level sites; but one unique may be bound in
  several blocks — purification splices one definition per continuation
  and passes one binder along goto chains — so process binding sites are
  validated by scoping and occurrence-signature agreement, not by global
  uniqueness. Fresh uniques for a Synolon pass come from the maximum over
  the whole program, blocks included.
- All rules are stated per-proc.

## 5. Machine semantics

This section defines the meaning of a well-formed process
as a stream function, independent of the translation to Hyle. The
translation realizes exactly this semantics (§5.6); the four-way
cosimulation in `rwc-test` tests the correspondence per test.

### 5.1 Semantic values

For each representable type τ (§3.1), the set `V_τ` of values:

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
  (§3.1); its machine meaning is the 128-bit residue. Integer literals
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
denotations (§6) need; the general data-to-bits encoding of ADTs
belongs to the translation (doc/hyle.md), not to this semantics.

**Zero values.** `zero_τ ∈ V_τ`, used by cell initials (§5.4) and the
`error` builtin (§6): `zero_(Vec n τ)` = n copies of `zero_τ`;
`zero_Bool = False`; `zero_() = ()`; `zero_(Finite n) = 0`;
`zero_Integer = 0`; `zero_(T τ̄) = C₀(zero_τ₁', …)` where `C₀` is T's
first constructor in declaration order. Well-defined by the same
induction.

### 5.2 Pure evaluation

Let ρ map term binders to values and η interpret externs (§5.5).
`E⟦e⟧ρ` is standard call-by-value big-step evaluation of the pure
fragment (§3.2):

- **Atoms**: variables look up ρ; an integer literal at type `Integer`,
  `Finite n`, or `Vec n Bool` denotes its residue/value at that type
  (§5.1); `vec [e₁,…]` denotes the tuple of its elements' values.
- **Constructors**: a saturated constructor spine denotes `C(v̄)`.
- **Definition calls**: a saturated call to a pure definition evaluates
  the definition's body with parameters bound to argument values —
  well-founded because the pure call graph is acyclic (§4,
  *pure-acyclicity*). A definition may be eta-reduced (fewer declared
  parameters than its signature has arrows — including a bare builtin
  body); it means its eta-expansion, exactly as the translation
  eta-expands before lowering. The same reading covers a builtin
  applied below its signature arity in an argument position (a
  partially applied `error` at function type denotes the function
  constantly `zero` at the result).
- **Builtins**: per the signature and denotation table of §6.
  Higher-order builtin arguments (`rwPrimVecMap`'s function argument: a
  lambda, a possibly-partially-applied reference to a definition, or a
  possibly-partially-applied operator builtin) are applied semantically,
  element by element.
- **Case**: evaluate the scrutinee; select the first *matching*
  constructor or literal alternative, binding the case binder to the
  scrutinee's value and field binders to its components; the default
  alternative (syntactically first, when present) fires only when no
  other alternative matches. Case analyses are total
  (a non-matching value with no default is ill-formed input; the
  translation compiles the last alternative as unconditional).
- **Joins**: `let join L(x̄) = e_L in e` evaluates `e` with `L` bound to
  its continuation; `jump L(ā)` evaluates `e_L` under `x̄ ↦ ā-values`.
  Jumps are tail transfers to lexically enclosing joins; the scoping
  discipline (doc/eidos.md §3.4) admits no recursion among joins, so this is
  structural.

Evaluation is total on well-formed programs (no exceptions, no
divergence): every partiality in the source discipline — literal fit,
static-argument requirements, `error` — is either rejected by lint/the
translation or given a total denotation (§6).

### 5.3 Configurations and the machine step

A machine state is `(ℓ, w̄, σ)`: a pause-target label ℓ, saved values w̄
for ℓ's parameters *except* the last (the resumed input, doc/eidos.md §3.4), and a
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
                               (selection and field binding as in §5.2)

The `goto` clause is well-founded by signal-guardedness (§4): the
goto-only subgraph is acyclic, so recursion decreases the block's rank
in any topological order of that subgraph. A `goto` supplies *all* of
the target's parameters, including its resumed-input slot.

The one-cycle step, for input value i:

    step(ℓ, w̄, σ, i) = X⟦body(ℓ)⟧(params(ℓ) ↦ w̄ ⧺ ⟨i⟩, σ)

yielding either `Step(o, ℓ′, w̄′, σ′)` — an emitted output and the next
state — or `Halt(a, σ′)`.

### 5.4 Initialization, streams, and halt

The initial cell store σ₀ maps each cell to its declared initial's value
(a closed pure expression, evaluated with extern models per doc/eidos.md §3.4) or, for
`undef` initials, to `zero_τ` — the write-before-read convention of doc/eidos.md §3.4
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
(§7; the strict mode `--no-halt` rejects reachable halt, and halt-free
processes denote total streams).

**Causality.** `o(t)` depends on `i(0), …, i(t)`: the resumed block
reads the current cycle's input combinationally (a Mealy machine, as in
doc/hyle.md §6.4). It is the machine *state* — label, saved arguments,
cells — that is registered and depends only on strictly earlier inputs.

### 5.5 Externs

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

### 5.6 Correspondence to Hyle

The translation (`ReWire.Synolon.ToHyle`) realizes this semantics as a
Hyle device: the machine state becomes the registers (label tag ⧺ saved
arguments ⧺ cells, in the translation's record layout), each block
becomes a definition, dispatch is a case on the label tag, and the
register initials come from compile-time evaluation of the entry block
(§5.4). The device's stream function (doc/hyle.md §6.4) agrees with
`𝔐⟦P⟧η` through the data-to-bits encoding on the observable trace — up
to and excluding the halting cycle, exactly (bit-for-bit) for halt-free
processes. The four-way cosimulation check in `rwc-test` tests exactly
this correspondence, and `rwc --certify` checks it per compilation with
a machine-checked validator (doc/certify.md).

## 6. Builtin signatures and denotations

The signature scheme and the machine-level denotation of every builtin
that survives purification (`ReWire.Builtins`; occurrences print as
`rwPrim<Name>`, doc/eidos.md §9). The scheme rule is doc/eidos.md §7's —
a `Prim` occurrence's carried type must be a substitution instance of its
builtin's scheme, checked by both lints against the one implementation
table (`ReWire.Eidos.BuiltinSigs`); the `rwPrimExtern` row is the one
exception, its occurrence types trusted rather than checked. The seven
builtins purification eliminates (the reactive operations) are listed
there and have no denotation here. Quantified `n, m, i` have kind `Nat`;
`a, b` kind `*`. In denotations, `x = bv(v)` and `y = bv(w)` are the bit
readings of the first and second `Vec _ Bool` arguments (§5.1), and `⟨·⟩ₙ`
the width-n residue; "as hyle `op`" means the denotation table of
doc/hyle.md §5.2, through `bv`. A **static** argument must be a
compile-time literal after inlining (a located error otherwise) — a
partiality of the *translation*, not of the denotation.

**Foreign mechanisms** (signatures; denotation per §5.5):

| builtin | signature | notes |
|---|---|---|
| `rwPrimExtern` | `∀ a. [(String, Integer)] → String → String → [(String, Integer)] → [(String, Integer)] → String → a → String → a` | params, clock, reset, ins, outs, module name, model, instance name (reserved; currently ignored) — all but the model static; becomes an `xcall` (combinational) or an instance (clocked) |
| `rwPrimCryptol` | `∀ a. String → String → a → a` | module file and function name static; model-carrying by construction (§5.5) |

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
| `rwPrimError` | `∀ a. String → a` | `zero_a` (§5.1) | the message is not compiled (no hardware representation); the translation warns, quoting the message when it is a literal |
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
| `rwPrimVecMap` | `∀ n a b. (a → b) → Vec n a → Vec n b` | elementwise application (§5.2) | |
| `rwPrimVecGenerate` | `∀ n a. (Finite n → a) → Vec n a` | `⟨f(0), …, f(n−1)⟩` (function argument as in §5.2) | |

## 7. The machine-step contract

The fold (`ReWire.Synolon.ToHyle`) is a fold over a proc's label table
producing:

- a `Label` enumeration, one constructor per **pause-target** block
  (goto-only blocks are intra-cycle and get no state);
- a step record, laid out `out | next | args | cells'` — output value,
  next label, the next label's saved parameters (a per-label zero-padded
  union field, LSB-aligned, sized by the widest parameter list), and the
  updated cells;
- one n-ary dispatch over labels;
- register initials and the initial label, by compile-time evaluation of
  `entry` to its first pause (total by §4 guardedness);
- a `halted` flag bit if `halt` is reachable, occupying the record's
  most-significant bit (post-halt outputs are unspecified; the strict
  mode `--no-halt` rejects reachable halt).

Tag width is `nbits(#labels)`; label constructor names derive from
`(proc name, source continuation name)` under one shared, deterministic
naming and ordering function, which fixes dispatch order and tag values.

## 8. Pass discipline

Every pass over Synolon preserves the invariants of §4 (a pass leaves the
program at least as well-formed as it found it), preserves binder
uniqueness where it holds (duplication goes through the refreshing clone
of doc/eidos.md §8), and is annotation-transparent.

**Purification** (`ReWire.Eidos.ToSynolon`, pass 7) consumes the
mono+ANF restriction of an Eidos program (doc/eidos.md §6), keeps the
definitions the machine calls and the datatypes they and the process
mention (§3.3), and mints one process from the device root: a `signal` becomes a `pause` to the
continuation's block (whose last parameter is the resumed input), a
`return` a `goto` (or a `halt` at the root), lifted `get`/`put` operations
cell commands (the cells are the layers of the deepest state stack among
the reactive definitions' types and every type the reactive code
reachable from the root carries — the partial evaluator may have inlined
a stateful computation into its extrude site — and each operation
resolves its cell from its own residual stack), an `extrude` a cell write followed by its body, a
reactive `case` a terminator case (arms with commands get their own
blocks; the case binder, an alias of the atom scrutinee, is substituted
away where GHC's case-binder swap left it live), join points
blocks and jumps gotos. A reactive call is compiled once per continuation
— block-graph splicing memoized on the (definition, continuation) pair —
so tail recursion closes through the memo table and becomes a `goto`. Two
shapes are rejected with located errors: a `NOINLINE` reactive callee on
the left-hand side of a bind (the user's opt-out from splicing), and
recursion *through* a bind's left-hand side (each pending continuation
would be a resumption-stack frame, and the machine has no stack). Blocks
are then closure-converted: a fixpoint over the block graph computes each
block's live-in locals, which become leading parameters that every
transfer site supplies. Cell initials are all `undef` (entry-block writes
are the initialization path, §5.4).

**Block-graph cleanup** (`ReWire.Synolon.Transform`, pass 8) iterates
three transforms to a fixpoint (until the block-label sequence is stable):

1. *Epsilon-block inlining*: a block with no commands whose terminator is a
   single `goto` is glue; references to it re-target its successor
   directly, with the argument substitution applied. A pause target is
   never inlined away — it is a machine state. Chain-following is
   fuel-bounded by the block count, because signal-guardedness is only
   checked after this pass.
2. *Alpha-equal block merge*: blocks whose bodies are alpha-equivalent
   (binders renumbered densely from a disjoint range; labels and cells
   compared by identity) merge into the first in declaration order, and
   references redirect to the survivor; iterated, since each round can
   unify the targets of further blocks. This is what keeps the state count
   minimal — an `INLINE`-duplicated continuation mints many identical
   pause targets. The key is the *printed* canonical block (§9), so the
   printer is normative for state counts.
3. *Unreachable-block purge*: purification drops the continuation of a
   computation that cannot return (an `error` at monadic type), orphaning
   the blocks compiled for it; orphaned pause targets would otherwise mint
   machine states and mask the never-pauses rule, so blocks unreachable
   from the entry are removed.

The pass also reports, under `-v`, the machine accounting: states (pause
targets plus the entry state), the tag width `nbits(states + 1)`, and a
merge-headroom diagnostic (how many further blocks and states a
bisimulation-style partition refinement would remove).

**The lint** (`ReWire.Synolon.Lint`) runs unconditionally after the
cleanup; under `--debug-lint` it also runs after purification, without
the signal-guardedness rule (the one rule the cleanup may establish, by
removing an orphaned unguarded block). Block normal form is checked at
both points: purification establishes it and the cleanup preserves it. **`--no-halt`** then rejects any
reachable `halt` — after the purge every block is reachable, so this is
syntactic presence of a `halt` terminator, including under terminator
cases. **The dump** the fold consumes is what `--synolon` writes as
`<out>.syn` (published by a same-directory temporary and a rename);
`--certify` binds it by SHA-256 and hands it to the validator, whose own
transcription of §9 reads it — the concrete syntax is part of the certify
trust base (doc/certify.md), so a grammar change lands in `verify/` first.

## 9. Concrete syntax (.syn)

The lexical conventions are doc/eidos.md §9's (`.syn` and `.eir` share one
lexer, `ReWire.Eidos.Lexer`, and one reserved-word table — the union of
both grammars' keywords, so a `.eir` file quotes `proc`, `block`, `put`, …
just as a `.syn` file quotes `top`). Line comments are `--`. The format
round-trips: `parse ∘ pretty` is the identity on programs modulo
annotations, and `pretty ∘ parse ∘ pretty ≡ pretty` is a tested fixpoint.

    prog  ::= data* defn* proc+
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

`data`, `defn`, `param`, `ty`, `sig`, `exp`, `app`, `arg`, `atom`, `bind`,
and `alt` are the Eidos productions (doc/eidos.md §9). The restrictions of
§3 are enforced by the lint, not the grammar: a definition signature is
monomorphic and no `@ty` argument occurs; no `join` or `jump` occurs inside
a block. `atom` in the machine positions is the Eidos `atom` production,
whose alternatives include `'(' exp ')'` — the printer parenthesizes any
non-atom operand there, so a `.syn` file can express every pure expression
the abstract syntax admits (§3.2). Labels print like variables (`L#12`) and
carry no signature (the parser reconstructs one, arrows from the block's
parameter types to the process output type — a bookkeeping convention,
labels are not values); terminator labels may reference blocks declared
later in the same process. Expressions inside processes parse in the
monomorphic, join-free scope.

## 10. Correspondence to the implementation

| Spec section | Module |
|---|---|
| §3.4 (proc syntax), §3.6 (programs) | `ReWire.Synolon.Syntax` (embedding `ReWire.Eidos.Syntax`) |
| §3.1–§3.3 (types, expressions, definitions) | `ReWire.Eidos.Syntax`, `ReWire.Eidos.Types` (shared) |
| §4 (the machine rules) | `ReWire.Synolon.Lint`, over `ReWire.Eidos.Lint`'s expression checker |
| §3.1 (representability: the width of a type) | `ReWire.Synolon.Repr` (shared by the lint and the fold) |
| §3.2 (atoms and primitive expressions) | `ReWire.Eidos.ANF` (`isAtom`, `isPrimExp`; shared by both lints) |
| §6 (builtin signatures) | `ReWire.Eidos.BuiltinSigs` (shared) |
| §5 (machine semantics) | no Haskell evaluator (as doc/eidos.md §5 says of Eidos): mechanized in `verify/Rwv/Eidos/Value.lean` (values, zero), `Eval.lean` (pure evaluation and the builtin table), and `verify/Rwv/Synolon/Machine.lean` (block execution, the step, streams); differentially tested by `rwv-synolon-diff` against `rwc --interpret` and by the four-way cosimulation in `rwc-test` |
| §7 (the machine fold, Synolon → Hyle) | `ReWire.Synolon.ToHyle` |
| §8 (purification, Eidos → Synolon) | `ReWire.Eidos.ToSynolon` |
| §8 (block-graph cleanup, machine accounting) | `ReWire.Synolon.Transform` |
| §9 (`.syn`) | `ReWire.Synolon.Pretty`, `ReWire.Synolon.Parse` (over `ReWire.Eidos.Pretty`, `ReWire.Eidos.Parse`, `ReWire.Eidos.Lexer`) |
| minted-name conventions (labels, lifted joins) | `ReWire.Eidos.Naming` (shared) |
| substitution, refreshing, unique supplies | `ReWire.Eidos.Subst` (shared) |

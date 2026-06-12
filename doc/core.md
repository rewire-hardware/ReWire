# Mantle: the ReWire core intermediate representation

**Status: normative (June 2026).** This document specifies the syntax and
semantics of *Mantle*, the bit-level IR implemented in `ReWire.Mantle.*`,
which replaced the previous Core IR. The name continues the geological
convention set by Crust. (The migration history lives in
`core-refactor-plan.md` and `refactor-report.md`, at the repo root.)

Mantle is the IR that the back ends consume: the pipeline becomes

    HSE  →  Crust  →  (purification)  →  Mantle  →  { Verilog, VHDL, Cryptol, interpreter }

Everything upstream of the purified program (Crust, `Crust.Purify`) is
unchanged by this proposal; Mantle replaces only the bit-level IR and its
consumers.

## 1. Design goals

Mantle is a *pure, first-order, monomorphic, total* functional language over
width-indexed bitvectors, together with a synchronous *device* construct
(registers, instances, and parallel wire equations) that makes the state
machine explicit. It is deliberately close to a subset of Cryptol: the
functional fragment admits a syntax-directed, width-preserving embedding into
Cryptol (§8.4), and the device construct corresponds to the standard Mealy
stream idiom.

The design goals, each traceable to a defect of the current Core catalogued in
`rtl-backends.md` (a local design note):

- **G1 — Explicit widths.** Every expression has a unique width derivable
  from its immediate subterms; all primitives are width-homogeneous; all
  resizing is via explicit `zext`/`sext`/`trunc` coercions. No back end
  reconstructs context-determined widths (`wcast`/`expWidth` and their VHDL
  and Cryptol counterparts disappear).
- **G2 — Explicit state.** Registers are declared IR objects with widths and
  initial values. The resumption layout (padding ∥ outputs ∥ dispatch) that
  the interpreter and all three back ends currently re-derive becomes the
  *producer's* concern, expressed once as ordinary slices and equations.
- **G3 — No implicit conventions.** Every mux has both arms ("a call with a
  nil alternative takes its branch" is gone); division by zero has one
  defined semantics implemented by every target; don't-care values are an
  explicit, semantically-defined node.
- **G4 — Named, fully-sized ports everywhere**, externs included; sequential
  (clocked) externs are device-level *instances*, not pretend function calls,
  resolving the current ambiguity about how many instances a clocked extern
  call denotes.
- **G5 — Abstract names.** Identifiers are arbitrary text; each printer
  legalizes names for its target (as today, via `Core.Mangle` and the VHDL
  extended-identifier escapes).
- **G6 — Semantics first.** The language has a compositional denotational
  semantics; the interpreter is its executable transcription, and each back
  end's correctness is a per-construct refinement statement (§8). This is
  what the four-way cosimulation check in `rwc-test` tests.

Non-goals (possible future extensions, not designed here): memory/array
primitives, multiple clock domains, register clock-enables (a derived form:
`next r := if en then e else r`), and sub-cycle timing of any kind.

## 2. Semantic preliminaries

**Bitvectors.** For n ∈ ℕ (including 0), the type of bitvectors of width n is

    BV(n) ≔ { 0, 1, …, 2ⁿ − 1 } ⊆ ℕ

i.e. values are identified with their unsigned denotations. BV(0) = {0} is a
unit type; its unique value is written ⟨⟩. Bit i of x ∈ BV(n), for
0 ≤ i < n, is bitᵢ(x) ≔ ⌊x / 2ⁱ⌋ mod 2; bit 0 is the least significant.
The *signed* (two's-complement) reading of x ∈ BV(n), n ≥ 1, is

    sint(x) ≔ x − 2ⁿ · bit_{n−1}(x)        and        sint : BV(0) → ℤ ≔ const 0.

Truncation to width n is ⟨v⟩ₙ ≔ v mod 2ⁿ for v ∈ ℤ (well-defined into BV(n)).

**Concatenation and slicing.** For x ∈ BV(m), y ∈ BV(n):

    x ⧺ y ≔ x · 2ⁿ + y                     ∈ BV(m+n)     (x supplies the high bits)
    sliceᵢ,ₖ(x) ≔ ⌊x / 2ⁱ⌋ mod 2ᵏ          ∈ BV(k),       for i + k ≤ m

⧺ is associative with unit ⟨⟩. Note the *LSB-at-0* indexing convention,
matching Verilog `x[i+k-1 : i]` and the existing `rwPrimBitSlice`; Cryptol's
MSB-first sequences require an index flip in the embedding (§8.4).

**Streams.** For a set A, A^ω ≔ ℕ → A is the set of streams over A, and A^*
the set of finite sequences. A function F : A^* → B is used to model a causal
stateful process: F(a₀ … a_{t−1}) is its output at time t, which thus depends
only on inputs *strictly before* t ("strict causality", §6.4).

**Environments.** For a finite map Γ, an environment ρ ⊨ Γ assigns to each
x ∈ dom(Γ) with Γ(x) = [n] a value ρ(x) ∈ BV(n).

## 3. Abstract syntax

Metavariables: n, m, k, i range over ℕ; v over ℕ; x, y over local variable
names; f over definition names; E over extern names; ι over instance names;
all names are arbitrary nonempty text (G5). Every AST node additionally
carries an annotation α (source location, `Annote`), which is semantically
inert and ignored throughout this document.

### 3.1 Types

    τ ::= [n]                              n ∈ ℕ

There is exactly one type former. There are no polymorphic types, no
functions-as-values, no sums or products: by the time a program reaches
Mantle, all of Crust's algebraic data has been compiled to bit-level
representations, and all higher-order structure has been eliminated.

### 3.2 Expressions

    e ::= lit n v                          literal, 0 ≤ v < 2ⁿ
        | undef n                          don't-care (semantically ≡ lit n 0, §5.1)
        | x                                variable (parameter or let-bound)
        | e₁ ⧺ e₂                          concatenation
        | e[i +: k]                        static slice (k bits starting at LSB-index i)
        | op⟨n̄⟩(e₁, …, e_k)                primitive application (§3.3); n̄ static naturals
        | f(e₁, …, e_k)                    definition call (saturated)
        | E⟨c₁, …, c_g⟩(e₁, …, e_k)        combinational extern call; cᵢ ∈ ℕ static generics
        | if e₁ then e₂ else e₃            mux (e₁ : [1]); both arms always present
        | let x = e₁ in e₂                 local binding (explicit sharing)

There is no pattern matching: Crust-level patterns are compiled by the
producer into slices, equality tests, and muxes (they already are, in effect —
`patMatches`/`patApply` in the current interpreter — but the compilation
becomes explicit IR rather than a convention shared by four consumers).

### 3.3 Primitives

Each primitive is a total function on bitvectors, width-homogeneous except
where noted. Static parameters (target widths, replication counts) are part
of the operator, written in ⟨·⟩; they are elaboration-time constants, never
runtime values.

| family        | operators                          | type                      | side conditions |
|---------------|------------------------------------|---------------------------|-----------------|
| arithmetic    | `add` `sub` `mul` `udiv` `umod` `pow` | [n] × [n] → [n]        |                 |
| bitwise       | `and` `or` `xor`                   | [n] × [n] → [n]           |                 |
|               | `not`                              | [n] → [n]                 |                 |
| shifts        | `shl` `lshr` `ashr`                | [n] × [k] → [n]           | amount width k arbitrary |
| comparison    | `eq` `ne` `ult` `ule` `ugt` `uge` `slt` `sle` `sgt` `sge` | [n] × [n] → [1] | |
| reduction     | `redand` `redor` `redxor`          | [n] → [1]                 |                 |
| coercion      | `zext⟨m⟩`                          | [n] → [m]                 | n ≤ m           |
|               | `sext⟨m⟩`                          | [n] → [m]                 | 1 ≤ n ≤ m       |
|               | `trunc⟨m⟩`                         | [n] → [m]                 | m ≤ n           |
| structural    | `rep⟨k⟩`                           | [n] → [k·n]               |                 |

The greater-than comparisons are definable by operand swap, but are kept as
primitives so that emitted RTL reads the way the source did (readability of
the generated IR and RTL is a stated goal). Logical
negation/conjunction/disjunction of flags are `not`/`and`/`or` at width 1;
"is nonzero" is `redor`. §9.1 gives the complete mapping from the current
Core `Prim` set.

### 3.4 Definitions

    defn ::= f : ([n₁], …, [n_k]) → [m]
             f(x₁, …, x_k) = e

Definitions are first-order, saturated, and closed except for references to
other definitions and to combinational externs. The call graph (including
extern→model edges, §3.5) must be acyclic (§4.4); consequently every
definition denotes a total function (§6.2).

### 3.5 Extern declarations

    extern ::= extern E
                 generics  g₁, …, g_q                       (q ≥ 0; elaboration-time ℕ parameters)
                 kind      comb | seq(clock cname, reset rname)
                 inputs    p₁ : [n₁], …, p_k : [n_k]        (named; k ≥ 0)
                 outputs   q₁ : [m₁], …, q_l : [m_l]        (named; l ≥ 1)
                 model     f                                 (optional; comb only)

Port names are mandatory (G4); ports left anonymous in the source-level
extern descriptor are given synthesized names (`p0, p1, …`) by the producer,
and the external implementations are expected to match — making the names
part of the IR removes the per-backend guesswork.

A **combinational** extern (kind `comb`) is callable in expressions,
`E⟨c̄⟩(ē)`, with result type [m₁ + ⋯ + m_l] — the concatenation of its
outputs in declaration order, high bits first; the caller slices. Its
semantics is that of an *uninterpreted function* (§6.1): the denotation of a
program is parameterized by an interpretation of its model-less combinational
externs, and two calls with equal arguments are interchangeable. (This is an
*assumption* about the external implementation, exactly the assumption RTL
synthesis already makes when it duplicates or merges combinational logic.)

If a `model f` is given, the interpretation of E is fixed to the denotation
of f, which must have signature ([n₁], …, [n_k]) → [m₁ + ⋯ + m_l]. Models do
not see generics (a carried-over limitation of the current extern-model
design). Model references are call-graph edges for the acyclicity check.

A **sequential** extern (kind `seq`) is stateful: it may only be
*instantiated* at device level (§3.6), never called in an expression. Its
clock and reset port names are emission metadata for the RTL targets. This
replaces the current encoding where `ExternSig`'s clock/reset fields are a
de-facto statefulness flag on a value-level call.

### 3.6 Devices

    device ::= device X
                 input     i₁ : [a₁], …                     (zero or more)
                 output    o₁ : [b₁], …                     (zero or more)
                 register  r₁ : [c₁] init w₁, …             (zero or more; wⱼ ∈ BV(cⱼ) literal)
                 instance  ι₁ of E₁⟨c̄₁⟩, …                  (zero or more; Eⱼ sequential)
                 body      B

    B ::= stmt₁ ; … ; stmt_z
    stmt ::= let x = e                                       local wire
           | o := e                                          output drive
           | next r := e                                     register update
           | ι.p := e                                        instance input drive

The body is a sequence of statements. Scoping (precisely in §4.3): inputs,
registers (their *current* values), and instance output ports (as qualified
names `ι.q`) are in scope throughout the body; a `let`-bound name is in scope
in statements strictly after its binding (no forward references, so
combinational well-foundedness holds by construction); output names are
*never* readable (assign-only — VHDL `out` ports aren't readable, and a
`let` covers the use case). Every output, every register, and every input
port of every instance must be assigned exactly once; `let` names at device
level serve double duty as the replacement for the current `SetRef`/`GetRef`
reference cells (§9.1): they are named wires, emitted by RTL back ends under
their given names.

A device is a Mealy machine: outputs at cycle t are a function of register
state at t and inputs at t; registers take their `next` values at t+1; at
t = 0 registers hold their `init` values. §6.4 makes this precise.

Clock and reset are *not* inputs: the semantics is synchronous and
cycle-indexed. The device carries emission metadata (clock and reset port
names, reset synchronicity and polarity — today's `--clock-name` /
`--reset-name` / `--sync-reset` / `--invert-reset` configuration) that
affects only the RTL realization, with a refinement obligation in §8.5.

### 3.7 Programs

    prog ::= extern* defn* device

A program is a set of extern declarations and definitions plus exactly one
device. All global names (externs, defns, the device, and within the device
its locals) are distinct in their respective namespaces.

## 4. Static semantics

### 4.1 Signatures and contexts

A program induces global signature maps:

    Σ_f(f) = ([n₁, …, n_k]; [m])                            for each defn
    Σ_x(E) = (q; [n₁, …, n_k]; [m₁, …, m_l]; κ)             for each extern, κ ∈ {comb, seq}

Local contexts Γ are finite maps from variable names to types. The
expression typing judgment is Σ; Γ ⊢ e : [n] (Σ abbreviates both global
maps). Typing is syntax-directed and each expression has at most one type;
checking is linear in the size of the term (given Σ).

### 4.2 Expression typing

    ──────────────────────── (T-Lit)   0 ≤ v < 2ⁿ          ──────────────────────── (T-Undef)
    Σ; Γ ⊢ lit n v : [n]                                    Σ; Γ ⊢ undef n : [n]

    Γ(x) = [n]
    ────────────────── (T-Var)
    Σ; Γ ⊢ x : [n]

    Σ; Γ ⊢ e₁ : [m]    Σ; Γ ⊢ e₂ : [n]                      Σ; Γ ⊢ e : [n]    i + k ≤ n
    ─────────────────────────────────── (T-Cat)             ─────────────────────────── (T-Slice)
    Σ; Γ ⊢ e₁ ⧺ e₂ : [m + n]                                Σ; Γ ⊢ e[i +: k] : [k]

    op⟨n̄⟩ : [w₁] × ⋯ × [w_k] → [m]  (per §3.3)    Σ; Γ ⊢ eⱼ : [wⱼ]  (1 ≤ j ≤ k)
    ───────────────────────────────────────────────────────────────────────────── (T-Prim)
    Σ; Γ ⊢ op⟨n̄⟩(e₁, …, e_k) : [m]

    Σ_f(f) = ([n₁, …, n_k]; [m])    Σ; Γ ⊢ eⱼ : [nⱼ]  (1 ≤ j ≤ k)
    ────────────────────────────────────────────────────────────── (T-Call)
    Σ; Γ ⊢ f(e₁, …, e_k) : [m]

    Σ_x(E) = (g; [n₁, …, n_k]; [m₁, …, m_l]; comb)    cⱼ ∈ ℕ    Σ; Γ ⊢ eⱼ : [nⱼ]
    ───────────────────────────────────────────────────────────────────────────── (T-XCall)
    Σ; Γ ⊢ E⟨c₁, …, c_g⟩(e₁, …, e_k) : [m₁ + ⋯ + m_l]

    Σ; Γ ⊢ e₁ : [1]    Σ; Γ ⊢ e₂ : [n]    Σ; Γ ⊢ e₃ : [n]
    ────────────────────────────────────────────────────── (T-If)
    Σ; Γ ⊢ if e₁ then e₂ else e₃ : [n]

    Σ; Γ ⊢ e₁ : [m]    Σ; Γ, x:[m] ⊢ e₂ : [n]
    ────────────────────────────────────────── (T-Let)
    Σ; Γ ⊢ let x = e₁ in e₂ : [n]

Width-0 expressions are first-class: `lit 0 0` (written `⟨⟩`), slices of
width 0, and width-0 parameters and ports are all legal, and the primitives
are total at n = 0 (§5.2 lists the conventions). Back ends erase width-0
values entirely (§8.6); the type system is what makes that erasure trivially
sound, since BV(0) is a unit type.

### 4.3 Definition, device, and program well-formedness

**Definitions.** `f(x₁,…,x_k) = e` with Σ_f(f) = ([n₁,…,n_k]; [m]) is
well-formed iff the xⱼ are distinct and Σ; x₁:[n₁], …, x_k:[n_k] ⊢ e : [m].

**Devices.** With inputs iⱼ:[aⱼ], outputs oⱼ:[bⱼ], registers rⱼ:[cⱼ] init wⱼ,
and instances ιⱼ of Eⱼ⟨c̄ⱼ⟩ where Σ_x(Eⱼ) = (gⱼ; ī; ō; seq):

- generic counts match: |c̄ⱼ| = gⱼ;
- register initials are in range: wⱼ ∈ BV(cⱼ);
- let the *ambient context* be
  Γ₀ ≔ { iⱼ : [aⱼ] } ∪ { rⱼ : [cⱼ] } ∪ { ι.q : [m] | q:[m] an output port of ι's extern };
- the body B = stmt₁; …; stmt_z is checked left to right starting from Γ₀:
  a statement `let x = e` requires Σ; Γ ⊢ e : [m] for some m and extends
  Γ with x:[m] (x fresh in Γ); the assignment statements require
  Σ; Γ ⊢ e : [b] where [b] is the declared width of the target (output,
  register, or instance input port) and do not extend Γ;
- each output o, each register r (as `next r`), and each input port ι.p of
  each instance is the target of exactly one assignment; nothing else may be
  a target; output names do not appear in any Γ.

**Programs.** All declarations well-formed; all global names distinct;
every called f and E is declared with the matching kind (comb for calls,
seq for instances); every `model f` reference resolves to a defn of the
required signature; and the relation

    f ↝ f′  iff  f′ is called in the body of f, or some extern called in the
                body of f has model f′

is acyclic over defn names (with the device body's expressions treated as
the body of a virtual root). Acyclicity bans recursion outright, as today
(`Core.Check.checkRecursion`); it is also what makes the semantics in §6 a
definition rather than a fixpoint assertion.

## 5. Dynamic semantics: values and primitives

### 5.1 Don't-care

`undef n` denotes ⟨0⟩ₙ — *defined* to be zero, not nondeterministic. The
node exists to preserve the information that the producer considered the
value irrelevant (the result of a live `error` call, or the unreachable arm
of a total case analysis): optimization passes may exploit it (replacing it
with any value of the right width is sound *for the program the producer
meant*, though it changes the bit-for-bit denotation and therefore golden
outputs), and back ends may surface it as a comment or synthesis attribute.
The cosimulation requirement — all targets bit-identical — is what forces a
single defined value; zero matches what the compiler does today.

### 5.2 Primitive denotations

For x, y in BV of the indicated widths (result truncations are implied by the
⟨·⟩ₙ notation; all operations are total):

| op            | denotation                                                       | edge cases |
|---------------|------------------------------------------------------------------|------------|
| `add`         | ⟨x + y⟩ₙ                                                          |            |
| `sub`         | ⟨x − y⟩ₙ                                                          | modular    |
| `mul`         | ⟨x · y⟩ₙ                                                          |            |
| `udiv`        | y ≠ 0: ⌊x / y⌋;  y = 0: 2ⁿ − 1                                    | SMT-LIB `bvudiv` convention |
| `umod`        | y ≠ 0: x mod y;  y = 0: x                                         | SMT-LIB `bvurem` convention |
| `pow`         | ⟨x^y⟩ₙ                                                            | 0⁰ = 1     |
| `and`/`or`/`xor` | bitwise                                                        |            |
| `not`         | ⟨2ⁿ − 1 − x⟩ₙ (bitwise complement)                                |            |
| `shl`         | ⟨x · 2^y⟩ₙ                                                        | y ≥ n ⇒ 0  |
| `lshr`        | ⌊x / 2^y⌋                                                         | y ≥ n ⇒ 0  |
| `ashr`        | ⟨sint(x) · 2^{−y}⟩ₙ rounded toward −∞, i.e. sign-filling          | y ≥ n ⇒ all-sign-bits |
| `eq`/`ne`     | x = y / x ≠ y, as ⟨1⟩₁/⟨0⟩₁                                       | on [0]: eq = 1, ne = 0 |
| `ult`/`ule`   | x < y / x ≤ y (unsigned)                                          | on [0]: ult = 0, ule = 1 |
| `ugt`/`uge`   | x > y / x ≥ y (unsigned)                                          | on [0]: ugt = 0, uge = 1 |
| `slt`/`sle`   | sint(x) < sint(y) / ≤                                             | on [0]: slt = 0, sle = 1 |
| `sgt`/`sge`   | sint(x) > sint(y) / ≥                                             | on [0]: sgt = 0, sge = 1 |
| `redand`      | 1 iff x = 2ⁿ − 1                                                  | n = 0 ⇒ 1 (vacuous truth) |
| `redor`       | 1 iff x ≠ 0                                                       | n = 0 ⇒ 0  |
| `redxor`      | parity of x (number of set bits, mod 2)                           | n = 0 ⇒ 0  |
| `zext⟨m⟩`     | x (value unchanged)                                               |            |
| `sext⟨m⟩`     | ⟨sint(x)⟩ₘ                                                        | requires n ≥ 1 |
| `trunc⟨m⟩`    | ⟨x⟩ₘ (keeps the m low bits)                                       |            |
| `rep⟨k⟩`      | x ⧺ x ⧺ ⋯ ⧺ x (k copies)                                          | k = 0 ⇒ ⟨⟩ |

The division-by-zero conventions follow SMT-LIB, deliberately: equivalence
checking against SMT-backed tools (SAW, the intended consumer of the Cryptol
back end) then needs no shimming. Every back end must *implement* these
conventions (a guard mux around `/` and `%` in Verilog and Cryptol; the VHDL
`rw_helpers` package already guards division) — division is rare and
expensive in hardware anyway, so the guard is noise-level (§8).

The `ashr` sign convention at n = 0 is vacuous (BV(0) is unit); `sext` from
width 0 is excluded by typing because there is no sign bit to extend.

## 6. Dynamic semantics: expressions, definitions, devices

### 6.1 Extern interpretations

Let X_c be the set of combinational externs of a program P. An
**interpretation** η assigns to each E ∈ X_c with
Σ_x(E) = (g; [n₁,…,n_k]; [m₁,…,m_l]; comb) a function

    η(E) : ℕ^g × BV(n₁) × ⋯ × BV(n_k) → BV(m₁ + ⋯ + m_l)

subject to: if E declares `model f`, then η(E)(c̄, x̄) = 𝔉⟦f⟧(x̄) for all c̄
(model semantics fixed, generic-independent; §6.2 defines 𝔉). The denotation
of a program is a function of η; programs whose externs all carry models (or
that have none) have an absolute denotation. This is the formal content of
"unclocked externs are uninterpreted functions" in the current Cryptol back
end, and of the interpreter's refusal to evaluate model-less externs.

### 6.2 Expressions and definitions

Fix a program P (hence Σ and the defn bodies) and an interpretation η.
Because the call graph is acyclic (§4.3), we may define, by well-founded
recursion on the call order, simultaneously:

- for each defn f with Σ_f(f) = ([n₁,…,n_k]; [m]), its denotation
  𝔉⟦f⟧ : BV(n₁) × ⋯ × BV(n_k) → BV(m);
- for each expression e with Σ; Γ ⊢ e : [n] and each ρ ⊨ Γ, its denotation
  ℰ⟦e⟧ρ ∈ BV(n):

```
ℰ⟦lit n v⟧ρ            = v
ℰ⟦undef n⟧ρ            = 0
ℰ⟦x⟧ρ                  = ρ(x)
ℰ⟦e₁ ⧺ e₂⟧ρ            = ℰ⟦e₁⟧ρ ⧺ ℰ⟦e₂⟧ρ
ℰ⟦e[i +: k]⟧ρ          = sliceᵢ,ₖ(ℰ⟦e⟧ρ)
ℰ⟦op⟨n̄⟩(ē)⟧ρ           = ⟦op⟨n̄⟩⟧(ℰ⟦e₁⟧ρ, …, ℰ⟦e_k⟧ρ)            (table in §5.2)
ℰ⟦f(ē)⟧ρ               = 𝔉⟦f⟧(ℰ⟦e₁⟧ρ, …, ℰ⟦e_k⟧ρ)
ℰ⟦E⟨c̄⟩(ē)⟧ρ            = η(E)(c̄, ℰ⟦e₁⟧ρ, …, ℰ⟦e_k⟧ρ)
ℰ⟦if e₁ then e₂ else e₃⟧ρ = ℰ⟦e₂⟧ρ  if ℰ⟦e₁⟧ρ = 1,  else ℰ⟦e₃⟧ρ
ℰ⟦let x = e₁ in e₂⟧ρ   = ℰ⟦e₂⟧(ρ[x ↦ ℰ⟦e₁⟧ρ])

𝔉⟦f⟧(v̄)                = ℰ⟦e_f⟧{x̄ ↦ v̄}        where f(x̄) = e_f
```

Note the mux is *eager* in the mathematical sense (both arms denote); since
the language is total, eager and non-strict readings coincide, and back ends
are free to realize the mux as hardware (both arms always computed) or as a
short-circuiting conditional (the Cryptol `if`).

### 6.3 Device bodies (the step function)

Fix a device with inputs ī, outputs ō, registers r̄ (widths c̄, initials w̄),
instances ῑ, and body B. Write

    S ≔ Π_j BV(cⱼ)        (states)         I ≔ Π_j BV(aⱼ)      (input bundles)
    O ≔ Π_j BV(bⱼ)        (output bundles)
    Q_ι ≔ Π_{q ∈ outs(ι)} BV(·)            (per-instance output bundles)   Q ≔ Π_ι Q_ι
    P_ι ≔ Π_{p ∈ ins(ι)} BV(·)             (per-instance input bundles)    Π ≔ Π_ι P_ι

The body defines the **step function**

    step : S × I × Q → S × O × Π

as follows: given (s, i, q), let ρ₀ be the environment mapping each register
name to its component of s, each input name to its component of i, and each
ι.q to its component of q. Process the statements of B in order, maintaining
ρ: a `let x = e` extends ρ with x ↦ ℰ⟦e⟧ρ; an assignment `t := e` records
ℰ⟦e⟧ρ as the value of target t (and leaves ρ unchanged). By well-formedness
every output, `next r`, and ι.p is recorded exactly once; collect them into
(s′, o, π) = step(s, i, q). Since assignments do not bind readable names,
their relative order is irrelevant; only the let-prefix order matters, and
the no-forward-reference rule makes the traversal well-defined. There is no
combinational feedback by construction.

### 6.4 Device semantics (streams)

A sequential extern instance ι is modeled by a **strictly causal stream
function**, given as F_ι : P_ι^* → Q_ι — from the finite history of its
inputs at cycles 0 … t−1 to its outputs at cycle t. (Strict causality says a
clocked extern's outputs this cycle depend only on past inputs, i.e. it is
register-like. A physical extern with a combinational input-to-output path is
outside this model — as it is outside today's model — and the obligation to
avoid creating combinational loops through such an extern rests with the
user, as it does with any RTL instantiation.)

Given η and a family F = (F_ι), the device denotes the stream function

    𝔇⟦device⟧ : I^ω → O^ω

defined by the (well-founded, by strict causality) recurrence

    s(0)   = w̄                                       (register initials)
    q_ι(t) = F_ι(π_ι(0) … π_ι(t−1))                  (instance outputs)
    (s(t+1), o(t), π(t)) = step(s(t), i(t), q(t))

with 𝔇⟦device⟧(i)(t) ≔ o(t). For an instance-free device this simplifies to
the familiar Mealy unfolding

    s(0) = w̄,    (s(t+1), o(t)) = step(s(t), i(t))

which is precisely what the interpreter (`ReWire.Mantle.Interp`) and the
Cryptol back end's `rw_device` stream comprehension implement; the
decomposition of the resumption vector into ⟨padding, outputs, dispatch-tag,
state⟩ is not their job: the producer has already split it into named
registers and output equations.

The interpreter realizes 𝔇 directly (it rejects programs containing
instances or model-less combinational externs). The finite-trace semantics
used by `--interpret` and the `.yaml` goldens is the n-prefix of 𝔇⟦device⟧
applied to the (zero-extended) input trace.

## 7. Metatheory

The following properties are immediate from the definitions but worth
recording, since the back ends and optimizer rely on them.

**Totality / type soundness.** If Σ; Γ ⊢ e : [n] and ρ ⊨ Γ then
ℰ⟦e⟧ρ ∈ BV(n) — defined, in range, no partiality anywhere (acyclicity gives
well-foundedness of 𝔉; §5.2 gives totality of the primitives). There is no
notion of run-time error in the language.

**Determinism and unique types.** Typing is syntax-directed; each e has at
most one type, computable in one bottom-up pass; ℰ is a function.

**Substitution / let-β.** ℰ⟦let x = e₁ in e₂⟧ρ = ℰ⟦e₂[e₁/x]⟧ρ, and
conversely any subterm may be hoisted into a fresh let. Hence inlining,
common-subexpression elimination, and let-floating are semantics-preserving;
the legality of CSE extends to combinational extern calls by the
uninterpreted-function reading of η (η(E) is a function, so equal arguments
give interchangeable calls).

**Defn inlining.** ℰ⟦f(ē)⟧ρ = ℰ⟦e_f[ē/x̄]⟧ρ. Whether a defn is emitted as a
module/function or inlined is therefore a free choice of a Mantle→Mantle
pass, not a semantic question — which is where the current backends'
"inline when uses == 1" heuristic moves (§8, and the migration plan).

**Congruences.** All constructs are congruent: equal-denotation subterms may
be replaced anywhere. The optimizer needs nothing beyond §5.2's equations and
these congruences (constant folding is literally evaluating ℰ on closed
terms).

**Width erasure at 0.** Any e : [0] satisfies ℰ⟦e⟧ρ = ⟨⟩; any context is
unchanged by replacing e with `lit 0 0`. (Soundness of the back ends' total
erasure of zero-width wires.)

**Cryptol embedding.** There is a compositional, width-preserving
translation ⌈·⌉ of the functional fragment into Cryptol with
⟦⌈e⌉⟧_Cryptol = ℰ⟦e⟧ (§8.4), and ⌈device⌉ as the standard stream idiom. This
is the precise sense in which Mantle is "a Cryptol subset": the Cryptol back
end is the embedding plus name legalization.

## 8. Realization on the targets

This section fixes, per construct, what each back end emits. With G1–G4 in
force, every row is a local template — no analysis beyond a topological walk.

### 8.1 Common structure

- Each defn ↦ one Verilog module / VHDL entity+architecture / Cryptol
  function. A prior Mantle→Mantle inline pass decides which defns still
  exist; back ends do not inline.
- `let` ↦ wire + continuous assign / signal + concurrent assignment /
  `where`-binding. Since every let has an explicit width, the
  temporary-hoisting that VHDL required for port actuals is just "introduce
  a let", performable mechanically by the printer when the actual isn't
  already atomic.
- Multi-target lvalues never arise: device equations assign one named target
  each (this is what replaces Verilog's `assign {a, b} = e` and the VHDL
  workaround for it).

### 8.2 Expressions

| construct        | Verilog                  | VHDL                                          | Cryptol |
|------------------|--------------------------|-----------------------------------------------|---------|
| `lit n v`        | `n'h…`                   | sized literal / `x"…"`                        | `(0x… : [n])` or binary |
| `undef n`        | `n'h0` (+ comment)       | zeros (+ comment)                             | `(zero : [n])` |
| `e₁ ⧺ e₂`        | `{e₁, e₂}`               | `e₁ & e₂`                                     | `e₁ # e₂` |
| `e[i +: k]`      | `e[i+k-1 : i]`           | `e(i+k-1 downto i)`                           | `take`{k} (drop`{n-i-k} e)` |
| `add` …          | `+` etc., widths equal   | `std_logic_vector(unsigned(a) + unsigned(b))` | `+` etc. |
| `udiv`/`umod`    | guard mux + `/`, `%`     | `rw_helpers` guarded div/mod                  | guard + `/`, `%` |
| `shl/lshr/ashr`  | `<<`,`>>`,`$signed(a)>>>`| `shift_left/right` with clamped amount        | `<<`, `>>`, `>>$` with width fix |
| `slt`/`sle`      | `$signed(a) < $signed(b)`| `signed(a) < signed(b)`                       | sign-flip compare |
| `zext⟨m⟩`        | implicit / `{pad, e}`    | `rw_resize`-style pad                         | `zero # e` |
| `sext⟨m⟩`        | `$signed` context or rep | `resize(signed(e), m)`                        | rep sign ⧺ e |
| `if`             | `c ? a : b`              | `when`/`else` (widths now always equal)       | `if c == 1 then a else b` |
| `f(ē)`           | instantiation + wire     | component instantiation (named ports)         | application |
| comb extern call | instantiation            | component instantiation                       | `parameter` fn or model call |

Because operand and result widths of every operator are equal by
construction, Verilog's context-determined sizing always agrees with the IR
(no `wcast`), and VHDL's exact-width rules are satisfied without resizing
helpers — `rw_helpers` shrinks to the division guards and shift-amount
clamping, the only places where the *operation* (not the widths) needs help.

### 8.3 Devices

- **Verilog**: one top module; each register ↦ declared `reg` with
  `initial`, all updated in one `always @(posedge clk, …)` block (reset
  branch loads the initials); each device `let` ↦ named wire + assign; each
  instance ↦ module instantiation with named port connections; outputs ↦
  continuous assigns.
- **VHDL**: entity + architecture; registers ↦ signals with `:=` initials
  updated in one `process (clk, rst)`; lets ↦ signals + concurrent
  assignments; instances ↦ component instantiations (named association;
  clock/reset connected directly, never through an intermediate signal — the
  delta-cycle rule); outputs ↦ concurrent assignments.
- **Cryptol**: the §6.4 recurrence as the existing `rw_device` idiom:
  `sts = [⟨w̄⟩] # [ step s i | s <- sts | i <- ins ]`, projecting outputs;
  instances are rejected (no pure realization), model-less comb externs go
  to a `parameter` block.
- **Interpreter**: transcribe §6.3/§6.4 literally over `BV`.

### 8.4 The Cryptol embedding ⌈·⌉

⌈[n]⌉ = `[n]`. Variables, lets, ifs, calls map one-to-one. The only
non-identity cases: slices flip to MSB-first (`e[i +: k]` on e : [n] becomes
`take`{k} (drop`{n−i−k} e)`); `udiv`/`umod` wrap Cryptol's (division-by-zero
erroring) `/`/`%` in the §5.2 guard; coercions and reductions map to small
fixed helper definitions (the current `rw'resize`/`rw'parity` family); names
are legalized with the current `rw_`-prefix scheme. Everything else is a
homomorphism. A device with neither instances nor model-less externs yields
a closed Cryptol module whose `rw_device` is extensionally 𝔇⟦device⟧ — the
artifact SAW consumes.

### 8.5 Reset refinement (RTL targets)

The abstract semantics has no reset. The RTL realization must satisfy: for
the configured reset protocol (assert reset for ≥ 1 cycle, deassert; sync or
async, either polarity), the post-deassertion sampled trace of the emitted
design equals 𝔇⟦device⟧ applied to the post-deassertion sampled inputs, and
likewise the no-reset power-on trace (via `initial`/`:=` initials) — this is
exactly what the existing cosimulation harness checks, generalized from the
current backends' hand-built state machines to a per-register rule.

### 8.6 Names and zero-width values

Names are legalized per target at print time (Verilog: `Core.Mangle`-style
z-encoding; VHDL: extended identifiers for anything that isn't a safe basic
identifier; Cryptol: sanitize + `rw_` prefix + dedup), never in the IR.
Zero-width wires, ports, registers, lets, and expression positions are
erased entirely by every printer (sound by §7); a defn or extern whose result
is [0], or a device port of width [0], simply vanishes from interfaces.

## 9. Relation to the previous Core IR (historical)

### 9.1 Construct mapping

| Core (current)                          | Mantle |
|-----------------------------------------|--------|
| `Lit a bv`                              | `lit n v` |
| `LVar a sz i`                           | named parameters / `let` |
| `Concat a e₁ e₂`                        | `e₁ ⧺ e₂` |
| `Call sz (Global g) disc ps els`, nil els | `let d = disc in g(d[…+:…], …)` — slices per `PatVar`, match assumed |
| `Call sz (Global g) disc ps els`        | `let d = disc in if ⋀ eq(d[…], litᵢ) then g(…) else els` |
| `Call sz (Prim p) …`                    | §3.3 op, same call/guard scheme; see prim table below |
| `Call sz (Const bv) …`                  | guarded literal |
| `Call sz (Extern … Nothing) …` unclocked| comb extern call (guarded as above) |
| `Call sz (Extern … (Just m)) …`         | comb extern call; `model m` on the declaration |
| clocked extern call                     | device-level `instance` + port equations (hoisted by the producer) |
| `SetRef r` / `GetRef r`                 | device-level `let r = …` / reference to it (hoisted by the producer) |
| `Defn`/`Sig`                            | defn (named params instead of positional LIds) |
| `Device`/`Wiring`/`loop`/`state0`       | `device`: registers + equations; resumption layout becomes explicit producer-emitted slices |
| `pausePadding`/`dispatchWires`/`resumptionSize` | gone (producer-internal) |
| nil-else don't-care convention          | unconditional call / explicit `if` |
| implicit context widths                 | explicit `zext`/`sext`/`trunc` |

Prim mapping for the constructors that don't carry over one-to-one:

| Core `Prim`         | Mantle |
|---------------------|--------|
| `LAnd`/`LOr`        | `and`/`or` at [1] over `redor`-coerced operands |
| `LNot`              | `not(redor(x))` |
| `XNor`              | `not(xor(x, y))` |
| `RNAnd`/`RNor`/`RXNor` | `not(red…)` |
| `MSBit`             | `x[n−1 +: 1]` |
| `Resize`            | `zext⟨m⟩` or `trunc⟨m⟩` (chosen statically) |
| `Reverse`           | concatenation of element slices |
| `Id`                | slices/concatenation (pattern apply) |
| `Replicate k`       | `rep⟨k⟩` |
| `RShift`/`RShiftArith` | `lshr`/`ashr` |

### 9.2 Why expression-structured rather than an RTLIL-style netlist

The retargeting postmortem (`rtl-backends.md`) sketched an RTLIL-like
successor. Mantle adopts every load-bearing item from that sketch — explicit
widths and conversions (its point 1), fixed-width named ports with explicit
signedness per operation (2), explicit registers with initials and reset
metadata (3), no multi-target lvalue shapes (4), mandatory named extern
ports (5), abstract names legalized per target (6) — but realizes them in a
functional expression language rather than a cell graph, because:

- two of the four consumers (Cryptol, the interpreter) are functional, and
  the project's distinguishing goal — verification via Cryptol/SAW and
  Isabelle — wants readable, structured functional output, which a netlist
  would have to *reconstruct*;
- the producer (`Crust.Purify`/`ToCore`) already emits a functional program;
  lowering to a netlist and re-deriving expression structure in two back
  ends is two transformations where zero are needed;
- the RTL emission benefits claimed for netlists come from the width/port
  explicitness, not from graph structure — with G1–G4, expression emission
  is the same per-template exercise (§8.2);
- a Mantle→RTLIL (or structural-Verilog-for-Yosys) emitter remains a small
  addition later, by exactly the §8 argument.

## 10. Concrete syntax

The textual format (printed by `rwc --core`-equivalent, parsed by the
`--from-core`-equivalent; round-tripping, used by the `.rwc`-style goldens):

```
program  ::= { decl }
decl     ::= extern | defn | device

extern   ::= "extern" name { genclause | portclause } [ "model" name ]
genclause ::= "generic" ident { "," ident }
            | "clock" ident | "reset" ident
portclause ::= ("input" | "output") ident ":" type

defn     ::= name ":" "(" [ type { "," type } ] ")" "->" type
             name { ident } "=" exp

device   ::= "device" name { devdecl } { stmt }
devdecl  ::= "input" ident ":" type
           | "output" ident ":" type
           | "register" ident ":" type "init" lit
           | "instance" ident "of" name [ "<" nat { "," nat } ">" ]
stmt     ::= "let" ident "=" exp
           | ident ":=" exp                    -- output
           | "next" ident ":=" exp             -- register
           | ident "." ident ":=" exp          -- instance input port

type     ::= "[" nat "]"
exp      ::= "let" ident "=" exp "in" exp
           | "if" exp "then" exp "else" exp
           | cat
cat      ::= app { "#" app }                   -- concat, associative
app      ::= prim { atom } | name [ "<" nat { "," nat } ">" ] { atom } | atom
atom     ::= lit | "undef" nat | ident | atom "[" nat "+:" nat "]" | "(" exp ")"
lit      ::= nat "'h" { hexdigit } | "0'"      -- e.g. 8'hff ; width 0: 0'
prim     ::= "add" | "sub" | … | "zext" nat | "sext" nat | "trunc" nat | "rep" nat
ident    ::= [A-Za-z_][A-Za-z0-9_.$]* | '"' … '"'   -- quoted form for arbitrary names
```

(Annotations are not part of the concrete syntax; the parser fills them with
file positions, as `Core.Parse` does today.) Example:

```
-- f computes (x + y) zero-extended, unless x < y.
f : ([8], [8]) -> [16]
f x y =
      let s = zext 16 (add x y) in
      if ult x y then s else (x # y)

extern fifo
      generic depth
      clock clk
      reset rst
      input  din  : [32]
      output dout : [32]

device Main
      input  x : [8]
      output y : [8]
      register s : [8] init 8'h0
      instance q of fifo<16>
      let t = trunc 8 (f s x)
      q.din  := zext 32 t
      y      := t
      next s := if eq t 8'hff then 8'h0 else t
```

The grammar is whitespace-insensitive (every clause begins with a keyword or
an identifier in a determined position); the printer uses the layout above.
Operator applications are prefix and parenthesized like Cryptol/Haskell
application, so the expression fragment reads as (and trivially maps to)
Cryptol.

## 11. Resolved design questions

Decided at design review (June 2026):

1. **Name.** "Mantle", permanently; no end-of-migration rename back to
   `ReWire.Core.*`.
2. **`undef` in goldens.** Prints distinctly as `undef n` (provenance in
   goldens outweighs diff noise).
3. **Greater-than prims.** `ugt`/`uge`/`sgt`/`sge` are *included*:
   readability of the generated IR and RTL is a major goal, and operand
   swapping works against it.
4. **Signed division.** Excluded for now (no current source-language route
   to it); the SMT-LIB conventions extend naturally if `sdiv`/`smod` are
   ever needed.
5. **Instance output latency.** Strict causality (§6.4) stands; no support
   for combinational-feedthrough externs for now (an explicit per-port
   `comb` annotation with a device-wide acyclicity check is the principled
   extension if ever required).

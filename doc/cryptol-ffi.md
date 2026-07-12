# The Cryptol foreign-function interface

ReWire programs can call [Cryptol](https://cryptol.net) functions and have
them realized in hardware alongside the rest of the program. Unlike an
`extern` — a black box the synthesis toolchain fills in with hand-written
HDL — a Cryptol foreign function is *compiled*: it appears in the generated
Verilog, VHDL, and Cryptol output, and the interpreter can evaluate it. The
Cryptol module is loaded and typechecked by the Cryptol implementation
itself, so the function is checked against the use-site type before a single
gate is emitted.

    GHC (Haskell types)  →  Cryptol typechecker (Cryptol types)  →  Hyle checker (bit widths)

This document describes what the interface is, the fragment of Cryptol it
supports, the bit-level representation contract, and the handful of places
where the hardware semantics deliberately differ from Cryptol's.

## 1. Using it

`ReWire.Cryptol.cryptol` (in `rewire-user`) has the shape of `extern`:

```haskell
import ReWire.Cryptol (cryptol)

-- Compile `bytemap` from crypto.cry at the type W 16 -> W 16.
crysub :: W 16 -> W 16
crysub = cryptol "crypto.cry" "bytemap" crysub
```

The three arguments are the Cryptol module file, the function name, and a
Haskell implementation used when the program runs under GHC (not `rwc`).
The self-reference idiom `f = cryptol "f.cry" "f" f` means "no Haskell
implementation" — exactly the `extern` model convention. The first two
arguments must be string literals; the use-site type must be monomorphic.

The `.cry` path is resolved against the calling module's directory, then the
`--loadpath`. Cryptol typechecking needs **`z3` on the PATH**, and the
translation is done by a separate executable, **`rwcry`**, that `rwc`
invokes out of process (found next to `rwc`, then on the PATH;
`RWC_RWCRY` overrides). `rwc` itself does not link the Cryptol toolchain.

## 2. The supported fragment

The interface realizes *combinational* Cryptol: first-order functions whose
inputs and outputs are finite bit-shaped values. Everything a Cryptol
function is elaborated to — every Core expression form, every prelude
primitive it calls — is either translated or rejected with a located error.

| Area | Supported |
|---|---|
| Scalars | `Bit`, words `[n]`, tuples, sequences `[n]a`; if-then-else; local `let`/`where` value bindings |
| Records & nominal types | records (construction, field selection, `{ r \| f = v }` update), newtypes (transparent), `enum`s with `case` — **interior-only** (see §3) |
| Modular | `Z n` as a bounded word; `Ring` ops, `fromInteger`, and the prime-field inverse `recip`/`(/.)` (Fermat) — **interior-only** |
| Functions | local function bindings; higher-order functions applied to statically known functions (named, lambdas, `curry`/`uncurry`, or chosen by `if`/`case`) |
| Recursion | type-indexed recursion (each call at a strictly smaller instantiation, e.g. `` pow`{e-1} ``); recursive *finite* comprehensions (`ys = a # [ f y | y <- ys ]`) |
| Sequences | comprehensions (fully unrolled), `foldl`/`foldr`/`scanl`, `@`/`!` (constant or variable index), `# take drop head last tail reverse split join splitAt transpose update updateEnd` rotates `<<< >>>`, shifts |
| Arithmetic | `+ - * / % ^^ negate` and the bitwise/comparison/reduction ops, signed `/$ %$ <$ …`, `min max sum product`, `lg2`, carry-less `pmult pdiv pmod` (constant divisor), the enumeration primitives (`[a..b]`, `[a,b..c]`, …) |
| Streams | infinite streams (`infFrom`, `infFromThen`, `iterate`, `repeat`, recursive `[inf]` definitions) when demand is statically bounded — consumed by a finite `take` or a constant index (see §5) |
| Modules | submodules, parameterized (functor) submodule instantiations, qualified names, multi-file imports |

Anything a *defined* prelude function is built from — `map`, `zipWith`,
`zip`, `sort` (as a network), `elem`, `generate`, `iterate`, … — comes along
for free once its defining constructs translate.

## 3. The representation contract

The translation is the inverse of the width-preserving embedding the Cryptol
backend uses (`doc/hyle.md` §8.4):

- `Bit` is one bit; a word `[n]` is a Hyle bitvector of the same numeric
  value and width `n`.
- In a sequence or tuple, **element 0 sits at the most-significant end**.
  `split`, `join`, and `splitAt` are therefore bit-identities.
- A record lays out like a tuple, fields in Cryptol's canonical
  (label-sorted) order.
- An `enum` value is `tag # padding # arguments`: the constructor's
  declaration index in `nbits(#constructors)` bits at the most-significant
  end, zero-padded to the widest constructor, then the argument bits — the
  same convention the Eidos fold uses for ReWire ADTs.
- `Z n` is `[nbits n]` holding the residue.

**Interior-only.** Records, enums, and `Z n` are supported *inside* a
Cryptol function but not at the FFI boundary: the entry point's type — the
Haskell type of the `cryptol` binding — must be built from words, vectors,
and tuples, because the ReWire side has no Cryptol-record counterpart. Wrap
a record/enum result in a word (`join`, `fromZ`+`fromInteger`, an explicit
`#`) at the boundary.

## 4. Typechecking, in three layers

1. **GHC** checks the Haskell side of the `cryptol` binding.
2. **Cryptol's own typechecker** loads the module (needs `z3`) and checks
   the requested function against the use-site type — mapped to Cryptol
   syntax — so a type mismatch is Cryptol's own error. This is also where an
   interior-only type used at the boundary is rejected (it cannot unify with
   the word/vector/tuple boundary type).
3. **The Hyle checker** guards the spliced-in definitions' widths.

## 5. Bounded-demand streams

An infinite stream is realizable only when how much of it is used is known at
compile time. The realizable consumers are a finite `` take`{k} `` and a
constant `@` index; a finite comprehension arm zipped against an infinite one
demands the finite length. Under such a consumer, `infFrom`, `infFromThen`,
`iterate`, `repeat`, an append with a finite front, `drop`, and a recursive
stream definition (`s = front # [ f s … | … ]`, unrolled element by element)
all realize their demanded prefix. A variable index, a reverse index (`!`),
or a bare `drop` of an infinite stream has *unbounded* demand and is
rejected — take a finite prefix or use a constant index.

## 6. Semantic-divergence ledger

The hardware semantics is total and deterministic; a few Cryptol behaviours
have no hardware counterpart and are given a defined one, documented here:

| Cryptol | Hardware realization |
|---|---|
| division / modulo by zero | SMT-LIB semantics (`x/0` is all-ones, `x%0` is `x`) — the same on every backend |
| `error`, `assert`, `undefined` | a zero "poison" constant of the result width; the result is defined but meaningless where the error would fire (a compile-time warning is issued) |
| `trace`, `traceVal` | the identity on the result (no I/O in hardware; a warning is issued) |
| `recip 0` / division by 0 in `Z p` | undefined in a field — Cryptol raises; the Fermat form gives 0. Do not rely on it. |
| `foldl'`, `deepseq`, `parmap` | their lazy/sequential equivalents (evaluation in hardware is total and parallel, so the distinction is invisible) |

## 7. Excluded: what would need a bigger ReWire

These require extending the ReWire/Hyle language, not just the translator,
and are rejected with a located error:

- **Floating point** (`Float e p` and its primitives).
- **`Integer`/`Rational` at runtime** — unbounded width. (Type-level
  naturals, demoted literals, constant-folded `Integer` arithmetic, and
  `Integral ix` at a word type all still work.)
- **Infinite streams as values** escaping to the boundary, or with unbounded
  demand — the faithful encoding is a clocked sub-FSM, a separate future
  feature. (Bounded demand works; see §5.)
- **Value-dependent recursion** (recursion whose depth depends on runtime
  data) — no finite unrolling exists. (Type-indexed and bounded-finite
  recursion do work.)
- **`random`**, **SMT arrays** (`Array.cry`), and the implementation-backed
  primitive modules (`SuiteB.cry`, `PrimeEC.cry`).
- **Cryptol `foreign` (C FFI) functions** with no Cryptol fallback body.
- **Uninstantiated parameterized modules** — point the FFI at an
  instantiation instead.

## 8. Sizing

Unrolling multiplies: a comprehension of length *n*, a fold over *n*
elements, a recursion of depth *d*, and a `Z p` inverse of *≈2·log₂ p*
modular multiplies all expand at compile time. A translation that exceeds a
node budget (default two million; set `RWC_CRY_MAX_NODES`, or `0` to
disable) fails with an actionable message rather than hanging a downstream
tool. Prefer table lookups to large `Z p` inverses, and keep unrolled
dimensions concrete and modest. As a reference point, a full AES-128 block
encryption and a SHA-256 block compression each translate to a few hundred
Hyle definitions and lint and simulate without trouble
(`tests/golden/aes128.hs`, `tests/golden/sha256ffi.hs`).

## 9. Under the hood

`rwc` scans the program for Cryptol foreign-function uses and invokes `rwcry`
once per distinct `(file, function, use-site type)`. `rwcry` loads and
typechecks the module, elaborates the instantiation `fn : type`,
monomorphizes it with Cryptol's specializer, and translates the resulting
closure of monomorphic definitions to a defns-only Hyle fragment, which `rwc`
splices into the program under `cry$<fn>$<i>`-prefixed names. The Cryptol
library is pinned to a specific commit (the typechecked-AST surface is not a
stable API); `rwcry` lives in the `rewire-cryptol` package, and `rwc` does
not depend on it. See `rewire-cryptol/src/ReWire/Cryptol/Translate.hs`.

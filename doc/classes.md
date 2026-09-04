# Type classes in ReWire

ReWire programs can define and use Haskell type classes. Classes compile by
ordinary dictionary passing — a class becomes a record of its methods, an
instance becomes a definition of that record, and a method call projects a
field — and the compiler's specialization and partial evaluation then
eliminate every dictionary at compile time. No dictionary, method table, or
dispatch logic survives into the generated hardware: a class-using program
synthesizes to exactly the circuit its specialized form describes, at zero
overhead. A program whose dictionaries *cannot* be resolved statically does
not compile (there is no runtime to defer to).

## What works

- **Classes and instances**, with any number of methods. (Single-method
  classes are represented specially by GHC — a newtype rather than a data
  dictionary — but compile the same.)
- **Superclasses** (`class Frob a => Blork a`), including methodless
  subclasses used as constraint aliases, and superclass methods reached
  through a subclass constraint.
- **Default methods**, including empty instances
  (`instance Frob Bool` with every method defaulted) and
  `DeriveAnyClass` (`data T = ... deriving Frob` for an all-defaults class).
- **Constraint-polymorphic functions** (`twice :: Frob a => a -> a`), used
  at any number of instance types in one design.
- **Instances with contexts**, resolved statically:

  ```haskell
  instance (Frob a, Frob b) => Frob (a, b)     -- class contexts
  instance KnownNat n => Frob (W n)            -- a width-polymorphic instance
  instance Frob a => Frob (Vec 4 a)            -- element contexts
  ```

- **Multi-parameter type classes** (`MultiParamTypeClasses`).
- **Classes across modules**: a class in one module, instances in another
  (orphans included), uses in a third.
- **Methods with reactive types** (`ReacT`/`Dev` results): dictionaries are
  eliminated before purification, so class-selected state machines compose
  like any others.
- **`INLINE`/`NOINLINE` pragmas on instance methods** carry through; a
  `NOINLINE` method does not obstruct dictionary elimination.
- **Marker (zero-method) classes with a kind annotation**:
  `class Marked (a :: Type)`. (Without the annotation GHC kind-generalizes
  the dictionary and the program is rejected — see below.)
- **Constraint-bearing constructor fields**
  (`data Box a where Box :: Frob a => a -> Box a`) — as long as every
  `case` on such a value can see the construction site after inlining.
  Scrutinize where the value is built, or mark the consuming function
  `INLINE`.

## What is rejected (each with a targeted error)

- **Methods of external classes** — `Eq`, `Ord`, `Show`, `Semigroup`,
  `Num`, and everything else not defined in your program. Their evidence is
  erased, whether the instance is derived (`deriving Eq`) or hand-written;
  a use of `==` at synthesis reports
  `unsupported use of a type class method: Eq.==`. Define your own class
  (or use the operators from `ReWire.Bits`) instead of instancing a base
  class.
- **`GeneralizedNewtypeDeriving` / `DerivingVia`** — the derived methods
  are the underlying instance's methods coerced across the newtype, and
  that coercion is not erasable; the program is rejected with a type
  mismatch located at the deriving clause (e.g.
  `has type Bool -> Bool but Main.N -> Main.N is expected`). Write the
  instance by hand.
- **Higher-rank methods** (a method with its own type variable, e.g.
  `pad :: a -> b -> a`): the dictionary field would be a polytype
  (`unsupported higher-rank type`).
- **Unannotated marker classes** (`class Marked a` with no methods, no
  superclass, no kind annotation): GHC kind-generalizes the dictionary
  constructor and the kind argument leaks
  (`type not in the ReWire vocabulary: GHC.Prim.TYPE`). Annotate the
  parameter: `class Marked (a :: Type)`.
- **Recursive class dictionaries** (only expressible with
  `ConstraintKinds`): `unsupported recursive class dictionary`.
- **Instances of `Monad` and friends for your own types** — the reactive
  monad stack (`ReacT`/`StateT`/`Identity`) is fixed, and monadic
  operators at any other monad are rejected
  (`monadic operator at unsupported monad`).

## Things to know

- **Module naming**: a class counts as a *user* class when it is defined in
  one of your own modules, judged by module-name namespace. A user module
  named under `GHC.*`, `Data.*`, `Control.*`, `System.*`, `Foreign.*`,
  `Text.*`, or `Unsafe.*` is treated as external — its classes' evidence
  is silently erased and method uses are rejected as external-class
  methods. Name your modules outside those namespaces.
- **Deep hierarchies and `--depth`**: dictionary elimination runs inside
  the partial-evaluation fixpoint, bounded by `--depth` (default 8). A
  very deep superclass/instance-context chain can need more rounds; raise
  the bound with `--depth N`. The same flag governs monomorphization: a
  chain of more than ten *distinct* constraint-polymorphic definitions,
  each calling the next, exceeds the specializer's generation bound
  (`Polymorphic function instantiation not terminating`) — also fixed by
  `--depth`.
- **A default method that calls its own sole method**
  (`frob x = frob x`-style knots) is an infinite loop in Haskell too; rwc
  rejects it as unsupported recursion.
- **Point-free method aliases at the method's own type**
  (`f :: Frob a => a -> a; f = frob`) desugar to a bare selector reference
  and are rejected (`unapplied class method`); eta-expand (`f x = frob x`).
  An alias at a *different* constraint (say, aliasing a superclass method
  under a subclass constraint) is fine — the desugarer inserts the
  adaptor itself.
- **The embedder (`rwe`) does not accept classes**: class and instance
  declarations — in the program or any module it imports — fail the
  embedder's front end. Type classes are an `rwc` feature.

## How it compiles (in brief)

The GHC front end desugars classes to Core dictionaries; the bridge keeps
user-class dictionaries as ordinary data (constraints become value
arrows, instance definitions become inlinable definitions, method calls
become field projections) while erasing built-in evidence (`KnownNat`,
`Monad`, ...). The Eidos passes then monomorphize by specialization,
inline the instance dictionaries, and partial-evaluate until every
definition is representable and dictionary-free — a fixpoint the compiler
*requires*: if a dictionary survives (for example, one selected by a
runtime value), compilation fails rather than emitting dynamic dispatch.
See `doc/eidos.md` for the IR-level story.

# Crossbar Switch

## What’s a Crossbar Switch?

To perform this exercise, I relied primarily on two sources to explain what a crossbar switch is; they are:

* [Wikipedia](https://en.wikipedia.org/wiki/Crossbar_switch)
* [The Crossbar Switch](http://www.cs.emory.edu/~cheung/Courses/355/Syllabus/90-parallel/CrossBar.html)

Given these explanations, I generated a Haskell implementation of a crossbar switch like function (see CrossbarSwitch.hs below). All the Haskell and ReWire code for this example can be found below.

* [CrossbarSwitch.hs](code/CrossbarSwitch.hs)

What follows is an explanation of this code. First, we consider the Haskell definition of a crossbar switch, written in monadic style. Then, we transform the Haskell definition of the switch into proper ReWire. This is important because it gives you a practical introduction to the differences between Haskell and ReWire..

### Just write it in Haskell first, then add a few bits to get your program into ReWire.

The usual mode of program development is to first write a version of the desired application in Haskell. The reasons to do this boil down to the GHC compiler being vastly more mature than the ReWire compiler, and so, for example, error messages are much more informative. Once all the kinks as it were are worked out in Haskell (e.g., getting something that typechecks, etc.), make whatever small tweaks are needed to get your program into the ReWire subset of Haskell. This section of the tutorial introduces the reader to this mode of program development.

#### The ReWire standard library
The standard definitions for ReWire programs live in the `rewire-user` package, which is bundled with the compiler; you bring them into scope with `import ReWire`. It includes the bit type (`Bit`), words of arbitrary width (`W n`, with the width as a type-level natural), and the device monad types (`ReacT`, `StateT`, `Identity`). Operations on words and vectors are in `ReWire.Bits` and `ReWire.Vectors`. Everything in these modules works with both `rwc` and GHC.

What follows is a crossbar switch function written in Haskell. We will take this as an input specification, by which we mean that it is not terribly important to actually understand what the crossbar function is calculating. Rather, what is interesting is what (little) must change in this specification to transform it into a proper ReWire specification.

```haskell
{-# LANGUAGE DataKinds #-}

import Prelude hiding ((^), (+))
import ReWire

switch :: t -> t -> Bool -> (t, t)
switch x _ True  = (x,x)
switch x y False = (x,y)

type W8 = W 8

data Maybe4 = Maybe4 (Maybe W8) (Maybe W8) (Maybe W8) (Maybe W8)

type Bool16 = (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool)

crossbar :: Maybe4 -> Bool16 -> Maybe4 
crossbar (Maybe4 x10 x20 x30 x40) (c11,c12,c13,c14,c21,c22,c23,c24,c31,c32,c33,c34,c41,c42,c43,c44)
   = let
          (x41,y31) = switch x40 Nothing c41
          (x42,y32) = switch x41 Nothing c42
          (x43,y33) = switch x42 Nothing c43
          (_,y34) = switch x43 Nothing c44

          (x31,y21) = switch x30 y31 c31
          (x32,y22) = switch x31 y32 c32
          (x33,y23) = switch x32 y33 c33
          (_,y24) = switch x33 y34 c34

          (x21,y11) = switch x20 y21 c21
          (x22,y12) = switch x21 y22 c22
          (x23,y13) = switch x22 y23 c23
          (_,y14) = switch x23 y24 c24

          (x11,y10) = switch x10 y11 c11
          (x12,y20) = switch x11 y12 c12
          (x13,y30) = switch x12 y13 c13
          (_,y40) = switch x13 y14 c14
     in
       Maybe4 y10 y20 y30 y40

data Inp = Inp Maybe4 Bool16 | NoInput
            
data Out = Out Maybe4 | Nix

dev :: Inp -> ReacT Inp Out Identity ()
dev (Inp m4 b16) = signal (Out (crossbar m4 b16)) >>= dev
dev NoInput      = signal Nix >>= dev

start :: ReacT Inp Out Identity ()
start = signal Nix >>= dev
```

## Crossbar Switch in ReWire

So what must change to compile this with `rwc`? Happily, the days when this
section described a long list of tweaks (rewriting `let` as `where`, manually
inlining polymorphic functions, expanding type synonyms, and so on) are gone:
the code above is already a valid ReWire program, exactly as written in
[CrossbarSwitch.hs](code/CrossbarSwitch.hs). A few things are still worth
knowing when adapting a Haskell program for `rwc`:

- **Imports.** Import `ReWire` (and friends like `ReWire.Bits`) instead of
  `Control.Monad.Identity`, `Control.Monad.State`, and so on -- the monad
  types (`Identity`, `StateT`, `ReacT`) are built in to ReWire and re-exported
  by the `ReWire` module.

- **Modules.** If the file has no module header, it is treated as `Main`, and
  `rwc` looks for the start symbol `Main.start` by default. If you put your
  device in a different module, point the compiler at its start symbol with
  `--start`.

- **Type signatures.** Give every top-level definition a type signature.
  (Signatures are how definitions are checked against their uses, so leaving
  them off leads to error messages pointing at surprising places.)

- **GHC-only code.** Anything `rwc` can't compile -- lists used at the top
  level, `ReWire.Interactive` imports for GHCi experimentation, class
  instances -- needs to be commented out (or moved to a separate file) before
  compiling. The `main` definition is the one exception: `rwc` ignores it.

Once that's settled, compile with the ReWire compiler `rwc`:

```
$ rwc CrossbarSwitch.hs
$ ls CrossbarSwitch.*
CrossbarSwitch.hs CrossbarSwitch.sv
```

By default `rwc` produces SystemVerilog (`.sv`); see `rwc --help` for other
targets and options. Depending on how successful one's translation into
ReWire is, one may receive error messages from the ReWire compiler. These are
improving, although there is admittedly much room for improvement as of this
writing.

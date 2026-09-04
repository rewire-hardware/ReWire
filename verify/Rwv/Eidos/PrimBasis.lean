/-
The primitive datatype basis for Eidos programs, a transcription of
rewire-frontend ReWire.Eidos.PrimBasis: the built-in type constructors
(with their data constructors, where they have any) that bridged
programs may reference without declaring — the unit and tuple families
(2..62), Bool, Maybe/Either (the GHC-internal dotted names, as the
bridge emits them), the abstract width-bearing types (Vec, Finite,
Proxy), and the reactive stack types (which exist only in Eidos, before procification).
The bridge prepends these to every program so consumers can resolve
constructor occurrences (`Rwv.Synolon.addPrims`, the program-level
entry point over `primDatas`).

The basis' type-variable uniques are NEGATIVE: the bridge mints
non-negative uniques, so basis binders can never collide with program
binders. Each declaration's variables live in a distinct hundred,
exactly as the Haskell mints them (`basisTv`).
-/
import Rwv.Eidos.Syntax

namespace Rwv.Eidos

namespace PrimBasis

private def monoSig (t : Ty) : Sig := ⟨[], t⟩

private def mkData (n : String) (k : Kind) (cs : List DataCon) : DataDefn :=
  { name := n, kind := k, cons := cs }

private def nullCtor (c t : String) : DataCon :=
  { name := c, sig := monoSig (.con t) }

/-- Basis type variables: negative uniques, disjoint per declaration
(each declaration gets a distinct hundred). -/
private def basisTv (decl i : Nat) (x : String) (k : Kind) : TyVar :=
  { occ := x, uniq := -(Int.ofNat (decl * 100 + i + 1)), kind := k }

private def kmonad : Kind := .fn .star .star

private def proxyCtor : DataCon :=
  let n := basisTv 1 0 "n" .nat
  { name := "Proxy", sig := ⟨[n], .app (.con "Proxy") (.var n)⟩ }

private def maybeData : DataDefn :=
  let tv := basisTv 2 0 "a" .star
  let mt : Ty := .app (.con "GHC.Internal.Maybe.Maybe") (.var tv)
  { name := "GHC.Internal.Maybe.Maybe"
  , kind := .fn .star .star
  , cons :=
      [ { name := "GHC.Internal.Maybe.Nothing", sig := ⟨[tv], mt⟩ }
      , { name := "GHC.Internal.Maybe.Just",    sig := ⟨[tv], .arrow (.var tv) mt⟩ } ] }

private def eitherData : DataDefn :=
  let tva := basisTv 3 0 "a" .star
  let tvb := basisTv 3 1 "b" .star
  let et : Ty := .app (.app (.con "GHC.Internal.Data.Either.Either") (.var tva)) (.var tvb)
  { name := "GHC.Internal.Data.Either.Either"
  , kind := .fn .star (.fn .star .star)
  , cons :=
      [ { name := "GHC.Internal.Data.Either.Left",  sig := ⟨[tva, tvb], .arrow (.var tva) et⟩ }
      , { name := "GHC.Internal.Data.Either.Right", sig := ⟨[tva, tvb], .arrow (.var tvb) et⟩ } ] }

private def tupleTvName (i : Nat) : String :=
  if i < 26 then String.singleton (Char.ofNat ('a'.toNat + i))
  else "t" ++ toString i

private def mkTuple (n : Nat) : DataDefn :=
  let name := "(" ++ String.ofList (List.replicate (n - 1) ',') ++ ")"
  let tvs  := (List.range n).map fun i => basisTv (10 + n) i (tupleTvName i) .star
  let k    := (List.replicate n Kind.star).foldr .fn .star
  let rt   := tvs.foldl (fun acc v => .app acc (.var v)) (Ty.con name)
  { name := name
  , kind := k
  , cons := [{ name := name, sig := ⟨tvs, tvs.foldr (fun v acc => .arrow (.var v) acc) rt⟩ }] }

end PrimBasis

open PrimBasis in
/-- The primitive basis, in the order the Haskell declares it. -/
def primDatas : List DataDefn :=
  [ mkData "()"       .star                                          [nullCtor "()" "()"]
  -- (The type-level arithmetic constructors +, -, * are recognized by
  -- name and need no declaration; their kinds construct Nat, not *, so
  -- they are not datatypes.)
  , mkData "Bool"     .star                                          [nullCtor "False" "Bool", nullCtor "True" "Bool"]
  , mkData "ExtDev"   (.fn .star (.fn .star .star))                  []
  , mkData "Finite"   (.fn .nat .star)                               []
  , mkData "Identity" kmonad                                         []
  , mkData "Integer"  .star                                          []
  , mkData "Proxy"    (.fn .nat .star)                               [proxyCtor]
  , mkData "ReacT"    (.fn .star (.fn .star (.fn kmonad kmonad)))    []
  , mkData "StateT"   (.fn .star (.fn kmonad kmonad))                []
  , mkData "String"   .star                                          []
  , mkData "Vec"      (.fn .nat (.fn .star .star))                   []
  , mkData "[_]"      (.fn .star .star)                              []
  , mkData "[]"       (.fn .star .star)                              []
  , maybeData
  , eitherData
  ]
  ++ ((List.range 61).map fun i => mkTuple (i + 2))

end Rwv.Eidos

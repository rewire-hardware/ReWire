/-
The Eidos typed IR, deep-embedded: a transcription of the abstract
syntax of doc/eidos.md §3, mirroring rewire-frontend
ReWire.Eidos.Syntax (the machine level — cells, blocks, processes,
programs, doc/synolon.md §3.4 — is Rwv.Synolon.Syntax). As in the
implementation, there is no separate well-typed machine-level
datatype: the pre-ToHyle fragment is carved out of the one expression
type by the Synolon well-formedness judgment (doc/eidos.md §4.1 and
doc/synolon.md §4), which is where the lint invariants live.

Names are pairs of display text and unique; equality and hashing are
by unique only (§2). Annotations are omitted (semantically inert).
-/
import Std.Data.HashMap

namespace Rwv.Eidos

/-- The 60 builtins of ReWire.Builtins, plus four retired names
(`unfold`, `vecFoldR`, `vecFoldL`, `usingExtern`) the parser still
accepts so older dumps load — they have no doc/synolon.md §6 signature and no
denotation. Occurrences print as `rwPrim<Name>`; signatures and
machine-level denotations: doc/synolon.md §6. -/
inductive Builtin where
  | error | «extern» | cryptol
  | bind | ret
  | put | get
  | signal | lift | extrude | unfold
  | vecFromList | vecReplicate | vecReverse | vecSlice | vecRSlice
  | vecIndex | vecIndexProxy
  | vecConcat
  | vecMap | vecFoldR | vecFoldL | vecGenerate
  | finite | finiteMinBound | finiteMaxBound | toFinite | toFiniteMod | fromFinite
  | natVal
  | bits | resize | bitSlice | bitIndex
  | add | sub | mul | div | mod | pow
  | lAnd | lOr
  | and | or
  | xor | xnor
  | lShift | rShift | rShiftArith
  | eq | gt | gtEq | lt | ltEq
  | lNot | not
  | rAnd | rNAnd | rOr | rNor | rXOr | rXNor
  | msBit
  | usingExtern
deriving DecidableEq, Repr, Hashable

/-- The builtin's user-facing name (`rwPrim<Name>`), as printed in
`.eir` (matching ReWire.Builtins.builtinName). -/
def Builtin.name : Builtin → String
  | .error => "rwPrimError" | .«extern» => "rwPrimExtern" | .cryptol => "rwPrimCryptol"
  | .bind => "rwPrimBind" | .ret => "rwPrimReturn"
  | .put => "rwPrimPut" | .get => "rwPrimGet"
  | .signal => "rwPrimSignal" | .lift => "rwPrimLift" | .extrude => "rwPrimExtrude"
  | .unfold => "rwPrimUnfold"
  | .vecFromList => "rwPrimVecFromList" | .vecReplicate => "rwPrimVecReplicate"
  | .vecReverse => "rwPrimVecReverse" | .vecSlice => "rwPrimVecSlice"
  | .vecRSlice => "rwPrimVecRSlice" | .vecIndex => "rwPrimVecIndex"
  | .vecIndexProxy => "rwPrimVecIndexProxy" | .vecConcat => "rwPrimVecConcat"
  | .vecMap => "rwPrimVecMap" | .vecFoldR => "rwPrimVecFoldR"
  | .vecFoldL => "rwPrimVecFoldL" | .vecGenerate => "rwPrimVecGenerate"
  | .finite => "rwPrimFinite" | .finiteMinBound => "rwPrimFiniteMinBound"
  | .finiteMaxBound => "rwPrimFiniteMaxBound" | .toFinite => "rwPrimToFinite"
  | .toFiniteMod => "rwPrimToFiniteMod" | .fromFinite => "rwPrimFromFinite"
  | .natVal => "rwPrimNatVal"
  | .bits => "rwPrimBits" | .resize => "rwPrimResize"
  | .bitSlice => "rwPrimBitSlice" | .bitIndex => "rwPrimBitIndex"
  | .add => "rwPrimAdd" | .sub => "rwPrimSub" | .mul => "rwPrimMul"
  | .div => "rwPrimDiv" | .mod => "rwPrimMod" | .pow => "rwPrimPow"
  | .lAnd => "rwPrimLAnd" | .lOr => "rwPrimLOr"
  | .and => "rwPrimAnd" | .or => "rwPrimOr"
  | .xor => "rwPrimXOr" | .xnor => "rwPrimXNor"
  | .lShift => "rwPrimLShift" | .rShift => "rwPrimRShift"
  | .rShiftArith => "rwPrimRShiftArith"
  | .eq => "rwPrimEq" | .gt => "rwPrimGt" | .gtEq => "rwPrimGtEq"
  | .lt => "rwPrimLt" | .ltEq => "rwPrimLtEq"
  | .lNot => "rwPrimLNot" | .not => "rwPrimNot"
  | .rAnd => "rwPrimRAnd" | .rNAnd => "rwPrimRNAnd" | .rOr => "rwPrimROr"
  | .rNor => "rwPrimRNor" | .rXOr => "rwPrimRXOr" | .rXNor => "rwPrimRXNor"
  | .msBit => "rwPrimMSBit"
  | .usingExtern => "rwPrimUsingExtern"

/-- Kinds (§3.1). -/
inductive Kind where
  | star
  | nat
  | fn (k₁ k₂ : Kind)
deriving DecidableEq, Repr

/-- A type variable: display text, unique, kind. Equality by unique
only (§2). -/
structure TyVar where
  occ  : String
  uniq : Int
  kind : Kind
deriving Repr

instance : BEq TyVar := ⟨fun a b => a.uniq == b.uniq⟩
instance : Hashable TyVar := ⟨fun a => hash a.uniq⟩

/-- Types (§3.1): no binders, no quantifiers. Type-level arithmetic is
application of the built-in constructors `+`, `-`, `*`. -/
inductive Ty where
  | con   (t : String)
  | app   (t₁ t₂ : Ty)
  | var   (a : TyVar)
  | nat   (n : Nat)
  | arrow (t₁ t₂ : Ty)
deriving Repr, Inhabited, BEq

/-- Signatures (§3.2): quantification exists only here. -/
structure Sig where
  tvs : List TyVar
  ty  : Ty
deriving Repr, Inhabited

/-- A term name: display text, unique, signature. Equality by unique
only. -/
structure Id where
  occ  : String
  uniq : Int
  sig  : Sig
deriving Repr, Inhabited

instance : BEq Id := ⟨fun a b => a.uniq == b.uniq⟩
instance : Hashable Id := ⟨fun a => hash a.uniq⟩

/-- A join-point label: display text, unique, arity. -/
structure JoinId where
  occ   : String
  uniq  : Int
  arity : Nat
deriving Repr

instance : BEq JoinId := ⟨fun a b => a.uniq == b.uniq⟩
instance : Hashable JoinId := ⟨fun a => hash a.uniq⟩

/-- Case-alternative constructors (§3.3): the default, when present,
comes first. -/
inductive AltCon where
  | dataAlt (c : String)
  | litAlt  (n : Int)
  | default
deriving DecidableEq, Repr

mutual

/-- Expressions (§3.3): 12 constructors. Constructor, primitive, and
integer-literal occurrences carry their full instantiated types. -/
inductive Exp where
  | var     (x : Id)
  | con     (ty : Ty) (c : String)
  | prim    (ty : Ty) (b : Builtin)
  | litInt  (ty : Ty) (n : Int)
  | litStr  (s : String)
  | litList (ty : Ty) (es : List Exp)
  | litVec  (ty : Ty) (es : List Exp)
  | app     (e : Exp) (arg : Arg)
  | lam     (x : Id) (e : Exp)
  | letE    (b : Bind) (e : Exp)
  | jump    (l : JoinId) (es : List Exp)
  | cases   (ty : Ty) (scrut : Exp) (binder : Id) (alts : List Alt)
deriving Repr

/-- Application arguments: term or type (§3.3). -/
inductive Arg where
  | eArg (e : Exp)
  | tArg (t : Ty)
deriving Repr

/-- Bindings (§3.4): non-recursive let, recursive group, join point. -/
inductive Bind where
  | nonRec (x : Id) (e : Exp)
  | recB   (bs : List (Id × Exp))
  | join   (l : JoinId) (params : List Id) (e : Exp)
deriving Repr

/-- A case alternative: constructor pattern, field binders, body. -/
inductive Alt where
  | mk (con : AltCon) (binders : List Id) (body : Exp)
deriving Repr

end

/-- Definition attributes (§3.5). -/
inductive DefnAttr where
  | inline
  | noinline
deriving DecidableEq, Repr

/-- Provenance of a compiler-minted definition clone (§3.5): the origin
definition's display name and — for specializer clones — the type
arguments it was instantiated at (`baked` marks the partial evaluator's
value-baked clones, whose baked arguments are terms, not types).
Semantically inert; carried for dumps and stable naming. -/
inductive SpecOrigin where
  | spec  (name : String) (args : List Ty)
  | baked (name : String)
deriving Repr

/-- A definition (§3.5): the parameter telescope matches a prefix of
the signature's arrow spine. -/
structure Defn where
  name   : Id
  params : List Id
  body   : Exp
  attr   : Option DefnAttr := none
  origin : Option SpecOrigin := none
deriving Repr

/-- A data constructor: name and signature quantifying exactly the
datatype's parameters (§3.6). -/
structure DataCon where
  name : String
  sig  : Sig
deriving Repr

/-- A datatype declaration (§3.6). -/
structure DataDefn where
  name : String
  kind : Kind
  cons : List DataCon
deriving Repr

end Rwv.Eidos

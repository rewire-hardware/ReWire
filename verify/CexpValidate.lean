/-
rwv-cexp-validate: the per-definition measurement driver for the
verified Eidos-side expression compiler (Rwv.Eidos.Cexp).

    rwv-cexp-validate <file.syn> <file.rwc> [--fuel=N] [-v]

parses the machine-level pass-8 dump and the compiled .rwc, mirrors the
reference translation's definition normalization (transDefn: peel
lambdas into parameters, eta-expand to signature arity; plus the
under-applied constructor/primitive saturation the differ uses —
see SynolonDiff.lean's header), matches Eidos pure definitions to Hyle
definitions by simulating the fold's naming (ToHyle.buildNameMap:
`defnBase` — the `$LL.` marker stripped — disambiguated by
`pickFresh` numeric suffixes over the emitted definitions in order),
and for each matched pair checks

    cexpFull (Eidos body over the Hyle parameter names)
      ≡  Bridge.symExp (Hyle body)

first by the VERIFIED per-defn validator `checkDefnPair`
(`checkDefnPair_sound`: the DAG leg `checkDefnPairDag` tried first,
then the `cfold`-syntactic leg plus the `cfoldW3` width-aware leg
under the parameter-width discipline),
then by unverified fallbacks: plain `cfoldW3` equality without the
parameter-width guard, and the BridgeDag hash-consing engine.

Verdicts per Eidos definition:
  OK-V      checkDefnPair = true (covered by checkDefnPair_sound)
  OK-W      cfoldW3-syntactic equality only (unverified leg)
  OK-DAG    equal after DAG normalization (engine leg only)
  MISMATCH  all legs disagree — a genuine or normalization miss
  GAP:...   cexpFull rejected the body (fragment gap, message quoted)
  SKIP:...  no matched Hyle defn / carrier defn / arity drift

This driver is UNTRUSTED measurement plumbing (including the
eta-saturation pre-pass and the name-map simulation); the verified
statement is Rwv.Eidos.Cexp.checkDefnPair_sound, about the saturated
definition pair actually passed to it.
-/
import Rwv.Synolon.Parse
import Rwv.Synolon.PrimBasis
import Rwv.Eidos.ForeignEnv
import Rwv.Eidos.Cexp
import Rwv.Hyle.Parse
import Rwv.Hyle.Bridge
import Rwv.Hyle.BridgeDag

open Rwv.Eidos
open Rwv.Synolon
open Rwv.Eidos.Cexp
open Rwv.Hyle (BV)
open Rwv.Hyle.Bridge (NF)
open Std (HashMap)

/-! ## Eta saturation (SynolonDiff's pre-pass, defn-level extended)

Under-applied constructor/primitive occurrences are wrapped in
lambdas supplying the missing arguments (fresh uniques minted from
-10⁹ down, a range no bridge or basis name occupies); then, per
definition, leading lambdas peel into parameters and a definition
still short of its signature arity is eta-expanded — exactly
ToHyle.transDefn's normalization, so the parameter telescopes align
with the Hyle definitions'. -/

namespace EtaSat

abbrev M := StateM Nat

def freshId (ty : Ty) : M Id := do
  let n ← get
  set (n + 1)
  pure { occ := s!"$eta{n}", uniq := -(1000000000 + (n : Int)), sig := ⟨[], ty⟩ }

/-- Flatten an application spine keeping every argument (type
arguments included, in position). -/
def flattenApp' (e : Exp) : Exp × List Arg := go [] e
where go (acc : List Arg) : Exp → Exp × List Arg
  | .app f a => go (a :: acc) f
  | e => (e, acc)

/-- Wrap `e` (of type `doms → ρ`) in lambdas supplying `doms`. -/
def wrapLam (doms : List Ty) (e : Exp) : M Exp := do
  let xs ← doms.mapM freshId
  pure (xs.foldr (fun x b => .lam x b)
        (xs.foldl (fun a x => .app a (.eArg (.var x))) e))

mutual

/-- Eta-saturate every constructor/primitive head in `e`. -/
partial def satExp (e : Exp) : M Exp := do
  let (h, args) := flattenApp' e
  let args' ← args.mapM fun
    | .eArg a => .eArg <$> satExp a
    | .tArg t => pure (.tArg t)
  let h' ← satHead h
  let e' := args'.foldl .app h'
  let k := (args.filter fun | .eArg _ => true | .tArg _ => false).length
  match h with
  | .con ty _ | .prim ty _ =>
      let doms := (Ty.flattenArrow ty).1
      if k < doms.length then wrapLam (doms.drop k) e' else pure e'
  | _ => pure e'

partial def satHead : Exp → M Exp
  | .lam x b            => .lam x <$> satExp b
  | .letE bnd b         => do pure (.letE (← satBind bnd) (← satExp b))
  | .jump l es          => .jump l <$> es.mapM satExp
  | .cases ty sc x alts => do
      pure (.cases ty (← satExp sc) x (← alts.mapM satAlt))
  | .litList ty es      => .litList ty <$> es.mapM satExp
  | .litVec ty es       => .litVec ty <$> es.mapM satExp
  | e => pure e

partial def satBind : Bind → M Bind
  | .nonRec x e   => .nonRec x <$> satExp e
  | .recB bs      => .recB <$> bs.mapM fun (x, e) => do pure (x, ← satExp e)
  | .join l ps e  => .join l ps <$> satExp e

partial def satAlt : Alt → M Alt
  | .mk c bs e => .mk c bs <$> satExp e

end

/-- Peel leading lambdas. -/
partial def peelLams : Exp → List Id × Exp
  | .lam x b => let (xs, b') := peelLams b; (x :: xs, b')
  | e => ([], e)

/-- transDefn's normalization: saturate the body, peel lambdas into
parameters, and eta-expand any residual signature arity. -/
def satDefn (d : Defn) : M Defn := do
  let body ← satExp d.body
  let (extra, core) := peelLams body
  let params := d.params ++ extra
  let doms := (Ty.flattenArrow d.name.sig.ty).1
  let missing := doms.drop params.length
  if missing.isEmpty then
    pure { d with params, body := core }
  else do
    let etas ← missing.mapM freshId
    pure { d with
      params := params ++ etas
      body := etas.foldl (fun a x => .app a (.eArg (.var x))) core }

end EtaSat

/-- Eta-saturate all definitions of a program (processes are not the
per-defn validator's concern). -/
def etaSaturateDefns (p : Program) : Program :=
  ((p.defns.mapM EtaSat.satDefn).map fun defns => { p with defns }).run' 0

/-! ## Nat-normalization (a driver pre-pass)

The verified compiler compares types syntactically (`teq`), while
pass-8 dumps carry unevaluated type arithmetic (`Vec (+ 9 1) Bool`).
Fold every nat-closed subterm to its literal, program-wide — the
semantic functions (`flatten`/`evalNat`/`matchTy`/`sizeOf`) are
insensitive to this, so it is measurement plumbing of the same status
as eta-saturation. -/

namespace NatNorm

def nId (x : Id) : Id :=
  { x with sig := { tvs := x.sig.tvs, ty := Ty.natNorm x.sig.ty } }

mutual

partial def nExp : Exp → Exp
  | .var x => .var (nId x)
  | .con ty c => .con (Ty.natNorm ty) c
  | .prim ty b => .prim (Ty.natNorm ty) b
  | .litInt ty n => .litInt (Ty.natNorm ty) n
  | .litStr s => .litStr s
  | .litList ty es => .litList (Ty.natNorm ty) (es.map nExp)
  | .litVec ty es => .litVec (Ty.natNorm ty) (es.map nExp)
  | .app e a => .app (nExp e) (nArg a)
  | .lam x e => .lam (nId x) (nExp e)
  | .letE b e => .letE (nBind b) (nExp e)
  | .jump l es => .jump l (es.map nExp)
  | .cases ty sc x alts => .cases (Ty.natNorm ty) (nExp sc) (nId x) (alts.map nAlt)

partial def nArg : Arg → Arg
  | .eArg e => .eArg (nExp e)
  | .tArg t => .tArg (Ty.natNorm t)

partial def nBind : Bind → Bind
  | .nonRec x e => .nonRec (nId x) (nExp e)
  | .recB bs => .recB (bs.map fun (x, e) => (nId x, nExp e))
  | .join l ps e => .join l (ps.map nId) (nExp e)

partial def nAlt : Alt → Alt
  | .mk c bs e => .mk c (bs.map nId) (nExp e)

end

def nDefn (d : Defn) : Defn :=
  { d with name := nId d.name, params := d.params.map nId, body := nExp d.body }

end NatNorm

/-- Nat-normalize every type in the program's definitions and data
declarations. -/
def natNormDefns (p : Program) : Program :=
  { p with
    defns := p.defns.map NatNorm.nDefn
    datas := p.datas.map fun d =>
      { d with cons := d.cons.map fun c =>
          { c with sig := { tvs := c.sig.tvs, ty := Ty.natNorm c.sig.ty } } } }

/-! ## The fold's naming (ToHyle.buildNameMap simulated) -/

/-- ToHyle's `defnBase`: strip the `$LL.` lifted-definition marker. -/
def defnBase (occ : String) : String :=
  if occ.startsWith "$LL." then ((occ.drop 4).toString) else occ

/-- Types.reacOrStateT: mentions the reactive stack anywhere. -/
partial def reacOrStateT : Ty → Bool
  | .con "ReacT" | .con "StateT" | .con "Identity" => true
  | .arrow t1 t2 | .app t1 t2 => reacOrStateT t1 || reacOrStateT t2
  | _ => false

/-- ToHyle's `emit` filter: dotted (non-primitive) name, monomorphic,
non-reactive. Only these lower to Hyle definitions (and claim names). -/
def emitB (d : Defn) : Bool :=
  d.name.occ.toList.contains '.'
    && d.name.sig.tvs.isEmpty
    && !reacOrStateT d.name.sig.ty

/-- Hyle.Mangle.pickFresh with an empty separator: the seed itself
when free, else the first free suffix-numbered variant. -/
partial def pickFresh (used : HashMap String Nat) (s : String) : String × HashMap String Nat :=
  go (used.getD s 0)
where
  go (k : Nat) : String × HashMap String Nat :=
    let cand := if k = 0 then s else s ++ toString k
    if used.contains cand then go (k + 1)
    else (cand, ((if cand == s then used else used.insert cand 1).insert s (k + 1)))

/-- The Hyle names of the emitted definitions, in definition order
(ToHyle.buildNameMap; `seed` pre-populates the used map — the driver
passes none, see the call site). -/
def buildNameMap (seed : List String) (defns : List Defn) : HashMap Int String :=
  (defns.filter emitB).foldl (init := (∅, seed.foldl (fun m s => m.insert s 1) ∅))
    (fun (m, used) d =>
      let (nm, used') := pickFresh used (defnBase d.name.occ)
      (m.insert d.name.uniq nm, used'))
  |>.1

/-! ## The DAG fallback -/

/-- Convert a bridge normal form into the hash-consing DAG through the
normalizing constructors. -/
partial def nfToDag (d : Rwv.Hyle.BridgeDag.Dag) : NF →
    Rwv.Hyle.BridgeDag.Dag × Nat
  | .var w x => Rwv.Hyle.BridgeDag.Dag.mkVar d w x
  | .lit v => Rwv.Hyle.BridgeDag.Dag.mkLit d v
  | .prim1 op a =>
      let (d, ra) := nfToDag d a
      Rwv.Hyle.BridgeDag.Dag.mk1D d op ra
  | .prim2 op a b =>
      let (d, ra) := nfToDag d a
      let (d, rb) := nfToDag d b
      Rwv.Hyle.BridgeDag.Dag.mk2D d op ra rb
  | .cat a b =>
      let (d, ra) := nfToDag d a
      let (d, rb) := nfToDag d b
      Rwv.Hyle.BridgeDag.Dag.mkCatD d ra rb
  | .slice i w e =>
      let (d, re) := nfToDag d e
      Rwv.Hyle.BridgeDag.Dag.mkSliceD d i w re
  | .ite c t e =>
      let (d, rc) := nfToDag d c
      let (d, rt) := nfToDag d t
      let (d, re) := nfToDag d e
      Rwv.Hyle.BridgeDag.Dag.mkIteD d rc rt re
  | .xcall w x gs a =>
      let (d, ra) := nfToDag d a
      Rwv.Hyle.BridgeDag.Dag.mkXcallD d w x gs ra

/-- DAG-engine comparison of two normal forms over shared variables. -/
def dagEq (n₁ n₂ : NF) : Bool :=
  let (d, r₁) := nfToDag Rwv.Hyle.BridgeDag.Dag.empty n₁
  let (d, r₂) := nfToDag d n₂
  if r₁ == r₂ then true
  else
    match Rwv.Hyle.BridgeDag.renorm d with
    | .error _ => false
    | .ok (e₁, m₁) =>
        match Rwv.Hyle.BridgeDag.renorm e₁ with
        | .error _ => false
        | .ok (_, m₂) =>
            Rwv.Hyle.BridgeDag.mIdx m₂ (Rwv.Hyle.BridgeDag.mIdx m₁ r₁)
              == Rwv.Hyle.BridgeDag.mIdx m₂ (Rwv.Hyle.BridgeDag.mIdx m₁ r₂)

structure Tally where
  okV : Nat := 0
  ok : Nat := 0
  okDag : Nat := 0
  mismatch : Nat := 0
  gap : Nat := 0
  skip : Nat := 0

def main (argv : List String) : IO UInt32 := do
  let mut fuel : Nat := 2000000
  let mut verbose := false
  let mut pos : List String := []
  for a in argv do
    if a = "-v" then verbose := true
    else if a.startsWith "--fuel=" then
      match (a.drop "--fuel=".length).toNat? with
      | some n => fuel := n
      | none =>
          IO.eprintln s!"cexp-validate: --fuel: expected a non-negative integer, got '{a.drop 7}'"
          return 2
    else if a.startsWith "-" && a ≠ "-" then
      IO.eprintln s!"cexp-validate: unknown option: {a}"
      return 2
    else pos := pos ++ [a]
  match pos with
  | [eirFile, rwcFile] => do
    let eirTxt ← IO.FS.readFile ⟨eirFile⟩
    let rwcTxt ← IO.FS.readFile ⟨rwcFile⟩
    match parseSyn eirTxt eirFile, Rwv.Hyle.parseProgram rwcTxt rwcFile with
    | .error e, _ => IO.eprintln s!"cexp-validate: {eirFile}: {e}"; return 1
    | _, .error e => IO.eprintln s!"cexp-validate: {rwcFile}: {e}"; return 1
    | .ok p₀, .ok hp => do
      let p := natNormDefns (etaSaturateDefns (addPrims p₀))
      -- The foreign tier: Cryptol splices and extern models read from
      -- the compiled program itself (CstepValidate's default), so the
      -- compile side's model inlining aligns with `symExp`'s.
      let Δ := addForeign (DEnv.ofDatas p.datas) rwcTxt hp
      let edm := mkDefnMap p.defns
      let hdm : HashMap String Rwv.Hyle.Defn :=
        HashMap.ofList (hp.defns.map fun d => (d.name, d))
      unless denvOk Δ do
        IO.eprintln "cexp-validate: denvOk failed (prim basis Bool/Vec discipline)"
        return 1
      -- No used-map seeding: the fold seeds Cryptol-fragment and extern
      -- names, but those live in distinct namespaces (`cry$…`, bare
      -- module names) that never collide with dotted defn bases; a
      -- mis-simulated name only yields a SKIP, never a false verdict.
      let names := buildNameMap [] p.defns
      let hyleFuel := Rwv.Hyle.Bridge.progFuel hp
      let hDmap := Rwv.Hyle.Bridge.dmapOf hp
      let hX := Rwv.Hyle.Sem.xenv hp
      let mut t : Tally := {}
      for d in p.defns do
        let nm := s!"{d.name.occ}#{d.name.uniq}"
        if !emitB d then
          if verbose then IO.println s!"SKIP      {nm}  (carrier: not emitted by the fold)"
          t := { t with skip := t.skip + 1 }
        else
          match names.get? d.name.uniq >>= hdm.get? with
          | none =>
              if verbose then
                IO.println s!"SKIP      {nm}  (no Hyle defn '{(names.get? d.name.uniq).getD (defnBase d.name.occ)}': inlined away or renamed)"
              t := { t with skip := t.skip + 1 }
          | some h =>
              if d.params.length ≠ h.params.length then
                IO.println s!"SKIP      {nm}  (arity {d.params.length} vs {h.params.length}: eta drift)"
                t := { t with skip := t.skip + 1 }
              -- The VERIFIED verdict first: its DAG leg absorbs the
              -- splice-inlining pairs whose TREE forms only exist as
              -- shared dags (sha256ffi — a tree `NF.xcallFree` walk
              -- tree-unfolds the sharing); the tree compilations below
              -- run only for the failure diagnosis.
              else if checkDefnPair Δ edm hDmap hX fuel hyleFuel d h then do
                -- the leg the VERIFIED checkDefnPair certifies
                if verbose then IO.println s!"OK-V      {nm}"
                t := { t with okV := t.okV + 1 }
              else do
                match cexpFull Δ edm fuel (mkParamGamma d.params h.params h.sig.params) d.body with
                | .error msg =>
                    IO.println s!"GAP       {nm}  ({msg})"
                    t := { t with gap := t.gap + 1 }
                | .ok (ne, _ty) =>
                    match Rwv.Hyle.Bridge.symExp hDmap hX hyleFuel
                            (mkParamRho h.params h.sig.params) h.body with
                    | .error msg =>
                        IO.println s!"SKIP      {nm}  (Hyle symExp: {msg})"
                        t := { t with skip := t.skip + 1 }
                    | .ok nh =>
                        if Rwv.Hyle.Bridge.cfoldW3 ne == Rwv.Hyle.Bridge.cfoldW3 nh then do
                          IO.println s!"OK-W      {nm}"
                          t := { t with ok := t.ok + 1 }
                        else if dagEq ne nh then do
                          IO.println s!"OK-DAG    {nm}"
                          t := { t with okDag := t.okDag + 1 }
                        else do
                          IO.println s!"MISMATCH  {nm}"
                          t := { t with mismatch := t.mismatch + 1 }
      IO.println s!"summary: {t.okV} ok-v, {t.ok} ok-w, {t.okDag} ok-dag, {t.mismatch} mismatch, {t.gap} gap, {t.skip} skip"
      return (if t.mismatch > 0 then 1 else 0)
  | _ => IO.eprintln "usage: rwv-cexp-validate <file.syn> <file.rwc> [--fuel=N] [-v]"; return 2

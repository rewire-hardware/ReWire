/-
The drivers' foreign-environment construction (stage A): instantiate
the DEnv foreign hooks from a compiled Hyle program — the trust
boundary of the validation plan §1.3, under which the rwcry-spliced
`cry$…` definitions (resp. a combinational extern's model definition)
ARE the foreign builtin's semantics.

  * `xtF` is the program's own extern-model composition (`Sem.xenv`
    then the definition denotations) keyed by extern name — exactly
    `evalExp`'s xcall reading.
  * `cryF`/`cryD` need the (file, function, monotype) ↦ entry-name
    map, which rwc does not emit structurally; `scrapeCry` recovers it
    from the `--| cryptol <file>::<fn> at <cty>` doc lines rwc prints
    on each spliced entry definition, with `cryTyText` transcribing
    ToHyle's `cryTy` rendering to match use-site monotypes against
    `<cty>`. The scrape is UNTRUSTED plumbing: a wrong entry shows up
    as a validation rejection, a trace mismatch, or a decode
    canonicality error — never silently.

`addForeign_foreignC` discharges the soundness premise
(`Rwv.Eidos.Cexp.ForeignC`) for the constructed environment: the
semantic hook is BY CONSTRUCTION the denotation of the definition the
syntactic map designates, so the premise of
`Rwv.Eidos.Cstep.validateProc_corresponds` is machine-checkably
satisfiable for the environments the drivers actually build.

The foreign program may be (and for the validator should be) the
PRE-OPTIMIZATION dump (`rwc -d 9`): the final .rwc no longer carries
splices the optimizer constant-folded away (zero-argument Cryptol
constants), while the raw fold carries them all.
-/
import Rwv.Eidos.Cexp

namespace Rwv.Eidos

open Rwv.Hyle (BV)

namespace ForeignEnv

/-- ToHyle's `cryTy` rendering, transcribed (one component; fueled —
recursion follows the type structure). -/
def cryTyGo : Nat → Ty → Except String String
  | 0, _ => throw "cryTy: fuel exhausted"
  | fuel + 1, t =>
    if ¬ (Ty.flattenArrow t).1.isEmpty then
      throw "a function-typed component cannot cross the Cryptol boundary"
    else match Ty.flatten t with
      | (.con "Bool", []) => pure "Bit"
      | (.con "Vec", [n, te]) =>
          (match Ty.evalNat n with
          | some k => do
              let te' ← cryTyGo fuel te
              pure (s!"[{k}]" ++ (if te' = "Bit" then "" else "(" ++ te' ++ ")"))
          | none => throw "open Vec length")
      | (.con c, targs) =>
          if Ty.isTupleCon c ∧ ¬ targs.isEmpty then do
            let as' ← targs.mapM (cryTyGo fuel)
            pure ("(" ++ String.intercalate ", " as' ++ ")")
          else if c = "()" ∧ targs.isEmpty then pure "()"
          else throw "unsupported type"
      | _ => throw "unsupported type"

/-- ToHyle's `cryTy`: a monomorphic Eidos type as Cryptol type text
(the format of the `--|` doc lines on spliced entry definitions). -/
def cryTyText (t : Ty) : Except String String :=
  match Ty.flattenArrow t with
  | ([], tr) => cryTyGo 1000000 tr
  | (ts, tr) => do
      let ts' ← ts.mapM (cryTyGo 1000000)
      let tr' ← cryTyGo 1000000 tr
      pure (String.intercalate " -> " (ts' ++ [tr']))

structure CryEntry where
  file  : String
  fn    : String
  cty   : String
  entry : String
deriving Repr

/-- Scrape the `--| cryptol <file>::<fn> at <cty>` doc lines from raw
.rwc text; each annotates the immediately following definition, whose
name is the next line's first token. Untrusted plumbing. -/
def scrapeCry (rwcTxt : String) : List CryEntry := Id.run do
  let mut out : List CryEntry := []
  let mut pending : Option (String × String × String) := none
  for l in rwcTxt.splitOn "\n" do
    match pending with
    | some (f, n, cty) =>
        let name := ((l.splitOn " ").headD "")
        if name ≠ "" then
          out := out ++ [⟨f, n, cty, name⟩]
        pending := none
    | none =>
        if l.startsWith "--| cryptol " then
          match ((l.drop "--| cryptol ".length).toString).splitOn "::" with
          | f :: fnAt@(_ :: _) =>
              match (String.intercalate "::" fnAt).splitOn " at " with
              | fn :: ctyParts@(_ :: _) =>
                  pending := some (f, fn, String.intercalate " at " ctyParts)
              | _ => pending := none
          | _ => pending := none
  return out

/-- The syntactic entry-name map from the scraped doc lines. -/
def cryDOf (entries : List CryEntry) : String → String → Ty → Option String :=
  fun f n t =>
    match cryTyText t with
    | .error _ => none
    | .ok cty =>
        (entries.find? (fun e => e.file == f && e.fn == n && e.cty == cty)).map (·.entry)

end ForeignEnv

open ForeignEnv in
/-- Instantiate the DEnv foreign hooks from a compiled program (see
the module header). The semantic hooks are BY CONSTRUCTION the
denotations of the definitions the syntactic maps designate
(`addForeign_foreignC` below); if the program's definition environment
does not denote, the semantic hooks stay empty and any foreign call
fails loudly at evaluation. -/
def addForeign (Δ : DEnv) (rwcTxt : String) (hp : Rwv.Hyle.Program) : DEnv :=
  let entries := scrapeCry rwcTxt
  let cryD := cryDOf entries
  let hyleDefs := Rwv.Hyle.Bridge.dmapOf hp
  let hyleFuel := Rwv.Hyle.Bridge.progFuel hp
  match Rwv.Hyle.Sem.mkFEnv hp with
  | .error _ => { Δ with cryD, hyleDefs, hyleFuel }
  | .ok F =>
      { Δ with
        cryD, hyleDefs, hyleFuel
        cryF := fun f n t => (cryD f n t).map (Cexp.callF F)
        xtF := fun s => ((Rwv.Hyle.Sem.xenv hp).get? s).map (Cexp.callF F) }

/-- The constructed foreign environment satisfies the soundness
premise: `cryF` is definitionally `F`'s denotation of the entry `cryD`
designates, and `F` implements the installed definition map by the
bridge's `mkFEnv` characterization. -/
theorem addForeign_foreignC {Δ : DEnv} {rwcTxt : String} {hp : Rwv.Hyle.Program}
    (hnd : (hp.defns.map (·.name)).Nodup) {F : Rwv.Hyle.Sem.FEnv}
    (hF : Rwv.Hyle.Sem.mkFEnv hp = .ok F) :
    ∃ X F', Rwv.Eidos.Cexp.ForeignC (addForeign Δ rwcTxt hp) X F' := by
  refine ⟨Rwv.Hyle.Sem.xenv hp, F, ?_, ?_⟩
  · show Rwv.Hyle.Bridge.FImplements (addForeign Δ rwcTxt hp).hyleDefs
      (Rwv.Hyle.Sem.xenv hp) F
    have hdefs : (addForeign Δ rwcTxt hp).hyleDefs = Rwv.Hyle.Bridge.dmapOf hp := by
      rw [addForeign, hF]
    rw [hdefs]
    exact Rwv.Hyle.Bridge.mkFEnv_implements hnd hF
  · intro f n t g hg
    have hg' : (addForeign Δ rwcTxt hp).cryD f n t = some g := hg
    rw [addForeign, hF] at hg'
    refine ⟨Rwv.Eidos.Cexp.callF F g, ?_, fun vs => rfl⟩
    show (addForeign Δ rwcTxt hp).cryF f n t = some (Rwv.Eidos.Cexp.callF F g)
    rw [addForeign, hF]
    show (ForeignEnv.cryDOf (ForeignEnv.scrapeCry rwcTxt) f n t).map (Cexp.callF F)
      = some (Rwv.Eidos.Cexp.callF F g)
    rw [show ForeignEnv.cryDOf (ForeignEnv.scrapeCry rwcTxt) f n t = some g from hg']
    rfl

#print axioms Rwv.Eidos.addForeign_foreignC

end Rwv.Eidos

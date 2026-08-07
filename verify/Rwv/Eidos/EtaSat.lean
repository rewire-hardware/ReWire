/-
Eta-saturation of constructor and primitive occurrences: every
con/prim head applied to fewer term arguments than its carried
instantiated type's arrow spine is wrapped in lambdas supplying the
missing arguments (`p ā` with k of n args becomes `λx̄. p ā x̄`).

Pass-8 dumps contain under-applied occurrences the committed evaluator
deliberately rejects (Eval decision note 4); the reference translation
eta-expands to signature arity during its mono+ANF normalization
(doc/eidos.md §6, ToHyle's etaExpand) before any of the machinery this
package mechanizes runs. The drivers apply this pass after parsing, so
the artifact the differ exercises and the validator certifies is the
SATURATED program — this module is untrusted normalization plumbing
shared by rwv-eidos-diff and rwv-cstep-validate, not part of any
soundness statement.

Fresh binder uniques are minted from -10⁹ down — far below both the
bridge's non-negative term uniques and the prim basis' small negatives
— so they cannot capture. Recursion is fueled (the library carries no
`partial` definitions); the drivers pass generous fuel and fail loudly
on exhaustion.
-/
import Rwv.Eidos.Syntax
import Rwv.Eidos.Types

namespace Rwv.Eidos.EtaSat

/-- Fresh-binder supply over loud failure: the state is the next fresh
magnitude; uniques are minted as `-(10⁹ + n)`. -/
abbrev M := StateT Nat (Except String)

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

def fuelErr : String := "etaSaturate: fuel exhausted"

mutual

/-- Eta-saturate every constructor/primitive head in `e`. -/
def satExp : Nat → Exp → M Exp
  | 0, _ => throw fuelErr
  | fuel + 1, e => do
      let (h, args) := flattenApp' e
      let args' ← args.mapM (satArg fuel)
      let h' ← satHead fuel h
      let e' := args'.foldl .app h'
      let k := (args.filter fun | .eArg _ => true | .tArg _ => false).length
      match h with
      | .con ty _ | .prim ty _ =>
          let doms := (Ty.flattenArrow ty).1
          if k < doms.length then wrapLam (doms.drop k) e' else pure e'
      | _ => pure e'

def satArg : Nat → Arg → M Arg
  | 0, _ => throw fuelErr
  | fuel + 1, .eArg a => .eArg <$> satExp fuel a
  | _ + 1, .tArg t => pure (.tArg t)

/-- The non-application head forms (an `.app` head is impossible after
flattening — its saturation happens through `satExp`'s spine). -/
def satHead : Nat → Exp → M Exp
  | 0, _ => throw fuelErr
  | fuel + 1, .lam x b => .lam x <$> satExp fuel b
  | fuel + 1, .letE bnd b => do pure (.letE (← satBind fuel bnd) (← satExp fuel b))
  | fuel + 1, .jump l es => .jump l <$> es.mapM (satExp fuel)
  | fuel + 1, .cases ty sc x alts => do
      pure (.cases ty (← satExp fuel sc) x (← alts.mapM (satAlt fuel)))
  | fuel + 1, .litList ty es => .litList ty <$> es.mapM (satExp fuel)
  | fuel + 1, .litVec ty es => .litVec ty <$> es.mapM (satExp fuel)
  | _ + 1, e => pure e

def satBind : Nat → Bind → M Bind
  | 0, _ => throw fuelErr
  | fuel + 1, .nonRec x e => .nonRec x <$> satExp fuel e
  | fuel + 1, .recB bs => .recB <$> bs.mapM fun (x, e) => do pure (x, ← satExp fuel e)
  | fuel + 1, .join l ps e => .join l ps <$> satExp fuel e

def satAlt : Nat → Alt → M Alt
  | 0, _ => throw fuelErr
  | fuel + 1, .mk c bs e => .mk c bs <$> satExp fuel e

end

def satCmd (fuel : Nat) : Cmd → M Cmd
  | .bind x e => .bind x <$> satExp fuel e
  | .get x c => pure (.get x c)
  | .put c e => .put c <$> satExp fuel e

mutual

def satTerm : Nat → Term → M Term
  | 0, _ => throw fuelErr
  | fuel + 1, .pause o l as => do
      pure (.pause (← satExp fuel o) l (← as.mapM (satExp fuel)))
  | fuel + 1, .goto l as => .goto l <$> as.mapM (satExp fuel)
  | fuel + 1, .halt e => .halt <$> satExp fuel e
  | fuel + 1, .cases sc as => do
      pure (.cases (← satExp fuel sc) (← as.mapM (satTAlt fuel)))

def satTAlt : Nat → TAlt → M TAlt
  | 0, _ => throw fuelErr
  | fuel + 1, .mk c bs t => .mk c bs <$> satTerm fuel t

end

def satBlock (fuel : Nat) (b : Block) : M Block := do
  pure { b with cmds := ← b.cmds.mapM (satCmd fuel), term := ← satTerm fuel b.term }

def satProc (fuel : Nat) (p : Proc) : M Proc := do
  let cells ← p.cells.mapM fun c => do
    pure { c with init := ← c.init.mapM (satExp fuel) }
  let entry ← satBlock fuel p.entry
  let blocks ← p.blocks.mapM fun (l, b) => do pure (l, ← satBlock fuel b)
  pure { p with cells, entry, blocks }

end EtaSat

/-- Eta-saturate all definition bodies and processes of a program (see
the module header; the drivers' pre-pass). -/
def etaSaturate (fuel : Nat) (p : Program) : Except String Program :=
  (go p).run' 0
where
  go (p : Program) : EtaSat.M Program := do
    let defns ← p.defns.mapM fun d => do
      pure { d with body := ← EtaSat.satExp fuel d.body }
    let procs ← p.procs.mapM (EtaSat.satProc fuel)
    pure { p with defns, procs }

end Rwv.Eidos

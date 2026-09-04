/-
Eta-saturation of constructor and primitive occurrences, machine half:
the traversal over commands, terminators, blocks, and processes, and
the program-level entry point `etaSaturate` — the drivers' pre-pass
(see Rwv.Eidos.EtaSat's module header for the rationale, the fresh
unique range, and the fueling convention). Untrusted normalization
plumbing shared by rwv-synolon-diff and rwv-cstep-validate, not part
of any soundness statement.
-/
import Rwv.Eidos.EtaSat
import Rwv.Synolon.Syntax

namespace Rwv.Synolon

open Rwv.Eidos

namespace EtaSat

open Rwv.Eidos.EtaSat

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

end Rwv.Synolon

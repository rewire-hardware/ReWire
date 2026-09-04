/-
The primitive datatype basis, program level: `addPrims` prepends
Rwv.Eidos.PrimBasis's `primDatas` to a Synolon program (the drivers'
and the bundle validator's first step after parsing).
-/
import Rwv.Eidos.PrimBasis
import Rwv.Synolon.Syntax

namespace Rwv.Synolon

open Rwv.Eidos


/-- Prepend the primitive basis (dropping any duplicate declarations,
which the bridge does not produce but hand-written input might). -/
def addPrims (p : Program) : Program :=
  let basisNames := primDatas.map (·.name)
  { p with datas := primDatas ++ p.datas.filter (fun d => ¬ basisNames.contains d.name) }

end Rwv.Synolon

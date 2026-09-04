/-
The Synolon machine-level IR, deep-embedded: a transcription of the
abstract syntax of doc/synolon.md §3.4 — state cells, commands,
terminators, blocks, processes, and programs — mirroring
rewire-frontend ReWire.Synolon.Syntax, over the Eidos expression
syntax (Rwv.Eidos.Syntax). Annotations are omitted (semantically
inert).
-/
import Rwv.Eidos.Syntax

namespace Rwv.Synolon

open Rwv.Eidos

/-! ## Processes (doc/synolon.md §3.4) -/

/-- A state cell: name, type, and optional initial (none = `undef`,
which denotes the zero value of the type; doc/synolon.md §3.4, doc/synolon.md §5.4). -/
structure Cell where
  name : String
  ty   : Ty
  init : Option Exp
deriving Repr

/-- Commands (doc/synolon.md §3.4): pure computation, cell read, cell write. -/
inductive Cmd where
  | bind (x : Id) (e : Exp)
  | get  (x : Id) (cell : String)
  | put  (cell : String) (e : Exp)
deriving Repr

mutual

/-- Terminators (doc/synolon.md §3.4): pause (emit and resume next cycle), goto
(intra-cycle transfer, saturated), halt, terminator case. -/
inductive Term where
  | pause (out : Exp) (l : Id) (args : List Exp)
  | goto  (l : Id) (args : List Exp)
  | halt  (e : Exp)
  | cases (scrut : Exp) (alts : List TAlt)
deriving Repr

/-- A terminator-case alternative (no case binder, doc/synolon.md §3.4). -/
inductive TAlt where
  | mk (con : AltCon) (binders : List Id) (term : Term)
deriving Repr

end

/-- A labeled block: parameters (the last is the resumed input for
pause targets), commands, terminator. -/
structure Block where
  params : List Id
  cmds   : List Cmd
  term   : Term
deriving Repr

/-- A process (doc/synolon.md §3.4): input/output types, optional clock-domain
annotation, cells, the parameterless entry block, and labeled blocks. -/
structure Proc where
  name   : String
  inTy   : Ty
  outTy  : Ty
  clock  : Option String := none
  cells  : List Cell
  entry  : Block
  blocks : List (Id × Block)
deriving Repr

/-- A program: datatypes, definitions, processes, and — in a legacy
Eidos machine-level dump, or hand-written input — the designated
device root. A machine-level (Synolon) program has no `top`: its
processes are its roots, so the field is `none` for compiler output
and the `top` rule (`Check.checkTop`) applies only when it is present. -/
structure Program where
  datas : List DataDefn
  defns : List Defn
  procs : List Proc
  top   : Option Id := none
deriving Repr

end Rwv.Synolon

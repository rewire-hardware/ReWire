/-
Parser for the Synolon concrete syntax (.syn; doc/synolon.md §9): the
process and program productions over the Eidos lexer, combinators,
and expression parser (Rwv.Eidos.Parse) — a transcription of
rewire-frontend ReWire.Synolon.Parse — the process half of the
elaboration pass, and the entry point `parseSyn`.

Block labels in terminators receive signatures reconstructed as arrows
from the block's parameter types to the process output type, and may
reference blocks declared later in the same process (resolved after
the whole process is parsed); the expressions embedded in cells and
blocks are elaborated by the Eidos elaborator with the process's
binders in scope.
-/
import Rwv.Eidos.Parse
import Rwv.Synolon.Syntax

namespace Rwv.Synolon

open Rwv.Eidos

namespace Parse

open Rwv.Eidos.Parse

/-! ## Processes (doc/synolon.md §3.4 and §9)

Terminator labels resolve after the whole process is parsed (a
terminator may target a block declared later); a label's signature is
arrows from its block's parameter types to the process output type (a
bookkeeping convention — labels are not values). -/

def pCell? : PM (Option Cell) := do
  if ← keywordOpt "state" then do
    let s ← pBareName
    expectTok .colon "':'"
    let t ← pTy {}
    expectTok .assign "':='"
    let e0 ← if ← keywordOpt "undef" then pure none else some <$> pExp sc0
    expectTok .semi "';'"
    pure (some { name := s, ty := t, init := e0 })
  else pure none

def pCmd? : PM (Option Cmd) := do
  if ← keywordOpt "put" then do
    let s ← pBareName
    let a ← pAtom sc0
    expectTok .semi "';'"
    pure (some (.put s a))
  else
    match ← peek? with
    | some ⟨.name _ _ (some _), _, _⟩ => do
        let (occ, u) ← pUniqName
        expectTok .dcolon "'::'"
        let t ← pTy {}
        expectTok .larrow "'<-'"
        let x : Id := ⟨occ, u, monoSig t⟩
        let c ← if ← keywordOpt "get" then Cmd.get x <$> pBareName
                else Cmd.bind x <$> pExp sc0
        expectTok .semi "';'"
        pure (some c)
    | _ => pure none

mutual

partial def pTerm : PM Term := do
  if ← keywordOpt "pause" then do
    let a ← pAtom sc0
    expectTok .arrow "'->'"
    let (occ, u) ← pUniqName
    pure (.pause a ⟨occ, u, pendingSig⟩ (← pParenSep (pAtom sc0)))
  else if ← keywordOpt "goto" then do
    let (occ, u) ← pUniqName
    pure (.goto ⟨occ, u, pendingSig⟩ (← pParenSep (pAtom sc0)))
  else if ← keywordOpt "halt" then do
    pure (.halt (← pAtom sc0))
  else if ← keywordOpt "case" then do
    let a ← pAtom sc0
    keyword "of"
    pure (.cases a (← pBracedSep pTAlt))
  else errP "expected a block terminator"

partial def pTAlt : PM TAlt := do
  match ← peek? with
  | some ⟨.name false "_" none, _, _⟩ => do
      advance
      expectTok .arrow "'->'"
      pure (.mk .default [] (← pTerm))
  | some ⟨.int n, _, _⟩ => do
      advance
      expectTok .arrow "'->'"
      pure (.mk (.litAlt n) [] (← pTerm))
  | _ => do
      let c ← pConName
      let ps ← manyP (pParam {})
      expectTok .arrow "'->'"
      pure (.mk (.dataAlt c) ps (← pTerm))

end

def pBlockBody : PM (List Cmd × Term) := do
  expectTok .lbrace "'{'"
  let cmds ← manyOpt pCmd?
  let term ← pTerm
  expectTok .rbrace "'}'"
  pure (cmds, term)

def pBlock? : PM (Option (Id × Block)) := do
  if ← keywordOpt "block" then do
    let (occ, u) ← pUniqName
    let ps ← pParenSep (pParam {})
    let (cmds, term) ← pBlockBody
    pure (some (⟨occ, u, pendingSig⟩, ⟨ps, cmds, term⟩))
  else pure none

/-- Rewrite terminator targets to the declared labels (with their
finalized signatures). -/
partial def resolveTerm (ltab : Std.HashMap Int Id) : Term → PM Term
  | .pause o l args => do pure (.pause o (← resolveLabel l) args)
  | .goto l args    => do pure (.goto (← resolveLabel l) args)
  | .halt e         => pure (.halt e)
  | .cases s alts   =>
      Term.cases s <$> alts.mapM fun (.mk c xs t) => TAlt.mk c xs <$> resolveTerm ltab t
where
  resolveLabel (l : Id) : PM Id :=
    match ltab[l.uniq]? with
    | some l' => pure l'
    | none    => errP s!"terminator targets an undeclared block label: {l.occ}#{l.uniq}"

/-- `proc P : ty ~> ty clock? { state* entry block* }` (the `proc`
keyword already consumed). -/
def pProc : PM Proc := do
  let n ← pBareName
  expectTok .colon "':'"
  let it ← pTy {}
  expectTok .squig "'~>'"
  let ot ← pTy {}
  let clk ← if ← tokOpt .at then do keyword "clock"; some <$> pBareName else pure none
  expectTok .lbrace "'{'"
  let cells ← manyOpt pCell?
  keyword "entry"
  let (ecmds, eterm) ← pBlockBody
  let blocks ← manyOpt pBlock?
  expectTok .rbrace "'}'"
  let ltab : Std.HashMap Int Id := blocks.foldl (init := {}) fun m (l, b) =>
    m.insert l.uniq ⟨l.occ, l.uniq, monoSig (b.params.foldr (fun p acc => .arrow p.sig.ty acc) ot)⟩
  let entry : Block := ⟨[], ecmds, ← resolveTerm ltab eterm⟩
  let blocks ← blocks.mapM fun (l, b) => do
    pure (ltab.getD l.uniq l, { b with term := ← resolveTerm ltab b.term : Block })
  pure { name := n, inTy := it, outTy := ot, clock := clk
       , cells := cells, entry := entry, blocks := blocks }

/-! ## Programs -/

/-- `data* defn* proc* ('top' var)?`, to end of input. The machine-level
(Synolon, `.syn`) form has no `top` line; a legacy Eidos machine-level
dump or a hand-written program may carry one, in which case it must
name a parsed definition (matched by unique) and takes that
definition's `Id`. -/
partial def pProgram : PM Program := do
  let ds ← manyOpt do
    if ← keywordOpt "data" then some <$> pDataDefn else pure none
  let fs ← manyOpt do
    match ← peek? with
    | some ⟨.name _ _ (some _), _, _⟩ => some <$> pDefn
    | _ => pure none
  let ps ← manyOpt do
    if ← keywordOpt "proc" then some <$> pProc else pure none
  let top? ← if ← keywordOpt "top" then some <$> pUniqName else pure none
  unless (← atEOF) do errP "expected end of input"
  match top? with
  | none => pure { datas := ds, defns := fs, procs := ps, top := none }
  | some (occ, u) =>
    match fs.find? (fun d => d.name.uniq == u) with
    | some d => pure { datas := ds, defns := fs, procs := ps, top := some d.name }
    | none   => errP s!"top: designated device root {occ}#{u} does not name a definition"

/-- Elaborate the expressions embedded in a process: cell initials in
the top-level scope; block bodies with parameters and (sequentially)
command binders in scope; terminator-alternative binders over their
terms. -/
partial def elabProc (env : Env) (p : Proc) : Except String Proc := do
  let cells ← p.cells.mapM fun c => do
    pure { c with init := ← c.init.mapM (elabExp env) }
  let entry  ← elabBlock p.entry
  let blocks ← p.blocks.mapM fun (l, b) => do pure (l, ← elabBlock b)
  pure { p with cells := cells, entry := entry, blocks := blocks }
where
  elabBlock (b : Block) : Except String Block := do
    let env₀ := b.params.foldr insertVar env
    let (env', cmds) ← b.cmds.foldlM (init := (env₀, ([] : List Cmd))) fun (e, acc) c => do
      match c with
      | .bind x rhs => do
          let rhs' ← elabExp e rhs
          pure (insertVar x e, Cmd.bind x rhs' :: acc)
      | .get x s    => pure (insertVar x e, Cmd.get x s :: acc)
      | .put s a    => do pure (e, Cmd.put s (← elabExp e a) :: acc)
    pure ⟨b.params, cmds.reverse, ← elabTerm env' b.term⟩

  elabTerm (e : Env) : Term → Except String Term
    | .pause o l args => do pure (.pause (← elabExp e o) l (← args.mapM (elabExp e)))
    | .goto l args    => do pure (.goto l (← args.mapM (elabExp e)))
    | .halt x         => Term.halt <$> elabExp e x
    | .cases s alts   => do
        pure (.cases (← elabExp e s) (← alts.mapM fun (.mk c xs t) =>
          TAlt.mk c xs <$> elabTerm (xs.foldr insertVar e) t))

def elabProgram (p : Program) : Except String Program := do
  let env : Env :=
    ⟨Std.HashMap.ofList (p.defns.map fun d => (d.name.uniq, d.name)), {}⟩
  let fs ← p.defns.mapM fun d =>
    withCtx s!"in definition {d.name.occ}#{d.name.uniq}" do
      pure { d with body := ← elabExp (d.params.foldr insertVar env) d.body }
  let ps ← p.procs.mapM fun pr => withCtx s!"in process {pr.name}" (elabProc env pr)
  pure { p with defns := fs, procs := ps }

end Parse

/-- Parse a program from the concrete syntax of doc/eidos.md §9 — the
machine-level `.syn` format (no `top` line), or a legacy Eidos
machine-level `.eir` dump with one — reconstructing the binder types
the format leaves implicit. The optional `fname` is used in error
messages. -/
def parseSyn (input : String) (fname : String := "<input>") : Except String Program := do
  let toks ← Parse.lexAll fname input
  match Parse.pProgram.run { fname := fname, toks := toks, i := 0 } with
  | .error e _ => .error e
  | .ok p _    => Parse.withCtx fname (Parse.elabProgram p)

end Rwv.Synolon

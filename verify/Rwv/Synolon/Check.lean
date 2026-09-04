/-
The Synolon well-formedness judgment, machine half (doc/synolon.md §4,
§6): the per-process machine rules (Lint.hs checkProc — cells, block
parameters and commands, terminators, pause targets and the
resumed-input convention, guardedness), pure-acyclicity, and the
whole-program judgment `Program.checkMachine`, over the expression
rules of Rwv.Eidos.Check (whose module header describes the whole
judgment, the builtin signature table, and the carrier-definition
tolerance this half relies on).

The two rules of doc/synolon.md §4 that the Haskell linter
(ReWire.Synolon.Lint) also enforces live here:

* **pure-acyclicity** — the call graph of the pure definitions
  reachable from a process (block bodies, cell initials, and
  transitively) is acyclic (a fueled DFS in the style of
  Rwv.Hyle.Check.checkRecursion; the Hyle checker's recursion rule
  covers the fold's output too). A `rec` binding *reachable from a
  process* is rejected by the same rule (local recursion breaks the
  same well-foundedness; the reference's evaluator-side counterpart is
  Rwv.Eidos.Eval's rejection of `recB`) — an unreachable `rec` is legal
  Eidos and is checked structurally like any binding;
* **representability** — every block parameter, state cell, process
  port, and halt answer has a fixed bit width (`DEnv.sizeOf`, shared
  with the translation's sizing in Rwv.Eidos.Value; the reference's is
  ReWire.Synolon.Repr, shared between its lint and its fold).

Every function is total: recursion over terminators is fueled (fuel
bounds *depth*, generously; exhaustion is an "rwv bug?" error, as in
Rwv.Hyle.Check), everything else is structural. Terminator-case
constructor alternatives compare field-binder types against the
constructor's instantiated field types, exactly as expression-level
`case` does, and terminator-case literal alternatives are
distinctness-checked, like expression-level ones (strictly stronger
than the historical Haskell linter there; see Rwv.Eidos.Check).
-/
import Rwv.Eidos.Check
import Rwv.Synolon.Syntax

namespace Rwv.Synolon

open Std (HashMap HashSet)
open Rwv.Eidos

namespace Check

open Rwv.Eidos.Check
/-! ## Processes (doc/synolon.md §4): the machine rules, per-proc
(Lint.hs checkProc) -/

/-- Fuel for terminator-tree recursion (terminator-case nesting
depth). -/
def termFuel : Nat := 100000

/-- The per-process context: label and cell tables, the process name,
and the input/output types. -/
structure PEnv where
  name  : String
  inTy  : Ty
  outTy : Ty
  ltab  : HashMap Int (Id × Block)
  ctab  : HashMap String Ty

def PEnv.ofProc (pr : Proc) : PEnv where
  name  := pr.name
  inTy  := pr.inTy
  outTy := pr.outTy
  ltab  := HashMap.ofList (pr.blocks.map fun (l, b) => (l.uniq, (l, b)))
  ctab  := HashMap.ofList (pr.cells.map fun c => (c.name, c.ty))

def cellTy (P : PEnv) (s : String) : Except String Ty :=
  match P.ctab.get? s with
  | some t => pure t
  | none   => throw s!"unknown state cell: {s} (process {P.name})"

def target (P : PEnv) (l : Id) : Except String (Id × Block) :=
  match P.ltab.get? l.uniq with
  | some lb => do checkOccSig l lb.1; pure lb
  | none    => throw s!"terminator targets an undeclared block label: {l.render}"

/-- Cell initials are closed (checked in the top-level-only
environment: no locals are in scope) and cell-typed; the cell's type
is representable (doc/synolon.md §4). -/
def checkCell (env : Env) (procName : String) (c : Cell) : Except String Unit := do
  checkTy env c.ty
  match env.Δ.sizeOf szFuel [] c.ty with
  | .ok _    => pure ()
  | .error e =>
      throw s!"state cell {c.name} of process {procName} is not representable at a fixed bit width ({e})"
  match c.init with
  | none   => pure ()
  | some e => do
      let t' ← checkExp (nonTail env) expFuel e
      unless Ty.eq c.ty t' do
        throw s!"the initial value of state cell {c.name} has type {t'.render}, not the cell's type {c.ty.render}"

def checkCmd (P : PEnv) (env : Env) (c : Cmd) : Except String Env :=
  match c with
  | .bind x rhs => do
      checkValueBinder env "command binder" x
      checkAgainst (nonTail env) expFuel rhs x.sig.ty
      pure (bindVar x env)
  | .get x s => do
      checkValueBinder env "command binder" x
      let t ← cellTy P s
      unless Ty.eq x.sig.ty t do
        throw s!"get: binder {x.render} has type {x.sig.ty.render}, not the type of state cell {s} ({t.render})"
      pure (bindVar x env)
  | .put s a => do
      let t ← cellTy P s
      checkAgainst (nonTail env) expFuel a t
      pure env

mutual

def checkTerm (env : Env) (P : PEnv) (fuel : Nat) (t : Term) : Except String Unit :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match t with
    | .pause out l args => do
        checkAgainst (nonTail env) expFuel out P.outTy
        let (lB, b) ← target P l
        if b.params.isEmpty then
          throw s!"pause target {lB.render} has no parameters (the last is the resumed input)"
        unless args.length == b.params.length - 1 do
          throw s!"pause to {lB.render} supplies {args.length} arguments (its target takes {b.params.length - 1} plus the resumed input)"
        (args.zip b.params).forM fun (a, p) => checkAgainst (nonTail env) expFuel a p.sig.ty
    | .goto l args => do
        let (lB, b) ← target P l
        unless args.length == b.params.length do
          throw s!"goto {lB.render} supplies {args.length} arguments (its target takes {b.params.length})"
        (args.zip b.params).forM fun (a, p) => checkAgainst (nonTail env) expFuel a p.sig.ty
    | .halt a => do
        -- The halt answer becomes a slice of the machine-step record
        -- (tagged per distinct answer type), so it needs a fixed
        -- width; its type is otherwise unconstrained.
        let ta ← checkExp (nonTail env) expFuel a
        match env.Δ.sizeOf szFuel [] ta with
        | .ok _    => pure ()
        | .error e =>
            throw s!"a halt answer of process {P.name} is not representable at a fixed bit width ({e})"
    | .cases a alts => do
        let ts ← checkExp (nonTail env) expFuel a
        if (alts.drop 1).any (fun (.mk c _ _) => isDefaultAlt c) then
          throw "the default terminator alternative must come first"
        checkDistinct (alts.filterMap fun (.mk c _ _) =>
          match c with
          | .dataAlt c' => some (c', s!"terminator alternative for constructor {c'}")
          | _           => none)
        checkDistinct (alts.filterMap fun (.mk c _ _) =>
          match c with
          | .litAlt n => some (n, s!"terminator alternative for literal {n}")
          | _         => none)
        if alts.isEmpty then
          throw "terminator case with no alternatives"
        alts.forM (checkTAlt env P fuel ts)
termination_by fuel

def checkTAlt (env : Env) (P : PEnv) (fuel : Nat) (ts : Ty) (a : TAlt) : Except String Unit :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match a with
    | .mk .default xs t => do
        unless xs.isEmpty do throw "default terminator alternative binds fields"
        checkTerm env P fuel t
    | .mk (.litAlt n) xs t => do
        unless xs.isEmpty do throw "literal terminator alternative binds fields"
        match litRep ts with
        | .bad => throw s!"literal terminator alternative on a scrutinee of type {ts.render}"
        | rep =>
            unless fitsRep rep n do
              throw s!"literal {n} is not representable at the scrutinee type {ts.render}"
        checkTerm env P fuel t
    | .mk (.dataAlt c) xs t => do
        let (tcon, sig) ← lookupCon env c
        let fields ← dconFieldTys c tcon sig ts
        unless xs.length == fields.length do
          throw s!"terminator alternative for {c} binds {xs.length} fields (the constructor has {fields.length})"
        xs.forM (checkValueBinder env "pattern binder")
        (xs.zip fields).forM fun (p, ft) =>
          unless Ty.eq p.sig.ty ft do
            throw s!"pattern binder {p.render}: type does not match the constructor's field type {ft.render}"
        checkTerm (xs.foldr bindVar env) P fuel t
termination_by fuel

end

def checkBlock (env : Env) (P : PEnv) (b : Block) : Except String Unit := do
  b.params.forM (checkValueBinder env "block parameter")
  let env' ← b.cmds.foldlM (checkCmd P) (b.params.foldr bindVar env)
  checkTerm env' P termFuel b.term

/-! ### Terminator-tree collectors (fueled, like the checks) -/

mutual

def termHasPause (fuel : Nat) (t : Term) : Except String Bool :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match t with
    | .pause ..     => pure true
    | .cases _ alts => alts.anyM fun (.mk _ _ t') => termHasPause fuel t'
    | _             => pure false
termination_by fuel

end

mutual

def termGotos (fuel : Nat) (acc : Array Id) (t : Term) : Except String (Array Id) :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match t with
    | .goto l _     => pure (acc.push l)
    | .cases _ alts => alts.foldlM (fun acc (.mk _ _ t') => termGotos fuel acc t') acc
    | _             => pure acc
termination_by fuel

end

mutual

def termPauseTargets (fuel : Nat) (acc : Array Int) (t : Term) : Except String (Array Int) :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match t with
    | .pause _ l _  => pure (acc.push l.uniq)
    | .cases _ alts => alts.foldlM (fun acc (.mk _ _ t') => termPauseTargets fuel acc t') acc
    | _             => pure acc
termination_by fuel

end

/-- Pause targets: their last parameter is the resumed input, typed by
the process input type. -/
def checkInput (P : PEnv) (pauseTgts : HashSet Int) (l : Id) (b : Block) :
    Except String Unit := do
  if pauseTgts.contains l.uniq then
    match b.params.getLast? with
    | some p =>
        unless Ty.eq p.sig.ty P.inTy do
          throw s!"the last parameter of pause target {l.render} (the resumed input) has type {p.sig.ty.render}, not the process input type {P.inTy.render}"
    | none => pure ()

/-- The DFS of the signal-guardedness rule: follow goto edges, failing
on a repeated node. Fuel bounds the stack depth, which acyclicity (or
the failure) caps at the block count. -/
def visitGoto (edges : HashMap Int (List Int)) (msg : String) (fuel : Nat)
    (stack : HashSet Int) (u : Int) : Except String Unit :=
  match fuel with
  | 0 => throw "goto subgraph deeper than the block count (rwv bug?)"
  | fuel + 1 =>
    if stack.contains u then throw msg
    else (edges.getD u []).forM (visitGoto edges msg fuel (stack.insert u))
termination_by fuel

/-- Signal-guardedness (doc/synolon.md §4): the goto-only subgraph of the block
graph is acyclic — every cycle crosses a pause. -/
def checkGuarded (pr : Proc) : Except String Unit := do
  let entryGotos ← termGotos termFuel #[] pr.entry.term
  let edges : HashMap Int (List Int) ← pr.blocks.foldlM (init := ∅) fun m (l, b) => do
    pure (m.insert l.uniq ((← termGotos termFuel #[] b.term).toList.map (·.uniq)))
  let msg := s!"process {pr.name}: a cycle of gotos crosses no pause (is recursion guarded by signal?)"
  let fuel := pr.blocks.length + 2
  for l in entryGotos do
    visitGoto edges msg fuel ∅ l.uniq
  for (l, _) in pr.blocks do
    visitGoto edges msg fuel ∅ l.uniq

def checkProc (env : Env) (pr : Proc) : Except String Unit := do
  checkTy env pr.inTy
  checkTy env pr.outTy
  -- The ports are the machine's layout boundary: fixed widths, like
  -- cells and block parameters (doc/synolon.md §4).
  match env.Δ.sizeOf szFuel [] pr.inTy with
  | .ok _    => pure ()
  | .error e =>
      throw s!"the input type of process {pr.name} is not representable at a fixed bit width ({e})"
  match env.Δ.sizeOf szFuel [] pr.outTy with
  | .ok _    => pure ()
  | .error e =>
      throw s!"the output type of process {pr.name} is not representable at a fixed bit width ({e})"
  -- Label distinctness (an addition over Lint.hs checkProc, whose
  -- whole-program mode covered it via uniqSites): the per-proc label
  -- table must be well-defined for target resolution and the fold.
  checkDistinct (pr.blocks.map fun (l, _) =>
    (l.uniq, s!"block label {l.render} of process {pr.name}"))
  checkDistinct (pr.cells.map fun c => (c.name, s!"state cell {c.name} of process {pr.name}"))
  pr.cells.forM (checkCell env pr.name)
  let P := PEnv.ofProc pr
  checkBlock env P pr.entry
  pr.blocks.forM fun (_, b) => checkBlock env P b
  let pauseTgts ← (pr.entry :: pr.blocks.map (·.2)).foldlM
    (fun acc b => termPauseTargets termFuel acc b.term) #[]
  let pauseSet : HashSet Int := HashSet.ofArray pauseTgts
  pr.blocks.forM fun (l, b) => checkInput P pauseSet l b
  let anyPause ← (pr.entry :: pr.blocks.map (·.2)).anyM
    fun b => termHasPause termFuel b.term
  unless anyPause do
    throw s!"process {pr.name} never pauses (no machine to generate)"
  checkGuarded pr

/-! ## Pure-acyclicity (doc/synolon.md §4; the Haskell linter enforces
it too, and the Hyle checker's recursion rule covers the fold's
output): the call graph of the pure definitions reachable from a
process is acyclic, and no `rec` binding is reachable. A fueled DFS
in the style of Rwv.Hyle.Check.checkRecursion. Intrinsic carriers are
leaves (their bodies are the validated error stubs, edge-free); a
reference into any other skipped definition is an error here as it is
at `lookupVar` — recursion must not hide inside an unchecked body. -/

mutual

/-- Collect the definition-reference candidates (all `Var` uniques) of
an expression, rejecting `rec` bindings (reachable local recursion
breaks the same well-foundedness the rule protects). -/
def expRefs (fuel : Nat) (acc : Array Int) (e : Exp) : Except String (Array Int) :=
  match fuel with
  | 0 => throw fuelErr
  | fuel + 1 =>
    match e with
    | .var x        => pure (acc.push x.uniq)
    | .con .. | .prim .. | .litInt .. | .litStr _ => pure acc
    | .litList _ es => es.foldlM (expRefs fuel) acc
    | .litVec _ es  => es.foldlM (expRefs fuel) acc
    | .app e a      => do
        let acc ← expRefs fuel acc e
        match a with
        | .eArg a' => expRefs fuel acc a'
        | .tArg _  => pure acc
    | .lam _ e      => expRefs fuel acc e
    | .letE b e     => do
        let acc ← match b with
          | .nonRec _ rhs => expRefs fuel acc rhs
          | .recB _       =>
              throw "a rec binding is reachable from a process (the pure call graph must be acyclic, doc/synolon.md §4)"
          | .join _ _ jb  => expRefs fuel acc jb
        expRefs fuel acc e
    | .jump _ es    => es.foldlM (expRefs fuel) acc
    | .cases _ scrut _ alts => do
        let acc ← expRefs fuel acc scrut
        alts.foldlM (fun acc (.mk _ _ b) => expRefs fuel acc b) acc
termination_by fuel

end

/-- Follow an edge into a (non-skipped) definition, pushing it on the
DFS stack; leaves (local binders, intrinsic carriers) contribute
nothing, and an edge into an unchecked (polymorphic/reactive) skipped
definition is an error. -/
def enterPure (defns : HashMap Int Defn) (unchecked : HashMap Int Defn) (fuel : Nat)
    (stack done : HashSet Int) (u : Int) : Except String (HashSet Int) :=
  match fuel with
  | 0 => throw "pure call graph deeper than the number of definitions (rwv bug?)"
  | fuel + 1 =>
    match defns.get? u with
    | none   =>
        match unchecked.get? u with
        | some d =>
            throw s!"{d.name.render}, a polymorphic or reactive carrier definition, is reachable from a process (its body is outside the checked machine fragment)"
        | none => pure done
    | some d =>
        if stack.contains u then
          throw s!"recursion among pure definitions reachable from a process: {d.name.render} (the pure call graph must be acyclic, doc/synolon.md §4)"
        else if done.contains u then pure done
        else do
          let refs ← expRefs expFuel #[] d.body
          let stack' := stack.insert u
          let done ← refs.foldlM (fun dn v => enterPure defns unchecked fuel stack' dn v) done
          pure (done.insert u)
termination_by fuel

/-- The reference roots of one process: every expression in its cells
and blocks. -/
def procRefs (pr : Proc) : Except String (Array Int) := do
  let mut acc : Array Int := #[]
  for c in pr.cells do
    if let some e := c.init then acc ← expRefs expFuel acc e
  for b in pr.entry :: pr.blocks.map (·.2) do
    for c in b.cmds do
      match c with
      | .bind _ rhs => acc ← expRefs expFuel acc rhs
      | .put _ a    => acc ← expRefs expFuel acc a
      | .get _ _    => pure ()
    acc ← termExpRefs termFuel acc b.term
  pure acc
where
  termExpRefs (fuel : Nat) (acc : Array Int) (t : Term) : Except String (Array Int) :=
    match fuel with
    | 0 => throw fuelErr
    | fuel + 1 =>
      match t with
      | .pause o _ args => do args.foldlM (expRefs expFuel) (← expRefs expFuel acc o)
      | .goto _ args    => args.foldlM (expRefs expFuel) acc
      | .halt e         => expRefs expFuel acc e
      | .cases s alts   => do
          alts.foldlM (fun acc (.mk _ _ t') => termExpRefs fuel acc t') (← expRefs expFuel acc s)
  termination_by fuel

def checkPureAcyclic (p : Program) : Except String Unit := do
  let checked : HashMap Int Defn :=
    HashMap.ofList ((p.defns.filter (fun d => !skipDefn d)).map fun d => (d.name.uniq, d))
  let unchecked : HashMap Int Defn :=
    HashMap.ofList ((p.defns.filter (fun d => skipDefn d && !intrinsicDefn d)).map
      fun d => (d.name.uniq, d))
  let mut done : HashSet Int := ∅
  for pr in p.procs do
    let roots ← procRefs pr
    for u in roots do
      done ← enterPure checked unchecked (p.defns.length + 1) ∅ done u

end Check

/-! ## The whole-program judgment -/

open Check Rwv.Eidos.Check in
/-- The Synolon well-formedness judgment (module header): the
whole-program pre-ToHyle check — global binder uniqueness and name
distinctness, datatype well-formedness, the definition rules on the
non-carrier definitions, the doc/synolon.md §4 machine rules per process,
pure-acyclicity, and the `top` rule — succeeding exactly when every
rule holds, with the first violation reported. -/
def Program.checkMachine (p : Program) : Except String Unit := do
  let env := mkEnv p.datas p.defns
  checkDistinct (← uniqSites p.datas p.defns)
  checkDistinct (p.datas.map fun d => (d.name, s!"datatype name {d.name}"))
  checkDistinct (p.datas.flatMap fun d => d.cons.map fun c => (c.name, s!"data constructor name {c.name}"))
  checkDistinct (p.procs.map fun pr => (pr.name, s!"process name {pr.name}"))
  p.datas.forM (checkDataDefn env)
  p.defns.forM fun d =>
    if intrinsicDefn d then checkIntrinsicStub d
    else if skipDefn d then pure ()
    else checkDefn env d
  p.procs.forM (checkProc env)
  checkPureAcyclic p
  match p.top with
  | some t => checkTop p.defns t
  | none   => pure ()

end Rwv.Synolon

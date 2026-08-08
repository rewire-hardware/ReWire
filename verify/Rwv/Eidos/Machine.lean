/-
The Eidos-M machine semantics (doc/eidos.md §7.5.3–§7.5.4): machine
states, block-body execution with intra-cycle goto chasing, the
one-cycle step, initialization by entry-block execution, and the
halt-prefix stream semantics.

Termination is by fuel: goto chains (and terminator-case nesting,
which shares the goto fuel) are bounded by guardedness and the block
structure, pure evaluation by pure-acyclicity, on well-formed
processes; the fuel-freeness theorem is not proved here. The goto
fuel and the pure evaluator's fuel are separate parameters.

The η tier: the whole layer threads the bit-level extern environment
`E` (defaulted empty) into the pure evaluator, so a machine's meaning
is parameterized by the interpretations of the model-less
combinational externs it calls — the same environment the Hyle device
run reads, which is what "both runs at the same η" means in the
correspondence statement.
-/
import Rwv.Eidos.Eval
import Std.Data.HashMap

namespace Rwv.Eidos

open Std (HashMap)

/-- A machine state (§7.5.3): a pause-target label (by unique), saved
values for its parameters except the resumed input, and the cell
store. -/
structure MState where
  label : Int
  args  : List Val
  cells : HashMap String Val

/-- The outcome of one block-body execution: a pause (emitted output
and next state) or a halt (the process result). -/
inductive StepOut where
  | step (out : Val) (next : MState)
  | halt (answer : Val)

namespace Machine

/-- Select a terminator-case alternative (§7.5.3, selection as in
§7.5.2, literal matching shared with the pure evaluator): first
matching constructor or literal alternative; the default
(syntactically first, when present) fires only when no other matches.
Terminator cases have no case binder (§7.1). -/
def selectTAlt (Δ : DEnv) (fuel : Nat) (scrut : Val) (alts : List TAlt) :
    Except String (List Id × Term) := do
  for alt in alts do
    match alt with
    | .mk (.dataAlt c) bs t =>
        match scrut with
        | .con _ c' _ => if c = c' then return (bs, t)
        | _ => pure ()
    | .mk (.litAlt n) bs t =>
        if ← Eval.litMatches Δ fuel scrut n then return (bs, t)
    | .mk .default _ _ => pure ()
  match alts.find? (fun | .mk .default _ _ => true | _ => false) with
  | some (.mk _ bs t) => pure (bs, t)
  | none => throw "terminator case: no matching alternative and no default"

/-- Bind constructor fields (when the scrutinee is a constructor
value) under the alternative's binders. -/
def bindFields (env : Eval.Env) (scrut : Val) (bs : List Id) : Eval.Env :=
  match scrut with
  | .con _ _ fields => (bs.zip fields).foldl (fun e (b, v) => (b.uniq, v) :: e) env
  | _ => env

/-- Run a block's commands, threading the environment and cell store
(the command clauses of `X⟦·⟧`, §7.5.3). -/
def runCmds (Δ : DEnv) (defns : HashMap Int Defn) (evalFuel : Nat)
    (env₀ : Eval.Env) (cells₀ : HashMap String Val) (cmds : List Cmd)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) :
    Except String (Eval.Env × HashMap String Val) :=
  cmds.foldlM (init := (env₀, cells₀)) fun (env, cells) cmd => do
    match cmd with
    | .bind x e => do
        let v ← eval Δ defns evalFuel env e E
        pure (((x.uniq, v) :: env), cells)
    | .get x s =>
        match cells.get? s with
        | some v => pure (((x.uniq, v) :: env), cells)
        | none => throw s!"get: unknown cell {s}"
    | .put s a => do
        let v ← eval Δ defns evalFuel env a E
        pure (env, cells.insert s v)

/-- Execute a block body (`X⟦cmds; term⟧` of §7.5.3) under an
environment already binding the block's parameters: thread the cell
store through the commands, then run the terminator. Gotos transfer to
another block intra-cycle, consuming goto fuel (bounded by
goto-acyclicity on well-formed processes). -/
def execBlock (Δ : DEnv) (defns : HashMap Int Defn) (blocks : HashMap Int Block)
    (evalFuel : Nat) (gotoFuel : Nat) (env₀ : Eval.Env) (cells₀ : HashMap String Val)
    (b : Block) (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) :
    Except String StepOut := do
  let (env, cells) ← runCmds Δ defns evalFuel env₀ cells₀ b.cmds E
  runTerm gotoFuel env cells b.term
where
  runTerm (gotoFuel : Nat) (env : Eval.Env) (cells : HashMap String Val) :
      Term → Except String StepOut
    | .pause out l args => do
        let o ← eval Δ defns evalFuel env out E
        let vs ← args.mapM (eval Δ defns evalFuel env · E)
        pure (.step o ⟨l.uniq, vs, cells⟩)
    | .goto l args => do
        match blocks.get? l.uniq with
        | none => throw s!"goto: unknown block {l.occ}"
        | some blk => do
            let vs ← args.mapM (eval Δ defns evalFuel env · E)
            if vs.length ≠ blk.params.length then
              throw s!"goto {l.occ}: arity mismatch"
            let env' := (blk.params.zip vs).foldl
              (fun e (p, v) => (p.uniq, v) :: e) ([] : Eval.Env)
            match gotoFuel with
            | 0 => throw "goto chain exhausted its fuel (unguarded loop?)"
            | gotoFuel' + 1 => do
                let (env'', cells') ← runCmds Δ defns evalFuel env' cells blk.cmds E
                runTerm gotoFuel' env'' cells' blk.term
    | .halt e => do
        pure (.halt (← eval Δ defns evalFuel env e E))
    | .cases scrutE alts => do
        let scrut ← eval Δ defns evalFuel env scrutE E
        let (bs, t) ← selectTAlt Δ evalFuel scrut alts
        match gotoFuel with
        | 0 => throw "terminator case exhausted its fuel"
        | gotoFuel' + 1 => runTerm gotoFuel' (bindFields env scrut bs) cells t
  termination_by t => (gotoFuel, sizeOf t)

/-- The initial cell store σ₀ (§7.5.4): declared initials evaluated
closed (none exist on the current pipeline); `undef` initials are the
zero value of the cell's type. -/
def initCells (Δ : DEnv) (defns : HashMap Int Defn) (evalFuel : Nat) (p : Proc)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) :
    Except String (HashMap String Val) :=
  p.cells.foldlM (init := (∅ : HashMap String Val)) fun σ c => do
    let v ← match c.init with
      | some e => eval Δ defns evalFuel [] e E
      | none   => Δ.zeroVal evalFuel c.ty
    pure (σ.insert c.name v)

/-- The one-cycle step (§7.5.3): resume the state's block with its
saved arguments and the cycle's input in the last parameter slot. -/
def step (Δ : DEnv) (defns : HashMap Int Defn) (blocks : HashMap Int Block)
    (evalFuel gotoFuel : Nat) (s : MState) (input : Val)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String StepOut := do
  match blocks.get? s.label with
  | none => throw s!"step: unknown label {s.label}"
  | some blk => do
      let vals := s.args ++ [input]
      if vals.length ≠ blk.params.length then
        throw s!"step: resumed block arity mismatch"
      let env := (blk.params.zip vals).foldl
        (fun e (p, v) => (p.uniq, v) :: e) ([] : Eval.Env)
      execBlock Δ defns blocks evalFuel gotoFuel env s.cells blk E

/-- One iteration of `Proc.run`'s fold: step the live machine state on
the cycle's input (pushing the emitted output), record a halt answer
(dropping the state), or — once halted — consume the input as a no-op.
Named (rather than inline in `Proc.run`) so proofs can reason about
the fold by its equations. -/
def foldStep (Δ : DEnv) (defns : HashMap Int Defn) (blocks : HashMap Int Block)
    (evalFuel gotoFuel : Nat) (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) :
    List Val × Option Val × Option MState → Val →
    Except String (List Val × Option Val × Option MState)
  | (acc, halted, s?), i => do
      match s?, halted with
      | some s, none => do
          match ← step Δ defns blocks evalFuel gotoFuel s i E with
          | .step o s' => pure (o :: acc, none, some s')
          | .halt a    => pure (acc, some a, none)
      | _, _ => pure (acc, halted, s?)

end Machine

/-- A finite observable trace (§7.5.4): the outputs up to (and
excluding) the halting cycle, and the process result if it halted. -/
structure MTrace where
  outs   : List Val
  halted : Option Val

/-- The n-prefix of 𝔐⟦P⟧ (§7.5.4): initialize the cells, run the
parameterless entry block to its first pause (the emitted value is
unobservable — the reset step), then iterate the one-cycle step over
the stimulus, ending early at a halt. -/
def Proc.run (Δ : DEnv) (defns : HashMap Int Defn) (evalFuel gotoFuel : Nat)
    (p : Proc) (inputs : List Val)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Except String MTrace := do
  let blocks : HashMap Int Block := HashMap.ofList (p.blocks.map fun (l, b) => (l.uniq, b))
  let σ₀ ← Machine.initCells Δ defns evalFuel p E
  match ← Machine.execBlock Δ defns blocks evalFuel gotoFuel [] σ₀ p.entry E with
  | .halt a => pure ⟨[], some a⟩
  | .step _o s₀ => do
      let (outsRev, halted, _) ← inputs.foldlM
          (init := (([] : List Val), (Option.none : Option Val), some s₀))
          (Machine.foldStep Δ defns blocks evalFuel gotoFuel E)
      pure ⟨outsRev.reverse, halted⟩

end Rwv.Eidos

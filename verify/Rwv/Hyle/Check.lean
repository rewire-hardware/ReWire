/-
Well-formedness checking for Hyle programs: a transcription of
doc/hyle.md §4 (implementation: rewire-backend ReWire.Hyle.Check).
Syntax-directed expression typing — every node's cached width is
verified bottom-up — declaration well-formedness, device scoping with
exactly-once assignment coverage, and acyclicity of the call graph
including extern-model edges.

On programs this checker accepts, the semantics (Rwv.Hyle.Semantics)
hits none of its error cases except the deliberate one (device
instances); Rwv.Hyle.Progress mechanizes this as a theorem
(`Program.run_progress`), under its side conditions (an instance-free
device, a successful `mkFEnv`).
-/
import Rwv.Hyle.Syntax
import Std.Data.HashMap
import Std.Data.HashSet

namespace Rwv.Hyle

open Std (HashMap HashSet)

namespace Check

structure Env where
  defns   : HashMap String Defn
  externs : HashMap String Extern

/-- An assignment-coverage target, typed so quoted names can never
collide across roles (an output literally named `"next r"` is distinct
from register `r`'s next-assignment, and an output cannot alias an
instance-input key). Diagnostics render separately from key identity. -/
inductive AssignKey where
  | output (o : String)
  | next   (r : String)
  | instIn (i p : String)
deriving DecidableEq, Hashable

def AssignKey.render : AssignKey → String
  | .output o => o
  | .next r => s!"next {r}"
  | .instIn i p => s!"{i}.{p}"

/-- Names in scope, with their widths. -/
abbrev Ctx := HashMap String Nat

def mkEnv (p : Program) : Env where
  defns   := HashMap.ofList (p.defns.map fun d => (d.name, d))
  externs := HashMap.ofList (p.externs.map fun e => (e.name, e))

def checkDistinct (what : String) (xs : List String) : Except String Unit := do
  let mut seen : HashSet String := ∅
  for x in xs do
    if seen.contains x then
      throw s!"duplicate {what}: {x}"
    seen := seen.insert x

/-- The result width of a primitive at the given operand widths, or
none if ill-typed (doc/hyle.md §3.3): width-homogeneous except the
shifts (arbitrary amount width), the coercions (with their side
conditions), and rep. -/
def opResultSize : Op → List Nat → Option Nat
  | .add, [n, m] | .sub, [n, m] | .mul, [n, m] | .udiv, [n, m]
  | .umod, [n, m] | .pow, [n, m] | .and, [n, m] | .or, [n, m]
  | .xor, [n, m] =>
      if n = m then some n else none
  | .not, [n] => some n
  | .shl, [n, _] | .lshr, [n, _] | .ashr, [n, _] => some n
  | .eq, [n, m] | .ne, [n, m] | .ult, [n, m] | .ule, [n, m]
  | .ugt, [n, m] | .uge, [n, m] | .slt, [n, m] | .sle, [n, m]
  | .sgt, [n, m] | .sge, [n, m] =>
      if n = m then some 1 else none
  | .redand, [_] | .redor, [_] | .redxor, [_] => some 1
  | .zext m,  [n] => if n ≤ m then some m else none
  | .sext m,  [n] => if 1 ≤ n ∧ n ≤ m then some m else none
  | .trunc m, [n] => if m ≤ n then some m else none
  | .rep k,   [n] => some (k * n)
  | _, _ => none

/-- The width of a combinational extern call: the concatenation of its
outputs. -/
def externResultSize (e : Extern) : Nat :=
  (e.outs.map Prod.snd).sum

def opName : Op → String
  | .add => "add" | .sub => "sub" | .mul => "mul" | .udiv => "udiv"
  | .umod => "umod" | .pow => "pow" | .and => "and" | .or => "or"
  | .xor => "xor" | .not => "not" | .shl => "shl" | .lshr => "lshr"
  | .ashr => "ashr" | .eq => "eq" | .ne => "ne" | .ult => "ult"
  | .ule => "ule" | .ugt => "ugt" | .uge => "uge" | .slt => "slt"
  | .sle => "sle" | .sgt => "sgt" | .sge => "sge" | .redand => "redand"
  | .redor => "redor" | .redxor => "redxor"
  | .zext _ => "zext" | .sext _ => "sext" | .trunc _ => "trunc"
  | .rep _ => "rep"

def checkArgs (who : String) (expected got : List Nat) : Except String Unit := do
  if expected.length ≠ got.length then
    throw s!"call to {who}: expected {expected.length} arguments, got {got.length}"
  for (i, w, w') in (List.range expected.length).zip (expected.zip got) do
    if w ≠ w' then
      throw s!"call to {who}: argument {i} has width {w'} (expected {w})"

/-- Verify every node bottom-up, including its cached width, and return
the expression's width. -/
def checkExp (env : Env) (ctx : Ctx) : Exp → Except String Nat
  | .lit v => .ok v.width
  | .undef w => .ok w
  | .var w x =>
      match ctx.get? x with
      | some w' =>
          if w = w' then .ok w
          else .error s!"variable {x}: cached width {w} does not match its binding ({w'})"
      | none => .error s!"unbound variable: {x}"
  | .cat e₁ e₂ => do
      let s₁ ← checkExp env ctx e₁
      let s₂ ← checkExp env ctx e₂
      .ok (s₁ + s₂)
  | .slice i w e => do
      let s ← checkExp env ctx e
      if i + w ≤ s then .ok w
      else .error s!"slice [{i} +: {w}] out of bounds for width {s}"
  | .prim w op args => do
      let szs ← args.attach.mapM fun ⟨a, _⟩ => checkExp env ctx a
      match opResultSize op szs with
      | some w' =>
          if w = w' then .ok w else .error s!"{opName op}: cached width mismatch"
      | none => .error s!"ill-typed application of {opName op} to operand widths {szs}"
  | .call w f args => do
      match env.defns.get? f with
      | none => .error s!"call to unknown definition: {f}"
      | some d => do
          let szs ← args.attach.mapM fun ⟨a, _⟩ => checkExp env ctx a
          checkArgs f d.sig.params szs
          if w = d.sig.result then .ok w
          else .error s!"call to {f}: cached width mismatch"
  | .xcall w x _cs args => do
      match env.externs.get? x with
      | none => .error s!"call to unknown extern: {x}"
      | some e => do
          if e.kind != .comb then
            throw s!"extern {x} is sequential and cannot be called (instantiate it at device level)"
          if _cs.length ≠ e.generics.length then
            throw s!"extern {x}: expected {e.generics.length} generic arguments, got {_cs.length}"
          let szs ← args.attach.mapM fun ⟨a, _⟩ => checkExp env ctx a
          checkArgs x (e.ins.map Prod.snd) szs
          if w = externResultSize e then .ok w
          else .error s!"call to extern {x}: cached width mismatch"
  | .ite w c t e => do
      let sc ← checkExp env ctx c
      if sc ≠ 1 then throw s!"if condition has width {sc} (expected 1)"
      let st ← checkExp env ctx t
      let se ← checkExp env ctx e
      if st ≠ se then throw s!"if branches have unequal widths ({st} and {se})"
      if w = st then .ok w else .error "if: cached width mismatch"
  | .letE w x rhs body => do
      let s₁ ← checkExp env ctx rhs
      let s₂ ← checkExp env (ctx.insert x s₁) body
      if w = s₂ then .ok w else .error s!"let {x}: cached width mismatch"

def checkExtern (env : Env) (e : Extern) : Except String Unit := do
  checkDistinct s!"port or generic name of extern {e.name}"
    (e.generics ++ e.ins.map Prod.fst ++ e.outs.map Prod.fst)
  for p in e.ins ++ e.outs do
    if p.1.isEmpty then throw s!"extern {e.name}: empty port name"
  if e.outs.isEmpty then throw s!"extern {e.name} has no outputs"
  match e.kind, e.model with
  | .comb, some g =>
      match env.defns.get? g with
      | none => throw s!"extern {e.name}: unknown model defn {g}"
      | some d => do
          if d.sig.params ≠ e.ins.map Prod.snd then
            throw s!"extern {e.name}: model {g}: argument widths do not match the extern's inputs"
          if d.sig.result ≠ externResultSize e then
            throw s!"extern {e.name}: model {g}: result width does not match the extern's outputs"
  | .seq _ _, some _ => throw s!"extern {e.name}: sequential externs cannot carry a model"
  | _, _ => pure ()

def checkDefn (env : Env) (d : Defn) : Except String Unit := do
  if d.params.length ≠ d.sig.params.length then
    throw s!"{d.name}: parameter count does not match signature"
  checkDistinct s!"parameter of {d.name}" d.params
  let sz ← checkExp env (HashMap.ofList (d.params.zip d.sig.params)) d.body
  if sz ≠ d.sig.result then
    throw s!"{d.name}: body width {sz} does not match declared result width {d.sig.result}"

def checkDevice (env : Env) (dev : Device) : Except String Unit := do
  let locals := dev.inputs.map Prod.fst ++ dev.outputs.map Prod.fst
             ++ dev.registers.map (·.name) ++ dev.instances.map (·.name)
             ++ dev.body.filterMap (fun | .sLet x _ => some x | _ => none)
  checkDistinct s!"local name of device {dev.name}" locals
  for x in locals do
    if x.isEmpty then throw "empty device-local name"
    if x.contains '.' then throw s!"device-local name may not contain a dot: {x}"
  for r in dev.registers do
    if r.init.width ≠ r.width then
      throw s!"register {r.name}: initial value width {r.init.width} does not match declared width {r.width}"
  -- Instances: kind/generic checks, and their output ports enter the
  -- ambient context as qualified names.
  let mut instCtx : Ctx := ∅
  for i in dev.instances do
    match env.externs.get? i.ext with
    | none => throw s!"instance {i.name}: unknown extern: {i.ext}"
    | some e => do
        if e.kind == .comb then
          throw s!"instance {i.name}: extern {i.ext} is combinational (call it instead)"
        if i.generics.length ≠ e.generics.length then
          throw s!"instance {i.name}: expected {e.generics.length} generic arguments, got {i.generics.length}"
        for (p, sz) in e.outs do
          instCtx := instCtx.insert s!"{i.name}.{p}" sz
  let outsCtx : Ctx := HashMap.ofList dev.outputs
  let ambient : Ctx := instCtx.union
    (HashMap.ofList dev.inputs |>.union (HashMap.ofList (dev.registers.map fun r => (r.name, r.width))))
  -- Statements, in order: lets extend the context (no forward
  -- references); outputs, register nexts, and instance inputs are
  -- assigned exactly once, at the declared width; outputs are never
  -- readable.
  let mut ctx := ambient
  let mut assigned : HashSet AssignKey := ∅
  let assignOnce (assigned : HashSet AssignKey) (ctx : Ctx) (target : AssignKey) (sz : Nat) (e : Exp) :
      Except String (HashSet AssignKey) := do
    if assigned.contains target then throw s!"{target.render} is assigned more than once"
    let sz' ← checkExp env ctx e
    if sz ≠ sz' then throw s!"assignment to {target.render}: width {sz'} (expected {sz})"
    pure (assigned.insert target)
  for stmt in dev.body do
    match stmt with
    | .sLet x e =>
        let sz ← checkExp env ctx e
        ctx := ctx.insert x sz
    | .sOutput o e =>
        match outsCtx.get? o with
        | none => throw s!"assignment to unknown output: {o}"
        | some sz => assigned ← assignOnce assigned ctx (.output o) sz e
    | .sNext r e =>
        match dev.registers.find? (·.name = r) with
        | none => throw s!"next-assignment to unknown register: {r}"
        | some reg => assigned ← assignOnce assigned ctx (.next r) reg.width e
    | .sInstIn i p e =>
        match dev.instances.find? (·.name = i) with
        | none => throw s!"assignment to unknown instance: {i}"
        | some inst =>
            match env.externs.get? inst.ext with
            | none => pure () -- already reported above
            | some ex =>
                match ex.ins.find? (·.1 = p) with
                | none => throw s!"instance {i} has no input port {p}"
                | some (_, sz) => assigned ← assignOnce assigned ctx (.instIn i p) sz e
  for (o, _) in dev.outputs do
    if ¬ assigned.contains (.output o) then throw s!"output {o} is never assigned"
  for r in dev.registers do
    if ¬ assigned.contains (.next r.name) then throw s!"register {r.name} is never assigned"
  for i in dev.instances do
    match env.externs.get? i.ext with
    | none => pure ()
    | some e =>
        for (p, _) in e.ins do
          if ¬ assigned.contains (.instIn i.name p) then
            throw s!"instance input {i.name}.{p} is never assigned"

/-- Acyclicity of the call graph, including extern-model edges: a DFS,
each definition visited at most once, with the recursion depth bounded
by the number of definitions (fuel). -/
def checkRecursion (env : Env) (defns : List Defn) : Except String Unit := do
  let _ ← defns.foldlM (init := (∅ : HashSet String)) fun done d =>
    if done.contains d.name then pure done
    else do
      let done' ← visit (defns.length + 1) (({} : HashSet String).insert d.name) done d.body
      pure (done'.insert d.name)
where
  /-- DFS over an expression; entering a callee's body consumes fuel. -/
  visit (fuel : Nat) (stack done : HashSet String) (e : Exp) : Except String (HashSet String) :=
    match e with
    | .lit _ | .undef _ | .var _ _ => pure done
    | .cat e₁ e₂ => do visit fuel stack (← visit fuel stack done e₁) e₂
    | .slice _ _ e => visit fuel stack done e
    | .prim _ _ args => args.attach.foldlM (fun acc ⟨a, _⟩ => visit fuel stack acc a) done
    | .ite _ c t e => do
        visit fuel stack (← visit fuel stack (← visit fuel stack done c) t) e
    | .letE _ _ rhs body => do visit fuel stack (← visit fuel stack done rhs) body
    | .call _ g args => do
        let done' ← args.attach.foldlM (fun acc ⟨a, _⟩ => visit fuel stack acc a) done
        enter fuel stack done' g
    | .xcall _ x _ args => do
        let done' ← args.attach.foldlM (fun acc ⟨a, _⟩ => visit fuel stack acc a) done
        match env.externs.get? x >>= (·.model) with
        | some g => enter fuel stack done' g
        | none => pure done'
  termination_by (fuel, sizeOf e, 1)

  /-- Follow an edge into a callee (or extern model), pushing it on the
  DFS stack. -/
  enter (fuel : Nat) (stack done : HashSet String) (g : String) :
      Except String (HashSet String) := do
    if stack.contains g then throw s!"unsupported use of recursion (hyle id: {g})"
    if done.contains g then pure done
    else match fuel, env.defns.get? g with
      | 0, _ => throw "call graph deeper than the number of definitions (rwv bug?)"
      | fuel' + 1, some d => do
          let done' ← visit fuel' (stack.insert g) done d.body
          pure (done'.insert g)
      | _, none => pure done
  termination_by (fuel, 0, 0)

end Check

/-- Check a whole program (doc/hyle.md §4): global-name distinctness,
extern and definition well-formedness, device scoping and coverage,
and call-graph acyclicity. -/
def Program.check (p : Program) : Except String Unit := do
  let env := Check.mkEnv p
  Check.checkDistinct "global name" (p.externs.map (·.name) ++ p.defns.map (·.name))
  for e in p.externs do Check.checkExtern env e
  for d in p.defns do Check.checkDefn env d
  Check.checkDevice env p.device
  Check.checkRecursion env p.defns

end Rwv.Hyle

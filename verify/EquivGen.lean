/-
rwv-hyle-equiv: the Hyle≃Hyle equivalence-certificate generator
(eidos-hyle-validation-plan.md §3.2 (M3), Phase 3 gate).

    rwv-hyle-equiv <raw.rwc> <final.rwc> [--out FILE.lean]
        [--defn-name NAME] [--timeout SEC] [--no-check] [--lake-dir DIR]
        [--hoist] [--max-mb N]

Parses and checks both Hyle programs (Rwv.Hyle.Parse / Rwv.Hyle.Check),
verifies the two devices expose identical interfaces (inputs, outputs,
registers — names, widths, initials), and emits a self-contained Lean
file containing each side's whole-step function over fixed-width
`BitVec` arguments (one per nonzero-width device input and register,
shared names and order across sides):

  - device body statements become `let` bindings in order;
  - definition calls are inlined at generation time: each call site
    `let`-binds its (nonzero-width) arguments and splices the callee's
    body, so `lhs`/`rhs` are two flat first-order `let` chains and the
    proof needs no rewrite-engine unfolding beyond the two root defs
    (measured: `simp only [<980 defs>]` unfolding at MiniISA scale
    exceeds 10^9 simp steps — call-site substitution makes simp-based
    unfolding tree-shaped — while the same expansion done by the
    generator elaborates fine; a byte budget (--max-mb, default 1024)
    guards against pathological DAG blowup, skipping the pair);
  - alternatively --hoist emits each used definition once per side as
    its own `def` (small files, DAG preserved in the term) with
    `simp (maxSteps := 10^9) only [<defs>]` unfolding — fine up to
    roughly 50 KB obligations, hopeless beyond (see above);
  - the step result is one BitVec: the concatenation of all outputs then
    all register-next values in declared order (zero-width skipped);
  - the obligation is `theorem step_equiv : ∀ args, lhs … = rhs …`,
    closed by unfolding and `bv_decide` (kernel-checked modulo
    bv_decide's per-computation compiler axiom for LRAT checking).

Zero-width (BV 0) policy — exactly the semantics' unit treatment
(doc/hyle.md §5.2 edge-case column; same convention as the Phase-0.5
SMT spike): width-0 expressions translate to a unit marker and are
eliminated. Width-0 lets bind unit, width-0 defn params/args are
dropped from signatures/applications, width-0 defns are dropped
entirely (calls to them are unit), width-0 outputs/nexts are dropped
from the step concatenation, Cat with a unit side is the other side,
width-0 slices/lits/undefs are unit, and primitives at width 0 follow
the table: eq/ule/uge/sle/sge = 1, ne/ult/ugt/slt/sgt = 0, redand = 1,
redor = redxor = 0, shift by a unit amount is the value unchanged,
zext m of unit is 0#m.

Op mapping mirrors Rwv.Hyle.Semantics.evalOp construct for construct
(see the comment on each case of `transPrim`).

Skipped pairs (exit 3, `RESULT: SKIP`): devices with instances (clocked
externs), any reachable XCall (bv_decide has no uninterpreted
functions), and pow at operand width > 128 (the static square-and-
multiply expansion would be enormous; pow does not occur in the corpus).

Exit codes: 0 proved (or emitted with --no-check), 1 failure (parse,
check, interface mismatch, or an obligation bv_decide rejects),
2 usage, 3 skip.
-/
import Rwv.Hyle.Syntax
import Rwv.Hyle.Parse
import Rwv.Hyle.Check
import Std.Data.HashMap

open Rwv.Hyle

namespace EquivGen

open Std (HashMap)

/-- A translated term: unit (width 0) or Lean source text of a positive
width. The text is bare — consumers parenthesize when embedding. -/
inductive Tm where
  | unit
  | tm (w : Nat) (s : String)

def Tm.width : Tm → Nat
  | .unit => 0
  | .tm w _ => w

/-- What a call site needs to know about a hoisted definition:
its generated Lean name (none for dropped width-0 defns), the widths of
its kept (nonzero-width) parameters, and its result width. -/
structure DefnInfo where
  leanName   : Option String
  keptParams : List Nat
  result     : Nat

structure GenSt where
  pfx    : String                          -- "L" or "R"
  fresh  : Nat := 0
  budget : Nat := 1 <<< 30                 -- inline-expansion byte budget
  memo   : HashMap String DefnInfo := ∅
  sizes  : HashMap String Nat := ∅         -- defn name ↦ body node count
  defs   : Array String := #[]             -- emitted def texts, callees first
  names  : Array String := #[]             -- their Lean names (for simp only)
  dmap   : Array (String × String) := #[]  -- original name ↦ Lean name

abbrev M := EStateM String GenSt

/-- Skips are errors with a recognizable prefix; `main` sorts them out. -/
def throwSkip (r : String) : M α := throw s!"SKIP:{r}"

def freshNat : M Nat := do
  let st ← get
  set { st with fresh := st.fresh + 1 }
  pure st.fresh

/-- A readability suffix for generated names: alphanumerics survive,
everything else becomes '_'. Uniqueness comes from the fresh counter or
emission index prefixed in front, never from the suffix. -/
def sanitize (s : String) : String :=
  String.ofList ((s.toList.map fun c => if c.isAlphanum || c == '_' then c else '_').take 24)

structure Ctx where
  defns  : HashMap String Defn
  inline : Bool

/-- Structural node count of an expression (for the inline budget). -/
partial def expSize : Exp → Nat
  | .lit _ | .undef _ | .var _ _ => 1
  | .cat a b        => 1 + expSize a + expSize b
  | .slice _ _ e    => 1 + expSize e
  | .prim _ _ es    => 1 + (es.map expSize).foldl (· + ·) 0
  | .call _ _ es    => 1 + (es.map expSize).foldl (· + ·) 0
  | .xcall _ _ _ es => 1 + (es.map expSize).foldl (· + ·) 0
  | .ite _ c t e    => 1 + expSize c + expSize t + expSize e
  | .letE _ _ a b   => 1 + expSize a + expSize b

/-- Charge the inline budget; a blown budget is a (loud) skip, not an
attempt to elaborate an exponential term. -/
def spendBudget (n : Nat) : M Unit := do
  let st ← get
  if st.budget < n then
    throwSkip "inline expansion exceeds the byte budget (--max-mb); pass --hoist or raise it"
  set { st with budget := st.budget - n }

/-- Argument texts that are plain identifiers need no rebinding. -/
def isSimpleName (s : String) : Bool :=
  match s.toList with
  | []      => false
  | c :: cs => (c.isAlpha || c == '_') && cs.all (fun c => c.isAlphanum || c == '_')

/-- Bool-valued comparison text and the width-0 (unit-operand) constant,
per doc/hyle.md §5.2's edge-case column / Semantics.evalOp's `cmp`. -/
def cmpInfo : Op → Option ((String → String → String) × Nat)
  | .eq  => some ((fun a b => s!"{a} == {b}"), 1)
  | .ne  => some ((fun a b => s!"{a} != {b}"), 0)
  | .ult => some ((fun a b => s!"BitVec.ult {a} {b}"), 0)
  | .ule => some ((fun a b => s!"BitVec.ule {a} {b}"), 1)
  | .ugt => some ((fun a b => s!"BitVec.ult {b} {a}"), 0)   -- ugt x y = ult y x
  | .uge => some ((fun a b => s!"BitVec.ule {b} {a}"), 1)   -- uge x y = ule y x
  | .slt => some ((fun a b => s!"BitVec.slt {a} {b}"), 0)
  | .sle => some ((fun a b => s!"BitVec.sle {a} {b}"), 1)
  | .sgt => some ((fun a b => s!"BitVec.slt {b} {a}"), 0)
  | .sge => some ((fun a b => s!"BitVec.sle {b} {a}"), 1)
  | _    => none

/-- Infix text for the width-preserving binary operators. -/
def binOpTxt : Op → Option (String → String → String)
  | .add  => some fun a b => s!"{a} + {b}"
  | .sub  => some fun a b => s!"{a} - {b}"
  | .mul  => some fun a b => s!"{a} * {b}"
  | .udiv => some fun a b => s!"BitVec.smtUDiv {a} {b}"  -- SMT-LIB div-by-zero, as evalOp
  | .umod => some fun a b => s!"{a} % {b}"               -- x % 0 = x, as evalOp
  | .and  => some fun a b => s!"{a} &&& {b}"
  | .or   => some fun a b => s!"{a} ||| {b}"
  | .xor  => some fun a b => s!"{a} ^^^ {b}"
  | _     => none

/-- ⟦op⟧ as Lean source, matching Rwv.Hyle.Semantics.evalOp exactly.
`sz` is the checker-verified result width (used only for sanity). -/
def transPrim (op : Op) (ts : List Tm) : M Tm := do
  match op, ts with
  -- Width-equal binary ops: + - * smtUDiv % &&& ||| ^^^ (evalOp `bin`;
  -- the setWidth reconciliation there is the identity on checked programs).
  | _, [.unit, .unit] =>
      match op with
      | .shl | .lshr | .ashr => pure .unit
      | _ =>
        match cmpInfo op with
        | some (_, z) => pure (.tm 1 s!"{z}#1")
        | none => pure .unit  -- arith/pow at width 0
  | .pow, [.tm n a, .tm n' y] => do
      -- Square-and-multiply over the exponent bits, statically unrolled
      -- (evalOp: powMod x.toNat y.toNat (2^n); BitVec mul wraps mod 2^n).
      unless n == n' do throw "pow: operand width mismatch"
      if n > 128 then throwSkip s!"pow at width {n}: static square-and-multiply expansion too large"
      let k ← freshNat
      let mut lines := #[ s!"let pwx{k} : BitVec {n} := ({a});"
                        , s!"let pwy{k} : BitVec {n} := ({y});"
                        , s!"let pwr{k}_0 : BitVec {n} := 1#{n};"
                        , s!"let pwp{k}_0 : BitVec {n} := pwx{k};" ]
      for i in [0:n] do
        lines := lines.push s!"let pwr{k}_{i+1} : BitVec {n} := if (BitVec.extractLsb' {i} 1 pwy{k}) == (1#1) then pwr{k}_{i} * pwp{k}_{i} else pwr{k}_{i};"
        if i + 1 < n then
          lines := lines.push s!"let pwp{k}_{i+1} : BitVec {n} := pwp{k}_{i} * pwp{k}_{i};"
      pure (.tm n (String.intercalate "\n" lines.toList ++ s!"\npwr{k}_{n}"))
  | _, [.tm w a, .tm w' b] =>
      match binOpTxt op with
      | some f => do
          unless w == w' do throw "binary op: operand width mismatch"
          pure (.tm w (f s!"({a})" s!"({b})"))
      | none =>
        match cmpInfo op with
        | some (f, _) => do
            -- evalOp `cmp`: Bool-valued comparison, then b1.
            unless w == w' do throw "comparison: operand width mismatch"
            pure (.tm 1 s!"if {f s!"({a})" s!"({b})"} then 1#1 else 0#1")
        | none =>
          match op with
          -- Shifts: amount stays a BitVec. Lean's heterogeneous
          -- x <<< y / x >>> y / sshiftRight' are defined as shifts by
          -- y.toNat — literally evalOp's x.bits <<< y.nat etc.
          | .shl  => pure (.tm w s!"({a}) <<< ({b})")
          | .lshr => pure (.tm w s!"({a}) >>> ({b})")
          | .ashr => pure (.tm w s!"BitVec.sshiftRight' ({a}) ({b})")
          | _ => throw "ill-formed binary primitive"
  -- Shift by a width-0 amount: shift by zero (§5.2 edge case).
  | .shl,  [t, .unit] => pure t
  | .lshr, [t, .unit] => pure t
  | .ashr, [t, .unit] => pure t
  | .shl,  [.unit, _] => pure .unit
  | .lshr, [.unit, _] => pure .unit
  | .ashr, [.unit, _] => pure .unit
  | .not, [.unit]   => pure .unit
  | .not, [.tm w a] => pure (.tm w s!"~~~({a})")   -- evalOp: ~~~ x.bits
  -- Reductions (evalOp: redand = x == allOnes, emitted as the literal
  -- 2^w−1; redor = x != 0; redxor = parity, as an xor-fold of bits).
  | .redand, [.unit]   => pure (.tm 1 "1#1")
  | .redand, [.tm w a] => pure (.tm 1 s!"if ({a}) == ({2 ^ w - 1}#{w}) then 1#1 else 0#1")
  | .redor,  [.unit]   => pure (.tm 1 "0#1")
  | .redor,  [.tm w a] => pure (.tm 1 s!"if ({a}) != (0#{w}) then 1#1 else 0#1")
  | .redxor, [.unit]   => pure (.tm 1 "0#1")
  | .redxor, [.tm w a] => do
      if w == 1 then pure (.tm 1 a)
      else do
        let k ← freshNat
        let bits := (List.range w).map fun i => s!"(BitVec.extractLsb' {i} 1 rx{k})"
        pure (.tm 1 s!"let rx{k} : BitVec {w} := ({a});\n{String.intercalate " ^^^ " bits}")
  -- Coercions (evalOp: zext/trunc = setWidth, sext = signExtend).
  | .zext m, [.unit]   => pure (if m == 0 then .unit else .tm m s!"0#{m}")
  | .zext m, [.tm _ a] => pure (if m == 0 then .unit else .tm m s!"BitVec.setWidth {m} ({a})")
  | .sext m, [.tm _ a] => pure (.tm m s!"BitVec.signExtend {m} ({a})")  -- 1 ≤ w ≤ m checked
  | .trunc m, [.unit]   => if m == 0 then pure .unit else throw "trunc: widening unit"
  | .trunc m, [.tm _ a] => pure (if m == 0 then .unit else .tm m s!"BitVec.setWidth {m} ({a})")
  | .rep _, [.unit]   => pure .unit
  | .rep k, [.tm w a] =>
      -- evalOp: x.bits.replicate k (width k*w; the BitVec (w*k) type is
      -- definitionally equal at literal widths).
      if k == 0 then pure .unit
      else if k == 1 then pure (.tm w a)
      else pure (.tm (k * w) s!"BitVec.replicate {k} ({a})")
  | _, _ => throw "ill-formed primitive application"

mutual

/-- Translate an expression under an environment mapping Hyle names to
translated terms. Text convention: results are bare; every embedding
parenthesizes; `let` chains stay flat (the body embeds unwrapped). -/
partial def transExp (ctx : Ctx) (env : HashMap String Tm) : Exp → M Tm
  | .lit v   => pure (if v.width == 0 then .unit else .tm v.width s!"{v.bits.toNat}#{v.width}")
  | .undef w => pure (if w == 0 then .unit else .tm w s!"0#{w}")   -- undef denotes zero (§5.1)
  | .var w x =>
      if w == 0 then pure .unit
      else
        match env.get? x with
        | some (.tm w' s) =>
            if w' == w then pure (.tm w s) else throw s!"variable {x}: width mismatch ({w'} vs {w})"
        | some .unit => throw s!"variable {x}: bound to unit but cached width is {w}"
        | none => throw s!"unbound variable {x}"
  | .cat e₁ e₂ => do
      let t₁ ← transExp ctx env e₁
      let t₂ ← transExp ctx env e₂
      match t₁, t₂ with
      | .unit, t => pure t
      | t, .unit => pure t
      | .tm w₁ s₁, .tm w₂ s₂ =>
          -- Hyle Cat: e₁ is the high bits = Lean ++'s MSB side.
          pure (.tm (w₁ + w₂) s!"({s₁}) ++ ({s₂})")
  | .slice i w e => do
      if w == 0 then pure .unit
      else
        match ← transExp ctx env e with
        | .unit => throw "slice of nonzero width from unit"
        | .tm we s =>
            if i + w ≤ we then pure (.tm w s!"BitVec.extractLsb' {i} {w} ({s})")
            else throw s!"slice [{i} +: {w}] out of range for width {we}"
  | .prim _ op args => do
      transPrim op (← args.mapM (transExp ctx env))
  | .call w f args => do
      if w == 0 then pure .unit   -- width-0 call: unit; args are pure, dropped
      else if ctx.inline then inlineCall ctx env w f args
      else do
        let di ← callInfo ctx f
        let some nm := di.leanName | throw s!"call to {f}: dropped defn with nonzero result"
        unless di.result == w do throw s!"call to {f}: result width mismatch"
        let ts ← args.mapM (transExp ctx env)
        let kept := ts.filterMap fun | .unit => none | .tm w s => some (w, s)
        unless kept.map (·.1) == di.keptParams do throw s!"call to {f}: argument width mismatch"
        if kept.isEmpty then pure (.tm w nm)
        else pure (.tm w (nm ++ " " ++ String.intercalate " " (kept.map fun (_, s) => s!"({s})")))
  | .xcall _ ext _ _ =>
      throwSkip s!"extern call to '{ext}' (bv_decide has no uninterpreted functions)"
  | .ite w c t e => do
      if w == 0 then pure .unit
      else do
        let tc ← transExp ctx env c
        let tt ← transExp ctx env t
        let te ← transExp ctx env e
        match tc, tt, te with
        | .tm 1 sc, .tm _ st, .tm _ se =>
            -- evalOp-side: the checker pins the condition to width 1, so
            -- the semantics' vc.nat ≠ 0 is exactly vc = 1#1.
            pure (.tm w s!"if ({sc}) == (1#1) then ({st}) else ({se})")
        | _, _, _ => throw "ill-formed if"
  | .letE _ x rhs body => do
      match ← transExp ctx env rhs with
      | .unit => transExp ctx (env.insert x .unit) body
      | .tm rw rs => do
          let n ← freshNat
          let nm := s!"x{n}_{sanitize x}"
          match ← transExp ctx (env.insert x (.tm rw nm)) body with
          | .unit => pure .unit   -- width-0 body: the pure rhs is droppable
          | .tm bw bs => pure (.tm bw s!"let {nm} : BitVec {rw} := ({rs});\n{bs}")

/-- Inline a call: `let`-bind the (nonzero-width) arguments, splice the
callee's body under the parameter environment. The call graph is
acyclic (checker-established), so this recursion is well-founded; the
budget bounds the total expansion. -/
partial def inlineCall (ctx : Ctx) (env : HashMap String Tm) (w : Nat) (f : String)
    (args : List Exp) : M Tm := do
  let some d := ctx.defns.get? f | throw s!"call to unknown definition {f}"
  unless d.sig.result == w do throw s!"call to {f}: result width mismatch"
  unless d.params.length == d.sig.params.length do throw s!"defn {f}: malformed parameter list"
  -- Budget: one body's worth of text per instantiation (nested calls
  -- charge for their own bodies as they are reached).
  let sz ← do
    match (← get).sizes.get? f with
    | some n => pure n
    | none =>
        let n := expSize d.body
        modify fun st => { st with sizes := st.sizes.insert f n }
        pure n
  spendBudget (32 * sz)
  let ts ← args.mapM (transExp ctx env)
  let mut benv : HashMap String Tm := ∅
  let mut argLets := ""
  for (p, pw, t) in d.params.zip (d.sig.params.zip ts) do
    match t with
    | .unit => do
        unless pw == 0 do throw s!"call to {f}: unit argument for width-{pw} parameter"
        benv := benv.insert p .unit
    | .tm aw s => do
        unless aw == pw do throw s!"call to {f}: argument width {aw} vs parameter width {pw}"
        if isSimpleName s then
          benv := benv.insert p (.tm aw s)
        else do
          let n ← freshNat
          let nm := s!"p{n}_{sanitize p}"
          argLets := argLets ++ s!"let {nm} : BitVec {aw} := ({s});\n"
          benv := benv.insert p (.tm aw nm)
  match ← transExp ctx benv d.body with
  | .unit => throw s!"defn {f}: unit body with nonzero result width {w}"
  | .tm bw bs => do
      unless bw == w do throw s!"defn {f}: body width {bw} vs declared {w}"
      pure (.tm w (argLets ++ bs))

/-- The hoisted-definition table: translate a callee on first use (the
call graph is acyclic — the checker established it), emitting its `def`
after its own callees, so `defs` is in dependency order. -/
partial def callInfo (ctx : Ctx) (f : String) : M DefnInfo := do
  if let some di := (← get).memo.get? f then
    return di
  let some d := ctx.defns.get? f | throw s!"call to unknown definition {f}"
  let di ← transDefn ctx d
  modify fun st => { st with memo := st.memo.insert f di }
  return di

partial def transDefn (ctx : Ctx) (d : Defn) : M DefnInfo := do
  if d.sig.result == 0 then
    return { leanName := none, keptParams := [], result := 0 }
  let mut env : HashMap String Tm := ∅
  let mut binders : Array String := #[]
  let mut kept : Array Nat := #[]
  for (p, w) in d.params.zip d.sig.params do
    if w == 0 then
      env := env.insert p .unit
    else do
      let nm := s!"p{kept.size}"
      binders := binders.push s!"({nm} : BitVec {w})"
      kept := kept.push w
      env := env.insert p (.tm w nm)
  let .tm bw btxt ← transExp ctx env d.body
    | throw s!"defn {d.name}: body is unit but result width is {d.sig.result}"
  unless bw == d.sig.result do throw s!"defn {d.name}: body width {bw} vs declared {d.sig.result}"
  let st ← get
  let leanName := s!"{st.pfx}_{st.defs.size}_{sanitize d.name}"
  let sig := if binders.isEmpty then "" else " " ++ String.intercalate " " binders.toList
  let text := s!"/-- {st.pfx}: `{d.name}` -/\ndef {leanName}{sig} : BitVec {d.sig.result} :=\n  ({btxt})\n"
  modify fun st => { st with defs  := st.defs.push text
                           , names := st.names.push leanName
                           , dmap  := st.dmap.push (d.name, leanName) }
  return { leanName := some leanName, keptParams := kept.toList, result := d.sig.result }

end

structure SideOut where
  defs     : Array String
  defNames : Array String
  dmap     : Array (String × String)
  mainText : String
  width    : Nat

/-- Translate one side's whole device step: statements in order as `let`
bindings, then the concatenation of outputs and register nexts in
declared order (§6.3, zero-width dropped). -/
def genSide (ctx : Ctx) (dev : Device) (env0 : HashMap String Tm)
    (mainName binderTxt : String) : M SideOut := do
  let mut env := env0
  let mut lines : Array String := #[]
  let mut outs : HashMap String Tm := ∅
  let mut nexts : HashMap String Tm := ∅
  let bindStmt (kind : String) (x : String) (t : Tm) (lines : Array String) :
      M (Tm × Array String) := do
    match t with
    | .unit => pure (.unit, lines)
    | .tm w s => do
        let n ← freshNat
        let nm := s!"{kind}{n}_{sanitize x}"
        pure (.tm w nm, lines.push s!"  let {nm} : BitVec {w} := ({s});")
  for st in dev.body do
    match st with
    | .sLet x e => do
        let (t, lines') ← bindStmt "x" x (← transExp ctx env e) lines
        lines := lines'
        env := env.insert x t
    | .sOutput o e => do
        let (t, lines') ← bindStmt "o" o (← transExp ctx env e) lines
        lines := lines'
        outs := outs.insert o t
    | .sNext r e => do
        let (t, lines') ← bindStmt "n" r (← transExp ctx env e) lines
        lines := lines'
        nexts := nexts.insert r t
    | .sInstIn i _ _ => throw s!"instance statement for {i} (instances should have been skipped)"
  -- The step result: outputs then register nexts, declared order.
  let mut parts : Array (String × Nat) := #[]
  let take (what : String) (m : HashMap String Tm) (x : String) (w : Nat)
      (parts : Array (String × Nat)) : M (Array (String × Nat)) := do
    match m.get? x with
    | some (.tm w' s) => do
        unless w' == w do throw s!"{what} {x}: width {w'} vs declared {w}"
        pure (parts.push (s, w))
    | some .unit => do
        unless w == 0 do throw s!"{what} {x}: unit but declared width {w}"
        pure parts
    | none => throw s!"{what} {x} never assigned"
  for (o, w) in dev.outputs do
    parts ← take "output" outs o w parts
  for r in dev.registers do
    parts ← take "register next for" nexts r.name r.width parts
  let width := parts.foldl (fun acc p => acc + p.2) 0
  let concatTxt :=
    match parts.toList with
    | [] => "-- no nonzero-width outputs or register nexts"
    | p :: ps => ps.foldl (fun (acc : String) (q : String × Nat) => s!"({acc}) ++ ({q.1})") p.1
  let mainText := s!"def {mainName}{binderTxt} : BitVec {width} :=\n"
    ++ String.intercalate "\n" lines.toList
    ++ (if lines.isEmpty then "" else "\n")
    ++ s!"  {concatTxt}\n"
  let st ← get
  pure { defs := st.defs, defNames := st.names, dmap := st.dmap
       , mainText := mainText, width := width }

/-! ## Interface comparison -/

def showPorts (ps : List (String × Nat)) : String :=
  String.intercalate ", " (ps.map fun (n, w) => s!"{n}:[{w}]")

def showRegs (rs : List Register) : String :=
  String.intercalate ", " (rs.map fun r => s!"{r.name}:[{r.width}] init {r.init.bits.toNat}")

/-- Identical device interfaces: inputs, outputs, registers — names,
widths, order, initials. Returns a loud diff on mismatch. -/
def compatible (d₁ d₂ : Device) : Except String Unit := do
  unless d₁.inputs == d₂.inputs do
    throw s!"device inputs differ:\n  raw:   {showPorts d₁.inputs}\n  final: {showPorts d₂.inputs}"
  unless d₁.outputs == d₂.outputs do
    throw s!"device outputs differ:\n  raw:   {showPorts d₁.outputs}\n  final: {showPorts d₂.outputs}"
  let regKey := fun (r : Register) => (r.name, r.width, r.init.width, r.init.bits.toNat)
  unless d₁.registers.map regKey == d₂.registers.map regKey do
    throw s!"device registers differ:\n  raw:   {showRegs d₁.registers}\n  final: {showRegs d₂.registers}"

end EquivGen

/-! ## Driver -/

open EquivGen

structure Args where
  rawFile   : String
  finalFile : String
  outFile   : Option String := none
  thmName   : String := "step_equiv"
  timeout   : Nat := 120
  noCheck   : Bool := false
  hoist     : Bool := false
  maxMB     : Nat := 1024
  lakeDir   : Option String := none

def usage : String :=
  "usage: rwv-hyle-equiv <raw.rwc> <final.rwc> [--out FILE.lean] [--defn-name NAME] [--timeout SEC] [--no-check] [--hoist] [--max-mb N] [--lake-dir DIR]"

def parseArgs (argv : List String) : Except String Args := do
  let mut positional : List String := []
  let mut outFile : Option String := none
  let mut thmName := "step_equiv"
  let mut timeout := 120
  let mut noCheck := false
  let mut hoist := false
  let mut maxMB := 1024
  let mut lakeDir : Option String := none
  let mut rest := argv
  repeat
    match rest with
    | [] => break
    | "--out" :: f :: more => outFile := some f; rest := more
    | "--defn-name" :: n :: more => thmName := n; rest := more
    | "--timeout" :: t :: more =>
        match t.toNat? with
        | some v => timeout := v; rest := more
        | none   => throw s!"--timeout: expected a number, got '{t}'"
    | "--max-mb" :: t :: more =>
        match t.toNat? with
        | some v => maxMB := v; rest := more
        | none   => throw s!"--max-mb: expected a number, got '{t}'"
    | "--no-check" :: more => noCheck := true; rest := more
    | "--hoist" :: more => hoist := true; rest := more
    | "--lake-dir" :: d :: more => lakeDir := some d; rest := more
    | arg :: more =>
        if arg.startsWith "--" then throw s!"unknown option: {arg}"
        else positional := positional ++ [arg]
        rest := more
  match positional with
  | [raw, fin] => return { rawFile := raw, finalFile := fin, outFile, thmName, timeout, noCheck, hoist, maxMB, lakeDir }
  | _ => throw usage

def parseChecked (path : String) : IO (Except String Program) := do
  let contents ← IO.FS.readFile ⟨path⟩
  match Rwv.Hyle.parseProgram contents path with
  | .error e => pure (.error s!"parse error: {e}")
  | .ok p =>
    match p.check with
    | .error e => pure (.error s!"check failed: {e}")
    | .ok () => pure (.ok p)

def main (argv : List String) : IO UInt32 := do
  let args ← match parseArgs argv with
    | .error e => IO.eprintln s!"rwv-hyle-equiv: {e}"; return 2
    | .ok a => pure a
  let p₁ ← match ← parseChecked args.rawFile with
    | .error e => IO.eprintln s!"rwv-hyle-equiv: {args.rawFile}: {e}"; return 1
    | .ok p => pure p
  let p₂ ← match ← parseChecked args.finalFile with
    | .error e => IO.eprintln s!"rwv-hyle-equiv: {args.finalFile}: {e}"; return 1
    | .ok p => pure p

  unless p₁.device.instances.isEmpty && p₂.device.instances.isEmpty do
    IO.println "RESULT: SKIP reason=device has extern instances (clocked externs)"
    return 3

  if let .error e := compatible p₁.device p₂.device then
    IO.eprintln s!"rwv-hyle-equiv: DEVICE INTERFACE MISMATCH:\n{e}"
    return 1

  -- Shared argument list: one BitVec binder per nonzero-width input and
  -- register, in declared order (identical on both sides by the check).
  let dev := p₁.device
  let ports : List (String × String × Nat × Option Nat) :=
    dev.inputs.map (fun (n, w) => ("input", n, w, none))
    ++ dev.registers.map (fun r => ("register", r.name, r.width, some r.init.bits.toNat))
  let mut env0 : Std.HashMap String Tm := ∅
  let mut argMeta : Array (String × String × String × Nat × Option Nat) := #[]
  for (kind, n, w, init) in ports do
    if w == 0 then
      env0 := env0.insert n .unit
    else do
      let nm := s!"a{argMeta.size}"
      argMeta := argMeta.push (nm, kind, n, w, init)
      env0 := env0.insert n (.tm w nm)
  let binderTxt := String.intercalate "" (argMeta.toList.map fun (nm, _, _, w, _) => s!" ({nm} : BitVec {w})")
  let argApp := String.intercalate "" (argMeta.toList.map fun (nm, _, _, _, _) => s!" {nm}")

  let run (pfx : String) (p : Program) (mainName : String) : Except String SideOut :=
    let ctx : Ctx := { defns := Std.HashMap.ofList (p.defns.map fun d => (d.name, d))
                     , inline := !args.hoist }
    match (genSide ctx p.device env0 mainName binderTxt).run
            { pfx := pfx, budget := args.maxMB * 1024 * 1024 } with
    | .ok so _ => .ok so
    | .error e _ => .error e
  let sides : Except String (SideOut × SideOut) := do
    pure (← run "L" p₁ "lhs", ← run "R" p₂ "rhs")
  let (soL, soR) ← match sides with
    | .error e =>
        if e.startsWith "SKIP:" then
          IO.println s!"RESULT: SKIP reason={e.drop 5}"
          return 3
        else do
          IO.eprintln s!"rwv-hyle-equiv: translation: {e}"
          return 1
    | .ok s => pure s

  unless soL.width == soR.width do
    IO.eprintln s!"rwv-hyle-equiv: internal: step widths differ ({soL.width} vs {soR.width})"
    return 1

  -- Assemble the obligation file.
  let header := String.intercalate "\n" <|
    [ "-- Generated by rwv-hyle-equiv (do not edit)."
    , s!"-- raw:   {args.rawFile}"
    , s!"-- final: {args.finalFile}"
    , s!"-- step result: BitVec {soL.width} = outputs then register nexts, declared order, zero-width dropped" ]
    ++ argMeta.toList.map (fun (nm, kind, orig, w, init) =>
         s!"-- ARG {nm} {kind} {orig} {w}" ++ (match init with | some v => s!" init {v}" | none => ""))
    ++ soL.dmap.toList.map (fun (o, l) => s!"-- DEFN L {o} -> {l}")
    ++ soR.dmap.toList.map (fun (o, l) => s!"-- DEFN R {o} -> {l}")
  let simpNames := ["lhs", "rhs"] ++ soL.defNames.toList ++ soR.defNames.toList
  let unfoldTac :=
    if args.hoist then
      -- Unfold throughout: needs the rewrite engine (slow at scale).
      s!"  simp (maxSteps := 1000000000) only [{String.intercalate ", " simpNames}]\n"
    else
      -- Fully-inlined bodies: two root-level unfoldings suffice
      -- (`rewrite`, not `rw`: no rfl attempt on the giant result).
      "  rewrite [lhs, rhs]\n"
  let thm :=
    if soL.width == 0 then
      s!"-- Both step results are zero-width: trivially equivalent.\ntheorem {args.thmName} : True := trivial\n"
    else
      let quant := if argMeta.isEmpty then "" else s!"∀{binderTxt}, "
      s!"theorem {args.thmName} : {quant}lhs{argApp} = rhs{argApp} := by\n"
        ++ "  intros\n"
        ++ unfoldTac
        ++ s!"  all_goals bv_decide (config := \{ timeout := {args.timeout} })\n"
  let body :=
    if soL.width == 0 then thm
    else String.intercalate "\n" (soL.defs.toList ++ soR.defs.toList ++ [soL.mainText, soR.mainText, thm])
  let fileTxt := header ++ "\n\nimport Std.Tactic.BVDecide\n\n"
    ++ "set_option maxHeartbeats 40000000\nset_option maxRecDepth 100000\n"
    ++ "set_option linter.unusedVariables false\n\n"
    ++ "namespace HyleEquiv\n\n" ++ body ++ "\nend HyleEquiv\n"

  let outPath := args.outFile.getD "equiv.lean"
  IO.FS.writeFile ⟨outPath⟩ fileTxt
  let nIns := dev.inputs.length
  let nOuts := dev.outputs.length
  let nRegs := dev.registers.length
  IO.println s!"interface: {nIns} inputs, {nOuts} outputs, {nRegs} registers; step width {soL.width}"
  IO.println s!"defns hoisted: L {soL.defNames.size}, R {soR.defNames.size}"
  IO.println s!"OBLIGATION: {outPath} bytes={fileTxt.utf8ByteSize}"

  if args.noCheck then
    return 0

  let t₀ ← IO.monoMsNow
  let out ← IO.Process.output
    { cmd := "lake", args := #["env", "lean", outPath]
    , cwd := args.lakeDir.map (⟨·⟩) }
  let t₁ ← IO.monoMsNow
  if out.exitCode == 0 then
    IO.println s!"RESULT: PROVED ms={t₁ - t₀} bytes={fileTxt.utf8ByteSize}"
    return 0
  else do
    let txt := out.stdout ++ out.stderr
    IO.eprintln txt
    let kind := if (txt.splitOn "ounterexample").length > 1 then "FAILED(counterexample)" else "FAILED"
    IO.println s!"RESULT: {kind} ms={t₁ - t₀} bytes={fileTxt.utf8ByteSize}"
    return 1

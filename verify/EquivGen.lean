/-
rwv-hyle-equiv: the Hyle≃Hyle equivalence-certificate generator
(eidos-hyle-validation-plan.md §3.2 (M3), Phase 3 gate).

    rwv-hyle-equiv <raw.rwc> <final.rwc> [--out FILE.lean]
        [--defn-name NAME] [--timeout SEC] [--no-check] [--lake-dir DIR]
        [--hoist] [--no-normalize] [--max-mb N]

Parses and checks both Hyle programs (Rwv.Hyle.Parse / Rwv.Hyle.Check),
verifies the two devices expose identical interfaces (inputs, outputs,
registers — names, widths, initials), and emits a self-contained Lean
file containing each side's whole-step function over fixed-width
`BitVec` arguments (one per nonzero-width device input and register,
shared names and order across sides).

Default mode — the normalization/cancellation layer (Phase 3 M3): both
sides' step terms are built into ONE hash-consed DAG (inlining calls at
generation time, memoized per (defn, argument-node) instantiation), so
structurally identical subterms — within a side and ACROSS sides — are
one node. Construction constant-folds through the mechanized semantics'
own `Sem.evalOp` (identical conventions: SMT-LIB division by zero,
shifts by ≥ n, width-0 edges) and applies exactly the local rewrites
`ReWire.Hyle.Transform.partialEval` performs (division/modulus by a
zero literal, the 1-bit eq-to-negation and boolean-mux peepholes,
double-negation cancellation, literal-condition mux selection, slice of
literal/slice/concat with piece splitting, identity slices, adjacent
literal and slice merging in concatenations, wire fusion — implicit,
since names resolve to their defining nodes). The raw and final
programs derive from the same source and the optimizer's rewrites are
local, so after folding the raw side the same way the final side was
folded, most of the DAG is shared and the residual miter is small. The
obligation is emitted as a single

  def miter (args…) : Bool := <shared let prelude> ; lhs' == rhs'
  theorem step_equiv : ∀ args…, miter args… = true

— one topologically-ordered `let` per multi-use (or large) node, used
by both sides; the theorem's meaning is unchanged from the two-def
shape (the step functions agree on all arguments). All of this runs on
the UNTRUSTED generator side: `bv_decide` still checks the final claim.

When the two roots are the SAME node (full cancellation — the common
case, since dedupe/inline/partialEval are exactly what the DAG mirrors),
the residual miter is x == x, and the obligation is emitted as the
common normal form `step` plus `miter := step args == step args`,
closed by `beq_self_eq_true` with `step` left folded. This is not just
an optimization: bv_decide's preprocessing zeta-expands goal `let`s, and
even a REFLEXIVE miter DNFs a 10-minute bound at MiniISA/cubehash scale
(gfmult's x == x costs 43 s), while the fast path is instant. bv_decide
remains the engine whenever a nonzero residual survives cancellation
(mutations produce counterexamples through it, unchanged). Emitted defs
are `noncomputable`: nothing evaluates them, and Lean codegen on a
many-thousand-let body was the next bottleneck (MiniISA: codegen alone
DNFs 10 min; noncomputable, the whole obligation proves in ~80 s).

--no-normalize restores the previous two-def flat-inlined shape (for
A/B measurement); --hoist likewise bypasses normalization. Legacy
behavior, both:

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
import Rwv.Hyle.Semantics
import Std.Data.HashMap

open Rwv.Hyle

deriving instance Hashable for Rwv.Hyle.Op

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

/-! ## The normalization layer: one cross-side hash-consed DAG

Both sides translate into a single node store. `push` is hash-consing:
a structurally identical node (same kind, same width, same children)
is the same node, so any subterm the optimizer left intact is shared
between `lhs'` and `rhs'` for free. The smart constructors fold
constants through `Sem.evalOp` (the mechanized semantics' own table —
no separately-invented edge cases) and mirror the local rewrites of
`ReWire.Hyle.Transform.partialEval`, so the raw side normalizes to the
shapes the optimizer produced on the final side. Children are created
before parents, so node ids are already a topological order and the
emission pass is a single ascending sweep. -/

/-- A DAG node kind. Static parameters live in the kind; children are
node ids. `arg i` is the shared step argument `a{i}`; `lit v` keeps
`v < 2^w`. `mux` children are [c, t, e] with c of width 1; `cat`
children are [hi, lo]; `slice off` has the slicee as its one child. -/
inductive NKind where
  | arg   (i : Nat)
  | lit   (v : Nat)
  | cat
  | slice (off : Nat)
  | mux
  | prim  (op : Op)
deriving BEq, Hashable, Inhabited

structure Node where
  w    : Nat
  kind : NKind
  kids : Array Nat
deriving BEq, Hashable, Inhabited

/-- A translated term in the DAG builder: unit (width 0) or a node. -/
inductive DTm where
  | unit
  | nd (i : Nat)

structure DagSt where
  nodes    : Array Node := #[]
  cons     : HashMap Node Nat := ∅
  budget   : Nat := 1 <<< 30
  sizes    : HashMap String Nat := ∅        -- defn name ↦ body node count
  callMemo : HashMap (String × List (Option Nat)) Nat := ∅

abbrev DM := EStateM String DagSt

def throwSkipD (r : String) : DM α := throw s!"SKIP:{r}"

def spendBudgetD (n : Nat) : DM Unit := do
  let st ← get
  if st.budget < n then
    throwSkipD "inline expansion exceeds the byte budget (--max-mb); pass --hoist or raise it"
  set { st with budget := st.budget - n }

/-- Hash-consed node creation. Children exist before their parent, so
ids are in topological order by construction. -/
def push (n : Node) : DM Nat := do
  let st ← get
  match st.cons.get? n with
  | some i => pure i
  | none =>
      let i := st.nodes.size
      set { st with nodes := st.nodes.push n, cons := st.cons.insert n i }
      pure i

def nodeAt (i : Nat) : DM Node := do
  match (← get).nodes[i]? with
  | some n => pure n
  | none   => throw s!"internal: bad node id {i}"

def mkLit (w v : Nat) : DM Nat := push { w, kind := .lit (v % 2 ^ w), kids := #[] }

def NKind.litVal? : NKind → Option Nat
  | .lit v => some v
  | _      => none

/-- The literal value of a node as the semantics' BV, for `evalOp`. -/
def Node.bv? (n : Node) : Option BV :=
  n.kind.litVal?.map fun v => ⟨n.w, BitVec.ofNat n.w v⟩

mutual

/-- ~~~x, with partialEval's double-negation cancellation (literal
negation folds via the all-literal rule in `mkPrim`; here it is folded
directly since `mkNot` is also entered from the peepholes). -/
partial def mkNot (a : Nat) : DM Nat := do
  let na ← nodeAt a
  match na.kind with
  | .lit v     => mkLit na.w (2 ^ na.w - 1 - v)
  | .prim .not => pure na.kids[0]!
  | _          => push { w := na.w, kind := .prim .not, kids := #[a] }

/-- The mux: literal-condition selection and the 1-bit boolean-mux
peephole (partialEval's `If` rules — a 1-bit mux between distinct
1-bit literals is the condition or its negation). -/
partial def mkMux (w : Nat) (c t e : Nat) : DM Nat := do
  let nc ← nodeAt c
  match nc.kind.litVal? with
  | some v => pure (if v ≠ 0 then t else e)
  | none => do
      let nt ← nodeAt t
      let ne ← nodeAt e
      match w, nc.w, nt.kind.litVal?, ne.kind.litVal? with
      | 1, 1, some tv, some ev =>
          if tv ≠ ev then (if tv == 1 then pure c else mkNot c)
          else push { w, kind := .mux, kids := #[c, t, e] }
      | _, _, _, _ => push { w, kind := .mux, kids := #[c, t, e] }

/-- MSB-first pieces of a cat spine (partialEval's `gather`). -/
partial def catPieces (i : Nat) : DM (List Nat) := do
  let n ← nodeAt i
  match n.kind with
  | .cat => pure ((← catPieces n.kids[0]!) ++ (← catPieces n.kids[1]!))
  | _    => pure [i]

/-- partialEval's `mergeCat` pass over MSB-first pieces: adjacent
literals merge (the left piece supplies the high bits — undefs are
already literals here, §5.1); adjacent slices of the same base merge
when the left slice begins where the right one ends (`unSlice` is
`mkSlice`'s identity rule); a merged piece re-enters the pass. -/
partial def mergePieces : List Nat → DM (List Nat)
  | a :: b :: rest => do
      let na ← nodeAt a
      let nb ← nodeAt b
      match na.kind, nb.kind with
      | .lit va, .lit vb =>
          mergePieces ((← mkLit (na.w + nb.w) (va * 2 ^ nb.w + vb)) :: rest)
      | .slice iL, .slice iR =>
          if na.kids[0]! == nb.kids[0]! && iL == iR + nb.w then
            mergePieces ((← mkSlice iR (na.w + nb.w) na.kids[0]!) :: rest)
          else
            return a :: (← mergePieces (b :: rest))
      | _, _ => return a :: (← mergePieces (b :: rest))
  | l => pure l

/-- Right-nested cat over MSB-first pieces (no re-merging: callers
merge first). -/
partial def buildCat : List Nat → DM Nat
  | []      => throw "internal: empty concatenation"
  | [p]     => pure p
  | p :: ps => do
      let r ← buildCat ps
      let np ← nodeAt p
      let nr ← nodeAt r
      push { w := np.w + nr.w, kind := .cat, kids := #[p, r] }

/-- e₁ ++ e₂ (e₁ high), normalized: gather, merge, rebuild. Both sides
pass through this, so association is canonical and irrelevant to
sharing. -/
partial def mkCat (a b : Nat) : DM Nat := do
  buildCat (← mergePieces ((← catPieces a) ++ (← catPieces b)))

/-- e[off +: k] (k ≥ 1; width-0 slices are unit upstream), with
partialEval's `peSlice` rules: identity slice, slice of literal, slice
of slice, slice of concatenation split at the piece boundaries. Fusion
through named wires is implicit — names resolve to their defining
nodes during translation. -/
partial def mkSlice (off k e : Nat) : DM Nat := do
  let ne ← nodeAt e
  if off == 0 && k == ne.w then pure e
  else match ne.kind with
  | .lit v    => mkLit k (v >>> off)
  | .slice i' => mkSlice (off + i') k ne.kids[0]!
  | .cat => do
      let ps ← catPieces e
      -- MSB-first pieces with LSB offsets.
      let mut rev : List (Nat × Nat × Nat) := []   -- (piece, lsb offset, width), LSB-first
      let mut total := 0
      for p in ps.reverse do
        let np ← nodeAt p
        rev := (p, total, np.w) :: rev
        total := total + np.w
      let mut subs : List Nat := []                -- rebuilt MSB-first
      for (p, o, wp) in rev.reverse do             -- walk LSB-first, cons to the front
        if o < off + k && off < o + wp then
          subs := (← mkSlice (max off o - o) (min (off + k) (o + wp) - max off o) p) :: subs
      buildCat (← mergePieces subs)
  | _ => push { w := k, kind := .slice off, kids := #[e] }

/-- A primitive node. All-literal applications fold through the
mechanized semantics' own `Sem.evalOp` (identical edge-case behavior
by construction); the non-literal rules are exactly partialEval's:
SMT-LIB division/modulus by a zero literal, the 1-bit eq peephole,
double negation. `pow` and `redxor` expand structurally
(square-and-multiply / xor fold), as the flat text path did. -/
partial def mkPrim (w : Nat) (op : Op) (args : List Nat) : DM Nat := do
  let ns ← args.mapM nodeAt
  if let some bvs := ns.mapM Node.bv? then
    match Sem.evalOp op bvs with
    | .ok bv => do
        unless bv.width == w do throw s!"constant fold: result width {bv.width} vs {w}"
        mkLit w bv.nat
    | .error e => throw s!"constant fold: {e}"
  else
    match op, args, ns with
    | .pow, [x, y], [nx, _] => mkPow nx.w x y
    | .udiv, [_, _], [_, ny] =>
        if ny.kind.litVal? == some 0 then mkLit w (2 ^ w - 1)
        else push { w, kind := .prim op, kids := args.toArray }
    | .umod, [x, _], [_, ny] =>
        if ny.kind.litVal? == some 0 then pure x
        else push { w, kind := .prim op, kids := args.toArray }
    | .eq, [x, y], [nx, ny] =>
        if nx.w == 1 then
          match nx.kind.litVal?, ny.kind.litVal? with
          | some v, _ => if v == 1 then pure y else mkNot y
          | _, some v => if v == 1 then pure x else mkNot x
          | _, _ => push { w, kind := .prim op, kids := args.toArray }
        else push { w, kind := .prim op, kids := args.toArray }
    | .not, [x], _ => mkNot x
    | .redxor, [x], [nx] =>
        if nx.w == 1 then pure x
        else do
          let mut acc ← mkSlice 0 1 x
          for i in [1:nx.w] do
            acc ← mkPrim 1 .xor [acc, ← mkSlice i 1 x]
          pure acc
    | .rep k, [x], _ =>
        if k == 1 then pure x
        else push { w, kind := .prim op, kids := args.toArray }
    | _, _, _ => push { w, kind := .prim op, kids := args.toArray }

/-- Square-and-multiply over the exponent bits (evalOp: powMod), built
as mux/mul nodes so partially-constant operands still fold. Operand
width > 128 is a skip, as in the flat path (pow is absent from the
corpus). -/
partial def mkPow (w : Nat) (x y : Nat) : DM Nat := do
  if w > 128 then
    throwSkipD s!"pow at width {w}: static square-and-multiply expansion too large"
  let mut r ← mkLit w 1
  let mut p := x
  for i in [0:w] do
    let c ← mkSlice i 1 y
    let m ← mkPrim w .mul [r, p]
    r ← mkMux w c m r
    if i + 1 < w then
      p ← mkPrim w .mul [p, p]
  pure r

end

structure DCtx where
  defns : HashMap String Defn

mutual

/-- Width-0-aware primitive translation: exactly `transPrim`'s unit
edge cases (doc/hyle.md §5.2 edge-case column), then the DAG smart
constructors. -/
partial def transPrimD (op : Op) (ts : List DTm) : DM DTm := do
  match op, ts with
  | _, [.unit, .unit] =>
      match op with
      | .shl | .lshr | .ashr => pure .unit
      | _ =>
        match cmpInfo op with
        | some (_, z) => pure (.nd (← mkLit 1 z))
        | none => pure .unit  -- arith/pow at width 0
  | .pow, [.nd a, .nd b] => do
      let na ← nodeAt a
      let nb ← nodeAt b
      unless na.w == nb.w do throw "pow: operand width mismatch"
      pure (.nd (← mkPrim na.w .pow [a, b]))
  | _, [.nd a, .nd b] => do
      let na ← nodeAt a
      let nb ← nodeAt b
      if (binOpTxt op).isSome then do
        unless na.w == nb.w do throw "binary op: operand width mismatch"
        pure (.nd (← mkPrim na.w op [a, b]))
      else if (cmpInfo op).isSome then do
        unless na.w == nb.w do throw "comparison: operand width mismatch"
        pure (.nd (← mkPrim 1 op [a, b]))
      else
        match op with
        | .shl | .lshr | .ashr => pure (.nd (← mkPrim na.w op [a, b]))
        | _ => throw "ill-formed binary primitive"
  -- Shift by a width-0 amount: shift by zero (§5.2 edge case).
  | .shl,  [t, .unit] => pure t
  | .lshr, [t, .unit] => pure t
  | .ashr, [t, .unit] => pure t
  | .shl,  [.unit, _] => pure .unit
  | .lshr, [.unit, _] => pure .unit
  | .ashr, [.unit, _] => pure .unit
  | .not, [.unit]   => pure .unit
  | .not, [.nd a]   => pure (.nd (← mkNot a))
  | .redand, [.unit] => pure (.nd (← mkLit 1 1))
  | .redand, [.nd a] => pure (.nd (← mkPrim 1 .redand [a]))
  | .redor,  [.unit] => pure (.nd (← mkLit 1 0))
  | .redor,  [.nd a] => pure (.nd (← mkPrim 1 .redor [a]))
  | .redxor, [.unit] => pure (.nd (← mkLit 1 0))
  | .redxor, [.nd a] => pure (.nd (← mkPrim 1 .redxor [a]))
  | .zext m, [.unit] => if m == 0 then pure .unit else pure (.nd (← mkLit m 0))
  | .zext m, [.nd a] => if m == 0 then pure .unit else pure (.nd (← mkPrim m (.zext m) [a]))
  | .sext m, [.nd a] => pure (.nd (← mkPrim m (.sext m) [a]))
  | .trunc m, [.unit] => if m == 0 then pure .unit else throw "trunc: widening unit"
  | .trunc m, [.nd a] => if m == 0 then pure .unit else pure (.nd (← mkPrim m (.trunc m) [a]))
  | .rep _, [.unit] => pure .unit
  | .rep k, [.nd a] => do
      if k == 0 then pure .unit
      else do
        let na ← nodeAt a
        pure (.nd (← mkPrim (k * na.w) (.rep k) [a]))
  | _, _ => throw "ill-formed primitive application"

/-- Translate an expression to the DAG under an environment mapping
Hyle names to terms. Hyle lets vanish here — sharing is the DAG's job,
and the emission pass reintroduces `let` bindings by use count. -/
partial def transExpD (ctx : DCtx) (env : HashMap String DTm) : Exp → DM DTm
  | .lit v   => if v.width == 0 then pure .unit else return .nd (← mkLit v.width v.nat)
  | .undef w => if w == 0 then pure .unit else return .nd (← mkLit w 0)  -- undef denotes zero (§5.1)
  | .var w x =>
      if w == 0 then pure .unit
      else
        match env.get? x with
        | some (.nd i) => do
            let n ← nodeAt i
            if n.w == w then pure (.nd i) else throw s!"variable {x}: width mismatch ({n.w} vs {w})"
        | some .unit => throw s!"variable {x}: bound to unit but cached width is {w}"
        | none => throw s!"unbound variable {x}"
  | .cat e₁ e₂ => do
      match (← transExpD ctx env e₁), (← transExpD ctx env e₂) with
      | .unit, t => pure t
      | t, .unit => pure t
      | .nd a, .nd b => return .nd (← mkCat a b)
  | .slice i w e => do
      if w == 0 then pure .unit
      else
        match ← transExpD ctx env e with
        | .unit => throw "slice of nonzero width from unit"
        | .nd a => do
            let na ← nodeAt a
            if i + w ≤ na.w then return .nd (← mkSlice i w a)
            else throw s!"slice [{i} +: {w}] out of range for width {na.w}"
  | .prim _ op args => do
      transPrimD op (← args.mapM (transExpD ctx env))
  | .call w f args => do
      if w == 0 then pure .unit   -- width-0 call: unit; args are pure, dropped
      else inlineCallD ctx env w f args
  | .xcall _ ext _ _ =>
      throwSkipD s!"extern call to '{ext}' (bv_decide has no uninterpreted functions)"
  | .ite w c t e => do
      if w == 0 then pure .unit
      else
        match (← transExpD ctx env c), (← transExpD ctx env t), (← transExpD ctx env e) with
        | .nd ci, .nd ti, .nd ei => do
            let nc ← nodeAt ci
            unless nc.w == 1 do throw "ill-formed if"
            return .nd (← mkMux w ci ti ei)
        | _, _, _ => throw "ill-formed if"
  | .letE _ x rhs body => do
      let t ← transExpD ctx env rhs
      transExpD ctx (env.insert x t) body

/-- Inline a call into the DAG, memoized per (defn, argument nodes):
repeated instantiations are one subgraph. The memo is per side (the
driver clears it between sides — same-named defns differ). The budget
is charged per fresh instantiation only. -/
partial def inlineCallD (ctx : DCtx) (env : HashMap String DTm) (w : Nat) (f : String)
    (args : List Exp) : DM DTm := do
  let some d := ctx.defns.get? f | throw s!"call to unknown definition {f}"
  unless d.sig.result == w do throw s!"call to {f}: result width mismatch"
  unless d.params.length == d.sig.params.length do throw s!"defn {f}: malformed parameter list"
  let ts ← args.mapM (transExpD ctx env)
  let key : String × List (Option Nat) := (f, ts.map fun | .unit => none | .nd i => some i)
  if let some r := (← get).callMemo.get? key then
    return .nd r
  let sz ← do
    match (← get).sizes.get? f with
    | some n => pure n
    | none =>
        let n := expSize d.body
        modify fun st => { st with sizes := st.sizes.insert f n }
        pure n
  spendBudgetD (32 * sz)
  let mut benv : HashMap String DTm := ∅
  for (p, pw, t) in d.params.zip (d.sig.params.zip ts) do
    match t with
    | .unit => do
        unless pw == 0 do throw s!"call to {f}: unit argument for width-{pw} parameter"
        benv := benv.insert p .unit
    | .nd i => do
        let ni ← nodeAt i
        unless ni.w == pw do throw s!"call to {f}: argument width {ni.w} vs parameter width {pw}"
        benv := benv.insert p (.nd i)
  match ← transExpD ctx benv d.body with
  | .unit => throw s!"defn {f}: unit body with nonzero result width {w}"
  | .nd r => do
      let nr ← nodeAt r
      unless nr.w == w do throw s!"defn {f}: body width {nr.w} vs declared {w}"
      modify fun st => { st with callMemo := st.callMemo.insert key r }
      pure (.nd r)

end

/-- Translate one side's whole device step into the DAG: statements in
order, then the concatenation of outputs and register nexts in declared
order (§6.3, zero-width dropped). Returns the root and the step width. -/
def genSideD (ctx : DCtx) (dev : Device) (env0 : HashMap String DTm) : DM (DTm × Nat) := do
  let mut env := env0
  let mut outs : HashMap String DTm := ∅
  let mut nexts : HashMap String DTm := ∅
  for st in dev.body do
    match st with
    | .sLet x e      => env := env.insert x (← transExpD ctx env e)
    | .sOutput o e   => outs := outs.insert o (← transExpD ctx env e)
    | .sNext r e     => nexts := nexts.insert r (← transExpD ctx env e)
    | .sInstIn i _ _ => throw s!"instance statement for {i} (instances should have been skipped)"
  let take := fun (what : String) (m : HashMap String DTm) (x : String) (w : Nat) => do
    match m.get? x with
    | some (.nd i) => do
        let n ← nodeAt i
        unless n.w == w do throw s!"{what} {x}: width {n.w} vs declared {w}"
        pure (some i)
    | some .unit => do
        unless w == 0 do throw s!"{what} {x}: unit but declared width {w}"
        pure (none : Option Nat)
    | none => throw s!"{what} {x} never assigned"
  let mut root : DTm := .unit
  let mut width := 0
  let addPart := fun (root : DTm) (i : Nat) => do
    match root with
    | .unit  => pure (DTm.nd i)
    | .nd r  => return DTm.nd (← mkCat r i)
  for (o, w) in dev.outputs do
    if let some i ← take "output" outs o w then
      root ← addPart root i
      width := width + w
  for r in dev.registers do
    if let some i ← take "register next for" nexts r.name r.width then
      root ← addPart root i
      width := width + r.width
  pure (root, width)

/-! ## DAG emission -/

structure DagStats where
  reachable : Nat
  shared    : Nat
  lhsOnly   : Nat
  rhsOnly   : Nat
  lits      : Nat
  lets      : Nat

/-- Lean source for one non-atom node, from its children's reference
texts (identifiers embed bare, anything else parenthesized). The op
texts match the flat path's exactly (see `transPrim`'s evalOp
citations). -/
def renderNode (nodes : Array Node) (refs : Array String) (i : Nat) : Except String String := do
  let nd := nodes[i]!
  let ref := fun (k : Nat) =>
    let s := refs[k]!
    if isSimpleName s then s else s!"({s})"
  let kw := fun (k : Nat) => nodes[k]!.w
  match nd.kind, nd.kids.toList with
  | .cat, [a, b]    => pure s!"{ref a} ++ {ref b}"
  | .slice off, [a] => pure s!"BitVec.extractLsb' {off} {nd.w} {ref a}"
  | .mux, [c, t, e] => pure s!"if {ref c} == (1#1) then {ref t} else {ref e}"
  | .prim op, kids =>
      match op, kids with
      | .not,     [a] => pure s!"~~~{ref a}"
      | .redand,  [a] => pure s!"if {ref a} == ({2 ^ kw a - 1}#{kw a}) then 1#1 else 0#1"
      | .redor,   [a] => pure s!"if {ref a} != (0#{kw a}) then 1#1 else 0#1"
      | .zext m,  [a] => pure s!"BitVec.setWidth {m} {ref a}"
      | .sext m,  [a] => pure s!"BitVec.signExtend {m} {ref a}"
      | .trunc m, [a] => pure s!"BitVec.setWidth {m} {ref a}"
      | .rep k,   [a] => pure s!"BitVec.replicate {k} {ref a}"
      | .shl,  [a, b] => pure s!"{ref a} <<< {ref b}"
      | .lshr, [a, b] => pure s!"{ref a} >>> {ref b}"
      | .ashr, [a, b] => pure s!"BitVec.sshiftRight' {ref a} {ref b}"
      | _, [a, b] =>
          match binOpTxt op with
          | some f => pure (f (ref a) (ref b))
          | none =>
            match cmpInfo op with
            | some (f, _) => pure s!"if {f (ref a) (ref b)} then 1#1 else 0#1"
            | none => throw "internal: unrenderable primitive node"
      | _, _ => throw "internal: ill-formed primitive node"
  | _, _ => throw "internal: renderNode on atom or ill-formed node"

/-- Render the DAG: one topologically-ordered shared `let` prelude plus
reference texts for the two roots. Atoms (arguments, literals) embed
inline; a node used once inlines into its consumer when its text is
small (the length cap bounds both duplication and nesting depth); every
node used twice or more — in particular everything shared across sides
— binds exactly once. -/
def emitDag (nodes : Array Node) (rootL rootR : Nat) :
    Except String (Array String × String × String × DagStats) := do
  let n := nodes.size
  unless rootL < n && rootR < n do throw "internal: root out of range"
  let mut reachL := Array.replicate n false
  let mut reachR := Array.replicate n false
  reachL := reachL.set! rootL true
  reachR := reachR.set! rootR true
  for j in [0:n] do
    let i := n - 1 - j
    let l := reachL[i]!
    let r := reachR[i]!
    if l || r then
      for k in nodes[i]!.kids do
        if l then reachL := reachL.set! k true
        if r then reachR := reachR.set! k true
  let mut uses := Array.replicate n 0
  uses := uses.set! rootL (uses[rootL]! + 1)
  uses := uses.set! rootR (uses[rootR]! + 1)
  let mut nReach := 0
  let mut nShared := 0
  let mut nL := 0
  let mut nR := 0
  let mut nLits := 0
  for i in [0:n] do
    let l := reachL[i]!
    let r := reachR[i]!
    if l || r then
      nReach := nReach + 1
      if l && r then nShared := nShared + 1
      else if l then nL := nL + 1
      else nR := nR + 1
      if let .lit _ := nodes[i]!.kind then nLits := nLits + 1
      for k in nodes[i]!.kids do
        uses := uses.set! k (uses[k]! + 1)
  let mut refs := Array.replicate n ""
  let mut lets : Array String := #[]
  for i in [0:n] do
    if reachL[i]! || reachR[i]! then
      let nd := nodes[i]!
      match nd.kind with
      | .arg a => refs := refs.set! i s!"a{a}"
      | .lit v => refs := refs.set! i s!"{v}#{nd.w}"
      | _ =>
          let txt ← renderNode nodes refs i
          if uses[i]! ≥ 2 || txt.length > 120 then
            lets := lets.push s!"  let n{i} : BitVec {nd.w} := {txt};"
            refs := refs.set! i s!"n{i}"
          else
            refs := refs.set! i txt
  return (lets, refs[rootL]!, refs[rootR]!,
          { reachable := nReach, shared := nShared, lhsOnly := nL, rhsOnly := nR
          , lits := nLits, lets := lets.size })

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
  rawFile     : String
  finalFile   : String
  outFile     : Option String := none
  thmName     : String := "step_equiv"
  timeout     : Nat := 120
  noCheck     : Bool := false
  hoist       : Bool := false
  noNormalize : Bool := false
  maxMB       : Nat := 1024
  lakeDir     : Option String := none

def usage : String :=
  "usage: rwv-hyle-equiv <raw.rwc> <final.rwc> [--out FILE.lean] [--defn-name NAME] [--timeout SEC] [--no-check] [--hoist] [--no-normalize] [--max-mb N] [--lake-dir DIR]"

def parseArgs (argv : List String) : Except String Args := do
  let mut positional : List String := []
  let mut outFile : Option String := none
  let mut thmName := "step_equiv"
  let mut timeout := 120
  let mut noCheck := false
  let mut hoist := false
  let mut noNormalize := false
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
    | "--no-normalize" :: more => noNormalize := true; rest := more
    | "--lake-dir" :: d :: more => lakeDir := some d; rest := more
    | arg :: more =>
        if arg.startsWith "--" then throw s!"unknown option: {arg}"
        else positional := positional ++ [arg]
        rest := more
  match positional with
  | [raw, fin] => return { rawFile := raw, finalFile := fin, outFile, thmName, timeout, noCheck, hoist, noNormalize, maxMB, lakeDir }
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

  let mut extraHeader : List String := []
  let mut body : String := ""
  let mut stepWidth : Nat := 0
  let quant := if argMeta.isEmpty then "" else s!"∀{binderTxt}, "
  let bvDecideTac := s!"  all_goals bv_decide (config := \{ timeout := {args.timeout} })\n"
  let trivialThm :=
    s!"-- Both step results are zero-width: trivially equivalent.\ntheorem {args.thmName} : True := trivial\n"

  if args.hoist || args.noNormalize then
    -- Legacy path: two per-side step functions, flat-inlined or hoisted.
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
    stepWidth := soL.width

    extraHeader := soL.dmap.toList.map (fun (o, l) => s!"-- DEFN L {o} -> {l}")
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
      if soL.width == 0 then trivialThm
      else
        s!"theorem {args.thmName} : {quant}lhs{argApp} = rhs{argApp} := by\n"
          ++ "  intros\n"
          ++ unfoldTac
          ++ bvDecideTac
    body :=
      if soL.width == 0 then thm
      else String.intercalate "\n" (soL.defs.toList ++ soR.defs.toList ++ [soL.mainText, soR.mainText, thm])
    IO.println s!"defns hoisted: L {soL.defNames.size}, R {soR.defNames.size}"
  else
    -- Normalized path: both sides in one hash-consed, constant-folded
    -- DAG; a single miter with a shared let prelude.
    let buildD : DM (DTm × Nat × DTm × Nat) := do
      let mut env0d : Std.HashMap String DTm := ∅
      let mut idx := 0
      for (_, n, w, _) in ports do
        if w == 0 then
          env0d := env0d.insert n .unit
        else do
          let i ← push { w, kind := .arg idx, kids := #[] }
          env0d := env0d.insert n (.nd i)
          idx := idx + 1
      let ctxL : DCtx := ⟨Std.HashMap.ofList (p₁.defns.map fun d => (d.name, d))⟩
      let ctxR : DCtx := ⟨Std.HashMap.ofList (p₂.defns.map fun d => (d.name, d))⟩
      let (rootL, wL) ← genSideD ctxL p₁.device env0d
      -- The defn tables differ per side: same-named defns are
      -- different objects, so the call memo must not cross sides.
      modify fun st => { st with callMemo := ∅, sizes := ∅ }
      let (rootR, wR) ← genSideD ctxR p₂.device env0d
      pure (rootL, wL, rootR, wR)
    match buildD.run { budget := args.maxMB * 1024 * 1024 } with
    | .error e _ =>
        if e.startsWith "SKIP:" then
          IO.println s!"RESULT: SKIP reason={e.drop 5}"
          return 3
        else do
          IO.eprintln s!"rwv-hyle-equiv: translation: {e}"
          return 1
    | .ok (rootL, wL, rootR, wR) st => do
        unless wL == wR do
          IO.eprintln s!"rwv-hyle-equiv: internal: step widths differ ({wL} vs {wR})"
          return 1
        stepWidth := wL
        if wL == 0 then
          body := trivialThm
        else
          match rootL, rootR with
          | .nd rl, .nd rr =>
              match emitDag st.nodes rl rr with
              | .error e => do
                  IO.eprintln s!"rwv-hyle-equiv: emission: {e}"
                  return 1
              | .ok (letLines, refL, refR, stats) => do
                  let wrapRef := fun (s : String) => if isSimpleName s then s else s!"({s})"
                  body :=
                    if rl == rr then
                      -- Full cancellation: both sides normalized to the
                      -- SAME DAG node. Emit the common normal form once
                      -- and close by reflexivity — `step` stays folded,
                      -- so nothing ever zeta-expands the let chain
                      -- (bv_decide's preprocessing cannot handle even a
                      -- reflexive miter at cubehash scale; see the
                      -- header). The theorem still says raw == final:
                      -- the two sides are this one function.
                      -- noncomputable: nothing evaluates these defs, and
                      -- compiling a many-thousand-let body is the
                      -- dominant cost at MiniISA scale (codegen DNF'd a
                      -- 10-minute bound; noncomputable proves in 81 s).
                      s!"noncomputable def step{binderTxt} : BitVec {wL} :=\n"
                        ++ String.intercalate "\n" letLines.toList
                        ++ (if letLines.isEmpty then "" else "\n")
                        ++ s!"  {wrapRef refL}\n\n"
                        ++ s!"-- miter = (raw step == final step); both normalized to `step`.\n"
                        ++ s!"noncomputable def miter{binderTxt} : Bool :=\n  step{argApp} == step{argApp}\n\n"
                        ++ s!"theorem {args.thmName} : {quant}miter{argApp} = true := by\n"
                        ++ "  intros\n"
                        ++ "  rewrite [miter]\n"
                        ++ "  exact beq_self_eq_true _\n"
                    else
                      s!"noncomputable def miter{binderTxt} : Bool :=\n"
                        ++ String.intercalate "\n" letLines.toList
                        ++ (if letLines.isEmpty then "" else "\n")
                        ++ s!"  {wrapRef refL} == {wrapRef refR}\n\n"
                        ++ s!"-- miter = (raw step == final step); equal for all arguments.\n"
                        ++ s!"theorem {args.thmName} : {quant}miter{argApp} = true := by\n"
                        ++ "  intros\n"
                        ++ "  rewrite [miter]\n"
                        ++ bvDecideTac
                  let statsTxt := s!"nodes={st.nodes.size} reachable={stats.reachable} "
                    ++ s!"shared={stats.shared} lhsOnly={stats.lhsOnly} rhsOnly={stats.rhsOnly} "
                    ++ s!"lits={stats.lits} lets={stats.lets} rootsIdentical={rl == rr}"
                  extraHeader := [s!"-- DAG: {statsTxt}"]
                  IO.println s!"DAG: {statsTxt}"
          | _, _ => do
              IO.eprintln "rwv-hyle-equiv: internal: nonzero step width but unit root"
              return 1

  -- Assemble the obligation file.
  let header := String.intercalate "\n" <|
    [ "-- Generated by rwv-hyle-equiv (do not edit)."
    , s!"-- raw:   {args.rawFile}"
    , s!"-- final: {args.finalFile}"
    , s!"-- step result: BitVec {stepWidth} = outputs then register nexts, declared order, zero-width dropped" ]
    ++ argMeta.toList.map (fun (nm, kind, orig, w, init) =>
         s!"-- ARG {nm} {kind} {orig} {w}" ++ (match init with | some v => s!" init {v}" | none => ""))
    ++ extraHeader
  let fileTxt := header ++ "\n\nimport Std.Tactic.BVDecide\n\n"
    ++ "set_option maxHeartbeats 40000000\nset_option maxRecDepth 100000\n"
    ++ "set_option linter.unusedVariables false\n\n"
    ++ "namespace HyleEquiv\n\n" ++ body ++ "\nend HyleEquiv\n"

  let outPath := args.outFile.getD "equiv.lean"
  IO.FS.writeFile ⟨outPath⟩ fileTxt
  let nIns := dev.inputs.length
  let nOuts := dev.outputs.length
  let nRegs := dev.registers.length
  IO.println s!"interface: {nIns} inputs, {nOuts} outputs, {nRegs} registers; step width {stepWidth}"
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

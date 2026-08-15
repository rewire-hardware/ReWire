/-
Progress and width preservation for the Hyle semantics on checked
programs: the mechanization of Check.lean's header claim that on
programs the §4 checker accepts, evaluation hits none of its error
cases except the deliberate ones.

The deliberate ones are exactly two, and each becomes a side
condition rather than a proof obligation:

  * device instances (`sInstIn`, the instance-free fragment) — the
    theorems take an instance-free device, under which a checked
    device body contains no instance statements at all;
  * model-less extern calls with static generic arguments — the
    checker accepts them (matching generic counts) but `evalExp`
    deliberately rejects them (doc/hyle.md §6.1). `Exp.etaGenericFree`
    is the computable sweep for their absence; note that model-less
    GENERIC-FREE calls read totally through `Sem.xapply`, so they need
    no extern-environment hypothesis at all.

The main results, bottom up:

  * `evalOp_progress` — every primitive at the widths `opResultSize`
    accepts evaluates, at the size it promised;
  * `evalExp_progress` — a `checkExp`-accepted expression evaluates,
    at its checked width, in any valuation supplying the context's
    names at their declared widths, given total closures for the
    expression's `deps`;
  * `mkFEnv_closOk` — on a checked program, every closure a successful
    `mkFEnv` denotes is total at its signature widths (induction along
    the topological order, through Bridge's `FImplements`);
  * `step_progress` / `run_progress` / `Program.run_progress` — a
    checked, instance-free, eta-generic-free device steps and runs on
    every width-disciplined stimulus, producing declared-width outputs
    cycle for cycle.

Together with the correspondence theorem these upgrade conditional
trace agreement toward refinement: a target that satisfies the checked
premises CAN run, so agreement is never vacuous. The composition lives
with the correspondence statement, not here — this file is pure Hyle.
-/
import Rwv.Hyle.Check
import Rwv.Hyle.Bridge

namespace Rwv.Hyle

open Std (HashMap HashSet)

/-! ## The eta-generic-free sweep -/

/-- No model-less extern call carries static generic arguments: the
one checked configuration `evalExp` deliberately rejects
(doc/hyle.md §6.1). Model-carrying calls are unconstrained — their
generics are ignored by the model path. -/
def Exp.etaGenericFree (X : Sem.XEnv) : Exp → Bool
  | .lit _ | .undef _ | .var _ _ => true
  | .cat e₁ e₂ => e₁.etaGenericFree X && e₂.etaGenericFree X
  | .slice _ _ e => e.etaGenericFree X
  | .prim _ _ args => args.attach.all fun ⟨a, _⟩ => a.etaGenericFree X
  | .call _ _ args => args.attach.all fun ⟨a, _⟩ => a.etaGenericFree X
  | .xcall _ ext gs args =>
      (match X.get? ext with
       | some _ => true
       | none => gs.isEmpty)
      && args.attach.all fun ⟨a, _⟩ => a.etaGenericFree X
  | .ite _ c t e => c.etaGenericFree X && t.etaGenericFree X && e.etaGenericFree X
  | .letE _ _ rhs body => rhs.etaGenericFree X && body.etaGenericFree X

def Stmt.etaGenericFree (X : Sem.XEnv) : Stmt → Bool
  | .sLet _ e | .sOutput _ e | .sNext _ e | .sInstIn _ _ e => e.etaGenericFree X

/-- Every expression of the program — definition bodies and the device
body — is eta-generic-free (at the program's own extern table). -/
def Program.etaGenericFree (p : Program) : Bool :=
  let X := Sem.xenv p
  p.defns.all (fun d => d.body.etaGenericFree X)
    && p.device.body.all (fun s => s.etaGenericFree X)

namespace Progress

/-! ## `Except`/`mapM` plumbing (house style: re-proved locally) -/

private theorem except_pure_def {α : Type} (a : α) :
    (pure a : Except String α) = .ok a := rfl

private theorem except_bind_error {α β : Type} (e : String) (f : α → Except String β) :
    ((Except.error e : Except String α) >>= f) = .error e := rfl

private theorem except_bind_ok {α β : Type} (a : α) (f : α → Except String β) :
    (Except.ok a >>= f) = f a := rfl

private theorem bind_ok {α β : Type} {x : Except String α}
    {f : α → Except String β} {b : β} (h : (x >>= f) = .ok b) :
    ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x with
  | error e => exact absurd h (by simp [except_bind_error])
  | ok a => exact ⟨a, rfl, h⟩

/-- Attachment is invisible to `mapM` of a function on the elements. -/
private theorem mapM_attach_erase {α β : Type} (f : α → Except String β) :
    ∀ (as : List α), as.attach.mapM (fun ⟨a, _⟩ => f a) = as.mapM f := by
  intro as
  induction as with
  | nil => rfl
  | cons a as ih =>
      simp only [List.attach_cons, List.mapM_cons, List.mapM_map]
      rw [show ((fun (x : {x // x ∈ a :: as}) => f x.val) ∘
            fun (x : {x // x ∈ as}) => (⟨x.val, by simp [x.property]⟩ : {x // x ∈ a :: as}))
          = fun (x : {x // x ∈ as}) => f x.val from rfl]
      rw [ih]

/-- The same fact at the projection-shaped lambda `evalExp`'s
equations expose (the two lambdas are definitionally equal by
structure eta). -/
private theorem mapM_attach_erase' {α β : Type} (f : α → Except String β) :
    ∀ (as : List α), as.attach.mapM (fun x => f x.val) = as.mapM f :=
  mapM_attach_erase f

/-- Attachment is likewise invisible to `all`. -/
private theorem all_attach_erase {α : Type} (f : α → Bool) :
    ∀ (as : List α), (as.attach.all fun ⟨a, _⟩ => f a) = as.all f := by
  intro as
  induction as with
  | nil => rfl
  | cons a as ih =>
      simp only [List.attach_cons, List.all_cons, List.all_map]
      rw [show ((fun (x : {x // x ∈ a :: as}) => f x.val) ∘
            fun (x : {x // x ∈ as}) => (⟨x.val, by simp [x.property]⟩ : {x // x ∈ a :: as}))
          = fun (x : {x // x ∈ as}) => f x.val from rfl]
      rw [ih]

/-- Each element of a successful `mapM` sweep itself succeeded. -/
private theorem mapM_ok_of_mem {α β : Type} {g : α → Except String β} :
    ∀ {as : List α} {bs : List β}, as.mapM g = .ok bs → ∀ a ∈ as, ∃ b, g a = .ok b := by
  intro as
  induction as with
  | nil => intro bs _ a ha; exact absurd ha (by simp)
  | cons x xs ih =>
      intro bs h a ha
      rw [List.mapM_cons] at h
      obtain ⟨b, hb, h₁⟩ := bind_ok h
      obtain ⟨bs', hbs, _⟩ := bind_ok h₁
      cases List.mem_cons.mp ha with
      | inl he => subst he; exact ⟨b, hb⟩
      | inr has => exact ih hbs a has

/-- Pointwise progress transported through `mapM`: if the checker's
sweep succeeded and every element that checks also evaluates at its
checked width, the evaluation sweep succeeds at those widths. -/
private theorem mapM_progress {α : Type} {ck : α → Except String Nat}
    {ev : α → Except String BV} :
    ∀ {as : List α} {szs : List Nat}, as.mapM ck = .ok szs →
      (∀ a ∈ as, ∀ w, ck a = .ok w → ∃ v, ev a = .ok v ∧ v.width = w) →
      ∃ vs, as.mapM ev = .ok vs ∧ vs.length = szs.length ∧
        ∀ i (h1 : i < vs.length) (h2 : i < szs.length), vs[i].width = szs[i] := by
  intro as
  induction as with
  | nil =>
      intro szs hck _
      rw [List.mapM_nil, except_pure_def] at hck
      injection hck with hck
      subst hck
      exact ⟨[], rfl, rfl, fun i h1 _ => absurd h1 (by simp)⟩
  | cons a as ih =>
      intro szs hck hpt
      rw [List.mapM_cons] at hck
      obtain ⟨w, hw, h₁⟩ := bind_ok hck
      obtain ⟨ws, hws, h₂⟩ := bind_ok h₁
      have h₃ : (Except.ok (w :: ws) : Except String (List Nat)) = .ok szs := h₂
      injection h₃ with h₃
      subst h₃
      obtain ⟨v, hv, hvw⟩ := hpt a List.mem_cons_self w hw
      obtain ⟨vs, hvs, hlen, hpts⟩ := ih hws (fun a ha => hpt a (List.mem_cons_of_mem _ ha))
      refine ⟨v :: vs, ?_, by simpa using hlen, ?_⟩
      · rw [List.mapM_cons, hv, except_bind_ok, hvs, except_bind_ok, except_pure_def]
      · intro i h1 h2
        cases i with
        | zero => simpa using hvw
        | succ i => simpa using hpts i (by simpa using h1) (by simpa using h2)

/-! ## Primitive progress -/

/-- Every primitive evaluates at the widths `opResultSize` accepts,
producing the width it promised. -/
theorem evalOp_progress {op : Op} {szs : List Nat} {w : Nat} {vs : List BV}
    (hsz : Check.opResultSize op szs = some w)
    (hlen : vs.length = szs.length)
    (hw : ∀ i (h1 : i < vs.length) (h2 : i < szs.length), vs[i].width = szs[i]) :
    ∃ r, Sem.evalOp op vs = .ok r ∧ r.width = w := by
  cases op with
  | add | sub | mul | udiv | umod | pow | and | or | xor =>
      -- Width-homogeneous binaries: szs = [n, n], result width n.
      match szs, hsz with
      | [n, m], hsz =>
        rw [Check.opResultSize] at hsz
        split at hsz
        case isTrue heq =>
          injection hsz with hsz
          subst hsz; subst heq
          match vs, hlen with
          | [x, y], _ =>
            have hx : x.width = n := hw 0 (by simp) (by simp)
            exact ⟨_, rfl, hx⟩
        case isFalse => exact absurd hsz (by simp)
  | not =>
      match szs, hsz with
      | [n], hsz =>
        rw [Check.opResultSize] at hsz
        injection hsz with hsz
        subst hsz
        match vs, hlen with
        | [x], _ => exact ⟨_, rfl, hw 0 (by simp) (by simp)⟩
  | shl | lshr | ashr =>
      match szs, hsz with
      | [n, m], hsz =>
        rw [Check.opResultSize] at hsz
        injection hsz with hsz
        subst hsz
        match vs, hlen with
        | [x, y], _ => exact ⟨_, rfl, hw 0 (by simp) (by simp)⟩
  | eq | ne | ult | ule | ugt | uge | slt | sle | sgt | sge =>
      -- Comparisons: szs = [n, n], result width 1.
      match szs, hsz with
      | [n, m], hsz =>
        rw [Check.opResultSize] at hsz
        split at hsz
        case isTrue heq =>
          injection hsz with hsz
          subst hsz; subst heq
          match vs, hlen with
          | [x, y], _ => exact ⟨_, rfl, rfl⟩
        case isFalse => exact absurd hsz (by simp)
  | redand | redor | redxor =>
      match szs, hsz with
      | [n], hsz =>
        rw [Check.opResultSize] at hsz
        injection hsz with hsz
        subst hsz
        match vs, hlen with
        | [x], _ => exact ⟨_, rfl, rfl⟩
  | zext m | sext m | trunc m =>
      match szs, hsz with
      | [n], hsz =>
        rw [Check.opResultSize] at hsz
        split at hsz
        case isTrue =>
          injection hsz with hsz
          subst hsz
          match vs, hlen with
          | [x], _ => exact ⟨_, rfl, rfl⟩
        case isFalse => exact absurd hsz (by simp)
  | rep k =>
      match szs, hsz with
      | [n], hsz =>
        rw [Check.opResultSize] at hsz
        injection hsz with hsz
        subst hsz
        match vs, hlen with
        | [x], _ =>
          refine ⟨_, rfl, ?_⟩
          have hx : x.width = n := hw 0 (by simp) (by simp)
          simp [hx, Nat.mul_comm]

/-! ## Checker inversions -/

/-- A successful width-list comparison: equal lengths, pointwise equal. -/
private theorem checkArgs_ok {who : String} {expected got : List Nat}
    (h : Check.checkArgs who expected got = .ok ()) :
    expected.length = got.length ∧
      ∀ i (h1 : i < expected.length) (h2 : i < got.length), expected[i] = got[i] := by
  unfold Check.checkArgs at h
  split at h
  case isTrue =>
    exact absurd h (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
  case isFalse hlen =>
    rw [Decidable.not_not] at hlen
    dsimp only at h
    obtain ⟨u, hfor, -⟩ := bind_ok h
    refine ⟨hlen, ?_⟩
    have hloop : ∀ (zs : List (Nat × Nat × Nat)) (u : PUnit),
        (forIn zs PUnit.unit (fun x __s =>
          if x.2.1 ≠ x.2.2 then do
            throw s!"call to {who}: argument {x.1} has width {x.2.2} (expected {x.2.1})"
            pure (ForInStep.yield PUnit.unit)
          else pure (ForInStep.yield PUnit.unit)) : Except String PUnit) = .ok u →
        ∀ z ∈ zs, z.2.1 = z.2.2 := by
      intro zs
      induction zs with
      | nil => intro u _ z hz; exact absurd hz (by simp)
      | cons p ps ih =>
          intro u hf z hz
          rw [List.forIn_cons] at hf
          by_cases hpz : p.2.1 = p.2.2
          · rw [if_neg (by simpa using hpz), except_pure_def, except_bind_ok] at hf
            cases List.mem_cons.mp hz with
            | inl hzp => subst hzp; exact hpz
            | inr hzs => exact ih u hf z hzs
          · rw [if_pos (by simpa using hpz)] at hf
            exact absurd hf
              (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
    intro i h1 h2
    have hz : (i, expected[i], got[i]) ∈ (List.range expected.length).zip (expected.zip got) := by
      have hb : i < ((List.range expected.length).zip (expected.zip got)).length := by
        simp only [List.length_zip, List.length_range]
        omega
      have hi : ((List.range expected.length).zip (expected.zip got))[i]'hb
          = (i, expected[i], got[i]) := by
        simp [List.getElem_zip, List.getElem_range]
      rw [← hi]
      exact List.getElem_mem hb
    exact hloop _ u hfor _ hz


/-! ## The progress predicates -/

/-- The valuation supplies every context-bound name at its declared
width. -/
def CtxAgree (ctx : Check.Ctx) (ρ : HashMap String BV) : Prop :=
  ∀ x w, ctx.get? x = some w → ∃ v, ρ.get? x = some v ∧ v.width = w

/-- The closure spec progress needs of a denoted name: present in the
checker's definition table, denoted in `F`, and total at the
signature's widths, producing the signature's result width. -/
def ClosOk (env : Check.Env) (F : Sem.FEnv) (f : String) : Prop :=
  ∃ d, env.defns.get? f = some d ∧ ∃ fn, F.get? f = some fn ∧
    ∀ vs : List BV, vs.length = d.sig.params.length →
      (∀ i (h1 : i < vs.length) (h2 : i < d.sig.params.length),
        vs[i].width = d.sig.params[i]) →
      ∃ r, fn vs = .ok r ∧ r.width = d.sig.result

/-- Coherence of the checker's extern table with the semantic model
table: every model edge resolves to a definition whose signature is
aligned with the extern's ports (`checkExtern`'s content). -/
def XModelsOk (env : Check.Env) (X : Sem.XEnv) : Prop :=
  ∀ ext g, X.get? ext = some g →
    ∃ ex, env.externs.get? ext = some ex ∧
      ∃ d, env.defns.get? g = some d ∧
        d.sig.params = ex.ins.map Prod.snd ∧
        d.sig.result = Check.externResultSize ex

private theorem ctxAgree_insert {ctx : Check.Ctx} {ρ : HashMap String BV}
    {x : String} {w : Nat} {v : BV}
    (h : CtxAgree ctx ρ) (hw : v.width = w) :
    CtxAgree (ctx.insert x w) (ρ.insert x v) := by
  intro y wy hy
  rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert] at hy
  rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert]
  split at hy
  case isTrue heq =>
      injection hy with hy
      subst hy
      exact ⟨v, by rw [if_pos heq], hw⟩
  case isFalse heq =>
      obtain ⟨v', hv', hvw'⟩ := h y wy (by rw [HashMap.get?_eq_getElem?]; exact hy)
      refine ⟨v', ?_, hvw'⟩
      rw [if_neg heq, ← HashMap.get?_eq_getElem?]
      exact hv'

/-! ## Expression progress -/

/-- A `checkExp`-accepted expression evaluates, at its checked width,
in any valuation supplying the context's names at their declared
widths, given total closures for the expression's `deps` and no
model-less generic extern calls. -/
theorem evalExp_progress {env : Check.Env} {X : Sem.XEnv} {F : Sem.FEnv} {E : Sem.EEnv}
    (hxm : XModelsOk env X) :
    ∀ (e : Exp) (ctx : Check.Ctx) (ρ : HashMap String BV) (w : Nat),
      Check.checkExp env ctx e = .ok w →
      CtxAgree ctx ρ →
      (∀ f ∈ Sem.deps X e, ClosOk env F f) →
      e.etaGenericFree X = true →
      ∃ v, evalExp F X ρ e E = .ok v ∧ v.width = w
  | .lit v, ctx, ρ, w, hck, _, _, _ => by
      simp only [Check.checkExp] at hck
      injection hck with hck
      exact ⟨v, by simp only [evalExp], hck⟩
  | .undef w0, ctx, ρ, w, hck, _, _, _ => by
      simp only [Check.checkExp] at hck
      injection hck with hck
      subst hck
      exact ⟨BV.zero w0, by simp only [evalExp], rfl⟩
  | .var w0 x, ctx, ρ, w, hck, hρ, _, _ => by
      simp only [Check.checkExp] at hck
      cases hctx : ctx.get? x with
      | none => rw [hctx] at hck; exact absurd hck (by simp)
      | some w' =>
          rw [hctx] at hck
          dsimp only at hck
          split at hck
          case isFalse => exact absurd hck (by simp)
          case isTrue heq =>
            injection hck with hck
            subst hck
            obtain ⟨v, hv, hvw⟩ := hρ x w' hctx
            refine ⟨v, ?_, by rw [hvw, heq]⟩
            simp only [evalExp]
            rw [hv]
  | .cat e₁ e₂, ctx, ρ, w, hck, hρ, hF, hgf => by
      simp only [Check.checkExp] at hck
      obtain ⟨s₁, h₁, hck⟩ := bind_ok hck
      obtain ⟨s₂, h₂, hck⟩ := bind_ok hck
      injection hck with hck
      simp only [Exp.etaGenericFree, Bool.and_eq_true] at hgf
      obtain ⟨v₁, hv₁, hw₁⟩ := evalExp_progress (E := E) hxm e₁ ctx ρ s₁ h₁ hρ
        (fun g hg => hF g (by simp only [Sem.deps, List.mem_append]; exact .inl hg)) hgf.1
      obtain ⟨v₂, hv₂, hw₂⟩ := evalExp_progress (E := E) hxm e₂ ctx ρ s₂ h₂ hρ
        (fun g hg => hF g (by simp only [Sem.deps, List.mem_append]; exact .inr hg)) hgf.2
      refine ⟨⟨v₁.width + v₂.width, v₁.bits ++ v₂.bits⟩, ?_, ?_⟩
      · simp only [evalExp]
        rw [hv₁, except_bind_ok, hv₂, except_bind_ok]
      · show v₁.width + v₂.width = w
        omega
  | .slice i w0 e, ctx, ρ, w, hck, hρ, hF, hgf => by
      simp only [Check.checkExp] at hck
      obtain ⟨s, hs, hck⟩ := bind_ok hck
      split at hck
      case isFalse => exact absurd hck (by simp)
      case isTrue =>
        injection hck with hck
        subst hck
        obtain ⟨v, hv, _⟩ := evalExp_progress (E := E) hxm e ctx ρ s hs hρ
          (fun g hg => hF g (by simpa [Sem.deps] using hg))
          (by simpa [Exp.etaGenericFree] using hgf)
        refine ⟨⟨w0, v.bits.extractLsb' i w0⟩, ?_, rfl⟩
        simp only [evalExp]
        rw [hv, except_bind_ok]
  | .prim w0 op args, ctx, ρ, w, hck, hρ, hF, hgf => by
      simp only [Check.checkExp] at hck
      rw [mapM_attach_erase] at hck
      obtain ⟨szs, hszs, hck⟩ := bind_ok hck
      cases hop : Check.opResultSize op szs with
      | none => rw [hop] at hck; exact absurd hck (by simp)
      | some w' =>
          rw [hop] at hck
          dsimp only at hck
          split at hck
          case isFalse => exact absurd hck (by simp)
          case isTrue heq =>
            injection hck with hck
            subst hck
            rw [Exp.etaGenericFree, all_attach_erase, List.all_eq_true] at hgf
            obtain ⟨vs, hvs, hlen, hpts⟩ := mapM_progress hszs
              (fun a ha w hcw => evalExp_progress (E := E) hxm a ctx ρ w hcw hρ
                (fun g hg => hF g (by
                  simp only [Sem.deps, List.mem_flatMap]
                  exact ⟨⟨a, ha⟩, List.mem_attach _ _, hg⟩))
                (hgf a ha))
            obtain ⟨r, hr, hrw⟩ := evalOp_progress hop hlen hpts
            refine ⟨r, ?_, by rw [hrw, heq]⟩
            simp only [evalExp]
            rw [mapM_attach_erase' (fun a => evalExp F X ρ a E) args, hvs, except_bind_ok]
            exact hr
  | .call w0 f args, ctx, ρ, w, hck, hρ, hF, hgf => by
      simp only [Check.checkExp] at hck
      cases hd : env.defns.get? f with
      | none => rw [hd] at hck; exact absurd hck (by simp)
      | some d =>
          rw [hd] at hck
          dsimp only at hck
          rw [mapM_attach_erase] at hck
          obtain ⟨szs, hszs, hck⟩ := bind_ok hck
          obtain ⟨u, hargs, hck⟩ := bind_ok hck
          cases u
          split at hck
          case isFalse => exact absurd hck (by simp)
          case isTrue heq =>
            injection hck with hck
            subst hck
            obtain ⟨halen, halw⟩ := checkArgs_ok hargs
            rw [Exp.etaGenericFree, all_attach_erase, List.all_eq_true] at hgf
            obtain ⟨vs, hvs, hlen, hpts⟩ := mapM_progress hszs
              (fun a ha w hcw => evalExp_progress (E := E) hxm a ctx ρ w hcw hρ
                (fun g hg => hF g (by
                  simp only [Sem.deps, List.mem_cons, List.mem_flatMap]
                  exact .inr ⟨⟨a, ha⟩, List.mem_attach _ _, hg⟩))
                (hgf a ha))
            obtain ⟨d', hd', fn, hfn, htot⟩ := hF f (by simp [Sem.deps])
            rw [hd] at hd'
            injection hd' with hd'
            subst hd'
            obtain ⟨r, hr, hrw⟩ := htot vs (by omega)
              (fun i h1 h2 => by rw [hpts i h1 (by omega), halw i h2 (by omega)])
            refine ⟨r, ?_, by rw [hrw, heq]⟩
            simp only [evalExp]
            rw [mapM_attach_erase' (fun a => evalExp F X ρ a E) args, hvs, except_bind_ok, hfn]
            exact hr
  | .xcall w0 x cs args, ctx, ρ, w, hck, hρ, hF, hgf => by
      simp only [Check.checkExp] at hck
      cases hex : env.externs.get? x with
      | none => rw [hex] at hck; exact absurd hck (by simp)
      | some ex =>
          rw [hex] at hck
          dsimp only at hck
          rw [Exp.etaGenericFree, all_attach_erase, Bool.and_eq_true, List.all_eq_true] at hgf
          split at hck
          case isTrue =>
            exact absurd hck (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
          case isFalse =>
            split at hck
            case isTrue =>
              exact absurd hck (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
            case isFalse =>
              rw [mapM_attach_erase] at hck
              obtain ⟨szs, hszs, hck⟩ := bind_ok hck
              obtain ⟨u, hargs, hck⟩ := bind_ok hck
              cases u
              split at hck
              case isFalse => exact absurd hck (by simp)
              case isTrue heq =>
                injection hck with hck
                subst hck
                obtain ⟨halen, halw⟩ := checkArgs_ok hargs
                obtain ⟨vs, hvs, hlen, hpts⟩ := mapM_progress hszs
                  (fun a ha w hcw => evalExp_progress (E := E) hxm a ctx ρ w hcw hρ
                    (fun g hg => hF g (by
                      simp only [Sem.deps, List.mem_append, List.mem_flatMap]
                      exact .inr ⟨⟨a, ha⟩, List.mem_attach _ _, hg⟩))
                    (hgf.2 a ha))
                cases hX : X.get? x with
                | some model =>
                    obtain ⟨ex', hex', dm, hdm, hsigp, hsigr⟩ := hxm x model hX
                    rw [hex] at hex'
                    injection hex' with hex'
                    subst hex'
                    obtain ⟨dm', hdm', fn, hfn, htot⟩ := hF model (by
                      simp only [Sem.deps, List.mem_append]
                      exact .inl (by rw [hX]; exact List.mem_cons_self))
                    rw [hdm] at hdm'
                    injection hdm' with hdm'
                    subst hdm'
                    have hlen2 : szs.length = ex.ins.length := by
                      rw [← halen, List.length_map]
                    obtain ⟨r, hr, hrw⟩ := htot vs
                      (by rw [hsigp, List.length_map]; omega)
                      (fun i h1 h2 => by
                        have h2m : i < (ex.ins.map Prod.snd).length := by
                          rw [← hsigp]; exact h2
                        have h2i : i < ex.ins.length := by
                          rw [List.length_map] at h2m; exact h2m
                        have e2 := halw i h2m (by omega)
                        have e1 := hpts i h1 (by omega)
                        simp only [hsigp]
                        rw [e1, ← e2])
                    refine ⟨r, ?_, by rw [hrw, hsigr, heq]⟩
                    simp only [evalExp]
                    rw [mapM_attach_erase' (fun a => evalExp F X ρ a E) args, hvs, except_bind_ok, hX]
                    dsimp only
                    rw [hfn]
                    exact hr
                | none =>
                    have hgs : cs.isEmpty = true := by
                      have hh := hgf.1
                      rw [hX] at hh
                      exact hh
                    refine ⟨Sem.xapply E x w0 (Sem.bvcat vs), ?_, ?_⟩
                    · simp only [evalExp]
                      rw [mapM_attach_erase' (fun a => evalExp F X ρ a E) args, hvs, except_bind_ok, hX]
                      dsimp only
                      rw [if_pos hgs]
                    · rw [Sem.xapply_width, heq]
  | .ite w0 c t e, ctx, ρ, w, hck, hρ, hF, hgf => by
      simp only [Check.checkExp] at hck
      obtain ⟨sc, hsc, hck⟩ := bind_ok hck
      simp only [Exp.etaGenericFree, Bool.and_eq_true] at hgf
      split at hck
      case isTrue =>
        exact absurd hck (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
      case isFalse =>
        obtain ⟨st, hst, hck⟩ := bind_ok hck
        obtain ⟨se, hse, hck⟩ := bind_ok hck
        split at hck
        case isTrue =>
          exact absurd hck (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
        case isFalse hne =>
          rw [Decidable.not_not] at hne
          split at hck
          case isFalse => exact absurd hck (by simp)
          case isTrue heq =>
            injection hck with hck
            subst hck
            obtain ⟨vc, hvc, _⟩ := evalExp_progress (E := E) hxm c ctx ρ sc hsc hρ
              (fun g hg => hF g (by
                simp only [Sem.deps, List.mem_append]; exact .inl (.inl hg))) hgf.1.1
            obtain ⟨vt, hvt, hwt⟩ := evalExp_progress (E := E) hxm t ctx ρ st hst hρ
              (fun g hg => hF g (by
                simp only [Sem.deps, List.mem_append]; exact .inl (.inr hg))) hgf.1.2
            obtain ⟨ve, hve, hwe⟩ := evalExp_progress (E := E) hxm e ctx ρ se hse hρ
              (fun g hg => hF g (by
                simp only [Sem.deps, List.mem_append]; exact .inr hg)) hgf.2
            by_cases hc : vc.nat ≠ 0
            · refine ⟨vt, ?_, by rw [hwt, heq]⟩
              simp only [evalExp]
              rw [hvc, except_bind_ok, if_pos hc]
              exact hvt
            · refine ⟨ve, ?_, by rw [hwe, ← hne, heq]⟩
              simp only [evalExp]
              rw [hvc, except_bind_ok, if_neg hc]
              exact hve
  | .letE w0 x rhs body, ctx, ρ, w, hck, hρ, hF, hgf => by
      simp only [Check.checkExp] at hck
      obtain ⟨s₁, h₁, hck⟩ := bind_ok hck
      obtain ⟨s₂, h₂, hck⟩ := bind_ok hck
      split at hck
      case isFalse => exact absurd hck (by simp)
      case isTrue heq =>
        injection hck with hck
        subst hck
        simp only [Exp.etaGenericFree, Bool.and_eq_true] at hgf
        obtain ⟨v₁, hv₁, hw₁⟩ := evalExp_progress (E := E) hxm rhs ctx ρ s₁ h₁ hρ
          (fun g hg => hF g (by simp only [Sem.deps, List.mem_append]; exact .inl hg)) hgf.1
        obtain ⟨v₂, hv₂, hw₂⟩ := evalExp_progress (E := E) hxm body
          (ctx.insert x s₁) (ρ.insert x v₁) s₂ h₂
          (ctxAgree_insert hρ hw₁)
          (fun g hg => hF g (by simp only [Sem.deps, List.mem_append]; exact .inr hg)) hgf.2
        refine ⟨v₂, ?_, by rw [hw₂, heq]⟩
        simp only [evalExp]
        rw [hv₁, except_bind_ok]
        exact hv₂
  termination_by e => sizeOf e
  decreasing_by all_goals
    (first
      | (have := List.sizeOf_lt_of_mem ha; simp +arith; omega)
      | (simp +arith; omega)
      | simp +arith)

/-! ## Association-list transport -/

/-- A successful `ofList` lookup comes from a pair in the list. -/
private theorem ofList_get?_mem {β : Type} {l : List (String × β)} {k : String} {b : β}
    (h : (HashMap.ofList l).get? k = some b) : (k, b) ∈ l := by
  rw [HashMap.get?_eq_getElem?, HashMap.ofList_eq_insertMany_empty,
      HashMap.getElem?_insertMany_list, HashMap.getElem?_empty, Option.or_none] at h
  rw [List.findSomeRev?_eq_findSome?_reverse] at h
  obtain ⟨⟨a, b'⟩, hmem, hab⟩ := List.exists_of_findSome?_eq_some h
  dsimp only at hab
  split at hab
  · rename_i heq
    injection hab with hab
    subst hab
    have : a = k := by simpa using heq
    subst this
    exact List.mem_reverse.mp hmem
  · exact absurd hab (by simp)

/-- With distinct keys, membership determines the `ofList` lookup. -/
private theorem mem_ofList_get? {β : Type} {l : List (String × β)} {k : String} {b : β}
    (hnd : (l.map Prod.fst).Nodup) (hm : (k, b) ∈ l) :
    (HashMap.ofList l).get? k = some b := by
  rw [HashMap.get?_eq_getElem?, HashMap.ofList_eq_insertMany_empty]
  refine HashMap.getElem?_insertMany_list_of_mem (by simp) ?_ hm
  have hp : l.Pairwise (fun p q => ¬p.1 = q.1) := by
    rw [List.Nodup, List.pairwise_map] at hnd
    exact hnd
  exact hp.imp (by intro p q h; simpa using h)

/-- The name-keyed definition table: a lookup is a membership (with
distinct names, both ways). -/
private theorem defnMap_get?_mem {defns : List Defn} {f : String} {d : Defn}
    (h : (HashMap.ofList (defns.map fun d => (d.name, d))).get? f = some d) :
    d ∈ defns ∧ d.name = f := by
  have hm := ofList_get?_mem h
  obtain ⟨d', hd', heq⟩ := List.mem_map.mp hm
  injection heq with h1 h2
  subst h2
  exact ⟨hd', h1⟩

private theorem mem_defnMap_get? {defns : List Defn} {d : Defn}
    (hnd : (defns.map (·.name)).Nodup) (hm : d ∈ defns) :
    (HashMap.ofList (defns.map fun d => (d.name, d))).get? d.name = some d := by
  refine mem_ofList_get? ?_ (List.mem_map.mpr ⟨d, hm, rfl⟩)
  rw [List.map_map]
  exact hnd

/-! ## Parameter telescopes -/

/-- Zipping a nodup key list keeps the keys nodup. -/
private theorem zip_keys_nodup {ps : List String} {β : Type} {vs : List β}
    (hnd : ps.Nodup) : ((ps.zip vs).map Prod.fst).Nodup := by
  rw [List.Nodup, List.pairwise_map, List.pairwise_iff_getElem]
  intro i j hi hj hij
  rw [List.length_zip] at hi hj
  have hne := List.pairwise_iff_getElem.mp hnd i j (by omega) (by omega) hij
  rw [List.getElem_zip, List.getElem_zip]
  exact fun heq => hne (by simpa using heq)

/-- The context/valuation pair a definition's body runs under: zipping
distinct parameter names against the declared widths and against
width-disciplined values agrees. -/
private theorem ctxAgree_zip {ps : List String} {ws : List Nat} {vs : List BV}
    (hnd : ps.Nodup) (hlen : vs.length = ws.length)
    (hw : ∀ i (h1 : i < vs.length) (h2 : i < ws.length), vs[i].width = ws[i]) :
    CtxAgree (HashMap.ofList (ps.zip ws)) (HashMap.ofList (ps.zip vs)) := by
  intro x w hx
  obtain ⟨i, hi, heq⟩ := List.getElem_of_mem (ofList_get?_mem hx)
  have hips : i < ps.length := by simp only [List.length_zip] at hi; omega
  have hiws : i < ws.length := by simp only [List.length_zip] at hi; omega
  have hivs : i < vs.length := by omega
  rw [List.getElem_zip] at heq
  injection heq with hxi hwi
  refine ⟨vs[i], ?_, by rw [hw i hivs hiws, hwi]⟩
  have hgz : (ps.zip vs)[i]'(by simp only [List.length_zip]; omega) = (ps[i], vs[i]) :=
    List.getElem_zip
  have hmem : (x, vs[i]) ∈ ps.zip vs := by
    rw [← hxi, ← hgz]
    exact List.getElem_mem _
  exact mem_ofList_get? (zip_keys_nodup hnd) hmem

/-! ## `checkDistinct` and the loop inversions -/

/-- A successful distinctness sweep from a seen-set: the list is nodup
and disjoint from the seen-set. -/
private theorem distinct_loop {what : String} :
    ∀ (xs : List String) (seen : HashSet String) (u : HashSet String),
      (forIn xs seen (fun x seen =>
        if seen.contains x = true then do
          throw s!"duplicate {what}: {x}"
          pure (ForInStep.yield (seen.insert x))
        else pure (ForInStep.yield (seen.insert x))) : Except String (HashSet String)) = .ok u →
      xs.Nodup ∧ ∀ x ∈ xs, seen.contains x = false := by
  intro xs
  induction xs with
  | nil => intro seen u _; exact ⟨List.nodup_nil, by simp⟩
  | cons a as ih =>
      intro seen u h
      rw [List.forIn_cons] at h
      by_cases hc : seen.contains a = true
      · rw [if_pos hc] at h
        exact absurd h (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
      · rw [if_neg hc, except_pure_def, except_bind_ok] at h
        obtain ⟨hnd, hnotin⟩ := ih (seen.insert a) u h
        refine ⟨List.nodup_cons.mpr ⟨?_, hnd⟩, ?_⟩
        · intro hmem
          have hcontra := hnotin a hmem
          rw [HashSet.contains_insert] at hcontra
          simp at hcontra
        · intro x hx
          cases List.mem_cons.mp hx with
          | inl he => subst he; simpa using hc
          | inr hxs =>
              have hcontra := hnotin x hxs
              rw [HashSet.contains_insert] at hcontra
              simp only [Bool.or_eq_false_iff] at hcontra
              exact hcontra.2

/-- A successful `checkDistinct` means a nodup list. -/
private theorem checkDistinct_nodup {what : String} {xs : List String}
    (h : Check.checkDistinct what xs = .ok ()) : xs.Nodup := by
  unfold Check.checkDistinct at h
  obtain ⟨u, hfor, -⟩ := bind_ok h
  exact (distinct_loop xs ∅ u hfor).1

/-- Every iteration of a state-free checking loop succeeded. -/
private theorem forIn_unit_ok {α : Type} {g : α → Except String Unit} :
    ∀ {l : List α} {u : PUnit},
      (forIn l PUnit.unit (fun a _ => do
        let _ ← g a
        pure (ForInStep.yield PUnit.unit)) : Except String PUnit) = .ok u →
      ∀ a ∈ l, g a = .ok () := by
  intro l
  induction l with
  | nil => intro u _ a ha; exact absurd ha (by simp)
  | cons x xs ih =>
      intro u h a ha
      rw [List.forIn_cons] at h
      obtain ⟨s, hs, hrest⟩ := bind_ok h
      obtain ⟨v, hv, hyield⟩ := bind_ok hs
      cases v
      cases List.mem_cons.mp ha with
      | inl he => subst he; exact hv
      | inr has =>
          have hyield' : (Except.ok (ForInStep.yield PUnit.unit) : Except String (ForInStep PUnit)) = .ok s := hyield
          injection hyield' with hyield'
          subst hyield'
          exact ih hrest a has

/-! ## `checkDefn` inversion -/

/-- What a successful `checkDefn` certifies. -/
private theorem checkDefn_ok {env : Check.Env} {d : Defn}
    (h : Check.checkDefn env d = .ok ()) :
    d.params.length = d.sig.params.length ∧ d.params.Nodup ∧
      Check.checkExp env (HashMap.ofList (d.params.zip d.sig.params)) d.body
        = .ok d.sig.result := by
  unfold Check.checkDefn at h
  split at h
  case isTrue =>
    exact absurd h (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
  case isFalse hlen =>
    rw [Decidable.not_not] at hlen
    dsimp only at h
    obtain ⟨u, hdist, h⟩ := bind_ok h
    cases u
    obtain ⟨sz, hsz, h⟩ := bind_ok h
    split at h
    case isTrue => exact absurd h (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
    case isFalse hne =>
      rw [Decidable.not_not] at hne
      exact ⟨hlen, checkDistinct_nodup hdist, by rw [hsz, hne]⟩

/-! ## `Program.check` and `checkExtern` inversions -/

/-- The pieces of a successful `Program.check` that progress consumes. -/
theorem check_ok {p : Program} (h : p.check = .ok ()) :
    (p.externs.map (·.name) ++ p.defns.map (·.name)).Nodup
    ∧ (∀ ex ∈ p.externs, Check.checkExtern (Check.mkEnv p) ex = .ok ())
    ∧ (∀ d ∈ p.defns, Check.checkDefn (Check.mkEnv p) d = .ok ())
    ∧ Check.checkDevice (Check.mkEnv p) p.device = .ok () := by
  unfold Program.check at h
  obtain ⟨u1, hdist, h⟩ := bind_ok h
  cases u1
  obtain ⟨u2, hext, h⟩ := bind_ok h
  obtain ⟨u3, hdefn, h⟩ := bind_ok h
  obtain ⟨u4, hdev, h⟩ := bind_ok h
  cases u4
  exact ⟨checkDistinct_nodup hdist, forIn_unit_ok hext, forIn_unit_ok hdefn, hdev⟩

/-- A model edge of the semantic extern table comes from a modeled
extern declaration. -/
private theorem xenv_get?_mem {p : Program} {x g : String}
    (h : (Sem.xenv p).get? x = some g) :
    ∃ e ∈ p.externs, e.name = x ∧ e.model = some g := by
  unfold Sem.xenv at h
  suffices hgen : ∀ (l : List Extern) (m : HashMap String String),
      ((l.foldl (fun m e =>
        match e.model with
        | some f => m.insert e.name f
        | none => m) m).get? x = some g) →
      (∃ e ∈ l, e.name = x ∧ e.model = some g) ∨ m.get? x = some g by
    rcases hgen p.externs ∅ h with h' | h'
    · exact h'
    · rw [HashMap.get?_eq_getElem?, HashMap.getElem?_empty] at h'
      exact absurd h' (by simp)
  intro l
  induction l with
  | nil => intro m hm; exact .inr hm
  | cons e l ih =>
      intro m hm
      rw [List.foldl_cons] at hm
      cases hmod : e.model with
      | none =>
          rw [hmod] at hm
          rcases ih _ hm with h' | h'
          · obtain ⟨e', he', hp⟩ := h'
            exact .inl ⟨e', List.mem_cons_of_mem _ he', hp⟩
          · exact .inr h'
      | some f =>
          rw [hmod] at hm
          rcases ih _ hm with h' | h'
          · obtain ⟨e', he', hp⟩ := h'
            exact .inl ⟨e', List.mem_cons_of_mem _ he', hp⟩
          · rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert] at h'
            split at h'
            case isTrue heq =>
              injection h' with h'
              exact .inl ⟨e, List.mem_cons_self, by simpa using heq, by rw [hmod, h']⟩
            case isFalse =>
              exact .inr (by rw [HashMap.get?_eq_getElem?]; exact h')

/-- What `checkExtern` certifies about a modeled extern: the model
resolves, with the signature aligned to the ports. -/
private theorem checkExtern_model {env : Check.Env} {ex : Extern} {g : String}
    (h : Check.checkExtern env ex = .ok ()) (hmod : ex.model = some g) :
    ∃ d, env.defns.get? g = some d ∧
      d.sig.params = ex.ins.map Prod.snd ∧
      d.sig.result = Check.externResultSize ex := by
  unfold Check.checkExtern at h
  obtain ⟨u1, hdist, h⟩ := bind_ok h
  cases u1
  obtain ⟨u2, hports, h⟩ := bind_ok h
  cases hkind : ex.kind with
  | seq a b =>
      simp only [hkind, hmod] at h
      split at h <;>
        exact absurd h (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
  | comb =>
      simp only [hkind, hmod] at h
      split at h
      case isTrue =>
        exact absurd h (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
      case isFalse =>
        cases hd : env.defns.get? g with
        | none =>
            rw [hd] at h
            exact absurd h (by simp [throw, throwThe, MonadExceptOf.throw])
        | some d =>
            rw [hd] at h
            dsimp only at h
            split at h
            case isTrue =>
              exact absurd h
                (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
            case isFalse hp =>
              rw [Decidable.not_not] at hp
              split at h
              case isTrue =>
                exact absurd h
                  (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
              case isFalse hr =>
                rw [Decidable.not_not] at hr
                exact ⟨d, rfl, hp, hr⟩

/-- A checked program's extern table is model-coherent. -/
theorem check_xModelsOk {p : Program}
    (hnd : (p.externs.map (·.name) ++ p.defns.map (·.name)).Nodup)
    (hext : ∀ ex ∈ p.externs, Check.checkExtern (Check.mkEnv p) ex = .ok ()) :
    XModelsOk (Check.mkEnv p) (Sem.xenv p) := by
  intro x g hx
  obtain ⟨e, he, hname, hmod⟩ := xenv_get?_mem hx
  have hnde : (p.externs.map (·.name)).Nodup := (List.nodup_append.mp hnd).1
  have hgete : (Check.mkEnv p).externs.get? x = some e := by
    subst hname
    refine mem_ofList_get? ?_ (List.mem_map.mpr ⟨e, he, rfl⟩)
    rw [List.map_map]
    exact hnde
  obtain ⟨d, hd, hp, hr⟩ := checkExtern_model (hext e he) hmod
  exact ⟨e, hgete, d, hd, hp, hr⟩

/-! ## Dependencies of checked expressions exist -/

/-- Every dependency of a checked expression names a definition: calls
by the checker's own lookup, extern models by table coherence. -/
private theorem checkExp_deps {env : Check.Env} {X : Sem.XEnv}
    (hxm : XModelsOk env X) :
    ∀ (e : Exp) (ctx : Check.Ctx) (w : Nat),
      Check.checkExp env ctx e = .ok w →
      ∀ g ∈ Sem.deps X e, (env.defns.get? g).isSome = true
  | .lit _, _, _, _ => by intro g hg; exact absurd hg (by simp [Sem.deps])
  | .undef _, _, _, _ => by intro g hg; exact absurd hg (by simp [Sem.deps])
  | .var _ _, _, _, _ => by intro g hg; exact absurd hg (by simp [Sem.deps])
  | .cat e₁ e₂, ctx, w, hck => by
      simp only [Check.checkExp] at hck
      obtain ⟨s₁, h₁, hck⟩ := bind_ok hck
      obtain ⟨s₂, h₂, _⟩ := bind_ok hck
      intro g hg
      rcases List.mem_append.mp (by simpa [Sem.deps] using hg) with hg' | hg'
      · exact checkExp_deps hxm e₁ ctx s₁ h₁ g hg'
      · exact checkExp_deps hxm e₂ ctx s₂ h₂ g hg'
  | .slice _ _ e, ctx, w, hck => by
      simp only [Check.checkExp] at hck
      obtain ⟨s, hs, _⟩ := bind_ok hck
      intro g hg
      exact checkExp_deps hxm e ctx s hs g (by simpa [Sem.deps] using hg)
  | .prim _ op args, ctx, w, hck => by
      simp only [Check.checkExp] at hck
      rw [mapM_attach_erase] at hck
      obtain ⟨szs, hszs, _⟩ := bind_ok hck
      intro g hg
      simp only [Sem.deps, List.mem_flatMap] at hg
      obtain ⟨⟨a, ha⟩, _, hg⟩ := hg
      obtain ⟨sa, hsa⟩ := mapM_ok_of_mem hszs a ha
      exact checkExp_deps hxm a ctx sa hsa g hg
  | .call _ f args, ctx, w, hck => by
      simp only [Check.checkExp] at hck
      cases hd : env.defns.get? f with
      | none => rw [hd] at hck; exact absurd hck (by simp)
      | some d =>
          rw [hd] at hck
          dsimp only at hck
          rw [mapM_attach_erase] at hck
          obtain ⟨szs, hszs, _⟩ := bind_ok hck
          intro g hg
          simp only [Sem.deps, List.mem_cons, List.mem_flatMap] at hg
          rcases hg with hg | ⟨⟨a, ha⟩, _, hg⟩
          · subst hg; rw [hd]; rfl
          · obtain ⟨sa, hsa⟩ := mapM_ok_of_mem hszs a ha
            exact checkExp_deps hxm a ctx sa hsa g hg
  | .xcall _ x cs args, ctx, w, hck => by
      simp only [Check.checkExp] at hck
      cases hex : env.externs.get? x with
      | none => rw [hex] at hck; exact absurd hck (by simp)
      | some ex =>
          rw [hex] at hck
          dsimp only at hck
          split at hck
          case isTrue =>
            exact absurd hck
              (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
          case isFalse =>
            split at hck
            case isTrue =>
              exact absurd hck
                (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
            case isFalse =>
              rw [mapM_attach_erase] at hck
              obtain ⟨szs, hszs, _⟩ := bind_ok hck
              intro g hg
              simp only [Sem.deps, List.mem_append, List.mem_flatMap] at hg
              rcases hg with hg | ⟨⟨a, ha⟩, _, hg⟩
              · cases hX : X.get? x with
                | none => rw [hX] at hg; exact absurd hg (by simp)
                | some model =>
                    rw [hX] at hg
                    obtain ⟨_, _, d, hd, _, _⟩ := hxm x model hX
                    have hgm : g = model := by simpa using hg
                    subst hgm
                    rw [hd]; rfl
              · obtain ⟨sa, hsa⟩ := mapM_ok_of_mem hszs a ha
                exact checkExp_deps hxm a ctx sa hsa g hg
  | .ite _ c t e, ctx, w, hck => by
      simp only [Check.checkExp] at hck
      obtain ⟨sc, hsc, hck⟩ := bind_ok hck
      split at hck
      case isTrue =>
        exact absurd hck (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
      case isFalse =>
        obtain ⟨st, hst, hck⟩ := bind_ok hck
        obtain ⟨se, hse, _⟩ := bind_ok hck
        intro g hg
        simp only [Sem.deps, List.mem_append] at hg
        rcases hg with (hg | hg) | hg
        · exact checkExp_deps hxm c ctx sc hsc g hg
        · exact checkExp_deps hxm t ctx st hst g hg
        · exact checkExp_deps hxm e ctx se hse g hg
  | .letE _ x rhs body, ctx, w, hck => by
      simp only [Check.checkExp] at hck
      obtain ⟨s₁, h₁, hck⟩ := bind_ok hck
      obtain ⟨s₂, h₂, _⟩ := bind_ok hck
      intro g hg
      simp only [Sem.deps, List.mem_append] at hg
      rcases hg with hg | hg
      · exact checkExp_deps hxm rhs ctx s₁ h₁ g hg
      · exact checkExp_deps hxm body (ctx.insert x s₁) s₂ h₂ g hg
  termination_by e => sizeOf e
  decreasing_by all_goals
    (first
      | (have := List.sizeOf_lt_of_mem ha; simp +arith; omega)
      | (simp +arith; omega)
      | simp +arith)

/-! ## The topological order and closure totality -/

/-- `topoDefns.go` only reorders: everything in the output was in the
accumulator or the remainder. -/
private theorem topo_go_sub {X : Sem.XEnv} :
    ∀ (fuel : Nat) (acc rem out : List Defn),
      Sem.topoDefns.go X fuel acc rem = .ok out →
      ∀ d ∈ out, d ∈ acc ∨ d ∈ rem := by
  intro fuel
  induction fuel with
  | zero =>
      intro acc rem out h d hd
      cases rem with
      | nil =>
          simp only [Sem.topoDefns.go] at h
          injection h with h
          subst h
          exact .inl (List.mem_reverse.mp hd)
      | cons r rs => exact absurd h (by simp [Sem.topoDefns.go])
  | succ fuel ih =>
      intro acc rem out h d hd
      cases rem with
      | nil =>
          simp only [Sem.topoDefns.go] at h
          injection h with h
          subst h
          exact .inl (List.mem_reverse.mp hd)
      | cons r rs =>
          rw [show Sem.topoDefns.go X (fuel + 1) acc (r :: rs)
              = (match (r :: rs).partition fun d =>
                   (Sem.deps X d.body).all fun f =>
                     (acc.map (·.name)).contains f || (r :: rs).all (·.name ≠ f) with
                 | (ready, rest) =>
                   if ready.isEmpty then
                     .error s!"recursion among definitions: {String.intercalate ", " ((r :: rs).map (·.name))}"
                   else
                     Sem.topoDefns.go X fuel (ready.reverse ++ acc) rest) from rfl] at h
          rw [List.partition_eq_filter_filter] at h
          dsimp only at h
          split at h
          · exact absurd h (by simp)
          · rcases ih _ _ _ h d hd with hin | hin
            · rcases List.mem_append.mp hin with h' | h'
              · exact .inr (List.mem_filter.mp (List.mem_reverse.mp h')).1
              · exact .inl h'
            · exact .inr (List.mem_filter.mp hin).1

/-- A successful `topoDefns` output draws from the input. -/
private theorem topoDefns_sub {X : Sem.XEnv} {defns ordered : List Defn}
    (h : Sem.topoDefns X defns = .ok ordered) : ∀ d ∈ ordered, d ∈ defns := by
  intro d hd
  rcases topo_go_sub defns.length [] defns ordered h d hd with h' | h'
  · exact absurd h' (by simp)
  · exact h'

/-- A successful `mkFEnv` came through a successful topological sort. -/
private theorem mkFEnv_ordered {p : Program} {E : Sem.EEnv} {F : Sem.FEnv}
    (hF : Sem.mkFEnv p E = .ok F) :
    ∃ ordered, Sem.topoDefns (Sem.xenv p) p.defns = .ok ordered := by
  unfold Sem.mkFEnv at hF
  obtain ⟨ordered, hord, -⟩ := bind_ok hF
  exact ⟨ordered, hord⟩

/-- A split view of `TopoList`: at any position, the dependencies
avoid the names at and after it. -/
private theorem topoList_split {X : Sem.XEnv} :
    ∀ {l : List Defn}, Bridge.TopoList X l →
      ∀ pre d suf, l = pre ++ d :: suf →
        ∀ g ∈ Sem.deps X d.body, g ∉ (d :: suf).map (·.name) := by
  intro l
  induction l with
  | nil =>
      intro _ pre d suf heq
      exact absurd heq (by cases pre <;> simp)
  | cons a l ih =>
      intro h pre d suf heq
      obtain ⟨ha, hl⟩ := h
      cases pre with
      | nil =>
          injection heq with h1 h2
          subst h1; subst h2
          exact ha
      | cons p pre' =>
          injection heq with h1 h2
          exact ih hl pre' d suf h2

/-- The per-program sweep, unpacked. -/
private theorem etaGenericFree_unpack {p : Program}
    (h : p.etaGenericFree = true) :
    (∀ d ∈ p.defns, d.body.etaGenericFree (Sem.xenv p) = true)
    ∧ (∀ s ∈ p.device.body, s.etaGenericFree (Sem.xenv p) = true) := by
  unfold Program.etaGenericFree at h
  simp only [Bool.and_eq_true, List.all_eq_true] at h
  exact h

/-- The induction along the topological order: walking the ordered
list left to right, every processed definition's closure is total —
its dependencies land strictly earlier, where totality is already
established. -/
private theorem chain_closOk {p : Program} {E : Sem.EEnv} {F : Sem.FEnv}
    {ordered : List Defn}
    (himpl : Bridge.FImplements (Bridge.dmapOf p) (Sem.xenv p) F E)
    (hnd : (p.defns.map (·.name)).Nodup)
    (hdefok : ∀ d ∈ p.defns, Check.checkDefn (Check.mkEnv p) d = .ok ())
    (hgf : ∀ d ∈ p.defns, d.body.etaGenericFree (Sem.xenv p) = true)
    (hxm : XModelsOk (Check.mkEnv p) (Sem.xenv p))
    (htopo : Bridge.TopoList (Sem.xenv p) ordered)
    (hsub : ∀ d ∈ ordered, d ∈ p.defns)
    (hper : ∀ d ∈ p.defns, d ∈ ordered) :
    ∀ (suf pre : List Defn), ordered = pre ++ suf →
      (∀ d ∈ pre, ClosOk (Check.mkEnv p) F d.name) →
      ∀ d ∈ suf, ClosOk (Check.mkEnv p) F d.name := by
  intro suf
  induction suf with
  | nil => intro pre _ _ d hd; exact absurd hd (by simp)
  | cons d rest ih =>
      intro pre hsplit hpre d' hd'
      have hdp : d ∈ p.defns := hsub d (by rw [hsplit]; simp)
      have hget : (Check.mkEnv p).defns.get? d.name = some d := mem_defnMap_get? hnd hdp
      have hdOk : ClosOk (Check.mkEnv p) F d.name := by
        obtain ⟨hplen, hpnodup, hbody⟩ := checkDefn_ok (hdefok d hdp)
        obtain ⟨fn, hfn, hfneq⟩ := himpl d.name d hget
        refine ⟨d, hget, fn, hfn, ?_⟩
        intro vs hvslen hvsw
        have hdeps : ∀ g ∈ Sem.deps (Sem.xenv p) d.body, ClosOk (Check.mkEnv p) F g := by
          intro g hg
          have hnotafter := topoList_split htopo pre d rest hsplit g hg
          have hex := checkExp_deps hxm d.body _ _ hbody g hg
          obtain ⟨dg, hdgget⟩ := Option.isSome_iff_exists.mp hex
          obtain ⟨hdgmem, hdgname⟩ := defnMap_get?_mem hdgget
          have hdgord : dg ∈ ordered := hper dg hdgmem
          rw [hsplit] at hdgord
          rcases List.mem_append.mp hdgord with hin | hin
          · have hdOk' := hpre dg hin
            rw [hdgname] at hdOk'
            exact hdOk'
          · exact absurd (List.mem_map.mpr ⟨dg, hin, hdgname⟩) hnotafter
        obtain ⟨v, hv, hvw⟩ := evalExp_progress (E := E) hxm d.body _ _ _ hbody
          (ctxAgree_zip hpnodup (by omega) hvsw) hdeps (hgf d hdp)
        refine ⟨v, ?_, hvw⟩
        rw [hfneq vs, Bridge.mkFn, if_pos (by omega : vs.length = d.params.length)]
        exact hv
      cases List.mem_cons.mp hd' with
      | inl he => subst he; exact hdOk
      | inr hrest =>
          exact ih (pre ++ [d]) (by rw [hsplit, List.append_assoc]; rfl)
            (by
              intro e he
              rcases List.mem_append.mp he with he' | he'
              · exact hpre e he'
              · have : e = d := by simpa using he'
                subst this
                exact hdOk)
            d' hrest

/-- On a checked program with a denoting definition environment, every
definition's closure is total at its signature widths (given
eta-generic-free bodies). -/
theorem mkFEnv_closOk {p : Program} {E : Sem.EEnv} {F : Sem.FEnv}
    (hnd : (p.defns.map (·.name)).Nodup)
    (hdefok : ∀ d ∈ p.defns, Check.checkDefn (Check.mkEnv p) d = .ok ())
    (hgf : ∀ d ∈ p.defns, d.body.etaGenericFree (Sem.xenv p) = true)
    (hxm : XModelsOk (Check.mkEnv p) (Sem.xenv p))
    (hF : Sem.mkFEnv p E = .ok F) :
    ∀ f, ((Check.mkEnv p).defns.get? f).isSome = true → ClosOk (Check.mkEnv p) F f := by
  have himpl := Bridge.mkFEnv_implements hnd hF
  obtain ⟨ordered, hord⟩ := mkFEnv_ordered hF
  obtain ⟨htopo, hndord, hper⟩ := Bridge.topoDefns_spec hord hnd
  have hsub := topoDefns_sub hord
  intro f hf
  obtain ⟨d, hget⟩ := Option.isSome_iff_exists.mp hf
  obtain ⟨hdmem, hdname⟩ := defnMap_get?_mem hget
  have hdord : d ∈ ordered := hper d hdmem
  have := chain_closOk himpl hnd hdefok hgf hxm htopo hsub hper ordered [] rfl
    (by intro e he; exact absurd he (by simp)) d hdord
  rw [← hdname]
  exact this

/-! ## Device-step progress -/

/-- Register-store discipline: every declared register present at its
declared width, and nothing else. -/
def RegsOk (dev : Device) (regs : HashMap String BV) : Prop :=
  (∀ reg ∈ dev.registers, ∃ v, regs.get? reg.name = some v ∧ v.width = reg.width)
  ∧ (∀ x v, regs.get? x = some v → ∃ reg ∈ dev.registers, reg.name = x)

/-- Total closures for every dependency of a checked expression. -/
private theorem closOk_deps {env : Check.Env} {X : Sem.XEnv} {F : Sem.FEnv}
    (hxm : XModelsOk env X)
    (hclos : ∀ f, (env.defns.get? f).isSome = true → ClosOk env F f)
    {ctx : Check.Ctx} {e : Exp} {w : Nat} (hck : Check.checkExp env ctx e = .ok w) :
    ∀ f ∈ Sem.deps X e, ClosOk env F f :=
  fun f hf => hclos f (checkExp_deps hxm e ctx w hck f hf)

/-- `Sem.step`'s fold body, named (transcribed from the definition;
the link is `step_unfold` below). -/
private def concBody (F : Sem.FEnv) (X : Sem.XEnv) (E : Sem.EEnv) :
    HashMap String BV × HashMap String BV × HashMap String BV → Stmt →
    Except String (HashMap String BV × HashMap String BV × HashMap String BV) :=
  fun (ρ, outs, nexts) stmt => do
    match stmt with
    | .sLet x e => do
        let v ← evalExp F X ρ e E
        pure (ρ.insert x v, outs, nexts)
    | .sOutput o e => do
        if outs.contains o then .error s!"output {o} assigned twice"
        let v ← evalExp F X ρ e E
        pure (ρ, outs.insert o v, nexts)
    | .sNext r e => do
        if nexts.contains r then .error s!"register {r} assigned twice"
        let v ← evalExp F X ρ e E
        pure (ρ, outs, nexts.insert r v)
    | .sInstIn inst _ _ => .error s!"device instance {inst}: outside the instance-free fragment"

/-- `Sem.step`'s output/next read-off, named. -/
private def concFinish (dev : Device) :
    HashMap String BV × HashMap String BV × HashMap String BV →
    Except String (List BV × HashMap String BV) :=
  fun (_, outs, nexts) => do
    let outVals ← dev.outputs.mapM fun (o, _) =>
      match outs.get? o with
      | some v => pure v
      | none   => .error s!"output {o} never assigned"
    let regVals ← dev.registers.foldlM (init := (∅ : HashMap String BV)) fun m r =>
      match nexts.get? r.name with
      | some v => pure (m.insert r.name v)
      | none   => .error s!"register {r.name} never assigned"
    pure (outVals, regVals)

/-- The committed `Sem.step`, as the composition of the named pieces. -/
private theorem step_unfold (F : Sem.FEnv) (X : Sem.XEnv) (E : Sem.EEnv) (dev : Device)
    (regs : HashMap String BV) (ins : List BV) :
    Sem.step F X dev regs ins E =
      if ins.length ≠ dev.inputs.length then
        .error s!"stimulus arity: got {ins.length} inputs, device has {dev.inputs.length}"
      else
        dev.body.foldlM (concBody F X E) (Bridge.stepEnv dev.inputs regs ins, ∅, ∅)
          >>= concFinish dev := by
  by_cases h : ins.length = dev.inputs.length
  · rw [if_neg (fun hne => hne h)]
    simp only [Sem.step, if_neg (fun hne : ins.length ≠ dev.inputs.length => hne h)]
    rfl
  · rw [if_pos h]
    simp only [Sem.step, if_pos (h : ins.length ≠ dev.inputs.length)]
    rfl

/-- Component/constructor behavior of the derived `BEq` on assignment
keys. -/
private theorem beq_output_output {a b : String} :
    (Check.AssignKey.output a == Check.AssignKey.output b) = (a == b) := by
  by_cases h : a = b
  · subst h; simp
  · have h1 : (Check.AssignKey.output a == Check.AssignKey.output b) = false := by
      rw [beq_eq_false_iff_ne]
      intro hc; injection hc with hc; exact h hc
    have h2 : (a == b) = false := by rw [beq_eq_false_iff_ne]; exact h
    rw [h1, h2]

private theorem beq_next_next {a b : String} :
    (Check.AssignKey.next a == Check.AssignKey.next b) = (a == b) := by
  by_cases h : a = b
  · subst h; simp
  · have h1 : (Check.AssignKey.next a == Check.AssignKey.next b) = false := by
      rw [beq_eq_false_iff_ne]
      intro hc; injection hc with hc; exact h hc
    have h2 : (a == b) = false := by rw [beq_eq_false_iff_ne]; exact h
    rw [h1, h2]

private theorem beq_output_next {a b : String} :
    (Check.AssignKey.output a == Check.AssignKey.next b) = false := by
  rw [beq_eq_false_iff_ne]
  intro hc
  exact Check.AssignKey.noConfusion hc

private theorem beq_next_output {a b : String} :
    (Check.AssignKey.next a == Check.AssignKey.output b) = false := by
  rw [beq_eq_false_iff_ne]
  intro hc
  exact Check.AssignKey.noConfusion hc

/-- The joint fold invariant: the checker's context/coverage state
mirrors the runtime environment and assignment maps. -/
private structure StInv (env : Check.Env) (outsCtx : Check.Ctx) (dev : Device)
    (ctx : Check.Ctx) (asg : HashSet Check.AssignKey)
    (ρ outs nexts : HashMap String BV) : Prop where
  agree : CtxAgree ctx ρ
  outsC : ∀ o, asg.contains (.output o) = outs.contains o
  outsW : ∀ o v, outs.get? o = some v → ∃ w, outsCtx.get? o = some w ∧ v.width = w
  nextsC : ∀ r, asg.contains (.next r) = nexts.contains r
  nextsW : ∀ r v, nexts.get? r = some v →
    ∃ reg, reg ∈ dev.registers ∧ reg.name = r ∧ v.width = reg.width

/-- What one successful iteration of the checker's device-body loop
certifies, per statement shape. Discharged at the use site by
reducing the concrete elaborated loop body. -/
private def CkStepSpec (env : Check.Env) (outsCtx : Check.Ctx) (dev : Device)
    (Φ : Stmt → Check.Ctx × HashSet Check.AssignKey →
      Except String (ForInStep (Check.Ctx × HashSet Check.AssignKey))) : Prop :=
  ∀ stmt (ctx : Check.Ctx) (asg : HashSet Check.AssignKey) s',
    Φ stmt (ctx, asg) = .ok s' →
    match stmt with
    | .sLet x e => ∃ sz, Check.checkExp env ctx e = .ok sz
        ∧ s' = .yield (ctx.insert x sz, asg)
    | .sOutput o e => ∃ sz, outsCtx.get? o = some sz
        ∧ asg.contains (.output o) = false
        ∧ Check.checkExp env ctx e = .ok sz
        ∧ s' = .yield (ctx, asg.insert (.output o))
    | .sNext r e => ∃ reg, reg ∈ dev.registers ∧ reg.name = r
        ∧ asg.contains (.next r) = false
        ∧ Check.checkExp env ctx e = .ok reg.width
        ∧ s' = .yield (ctx, asg.insert (.next r))
    | .sInstIn _ _ _ => False

/-- The parallel fold: a successful checker walk from a mirrored state
gives a successful runtime fold, states mirrored throughout. -/
private theorem fold_progress {env : Check.Env} {X : Sem.XEnv} {F : Sem.FEnv} {E : Sem.EEnv}
    {outsCtx : Check.Ctx} {dev : Device}
    {Φ : Stmt → Check.Ctx × HashSet Check.AssignKey →
      Except String (ForInStep (Check.Ctx × HashSet Check.AssignKey))}
    (hxm : XModelsOk env X)
    (hclos : ∀ f, (env.defns.get? f).isSome = true → ClosOk env F f)
    (hΦ : CkStepSpec env outsCtx dev Φ) :
    ∀ (stmts : List Stmt) (ctx : Check.Ctx) (asg : HashSet Check.AssignKey)
      (ρ outs nexts : HashMap String BV) (sB : Check.Ctx × HashSet Check.AssignKey),
      forIn stmts (ctx, asg) Φ = .ok sB →
      (∀ s ∈ stmts, s.etaGenericFree X = true) →
      StInv env outsCtx dev ctx asg ρ outs nexts →
      ∃ ρ' outs' nexts',
        stmts.foldlM (concBody F X E) (ρ, outs, nexts) = .ok (ρ', outs', nexts') ∧
        StInv env outsCtx dev sB.1 sB.2 ρ' outs' nexts' := by
  intro stmts
  induction stmts with
  | nil =>
      intro ctx asg ρ outs nexts sB hfor _ hInv
      rw [List.forIn_nil] at hfor
      injection hfor with hfor
      subst hfor
      exact ⟨ρ, outs, nexts, rfl, hInv⟩
  | cons stmt rest ih =>
      intro ctx asg ρ outs nexts sB hfor hgf hInv
      rw [List.forIn_cons] at hfor
      obtain ⟨s', hstep, hcont⟩ := bind_ok hfor
      have hspec := hΦ stmt ctx asg s' hstep
      have hgfs := hgf stmt List.mem_cons_self
      have hgfr : ∀ s ∈ rest, s.etaGenericFree X = true :=
        fun s hs => hgf s (List.mem_cons_of_mem _ hs)
      cases stmt with
      | sLet x e =>
          obtain ⟨sz, hck, hs'⟩ := hspec
          subst hs'
          dsimp only at hcont
          obtain ⟨v, hv, hvw⟩ := evalExp_progress (E := E) hxm e ctx ρ sz hck hInv.agree
            (closOk_deps hxm hclos hck) hgfs
          obtain ⟨ρ', outs', nexts', hfold, hInv'⟩ := ih (ctx.insert x sz) asg
            (ρ.insert x v) outs nexts sB hcont hgfr
            ⟨ctxAgree_insert hInv.agree hvw, hInv.outsC, hInv.outsW, hInv.nextsC, hInv.nextsW⟩
          refine ⟨ρ', outs', nexts', ?_, hInv'⟩
          rw [List.foldlM_cons]
          show (concBody F X E (ρ, outs, nexts) (.sLet x e) >>= _) = _
          rw [show concBody F X E (ρ, outs, nexts) (.sLet x e)
              = (evalExp F X ρ e E >>= fun v => pure (ρ.insert x v, outs, nexts)) from rfl]
          rw [hv, except_bind_ok, except_pure_def, except_bind_ok]
          exact hfold
      | sOutput o e =>
          obtain ⟨sz, hoc, hnc, hck, hs'⟩ := hspec
          subst hs'
          dsimp only at hcont
          obtain ⟨v, hv, hvw⟩ := evalExp_progress (E := E) hxm e ctx ρ sz hck hInv.agree
            (closOk_deps hxm hclos hck) hgfs
          have hocont : outs.contains o = false := by rw [← hInv.outsC]; exact hnc
          have hInvN : StInv env outsCtx dev ctx (asg.insert (.output o))
              ρ (outs.insert o v) nexts := by
            refine ⟨hInv.agree, ?_, ?_, ?_, hInv.nextsW⟩
            · intro o'
              rw [HashSet.contains_insert, HashMap.contains_insert, ← hInv.outsC,
                  beq_output_output]
            · intro o' v' hv'
              rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert] at hv'
              split at hv'
              case isTrue heq =>
                injection hv' with hv'
                subst hv'
                have : o = o' := by simpa using heq
                subst this
                exact ⟨sz, hoc, hvw⟩
              case isFalse =>
                exact hInv.outsW o' v' (by rw [HashMap.get?_eq_getElem?]; exact hv')
            · intro r
              rw [HashSet.contains_insert, ← hInv.nextsC, beq_output_next, Bool.false_or]
          obtain ⟨ρ', outs', nexts', hfold, hInv'⟩ := ih ctx (asg.insert (.output o))
            ρ (outs.insert o v) nexts sB hcont hgfr hInvN
          refine ⟨ρ', outs', nexts', ?_, hInv'⟩
          rw [List.foldlM_cons]
          show (concBody F X E (ρ, outs, nexts) (.sOutput o e) >>= _) = _
          rw [show concBody F X E (ρ, outs, nexts) (.sOutput o e)
              = (if outs.contains o then .error s!"output {o} assigned twice"
                 else evalExp F X ρ e E >>= fun v => pure (ρ, outs.insert o v, nexts)) from rfl]
          rw [if_neg (by rw [hocont]; exact Bool.false_ne_true)]
          rw [hv, except_bind_ok, except_pure_def, except_bind_ok]
          exact hfold
      | sNext r e =>
          obtain ⟨reg, hregm, hregn, hnc, hck, hs'⟩ := hspec
          subst hs'
          dsimp only at hcont
          obtain ⟨v, hv, hvw⟩ := evalExp_progress (E := E) hxm e ctx ρ reg.width hck hInv.agree
            (closOk_deps hxm hclos hck) hgfs
          have hncont : nexts.contains r = false := by rw [← hInv.nextsC]; exact hnc
          have hInvN : StInv env outsCtx dev ctx (asg.insert (.next r))
              ρ outs (nexts.insert r v) := by
            refine ⟨hInv.agree, ?_, hInv.outsW, ?_, ?_⟩
            · intro o'
              rw [HashSet.contains_insert, ← hInv.outsC, beq_next_output, Bool.false_or]
            · intro r'
              rw [HashSet.contains_insert, HashMap.contains_insert, ← hInv.nextsC,
                  beq_next_next]
            · intro r' v' hv'
              rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert] at hv'
              split at hv'
              case isTrue heq =>
                injection hv' with hv'
                subst hv'
                have : r = r' := by simpa using heq
                subst this
                exact ⟨reg, hregm, hregn, hvw⟩
              case isFalse =>
                exact hInv.nextsW r' v' (by rw [HashMap.get?_eq_getElem?]; exact hv')
          obtain ⟨ρ', outs', nexts', hfold, hInv'⟩ := ih ctx (asg.insert (.next r))
            ρ outs (nexts.insert r v) sB hcont hgfr hInvN
          refine ⟨ρ', outs', nexts', ?_, hInv'⟩
          rw [List.foldlM_cons]
          show (concBody F X E (ρ, outs, nexts) (.sNext r e) >>= _) = _
          rw [show concBody F X E (ρ, outs, nexts) (.sNext r e)
              = (if nexts.contains r then .error s!"register {r} assigned twice"
                 else evalExp F X ρ e E >>= fun v => pure (ρ, outs, nexts.insert r v)) from rfl]
          rw [if_neg (by rw [hncont]; exact Bool.false_ne_true)]
          rw [hv, except_bind_ok, except_pure_def, except_bind_ok]
          exact hfold
      | sInstIn i pt e => exact absurd hspec (by simp)

/-- Reading off the outputs succeeds, at the declared widths. -/
private theorem outVals_progress {outs : HashMap String BV} :
    ∀ (l : List (String × Nat)),
      (∀ pr ∈ l, ∃ v, outs.get? pr.1 = some v ∧ v.width = pr.2) →
      ∃ vals, (l.mapM (fun pr =>
          match outs.get? pr.1 with
          | some v => pure v
          | none => .error s!"output {pr.1} never assigned") : Except String (List BV)) = .ok vals
        ∧ vals.length = l.length
        ∧ ∀ i (h1 : i < vals.length) (h2 : i < l.length), vals[i].width = (l[i]).2 := by
  intro l
  induction l with
  | nil => exact fun _ => ⟨[], rfl, rfl, fun i h1 _ => absurd h1 (by simp)⟩
  | cons pr rest ih =>
      intro hpt
      obtain ⟨v, hv, hvw⟩ := hpt pr List.mem_cons_self
      obtain ⟨vals, hvals, hlen, hw⟩ := ih (fun q hq => hpt q (List.mem_cons_of_mem _ hq))
      refine ⟨v :: vals, ?_, by simpa using hlen, ?_⟩
      · simp only [List.mapM_cons, hv, pure_bind]
        rw [hvals, except_bind_ok, except_pure_def]
      · intro i h1 h2
        cases i with
        | zero => simpa using hvw
        | succ i => simpa using hw i (by simpa using h1) (by simpa using h2)

/-- Building the next register store succeeds, disciplined. -/
private theorem regVals_progress {nexts : HashMap String BV} :
    ∀ (l : List Register) (m₀ : HashMap String BV),
      (l.map (·.name)).Nodup →
      (∀ reg ∈ l, ∃ v, nexts.get? reg.name = some v ∧ v.width = reg.width) →
      ∃ m', (l.foldlM (init := m₀) (fun m r =>
          match nexts.get? r.name with
          | some v => pure (m.insert r.name v)
          | none => .error s!"register {r.name} never assigned") : Except String _) = .ok m'
        ∧ (∀ reg ∈ l, ∃ v, m'.get? reg.name = some v ∧ v.width = reg.width)
        ∧ (∀ x v, m'.get? x = some v → m₀.get? x = some v ∨ ∃ reg ∈ l, reg.name = x)
        ∧ (∀ x v', m₀.get? x = some v' → (∀ reg ∈ l, reg.name ≠ x) → m'.get? x = some v') := by
  intro l
  induction l with
  | nil =>
      intro m₀ _ _
      exact ⟨m₀, rfl, by simp, fun x v h => .inl h, fun x v' h _ => h⟩
  | cons r rest ih =>
      intro m₀ hnod hpt
      obtain ⟨v, hv, hvw⟩ := hpt r List.mem_cons_self
      have hnod' : (rest.map (·.name)).Nodup := by
        rw [List.map_cons, List.nodup_cons] at hnod
        exact hnod.2
      have hnotin : r.name ∉ rest.map (·.name) := by
        rw [List.map_cons, List.nodup_cons] at hnod
        exact hnod.1
      obtain ⟨m', hfold, hall, hdom, hpres⟩ := ih (m₀.insert r.name v) hnod'
        (fun q hq => hpt q (List.mem_cons_of_mem _ hq))
      refine ⟨m', ?_, ?_, ?_, ?_⟩
      · simp only [List.foldlM_cons, hv, pure_bind]
        exact hfold
      · intro reg hreg
        cases List.mem_cons.mp hreg with
        | inl he =>
            subst he
            refine ⟨v, ?_, hvw⟩
            refine hpres reg.name v ?_ ?_
            · rw [HashMap.get?_eq_getElem?]
              exact HashMap.getElem?_insert_self
            · intro q hq hqn
              exact hnotin (hqn ▸ List.mem_map.mpr ⟨q, hq, rfl⟩)
        | inr hrest => exact hall reg hrest
      · intro x vx hx
        rcases hdom x vx hx with hin | hin
        · rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert] at hin
          split at hin
          case isTrue heq =>
            exact .inr ⟨r, List.mem_cons_self, by simpa using heq⟩
          case isFalse =>
            exact .inl (by rw [HashMap.get?_eq_getElem?]; exact hin)
        · obtain ⟨reg, hreg, hname⟩ := hin
          exact .inr ⟨reg, List.mem_cons_of_mem _ hreg, hname⟩
      · intro x v' hx hne
        refine hpres x v' ?_ (fun q hq => hne q (List.mem_cons_of_mem _ hq))
        rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert,
            if_neg (by
              simp only [beq_iff_eq]
              exact fun hc => hne r List.mem_cons_self hc), ← HashMap.get?_eq_getElem?]
        exact hx

/-- The register-initial width sweep, inverted. -/
private theorem regInit_loop {registers : List Register} {u : PUnit}
    (h : (forIn registers PUnit.unit (fun r __s =>
        if r.init.width ≠ r.width then do
          throw s!"register {r.name}: initial value width {r.init.width} does not match declared width {r.width}"
          pure (ForInStep.yield PUnit.unit)
        else pure (ForInStep.yield PUnit.unit)) : Except String PUnit) = .ok u) :
    ∀ r ∈ registers, r.init.width = r.width := by
  induction registers with
  | nil => intro r hr; exact absurd hr (by simp)
  | cons a as ih =>
      rw [List.forIn_cons] at h
      by_cases ha : a.init.width = a.width
      · rw [if_neg (by simpa using ha)] at h
        simp only [pure_bind] at h
        intro r hr
        cases List.mem_cons.mp hr with
        | inl he => subst he; exact ha
        | inr hrs => exact ih h r hrs
      · rw [if_pos (by simpa using ha)] at h
        exact absurd h (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])

/-- The output-coverage sweep, inverted. -/
private theorem coverOut_loop {outputs : List (String × Nat)}
    {asg : HashSet Check.AssignKey} {u : PUnit}
    (h : (forIn outputs PUnit.unit (fun x __s =>
        match x with
        | (o, _) =>
          if ¬asg.contains (Check.AssignKey.output o) = true then do
            throw s!"output {o} is never assigned"
            pure (ForInStep.yield PUnit.unit)
          else pure (ForInStep.yield PUnit.unit)) : Except String PUnit) = .ok u) :
    ∀ pr ∈ outputs, asg.contains (.output pr.1) = true := by
  induction outputs with
  | nil => intro pr hpr; exact absurd hpr (by simp)
  | cons a as ih =>
      obtain ⟨o, wo⟩ := a
      rw [List.forIn_cons] at h
      dsimp only at h
      by_cases ha : asg.contains (.output o) = true
      · rw [if_neg (by simpa using ha)] at h
        simp only [pure_bind] at h
        intro pr hpr
        cases List.mem_cons.mp hpr with
        | inl he => subst he; exact ha
        | inr hps => exact ih h pr hps
      · rw [if_pos (by simpa using ha)] at h
        exact absurd h (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])

/-- The register-coverage sweep, inverted. -/
private theorem coverReg_loop {registers : List Register}
    {asg : HashSet Check.AssignKey} {u : PUnit}
    (h : (forIn registers PUnit.unit (fun r __s =>
        if ¬asg.contains (Check.AssignKey.next r.name) = true then do
          throw s!"register {r.name} is never assigned"
          pure (ForInStep.yield PUnit.unit)
        else pure (ForInStep.yield PUnit.unit)) : Except String PUnit) = .ok u) :
    ∀ r ∈ registers, asg.contains (.next r.name) = true := by
  induction registers with
  | nil => intro r hr; exact absurd hr (by simp)
  | cons a as ih =>
      rw [List.forIn_cons] at h
      by_cases ha : asg.contains (.next a.name) = true
      · rw [if_neg (by simpa using ha)] at h
        simp only [pure_bind] at h
        intro r hr
        cases List.mem_cons.mp hr with
        | inl he => subst he; exact ha
        | inr hrs => exact ih h r hrs
      · rw [if_pos (by simpa using ha)] at h
        exact absurd h (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])

/-- `getElem?_union` at the bare `HashMap.union` application the
elaborator leaves behind. -/
private theorem getElem?_hUnion {β : Type} {m₁ m₂ : HashMap String β} {k : String} :
    (HashMap.union m₁ m₂)[k]? = m₂[k]?.or m₁[k]? :=
  HashMap.getElem?_union

/-- The initial mirrored state: the checker's ambient context agrees
with the step's starting valuation. -/
private theorem ambient_agree {inputs : List (String × Nat)} {registers : List Register}
    {regs : HashMap String BV} {ins : List BV}
    (hnodin : (inputs.map Prod.fst).Nodup)
    (hdisj : ∀ a ∈ inputs.map Prod.fst, ∀ b ∈ registers.map (·.name), a ≠ b)
    (hregs : (∀ reg ∈ registers, ∃ v, regs.get? reg.name = some v ∧ v.width = reg.width)
             ∧ (∀ x v, regs.get? x = some v → ∃ reg ∈ registers, reg.name = x))
    (hlen : ins.length = inputs.length)
    (hinsw : ∀ i (h1 : i < ins.length) (h2 : i < inputs.length),
      ins[i].width = (inputs[i]).2) :
    CtxAgree ((∅ : Check.Ctx).union ((HashMap.ofList inputs).union
        (HashMap.ofList (registers.map fun r => (r.name, r.width)))))
      (Bridge.stepEnv inputs regs ins) := by
  intro x w hx
  rw [HashMap.get?_eq_getElem?, getElem?_hUnion, getElem?_hUnion] at hx
  rw [show ((∅ : Check.Ctx)[x]? : Option Nat) = none from HashMap.getElem?_empty,
      Option.or_none] at hx
  unfold Bridge.stepEnv
  cases hr : (HashMap.ofList (registers.map fun r => (r.name, r.width)))[x]? with
  | some wr =>
      -- A register name: the register store answers, at its width.
      rw [hr] at hx
      simp only [Option.some_or] at hx
      injection hx with hx
      subst hx
      have hmem := ofList_get?_mem (l := registers.map fun r => (r.name, r.width))
        (by rw [HashMap.get?_eq_getElem?]; exact hr)
      obtain ⟨reg, hreg, heq⟩ := List.mem_map.mp hmem
      injection heq with h1 h2
      obtain ⟨v, hv, hvw⟩ := hregs.1 reg hreg
      have hvx : regs.get? x = some v := h1 ▸ hv
      refine ⟨v, ?_, by rw [hvw, h2]⟩
      rw [HashMap.get?_eq_getElem?, getElem?_hUnion,
          ← HashMap.get?_eq_getElem? (m := regs), hvx]
      rfl
  | none =>
      -- An input name: the zipped stimulus answers; the register store
      -- cannot shadow it (name disjointness).
      rw [hr, Option.none_or] at hx
      have hmem := ofList_get?_mem (by rw [HashMap.get?_eq_getElem?]; exact hx)
      obtain ⟨i, hilen, heq⟩ := List.getElem_of_mem hmem
      have hiins : i < ins.length := by omega
      have hx1 : inputs[i].1 = x := by rw [heq]
      have hw2 : inputs[i].2 = w := by rw [heq]
      have hinm : x ∈ inputs.map Prod.fst :=
        List.mem_map.mpr ⟨inputs[i], List.getElem_mem _, hx1⟩
      have hregnone : regs.get? x = none := by
        cases hrx : regs.get? x with
        | none => rfl
        | some v =>
            obtain ⟨reg, hreg, hname⟩ := hregs.2 x v hrx
            exact absurd hname.symm
              (hdisj x hinm reg.name (List.mem_map.mpr ⟨reg, hreg, rfl⟩))
      refine ⟨ins[i], ?_, by rw [hinsw i hiins hilen, hw2]⟩
      rw [HashMap.get?_eq_getElem?, getElem?_hUnion,
          ← HashMap.get?_eq_getElem? (m := regs), hregnone, Option.none_or,
          ← HashMap.get?_eq_getElem?]
      have hgz : ((inputs.map Prod.fst).zip ins)[i]'(by
          simp only [List.length_zip, List.length_map]; omega) = (x, ins[i]) := by
        rw [List.getElem_zip]
        congr 1
        rw [List.getElem_map]
        exact hx1
      exact mem_ofList_get? (zip_keys_nodup hnodin) (hgz ▸ List.getElem_mem _)

/-- Registers with distinct names are determined by their names. -/
private theorem nodup_names_inj {l : List Register}
    (hnod : (l.map (·.name)).Nodup) {a b : Register}
    (ha : a ∈ l) (hb : b ∈ l) (heq : a.name = b.name) : a = b := by
  obtain ⟨i, hi, hia⟩ := List.getElem_of_mem ha
  obtain ⟨j, hj, hjb⟩ := List.getElem_of_mem hb
  by_cases hij : i = j
  · subst hij; rw [← hia, ← hjb]
  · exfalso
    have hne := List.pairwise_iff_getElem.mp hnod
    rcases Nat.lt_or_ge i j with hlt | hge
    · exact hne i j (by simpa using hi) (by simpa using hj) hlt
        (by rw [List.getElem_map, List.getElem_map, hia, hjb]; exact heq)
    · have hlt : j < i := by omega
      exact hne j i (by simpa using hj) (by simpa using hi) hlt
        (by rw [List.getElem_map, List.getElem_map, hia, hjb]; exact heq.symm)

/-- Reading off a finished step succeeds, disciplined. -/
private theorem finish_progress {env : Check.Env} {dev : Device}
    {ctxF : Check.Ctx} {asgF : HashSet Check.AssignKey}
    {ρ outs nexts : HashMap String BV}
    (hInv : StInv env (HashMap.ofList dev.outputs) dev ctxF asgF ρ outs nexts)
    (hnodOut : (dev.outputs.map Prod.fst).Nodup)
    (hnodReg : (dev.registers.map (·.name)).Nodup)
    (hcovO : ∀ pr ∈ dev.outputs, asgF.contains (.output pr.1) = true)
    (hcovR : ∀ reg ∈ dev.registers, asgF.contains (.next reg.name) = true) :
    ∃ outVals regVals, concFinish dev (ρ, outs, nexts) = .ok (outVals, regVals)
      ∧ outVals.length = dev.outputs.length
      ∧ (∀ i (h1 : i < outVals.length) (h2 : i < dev.outputs.length),
          outVals[i].width = (dev.outputs[i]).2)
      ∧ RegsOk dev regVals := by
  have houts : ∀ pr ∈ dev.outputs, ∃ v, outs.get? pr.1 = some v ∧ v.width = pr.2 := by
    intro pr hpr
    have hc : outs.contains pr.1 = true := by rw [← hInv.outsC]; exact hcovO pr hpr
    rw [HashMap.contains_eq_isSome_getElem?] at hc
    obtain ⟨v, hv⟩ := Option.isSome_iff_exists.mp hc
    obtain ⟨w', hw', hvw⟩ := hInv.outsW pr.1 v (by rw [HashMap.get?_eq_getElem?]; exact hv)
    have hprw : (HashMap.ofList dev.outputs).get? pr.1 = some pr.2 :=
      mem_ofList_get? hnodOut hpr
    rw [hw'] at hprw
    injection hprw with hprw
    exact ⟨v, by rw [HashMap.get?_eq_getElem?]; exact hv, by rw [hvw, hprw]⟩
  have hnexts : ∀ reg ∈ dev.registers,
      ∃ v, nexts.get? reg.name = some v ∧ v.width = reg.width := by
    intro reg hreg
    have hc : nexts.contains reg.name = true := by rw [← hInv.nextsC]; exact hcovR reg hreg
    rw [HashMap.contains_eq_isSome_getElem?] at hc
    obtain ⟨v, hv⟩ := Option.isSome_iff_exists.mp hc
    obtain ⟨reg', hreg', hname', hvw⟩ := hInv.nextsW reg.name v
      (by rw [HashMap.get?_eq_getElem?]; exact hv)
    have : reg' = reg := nodup_names_inj hnodReg hreg' hreg hname'
    subst this
    exact ⟨v, by rw [HashMap.get?_eq_getElem?]; exact hv, hvw⟩
  obtain ⟨outVals, hov, holen, how⟩ := outVals_progress dev.outputs houts
  obtain ⟨regVals, hrv, hrall, hrdom, -⟩ := regVals_progress dev.registers ∅ hnodReg hnexts
  refine ⟨outVals, regVals, ?_, holen, how, hrall, ?_⟩
  · rw [show concFinish dev (ρ, outs, nexts)
        = ((dev.outputs.mapM fun (o, _) =>
            match outs.get? o with
            | some v => pure v
            | none => .error s!"output {o} never assigned") >>= fun outVals =>
          (dev.registers.foldlM (init := (∅ : HashMap String BV)) fun m r =>
            match nexts.get? r.name with
            | some v => pure (m.insert r.name v)
            | none => .error s!"register {r.name} never assigned") >>= fun regVals =>
          pure (outVals, regVals)) from rfl]
    rw [hov, except_bind_ok, hrv, except_bind_ok, except_pure_def]
  · intro x v hx
    rcases hrdom x v hx with h | h
    · rw [HashMap.get?_eq_getElem?, HashMap.getElem?_empty] at h
      exact absurd h (by simp)
    · exact h

/-- A checked, instance-free, eta-generic-free device steps on every
width-disciplined input row and register store, producing
declared-width outputs and a disciplined next store. -/
theorem step_progress {p : Program} {F : Sem.FEnv} {E : Sem.EEnv}
    {regs : HashMap String BV} {ins : List BV}
    (hdev : Check.checkDevice (Check.mkEnv p) p.device = .ok ())
    (hinst : p.device.instances = [])
    (hgf : ∀ s ∈ p.device.body, s.etaGenericFree (Sem.xenv p) = true)
    (hxm : XModelsOk (Check.mkEnv p) (Sem.xenv p))
    (hclos : ∀ f, ((Check.mkEnv p).defns.get? f).isSome = true →
      ClosOk (Check.mkEnv p) F f)
    (hregs : RegsOk p.device regs)
    (hins : ins.length = p.device.inputs.length)
    (hinsw : ∀ i (h1 : i < ins.length) (h2 : i < p.device.inputs.length),
      ins[i].width = (p.device.inputs[i]).2) :
    ∃ outs regs', Sem.step F (Sem.xenv p) p.device regs ins E = .ok (outs, regs')
      ∧ outs.length = p.device.outputs.length
      ∧ (∀ i (h1 : i < outs.length) (h2 : i < p.device.outputs.length),
          outs[i].width = (p.device.outputs[i]).2)
      ∧ RegsOk p.device regs' := by
  unfold Check.checkDevice at hdev
  rw [hinst] at hdev
  dsimp only at hdev
  obtain ⟨u1, hloc, hdev⟩ := bind_ok hdev
  cases u1
  obtain ⟨u2, -, hdev⟩ := bind_ok hdev
  obtain ⟨u3, hrinit, hdev⟩ := bind_ok hdev
  rw [List.forIn_nil] at hdev
  simp only [pure_bind] at hdev
  obtain ⟨sB, hbody, hdev⟩ := bind_ok hdev
  obtain ⟨u4, hcovO', hdev⟩ := bind_ok hdev
  obtain ⟨u5, hcovR', -⟩ := bind_ok hdev
  -- Name discipline from the locals sweep (instance names gone).
  have hnodloc := checkDistinct_nodup hloc
  simp only [List.map_nil, List.append_nil] at hnodloc
  have h1 := List.nodup_append.mp hnodloc
  have h2 := List.nodup_append.mp h1.1
  have h3 := List.nodup_append.mp h2.1
  have hnodin : (p.device.inputs.map Prod.fst).Nodup := h3.1
  have hnodout : (p.device.outputs.map Prod.fst).Nodup := h3.2.1
  have hnodreg : (p.device.registers.map (·.name)).Nodup := h2.2.1
  have hdisj : ∀ a ∈ p.device.inputs.map Prod.fst,
      ∀ b ∈ p.device.registers.map (·.name), a ≠ b := by
    intro a ha b hb
    exact h2.2.2 a (List.mem_append.mpr (.inl ha)) b hb
  -- The initial mirrored state.
  have hInv0 : StInv (Check.mkEnv p) (HashMap.ofList p.device.outputs) p.device
      (HashMap.union ∅ ((HashMap.ofList p.device.inputs).union
        (HashMap.ofList (p.device.registers.map fun r => (r.name, r.width)))))
      ∅ (Bridge.stepEnv p.device.inputs regs ins) ∅ ∅ := by
    refine ⟨ambient_agree hnodin hdisj ⟨hregs.1, hregs.2⟩ hins hinsw, ?_, ?_, ?_, ?_⟩
    · intro o
      rw [HashSet.contains_empty]
      rw [show (∅ : HashMap String BV).contains o = false by
        rw [HashMap.contains_eq_isSome_getElem?, HashMap.getElem?_empty]; rfl]
    · intro o v hv
      rw [HashMap.get?_eq_getElem?, HashMap.getElem?_empty] at hv
      exact absurd hv (by simp)
    · intro r
      rw [HashSet.contains_empty]
      rw [show (∅ : HashMap String BV).contains r = false by
        rw [HashMap.contains_eq_isSome_getElem?, HashMap.getElem?_empty]; rfl]
    · intro r v hv
      rw [HashMap.get?_eq_getElem?, HashMap.getElem?_empty] at hv
      exact absurd hv (by simp)
  -- The parallel fold, with the per-statement spec discharged against
  -- the concrete elaborated loop body.
  obtain ⟨ρ', outs', nexts', hfold, hInvF⟩ :=
    fold_progress (E := E) hxm hclos
      (by
        intro stmt ctx asg s' hstep
        cases stmt with
        | sLet x e =>
            dsimp only at hstep
            obtain ⟨sz, hck, hstep⟩ := bind_ok hstep
            injection hstep with hstep
            exact ⟨sz, hck, hstep.symm⟩
        | sOutput o e =>
            dsimp only at hstep
            cases ho : (HashMap.ofList p.device.outputs).get? o with
            | none =>
                rw [ho] at hstep
                exact absurd hstep
                  (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
            | some sz =>
                rw [ho] at hstep
                dsimp only at hstep
                obtain ⟨asg', hao, hstep⟩ := bind_ok hstep
                rw [except_pure_def] at hstep
                injection hstep with hstep
                split at hao
                case isTrue =>
                  exact absurd hao
                    (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
                case isFalse hnc =>
                  obtain ⟨sz', hck, hao⟩ := bind_ok hao
                  split at hao
                  case isTrue =>
                    exact absurd hao
                      (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
                  case isFalse hne =>
                    rw [Decidable.not_not] at hne
                    rw [except_pure_def] at hao
                    injection hao with hao
                    subst hao
                    exact ⟨sz, ho, by simpa using hnc, by rw [hck, hne], hstep.symm⟩
        | sNext r e =>
            dsimp only at hstep
            cases hf : List.find? (fun x => decide (x.name = r)) p.device.registers with
            | none =>
                rw [hf] at hstep
                exact absurd hstep
                  (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
            | some reg =>
                rw [hf] at hstep
                dsimp only at hstep
                obtain ⟨asg', hao, hstep⟩ := bind_ok hstep
                rw [except_pure_def] at hstep
                injection hstep with hstep
                split at hao
                case isTrue =>
                  exact absurd hao
                    (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
                case isFalse hnc =>
                  obtain ⟨sz', hck, hao⟩ := bind_ok hao
                  split at hao
                  case isTrue =>
                    exact absurd hao
                      (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind])
                  case isFalse hne =>
                    rw [Decidable.not_not] at hne
                    rw [except_pure_def] at hao
                    injection hao with hao
                    subst hao
                    refine ⟨reg, List.mem_of_find?_eq_some hf, ?_, by simpa using hnc,
                      by rw [hck, hne], hstep.symm⟩
                    have := List.find?_some hf
                    simpa using this
        | sInstIn i pt e =>
            dsimp only at hstep
            rw [List.find?_nil] at hstep
            exact absurd hstep
              (by simp [throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind]))
      p.device.body _ _ _ _ _ sB hbody hgf hInv0
  -- Coverage, and the read-off.
  have hcovO := coverOut_loop hcovO'
  have hcovR := coverReg_loop hcovR'
  obtain ⟨outVals, regVals, hfin, holen, how, hrOk⟩ :=
    finish_progress hInvF hnodout hnodreg
      (fun pr hpr => by rw [show sB.2 = sB.snd from rfl] at hcovO ⊢; exact hcovO pr hpr)
      (fun reg hreg => hcovR reg hreg)
  refine ⟨outVals, regVals, ?_, holen, how, hrOk⟩
  rw [step_unfold, if_neg (by omega)]
  rw [hfold, except_bind_ok]
  exact hfin

/-- The register-initial facts of a checked device. -/
private theorem checkDevice_inits {p : Program}
    (hdev : Check.checkDevice (Check.mkEnv p) p.device = .ok ())
    (hinst : p.device.instances = []) :
    (∀ r ∈ p.device.registers, r.init.width = r.width)
    ∧ (p.device.registers.map (·.name)).Nodup := by
  unfold Check.checkDevice at hdev
  rw [hinst] at hdev
  dsimp only at hdev
  obtain ⟨u1, hloc, hdev⟩ := bind_ok hdev
  cases u1
  obtain ⟨u2, -, hdev⟩ := bind_ok hdev
  obtain ⟨u3, hrinit, -⟩ := bind_ok hdev
  have hnodloc := checkDistinct_nodup hloc
  simp only [List.map_nil, List.append_nil] at hnodloc
  have h1 := List.nodup_append.mp hnodloc
  have h2 := List.nodup_append.mp h1.1
  exact ⟨regInit_loop hrinit, h2.2.1⟩

/-- The declared initials form a disciplined register store. -/
private theorem initRegs_regsOk {dev : Device}
    (hnodreg : (dev.registers.map (·.name)).Nodup)
    (hinits : ∀ r ∈ dev.registers, r.init.width = r.width) :
    RegsOk dev (Sem.initRegs dev) := by
  constructor
  · intro reg hreg
    refine ⟨reg.init, ?_, hinits reg hreg⟩
    refine mem_ofList_get? ?_ (List.mem_map.mpr ⟨reg, hreg, rfl⟩)
    rw [List.map_map]
    exact hnodreg
  · intro x v hx
    obtain ⟨reg, hreg, heq⟩ := List.mem_map.mp (ofList_get?_mem hx)
    injection heq with h1 h2
    exact ⟨reg, hreg, h1⟩

/-- A checked, instance-free, eta-generic-free device runs on every
width-disciplined stimulus, producing declared-width outputs cycle
for cycle. -/
theorem runLoop_progress {p : Program} {F : Sem.FEnv} {E : Sem.EEnv}
    (hdev : Check.checkDevice (Check.mkEnv p) p.device = .ok ())
    (hinst : p.device.instances = [])
    (hgf : ∀ s ∈ p.device.body, s.etaGenericFree (Sem.xenv p) = true)
    (hxm : XModelsOk (Check.mkEnv p) (Sem.xenv p))
    (hclos : ∀ f, ((Check.mkEnv p).defns.get? f).isSome = true →
      ClosOk (Check.mkEnv p) F f) :
    ∀ (stim : List (List BV)) (regs : HashMap String BV),
      RegsOk p.device regs →
      (∀ ins ∈ stim, ins.length = p.device.inputs.length ∧
        ∀ i (h1 : i < ins.length) (h2 : i < p.device.inputs.length),
          ins[i].width = (p.device.inputs[i]).2) →
      ∀ (acc : List (List BV)),
      ∃ regsF outsRev,
        (stim.foldlM (Sem.foldStep F (Sem.xenv p) p.device E) (regs, acc))
          = .ok (regsF, outsRev)
        ∧ outsRev.length = stim.length + acc.length
        ∧ ∀ outs ∈ outsRev, outs ∈ acc ∨
            (outs.length = p.device.outputs.length ∧
              ∀ i (h1 : i < outs.length) (h2 : i < p.device.outputs.length),
                outs[i].width = (p.device.outputs[i]).2) := by
  intro stim
  induction stim with
  | nil =>
      intro regs _ _ acc
      exact ⟨regs, acc, rfl, by simp, fun outs h => .inl h⟩
  | cons ins rest ih =>
      intro regs hregs hstim acc
      obtain ⟨hlen, hw⟩ := hstim ins List.mem_cons_self
      obtain ⟨outs, regs', hstep, holen, how, hrOk⟩ :=
        step_progress (E := E) hdev hinst hgf hxm hclos hregs hlen hw
      obtain ⟨regsF, outsRev, hfold, hrlen, hrmem⟩ := ih regs' hrOk
        (fun i hi => hstim i (List.mem_cons_of_mem _ hi)) (outs :: acc)
      refine ⟨regsF, outsRev, ?_, by simp at hrlen ⊢; omega, ?_⟩
      · rw [List.foldlM_cons]
        rw [show Sem.foldStep F (Sem.xenv p) p.device E (regs, acc) ins
            = (Sem.step F (Sem.xenv p) p.device regs ins E >>= fun (outs, regs') =>
                pure (regs', outs :: acc)) from rfl]
        rw [hstep, except_bind_ok]
        dsimp only
        simp only [pure_bind]
        exact hfold
      · intro o ho
        rcases hrmem o ho with hin | hok
        · cases List.mem_cons.mp hin with
          | inl he => subst he; exact .inr ⟨holen, how⟩
          | inr ha => exact .inl ha
        · exact .inr hok

/-- Top-level progress: a checked, instance-free, eta-generic-free
program with a denoting definition environment runs on every
width-disciplined stimulus, producing declared-width outputs cycle for
cycle — execution cannot fail, so trace correspondence is never
vacuous. -/
theorem Program.run_progress {p : Program} {E : Sem.EEnv} {stim : List (List BV)}
    (hck : p.check = .ok ())
    (hinst : p.device.instances = [])
    (hgf : p.etaGenericFree = true)
    (hstim : ∀ ins ∈ stim, ins.length = p.device.inputs.length ∧
      ∀ i (h1 : i < ins.length) (h2 : i < p.device.inputs.length),
        ins[i].width = (p.device.inputs[i]).2)
    {F : Sem.FEnv} (hF : Sem.mkFEnv p E = .ok F) :
    ∃ trace, Program.run p stim E = .ok trace ∧ trace.length = stim.length ∧
      ∀ outs ∈ trace, outs.length = p.device.outputs.length ∧
        ∀ i (h1 : i < outs.length) (h2 : i < p.device.outputs.length),
          outs[i].width = (p.device.outputs[i]).2 := by
  obtain ⟨hnd, hext, hdefok, hdev⟩ := check_ok hck
  have hxm := check_xModelsOk hnd hext
  obtain ⟨hgfd, hgfs⟩ := etaGenericFree_unpack hgf
  have hnddefs : (p.defns.map (·.name)).Nodup := (List.nodup_append.mp hnd).2.1
  have hclos := mkFEnv_closOk hnddefs hdefok hgfd hxm hF
  obtain ⟨hinits, hnodreg⟩ := checkDevice_inits hdev hinst
  obtain ⟨regsF, outsRev, hfold, hrlen, hrmem⟩ :=
    runLoop_progress (E := E) hdev hinst hgfs hxm hclos stim (Sem.initRegs p.device)
      (initRegs_regsOk hnodreg hinits) hstim []
  refine ⟨outsRev.reverse, ?_, by simpa using hrlen, ?_⟩
  · unfold Program.run
    rw [hF, except_bind_ok]
    unfold Sem.run
    rw [hfold, except_bind_ok]
    rfl
  · intro outs ho
    rcases hrmem outs (List.mem_reverse.mp ho) with hin | hok
    · exact absurd hin (by simp)
    · exact hok

#print axioms Rwv.Hyle.Progress.step_progress
#print axioms Rwv.Hyle.Progress.Program.run_progress

end Progress

end Rwv.Hyle

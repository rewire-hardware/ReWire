/-
The canonical bits-to-data decoder `decode` — the inverse of the
representation function `Val.rep` (doc/eidos.md §7.5.1, ToHyle's
ctorRep layout: tag | zero pad | fields, MSB-first) — and THE
round-trip lemma of the foreign tier's semantic heart:

    decode_rep : decode Δ fuel t bv = .ok v → Val.rep Δ fuel v = .ok bv

`decode` is canonicality-checked: it errors (rather than repairs) on
width mismatches, out-of-range constructor tags, tags that alias a
duplicate constructor name (the `ctorTag` cross-check), nonzero pad
bits, and out-of-range `Finite` values — so a successful decode
produces exactly the canonical value whose representation is the
input, which is what `decode_rep` says with no side conditions.

The companion lemmas live downstream of their dependencies:
  * `decode_mono` (fuel monotonicity) in Rwv.Eidos.FuelMono;
  * `decode_vty` (decoded values are `VTy`-canonical) and
    `rep_decode` (decode ∘ rep = id on canonical, proxy-normal
    values) in Rwv.Eidos.Cexp, where `VTy` lives.

`Val.RepCanon` (defined here — it needs only `Val`) carves out the
domain on which `rep` is injective, for `rep_decode` (the inverse
direction, proved in Rwv.Eidos.Cstep): proxy-normal constructor forms
— the prim basis declares an actual `Proxy` data constructor, so both
`.proxy` and `.con _ "Proxy" []` inhabit `Proxy n` with the same
(empty) representation, and `decode` canonically produces `.proxy` —
and in-range `Finite` values (`VTy` deliberately does not track the
Finite bound, but `rep` wraps out-of-range values to their nbits-width
residue, which decodes to the wrapped value).

Per house style, the BV concatenation/slice kit is re-proved locally
(the committed files' copies are private); it is exported from this
file's `Decode` namespace so the downstream halves of the round trip
(Rwv.Eidos.Cexp) can reason about `decode`'s slices without a third
copy.
-/
import Rwv.Eidos.Value

namespace Rwv.Eidos

open Rwv.Hyle (BV)

/-! ## The local BV kit (projection level: widths and `getLsbD`) -/

namespace Decode

theorem bv_ext {x y : BV} (hw : x.width = y.width)
    (h : ∀ i, x.bits.getLsbD i = y.bits.getLsbD i) : x = y := by
  cases x with | mk wx bx =>
  cases y with | mk wy bv =>
  dsimp only at hw h
  subst hw
  exact congrArg (BV.mk wx) (BitVec.eq_of_getLsbD_eq fun i _ => h i)

/-- The bundled concatenation (left operand at the MSB end). -/
def bvCat (a b : BV) : BV := ⟨a.width + b.width, a.bits ++ b.bits⟩

theorem bvCat_getLsbD (a b : BV) (i : Nat) :
    (bvCat a b).bits.getLsbD i =
      if i < b.width then b.bits.getLsbD i else a.bits.getLsbD (i - b.width) := by
  simp [bvCat, BitVec.getLsbD_append]

theorem getLsbD_ge {n : Nat} (x : BitVec n) {i : Nat} (h : n ≤ i) :
    x.getLsbD i = false :=
  x.getLsbD_of_ge i h

theorem bvCat_width (a b : BV) : (bvCat a b).width = a.width + b.width := rfl

theorem bvCat_zero_left {a b : BV} (h : a.width = 0) : bvCat a b = b := by
  refine bv_ext (by rw [bvCat_width, h, Nat.zero_add]) ?_
  intro i
  rw [bvCat_getLsbD]
  by_cases hi : i < b.width
  · rw [if_pos hi]
  · rw [if_neg hi, getLsbD_ge a.bits (by omega), getLsbD_ge b.bits (by omega)]

theorem bvCat_zero_right {a b : BV} (h : b.width = 0) : bvCat a b = a := by
  refine bv_ext (by rw [bvCat_width, h, Nat.add_zero]) ?_
  intro i
  rw [bvCat_getLsbD, if_neg (by omega), h, Nat.sub_zero]

theorem bvCat_assoc (a b c : BV) : bvCat (bvCat a b) c = bvCat a (bvCat b c) := by
  refine bv_ext (by simp only [bvCat_width]; omega) ?_
  intro i
  rw [bvCat_getLsbD (bvCat a b) c i, bvCat_getLsbD a (bvCat b c) i]
  rcases Nat.lt_or_ge i c.width with hc | hc
  · rw [if_pos hc, if_pos (show i < (bvCat b c).width by rw [bvCat_width]; omega),
        bvCat_getLsbD b c i, if_pos hc]
  · rcases Nat.lt_or_ge (i - c.width) b.width with hb | hb
    · rw [if_neg (by omega), bvCat_getLsbD a b (i - c.width), if_pos hb,
          if_pos (show i < (bvCat b c).width by rw [bvCat_width]; omega),
          bvCat_getLsbD b c i, if_neg (by omega)]
    · rw [if_neg (by omega), bvCat_getLsbD a b (i - c.width), if_neg (by omega),
          if_neg (show ¬ i < (bvCat b c).width by rw [bvCat_width]; omega), bvCat_width]
      congr 1
      omega

/-- `Val.bvConcat` as a fold of the bundled concatenation. -/
def catAll (xs : List BV) : BV := xs.foldl bvCat BV.nil

theorem bvConcat_eq (xs : List BV) : Val.bvConcat xs = catAll xs := rfl

theorem foldl_bvCat (l : List BV) :
    ∀ acc, l.foldl bvCat acc = bvCat acc (catAll l) := by
  induction l with
  | nil => intro acc; exact (bvCat_zero_right rfl).symm
  | cons y ys ih =>
      intro acc
      rw [List.foldl_cons, ih (bvCat acc y), bvCat_assoc]
      congr 1
      simp only [catAll, List.foldl_cons]
      rw [show bvCat BV.nil y = y from bvCat_zero_left rfl, ih y]
      simp only [catAll]

theorem catAll_cons (x : BV) (xs : List BV) :
    catAll (x :: xs) = bvCat x (catAll xs) := by
  simp only [catAll, List.foldl_cons]
  rw [show bvCat BV.nil x = x from bvCat_zero_left rfl]
  exact foldl_bvCat xs x

theorem catAll_nil : catAll [] = BV.nil := rfl

theorem catAll_width (xs : List BV) :
    (catAll xs).width = (xs.map (·.width)).sum := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
      rw [catAll_cons, bvCat_width, ih, List.map_cons, List.sum_cons]

/-- The bundled slice (LSB offset, width). -/
def sliceBV (x : BV) (i w : Nat) : BV := ⟨w, x.bits.extractLsb' i w⟩

theorem sliceBV_width (x : BV) (i w : Nat) : (sliceBV x i w).width = w := rfl

theorem sliceBV_getLsbD (x : BV) (i w j : Nat) :
    (sliceBV x i w).bits.getLsbD j = (decide (j < w) && x.bits.getLsbD (i + j)) := by
  simp [sliceBV, BitVec.getLsbD_extractLsb']

/-- The top `w` bits (the MSB end). -/
def takeTop (w : Nat) (x : BV) : BV := sliceBV x (x.width - w) w

/-- Everything below the top `w` bits. -/
def dropTop (w : Nat) (x : BV) : BV := sliceBV x 0 (x.width - w)

theorem takeTop_width (w : Nat) (x : BV) : (takeTop w x).width = w := rfl

theorem dropTop_width (w : Nat) (x : BV) : (dropTop w x).width = x.width - w := rfl

/-- Splitting off the top `w` bits and re-concatenating is the
identity (when `w` fits). -/
theorem cat_takeTop_dropTop {w : Nat} {x : BV} (h : w ≤ x.width) :
    bvCat (takeTop w x) (dropTop w x) = x := by
  refine bv_ext (show w + (x.width - w) = x.width by omega) ?_
  intro i
  rw [bvCat_getLsbD]
  by_cases hi : i < x.width - w
  · rw [if_pos (show i < (dropTop w x).width from hi)]
    simp only [dropTop]
    rw [sliceBV_getLsbD, decide_eq_true hi, Bool.true_and, Nat.zero_add]
  · rw [if_neg (show ¬ i < (dropTop w x).width from hi)]
    show (takeTop w x).bits.getLsbD (i - (x.width - w)) = x.bits.getLsbD i
    simp only [takeTop]
    rw [sliceBV_getLsbD]
    by_cases hj : i - (x.width - w) < w
    · rw [decide_eq_true hj, Bool.true_and]
      congr 1
      omega
    · rw [decide_eq_false hj, Bool.false_and, getLsbD_ge x.bits (by omega)]

/-- The top bits of a concatenation are its left piece. -/
theorem takeTop_cat {a b : BV} : takeTop a.width (bvCat a b) = a := by
  have hsub : (bvCat a b).width - a.width = b.width := by rw [bvCat_width]; omega
  refine bv_ext rfl ?_
  intro j
  simp only [takeTop]
  rw [sliceBV_getLsbD, hsub, bvCat_getLsbD]
  by_cases hj : j < a.width
  · rw [decide_eq_true hj, Bool.true_and, if_neg (by omega)]
    congr 1
    omega
  · rw [decide_eq_false hj, Bool.false_and, getLsbD_ge a.bits (by omega)]

/-- Below the top bits of a concatenation is its right piece. -/
theorem dropTop_cat {a b : BV} : dropTop a.width (bvCat a b) = b := by
  have hsub : (bvCat a b).width - a.width = b.width := by rw [bvCat_width]; omega
  refine bv_ext (show (dropTop a.width (bvCat a b)).width = b.width
    from (dropTop_width _ _).trans hsub) ?_
  intro j
  simp only [dropTop]
  rw [sliceBV_getLsbD, hsub, Nat.zero_add, bvCat_getLsbD]
  by_cases hj : j < b.width
  · rw [decide_eq_true hj, Bool.true_and, if_pos hj]
  · rw [decide_eq_false hj, Bool.false_and, getLsbD_ge b.bits (by omega)]

/-- Same-width bit vectors with equal unsigned readings are equal. -/
theorem bv_nat_ext {x y : BV} (hw : x.width = y.width) (hn : x.nat = y.nat) : x = y := by
  cases x with | mk wx bx =>
  cases y with | mk wy by' =>
  dsimp only at hw
  subst hw
  simp only [BV.nat] at hn
  exact congrArg (BV.mk wx) (BitVec.eq_of_toNat_eq hn)

/-- Re-bundling a bit vector's unsigned reading at its own width is
the identity. -/
theorem ofNat_nat_self (w : Nat) (x : BV) (hw : x.width = w) :
    (⟨w, BitVec.ofNat w x.nat⟩ : BV) = x := by
  subst hw
  refine bv_nat_ext rfl ?_
  simp only [BV.nat, BitVec.toNat_ofNat]
  exact Nat.mod_eq_of_lt x.bits.isLt

/-- A zero-reading bit vector has all-false bits. -/
theorem nat_zero_getLsbD {x : BV} (h : x.nat = 0) (i : Nat) :
    x.bits.getLsbD i = false := by
  have hb : x.bits = 0 := by
    apply BitVec.eq_of_toNat_eq
    simpa [BV.nat] using h
  rw [hb]
  simp

/-- Erase the `attach` from a mapM over the attached list (the shape
`Val.rep`'s vec/con cases produce). -/
theorem mapM_attach_erase {α β : Type} (f : α → Except String β) :
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

/-- The reassembly of a constructor representation from its decoded
slices: for `tagW + fw ≤ bv.width` with a zero pad region, the
`tag slice | zero pad | fields slice` concatenation is `bv` itself. -/
theorem con_reassemble {bv : BV} {tagW fw : Nat} (hle : tagW + fw ≤ bv.width)
    (hpad : (sliceBV bv fw (bv.width - tagW - fw)).nat = 0) :
    Val.bvConcat [takeTop tagW bv, ⟨bv.width - tagW - fw, 0⟩, sliceBV bv 0 fw] = bv := by
  rw [bvConcat_eq, catAll_cons, catAll_cons, catAll_cons, catAll_nil,
    show bvCat (sliceBV bv 0 fw) BV.nil = sliceBV bv 0 fw from bvCat_zero_right rfl]
  refine bv_ext (show tagW + (bv.width - tagW - fw + fw) = bv.width by omega) ?_
  intro i
  rw [bvCat_getLsbD]
  by_cases h1 : i < bv.width - tagW - fw + fw
  · rw [if_pos (show i < (bvCat (⟨bv.width - tagW - fw, 0⟩ : BV) (sliceBV bv 0 fw)).width
        by rw [bvCat_width]; exact h1)]
    rw [bvCat_getLsbD]
    by_cases h2 : i < fw
    · rw [if_pos (show i < (sliceBV bv 0 fw).width from h2), sliceBV_getLsbD, Nat.zero_add,
        decide_eq_true h2, Bool.true_and]
    · rw [if_neg (show ¬ i < (sliceBV bv 0 fw).width from h2)]
      show (BitVec.ofNat (bv.width - tagW - fw) 0).getLsbD (i - fw) = bv.bits.getLsbD i
      have hpb := nat_zero_getLsbD hpad (i - fw)
      rw [sliceBV_getLsbD, decide_eq_true (show i - fw < bv.width - tagW - fw by omega),
        Bool.true_and, show fw + (i - fw) = i by omega] at hpb
      rw [hpb]
      simp
  · rw [if_neg (show ¬ i < (bvCat (⟨bv.width - tagW - fw, 0⟩ : BV) (sliceBV bv 0 fw)).width
        by rw [bvCat_width]; exact h1)]
    show (takeTop tagW bv).bits.getLsbD (i - (bv.width - tagW - fw + fw))
      = bv.bits.getLsbD i
    simp only [takeTop]
    rw [sliceBV_getLsbD]
    by_cases h3 : i - (bv.width - tagW - fw + fw) < tagW
    · rw [decide_eq_true h3, Bool.true_and]
      congr 1
      omega
    · rw [decide_eq_false h3, Bool.false_and, getLsbD_ge bv.bits (by omega)]

end Decode

/-! ## The decoder -/

open Decode in
/-- Select the constructor a bit pattern's tag designates: the tagless
eponymous constructor for the tuple family; otherwise the declared
constructor at the tag's index (an out-of-range tag is a canonicality
error). Returns the constructor name and the tag width. -/
def selectCtor (Δ : DEnv) (tc : String) (bv : BV) : Except String (String × Nat) :=
  if Ty.isTupleCon tc then pure (tc, 0)
  else match Δ.ctors.get? tc with
    | some cs =>
        (match cs[(takeTop (nbits cs.length) bv).nat]? with
        | some cn => pure (cn, nbits cs.length)
        | none => throw "decode: constructor tag out of range")
    | none => throw s!"decode: unknown datatype {tc}"

open Decode in
mutual

/-- `decode Δ fuel t bv`: the canonical value of the representable
type `t` whose representation is `bv` — the inverse of `Val.rep`,
canonicality-checked (width mismatches, out-of-range or aliased
constructor tags, nonzero pads, and out-of-range `Finite` values are
errors). The fuel discipline mirrors `Val.rep`'s exactly (one unit per
value level; `sizeOf` at the undecremented fuel in the constructor
case), so `decode_rep` holds at the same fuel. -/
def decode (Δ : DEnv) : Nat → Ty → BV → Except String Val
  | 0, _, _ => throw "decode: fuel exhausted"
  | fuel + 1, t, bv =>
    match Ty.flatten t with
    | (.con "Vec", [n, te]) =>
        (match Ty.evalNat n with
        | some k => do
            let we ← Δ.sizeOf fuel [] te
            if bv.width = k * we then do
              let fields ← decodeFields Δ fuel (List.replicate k (te, we)) bv
              pure (.vec fields)
            else throw "decode: Vec width mismatch"
        | none => throw "decode: open Vec length")
    | (.con "Finite", [n]) =>
        (match Ty.evalNat n with
        | some k =>
            if bv.width = nbits k then
              if bv.nat < k then pure (.finite k bv.nat)
              else throw "decode: Finite value out of range"
            else throw "decode: Finite width mismatch"
        | none => throw "decode: open Finite bound")
    | (.con "Integer", []) =>
        if bv.width = 128 then pure (.integer (bv.bits.setWidth 128))
        else throw "decode: Integer width mismatch"
    | (.con "Proxy", _) =>
        if bv.width = 0 then pure .proxy
        else throw "decode: Proxy width mismatch"
    | (.con tc, _) => do
        let whole ← Δ.sizeOf (fuel + 1) [] t
        if bv.width = whole then do
          let cw ← selectCtor Δ tc bv
          let (tag, tagW') ← Δ.ctorTag t cw.1
          if tagW' = cw.2 ∧ tag = (takeTop cw.2 bv).nat then
            match Δ.ctorSig.get? cw.1 with
            | some sig => do
                let sub ← DEnv.matchTy (Ty.flattenArrow sig.ty).2 t
                let instTys := (Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)
                let ws ← instTys.mapM (Δ.sizeOf fuel [])
                if cw.2 + ws.sum ≤ whole then
                  if (sliceBV bv ws.sum (whole - cw.2 - ws.sum)).nat = 0 then do
                    let fields ← decodeFields Δ fuel (instTys.zip ws) (sliceBV bv 0 ws.sum)
                    pure (.con t cw.1 fields)
                  else throw "decode: nonzero pad bits"
                else throw s!"decode: constructor {cw.1} payload exceeds the type width"
            | none => throw s!"decode: unknown constructor signature: {cw.1}"
          else throw "decode: constructor tag aliases a duplicate name"
        else throw "decode: width mismatch"
    | _ => throw "decode: unrepresentable type"
  termination_by fuel _ _ => (fuel, 0)

/-- Decode a MSB-first sequence of fields at their (type, width)
schedule, consuming the region from the top. Each level checks the
region width, so a success needs no external width invariant. -/
def decodeFields (Δ : DEnv) (fuel : Nat) : List (Ty × Nat) → BV → Except String (List Val)
  | [], bv =>
      if bv.width = 0 then pure [] else throw "decode: leftover field bits"
  | (t, w) :: rest, bv =>
      if bv.width = w + (rest.map (·.2)).sum then do
        let v ← decode Δ fuel t (takeTop w bv)
        let vs ← decodeFields Δ fuel rest (dropTop w bv)
        pure (v :: vs)
      else throw "decode: field region width mismatch"
  termination_by tws _ => (fuel, tws.length + 1)

end

/-! ## Representation canonicality (the domain of the inverse direction) -/

/-- The values `rep` is injective on: the canonical inhabitant of
`Proxy n` is `.proxy` (never the prim basis' `Proxy` data constructor
applied — both rep to the empty bit vector), and `Finite` values are
in range (out-of-range values rep to their wrapped residues). The
inverse round trip `rep_decode` (Rwv.Eidos.Cstep) holds on this
domain. -/
inductive Val.RepCanon : Val → Prop where
  | vec {es : List Val} : (∀ e ∈ es, Val.RepCanon e) → Val.RepCanon (.vec es)
  | integer {v : BitVec 128} : Val.RepCanon (.integer v)
  | finite {b i : Nat} : i < b → Val.RepCanon (.finite b i)
  | str {s : String} : Val.RepCanon (.str s)
  | proxy : Val.RepCanon .proxy
  | con {ty : Ty} {c : String} {fields : List Val} :
      (Ty.flatten ty).1 ≠ .con "Proxy" →
      (∀ f ∈ fields, Val.RepCanon f) →
      Val.RepCanon (.con ty c fields)
  | closL {x : Id} {env : List (Int × Val)} {body : Exp} :
      Val.RepCanon (.closL x env body)
  | closD {f : Id} {pre : List Val} : Val.RepCanon (.closD f pre)

/-! ## Inversion plumbing -/

namespace Decode

theorem error_ne_ok {α : Type} {msg : String} {a : α} {P : Prop}
    (h : (Except.error msg : Except String α) = .ok a) : P := by
  cases h

theorem except_bind_eq_ok {α β : Type} {x : Except String α}
    {f : α → Except String β} {b : β} (h : (x >>= f) = .ok b) :
    ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x with
  | error e => exact error_ne_ok h
  | ok a => exact ⟨a, rfl, h⟩

theorem except_pure_def {α : Type} (a : α) :
    (pure a : Except String α) = .ok a := rfl

theorem except_bind_ok {α β : Type} (a : α) (f : α → Except String β) :
    (Except.ok a >>= f) = f a := rfl

end Decode

/-! ## Slice positions inside a three-piece concatenation (the
`tag | pad | fields` layout, for the inverse direction) -/

namespace Decode

/-- The low `b.width` bits of a concatenation are its right piece. -/
theorem sliceBV_cat_low0 {a b : BV} : sliceBV (bvCat a b) 0 b.width = b := by
  refine bv_ext rfl ?_
  intro j
  rw [sliceBV_getLsbD, Nat.zero_add, bvCat_getLsbD]
  by_cases hj : j < b.width
  · rw [decide_eq_true hj, Bool.true_and, if_pos hj]
  · rw [decide_eq_false hj, Bool.false_and, getLsbD_ge b.bits (by omega)]

/-- The middle piece of a three-piece concatenation, sliced at its
position. -/
theorem sliceBV_mid {a b c : BV} : sliceBV (bvCat a (bvCat b c)) c.width b.width = b := by
  refine bv_ext rfl ?_
  intro j
  rw [sliceBV_getLsbD, bvCat_getLsbD]
  by_cases hj : j < b.width
  · rw [decide_eq_true hj, Bool.true_and,
      if_pos (show c.width + j < (bvCat b c).width by rw [bvCat_width]; omega),
      bvCat_getLsbD, if_neg (by omega)]
    congr 1
    omega
  · rw [decide_eq_false hj, Bool.false_and, getLsbD_ge b.bits (by omega)]

end Decode

/-! ## The intro direction for field regions (rep_decode's engine) -/

open Decode in
/-- Pointwise widths along a field schedule. -/
theorem zip_widths_eq :
    ∀ (tws : List (Ty × Nat)) (vs : List Val) (bs : List BV),
      vs.length = tws.length → bs.length = tws.length →
      (∀ tvb ∈ tws.zip (vs.zip bs), (tvb.2.2 : BV).width = tvb.1.2) →
      bs.map (·.width) = tws.map (·.2) := by
  intro tws
  induction tws with
  | nil =>
      intro vs bs _ hlb _
      have hb : bs = [] := List.length_eq_zero_iff.mp (by simpa using hlb)
      subst hb
      rfl
  | cons tw rest ih =>
      intro vs bs hlv hlb hmem
      obtain ⟨t, w⟩ := tw
      cases vs with
      | nil => simp at hlv
      | cons v vs' =>
      cases bs with
      | nil => simp at hlb
      | cons b bs' =>
          have hhead : b.width = w := by
            simpa using hmem ((t, w), v, b) (by simp)
          have htail := ih vs' bs' (by simpa using hlv) (by simpa using hlb)
            (fun tvb htvb => hmem tvb (by simp [htvb]))
          simp only [List.map_cons, hhead, htail]

open Decode in
/-- The intro direction of `decodeFields`: pointwise-decodable pieces
at the schedule's widths decode from their concatenation. -/
theorem decodeFields_intro {Δ : DEnv} {f : Nat} :
    ∀ (tws : List (Ty × Nat)) (vs : List Val) (bs : List BV),
      vs.length = tws.length → bs.length = tws.length →
      (∀ tvb ∈ tws.zip (vs.zip bs),
        (tvb.2.2 : BV).width = tvb.1.2 ∧ decode Δ f tvb.1.1 tvb.2.2 = .ok tvb.2.1) →
      decodeFields Δ f tws (Val.bvConcat bs) = .ok vs := by
  intro tws
  induction tws with
  | nil =>
      intro vs bs hlv hlb _
      have hv : vs = [] := List.length_eq_zero_iff.mp (by simpa using hlv)
      have hb : bs = [] := List.length_eq_zero_iff.mp (by simpa using hlb)
      subst hv
      subst hb
      rw [decodeFields, if_pos (show (Val.bvConcat []).width = 0 from rfl)]
      rfl
  | cons tw rest ih =>
      intro vs bs hlv hlb hmem
      obtain ⟨t, w⟩ := tw
      cases vs with
      | nil => simp at hlv
      | cons v vs' =>
      cases bs with
      | nil => simp at hlb
      | cons b bs' =>
          have hbw : b.width = w := by
            simpa using (hmem ((t, w), v, b) (by simp)).1
          have hdec : decode Δ f t b = .ok v := by
            simpa using (hmem ((t, w), v, b) (by simp)).2
          have hwtl : bs'.map (·.width) = rest.map (·.2) :=
            zip_widths_eq rest vs' bs' (by simpa using hlv) (by simpa using hlb)
              (fun tvb htvb => (hmem tvb (by simp [htvb])).1)
          have hcat : Val.bvConcat (b :: bs') = bvCat b (Val.bvConcat bs') := by
            rw [bvConcat_eq, catAll_cons, ← bvConcat_eq]
          rw [decodeFields, if_pos (by
            rw [hcat, bvCat_width, bvConcat_eq, catAll_width, hwtl, hbw])]
          rw [hcat]
          have htk : takeTop w (bvCat b (Val.bvConcat bs')) = b := by
            rw [← hbw]
            exact takeTop_cat
          have hdr : dropTop w (bvCat b (Val.bvConcat bs')) = Val.bvConcat bs' := by
            rw [← hbw]
            exact dropTop_cat
          rw [htk, hdec, except_bind_ok, hdr,
            ih vs' bs' (by simpa using hlv) (by simpa using hlb)
              (fun tvb htvb => hmem tvb (by simp [htvb])),
            except_bind_ok, except_pure_def]

/-! ## Arm-reduction lemmas (the `decode` dispatch, resolved per head;
the string-literal matcher does not reduce under `dsimp`, so the
one-time `split` lives here and downstream proofs rewrite) -/

open Decode in
theorem decode_vec_red {Δ : DEnv} {fuel : Nat} {t n te : Ty} {bv : BV}
    (hfl : Ty.flatten t = (.con "Vec", [n, te])) :
    decode Δ (fuel + 1) t bv
      = (match Ty.evalNat n with
        | some k => do
            let we ← Δ.sizeOf fuel [] te
            if bv.width = k * we then do
              let fields ← decodeFields Δ fuel (List.replicate k (te, we)) bv
              pure (Val.vec fields)
            else throw "decode: Vec width mismatch"
        | none => throw "decode: open Vec length") := by
  rw [decode, hfl]
  rfl

open Decode in
theorem decode_finite_red {Δ : DEnv} {fuel : Nat} {t n : Ty} {bv : BV}
    (hfl : Ty.flatten t = (.con "Finite", [n])) :
    decode Δ (fuel + 1) t bv
      = (match Ty.evalNat n with
        | some k =>
            if bv.width = nbits k then
              if bv.nat < k then pure (.finite k bv.nat)
              else throw "decode: Finite value out of range"
            else throw "decode: Finite width mismatch"
        | none => throw "decode: open Finite bound") := by
  rw [decode, hfl]
  rfl

open Decode in
theorem decode_integer_red {Δ : DEnv} {fuel : Nat} {t : Ty} {bv : BV}
    (hfl : Ty.flatten t = (.con "Integer", [])) :
    decode Δ (fuel + 1) t bv
      = (if bv.width = 128 then pure (.integer (bv.bits.setWidth 128))
         else throw "decode: Integer width mismatch") := by
  rw [decode, hfl]
  rfl

open Decode in
theorem decode_proxy_red {Δ : DEnv} {fuel : Nat} {t : Ty} {args : List Ty} {bv : BV}
    (hfl : Ty.flatten t = (.con "Proxy", args)) :
    decode Δ (fuel + 1) t bv
      = (if bv.width = 0 then pure .proxy
         else throw "decode: Proxy width mismatch") := by
  rw [decode, hfl]
  rfl

open Decode in
theorem decode_con_red {Δ : DEnv} {fuel : Nat} {t : Ty} {tc : String} {args : List Ty}
    {bv : BV} (hfl : Ty.flatten t = (.con tc, args))
    (h1 : tc ≠ "Vec") (h2 : tc ≠ "Finite") (h3 : tc ≠ "Integer") (h4 : tc ≠ "Proxy") :
    decode Δ (fuel + 1) t bv
      = (do
          let whole ← Δ.sizeOf (fuel + 1) [] t
          if bv.width = whole then do
            let cw ← selectCtor Δ tc bv
            let (tag, tagW') ← Δ.ctorTag t cw.1
            if tagW' = cw.2 ∧ tag = (takeTop cw.2 bv).nat then
              match Δ.ctorSig.get? cw.1 with
              | some sig => do
                  let sub ← DEnv.matchTy (Ty.flattenArrow sig.ty).2 t
                  let instTys := (Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub)
                  let ws ← instTys.mapM (Δ.sizeOf fuel [])
                  if cw.2 + ws.sum ≤ whole then
                    if (sliceBV bv ws.sum (whole - cw.2 - ws.sum)).nat = 0 then do
                      let fields ← decodeFields Δ fuel (instTys.zip ws)
                        (sliceBV bv 0 ws.sum)
                      pure (.con t cw.1 fields)
                    else throw "decode: nonzero pad bits"
                  else throw s!"decode: constructor {cw.1} payload exceeds the type width"
              | none => throw s!"decode: unknown constructor signature: {cw.1}"
            else throw "decode: constructor tag aliases a duplicate name"
          else throw "decode: width mismatch") := by
  rw [decode, hfl]
  split
  · rename_i heq
    exfalso
    have hh : Ty.con tc = Ty.con "Vec" := congrArg Prod.fst heq
    injection hh with hh
    exact h1 hh
  · rename_i heq
    exfalso
    have hh : Ty.con tc = Ty.con "Finite" := congrArg Prod.fst heq
    injection hh with hh
    exact h2 hh
  · rename_i heq
    exfalso
    have hh : Ty.con tc = Ty.con "Integer" := congrArg Prod.fst heq
    injection hh with hh
    exact h3 hh
  · rename_i heq
    exfalso
    have hh : Ty.con tc = Ty.con "Proxy" := congrArg Prod.fst heq
    injection hh with hh
    exact h4 hh
  · rename_i heq
    injection heq with h1 h2
    injection h1 with h1
    subst h1
    rfl
  · rename_i hcon
    exact (hcon tc args rfl).elim

/-! ## Inversions for the canonicality lemma (decode_vty, downstream) -/

open Decode in
/-- What a successful constructor selection guarantees. -/
theorem selectCtor_inv {Δ : DEnv} {tc : String} {bv : BV} {cn : String} {tagW : Nat}
    (h : selectCtor Δ tc bv = .ok (cn, tagW)) :
    (Ty.isTupleCon tc = true ∧ cn = tc ∧ tagW = 0) ∨
    (Ty.isTupleCon tc = false ∧ ∃ cs, Δ.ctors.get? tc = some cs ∧ cn ∈ cs ∧
      tagW = nbits cs.length) := by
  rw [selectCtor] at h
  split at h
  · rename_i htup
    rw [except_pure_def] at h
    injection h with h
    exact .inl ⟨htup, (Prod.mk.injEq .. ▸ h).1.symm ▸ ⟨rfl, ((Prod.mk.injEq .. ▸ h).2).symm ▸ rfl⟩⟩
  · rename_i htup
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i cs hcs
    split at h
    rotate_left
    · exact error_ne_ok h
    rename_i cn' hcn
    rw [except_pure_def] at h
    injection h with h
    have h1 : cn' = cn := congrArg Prod.fst h
    have h2 : nbits cs.length = tagW := congrArg Prod.snd h
    refine .inr ⟨Bool.of_not_eq_true htup, cs, hcs, ?_, h2.symm⟩
    rw [← h1]
    exact List.mem_of_getElem? hcn

open Decode in
/-- Pointwise inversion of a field decode: every decoded value came
from a decode at its schedule entry's type. -/
theorem decodeFields_pointwise {Δ : DEnv} {fuel : Nat} :
    ∀ {tws : List (Ty × Nat)} {bv : BV} {vs : List Val},
      decodeFields Δ fuel tws bv = .ok vs →
      ∀ i (h1 : i < vs.length) (h2 : i < tws.length),
        ∃ bv', decode Δ fuel (tws[i].1) bv' = .ok vs[i] := by
  intro tws
  induction tws with
  | nil =>
      intro bv vs h i h1 h2
      exact absurd h2 (by simp)
  | cons tw rest ih =>
      intro bv vs h i h1 h2
      obtain ⟨t, w⟩ := tw
      rw [decodeFields] at h
      split at h
      rotate_left
      · exact error_ne_ok h
      obtain ⟨v, hv, h⟩ := except_bind_eq_ok h
      obtain ⟨vs', hvs, h⟩ := except_bind_eq_ok h
      rw [except_pure_def] at h
      injection h with h
      subst h
      match i, h1, h2 with
      | 0, _, _ => exact ⟨takeTop w bv, hv⟩
      | i + 1, h1, h2 =>
          simpa using ih hvs i (by simpa using h1) (by simpa using h2)

/-! ## Successful decodes have the schedule's length -/

open Decode in
theorem decodeFields_length {Δ : DEnv} {fuel : Nat} :
    ∀ {tws : List (Ty × Nat)} {bv : BV} {vs : List Val},
      decodeFields Δ fuel tws bv = .ok vs → vs.length = tws.length := by
  intro tws
  induction tws with
  | nil =>
      intro bv vs h
      rw [decodeFields] at h
      split at h
      · rw [except_pure_def] at h
        injection h with h
        subst h
        rfl
      · exact error_ne_ok h
  | cons tw rest ih =>
      intro bv vs h
      obtain ⟨t, w⟩ := tw
      rw [decodeFields] at h
      split at h
      · obtain ⟨v, _hv, h⟩ := except_bind_eq_ok h
        obtain ⟨vs', hvs, h⟩ := except_bind_eq_ok h
        rw [except_pure_def] at h
        injection h with h
        subst h
        simp [ih hvs]
      · exact error_ne_ok h

/-! ## THE round-trip: rep ∘ decode = id -/

open Decode in
/-- The field half of `decode_rep`, at a fixed fuel with the decoder's
round trip at that fuel as a hypothesis (the mutual induction,
unbundled: the outer induction is on fuel, this inner one on the field
schedule). -/
private theorem decodeFields_rep_of {Δ : DEnv} {fuel : Nat}
    (IH : ∀ {t : Ty} {bv : BV} {v : Val},
      decode Δ fuel t bv = .ok v → Val.rep Δ fuel v = .ok bv) :
    ∀ {tws : List (Ty × Nat)} {bv : BV} {vs : List Val},
      decodeFields Δ fuel tws bv = .ok vs →
      ∃ bs, vs.mapM (Val.rep Δ fuel) = .ok bs ∧ Val.bvConcat bs = bv := by
  intro tws
  induction tws with
  | nil =>
      intro bv vs h
      rw [decodeFields] at h
      split at h
      rotate_left
      · exact error_ne_ok h
      rename_i hw
      rw [except_pure_def] at h
      injection h with h
      subst h
      refine ⟨[], rfl, ?_⟩
      refine bv_ext (show (Val.bvConcat []).width = bv.width by rw [hw]; rfl) ?_
      intro i
      rw [getLsbD_ge (Val.bvConcat []).bits (Nat.zero_le i),
        getLsbD_ge bv.bits (by omega)]
  | cons tw rest ih =>
      intro bv vs h
      obtain ⟨t, w⟩ := tw
      rw [decodeFields] at h
      split at h
      rotate_left
      · exact error_ne_ok h
      rename_i hw
      obtain ⟨v, hv, h⟩ := except_bind_eq_ok h
      obtain ⟨vs', hvs, h⟩ := except_bind_eq_ok h
      rw [except_pure_def] at h
      injection h with h
      subst h
      obtain ⟨bs', hbs, hcat⟩ := ih hvs
      refine ⟨takeTop w bv :: bs', ?_, ?_⟩
      · rw [List.mapM_cons, IH hv, except_bind_ok, hbs, except_bind_ok, except_pure_def]
      · rw [bvConcat_eq, catAll_cons, ← bvConcat_eq, hcat]
        exact cat_takeTop_dropTop (by omega)

open Decode in
/-- THE round-trip lemma (forward direction): a successful decode's
value represents back to exactly the input bits, at the same fuel. -/
theorem decode_rep {Δ : DEnv} :
    ∀ {fuel : Nat} {t : Ty} {bv : BV} {v : Val},
      decode Δ fuel t bv = .ok v → Val.rep Δ fuel v = .ok bv := by
  intro fuel
  induction fuel with
  | zero =>
      intro t bv v h
      rw [decode] at h
      exact error_ne_ok h
  | succ fuel ih =>
      intro t bv v h
      rw [decode] at h
      split at h
      · -- Vec
        split at h
        rotate_left
        · exact error_ne_ok h
        obtain ⟨we, _hwe, h⟩ := except_bind_eq_ok h
        split at h
        rotate_left
        · exact error_ne_ok h
        obtain ⟨fields, hfs, h⟩ := except_bind_eq_ok h
        rw [except_pure_def] at h
        injection h with h
        subst h
        obtain ⟨bs, hbs, hcat⟩ := decodeFields_rep_of (fun {t bv v} => ih) hfs
        rw [Val.rep, mapM_attach_erase, hbs, except_bind_ok, except_pure_def, hcat]
      · -- Finite
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i k _
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hw
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hlt
        rw [except_pure_def] at h
        injection h with h
        subst h
        rw [Val.rep, except_pure_def]
        exact congrArg Except.ok (ofNat_nat_self (nbits k) bv hw)
      · -- Integer
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hw
        rw [except_pure_def] at h
        injection h with h
        subst h
        rw [Val.rep, except_pure_def]
        refine congrArg Except.ok (bv_ext (show (128 : Nat) = bv.width from hw.symm) ?_)
        intro i
        simp only [BitVec.getLsbD_setWidth]
        by_cases hi : i < 128
        · rw [decide_eq_true hi, Bool.true_and]
        · rw [decide_eq_false hi, Bool.false_and, getLsbD_ge bv.bits (by omega)]
      · -- Proxy
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hw
        rw [except_pure_def] at h
        injection h with h
        subst h
        rw [Val.rep, except_pure_def]
        refine congrArg Except.ok (bv_ext (show Rwv.Hyle.BV.nil.width = bv.width
          by rw [hw]; rfl) ?_)
        intro i
        rw [getLsbD_ge Rwv.Hyle.BV.nil.bits (Nat.zero_le i), getLsbD_ge bv.bits (by omega)]
      · -- Datatype: tag | zero pad | fields
        obtain ⟨whole, hwhole, h⟩ := except_bind_eq_ok h
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hbw
        obtain ⟨cw, _hcw, h⟩ := except_bind_eq_ok h
        obtain ⟨tt, htag, h⟩ := except_bind_eq_ok h
        obtain ⟨tag, tagW'⟩ := tt
        dsimp only at h
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i htw
        obtain ⟨htw1, htw2⟩ := htw
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i sig hsig
        obtain ⟨sub, hsub, h⟩ := except_bind_eq_ok h
        obtain ⟨ws, hws, h⟩ := except_bind_eq_ok h
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hle
        split at h
        rotate_left
        · exact error_ne_ok h
        rename_i hpad
        obtain ⟨fields, hfs, h⟩ := except_bind_eq_ok h
        rw [except_pure_def] at h
        injection h with h
        subst h
        obtain ⟨bs, hbs, hcat⟩ := decodeFields_rep_of (fun {t bv v} => ih) hfs
        rw [Val.rep, hwhole, except_bind_ok, htag, except_bind_ok]
        dsimp only
        rw [mapM_attach_erase, hbs, except_bind_ok]
        have hcw' : (Val.bvConcat bs).width = ws.sum := by
          rw [hcat]
          rfl
        rw [if_pos (show tagW' + (Val.bvConcat bs).width ≤ whole by
          rw [hcw', htw1]; exact hle)]
        rw [except_pure_def]
        refine congrArg Except.ok ?_
        rw [hcw', hcat, htw1, htw2,
          show (⟨cw.2, BitVec.ofNat cw.2 (takeTop cw.2 bv).nat⟩ : BV)
            = takeTop cw.2 bv from ofNat_nat_self cw.2 (takeTop cw.2 bv) rfl]
        have hb : bv.width = whole := hbw
        subst hb
        exact con_reassemble hle hpad
      · exact error_ne_ok h

end Rwv.Eidos

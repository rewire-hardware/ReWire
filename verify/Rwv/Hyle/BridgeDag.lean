/-
The DAG tier of the verified equivalence checker: a hash-consed node
store that scales the Rwv.Hyle.Bridge checker to the pairs whose tree
normal forms blow up (MiniISA, TinyISA, gfmult, sha256ffi).

Design: soundness is a REDUCTION to the tree checker. `symExpDag`
mirrors `symExp` raw-node-for-raw-node (hash-consing is invisible to
the tree reading), so a successful DAG run certifies a successful
`symExp` run whose result is the reading of the returned index —
without ever materializing the tree. Three store renormalization
passes then mirror `NF.cfoldW` constructor for constructor
(`read (m i) = NF.cfoldW (read i)`, syntactically), so final root
indices read to exactly the `cfoldW3`-images `checkEquivW` compares.
`checkEquivDag p₁ p₂ = true` therefore implies
`checkEquivW p₁ p₂ = true` (`checkEquivDag_toW`), and soundness is
`checkEquivW_sound` composed with that implication — every BV and
semantic argument stays in the committed Bridge proof. The rewrite-free
variant (`checkEquivDagRaw`) reduces to `checkEquiv` the same way and
inherits the UNCONDITIONAL theorem.

The store invariant (`Dag.WF`):
* `child_lt` — children strictly earlier, so the tree reading `read`
  is a total strong recursion on the index;
* `width_coh` — each node caches exactly `annWidth` of its reading,
  which lets the node-level rewrite guards mirror the tree-level
  `annWidth` guards in constant time;
* `coh`/`coh₂` — the hash-cons map is a two-sided inverse of the node
  array, giving canonicity (`WF.canon`: equal readings force equal
  indices), which aligns the node-level "same base" index test in
  adjacent-slice merging with the tree-level structural equality test.

The η tier: `DNode` mirrors Bridge's uninterpreted extern-call node as
`xcall w ext a` — one packed-argument child, built by `xpackD` (the
node-level `NF.xpack`, a left `mkCatD` fold from the empty literal)
under `mkXcallD`, whose `width_coh` obligation is free (`annWidth`
answers the cached width unconditionally, by the `xapply` clamp).
`symExpDag` carries the extern table `X` exactly as `symExp` does —
model-carrying calls and generic instantiations reject, sharing the
tree evaluator's messages — and the simulation extends node for node,
so `checkEquivDag`/`checkEquivDagRaw` inherit Bridge's ∀E run
equality. The renormalizer passes the node through, renormalizing only
the packed child, mirroring `cfoldW`. `xcallFreeIdx` is the store-side
`NF.xcallFree` (guard-else branches answer `true`, matching `read`'s
empty-literal reading), certified by `xcallFreeIdx_read` — the gate the
Cryptol splice-inlining mirror consumes. TRAP: `xcallFreeIdx`'s bare
recursion tree-unfolds the store's sharing (exponential on the SHA-256
splices — it was sha256ffi's 300s "timeout"); executable gates must use
the memoized `xcallFreeM` (one bottom-up table pass, `xcallFreeTab`),
which is pointwise EQUAL to it (`xcallFreeM_eq`), so proofs stay
stated against the spec form.

Per house style, the `Except`/`HashMap.ofList` helpers private to
Bridge are re-proved locally.
-/
import Rwv.Hyle.Bridge

namespace Rwv.Hyle.BridgeDag

open Std (HashMap)
open Rwv.Hyle
open Rwv.Hyle.Bridge

instance : Hashable BV := ⟨fun v => hash (v.width, v.bits.toNat)⟩

deriving instance Hashable for Op

/-! ## The node store -/

/-- A DAG node: `NF` one level deep, children as indices into the
store. Every constructor caches the `annWidth` of its tree reading
(`lit` carries it inside the literal), so rewrite guards read widths
in constant time. -/
inductive DNode where
  | var   (w : Nat) (x : String)
  | lit   (v : BV)
  | prim1 (w : Nat) (op : Op) (a : Nat)
  | prim2 (w : Nat) (op : Op) (a b : Nat)
  | cat   (w : Nat) (a b : Nat)
  | slice (i w : Nat) (e : Nat)
  | ite   (w : Nat) (c t e : Nat)
  | xcall (w : Nat) (ext : String) (a : Nat)
deriving DecidableEq, Hashable, Repr

namespace DNode

/-- The cached width. -/
def width : DNode → Nat
  | .var w _ => w
  | .lit v => v.width
  | .prim1 w _ _ => w
  | .prim2 w _ _ _ => w
  | .cat w _ _ => w
  | .slice _ w _ => w
  | .ite w _ _ _ => w
  | .xcall w _ _ => w

/-- Child indices. -/
def children : DNode → List Nat
  | .var _ _ | .lit _ => []
  | .prim1 _ _ a => [a]
  | .prim2 _ _ a b => [a, b]
  | .cat _ a b => [a, b]
  | .slice _ _ e => [e]
  | .ite _ c t e => [c, t, e]
  | .xcall _ _ a => [a]

/-- The one-level tree reading: children through `f`. The cached
width is dropped exactly where `NF` carries none. -/
def toNF (n : DNode) (f : Nat → NF) : NF :=
  match n with
  | .var w x => .var w x
  | .lit v => .lit v
  | .prim1 _ op a => .prim1 op (f a)
  | .prim2 _ op a b => .prim2 op (f a) (f b)
  | .cat _ a b => .cat (f a) (f b)
  | .slice i w e => .slice i w (f e)
  | .ite _ c t e => .ite (f c) (f t) (f e)
  | .xcall w ext a => .xcall w ext (f a)

theorem toNF_congr {n : DNode} {f g : Nat → NF} (h : ∀ j ∈ n.children, f j = g j) :
    n.toNF f = n.toNF g := by
  cases n <;> simp_all [toNF, children]

end DNode

/-- The node store: an append-only array of nodes plus the hash-cons
map. -/
structure Dag where
  nodes : Array DNode
  hmap  : HashMap DNode Nat

namespace Dag

def empty : Dag := ⟨#[], ∅⟩

def size (d : Dag) : Nat := d.nodes.size

/-- The cached width of an index (0 out of range — never consulted
there under `WF`). -/
def widthOf (d : Dag) (i : Nat) : Nat :=
  match d.nodes[i]? with
  | some n => n.width
  | none => 0

/-- The tree reading of an index: strong recursion on the index, with
out-of-range children (impossible under `WF.child_lt`) reading as the
empty literal. This is the specification device connecting the store
to Bridge's `NF` layer; the checker never executes it. -/
def read (d : Dag) (i : Nat) : NF :=
  match d.nodes[i]? with
  | some (.var w x) => .var w x
  | some (.lit v) => .lit v
  | some (.prim1 _ op a) =>
      .prim1 op (if _h : a < i then d.read a else .lit BV.nil)
  | some (.prim2 _ op a b) =>
      .prim2 op (if _h : a < i then d.read a else .lit BV.nil)
                (if _h : b < i then d.read b else .lit BV.nil)
  | some (.cat _ a b) =>
      .cat (if _h : a < i then d.read a else .lit BV.nil)
           (if _h : b < i then d.read b else .lit BV.nil)
  | some (.slice j w e) =>
      .slice j w (if _h : e < i then d.read e else .lit BV.nil)
  | some (.ite _ c t e) =>
      .ite (if _h : c < i then d.read c else .lit BV.nil)
           (if _h : t < i then d.read t else .lit BV.nil)
           (if _h : e < i then d.read e else .lit BV.nil)
  | some (.xcall w ext a) =>
      .xcall w ext (if _h : a < i then d.read a else .lit BV.nil)
  | none => .lit BV.nil
termination_by i

/-- Hash-consed node creation: an existing structurally identical node
is returned by index; otherwise the node is appended. -/
def push (d : Dag) (n : DNode) : Dag × Nat :=
  match d.hmap[n]? with
  | some j => (d, j)
  | none => (⟨d.nodes.push n, d.hmap.insert n d.nodes.size⟩, d.nodes.size)

/-- Store extension: `d₂` has `d₁` as a prefix. -/
structure Ext (d₁ d₂ : Dag) : Prop where
  size_le  : d₁.size ≤ d₂.size
  nodes_eq : ∀ i, i < d₁.size → d₂.nodes[i]? = d₁.nodes[i]?

theorem Ext.refl (d : Dag) : Ext d d := ⟨Nat.le_refl _, fun _ _ => rfl⟩

theorem Ext.trans {d₁ d₂ d₃ : Dag} (h₁ : Ext d₁ d₂) (h₂ : Ext d₂ d₃) : Ext d₁ d₃ :=
  ⟨Nat.le_trans h₁.size_le h₂.size_le,
   fun i hi => (h₂.nodes_eq i (Nat.lt_of_lt_of_le hi h₁.size_le)).trans (h₁.nodes_eq i hi)⟩

/-- The store invariant (see the header comment). -/
structure WF (d : Dag) : Prop where
  child_lt  : ∀ (i : Nat) (n : DNode), d.nodes[i]? = some n → ∀ j ∈ n.children, j < i
  width_coh : ∀ (i : Nat) (n : DNode), d.nodes[i]? = some n → annWidth (d.read i) = some n.width
  coh  : ∀ (n : DNode) (j : Nat), d.hmap[n]? = some j → d.nodes[j]? = some n
  coh₂ : ∀ (j : Nat) (n : DNode), d.nodes[j]? = some n → d.hmap[n]? = some j

theorem WF.empty : Dag.empty.WF := by
  refine ⟨?_, ?_, ?_, ?_⟩ <;> intro i n h <;> simp [Dag.empty] at h

/-- An in-range index holds a node. -/
theorem node_of_lt {d : Dag} {i : Nat} (h : i < d.size) : ∃ n, d.nodes[i]? = some n :=
  ⟨d.nodes[i], Array.getElem?_eq_getElem h⟩

/-- In-range-ness from a successful node lookup. -/
theorem lt_of_node {d : Dag} {i : Nat} {n : DNode} (h : d.nodes[i]? = some n) : i < d.size :=
  Array.getElem?_eq_some_iff.mp h |>.1

/-- One-step unfolding of the reading at a node whose children are in
guard range. -/
theorem read_eq {d : Dag} {i : Nat} {n : DNode}
    (hlt : ∀ j ∈ n.children, j < i) (h : d.nodes[i]? = some n) :
    d.read i = n.toNF d.read := by
  cases n with
  | var w x => rw [read, h]; rfl
  | lit v => rw [read, h]; rfl
  | prim1 w op a =>
      rw [read, h]
      show NF.prim1 op (if _h : a < i then d.read a else .lit BV.nil) = _
      rw [dif_pos (hlt a (by simp [DNode.children]))]
      rfl
  | prim2 w op a b =>
      rw [read, h]
      show NF.prim2 op (if _h : a < i then d.read a else .lit BV.nil)
        (if _h : b < i then d.read b else .lit BV.nil) = _
      rw [dif_pos (hlt a (by simp [DNode.children])),
          dif_pos (hlt b (by simp [DNode.children]))]
      rfl
  | cat w a b =>
      rw [read, h]
      show NF.cat (if _h : a < i then d.read a else .lit BV.nil)
        (if _h : b < i then d.read b else .lit BV.nil) = _
      rw [dif_pos (hlt a (by simp [DNode.children])),
          dif_pos (hlt b (by simp [DNode.children]))]
      rfl
  | slice j w e =>
      rw [read, h]
      show NF.slice j w (if _h : e < i then d.read e else .lit BV.nil) = _
      rw [dif_pos (hlt e (by simp [DNode.children]))]
      rfl
  | ite w c t e =>
      rw [read, h]
      show NF.ite (if _h : c < i then d.read c else .lit BV.nil)
        (if _h : t < i then d.read t else .lit BV.nil)
        (if _h : e < i then d.read e else .lit BV.nil) = _
      rw [dif_pos (hlt c (by simp [DNode.children])),
          dif_pos (hlt t (by simp [DNode.children])),
          dif_pos (hlt e (by simp [DNode.children]))]
      rfl
  | xcall w ext a =>
      rw [read, h]
      show NF.xcall w ext (if _h : a < i then d.read a else .lit BV.nil) = _
      rw [dif_pos (hlt a (by simp [DNode.children]))]
      rfl

/-- Readings of in-range indices are stable under extension. -/
theorem read_ext {d₁ d₂ : Dag} (hext : Ext d₁ d₂) :
    ∀ i, i < d₁.size → d₂.read i = d₁.read i := by
  intro i
  induction i using Nat.strongRecOn with
  | _ i ih =>
    intro hi
    rw [read, read, hext.nodes_eq i hi]
    cases h : d₁.nodes[i]? with
    | none => rfl
    | some n =>
        have hstep : ∀ j, j < i → (if _ : j < i then d₂.read j else .lit BV.nil)
            = (if _ : j < i then d₁.read j else .lit BV.nil) := by
          intro j hj
          rw [dif_pos hj, dif_pos hj, ih j hj (Nat.lt_trans hj hi)]
        have hstep' : ∀ j, (if _ : j < i then d₂.read j else .lit BV.nil)
            = (if _ : j < i then d₁.read j else .lit BV.nil) := by
          intro j
          by_cases hj : j < i
          · exact hstep j hj
          · rw [dif_neg hj, dif_neg hj]
        cases n <;> simp only [hstep']

/-- No uninterpreted extern node in the sub-dag of an index: the
decidable mirror of `NF.xcallFree` on the reading (the Cryptol
splice-inlining mirror gates on it). Guard-else and out-of-range
branches answer `true`, matching `read`'s `.lit BV.nil` reading
there. -/
def xcallFreeIdx (d : Dag) (i : Nat) : Bool :=
  match d.nodes[i]? with
  | some (.var _ _) => true
  | some (.lit _) => true
  | some (.prim1 _ _ a) => if _h : a < i then d.xcallFreeIdx a else true
  | some (.prim2 _ _ a b) =>
      (if _h : a < i then d.xcallFreeIdx a else true)
        && (if _h : b < i then d.xcallFreeIdx b else true)
  | some (.cat _ a b) =>
      (if _h : a < i then d.xcallFreeIdx a else true)
        && (if _h : b < i then d.xcallFreeIdx b else true)
  | some (.slice _ _ e) => if _h : e < i then d.xcallFreeIdx e else true
  | some (.ite _ c t e) =>
      (if _h : c < i then d.xcallFreeIdx c else true)
        && (if _h : t < i then d.xcallFreeIdx t else true)
        && (if _h : e < i then d.xcallFreeIdx e else true)
  | some (.xcall _ _ _) => false
  | none => true
termination_by i

/-- A `true` `xcallFreeIdx` verdict certifies `NF.xcallFree` of the
reading. -/
theorem xcallFreeIdx_read {d : Dag} :
    ∀ i, d.xcallFreeIdx i = true → (d.read i).xcallFree = true := by
  intro i
  induction i using Nat.strongRecOn with
  | _ i ih =>
    intro h
    rw [xcallFreeIdx] at h
    rw [read]
    cases hn : d.nodes[i]? with
    | none => rfl
    | some n =>
        rw [hn] at h
        have step : ∀ j, (if _h : j < i then d.xcallFreeIdx j else true) = true →
            ((if _h : j < i then d.read j else .lit BV.nil)).xcallFree = true := by
          intro j hj
          by_cases hji : j < i
          · rw [dif_pos hji] at hj ⊢
            exact ih j hji hj
          · rw [dif_neg hji]
            rfl
        cases n with
        | var w x => rfl
        | lit v => rfl
        | prim1 w op a =>
            dsimp only at h
            exact step a h
        | prim2 w op a b =>
            dsimp only at h
            rw [Bool.and_eq_true] at h
            show ((if _h : a < i then d.read a else .lit BV.nil).xcallFree
              && (if _h : b < i then d.read b else .lit BV.nil).xcallFree) = true
            rw [Bool.and_eq_true]
            exact ⟨step a h.1, step b h.2⟩
        | cat w a b =>
            dsimp only at h
            rw [Bool.and_eq_true] at h
            show ((if _h : a < i then d.read a else .lit BV.nil).xcallFree
              && (if _h : b < i then d.read b else .lit BV.nil).xcallFree) = true
            rw [Bool.and_eq_true]
            exact ⟨step a h.1, step b h.2⟩
        | slice j w e =>
            dsimp only at h
            exact step e h
        | ite w c t e =>
            dsimp only at h
            rw [Bool.and_eq_true, Bool.and_eq_true] at h
            show ((if _h : c < i then d.read c else .lit BV.nil).xcallFree
              && (if _h : t < i then d.read t else .lit BV.nil).xcallFree
              && (if _h : e < i then d.read e else .lit BV.nil).xcallFree) = true
            rw [Bool.and_eq_true, Bool.and_eq_true]
            exact ⟨⟨step c h.1.1, step t h.1.2⟩, step e h.2⟩
        | xcall w ext a =>
            dsimp only at h
            exact absurd h (by simp)

/-- One row of the memoized `xcallFreeIdx` table: the verdict for the
node at index `i`, reading children's verdicts from the accumulated
prefix table (children outside the table answer `true`, matching the
recursion's out-of-range arms). -/
def xcallFreeStep (d : Dag) (i : Nat) (acc : Array Bool) : Bool :=
  match d.nodes[i]? with
  | some (.var _ _) => true
  | some (.lit _) => true
  | some (.prim1 _ _ a) => if _h : a < acc.size then acc[a] else true
  | some (.prim2 _ _ a b) =>
      (if _h : a < acc.size then acc[a] else true)
        && (if _h : b < acc.size then acc[b] else true)
  | some (.cat _ a b) =>
      (if _h : a < acc.size then acc[a] else true)
        && (if _h : b < acc.size then acc[b] else true)
  | some (.slice _ _ e) => if _h : e < acc.size then acc[e] else true
  | some (.ite _ c t e) =>
      (if _h : c < acc.size then acc[c] else true)
        && (if _h : t < acc.size then acc[t] else true)
        && (if _h : e < acc.size then acc[e] else true)
  | some (.xcall _ _ _) => false
  | none => true

/-- The bottom-up `xcallFreeIdx` table over the store prefix `[0, n)`:
entry `j` is `d.xcallFreeIdx j` (`xcallFreeTab_get`). The bare
recursion re-walks shared children — exponential on stores with heavy
sharing (the SHA-256 splice) — while this pass is linear. -/
def xcallFreeTab (d : Dag) : Nat → Array Bool
  | 0 => #[]
  | i + 1 =>
      let acc := d.xcallFreeTab i
      acc.push (d.xcallFreeStep i acc)

theorem xcallFreeTab_size (d : Dag) : ∀ n, (d.xcallFreeTab n).size = n := by
  intro n
  induction n with
  | zero => rfl
  | succ i ih =>
      show ((d.xcallFreeTab i).push (d.xcallFreeStep i (d.xcallFreeTab i))).size = i + 1
      rw [Array.size_push, ih]

/-- A row computed against a faithful prefix table is the recursion's
verdict. -/
theorem xcallFreeStep_eq (d : Dag) {i : Nat} {acc : Array Bool} (hsz : acc.size = i)
    (hget : ∀ j (hj : j < acc.size), acc[j] = d.xcallFreeIdx j) :
    d.xcallFreeStep i acc = d.xcallFreeIdx i := by
  subst hsz
  have step : ∀ a, (if _h : a < acc.size then acc[a] else true)
      = (if _h : a < acc.size then d.xcallFreeIdx a else true) := by
    intro a
    by_cases h : a < acc.size
    · rw [dif_pos h, dif_pos h]
      exact hget a h
    · rw [dif_neg h, dif_neg h]
  rw [xcallFreeStep, xcallFreeIdx]
  cases hn : d.nodes[acc.size]? with
  | none => rfl
  | some n =>
      cases n with
      | var w x => rfl
      | lit v => rfl
      | prim1 w op a => exact step a
      | prim2 w op a b => dsimp only; rw [step a, step b]
      | cat w a b => dsimp only; rw [step a, step b]
      | slice j w e => exact step e
      | ite w c t e => dsimp only; rw [step c, step t, step e]
      | xcall w ext a => rfl

/-- Every table entry is the recursion's verdict. -/
theorem xcallFreeTab_get (d : Dag) : ∀ n j (hj : j < (d.xcallFreeTab n).size),
    (d.xcallFreeTab n)[j] = d.xcallFreeIdx j := by
  intro n
  induction n with
  | zero => intro j hj; exact absurd hj (by simp [xcallFreeTab])
  | succ i ih =>
      intro j hj
      have hsz := d.xcallFreeTab_size i
      have hj' : j < ((d.xcallFreeTab i).push (d.xcallFreeStep i (d.xcallFreeTab i))).size := hj
      show ((d.xcallFreeTab i).push (d.xcallFreeStep i (d.xcallFreeTab i)))[j]'hj'
        = d.xcallFreeIdx j
      by_cases hji : j < (d.xcallFreeTab i).size
      · rw [Array.getElem_push_lt hji]
        exact ih j hji
      · have hje : j = (d.xcallFreeTab i).size := by
          rw [Array.size_push] at hj'
          omega
        subst hje
        rw [Array.getElem_push_eq, hsz]
        exact xcallFreeStep_eq d hsz ih

/-- Memoized `xcallFreeIdx`: build the bottom-up table below the root,
then read the root's row — linear where the bare recursion
tree-unfolds the sharing. Equal to `xcallFreeIdx` (`xcallFreeM_eq`);
the executable gates use this form. -/
def xcallFreeM (d : Dag) (i : Nat) : Bool :=
  (d.xcallFreeTab (i + 1))[i]'(by rw [xcallFreeTab_size]; omega)

theorem xcallFreeM_eq (d : Dag) (i : Nat) : d.xcallFreeM i = d.xcallFreeIdx i :=
  xcallFreeTab_get d (i + 1) i (by rw [xcallFreeTab_size]; omega)

/-- The cached width is the reading's `annWidth`, in range. -/
theorem widthOf_eq {d : Dag} (hwf : d.WF) {i : Nat} (hi : i < d.size) :
    annWidth (d.read i) = some (d.widthOf i) := by
  obtain ⟨n, hn⟩ := node_of_lt hi
  rw [widthOf, hn]
  exact hwf.width_coh i n hn

/-- The push specification: the store extends, stays well-formed
(given the pushed node's children are in range and its cached width is
its reading's `annWidth`), and the returned index reads as the pushed
node over the OLD store's readings. -/
theorem push_spec {d : Dag} {n : DNode} {d' : Dag} {r : Nat} (hwf : d.WF)
    (hch : ∀ j ∈ n.children, j < d.size)
    (hw : annWidth (n.toNF d.read) = some n.width)
    (h : d.push n = (d', r)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧ d'.read r = n.toNF d.read := by
  rw [push] at h
  cases hget : d.hmap[n]? with
  | some j =>
      rw [hget] at h
      have h₁ : d = d' := congrArg Prod.fst h
      have h₂ : j = r := congrArg Prod.snd h
      rw [← h₁, ← h₂]
      have hnode := hwf.coh n j hget
      exact ⟨hwf, Ext.refl _, lt_of_node hnode,
        read_eq (hwf.child_lt j n hnode) hnode⟩
  | none =>
      rw [hget] at h
      have h₁ : (⟨d.nodes.push n, d.hmap.insert n d.nodes.size⟩ : Dag) = d' :=
        congrArg Prod.fst h
      have h₂ : d.nodes.size = r := congrArg Prod.snd h
      have hsz : d'.size = d.size + 1 := by
        rw [← h₁]
        show (d.nodes.push n).size = d.nodes.size + 1
        simp
      have hext : d.Ext d' := by
        refine ⟨by omega, ?_⟩
        intro i hi
        rw [← h₁]
        exact (Array.getElem?_push_lt hi).trans (Array.getElem?_eq_getElem hi).symm
      have hnew : d'.nodes[d.size]? = some n := by
        rw [← h₁]
        show (d.nodes.push n)[d.nodes.size]? = some n
        simp
      have hold : ∀ i, i < d.size → d'.nodes[i]? = d.nodes[i]? := hext.nodes_eq
      have hread : d'.read d.size = n.toNF d.read := by
        have hr1 : d'.read d.size = n.toNF d'.read :=
          read_eq (fun j hj => hch j hj) hnew
        rw [hr1]
        exact DNode.toNF_congr fun j hj => read_ext hext j (hch j hj)
      have hreads : ∀ i, i < d.size → d'.read i = d.read i := read_ext hext
      have hmapget : ∀ m : DNode, d'.hmap[m]? =
          if m = n then some d.size else d.hmap[m]? := by
        intro m
        rw [← h₁]
        show (d.hmap.insert n d.nodes.size)[m]? = _
        rw [HashMap.getElem?_insert]
        by_cases hm : m = n
        · subst hm
          simp [Dag.size]
        · rw [if_neg hm, if_neg (by simp only [beq_iff_eq]; exact fun hc => hm hc.symm)]
      have hr : r < d'.size := by
        rw [← h₂, hsz]
        show d.size < d.size + 1
        omega
      have hread' : d'.read r = n.toNF d.read := by
        rw [← h₂]
        exact hread
      refine ⟨⟨?_, ?_, ?_, ?_⟩, hext, hr, hread'⟩
      · -- child_lt
        intro i m hm j hj
        by_cases hi : i < d.size
        · rw [hold i hi] at hm
          exact hwf.child_lt i m hm j hj
        · have hieq : i = d.size := by
            have := lt_of_node hm
            rw [hsz] at this
            omega
          subst hieq
          rw [hnew] at hm
          injection hm with hm
          subst hm
          exact hch j hj
      · -- width_coh
        intro i m hm
        by_cases hi : i < d.size
        · rw [hold i hi] at hm
          rw [hreads i hi]
          exact hwf.width_coh i m hm
        · have hieq : i = d.size := by
            have := lt_of_node hm
            rw [hsz] at this
            omega
          subst hieq
          rw [hnew] at hm
          injection hm with hm
          subst hm
          rw [hread]
          exact hw
      · -- coh
        intro m j hj
        rw [hmapget] at hj
        by_cases hm : m = n
        · subst hm
          rw [if_pos rfl] at hj
          injection hj with hj
          subst hj
          exact hnew
        · rw [if_neg hm] at hj
          have := hwf.coh m j hj
          rw [hold j (lt_of_node this)]
          exact this
      · -- coh₂
        intro j m hm
        rw [hmapget]
        by_cases hj : j < d.size
        · rw [hold j hj] at hm
          have := hwf.coh₂ j m hm
          rw [if_neg ?_]
          · exact this
          · intro hc
            subst hc
            rw [hget] at this
            exact absurd this (by simp)
        · have hjeq : j = d.size := by
            have := lt_of_node hm
            rw [hsz] at this
            omega
          subst hjeq
          rw [hnew] at hm
          injection hm with hm
          subst hm
          rw [if_pos rfl]

/-- Canonicity: under the two-sided hash-cons coherence, equal
readings force equal indices. This is what aligns node-level index
guards with Bridge's tree-level structural guards. -/
theorem WF.canon {d : Dag} (hwf : d.WF) :
    ∀ i j, i < d.size → j < d.size → d.read i = d.read j → i = j := by
  suffices h : ∀ k i j, i + j ≤ k → i < d.size → j < d.size → d.read i = d.read j → i = j by
    intro i j
    exact h (i + j) i j (Nat.le_refl _)
  intro k
  induction k using Nat.strongRecOn with
  | _ k ih =>
    intro i j hk hi hj heq
    obtain ⟨ni, hni⟩ := node_of_lt hi
    obtain ⟨nj, hnj⟩ := node_of_lt hj
    have hci := hwf.child_lt i ni hni
    have hcj := hwf.child_lt j nj hnj
    rw [read_eq hci hni, read_eq hcj hnj] at heq
    -- a same-index conclusion via the hash-cons map, once the nodes agree
    have finish : ni = nj → i = j := by
      intro hn
      have h₁ := hwf.coh₂ i ni hni
      have h₂ := hwf.coh₂ j nj hnj
      rw [hn] at h₁
      exact Option.some_inj.mp (h₁.symm.trans h₂)
    -- widths agree via width_coh
    have hwidth : ni.width = nj.width := by
      have h₁ := hwf.width_coh i ni hni
      have h₂ := hwf.width_coh j nj hnj
      rw [read_eq hci hni] at h₁
      rw [read_eq hcj hnj] at h₂
      rw [heq] at h₁
      exact Option.some_inj.mp (h₁.symm.trans h₂)
    -- child equality via the inductive hypothesis
    have child : ∀ a b, a ∈ ni.children → b ∈ nj.children →
        d.read a = d.read b → a = b := by
      intro a b ha hb hab
      have ha' := hci a ha
      have hb' := hcj b hb
      exact ih (a + b) (by omega) a b (Nat.le_refl _)
        (Nat.lt_trans ha' hi) (Nat.lt_trans hb' hj) hab
    apply finish
    cases ni <;> cases nj <;>
      simp only [DNode.toNF, NF.var.injEq, NF.lit.injEq, NF.prim1.injEq, NF.prim2.injEq,
        NF.cat.injEq, NF.slice.injEq, NF.ite.injEq, NF.xcall.injEq, DNode.width, reduceCtorEq]
        at heq hwidth <;>
      first
      -- lit: the literal is the whole node
      | (rw [heq])
      -- var: width and name carried by the reading
      | (rw [heq.1, heq.2])
      -- slice: both statics carried by the reading, one child
      | (rw [heq.1, heq.2.1,
             child _ _ (by simp [DNode.children]) (by simp [DNode.children]) heq.2.2])
      -- prim1: cached width via hwidth, op and one child from the reading
      | (rw [hwidth, heq.1,
             child _ _ (by simp [DNode.children]) (by simp [DNode.children]) heq.2])
      -- prim2: cached width, op, two children
      | (rw [hwidth, heq.1,
             child _ _ (by simp [DNode.children]) (by simp [DNode.children]) heq.2.1,
             child _ _ (by simp [DNode.children]) (by simp [DNode.children]) heq.2.2])
      -- cat: cached width, two children
      | (rw [hwidth,
             child _ _ (by simp [DNode.children]) (by simp [DNode.children]) heq.1,
             child _ _ (by simp [DNode.children]) (by simp [DNode.children]) heq.2])
      -- ite: cached width, three children
      | (rw [hwidth,
             child _ _ (by simp [DNode.children]) (by simp [DNode.children]) heq.1,
             child _ _ (by simp [DNode.children]) (by simp [DNode.children]) heq.2.1,
             child _ _ (by simp [DNode.children]) (by simp [DNode.children]) heq.2.2])


end Dag

/-! ## Constructors

Every constructor satisfies the same contract `Mk`: the store extends
and stays well-formed, the result is in range, and it READS as a
prescribed tree — for the rewrite constructors, exactly the
corresponding Bridge tree constructor applied to the arguments'
readings. All rewrite guards consult cached widths, aligned with the
tree-level `annWidth` guards by `width_coh`. -/

/-- The constructor contract. -/
def Mk (d : Dag) (res : Dag × Nat) (nf : NF) : Prop :=
  res.1.WF ∧ d.Ext res.1 ∧ res.2 < res.1.size ∧ res.1.read res.2 = nf

namespace Mk

theorem wf {d : Dag} {res : Dag × Nat} {nf : NF} (h : Mk d res nf) : res.1.WF := h.1
theorem ext {d : Dag} {res : Dag × Nat} {nf : NF} (h : Mk d res nf) : d.Ext res.1 := h.2.1
theorem lt {d : Dag} {res : Dag × Nat} {nf : NF} (h : Mk d res nf) : res.2 < res.1.size := h.2.2.1
theorem read {d : Dag} {res : Dag × Nat} {nf : NF} (h : Mk d res nf) : res.1.read res.2 = nf :=
  h.2.2.2

/-- Rephrase the reading. -/
theorem cast {d : Dag} {res : Dag × Nat} {nf nf' : NF} (h : Mk d res nf) (he : nf = nf') :
    Mk d res nf' := he ▸ h

end Mk

namespace Dag

/-- `push`, in contract form. -/
theorem push_mk {d : Dag} {n : DNode} (hwf : d.WF)
    (hch : ∀ j ∈ n.children, j < d.size)
    (hw : annWidth (n.toNF d.read) = some n.width) :
    Mk d (d.push n) (n.toNF d.read) := by
  rcases hp : d.push n with ⟨d', r⟩
  exact push_spec hwf hch hw hp

/-- An existing index, in contract form. -/
theorem self_mk {d : Dag} (hwf : d.WF) {r : Nat} (hr : r < d.size) : Mk d (d, r) (d.read r) :=
  ⟨hwf, Ext.refl d, hr, rfl⟩

private theorem no_children {j : Nat} {l : List Nat} (hl : l = []) (h : j ∈ l) : False := by
  subst hl; simp at h

private theorem mem1 {j a : Nat} (h : j ∈ [a]) : j = a := by simpa using h

private theorem mem2 {j a b : Nat} (h : j ∈ [a, b]) : j = a ∨ j = b := by simpa using h

private theorem mem3 {j a b c : Nat} (h : j ∈ [a, b, c]) : j = a ∨ j = b ∨ j = c := by
  simpa using h

/-! ### Raw constructors (the `symExpDag` alphabet) -/

def mkLit (d : Dag) (v : BV) : Dag × Nat := d.push (.lit v)

theorem mkLit_spec {d : Dag} (hwf : d.WF) (v : BV) : Mk d (d.mkLit v) (.lit v) :=
  push_mk hwf (fun _ hj => absurd hj (by simp [DNode.children])) rfl

def mkVar (d : Dag) (w : Nat) (x : String) : Dag × Nat := d.push (.var w x)

theorem mkVar_spec {d : Dag} (hwf : d.WF) (w : Nat) (x : String) :
    Mk d (d.mkVar w x) (.var w x) :=
  push_mk hwf (fun _ hj => absurd hj (by simp [DNode.children])) rfl

def rawCat (d : Dag) (a b : Nat) : Dag × Nat :=
  d.push (.cat (d.widthOf a + d.widthOf b) a b)

theorem rawCat_spec {d : Dag} (hwf : d.WF) {a b : Nat} (ha : a < d.size) (hb : b < d.size) :
    Mk d (d.rawCat a b) (.cat (d.read a) (d.read b)) := by
  refine push_mk hwf ?_ ?_
  · intro j hj
    rcases mem2 hj with h | h <;> (subst h; assumption)
  · show annWidth (.cat (d.read a) (d.read b)) = some (d.widthOf a + d.widthOf b)
    simp only [annWidth, widthOf_eq hwf ha, widthOf_eq hwf hb]

def rawSlice (d : Dag) (i w e : Nat) : Dag × Nat := d.push (.slice i w e)

theorem rawSlice_spec {d : Dag} (hwf : d.WF) {e : Nat} (i w : Nat) (he : e < d.size) :
    Mk d (d.rawSlice i w e) (.slice i w (d.read e)) := by
  refine push_mk hwf ?_ rfl
  intro j hj
  rw [mem1 hj]
  exact he

def rawPrim1 (d : Dag) (op : Op) (a : Nat) : Dag × Nat :=
  d.push (.prim1 (opWidth1 op (d.widthOf a)) op a)

theorem rawPrim1_spec {d : Dag} (hwf : d.WF) {op : Op} {a : Nat} (ha : a < d.size)
    (hop : opArity op = 1) :
    Mk d (d.rawPrim1 op a) (.prim1 op (d.read a)) := by
  refine push_mk hwf ?_ ?_
  · intro j hj
    rw [mem1 hj]
    exact ha
  · show annWidth (.prim1 op (d.read a)) = some (opWidth1 op (d.widthOf a))
    simp only [annWidth, if_pos hop, widthOf_eq hwf ha, Option.map_some]

def rawPrim2 (d : Dag) (op : Op) (a b : Nat) : Dag × Nat :=
  d.push (.prim2 (opWidth2 op (d.widthOf a)) op a b)

theorem rawPrim2_spec {d : Dag} (hwf : d.WF) {op : Op} {a b : Nat} (ha : a < d.size)
    (hb : b < d.size) (hop : opArity op = 2) :
    Mk d (d.rawPrim2 op a b) (.prim2 op (d.read a) (d.read b)) := by
  refine push_mk hwf ?_ ?_
  · intro j hj
    rcases mem2 hj with h | h <;> (subst h; assumption)
  · show annWidth (.prim2 op (d.read a) (d.read b)) = some (opWidth2 op (d.widthOf a))
    simp only [annWidth, if_pos hop, widthOf_eq hwf ha, Option.map_some]

def rawIte (d : Dag) (c t e : Nat) : Dag × Nat :=
  d.push (.ite (d.widthOf t) c t e)

theorem rawIte_spec {d : Dag} (hwf : d.WF) {c t e : Nat} (hc : c < d.size) (ht : t < d.size)
    (he : e < d.size) (harm : d.widthOf t = d.widthOf e) :
    Mk d (d.rawIte c t e) (.ite (d.read c) (d.read t) (d.read e)) := by
  refine push_mk hwf ?_ ?_
  · intro j hj
    rcases mem3 hj with h | h | h <;> (subst h; assumption)
  · show annWidth (.ite (d.read c) (d.read t) (d.read e)) = some (d.widthOf t)
    simp only [annWidth, widthOf_eq hwf ht, widthOf_eq hwf he, ← harm]
    simp

/-- The uninterpreted extern-call node for a MODEL-LESS extern: the
cached width is `annWidth`'s unconditional answer, so `width_coh` is
free. Serves both the raw alphabet and the renormalizer (`cfoldW`
passes the node through, recursing only into the packed argument). -/
def mkXcallD (d : Dag) (w : Nat) (ext : String) (a : Nat) : Dag × Nat :=
  d.push (.xcall w ext a)

theorem mkXcallD_spec {d : Dag} (hwf : d.WF) {w : Nat} {ext : String} {a : Nat}
    (ha : a < d.size) :
    Mk d (d.mkXcallD w ext a) (.xcall w ext (d.read a)) := by
  refine push_mk hwf ?_ rfl
  intro j hj
  rw [mem1 hj]
  exact ha

/-- The packed-argument concatenation, node-level: mirrors `NF.xpack`
(a left fold of `cat` from the empty literal) reading for reading. -/
def xpackD (d : Dag) (ns : List Nat) : Dag × Nat :=
  ns.foldl (fun acc n => acc.1.rawCat acc.2 n) (d.mkLit BV.nil)

private theorem xpackD_go {d : Dag} :
    ∀ (ns : List Nat) (dacc : Dag) (racc : Nat) (nf : NF),
      dacc.WF → d.Ext dacc → racc < dacc.size → dacc.read racc = nf →
      (∀ n ∈ ns, n < d.size) →
      Mk dacc (ns.foldl (fun acc n => acc.1.rawCat acc.2 n) (dacc, racc))
        ((ns.map d.read).foldl .cat nf) := by
  intro ns
  induction ns with
  | nil =>
      intro dacc racc nf hwf hext hr hread hns
      rw [List.foldl_nil, List.map_nil, List.foldl_nil, ← hread]
      exact self_mk hwf hr
  | cons n ns ih =>
      intro dacc racc nf hwf hext hr hread hns
      rw [List.foldl_cons, List.map_cons, List.foldl_cons]
      have hn : n < dacc.size :=
        Nat.lt_of_lt_of_le (hns n List.mem_cons_self) hext.size_le
      have M := rawCat_spec hwf hr hn
      have hcast : dacc.read racc = nf := hread
      have hreads : (dacc.rawCat racc n).1.read (dacc.rawCat racc n).2
          = NF.cat nf (d.read n) := by
        rw [M.read, hcast, read_ext hext n (hns n List.mem_cons_self)]
      have := ih (dacc.rawCat racc n).1 (dacc.rawCat racc n).2 (.cat nf (d.read n))
        M.wf (hext.trans M.ext) M.lt hreads
        (fun q hq => hns q (List.mem_cons_of_mem _ hq))
      exact ⟨this.wf, M.ext.trans this.ext, this.lt, this.read⟩

theorem xpackD_spec {d : Dag} (hwf : d.WF) {ns : List Nat}
    (hns : ∀ n ∈ ns, n < d.size) :
    Mk d (d.xpackD ns) (.xpack (ns.map d.read)) := by
  have M := mkLit_spec hwf BV.nil
  have hgo := xpackD_go ns (d.mkLit BV.nil).1 (d.mkLit BV.nil).2 (.lit BV.nil)
    M.wf M.ext M.lt M.read hns
  exact ⟨hgo.wf, M.ext.trans hgo.ext, hgo.lt, hgo.read⟩

/-! ### The `cfoldW` rewrite constructors -/

/-- Local port of Bridge's `mk1W` unfoldings (its per-case content is
inside a private proof there). -/
private theorem mk1W_ne_not {op : Op} (h : op ≠ .not) (a : NF) :
    NF.mk1W op a = NF.mk1 op a := by
  cases op <;> first | exact absurd rfl h | rfl

private theorem mk1W_not_default (a : NF) (h : ∀ b, a ≠ NF.prim1 .not b) :
    NF.mk1W .not a = NF.mk1 .not a := by
  cases a with
  | prim1 op b => cases op <;> first | exact absurd rfl (h b) | rfl
  | _ => rfl

/-- `mk1W`, node-level: double negation returns the grandchild index;
literal operands fold through `Sem.evalOp`. -/
def mk1D (d : Dag) (op : Op) (a : Nat) : Dag × Nat :=
  match d.nodes[a]? with
  | some (.lit v) =>
      (match Sem.evalOp op [v] with
      | .ok r => d.push (.lit r)
      | .error _ => d.rawPrim1 op a)
  | some (.prim1 _ op' b) =>
      if op = .not ∧ op' = .not then (d, b) else d.rawPrim1 op a
  | _ => d.rawPrim1 op a

theorem mk1D_spec {d : Dag} (hwf : d.WF) {op : Op} {a : Nat} (ha : a < d.size)
    (hop : opArity op = 1) :
    Mk d (d.mk1D op a) (NF.mk1W op (d.read a)) := by
  obtain ⟨na, hna⟩ := node_of_lt ha
  have hra : d.read a = na.toNF d.read := read_eq (hwf.child_lt a na hna) hna
  rw [mk1D, hna]
  cases na with
  | lit v =>
      have hra' : d.read a = NF.lit v := hra
      show Mk d (match Sem.evalOp op [v] with
                 | .ok r => d.push (.lit r)
                 | .error _ => d.rawPrim1 op a) (NF.mk1W op (d.read a))
      have hWa : NF.mk1W op (d.read a) = NF.mk1 op (.lit v) := by
        rw [hra']
        by_cases hn : op = .not
        · subst hn; rfl
        · exact mk1W_ne_not hn _
      rw [hWa]
      cases hev : Sem.evalOp op [v] with
      | ok r =>
          have hfold : NF.mk1 op (.lit v) = .lit r := by
            simp only [NF.mk1, hev]
          rw [hfold]
          exact mkLit_spec hwf r
      | error e =>
          have hfold : NF.mk1 op (.lit v) = .prim1 op (.lit v) := by
            simp only [NF.mk1, hev]
          rw [hfold, ← hra']
          exact rawPrim1_spec hwf ha hop
  | prim1 w' op' b =>
      show Mk d (if op = .not ∧ op' = .not then (d, b) else d.rawPrim1 op a) _
      by_cases hnn : op = .not ∧ op' = .not
      · rw [if_pos hnn]
        obtain ⟨h1, h2⟩ := hnn
        subst h1; subst h2
        have hb : b < d.size :=
          Nat.lt_trans (hwf.child_lt a _ hna b (by simp [DNode.children])) ha
        refine (self_mk hwf hb).cast ?_
        rw [hra]
        rfl
      · rw [if_neg hnn]
        refine (rawPrim1_spec hwf ha hop).cast ?_
        rw [hra]
        show NF.prim1 op (NF.prim1 op' (d.read b)) = NF.mk1W op (NF.prim1 op' (d.read b))
        by_cases hn : op = .not
        · subst hn
          have hn' : op' ≠ .not := fun hc => hnn ⟨rfl, hc⟩
          rw [mk1W_not_default _ (fun c hc => hn' (by injection hc))]
          rfl
        · rw [mk1W_ne_not hn]
          rfl
  | var w x =>
      refine (rawPrim1_spec hwf ha hop).cast ?_
      rw [hra]
      show NF.prim1 op (NF.var w x) = NF.mk1W op (NF.var w x)
      by_cases hn : op = .not
      · subst hn; rfl
      · rw [mk1W_ne_not hn]; rfl
  | prim2 w' op' b c =>
      refine (rawPrim1_spec hwf ha hop).cast ?_
      rw [hra]
      show NF.prim1 op (NF.prim2 op' (d.read b) (d.read c)) = _
      by_cases hn : op = .not
      · subst hn; rfl
      · rw [mk1W_ne_not hn]; rfl
  | cat w' b c =>
      refine (rawPrim1_spec hwf ha hop).cast ?_
      rw [hra]
      show NF.prim1 op (NF.cat (d.read b) (d.read c)) = _
      by_cases hn : op = .not
      · subst hn; rfl
      · rw [mk1W_ne_not hn]; rfl
  | slice j w' e =>
      refine (rawPrim1_spec hwf ha hop).cast ?_
      rw [hra]
      show NF.prim1 op (NF.slice j w' (d.read e)) = _
      by_cases hn : op = .not
      · subst hn; rfl
      · rw [mk1W_ne_not hn]; rfl
  | ite w' c t e =>
      refine (rawPrim1_spec hwf ha hop).cast ?_
      rw [hra]
      show NF.prim1 op (NF.ite (d.read c) (d.read t) (d.read e)) = _
      by_cases hn : op = .not
      · subst hn; rfl
      · rw [mk1W_ne_not hn]; rfl
  | xcall w' ext c =>
      refine (rawPrim1_spec hwf ha hop).cast ?_
      rw [hra]
      show NF.prim1 op (NF.xcall w' ext (d.read c)) = _
      by_cases hn : op = .not
      · subst hn; rfl
      · rw [mk1W_ne_not hn]; rfl


end Dag

/-! ### `mk2W`, node-level -/

/-- Local unfoldings of Bridge's `mk2`/`mk2W` overlapping matches
(robust against matcher column order by full case splits). -/
private theorem mk2_default {op : Op} {X Y : NF}
    (h : (∀ v, X ≠ .lit v) ∨ (∀ u, Y ≠ .lit u)) :
    NF.mk2 op X Y = .prim2 op X Y := by
  cases X <;> cases Y <;>
    first
    | (rcases h with h | h <;> exact absurd rfl (h _))
    | rfl

private theorem mk2W_umod_lit (X : NF) (u : BV) :
    NF.mk2W .umod X (.lit u)
      = if u.nat = 0 then X else NF.mk2 .umod X (.lit u) := by
  cases X <;> rfl

private theorem mk2W_umod_default (X Y : NF) (h : ∀ u, Y ≠ .lit u) :
    NF.mk2W .umod X Y = NF.mk2 .umod X Y := by
  cases X <;> cases Y <;> first | exact absurd rfl (h _) | rfl

private theorem mk2W_eq_litR (X : NF) (u : BV) :
    NF.mk2W .eq X (.lit u)
      = if annWidth X = some 1 ∧ u = ⟨1, 1#1⟩ then X
        else if annWidth X = some 1 ∧ u = ⟨1, 0#1⟩ then NF.mk1W .not X
        else NF.mk2 .eq X (.lit u) := by
  cases X <;> rfl

private theorem mk2W_eq_litL (Y : NF) (u : BV) (h : ∀ w, Y ≠ .lit w) :
    NF.mk2W .eq (.lit u) Y
      = if annWidth Y = some 1 ∧ u = ⟨1, 1#1⟩ then Y
        else if annWidth Y = some 1 ∧ u = ⟨1, 0#1⟩ then NF.mk1W .not Y
        else NF.mk2 .eq (.lit u) Y := by
  cases Y <;> first | exact absurd rfl (h _) | rfl

private theorem mk2W_eq_default (X Y : NF) (hX : ∀ u, X ≠ .lit u) (hY : ∀ u, Y ≠ .lit u) :
    NF.mk2W .eq X Y = NF.mk2 .eq X Y := by
  cases X <;> cases Y <;>
    first
    | exact absurd rfl (hX _)
    | exact absurd rfl (hY _)
    | rfl

private theorem mk2W_other {op : Op} (h1 : op ≠ .umod) (h2 : op ≠ .eq) (X Y : NF) :
    NF.mk2W op X Y = NF.mk2 op X Y := by
  cases op <;>
    first
    | exact absurd rfl h1
    | exact absurd rfl h2
    | rfl
    | (cases X <;> cases Y <;> rfl)

namespace Dag

/-- `mk2`, node-level: all-literal operands fold through `Sem.evalOp`. -/
def mk2fold (d : Dag) (op : Op) (a b : Nat) : Dag × Nat :=
  match d.nodes[a]?, d.nodes[b]? with
  | some (.lit v), some (.lit u) =>
      (match Sem.evalOp op [v, u] with
      | .ok r => d.push (.lit r)
      | .error _ => d.rawPrim2 op a b)
  | _, _ => d.rawPrim2 op a b

theorem mk2fold_spec {d : Dag} (hwf : d.WF) {op : Op} {a b : Nat} (ha : a < d.size)
    (hb : b < d.size) (hop : opArity op = 2) :
    Mk d (d.mk2fold op a b) (NF.mk2 op (d.read a) (d.read b)) := by
  obtain ⟨na, hna⟩ := node_of_lt ha
  obtain ⟨nb, hnb⟩ := node_of_lt hb
  have hra : d.read a = na.toNF d.read := read_eq (hwf.child_lt a na hna) hna
  have hrb : d.read b = nb.toNF d.read := read_eq (hwf.child_lt b nb hnb) hnb
  rw [mk2fold, hna, hnb]
  cases na <;> cases nb <;>
    first
    | (rename_i v u
       have hra' : d.read a = NF.lit v := hra
       have hrb' : d.read b = NF.lit u := hrb
       show Mk d (match Sem.evalOp op [v, u] with
                  | .ok r => d.push (.lit r)
                  | .error _ => d.rawPrim2 op a b) (NF.mk2 op (d.read a) (d.read b))
       rw [hra', hrb']
       cases hev : Sem.evalOp op [v, u] with
       | ok r =>
           rw [show NF.mk2 op (.lit v) (.lit u) = .lit r from by simp only [NF.mk2, hev]]
           exact mkLit_spec hwf r
       | error e =>
           rw [show NF.mk2 op (.lit v) (.lit u) = .prim2 op (.lit v) (.lit u) from by
                 simp only [NF.mk2, hev],
               ← hra', ← hrb']
           exact rawPrim2_spec hwf ha hb hop)
    | (show Mk d (d.rawPrim2 op a b) (NF.mk2 op (d.read a) (d.read b))
       have hne : ∀ v, d.read a ≠ NF.lit v := by
         intro v hc
         rw [hra] at hc
         simp [DNode.toNF] at hc
       rw [mk2_default (Or.inl hne)]
       exact rawPrim2_spec hwf ha hb hop)
    | (show Mk d (d.rawPrim2 op a b) (NF.mk2 op (d.read a) (d.read b))
       have hne : ∀ u, d.read b ≠ NF.lit u := by
         intro u hc
         rw [hrb] at hc
         simp [DNode.toNF] at hc
       rw [mk2_default (Or.inr hne)]
       exact rawPrim2_spec hwf ha hb hop)

/-- The `eq` peephole row with a literal LEFT operand (`mk2W`'s third
row — reached only when the right operand is not a literal). -/
def mk2eqL (d : Dag) (a b : Nat) : Dag × Nat :=
  match d.nodes[a]? with
  | some (.lit u) =>
      if d.widthOf b = 1 ∧ u = ⟨1, 1#1⟩ then (d, b)
      else if d.widthOf b = 1 ∧ u = ⟨1, 0#1⟩ then d.mk1D .not b
      else d.mk2fold .eq a b
  | _ => d.mk2fold .eq a b

theorem mk2eqL_spec {d : Dag} (hwf : d.WF) {a b : Nat} (ha : a < d.size) (hb : b < d.size)
    (hbne : ∀ u, d.read b ≠ NF.lit u) :
    Mk d (d.mk2eqL a b) (NF.mk2W .eq (d.read a) (d.read b)) := by
  obtain ⟨na, hna⟩ := node_of_lt ha
  have hra : d.read a = na.toNF d.read := read_eq (hwf.child_lt a na hna) hna
  have hwb : annWidth (d.read b) = some (d.widthOf b) := widthOf_eq hwf hb
  have hiffb : (annWidth (d.read b) = some 1) ↔ (d.widthOf b = 1) := by
    rw [hwb]
    exact ⟨fun h => Option.some_inj.mp h, fun h => by rw [h]⟩
  rw [mk2eqL, hna]
  cases na with
  | lit u =>
      have hra' : d.read a = NF.lit u := hra
      show Mk d (if d.widthOf b = 1 ∧ u = ⟨1, 1#1⟩ then (d, b)
                 else if d.widthOf b = 1 ∧ u = ⟨1, 0#1⟩ then d.mk1D .not b
                 else d.mk2fold .eq a b)
        (NF.mk2W .eq (d.read a) (d.read b))
      rw [hra', mk2W_eq_litL _ _ hbne]
      by_cases h1 : d.widthOf b = 1 ∧ u = ⟨1, 1#1⟩
      · have h1' : annWidth (d.read b) = some 1 ∧ u = ⟨1, 1#1⟩ := ⟨hiffb.mpr h1.1, h1.2⟩
        rw [if_pos h1, if_pos h1']
        exact self_mk hwf hb
      · have h1' : ¬(annWidth (d.read b) = some 1 ∧ u = ⟨1, 1#1⟩) :=
          fun hc => h1 ⟨hiffb.mp hc.1, hc.2⟩
        rw [if_neg h1, if_neg h1']
        by_cases h2 : d.widthOf b = 1 ∧ u = ⟨1, 0#1⟩
        · have h2' : annWidth (d.read b) = some 1 ∧ u = ⟨1, 0#1⟩ := ⟨hiffb.mpr h2.1, h2.2⟩
          rw [if_pos h2, if_pos h2']
          exact mk1D_spec hwf hb rfl
        · have h2' : ¬(annWidth (d.read b) = some 1 ∧ u = ⟨1, 0#1⟩) :=
            fun hc => h2 ⟨hiffb.mp hc.1, hc.2⟩
          rw [if_neg h2, if_neg h2', ← hra']
          exact mk2fold_spec hwf ha hb rfl
  | var w x =>
      have hane : ∀ u, d.read a ≠ NF.lit u := by
        intro u hc
        rw [hra] at hc
        simp [DNode.toNF] at hc
      show Mk d (d.mk2fold .eq a b) (NF.mk2W .eq (d.read a) (d.read b))
      rw [mk2W_eq_default _ _ hane hbne]
      exact mk2fold_spec hwf ha hb rfl
  | prim1 w op c =>
      have hane : ∀ u, d.read a ≠ NF.lit u := by
        intro u hc
        rw [hra] at hc
        simp [DNode.toNF] at hc
      show Mk d (d.mk2fold .eq a b) (NF.mk2W .eq (d.read a) (d.read b))
      rw [mk2W_eq_default _ _ hane hbne]
      exact mk2fold_spec hwf ha hb rfl
  | prim2 w op c e =>
      have hane : ∀ u, d.read a ≠ NF.lit u := by
        intro u hc
        rw [hra] at hc
        simp [DNode.toNF] at hc
      show Mk d (d.mk2fold .eq a b) (NF.mk2W .eq (d.read a) (d.read b))
      rw [mk2W_eq_default _ _ hane hbne]
      exact mk2fold_spec hwf ha hb rfl
  | cat w c e =>
      have hane : ∀ u, d.read a ≠ NF.lit u := by
        intro u hc
        rw [hra] at hc
        simp [DNode.toNF] at hc
      show Mk d (d.mk2fold .eq a b) (NF.mk2W .eq (d.read a) (d.read b))
      rw [mk2W_eq_default _ _ hane hbne]
      exact mk2fold_spec hwf ha hb rfl
  | slice j w e =>
      have hane : ∀ u, d.read a ≠ NF.lit u := by
        intro u hc
        rw [hra] at hc
        simp [DNode.toNF] at hc
      show Mk d (d.mk2fold .eq a b) (NF.mk2W .eq (d.read a) (d.read b))
      rw [mk2W_eq_default _ _ hane hbne]
      exact mk2fold_spec hwf ha hb rfl
  | ite w c t e =>
      have hane : ∀ u, d.read a ≠ NF.lit u := by
        intro u hc
        rw [hra] at hc
        simp [DNode.toNF] at hc
      show Mk d (d.mk2fold .eq a b) (NF.mk2W .eq (d.read a) (d.read b))
      rw [mk2W_eq_default _ _ hane hbne]
      exact mk2fold_spec hwf ha hb rfl
  | xcall w ext c =>
      have hane : ∀ u, d.read a ≠ NF.lit u := by
        intro u hc
        rw [hra] at hc
        simp [DNode.toNF] at hc
      show Mk d (d.mk2fold .eq a b) (NF.mk2W .eq (d.read a) (d.read b))
      rw [mk2W_eq_default _ _ hane hbne]
      exact mk2fold_spec hwf ha hb rfl

/-- `mk2W`, node-level: modulus by a zero literal is the identity; the
1-bit equality peepholes read the cached operand width; everything
else constant-folds. -/
def mk2D (d : Dag) (op : Op) (a b : Nat) : Dag × Nat :=
  match op with
  | .umod =>
      (match d.nodes[b]? with
      | some (.lit u) => if u.nat = 0 then (d, a) else d.mk2fold .umod a b
      | _ => d.mk2fold .umod a b)
  | .eq =>
      (match d.nodes[b]? with
      | some (.lit u) =>
          if d.widthOf a = 1 ∧ u = ⟨1, 1#1⟩ then (d, a)
          else if d.widthOf a = 1 ∧ u = ⟨1, 0#1⟩ then d.mk1D .not a
          else d.mk2fold .eq a b
      | _ => d.mk2eqL a b)
  | op => d.mk2fold op a b

theorem mk2D_spec {d : Dag} (hwf : d.WF) {op : Op} {a b : Nat} (ha : a < d.size)
    (hb : b < d.size) (hop : opArity op = 2) :
    Mk d (d.mk2D op a b) (NF.mk2W op (d.read a) (d.read b)) := by
  obtain ⟨na, hna⟩ := node_of_lt ha
  obtain ⟨nb, hnb⟩ := node_of_lt hb
  have hra : d.read a = na.toNF d.read := read_eq (hwf.child_lt a na hna) hna
  have hrb : d.read b = nb.toNF d.read := read_eq (hwf.child_lt b nb hnb) hnb
  have hwa : annWidth (d.read a) = some (d.widthOf a) := widthOf_eq hwf ha
  have hwb : annWidth (d.read b) = some (d.widthOf b) := widthOf_eq hwf hb
  have hlitb : ∀ u : BV, d.nodes[b]? = some (.lit u) → d.read b = NF.lit u := by
    intro u h
    rw [hnb] at h
    injection h with h
    rw [hrb, h]
    rfl
  have hlita : ∀ u : BV, d.nodes[a]? = some (.lit u) → d.read a = NF.lit u := by
    intro u h
    rw [hna] at h
    injection h with h
    rw [hra, h]
    rfl
  by_cases hu : op = .umod
  · subst hu
    rw [mk2D, hnb]
    cases nb with
    | lit u =>
        have hrb' : d.read b = NF.lit u := hlitb u hnb
        show Mk d (if u.nat = 0 then (d, a) else d.mk2fold .umod a b)
          (NF.mk2W .umod (d.read a) (d.read b))
        rw [hrb', mk2W_umod_lit]
        by_cases hz : u.nat = 0
        · rw [if_pos hz, if_pos hz]
          exact self_mk hwf ha
        · rw [if_neg hz, if_neg hz, ← hrb']
          exact mk2fold_spec hwf ha hb hop
    | var w x =>
        show Mk d (d.mk2fold .umod a b) (NF.mk2W .umod (d.read a) (d.read b))
        have hne : ∀ u, d.read b ≠ NF.lit u := by
          intro u hc; rw [hrb] at hc; simp [DNode.toNF] at hc
        rw [mk2W_umod_default _ _ hne]
        exact mk2fold_spec hwf ha hb hop
    | prim1 w' op' c =>
        show Mk d (d.mk2fold .umod a b) (NF.mk2W .umod (d.read a) (d.read b))
        have hne : ∀ u, d.read b ≠ NF.lit u := by
          intro u hc; rw [hrb] at hc; simp [DNode.toNF] at hc
        rw [mk2W_umod_default _ _ hne]
        exact mk2fold_spec hwf ha hb hop
    | prim2 w' op' c e =>
        show Mk d (d.mk2fold .umod a b) (NF.mk2W .umod (d.read a) (d.read b))
        have hne : ∀ u, d.read b ≠ NF.lit u := by
          intro u hc; rw [hrb] at hc; simp [DNode.toNF] at hc
        rw [mk2W_umod_default _ _ hne]
        exact mk2fold_spec hwf ha hb hop
    | cat w' c e =>
        show Mk d (d.mk2fold .umod a b) (NF.mk2W .umod (d.read a) (d.read b))
        have hne : ∀ u, d.read b ≠ NF.lit u := by
          intro u hc; rw [hrb] at hc; simp [DNode.toNF] at hc
        rw [mk2W_umod_default _ _ hne]
        exact mk2fold_spec hwf ha hb hop
    | slice j w' e =>
        show Mk d (d.mk2fold .umod a b) (NF.mk2W .umod (d.read a) (d.read b))
        have hne : ∀ u, d.read b ≠ NF.lit u := by
          intro u hc; rw [hrb] at hc; simp [DNode.toNF] at hc
        rw [mk2W_umod_default _ _ hne]
        exact mk2fold_spec hwf ha hb hop
    | ite w' c t e =>
        show Mk d (d.mk2fold .umod a b) (NF.mk2W .umod (d.read a) (d.read b))
        have hne : ∀ u, d.read b ≠ NF.lit u := by
          intro u hc; rw [hrb] at hc; simp [DNode.toNF] at hc
        rw [mk2W_umod_default _ _ hne]
        exact mk2fold_spec hwf ha hb hop
    | xcall w' ext c =>
        show Mk d (d.mk2fold .umod a b) (NF.mk2W .umod (d.read a) (d.read b))
        have hne : ∀ u, d.read b ≠ NF.lit u := by
          intro u hc; rw [hrb] at hc; simp [DNode.toNF] at hc
        rw [mk2W_umod_default _ _ hne]
        exact mk2fold_spec hwf ha hb hop
  · by_cases he : op = .eq
    · subst he
      have hiffa : (annWidth (d.read a) = some 1) ↔ (d.widthOf a = 1) := by
        rw [hwa]
        exact ⟨fun h => Option.some_inj.mp h, fun h => by rw [h]⟩
      have hiffb : (annWidth (d.read b) = some 1) ↔ (d.widthOf b = 1) := by
        rw [hwb]
        exact ⟨fun h => Option.some_inj.mp h, fun h => by rw [h]⟩
      rw [mk2D, hnb]
      cases nb <;>
        first
        | (rename_i u
           have hrb' : d.read b = NF.lit u := hlitb u hnb
           show Mk d (if d.widthOf a = 1 ∧ u = ⟨1, 1#1⟩ then (d, a)
                      else if d.widthOf a = 1 ∧ u = ⟨1, 0#1⟩ then d.mk1D .not a
                      else d.mk2fold .eq a b)
             (NF.mk2W .eq (d.read a) (d.read b))
           rw [hrb', mk2W_eq_litR]
           by_cases h1 : d.widthOf a = 1 ∧ u = ⟨1, 1#1⟩
           · have h1' : annWidth (d.read a) = some 1 ∧ u = ⟨1, 1#1⟩ := ⟨hiffa.mpr h1.1, h1.2⟩
             rw [if_pos h1, if_pos h1']
             exact self_mk hwf ha
           · have h1' : ¬(annWidth (d.read a) = some 1 ∧ u = ⟨1, 1#1⟩) :=
               fun hc => h1 ⟨hiffa.mp hc.1, hc.2⟩
             rw [if_neg h1, if_neg h1']
             by_cases h2 : d.widthOf a = 1 ∧ u = ⟨1, 0#1⟩
             · have h2' : annWidth (d.read a) = some 1 ∧ u = ⟨1, 0#1⟩ := ⟨hiffa.mpr h2.1, h2.2⟩
               rw [if_pos h2, if_pos h2']
               exact mk1D_spec hwf ha rfl
             · have h2' : ¬(annWidth (d.read a) = some 1 ∧ u = ⟨1, 0#1⟩) :=
                 fun hc => h2 ⟨hiffa.mp hc.1, hc.2⟩
               rw [if_neg h2, if_neg h2', ← hrb']
               exact mk2fold_spec hwf ha hb hop)
        | (have hbne : ∀ u, d.read b ≠ NF.lit u := by
             intro u hc
             rw [hrb] at hc
             simp [DNode.toNF] at hc
           show Mk d (d.mk2eqL a b) (NF.mk2W .eq (d.read a) (d.read b))
           exact mk2eqL_spec hwf ha hb hbne)
    · rw [show d.mk2D op a b = d.mk2fold op a b from by
            cases op <;> first | exact absurd rfl hu | exact absurd rfl he | rfl,
          mk2W_other hu he]
      exact mk2fold_spec hwf ha hb hop


end Dag

/-! ### `mkIteW`, node-level -/

private theorem mkIteW_litlit (c : NF) (v u : BV) :
    NF.mkIteW c (.lit v) (.lit u)
      = if v.width = 1 ∧ u.width = 1 ∧ v.nat = 1 ∧ u.nat = 0 ∧ annWidth c = some 1 then c
        else if v.width = 1 ∧ u.width = 1 ∧ v.nat = 0 ∧ u.nat = 1 ∧ annWidth c = some 1 then
          NF.mk1W .not c
        else NF.mkIte c (.lit v) (.lit u) := by
  cases c <;> rfl

private theorem mkIteW_default (c t e : NF) (h : (∀ v, t ≠ .lit v) ∨ (∀ u, e ≠ .lit u)) :
    NF.mkIteW c t e = NF.mkIte c t e := by
  cases t <;> cases e <;>
    first
    | (rcases h with h | h <;> exact absurd rfl (h _))
    | rfl

namespace Dag

/-- `mkIte`, node-level: literal-condition selection. -/
def mkIteFold (d : Dag) (c t e : Nat) : Dag × Nat :=
  match d.nodes[c]? with
  | some (.lit v) => if v.nat ≠ 0 then (d, t) else (d, e)
  | _ => d.rawIte c t e

theorem mkIteFold_spec {d : Dag} (hwf : d.WF) {c t e : Nat} (hc : c < d.size)
    (ht : t < d.size) (he : e < d.size) (harm : d.widthOf t = d.widthOf e) :
    Mk d (d.mkIteFold c t e) (NF.mkIte (d.read c) (d.read t) (d.read e)) := by
  obtain ⟨nc, hnc⟩ := node_of_lt hc
  have hrc : d.read c = nc.toNF d.read := read_eq (hwf.child_lt c nc hnc) hnc
  rw [mkIteFold, hnc]
  cases nc <;>
    first
    | (rename_i v
       have hrc' : d.read c = NF.lit v := hrc
       show Mk d (if v.nat ≠ 0 then (d, t) else (d, e))
         (NF.mkIte (d.read c) (d.read t) (d.read e))
       rw [hrc',
           show NF.mkIte (NF.lit v) (d.read t) (d.read e)
             = if v.nat ≠ 0 then d.read t else d.read e from rfl]
       by_cases hz : v.nat ≠ 0
       · rw [if_pos hz, if_pos hz]
         exact self_mk hwf ht
       · rw [if_neg hz, if_neg hz]
         exact self_mk hwf he)
    | (show Mk d (d.rawIte c t e) (NF.mkIte (d.read c) (d.read t) (d.read e))
       refine (rawIte_spec hwf hc ht he harm).cast ?_
       rw [hrc]
       rfl)

/-- `mkIteW`, node-level: the 1-bit boolean-mux peepholes (cached
condition width), then literal-condition selection. -/
def mkIteD (d : Dag) (c t e : Nat) : Dag × Nat :=
  match d.nodes[t]?, d.nodes[e]? with
  | some (.lit v), some (.lit u) =>
      if v.width = 1 ∧ u.width = 1 ∧ v.nat = 1 ∧ u.nat = 0 ∧ d.widthOf c = 1 then (d, c)
      else if v.width = 1 ∧ u.width = 1 ∧ v.nat = 0 ∧ u.nat = 1 ∧ d.widthOf c = 1 then
        d.mk1D .not c
      else d.mkIteFold c t e
  | _, _ => d.mkIteFold c t e

theorem mkIteD_spec {d : Dag} (hwf : d.WF) {c t e : Nat} (hc : c < d.size)
    (ht : t < d.size) (he : e < d.size) (harm : d.widthOf t = d.widthOf e) :
    Mk d (d.mkIteD c t e) (NF.mkIteW (d.read c) (d.read t) (d.read e)) := by
  obtain ⟨nt, hnt⟩ := node_of_lt ht
  obtain ⟨ne, hne⟩ := node_of_lt he
  have hrt : d.read t = nt.toNF d.read := read_eq (hwf.child_lt t nt hnt) hnt
  have hre : d.read e = ne.toNF d.read := read_eq (hwf.child_lt e ne hne) hne
  have hiffc : (annWidth (d.read c) = some 1) ↔ (d.widthOf c = 1) := by
    rw [widthOf_eq hwf hc]
    exact ⟨fun h => Option.some_inj.mp h, fun h => by rw [h]⟩
  rw [mkIteD, hnt, hne]
  cases nt <;> cases ne <;>
    first
    | (rename_i v u
       have hrt' : d.read t = NF.lit v := hrt
       have hre' : d.read e = NF.lit u := hre
       show Mk d (if v.width = 1 ∧ u.width = 1 ∧ v.nat = 1 ∧ u.nat = 0 ∧ d.widthOf c = 1
                  then (d, c)
                  else if v.width = 1 ∧ u.width = 1 ∧ v.nat = 0 ∧ u.nat = 1 ∧ d.widthOf c = 1
                  then d.mk1D .not c
                  else d.mkIteFold c t e)
         (NF.mkIteW (d.read c) (d.read t) (d.read e))
       rw [hrt', hre', mkIteW_litlit]
       by_cases h1 : v.width = 1 ∧ u.width = 1 ∧ v.nat = 1 ∧ u.nat = 0 ∧ d.widthOf c = 1
       · have h1' : v.width = 1 ∧ u.width = 1 ∧ v.nat = 1 ∧ u.nat = 0
             ∧ annWidth (d.read c) = some 1 :=
           ⟨h1.1, h1.2.1, h1.2.2.1, h1.2.2.2.1, hiffc.mpr h1.2.2.2.2⟩
         rw [if_pos h1, if_pos h1']
         exact self_mk hwf hc
       · have h1' : ¬(v.width = 1 ∧ u.width = 1 ∧ v.nat = 1 ∧ u.nat = 0
             ∧ annWidth (d.read c) = some 1) :=
           fun hx => h1 ⟨hx.1, hx.2.1, hx.2.2.1, hx.2.2.2.1, hiffc.mp hx.2.2.2.2⟩
         rw [if_neg h1, if_neg h1']
         by_cases h2 : v.width = 1 ∧ u.width = 1 ∧ v.nat = 0 ∧ u.nat = 1 ∧ d.widthOf c = 1
         · have h2' : v.width = 1 ∧ u.width = 1 ∧ v.nat = 0 ∧ u.nat = 1
               ∧ annWidth (d.read c) = some 1 :=
             ⟨h2.1, h2.2.1, h2.2.2.1, h2.2.2.2.1, hiffc.mpr h2.2.2.2.2⟩
           rw [if_pos h2, if_pos h2']
           exact mk1D_spec hwf hc rfl
         · have h2' : ¬(v.width = 1 ∧ u.width = 1 ∧ v.nat = 0 ∧ u.nat = 1
               ∧ annWidth (d.read c) = some 1) :=
             fun hx => h2 ⟨hx.1, hx.2.1, hx.2.2.1, hx.2.2.2.1, hiffc.mp hx.2.2.2.2⟩
           rw [if_neg h2, if_neg h2', ← hrt', ← hre']
           exact mkIteFold_spec hwf hc ht he harm)
    | (show Mk d (d.mkIteFold c t e) (NF.mkIteW (d.read c) (d.read t) (d.read e))
       have hne1 : ∀ v, d.read t ≠ NF.lit v := by
         intro v hx
         rw [hrt] at hx
         simp [DNode.toNF] at hx
       rw [mkIteW_default _ _ _ (Or.inl hne1)]
       exact mkIteFold_spec hwf hc ht he harm)
    | (show Mk d (d.mkIteFold c t e) (NF.mkIteW (d.read c) (d.read t) (d.read e))
       have hne2 : ∀ u, d.read e ≠ NF.lit u := by
         intro u hx
         rw [hre] at hx
         simp [DNode.toNF] at hx
       rw [mkIteW_default _ _ _ (Or.inr hne2)]
       exact mkIteFold_spec hwf hc ht he harm)


end Dag

namespace Dag

/-! ### `mkSliceW`, node-level -/

/-- `mkSliceW`, node-level: slice of literal folds, slice of slice
fuses, slice of concatenation selects or splits at the low side's
cached width, full-width slices are the identity. The defensive
child-range guards are true under `WF` (making the behavior exactly
the tree constructor's) and only bound the recursion. -/
def mkSliceD (d : Dag) (i w : Nat) (e : Nat) : Dag × Nat :=
  match d.nodes[e]? with
  | some (.lit v) => d.push (.lit ⟨w, v.bits.extractLsb' i w⟩)
  | some (.slice j v e') =>
      if _h : i + w ≤ v ∧ e' < e then d.mkSliceD (j + i) w e'
      else d.push (.slice i w e)
  | some (.cat _ a b) =>
      if _h : a < e ∧ b < e then
        if d.widthOf b ≤ i then d.mkSliceD (i - d.widthOf b) w a
        else if i + w ≤ d.widthOf b then d.mkSliceD i w b
        else
          let r1 := d.mkSliceD 0 (i + w - d.widthOf b) a
          let r2 := r1.1.mkSliceD i (d.widthOf b - i) b
          r2.1.push (.cat (r2.1.widthOf r1.2 + r2.1.widthOf r2.2) r1.2 r2.2)
      else d.push (.slice i w e)
  | some n => if i = 0 ∧ w = n.width then (d, e) else d.push (.slice i w e)
  | none => d.push (.slice i w e)
termination_by e
decreasing_by all_goals omega

theorem mkSliceD_spec :
    ∀ (e : Nat) (d : Dag) (i w : Nat), d.WF → e < d.size →
      Mk d (d.mkSliceD i w e) (NF.mkSliceW i w (d.read e)) := by
  intro e
  induction e using Nat.strongRecOn with
  | _ e ih =>
    intro d i w hwf he
    obtain ⟨ne, hne⟩ := node_of_lt he
    have hre : d.read e = ne.toNF d.read := read_eq (hwf.child_lt e ne hne) hne
    rw [mkSliceD, hne]
    cases ne with
    | lit v =>
        have hre' : d.read e = NF.lit v := hre
        rw [hre']
        show Mk d (d.push (.lit ⟨w, v.bits.extractLsb' i w⟩))
          (NF.mkSliceW i w (NF.lit v))
        simp only [NF.mkSliceW]
        exact mkLit_spec hwf _
    | slice j v e' =>
        have hre' : d.read e = NF.slice j v (d.read e') := hre
        have he' : e' < e := hwf.child_lt e _ hne e' (by simp [DNode.children])
        rw [hre']
        show Mk d (if _h : i + w ≤ v ∧ e' < e then d.mkSliceD (j + i) w e'
                   else d.push (.slice i w e))
          (NF.mkSliceW i w (NF.slice j v (d.read e')))
        simp only [NF.mkSliceW]
        by_cases hle : i + w ≤ v
        · rw [dif_pos ⟨hle, he'⟩, if_pos hle]
          exact ih e' he' d (j + i) w hwf (Nat.lt_trans he' he)
        · rw [dif_neg (fun hx => hle hx.1), if_neg hle]
          refine (rawSlice_spec hwf i w he).cast ?_
          rw [hre']
    | cat cw a b =>
        have hre' : d.read e = NF.cat (d.read a) (d.read b) := hre
        have hab : a < e ∧ b < e :=
          ⟨hwf.child_lt e _ hne a (by simp [DNode.children]),
           hwf.child_lt e _ hne b (by simp [DNode.children])⟩
        have ha' : a < d.size := Nat.lt_trans hab.1 he
        have hb' : b < d.size := Nat.lt_trans hab.2 he
        have hwb : annWidth (d.read b) = some (d.widthOf b) := widthOf_eq hwf hb'
        rw [hre']
        show Mk d (if _h : a < e ∧ b < e then
                     if d.widthOf b ≤ i then d.mkSliceD (i - d.widthOf b) w a
                     else if i + w ≤ d.widthOf b then d.mkSliceD i w b
                     else
                       let r1 := d.mkSliceD 0 (i + w - d.widthOf b) a
                       let r2 := r1.1.mkSliceD i (d.widthOf b - i) b
                       r2.1.push (.cat (r2.1.widthOf r1.2 + r2.1.widthOf r2.2) r1.2 r2.2)
                   else d.push (.slice i w e))
          (NF.mkSliceW i w (NF.cat (d.read a) (d.read b)))
        simp only [NF.mkSliceW]
        rw [hwb, dif_pos hab]
        show Mk d (if d.widthOf b ≤ i then d.mkSliceD (i - d.widthOf b) w a
                   else if i + w ≤ d.widthOf b then d.mkSliceD i w b
                   else
                     let r1 := d.mkSliceD 0 (i + w - d.widthOf b) a
                     let r2 := r1.1.mkSliceD i (d.widthOf b - i) b
                     r2.1.push (.cat (r2.1.widthOf r1.2 + r2.1.widthOf r2.2) r1.2 r2.2))
          (if d.widthOf b ≤ i then NF.mkSliceW (i - d.widthOf b) w (d.read a)
           else if i + w ≤ d.widthOf b then NF.mkSliceW i w (d.read b)
           else NF.cat (NF.mkSliceW 0 (i + w - d.widthOf b) (d.read a))
                       (NF.mkSliceW i (d.widthOf b - i) (d.read b)))
        by_cases h1 : d.widthOf b ≤ i
        · rw [if_pos h1, if_pos h1]
          exact ih a hab.1 d (i - d.widthOf b) w hwf ha'
        · rw [if_neg h1, if_neg h1]
          by_cases h2 : i + w ≤ d.widthOf b
          · rw [if_pos h2, if_pos h2]
            exact ih b hab.2 d i w hwf hb'
          · rw [if_neg h2, if_neg h2]
            have S1 := ih a hab.1 d 0 (i + w - d.widthOf b) hwf ha'
            rcases hp1 : d.mkSliceD 0 (i + w - d.widthOf b) a with ⟨d₁, ra⟩
            rw [hp1] at S1
            have hb₁ : b < d₁.size := Nat.lt_of_lt_of_le hb' S1.ext.size_le
            have S2 := ih b hab.2 d₁ i (d.widthOf b - i) S1.wf hb₁
            rcases hp2 : d₁.mkSliceD i (d.widthOf b - i) b with ⟨d₂, rb⟩
            rw [hp2] at S2
            simp only [hp2]
            have hra₂ : ra < d₂.size := Nat.lt_of_lt_of_le S1.lt S2.ext.size_le
            have hreads : d₂.read ra = NF.mkSliceW 0 (i + w - d.widthOf b) (d.read a) := by
              rw [read_ext S2.ext ra S1.lt]
              exact S1.read
            have hreadb : d₂.read rb = NF.mkSliceW i (d.widthOf b - i) (d.read b) := by
              rw [S2.read, read_ext S1.ext b hb']
            obtain ⟨W3, E3, L3, R3⟩ := push_mk (n := .cat (d₂.widthOf ra + d₂.widthOf rb) ra rb)
              S2.wf
              (fun j hj => by
                rcases mem2 hj with h | h <;> subst h
                · exact hra₂
                · exact S2.lt)
              (by
                show annWidth (.cat (d₂.read ra) (d₂.read rb))
                  = some (d₂.widthOf ra + d₂.widthOf rb)
                simp only [annWidth, widthOf_eq S2.wf hra₂, widthOf_eq S2.wf S2.lt])
            refine ⟨W3, (S1.ext.trans S2.ext).trans E3, L3, ?_⟩
            rw [R3]
            show NF.cat (d₂.read ra) (d₂.read rb) = _
            rw [hreads, hreadb]
    | var vw x =>
        have hre' : d.read e = NF.var vw x := hre
        rw [hre']
        show Mk d (if i = 0 ∧ w = (DNode.var vw x).width then (d, e)
                   else d.push (.slice i w e))
          (NF.mkSliceW i w (NF.var vw x))
        simp only [NF.mkSliceW, DNode.width]
        rw [show annWidth (NF.var vw x) = some vw from rfl]
        show Mk d (if i = 0 ∧ w = vw then (d, e) else d.push (.slice i w e))
          (if i = 0 ∧ w = vw then NF.var vw x else NF.slice i w (NF.var vw x))
        by_cases hid : i = 0 ∧ w = vw
        · rw [if_pos hid, if_pos hid, ← hre']
          exact self_mk hwf he
        · rw [if_neg hid, if_neg hid]
          refine (rawSlice_spec hwf i w he).cast ?_
          rw [hre']
    | prim1 pw op a =>
        have hre' : d.read e = NF.prim1 op (d.read a) := hre
        have hann : annWidth (d.read e) = some pw := hwf.width_coh e _ hne
        rw [hre']
        rw [hre'] at hann
        show Mk d (if i = 0 ∧ w = (DNode.prim1 pw op a).width then (d, e)
                   else d.push (.slice i w e))
          (NF.mkSliceW i w (NF.prim1 op (d.read a)))
        simp only [NF.mkSliceW, DNode.width]
        rw [hann]
        show Mk d (if i = 0 ∧ w = pw then (d, e) else d.push (.slice i w e))
          (if i = 0 ∧ w = pw then NF.prim1 op (d.read a)
           else NF.slice i w (NF.prim1 op (d.read a)))
        by_cases hid : i = 0 ∧ w = pw
        · rw [if_pos hid, if_pos hid, ← hre']
          exact self_mk hwf he
        · rw [if_neg hid, if_neg hid]
          refine (rawSlice_spec hwf i w he).cast ?_
          rw [hre']
    | prim2 pw op a b =>
        have hre' : d.read e = NF.prim2 op (d.read a) (d.read b) := hre
        have hann : annWidth (d.read e) = some pw := hwf.width_coh e _ hne
        rw [hre']
        rw [hre'] at hann
        show Mk d (if i = 0 ∧ w = (DNode.prim2 pw op a b).width then (d, e)
                   else d.push (.slice i w e))
          (NF.mkSliceW i w (NF.prim2 op (d.read a) (d.read b)))
        simp only [NF.mkSliceW, DNode.width]
        rw [hann]
        show Mk d (if i = 0 ∧ w = pw then (d, e) else d.push (.slice i w e))
          (if i = 0 ∧ w = pw then NF.prim2 op (d.read a) (d.read b)
           else NF.slice i w (NF.prim2 op (d.read a) (d.read b)))
        by_cases hid : i = 0 ∧ w = pw
        · rw [if_pos hid, if_pos hid, ← hre']
          exact self_mk hwf he
        · rw [if_neg hid, if_neg hid]
          refine (rawSlice_spec hwf i w he).cast ?_
          rw [hre']
    | ite iw c t u =>
        have hre' : d.read e = NF.ite (d.read c) (d.read t) (d.read u) := hre
        have hann : annWidth (d.read e) = some iw := hwf.width_coh e _ hne
        rw [hre']
        rw [hre'] at hann
        show Mk d (if i = 0 ∧ w = (DNode.ite iw c t u).width then (d, e)
                   else d.push (.slice i w e))
          (NF.mkSliceW i w (NF.ite (d.read c) (d.read t) (d.read u)))
        simp only [NF.mkSliceW, DNode.width]
        rw [hann]
        show Mk d (if i = 0 ∧ w = iw then (d, e) else d.push (.slice i w e))
          (if i = 0 ∧ w = iw then NF.ite (d.read c) (d.read t) (d.read u)
           else NF.slice i w (NF.ite (d.read c) (d.read t) (d.read u)))
        by_cases hid : i = 0 ∧ w = iw
        · rw [if_pos hid, if_pos hid, ← hre']
          exact self_mk hwf he
        · rw [if_neg hid, if_neg hid]
          refine (rawSlice_spec hwf i w he).cast ?_
          rw [hre']
    | xcall xw ext a =>
        have hre' : d.read e = NF.xcall xw ext (d.read a) := hre
        have hann : annWidth (d.read e) = some xw := hwf.width_coh e _ hne
        rw [hre']
        rw [hre'] at hann
        show Mk d (if i = 0 ∧ w = (DNode.xcall xw ext a).width then (d, e)
                   else d.push (.slice i w e))
          (NF.mkSliceW i w (NF.xcall xw ext (d.read a)))
        simp only [NF.mkSliceW, DNode.width]
        rw [hann]
        show Mk d (if i = 0 ∧ w = xw then (d, e) else d.push (.slice i w e))
          (if i = 0 ∧ w = xw then NF.xcall xw ext (d.read a)
           else NF.slice i w (NF.xcall xw ext (d.read a)))
        by_cases hid : i = 0 ∧ w = xw
        · rw [if_pos hid, if_pos hid, ← hre']
          exact self_mk hwf he
        · rw [if_neg hid, if_neg hid]
          refine (rawSlice_spec hwf i w he).cast ?_
          rw [hre']


end Dag

/-! ### `mkCatW`, node-level -/

private theorem mergeStep_nil (p : NF) : NF.mergeStep p [] = [p] := by
  cases p <;> rfl

private theorem catPieces_non_cat (X : NF) (h : ∀ a b, X ≠ .cat a b) :
    NF.catPieces X = [X] := by
  cases X <;> first | exact absurd rfl (h _ _) | simp [NF.catPieces]

/-- The list-result constructor contract (for the concatenation-spine
passes): extended well-formed store, in-range results, prescribed
readings. -/
def MkL (d : Dag) (res : Dag × List Nat) (nfs : List NF) : Prop :=
  res.1.WF ∧ d.Ext res.1 ∧ (∀ p ∈ res.2, p < res.1.size) ∧ res.2.map res.1.read = nfs

namespace MkL

theorem wf {d : Dag} {res : Dag × List Nat} {nfs : List NF} (h : MkL d res nfs) :
    res.1.WF := h.1
theorem ext {d : Dag} {res : Dag × List Nat} {nfs : List NF} (h : MkL d res nfs) :
    d.Ext res.1 := h.2.1
theorem lt {d : Dag} {res : Dag × List Nat} {nfs : List NF} (h : MkL d res nfs) :
    ∀ p ∈ res.2, p < res.1.size := h.2.2.1
theorem map {d : Dag} {res : Dag × List Nat} {nfs : List NF} (h : MkL d res nfs) :
    res.2.map res.1.read = nfs := h.2.2.2

end MkL

namespace Dag

/-- Readings of an in-range index list are stable under extension. -/
private theorem map_read_ext {d₁ d₂ : Dag} (hext : d₁.Ext d₂) {l : List Nat}
    (hl : ∀ q ∈ l, q < d₁.size) : l.map d₂.read = l.map d₁.read :=
  List.map_congr_left fun q hq => read_ext hext q (hl q hq)

/-- The MSB-first pieces of a concatenation spine (`catPieces`,
node-level). Pieces are the node itself or strict descendants. -/
def catPiecesD (d : Dag) (i : Nat) : List Nat :=
  match d.nodes[i]? with
  | some (.cat _ a b) =>
      (if _h : a < i then d.catPiecesD a else [a])
        ++ (if _h : b < i then d.catPiecesD b else [b])
  | _ => [i]
termination_by i

theorem catPiecesD_cat {d : Dag} {i cw a b : Nat} (h : d.nodes[i]? = some (.cat cw a b))
    (ha : a < i) (hb : b < i) :
    d.catPiecesD i = d.catPiecesD a ++ d.catPiecesD b := by
  rw [catPiecesD, h]
  show (if _h : a < i then d.catPiecesD a else [a])
    ++ (if _h : b < i then d.catPiecesD b else [b]) = _
  rw [dif_pos ha, dif_pos hb]

theorem catPiecesD_other {d : Dag} {i : Nat} {n : DNode} (h : d.nodes[i]? = some n)
    (hn : ∀ cw a b, n ≠ DNode.cat cw a b) : d.catPiecesD i = [i] := by
  rw [catPiecesD, h]
  cases n <;> first | exact absurd rfl (hn _ _ _) | rfl

theorem catPiecesD_spec :
    ∀ (i : Nat) (d : Dag), d.WF → i < d.size →
      (∀ p ∈ d.catPiecesD i, p < d.size) ∧
      (d.catPiecesD i).map d.read = NF.catPieces (d.read i) := by
  intro i
  induction i using Nat.strongRecOn with
  | _ i ih =>
    intro d hwf hi
    obtain ⟨ni, hni⟩ := node_of_lt hi
    have hri : d.read i = ni.toNF d.read := read_eq (hwf.child_lt i ni hni) hni
    cases ni <;>
      first
      | (rename_i cw a b
         have hab : a < i ∧ b < i :=
           ⟨hwf.child_lt i _ hni a (by simp [DNode.children]),
            hwf.child_lt i _ hni b (by simp [DNode.children])⟩
         have hri' : d.read i = NF.cat (d.read a) (d.read b) := hri
         obtain ⟨hra1, hra2⟩ := ih a hab.1 d hwf (Nat.lt_trans hab.1 hi)
         obtain ⟨hrb1, hrb2⟩ := ih b hab.2 d hwf (Nat.lt_trans hab.2 hi)
         rw [catPiecesD_cat hni hab.1 hab.2, hri']
         constructor
         · intro p hp
           rcases List.mem_append.mp hp with hp | hp
           · exact hra1 p hp
           · exact hrb1 p hp
         · rw [List.map_append, hra2, hrb2]
           simp only [NF.catPieces])
      | (have hnc : ∀ x y, d.read i ≠ NF.cat x y := by
           intro x y hc
           rw [hri] at hc
           simp [DNode.toNF] at hc
         rw [catPiecesD_other hni (by intro cw2 a2 b2 hc; cases hc)]
         constructor
         · intro p hp
           have hpi : p = i := by simpa using hp
           subst hpi
           exact hi
         · rw [List.map_cons, List.map_nil, catPieces_non_cat (d.read i) hnc])

/-- One merge step against an already-merged tail (`mergeStep`,
node-level): a literal absorbs a literal head; a slice fuses with an
adjacent slice head of the same base — the "same base" test is INDEX
equality, aligned with the tree-level structural test by canonicity. -/
def mergeStepD (d : Dag) (p : Nat) (merged : List Nat) : Dag × List Nat :=
  match merged with
  | [] => (d, [p])
  | m :: rest =>
      match d.nodes[p]?, d.nodes[m]? with
      | some (.lit v), some (.lit u) =>
          let r := d.push (.lit ⟨v.width + u.width, v.bits ++ u.bits⟩)
          (r.1, r.2 :: rest)
      | some (.slice i₁ w₁ e₁), some (.slice i₂ w₂ e₂) =>
          if e₁ = e₂ ∧ i₁ = i₂ + w₂ then
            let r := d.mkSliceD i₂ (w₁ + w₂) e₁
            (r.1, r.2 :: rest)
          else (d, p :: merged)
      | _, _ => (d, p :: merged)

/-- Attach a merged head to a tail (kept as a named function so the
step equations below have a stable right-hand side). -/
def consSnd (r : Dag × Nat) (rest : List Nat) : Dag × List Nat := (r.1, r.2 :: rest)

/-- The `Mk`-to-`MkL` composition for `consSnd`. -/
theorem MkL_cons {d : Dag} {r : Dag × Nat} {nf : NF} (h : Mk d r nf) {rest : List Nat}
    (hrest : ∀ q ∈ rest, q < d.size) :
    MkL d (consSnd r rest) (nf :: rest.map d.read) := by
  refine ⟨h.wf, h.ext, ?_, ?_⟩
  · intro q hq
    rcases List.mem_cons.mp hq with hq | hq
    · subst hq
      exact h.lt
    · exact Nat.lt_of_lt_of_le (hrest q hq) h.ext.size_le
  · show (r.2 :: rest).map r.1.read = nf :: rest.map d.read
    rw [List.map_cons, h.read, map_read_ext h.ext hrest]

private def isLit : DNode → Bool
  | .lit _ => true
  | _ => false

private def isSlice : DNode → Bool
  | .slice _ _ _ => true
  | _ => false

theorem mergeStepD_litlit {d : Dag} {p m : Nat} {rest : List Nat} {v u : BV}
    (hnp : d.nodes[p]? = some (.lit v)) (hnm : d.nodes[m]? = some (.lit u)) :
    d.mergeStepD p (m :: rest)
      = consSnd (d.push (.lit ⟨v.width + u.width, v.bits ++ u.bits⟩)) rest := by
  rw [mergeStepD, hnp, hnm]
  rfl

theorem mergeStepD_slices {d : Dag} {p m : Nat} {rest : List Nat} {i₁ w₁ e₁ i₂ w₂ e₂ : Nat}
    (hnp : d.nodes[p]? = some (.slice i₁ w₁ e₁)) (hnm : d.nodes[m]? = some (.slice i₂ w₂ e₂)) :
    d.mergeStepD p (m :: rest)
      = if e₁ = e₂ ∧ i₁ = i₂ + w₂ then consSnd (d.mkSliceD i₂ (w₁ + w₂) e₁) rest
        else (d, p :: m :: rest) := by
  rw [mergeStepD, hnp, hnm]
  rfl

theorem mergeStepD_default {d : Dag} {p m : Nat} {rest : List Nat} {np nm : DNode}
    (hnp : d.nodes[p]? = some np) (hnm : d.nodes[m]? = some nm)
    (h : ((isLit np && isLit nm) || (isSlice np && isSlice nm)) = false) :
    d.mergeStepD p (m :: rest) = (d, p :: m :: rest) := by
  rw [mergeStepD, hnp, hnm]
  cases np <;> cases nm <;> first | rfl | (simp [isLit, isSlice] at h)

theorem mergeStepD_spec {d : Dag} (hwf : d.WF) {p : Nat} {merged : List Nat}
    (hp : p < d.size) (hm : ∀ q ∈ merged, q < d.size) :
    MkL d (d.mergeStepD p merged) (NF.mergeStep (d.read p) (merged.map d.read)) := by
  obtain ⟨np, hnp⟩ := node_of_lt hp
  have hrp : d.read p = np.toNF d.read := read_eq (hwf.child_lt p np hnp) hnp
  cases merged with
  | nil =>
      rw [mergeStepD, List.map_nil, mergeStep_nil]
      exact ⟨hwf, Ext.refl d, by simpa using hp, by simp⟩
  | cons m rest =>
      have hm' : m < d.size := hm m List.mem_cons_self
      have hrest : ∀ q ∈ rest, q < d.size := fun q hq => hm q (List.mem_cons_of_mem _ hq)
      obtain ⟨nm, hnm⟩ := node_of_lt hm'
      have hrm : d.read m = nm.toNF d.read := read_eq (hwf.child_lt m nm hnm) hnm
      cases np <;> cases nm <;>
        first
        | -- literal absorbs literal
          (rename_i v u
           rw [mergeStepD_litlit hnp hnm]
           have hrp' : d.read p = NF.lit v := hrp
           have hrm' : d.read m = NF.lit u := hrm
           rw [List.map_cons, hrp', hrm',
               show NF.mergeStep (NF.lit v) (NF.lit u :: rest.map d.read)
                 = NF.lit ⟨v.width + u.width, v.bits ++ u.bits⟩ :: rest.map d.read from rfl]
           exact MkL_cons (mkLit_spec hwf ⟨v.width + u.width, v.bits ++ u.bits⟩) hrest)
        | -- adjacent slices of a common base
          (rename_i i₁ w₁ e₁ i₂ w₂ e₂
           rw [mergeStepD_slices hnp hnm]
           have hrp' : d.read p = NF.slice i₁ w₁ (d.read e₁) := hrp
           have hrm' : d.read m = NF.slice i₂ w₂ (d.read e₂) := hrm
           have he₁ : e₁ < d.size :=
             Nat.lt_trans (hwf.child_lt p _ hnp e₁ (by simp [DNode.children])) hp
           have he₂ : e₂ < d.size :=
             Nat.lt_trans (hwf.child_lt m _ hnm e₂ (by simp [DNode.children])) hm'
           rw [List.map_cons, hrp', hrm',
               show NF.mergeStep (NF.slice i₁ w₁ (d.read e₁))
                     (NF.slice i₂ w₂ (d.read e₂) :: rest.map d.read)
                 = if d.read e₁ = d.read e₂ ∧ i₁ = i₂ + w₂ then
                     NF.mkSliceW i₂ (w₁ + w₂) (d.read e₁) :: rest.map d.read
                   else NF.slice i₁ w₁ (d.read e₁) :: NF.slice i₂ w₂ (d.read e₂)
                     :: rest.map d.read from rfl]
           by_cases hg : e₁ = e₂ ∧ i₁ = i₂ + w₂
           · have hg' : d.read e₁ = d.read e₂ ∧ i₁ = i₂ + w₂ := ⟨by rw [hg.1], hg.2⟩
             rw [if_pos hg, if_pos hg']
             exact MkL_cons (mkSliceD_spec e₁ d i₂ (w₁ + w₂) hwf he₁) hrest
           · have hg' : ¬(d.read e₁ = d.read e₂ ∧ i₁ = i₂ + w₂) :=
               fun hx => hg ⟨hwf.canon e₁ e₂ he₁ he₂ hx.1, hx.2⟩
             rw [if_neg hg, if_neg hg']
             refine ⟨hwf, Ext.refl d, ?_, ?_⟩
             · intro q hq
               rcases List.mem_cons.mp hq with hq | hq
               · subst hq; exact hp
               · rcases List.mem_cons.mp hq with hq | hq
                 · subst hq; exact hm'
                 · exact hrest q hq
             · rw [List.map_cons, List.map_cons, hrp', hrm'])
        | -- no merge
          (rw [mergeStepD_default hnp hnm (by rfl)]
           refine ⟨hwf, Ext.refl d, ?_, ?_⟩
           · intro q hq
             rcases List.mem_cons.mp hq with hq | hq
             · subst hq; exact hp
             · rcases List.mem_cons.mp hq with hq | hq
               · subst hq; exact hm'
               · exact hrest q hq
           · simp only [List.map_cons]
             rw [hrp, hrm]
             rfl)

/-- Merge adjacent pieces, right to left (`mergePieces`, node-level). -/
def mergePiecesD (d : Dag) : List Nat → Dag × List Nat
  | [] => (d, [])
  | p :: rest =>
      let r := d.mergePiecesD rest
      r.1.mergeStepD p r.2

theorem mergePiecesD_spec :
    ∀ (l : List Nat) (d : Dag), d.WF → (∀ q ∈ l, q < d.size) →
      MkL d (d.mergePiecesD l) (NF.mergePieces (l.map d.read)) := by
  intro l
  induction l with
  | nil =>
      intro d hwf _
      exact ⟨hwf, Ext.refl d, by simp [mergePiecesD], by simp [mergePiecesD, NF.mergePieces]⟩
  | cons p rest ihl =>
      intro d hwf hl
      have hp : p < d.size := hl p List.mem_cons_self
      have hrest : ∀ q ∈ rest, q < d.size := fun q hq => hl q (List.mem_cons_of_mem _ hq)
      have M := ihl d hwf hrest
      rcases hr : d.mergePiecesD rest with ⟨d₁, l₁⟩
      rw [hr] at M
      have hp₁ : p < d₁.size := Nat.lt_of_lt_of_le hp M.ext.size_le
      have S := mergeStepD_spec M.wf hp₁ M.lt
      simp only [mergePiecesD, hr]
      rw [List.map_cons]
      simp only [NF.mergePieces]
      rw [← M.map, ← read_ext M.ext p hp]
      exact ⟨S.wf, M.ext.trans S.ext, S.lt, S.map⟩

/-- Rebuild a right-nested concatenation from MSB-first pieces
(`rebuildCat`, node-level). -/
def rebuildCatD (d : Dag) : List Nat → Dag × Nat
  | [] => d.push (.lit BV.nil)
  | [p] => (d, p)
  | p :: ps =>
      let r := d.rebuildCatD ps
      r.1.push (.cat (r.1.widthOf p + r.1.widthOf r.2) p r.2)

theorem rebuildCatD_spec :
    ∀ (l : List Nat) (d : Dag), d.WF → (∀ q ∈ l, q < d.size) →
      Mk d (d.rebuildCatD l) (NF.rebuildCat (l.map d.read)) := by
  intro l
  induction l with
  | nil =>
      intro d hwf _
      exact mkLit_spec hwf BV.nil
  | cons p ps ihl =>
      intro d hwf hl
      have hp : p < d.size := hl p List.mem_cons_self
      have hps : ∀ q ∈ ps, q < d.size := fun q hq => hl q (List.mem_cons_of_mem _ hq)
      cases ps with
      | nil =>
          simp only [rebuildCatD, List.map_cons, List.map_nil, NF.rebuildCat]
          exact self_mk hwf hp
      | cons q ps' =>
          have M := ihl d hwf hps
          rcases hr : d.rebuildCatD (q :: ps') with ⟨d₁, r₁⟩
          rw [hr] at M
          have hp₁ : p < d₁.size := Nat.lt_of_lt_of_le hp M.ext.size_le
          simp only [rebuildCatD, hr]
          obtain ⟨W, E, L, R⟩ := push_mk (n := .cat (d₁.widthOf p + d₁.widthOf r₁) p r₁)
            M.wf
            (fun j hj => by
              rcases mem2 hj with h | h <;> subst h
              · exact hp₁
              · exact M.lt)
            (by
              show annWidth (.cat (d₁.read p) (d₁.read r₁))
                = some (d₁.widthOf p + d₁.widthOf r₁)
              simp only [annWidth, widthOf_eq M.wf hp₁, widthOf_eq M.wf M.lt])
          refine ⟨W, M.ext.trans E, L, ?_⟩
          rw [R]
          show NF.cat (d₁.read p) (d₁.read r₁) = NF.rebuildCat ((p :: q :: ps').map d.read)
          rw [read_ext M.ext p hp, M.read]
          simp only [List.map_cons, NF.rebuildCat]

/-- `mkCatW`, node-level: gather the spine, merge adjacent pieces,
rebuild. -/
def mkCatD (d : Dag) (a b : Nat) : Dag × Nat :=
  let r := d.mergePiecesD (d.catPiecesD a ++ d.catPiecesD b)
  r.1.rebuildCatD r.2

theorem mkCatD_spec {d : Dag} (hwf : d.WF) {a b : Nat} (ha : a < d.size) (hb : b < d.size) :
    Mk d (d.mkCatD a b) (NF.mkCatW (d.read a) (d.read b)) := by
  obtain ⟨hpa1, hpa2⟩ := catPiecesD_spec a d hwf ha
  obtain ⟨hpb1, hpb2⟩ := catPiecesD_spec b d hwf hb
  have happ : ∀ q ∈ d.catPiecesD a ++ d.catPiecesD b, q < d.size := by
    intro q hq
    rcases List.mem_append.mp hq with hq | hq
    · exact hpa1 q hq
    · exact hpb1 q hq
  have M := mergePiecesD_spec (d.catPiecesD a ++ d.catPiecesD b) d hwf happ
  rcases hr : d.mergePiecesD (d.catPiecesD a ++ d.catPiecesD b) with ⟨d₁, l₁⟩
  rw [hr] at M
  have R := rebuildCatD_spec l₁ d₁ M.wf M.lt
  simp only [mkCatD, hr]
  rw [show NF.mkCatW (d.read a) (d.read b)
        = NF.rebuildCat (NF.mergePieces (NF.catPieces (d.read a)
            ++ NF.catPieces (d.read b))) from rfl,
      ← hpa2, ← hpb2, ← List.map_append, ← M.map]
  exact ⟨R.wf, M.ext.trans R.ext, R.lt, R.read⟩


end Dag

/-! ## Local `Except` and `HashMap.ofList` helpers

Ports of Bridge's private helpers (per house style). -/

private theorem except_pure_def {α : Type} (a : α) :
    (pure a : Except String α) = .ok a := rfl

private theorem except_bind_ok {α β : Type} (a : α) (f : α → Except String β) :
    (Except.ok a >>= f) = f a := rfl

private theorem except_bind_eq_ok {α β : Type} {x : Except String α}
    {f : α → Except String β} {b : β} (h : (x >>= f) = .ok b) :
    ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x with
  | error e =>
      exact absurd h (by
        rw [show ((Except.error e : Except String α) >>= f) = .error e from rfl]
        simp)
  | ok a => exact ⟨a, rfl, h⟩

private theorem ofList_get?_some {β : Type} {l : List (String × β)} {k : String} {b : β}
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

private theorem findSome?_option_map {α β γ : Type} {g : α → Option β} {h : β → γ} :
    ∀ (l : List α), l.findSome? (fun a => (g a).map h) = (l.findSome? g).map h := by
  intro l
  induction l with
  | nil => rfl
  | cons a l ih =>
      rw [List.findSome?_cons, List.findSome?_cons]
      cases hg : g a with
      | none => simpa using ih
      | some b => simp

private theorem get?_ofList_map_snd {β γ : Type} (h : β → γ) (l : List (String × β))
    (k : String) :
    (HashMap.ofList (l.map fun p => (p.1, h p.2))).get? k
      = ((HashMap.ofList l).get? k).map h := by
  rw [HashMap.get?_eq_getElem?, HashMap.get?_eq_getElem?,
      HashMap.ofList_eq_insertMany_empty, HashMap.ofList_eq_insertMany_empty,
      HashMap.getElem?_insertMany_list, HashMap.getElem?_insertMany_list,
      HashMap.getElem?_empty, HashMap.getElem?_empty, Option.or_none, Option.or_none,
      List.findSomeRev?_eq_findSome?_reverse, List.findSomeRev?_eq_findSome?_reverse,
      ← List.map_reverse, List.findSome?_map]
  have hfn : ((fun x : String × γ => if x.1 == k then some x.2 else none) ∘
            fun p : String × β => (p.1, h p.2))
        = fun p : String × β => ((if p.1 == k then some p.2 else none).map h) := by
    funext p
    by_cases hp : p.1 == k <;> simp [Function.comp, hp]
  rw [hfn, findSome?_option_map]

/-! ## The raw DAG symbolic evaluator and its simulation -/

open Dag

/-- The simulation relation between the DAG evaluator's index
environment and `symExp`'s tree environment: pointwise readings, plus
range. -/
structure EnvSim (d : Dag) (ρD : HashMap String Nat) (ρS : HashMap String NF) : Prop where
  reads : ∀ x, ρS.get? x = (ρD.get? x).map d.read
  inRange : ∀ x i, ρD.get? x = some i → i < d.size

theorem EnvSim.mono {d d' : Dag} {ρD : HashMap String Nat} {ρS : HashMap String NF}
    (h : EnvSim d ρD ρS) (hext : d.Ext d') : EnvSim d' ρD ρS := by
  refine ⟨fun x => ?_, fun x i hi => Nat.lt_of_lt_of_le (h.inRange x i hi) hext.size_le⟩
  rw [h.reads x]
  cases hx : ρD.get? x with
  | none => rfl
  | some i =>
      simp only [Option.map_some]
      rw [read_ext hext i (h.inRange x i hx)]

theorem EnvSim.insert {d : Dag} {ρD : HashMap String Nat} {ρS : HashMap String NF}
    (h : EnvSim d ρD ρS) (x : String) {i : Nat} (hi : i < d.size) :
    EnvSim d (ρD.insert x i) (ρS.insert x (d.read i)) := by
  refine ⟨fun y => ?_, fun y j hj => ?_⟩
  · rw [HashMap.get?_eq_getElem?, HashMap.get?_eq_getElem?, HashMap.getElem?_insert,
        HashMap.getElem?_insert]
    split
    · rfl
    · rw [← HashMap.get?_eq_getElem?, ← HashMap.get?_eq_getElem?]
      exact h.reads y
  · rw [HashMap.get?_eq_getElem?, HashMap.getElem?_insert] at hj
    split at hj
    · injection hj with hj
      subst hj
      exact hi
    · exact h.inRange y j (by rw [HashMap.get?_eq_getElem?]; exact hj)

theorem EnvSim.zip {d : Dag} (ps : List String) (ns : List Nat)
    (hns : ∀ n ∈ ns, n < d.size) :
    EnvSim d (HashMap.ofList (ps.zip ns)) (HashMap.ofList (ps.zip (ns.map d.read))) := by
  refine ⟨fun x => ?_, fun x i hx => ?_⟩
  · have hz : ps.zip (ns.map d.read) = (ps.zip ns).map fun q => (q.1, d.read q.2) := by
      rw [List.zip_map_right]
      exact List.map_congr_left fun q _ => rfl
    rw [hz, get?_ofList_map_snd]
  · exact hns i (List.of_mem_zip (ofList_get?_some hx)).2

/-- State-threaded `mapM` over the store. -/
def mapDag (f : Exp → Dag → Except String (Dag × Nat)) :
    List Exp → Dag → Except String (Dag × List Nat)
  | [], d => .ok (d, [])
  | e :: es, d => do
      let (d₁, n) ← f e d
      let (d₂, ns) ← mapDag f es d₁
      .ok (d₂, n :: ns)

/-- The DAG symbolic evaluator: `symExp` raw-node-for-raw-node, with
lets binding indices and calls inlined through the definition map —
the store is the only place terms live, so sharing through lets and
hash-consing replaces `symExp`'s tree duplication. The only additions
are the width-discipline checks that keep the store's `width_coh`
(`symExp` has no analogous checks, so they can only make the DAG
evaluator fail more often — never differently). -/
def symExpDag (dmap : HashMap String Defn) (X : Sem.XEnv) :
    Nat → HashMap String Nat → Exp → Dag → Except String (Dag × Nat)
  | 0, _, _, _ => .error "symExpDag: out of fuel"
  | fuel + 1, ρ, e, d =>
    match e with
    | .lit v => .ok (d.mkLit v)
    | .undef w => .ok (d.mkLit (BV.zero w))
    | .var _ x =>
        match ρ.get? x with
        | some i => .ok (d, i)
        | none => .error s!"unbound variable {x}"
    | .cat e₁ e₂ => do
        let (d₁, n₁) ← symExpDag dmap X fuel ρ e₁ d
        let (d₂, n₂) ← symExpDag dmap X fuel ρ e₂ d₁
        .ok (d₂.rawCat n₁ n₂)
    | .slice i w e => do
        let (d₁, n) ← symExpDag dmap X fuel ρ e d
        .ok (d₁.rawSlice i w n)
    | .prim _ op args => do
        let (d₁, ns) ← mapDag (symExpDag dmap X fuel ρ) args d
        match ns with
        | [a] =>
            if opArity op = 1 then .ok (d₁.rawPrim1 op a) else .error "prim: arity mismatch"
        | [a, b] =>
            if opArity op = 2 then .ok (d₁.rawPrim2 op a b) else .error "prim: arity mismatch"
        | _ => .error "prim: arity mismatch"
    | .call _ f args => do
        let (d₁, ns) ← mapDag (symExpDag dmap X fuel ρ) args d
        match dmap.get? f with
        | none => .error s!"unknown definition {f}"
        | some dn =>
            if ns.length = dn.params.length then
              symExpDag dmap X fuel (HashMap.ofList (dn.params.zip ns)) dn.body d₁
            else .error s!"{f}: arity mismatch"
    | .xcall w ext gs args => do
        let (d₁, ns) ← mapDag (symExpDag dmap X fuel ρ) args d
        match X.get? ext with
        | some _ =>
            .error s!"extern {ext} has a model: the model-carrying validator row is out of scope"
        | none =>
            if gs.isEmpty then
              let (d₂, pk) := d₁.xpackD ns
              .ok (d₂.mkXcallD w ext pk)
            else .error s!"extern {ext}: generic model-less externs are out of scope"
    | .ite _ c t e => do
        let (d₁, nc) ← symExpDag dmap X fuel ρ c d
        let (d₂, nt) ← symExpDag dmap X fuel ρ t d₁
        let (d₃, nu) ← symExpDag dmap X fuel ρ e d₂
        if d₃.widthOf nt = d₃.widthOf nu then .ok (d₃.rawIte nc nt nu)
        else .error "ite: arm width mismatch"
    | .letE _ x rhs body => do
        let (d₁, n) ← symExpDag dmap X fuel ρ rhs d
        symExpDag dmap X fuel (ρ.insert x n) body d₁

/-- Repackage a constructor contract at a destructured result pair. -/
private theorem mk_out {d d' : Dag} {r : Nat} {res : Dag × Nat} {nf : NF}
    (h : Mk d res nf) (heq : res = (d', r)) :
    d'.WF ∧ d.Ext d' ∧ r < d'.size ∧ d'.read r = nf := by
  rw [heq] at h
  exact ⟨h.wf, h.ext, h.lt, h.read⟩

/-- The simulation conclusion bundle. -/
private def SimOut (dmap : HashMap String Defn) (X : Sem.XEnv) (fuel : Nat)
    (ρS : HashMap String NF)
    (e : Exp) (d d' : Dag) (r : Nat) : Prop :=
  d'.WF ∧ d.Ext d' ∧ r < d'.size ∧ symExp dmap X fuel ρS e = .ok (d'.read r)

/-- The `mapDag` leg of the simulation, parameterized by the pointwise
simulation of the elements. -/
private theorem mapDag_sim {dmap : HashMap String Defn} {X : Sem.XEnv} {fuel : Nat}
    {ρD : HashMap String Nat} {ρS : HashMap String NF}
    (hpt : ∀ e (d d' : Dag) (r : Nat), d.WF → EnvSim d ρD ρS →
        symExpDag dmap X fuel ρD e d = .ok (d', r) →
        SimOut dmap X fuel ρS e d d' r) :
    ∀ (es : List Exp) (d d' : Dag) (ns : List Nat), d.WF → EnvSim d ρD ρS →
      mapDag (symExpDag dmap X fuel ρD) es d = .ok (d', ns) →
      d'.WF ∧ d.Ext d' ∧ (∀ n ∈ ns, n < d'.size) ∧
        es.mapM (symExp dmap X fuel ρS) = .ok (ns.map d'.read) := by
  intro es
  induction es with
  | nil =>
      intro d d' ns hwf henv h
      rw [mapDag] at h
      injection h with h
      injection h with h₁ h₂
      subst h₁
      subst h₂
      exact ⟨hwf, Ext.refl d, by simp, rfl⟩
  | cons e es ihe =>
      intro d d' ns hwf henv h
      rw [mapDag] at h
      obtain ⟨⟨d₁, n⟩, h₁, h⟩ := except_bind_eq_ok h
      obtain ⟨⟨d₂, ns'⟩, h₂, h⟩ := except_bind_eq_ok h
      injection h with h
      injection h with hd hns
      subst hd
      subst hns
      obtain ⟨W₁, E₁, L₁, S₁⟩ := hpt e d d₁ n hwf henv h₁
      obtain ⟨W₂, E₂, L₂, S₂⟩ := ihe d₁ d₂ ns' W₁ (henv.mono E₁) h₂
      refine ⟨W₂, E₁.trans E₂, ?_, ?_⟩
      · intro q hq
        rcases List.mem_cons.mp hq with hq | hq
        · subst hq
          exact Nat.lt_of_lt_of_le L₁ E₂.size_le
        · exact L₂ q hq
      · rw [List.mapM_cons, S₁, except_bind_ok, S₂, except_bind_ok, except_pure_def,
            List.map_cons, read_ext E₂ n L₁]

/-- THE simulation: a successful DAG evaluation certifies a successful
tree evaluation whose result is the reading of the returned index. -/
theorem symExpDag_sim {dmap : HashMap String Defn} {X : Sem.XEnv} :
    ∀ (fuel : Nat) (e : Exp) (ρD : HashMap String Nat) (ρS : HashMap String NF)
      (d d' : Dag) (r : Nat), d.WF → EnvSim d ρD ρS →
      symExpDag dmap X fuel ρD e d = .ok (d', r) →
      d'.WF ∧ d.Ext d' ∧ r < d'.size ∧ symExp dmap X fuel ρS e = .ok (d'.read r) := by
  intro fuel
  induction fuel with
  | zero =>
      intro e ρD ρS d d' r _ _ hs
      exact absurd hs (by simp [symExpDag])
  | succ fuel ih =>
      intro e ρD ρS d d' r hwf henv hs
      cases e with
      | lit v =>
          simp only [symExpDag] at hs
          injection hs with hs
          obtain ⟨W, E, L, R⟩ := mk_out (mkLit_spec hwf v) hs
          exact ⟨W, E, L, by rw [symExp, R]⟩
      | undef w =>
          simp only [symExpDag] at hs
          injection hs with hs
          obtain ⟨W, E, L, R⟩ := mk_out (mkLit_spec hwf (BV.zero w)) hs
          exact ⟨W, E, L, by rw [symExp, R]⟩
      | var w x =>
          simp only [symExpDag] at hs
          cases hx : ρD.get? x with
          | none => rw [hx] at hs; exact absurd hs (by simp)
          | some i =>
              rw [hx] at hs
              injection hs with hs
              injection hs with h₁ h₂
              rw [← h₁, ← h₂]
              refine ⟨hwf, Ext.refl _, henv.inRange x i hx, ?_⟩
              rw [symExp, henv.reads x, hx]
              rfl
      | cat e₁ e₂ =>
          simp only [symExpDag] at hs
          obtain ⟨⟨d₁, n₁⟩, h₁, hs⟩ := except_bind_eq_ok hs
          obtain ⟨⟨d₂, n₂⟩, h₂, hs⟩ := except_bind_eq_ok hs
          injection hs with hs
          obtain ⟨W₁, E₁, L₁, S₁⟩ := ih e₁ ρD ρS d d₁ n₁ hwf henv h₁
          obtain ⟨W₂, E₂, L₂, S₂⟩ := ih e₂ ρD ρS d₁ d₂ n₂ W₁ (henv.mono E₁) h₂
          have hn₁ : n₁ < d₂.size := Nat.lt_of_lt_of_le L₁ E₂.size_le
          obtain ⟨W, E, L, R⟩ := mk_out (rawCat_spec W₂ hn₁ L₂) hs
          refine ⟨W, (E₁.trans E₂).trans E, L, ?_⟩
          rw [symExp, S₁, except_bind_ok, S₂, except_bind_ok, R,
              read_ext E₂ n₁ L₁]
      | slice i w e =>
          simp only [symExpDag] at hs
          obtain ⟨⟨d₁, n⟩, h₁, hs⟩ := except_bind_eq_ok hs
          injection hs with hs
          obtain ⟨W₁, E₁, L₁, S₁⟩ := ih e ρD ρS d d₁ n hwf henv h₁
          obtain ⟨W, E, L, R⟩ := mk_out (rawSlice_spec W₁ i w L₁) hs
          exact ⟨W, E₁.trans E, L, by rw [symExp, S₁, except_bind_ok, R]⟩
      | prim w op args =>
          simp only [symExpDag] at hs
          obtain ⟨⟨d₁, ns⟩, h₁, hs⟩ := except_bind_eq_ok hs
          obtain ⟨W₁, E₁, L₁, S₁⟩ := mapDag_sim (fun e d d' r hw he hx => ih e ρD ρS d d' r hw he hx)
            args d d₁ ns hwf henv h₁
          rw [symExp]
          rw [S₁, except_bind_ok]
          match ns, hs with
          | [a], hs => ?one
          | [a, b], hs => ?two
          | [], hs => exact absurd hs (by simp)
          | _ :: _ :: _ :: _, hs => exact absurd hs (by simp)
          case one =>
            dsimp only at hs
            split at hs
            · rename_i hop
              injection hs with hs
              obtain ⟨W, E, L, R⟩ := mk_out (rawPrim1_spec W₁ (L₁ a List.mem_cons_self) hop) hs
              refine ⟨W, E₁.trans E, L, ?_⟩
              rw [List.map_cons, List.map_nil]
              dsimp only
              rw [if_pos hop, R]
            · exact absurd hs (by simp)
          case two =>
            dsimp only at hs
            split at hs
            · rename_i hop
              injection hs with hs
              obtain ⟨W, E, L, R⟩ := mk_out
                (rawPrim2_spec W₁ (L₁ a List.mem_cons_self)
                  (L₁ b (List.mem_cons_of_mem _ List.mem_cons_self)) hop) hs
              refine ⟨W, E₁.trans E, L, ?_⟩
              rw [List.map_cons, List.map_cons, List.map_nil]
              dsimp only
              rw [if_pos hop, R]
            · exact absurd hs (by simp)
      | call w f args =>
          simp only [symExpDag] at hs
          obtain ⟨⟨d₁, ns⟩, h₁, hs⟩ := except_bind_eq_ok hs
          obtain ⟨W₁, E₁, L₁, S₁⟩ := mapDag_sim (fun e d d' r hw he hx => ih e ρD ρS d d' r hw he hx)
            args d d₁ ns hwf henv h₁
          rw [symExp]
          rw [S₁, except_bind_ok]
          cases hd : dmap.get? f with
          | none => rw [hd] at hs; exact absurd hs (by simp)
          | some dn =>
              rw [hd] at hs
              dsimp only at hs
              split at hs
              · rename_i hlen
                obtain ⟨W₂, E₂, L₂, S₂⟩ := ih dn.body _ _ d₁ d' r W₁
                  (EnvSim.zip dn.params ns L₁) hs
                refine ⟨W₂, (E₁.trans E₂), L₂, ?_⟩
                dsimp only
                rw [if_pos (by rw [List.length_map]; exact hlen)]
                exact S₂
              · exact absurd hs (by simp)
      | xcall w ext gs args =>
          simp only [symExpDag] at hs
          obtain ⟨⟨d₁, ns⟩, h₁, hs⟩ := except_bind_eq_ok hs
          obtain ⟨W₁, E₁, L₁, S₁⟩ := mapDag_sim (fun e d d' r hw he hx => ih e ρD ρS d d' r hw he hx)
            args d d₁ ns hwf henv h₁
          rw [symExp]
          rw [S₁, except_bind_ok]
          cases hX : X.get? ext with
          | some m => rw [hX] at hs; exact absurd hs (by simp)
          | none =>
              rw [hX] at hs
              dsimp only at hs ⊢
              split at hs
              · rename_i hgs
                rw [if_pos hgs]
                rcases hxp : d₁.xpackD ns with ⟨d₂, pk⟩
                rw [hxp] at hs
                dsimp only at hs
                injection hs with hs
                obtain ⟨W₂, E₂, L₂, R₂⟩ := mk_out (xpackD_spec W₁ L₁) hxp
                obtain ⟨W, E, L, R⟩ := mk_out (mkXcallD_spec W₂ L₂) hs
                refine ⟨W, (E₁.trans E₂).trans E, L, ?_⟩
                rw [R, R₂]
              · exact absurd hs (by simp)
      | ite w c t e =>
          simp only [symExpDag] at hs
          obtain ⟨⟨d₁, nc⟩, h₁, hs⟩ := except_bind_eq_ok hs
          obtain ⟨⟨d₂, nt⟩, h₂, hs⟩ := except_bind_eq_ok hs
          obtain ⟨⟨d₃, nu⟩, h₃, hs⟩ := except_bind_eq_ok hs
          obtain ⟨W₁, E₁, L₁, S₁⟩ := ih c ρD ρS d d₁ nc hwf henv h₁
          obtain ⟨W₂, E₂, L₂, S₂⟩ := ih t ρD ρS d₁ d₂ nt W₁ (henv.mono E₁) h₂
          obtain ⟨W₃, E₃, L₃, S₃⟩ := ih e ρD ρS d₂ d₃ nu W₂ ((henv.mono E₁).mono E₂) h₃
          split at hs
          · rename_i harm
            injection hs with hs
            have hnc : nc < d₃.size :=
              Nat.lt_of_lt_of_le L₁ (E₂.trans E₃).size_le
            have hnt : nt < d₃.size := Nat.lt_of_lt_of_le L₂ E₃.size_le
            obtain ⟨W, E, L, R⟩ := mk_out (rawIte_spec W₃ hnc hnt L₃ harm) hs
            refine ⟨W, ((E₁.trans E₂).trans E₃).trans E, L, ?_⟩
            rw [symExp, S₁, except_bind_ok, S₂, except_bind_ok, S₃, except_bind_ok, R,
                read_ext (E₂.trans E₃) nc L₁, read_ext E₃ nt L₂]
          · exact absurd hs (by simp)
      | letE w x rhs body =>
          simp only [symExpDag] at hs
          obtain ⟨⟨d₁, n⟩, h₁, hs⟩ := except_bind_eq_ok hs
          obtain ⟨W₁, E₁, L₁, S₁⟩ := ih rhs ρD ρS d d₁ n hwf henv h₁
          obtain ⟨W₂, E₂, L₂, S₂⟩ := ih body _ _ d₁ d' r W₁
            ((henv.mono E₁).insert x L₁) hs
          exact ⟨W₂, E₁.trans E₂, L₂, by rw [symExp, S₁, except_bind_ok]; exact S₂⟩

/-! ## The DAG device step and its simulation -/

/-- `symBody`, DAG-level: the store rides in the fold state; the three
name maps hold indices. Decision structure and order mirror `symBody`
exactly. -/
def symBodyDag (dmap : HashMap String Defn) (X : Sem.XEnv) (fuel : Nat) :
    Dag × HashMap String Nat × HashMap String Nat × HashMap String Nat → Stmt →
    Except String (Dag × HashMap String Nat × HashMap String Nat × HashMap String Nat) :=
  fun (d, ρ, outs, nexts) stmt => do
    match stmt with
    | .sLet x e => do
        let (d₁, n) ← symExpDag dmap X fuel ρ e d
        pure (d₁, ρ.insert x n, outs, nexts)
    | .sOutput o e => do
        if outs.contains o then .error s!"output {o} assigned twice"
        let (d₁, n) ← symExpDag dmap X fuel ρ e d
        pure (d₁, ρ, outs.insert o n, nexts)
    | .sNext r e => do
        if nexts.contains r then .error s!"register {r} assigned twice"
        let (d₁, n) ← symExpDag dmap X fuel ρ e d
        pure (d₁, ρ, outs, nexts.insert r n)
    | .sInstIn inst _ _ =>
        .error s!"device instance {inst}: outside the instance-free fragment"

/-- `symFinish`, DAG-level. -/
def symFinishDag (dev : Device) :
    Dag × HashMap String Nat × HashMap String Nat × HashMap String Nat →
    Except String (Dag × List (String × Nat) × List (String × Nat)) :=
  fun (d, _, outs, nexts) => do
    let outsL ← dev.outputs.mapM fun (o, _) =>
      match outs.get? o with
      | some n => pure (o, n)
      | none => .error s!"output {o} never assigned"
    let nextsL ← dev.registers.mapM fun r =>
      match nexts.get? r.name with
      | some n => pure (r.name, n)
      | none => .error s!"register {r.name} never assigned"
    pure (d, outsL, nextsL)

/-- `initSymEnv`, DAG-level: push one variable node per input and
register. -/
def initSymEnvDag (dev : Device) (d : Dag) : Dag × HashMap String Nat :=
  (dev.inputs ++ dev.registers.map fun r => (r.name, r.width)).foldl
    (fun (acc : Dag × HashMap String Nat) p =>
      ((acc.1.mkVar p.2 p.1).1, acc.2.insert p.1 (acc.1.mkVar p.2 p.1).2)) (d, ∅)

/-- The DAG device step: seed the shared store `d` with the interface
variables, fold the body, read off outputs and register nexts. -/
def symStepDag (dmap : HashMap String Defn) (X : Sem.XEnv) (fuel : Nat) (dev : Device)
    (d : Dag) :
    Except String (Dag × List (String × Nat) × List (String × Nat)) :=
  dev.body.foldlM (symBodyDag dmap X fuel)
    ((initSymEnvDag dev d).1, (initSymEnvDag dev d).2, ∅, ∅) >>= symFinishDag dev

/-- `contains` transports along the simulation relation. -/
theorem EnvSim.contains_eq {d : Dag} {mD : HashMap String Nat} {mS : HashMap String NF}
    (h : EnvSim d mD mS) (k : String) : mS.contains k = mD.contains k := by
  rw [HashMap.contains_eq_isSome_getElem?, HashMap.contains_eq_isSome_getElem?,
      ← HashMap.get?_eq_getElem?, ← HashMap.get?_eq_getElem?, h.reads k]
  cases mD.get? k <;> rfl

theorem EnvSim.empty (d : Dag) : EnvSim d (∅ : HashMap String Nat) (∅ : HashMap String NF) := by
  refine ⟨fun x => ?_, fun x i h => ?_⟩
  · rw [HashMap.get?_eq_getElem?, HashMap.get?_eq_getElem?, HashMap.getElem?_empty,
        HashMap.getElem?_empty]
    rfl
  · rw [HashMap.get?_eq_getElem?, HashMap.getElem?_empty] at h
    exact absurd h (by simp)

/-- The initial-environment fold, simulated. -/
private theorem initFold_sim :
    ∀ (l : List (String × Nat)) (d : Dag) (ρD : HashMap String Nat) (ρS : HashMap String NF),
      d.WF → EnvSim d ρD ρS →
      (l.foldl (fun (acc : Dag × HashMap String Nat) p =>
          ((acc.1.mkVar p.2 p.1).1, acc.2.insert p.1 (acc.1.mkVar p.2 p.1).2)) (d, ρD)).1.WF ∧
      d.Ext (l.foldl (fun (acc : Dag × HashMap String Nat) p =>
          ((acc.1.mkVar p.2 p.1).1, acc.2.insert p.1 (acc.1.mkVar p.2 p.1).2)) (d, ρD)).1 ∧
      EnvSim (l.foldl (fun (acc : Dag × HashMap String Nat) p =>
          ((acc.1.mkVar p.2 p.1).1, acc.2.insert p.1 (acc.1.mkVar p.2 p.1).2)) (d, ρD)).1
        (l.foldl (fun (acc : Dag × HashMap String Nat) p =>
          ((acc.1.mkVar p.2 p.1).1, acc.2.insert p.1 (acc.1.mkVar p.2 p.1).2)) (d, ρD)).2
        (l.foldl (fun ρ p => ρ.insert p.1 (NF.var p.2 p.1)) ρS) := by
  intro l
  induction l with
  | nil =>
      intro d ρD ρS hwf henv
      exact ⟨hwf, Ext.refl d, henv⟩
  | cons p l ihl =>
      intro d ρD ρS hwf henv
      rw [List.foldl_cons, List.foldl_cons]
      have M := mkVar_spec hwf p.2 p.1
      have henv' : EnvSim (d.mkVar p.2 p.1).1 (ρD.insert p.1 (d.mkVar p.2 p.1).2)
          (ρS.insert p.1 (NF.var p.2 p.1)) := by
        have h₁ := (henv.mono M.ext).insert p.1 M.lt
        rw [M.read] at h₁
        exact h₁
      obtain ⟨W, E, S⟩ := ihl (d.mkVar p.2 p.1).1 _ _ M.wf henv'
      exact ⟨W, M.ext.trans E, S⟩

/-- The body fold, simulated: a successful DAG fold certifies a
successful tree fold with corresponding components. -/
private theorem bodyFold_sim {dmap : HashMap String Defn} {X : Sem.XEnv} {fuel : Nat} :
    ∀ (stmts : List Stmt) (d : Dag) (ρD outsD nextsD : HashMap String Nat)
      (ρS outsS nextsS : HashMap String NF)
      (res : Dag × HashMap String Nat × HashMap String Nat × HashMap String Nat),
      d.WF → EnvSim d ρD ρS → EnvSim d outsD outsS → EnvSim d nextsD nextsS →
      stmts.foldlM (symBodyDag dmap X fuel) (d, ρD, outsD, nextsD) = .ok res →
      ∃ ρS' outsS' nextsS',
        stmts.foldlM (symBody dmap X fuel) (ρS, outsS, nextsS) = .ok (ρS', outsS', nextsS') ∧
        res.1.WF ∧ d.Ext res.1 ∧ EnvSim res.1 res.2.1 ρS' ∧
        EnvSim res.1 res.2.2.1 outsS' ∧ EnvSim res.1 res.2.2.2 nextsS' := by
  intro stmts
  induction stmts with
  | nil =>
      intro d ρD outsD nextsD ρS outsS nextsS res hwf hρ ho hn h
      rw [List.foldlM_nil] at h
      injection h with h
      subst h
      exact ⟨ρS, outsS, nextsS, rfl, hwf, Ext.refl d, hρ, ho, hn⟩
  | cons stmt stmts ih =>
      intro d ρD outsD nextsD ρS outsS nextsS res hwf hρ ho hn h
      rw [List.foldlM_cons] at h
      obtain ⟨⟨d₁, ρD₁, outsD₁, nextsD₁⟩, hbody, hrest⟩ := except_bind_eq_ok h
      rw [List.foldlM_cons]
      cases stmt with
      | sLet x e =>
          dsimp only [symBodyDag] at hbody
          obtain ⟨⟨d₂, n⟩, hne, hpure⟩ := except_bind_eq_ok hbody
          rw [except_pure_def] at hpure
          injection hpure with hpure
          injection hpure with h₁ h₂
          injection h₂ with h₂ h₃
          injection h₃ with h₃ h₄
          subst h₁; subst h₂; subst h₃; subst h₄
          obtain ⟨W₁, E₁, L₁, S₁⟩ := symExpDag_sim fuel e ρD ρS d d₂ n hwf hρ hne
          obtain ⟨ρS', outsS', nextsS', hfold, hout⟩ := ih d₂ _ _ _
            (ρS.insert x (d₂.read n)) outsS nextsS res W₁
            ((hρ.mono E₁).insert x L₁) (ho.mono E₁) (hn.mono E₁) hrest
          refine ⟨ρS', outsS', nextsS', ?_, hout.1, E₁.trans hout.2.1, hout.2.2⟩
          dsimp only [symBody]
          rw [S₁, except_bind_ok, except_pure_def, except_bind_ok]
          exact hfold
      | sOutput o e =>
          dsimp only [symBodyDag] at hbody
          cases hcont : outsD.contains o with
          | true => rw [hcont] at hbody; exact nomatch hbody
          | false =>
              rw [hcont] at hbody
              simp only [Bool.false_eq_true, if_false] at hbody
              obtain ⟨⟨d₂, n⟩, hne, hpure⟩ := except_bind_eq_ok hbody
              rw [except_pure_def] at hpure
              injection hpure with hpure
              injection hpure with h₁ h₂
              injection h₂ with h₂ h₃
              injection h₃ with h₃ h₄
              subst h₁; subst h₂; subst h₃; subst h₄
              obtain ⟨W₁, E₁, L₁, S₁⟩ := symExpDag_sim fuel e ρD ρS d d₂ n hwf hρ hne
              obtain ⟨ρS', outsS', nextsS', hfold, hout⟩ := ih d₂ _ _ _
                ρS (outsS.insert o (d₂.read n)) nextsS res W₁
                (hρ.mono E₁) ((ho.mono E₁).insert o L₁) (hn.mono E₁) hrest
              refine ⟨ρS', outsS', nextsS', ?_, hout.1, E₁.trans hout.2.1, hout.2.2⟩
              dsimp only [symBody]
              rw [ho.contains_eq o, hcont]
              simp only [Bool.false_eq_true, if_false]
              rw [S₁, except_bind_ok, except_pure_def, except_bind_ok]
              exact hfold
      | sNext r e =>
          dsimp only [symBodyDag] at hbody
          cases hcont : nextsD.contains r with
          | true => rw [hcont] at hbody; exact nomatch hbody
          | false =>
              rw [hcont] at hbody
              simp only [Bool.false_eq_true, if_false] at hbody
              obtain ⟨⟨d₂, n⟩, hne, hpure⟩ := except_bind_eq_ok hbody
              rw [except_pure_def] at hpure
              injection hpure with hpure
              injection hpure with h₁ h₂
              injection h₂ with h₂ h₃
              injection h₃ with h₃ h₄
              subst h₁; subst h₂; subst h₃; subst h₄
              obtain ⟨W₁, E₁, L₁, S₁⟩ := symExpDag_sim fuel e ρD ρS d d₂ n hwf hρ hne
              obtain ⟨ρS', outsS', nextsS', hfold, hout⟩ := ih d₂ _ _ _
                ρS outsS (nextsS.insert r (d₂.read n)) res W₁
                (hρ.mono E₁) (ho.mono E₁) ((hn.mono E₁).insert r L₁) hrest
              refine ⟨ρS', outsS', nextsS', ?_, hout.1, E₁.trans hout.2.1, hout.2.2⟩
              dsimp only [symBody]
              rw [hn.contains_eq r, hcont]
              simp only [Bool.false_eq_true, if_false]
              rw [S₁, except_bind_ok, except_pure_def, except_bind_ok]
              exact hfold
      | sInstIn inst port e =>
          dsimp only [symBodyDag] at hbody
          exact absurd hbody (by simp)

/-- The output read-off, simulated. -/
private theorem outs_mapM_sim {d : Dag} {mD : HashMap String Nat} {mS : HashMap String NF}
    (hsim : EnvSim d mD mS) :
    ∀ (l : List (String × Nat)) (L : List (String × Nat)),
      l.mapM (fun (p : String × Nat) =>
        match mD.get? p.1 with
        | some n => pure (p.1, n)
        | none => Except.error s!"output {p.1} never assigned") = .ok L →
      (∀ q ∈ L, q.2 < d.size) ∧
      l.mapM (fun (p : String × Nat) =>
        match mS.get? p.1 with
        | some n => pure (p.1, n)
        | none => Except.error s!"output {p.1} never assigned")
        = .ok (L.map fun q => (q.1, d.read q.2)) := by
  intro l
  induction l with
  | nil =>
      intro L h
      rw [List.mapM_nil, except_pure_def] at h
      injection h with h
      subst h
      exact ⟨by simp, rfl⟩
  | cons p l ihl =>
      intro L h
      rw [List.mapM_cons] at h
      obtain ⟨q, hq, h⟩ := except_bind_eq_ok h
      obtain ⟨L', hL, h⟩ := except_bind_eq_ok h
      rw [except_pure_def] at h
      injection h with h
      subst h
      cases hget : mD.get? p.1 with
      | none => rw [hget] at hq; exact absurd hq (by simp)
      | some n =>
          rw [hget] at hq
          injection hq with hq
          subst hq
          obtain ⟨hlt, hmapM⟩ := ihl L' hL
          have hn : n < d.size := hsim.inRange p.1 n hget
          refine ⟨?_, ?_⟩
          · intro q hq
            rcases List.mem_cons.mp hq with hq | hq
            · subst hq
              exact hn
            · exact hlt q hq
          · have helem : (match mS.get? p.1 with
                | some n => pure (p.1, n)
                | none => Except.error s!"output {p.1} never assigned")
                = (Except.ok (p.1, d.read n) : Except String (String × NF)) := by
              rw [hsim.reads p.1, hget]
              rfl
            rw [List.mapM_cons, helem, except_bind_ok, hmapM, except_bind_ok,
                except_pure_def, List.map_cons]

/-- The register-next read-off, simulated. -/
private theorem nexts_mapM_sim {d : Dag} {mD : HashMap String Nat} {mS : HashMap String NF}
    (hsim : EnvSim d mD mS) :
    ∀ (l : List Register) (L : List (String × Nat)),
      l.mapM (fun r =>
        match mD.get? r.name with
        | some n => pure (r.name, n)
        | none => Except.error s!"register {r.name} never assigned") = .ok L →
      (∀ q ∈ L, q.2 < d.size) ∧
      l.mapM (fun r =>
        match mS.get? r.name with
        | some n => pure (r.name, n)
        | none => Except.error s!"register {r.name} never assigned")
        = .ok (L.map fun q => (q.1, d.read q.2)) := by
  intro l
  induction l with
  | nil =>
      intro L h
      rw [List.mapM_nil, except_pure_def] at h
      injection h with h
      subst h
      exact ⟨by simp, rfl⟩
  | cons r l ihl =>
      intro L h
      rw [List.mapM_cons] at h
      obtain ⟨q, hq, h⟩ := except_bind_eq_ok h
      obtain ⟨L', hL, h⟩ := except_bind_eq_ok h
      rw [except_pure_def] at h
      injection h with h
      subst h
      cases hget : mD.get? r.name with
      | none => rw [hget] at hq; exact absurd hq (by simp)
      | some n =>
          rw [hget] at hq
          injection hq with hq
          subst hq
          obtain ⟨hlt, hmapM⟩ := ihl L' hL
          have hn : n < d.size := hsim.inRange r.name n hget
          refine ⟨?_, ?_⟩
          · intro q hq
            rcases List.mem_cons.mp hq with hq | hq
            · subst hq
              exact hn
            · exact hlt q hq
          · have helem : (match mS.get? r.name with
                | some n => pure (r.name, n)
                | none => Except.error s!"register {r.name} never assigned")
                = (Except.ok (r.name, d.read n) : Except String (String × NF)) := by
              rw [hsim.reads r.name, hget]
              rfl
            rw [List.mapM_cons, helem, except_bind_ok, hmapM, except_bind_ok,
                except_pure_def, List.map_cons]

/-- The step simulation: a successful DAG step certifies a successful
tree `symStep` whose per-output and per-next trees are the readings of
the returned indices. -/
theorem symStepDag_sim {dmap : HashMap String Defn} {X : Sem.XEnv} {fuel : Nat}
    {dev : Device}
    {d d' : Dag} {outsL nextsL : List (String × Nat)} (hwf : d.WF)
    (h : symStepDag dmap X fuel dev d = .ok (d', outsL, nextsL)) :
    d'.WF ∧ d.Ext d' ∧ (∀ p ∈ outsL, p.2 < d'.size) ∧ (∀ p ∈ nextsL, p.2 < d'.size) ∧
    symStep dmap X fuel dev
      = .ok ⟨outsL.map (fun p => (p.1, d'.read p.2)),
             nextsL.map (fun p => (p.1, d'.read p.2))⟩ := by
  rw [symStepDag] at h
  obtain ⟨⟨d₁, ρD₁, outsD₁, nextsD₁⟩, hfold, hfin⟩ := except_bind_eq_ok h
  obtain ⟨W₀, E₀, henv₀⟩ := initFold_sim
    (dev.inputs ++ dev.registers.map fun r => (r.name, r.width)) d ∅ ∅ hwf (EnvSim.empty d)
  obtain ⟨ρS', outsS', nextsS', hfoldS, W₁, E₁, hρ₁, ho₁, hn₁⟩ :=
    bodyFold_sim dev.body _ _ _ _ _ _ _ (res := (d₁, ρD₁, outsD₁, nextsD₁))
      W₀ henv₀ (EnvSim.empty _) (EnvSim.empty _) hfold
  dsimp only [symFinishDag] at hfin
  obtain ⟨outsL', hoL, hfin⟩ := except_bind_eq_ok hfin
  obtain ⟨nextsL', hnL, hfin⟩ := except_bind_eq_ok hfin
  rw [except_pure_def] at hfin
  injection hfin with hfin
  injection hfin with h₁ h₂
  injection h₂ with h₂ h₃
  subst h₁; subst h₂; subst h₃
  obtain ⟨hoRange, hoS⟩ := outs_mapM_sim ho₁ dev.outputs outsL' hoL
  obtain ⟨hnRange, hnS⟩ := nexts_mapM_sim hn₁ dev.registers nextsL' hnL
  refine ⟨W₁, E₀.trans E₁, hoRange, hnRange, ?_⟩
  rw [symStep, initSymEnv, hfoldS, except_bind_ok]
  show (do
      let outsL ← dev.outputs.mapM fun (p : String × Nat) =>
        match outsS'.get? p.1 with
        | some n => pure (p.1, n)
        | none => Except.error s!"output {p.1} never assigned"
      let nextsL ← dev.registers.mapM fun r =>
        match nextsS'.get? r.name with
        | some n => pure (r.name, n)
        | none => Except.error s!"register {r.name} never assigned"
      pure (⟨outsL, nextsL⟩ : StepNF))
    = Except.ok ⟨outsL'.map (fun p => (p.1, d₁.read p.2)),
                 nextsL'.map (fun p => (p.1, d₁.read p.2))⟩
  rw [hoS, except_bind_ok, hnS, except_bind_ok, except_pure_def]

/-! ## Store renormalization: one `cfoldW` round over the whole store

`renorm` sweeps the store in index order, rebuilding every node
through the rewrite constructors with already-renormalized children
(children strictly precede parents). Its spec is the syntactic
mirroring `read (m[i]) = NF.cfoldW (read i)`; three sweeps therefore
read to exactly `cfoldW3`, the normalizer `checkEquivW` compares
under. The runtime arity/arm-width checks discharge the constructor
spec hypotheses; on `WF` stores fed by `symExpDag` they never fire. -/

/-- The renormalized image of an index (0 out of range — the spec
carries range). -/
def mIdx (m : Array Nat) (i : Nat) : Nat := m[i]?.getD 0

private theorem mIdx_eq {m : Array Nat} {a : Nat} (h : a < m.size) : mIdx m a = m[a] := by
  rw [mIdx, Array.getElem?_eq_getElem h]
  rfl

/-- Renormalize one node, children through the map. -/
def renormNode (d : Dag) (m : Array Nat) : DNode → Except String (Dag × Nat)
  | .var w x => .ok (d.mkVar w x)
  | .lit v => .ok (d.mkLit v)
  | .prim1 _ op a =>
      if opArity op = 1 then .ok (d.mk1D op (mIdx m a))
      else .error "renorm: prim1 arity"
  | .prim2 _ op a b =>
      if opArity op = 2 then .ok (d.mk2D op (mIdx m a) (mIdx m b))
      else .error "renorm: prim2 arity"
  | .cat _ a b => .ok (d.mkCatD (mIdx m a) (mIdx m b))
  | .slice i w e => .ok (d.mkSliceD i w (mIdx m e))
  | .ite _ c t e =>
      if d.widthOf (mIdx m t) = d.widthOf (mIdx m e) then
        .ok (d.mkIteD (mIdx m c) (mIdx m t) (mIdx m e))
      else .error "renorm: ite arm widths"
  | .xcall w ext a => .ok (d.mkXcallD w ext (mIdx m a))

def renormGo : Nat → Dag → Array Nat → Except String (Dag × Array Nat)
  | 0, d, m => .ok (d, m)
  | k + 1, d, m =>
      match d.nodes[m.size]? with
      | none => .error "renorm: index out of range"
      | some n => do
          let (d₁, r) ← renormNode d m n
          renormGo k d₁ (m.push r)

/-- One full `cfoldW` sweep of the store. -/
def renorm (d : Dag) : Except String (Dag × Array Nat) := renormGo d.size d #[]

private theorem renormNode_spec {d₀ dc : Dag} {m : Array Nat} {n : DNode} {i : Nat}
    (hwf₀ : d₀.WF) (hwfc : dc.WF) (_hext : d₀.Ext dc)
    (_hi : i < d₀.size) (hm : i = m.size) (hn : d₀.nodes[i]? = some n)
    (hinv : ∀ j, (hj : j < m.size) → m[j] < dc.size ∧ dc.read m[j] = NF.cfoldW (d₀.read j))
    {d₁ : Dag} {r : Nat} (h : renormNode dc m n = .ok (d₁, r)) :
    d₁.WF ∧ dc.Ext d₁ ∧ r < d₁.size ∧ d₁.read r = NF.cfoldW (d₀.read i) := by
  have hread : d₀.read i = n.toNF d₀.read := read_eq (hwf₀.child_lt i n hn) hn
  have hchild : ∀ j ∈ n.children, j < m.size := fun j hj => by
    rw [← hm]
    exact hwf₀.child_lt i n hn j hj
  cases n with
  | var w x =>
      rw [renormNode] at h
      injection h with h
      obtain ⟨W, E, L, R⟩ := mk_out (mkVar_spec hwfc w x) h
      refine ⟨W, E, L, ?_⟩
      rw [R, hread]
      rfl
  | lit v =>
      rw [renormNode] at h
      injection h with h
      obtain ⟨W, E, L, R⟩ := mk_out (mkLit_spec hwfc v) h
      refine ⟨W, E, L, ?_⟩
      rw [R, hread]
      rfl
  | prim1 w op a =>
      rw [renormNode] at h
      split at h
      · rename_i hop
        injection h with h
        have ha : a < m.size := hchild a (by simp [DNode.children])
        obtain ⟨hra, hca⟩ := hinv a ha
        rw [mIdx_eq ha] at h
        obtain ⟨W, E, L, R⟩ := mk_out (mk1D_spec hwfc hra hop) h
        refine ⟨W, E, L, ?_⟩
        rw [R, hca, hread]
        show NF.mk1W op (NF.cfoldW (d₀.read a)) = NF.cfoldW (NF.prim1 op (d₀.read a))
        simp only [NF.cfoldW]
      · exact absurd h (by simp)
  | prim2 w op a b =>
      rw [renormNode] at h
      split at h
      · rename_i hop
        injection h with h
        have ha : a < m.size := hchild a (by simp [DNode.children])
        have hb : b < m.size := hchild b (by simp [DNode.children])
        obtain ⟨hra, hca⟩ := hinv a ha
        obtain ⟨hrb, hcb⟩ := hinv b hb
        rw [mIdx_eq ha, mIdx_eq hb] at h
        obtain ⟨W, E, L, R⟩ := mk_out (mk2D_spec hwfc hra hrb hop) h
        refine ⟨W, E, L, ?_⟩
        rw [R, hca, hcb, hread]
        show NF.mk2W op (NF.cfoldW (d₀.read a)) (NF.cfoldW (d₀.read b))
          = NF.cfoldW (NF.prim2 op (d₀.read a) (d₀.read b))
        simp only [NF.cfoldW]
      · exact absurd h (by simp)
  | cat w a b =>
      rw [renormNode] at h
      injection h with h
      have ha : a < m.size := hchild a (by simp [DNode.children])
      have hb : b < m.size := hchild b (by simp [DNode.children])
      obtain ⟨hra, hca⟩ := hinv a ha
      obtain ⟨hrb, hcb⟩ := hinv b hb
      rw [mIdx_eq ha, mIdx_eq hb] at h
      obtain ⟨W, E, L, R⟩ := mk_out (mkCatD_spec hwfc hra hrb) h
      refine ⟨W, E, L, ?_⟩
      rw [R, hca, hcb, hread]
      show NF.mkCatW (NF.cfoldW (d₀.read a)) (NF.cfoldW (d₀.read b))
        = NF.cfoldW (NF.cat (d₀.read a) (d₀.read b))
      simp only [NF.cfoldW]
  | slice j w e =>
      rw [renormNode] at h
      injection h with h
      have he : e < m.size := hchild e (by simp [DNode.children])
      obtain ⟨hre, hce⟩ := hinv e he
      rw [mIdx_eq he] at h
      obtain ⟨W, E, L, R⟩ := mk_out (mkSliceD_spec m[e] dc j w hwfc hre) h
      refine ⟨W, E, L, ?_⟩
      rw [R, hce, hread]
      show NF.mkSliceW j w (NF.cfoldW (d₀.read e)) = NF.cfoldW (NF.slice j w (d₀.read e))
      simp only [NF.cfoldW]
  | ite w c t e =>
      rw [renormNode] at h
      split at h
      · rename_i harm
        injection h with h
        have hc : c < m.size := hchild c (by simp [DNode.children])
        have ht : t < m.size := hchild t (by simp [DNode.children])
        have he : e < m.size := hchild e (by simp [DNode.children])
        obtain ⟨hrc, hcc⟩ := hinv c hc
        obtain ⟨hrt, hct⟩ := hinv t ht
        obtain ⟨hre, hce⟩ := hinv e he
        rw [mIdx_eq ht, mIdx_eq he] at harm
        rw [mIdx_eq hc, mIdx_eq ht, mIdx_eq he] at h
        obtain ⟨W, E, L, R⟩ := mk_out (mkIteD_spec hwfc hrc hrt hre harm) h
        refine ⟨W, E, L, ?_⟩
        rw [R, hcc, hct, hce, hread]
        show NF.mkIteW (NF.cfoldW (d₀.read c)) (NF.cfoldW (d₀.read t)) (NF.cfoldW (d₀.read e))
          = NF.cfoldW (NF.ite (d₀.read c) (d₀.read t) (d₀.read e))
        simp only [NF.cfoldW]
      · exact absurd h (by simp)
  | xcall w ext a =>
      rw [renormNode] at h
      injection h with h
      have ha : a < m.size := hchild a (by simp [DNode.children])
      obtain ⟨hra, hca⟩ := hinv a ha
      rw [mIdx_eq ha] at h
      obtain ⟨W, E, L, R⟩ := mk_out (mkXcallD_spec hwfc hra) h
      refine ⟨W, E, L, ?_⟩
      rw [R, hca, hread]
      show NF.xcall w ext (NF.cfoldW (d₀.read a)) = NF.cfoldW (NF.xcall w ext (d₀.read a))
      simp only [NF.cfoldW]

private theorem renormGo_spec :
    ∀ (k : Nat) (d₀ dc : Dag) (m : Array Nat) (d' : Dag) (m' : Array Nat),
      d₀.WF → dc.WF → d₀.Ext dc → m.size + k = d₀.size →
      (∀ j, (hj : j < m.size) → m[j] < dc.size ∧ dc.read m[j] = NF.cfoldW (d₀.read j)) →
      renormGo k dc m = .ok (d', m') →
      d'.WF ∧ dc.Ext d' ∧ m'.size = d₀.size ∧
      (∀ j, (hj : j < m'.size) → m'[j] < d'.size ∧ d'.read m'[j] = NF.cfoldW (d₀.read j)) := by
  intro k
  induction k with
  | zero =>
      intro d₀ dc m d' m' hwf₀ hwfc hext hsz hinv h
      rw [renormGo] at h
      injection h with h
      injection h with h₁ h₂
      subst h₁
      subst h₂
      exact ⟨hwfc, Ext.refl dc, by omega, hinv⟩
  | succ k ih =>
      intro d₀ dc m d' m' hwf₀ hwfc hext hsz hinv h
      rw [renormGo] at h
      have hi : m.size < d₀.size := by omega
      obtain ⟨n, hn⟩ := node_of_lt hi
      have hnc : dc.nodes[m.size]? = some n := by
        rw [hext.nodes_eq m.size hi]
        exact hn
      rw [hnc] at h
      obtain ⟨⟨d₁, r⟩, hrn, h⟩ := except_bind_eq_ok h
      obtain ⟨W₁, E₁, L₁, R₁⟩ :=
        renormNode_spec hwf₀ hwfc hext hi rfl hn hinv hrn
      have hinv' : ∀ j, (hj : j < (m.push r).size) →
          (m.push r)[j] < d₁.size ∧ d₁.read (m.push r)[j] = NF.cfoldW (d₀.read j) := by
        intro j hj
        rw [Array.size_push] at hj
        by_cases hjm : j < m.size
        · rw [Array.getElem_push_lt hjm]
          obtain ⟨hr, hc⟩ := hinv j hjm
          exact ⟨Nat.lt_of_lt_of_le hr E₁.size_le, by rw [read_ext E₁ _ hr, hc]⟩
        · have hj' : j = m.size := by omega
          subst hj'
          rw [Array.getElem_push_eq]
          exact ⟨L₁, R₁⟩
      obtain ⟨W₂, E₂, hsz₂, hinv₂⟩ := ih d₀ d₁ (m.push r) d' m' hwf₀ W₁
        (hext.trans E₁) (by rw [Array.size_push]; omega) hinv' h
      exact ⟨W₂, E₁.trans E₂, hsz₂, hinv₂⟩

theorem renorm_spec {d : Dag} {d' : Dag} {m : Array Nat} (hwf : d.WF)
    (h : renorm d = .ok (d', m)) :
    d'.WF ∧ d.Ext d' ∧ m.size = d.size ∧
    (∀ j, j < d.size → mIdx m j < d'.size ∧ d'.read (mIdx m j) = NF.cfoldW (d.read j)) := by
  obtain ⟨W, E, hsz, hinv⟩ := renormGo_spec d.size d d #[] d' m hwf hwf (Ext.refl d)
    (by simp) (by intro j hj; simp at hj) h
  refine ⟨W, E, hsz, ?_⟩
  intro j hj
  have hj' : j < m.size := by omega
  rw [mIdx_eq hj']
  exact hinv j hj'

/-! ## The DAG equivalence checkers

Both sides are evaluated into ONE shared store (nothing is cleared
between sides, so identical subterms — across sides included — share
indices), and the verdict on each output/next is INDEX equality. The
width-aware tier (`checkEquivDag`) compares after three `renorm`
sweeps; the rewrite-free tier (`checkEquivDagRaw`) compares the raw
step indices. Soundness is by reduction: a `true` verdict implies the
corresponding tree checker's `true` verdict, so `checkEquivW_sound` /
`checkEquiv_sound` apply unchanged. -/

/-- The width-aware DAG equivalence checker. The Boolean gates mirror
`checkEquivW`'s conjunct for conjunct; the two `cfoldPairsW`
comparisons become index comparisons after renormalization, and the
`nextsWidthsOkB` gate becomes a cached-width gate on the final next
indices. -/
def checkEquivDag (p₁ p₂ : Program) : Bool :=
  match Sem.mkFEnv p₁, Sem.mkFEnv p₂,
        symStepDag (dmapOf p₁) (Sem.xenv p₁) (progFuel p₁) p₁.device Dag.empty with
  | .ok _, .ok _, .ok (d₁, outs₁, nexts₁) =>
    (match symStepDag (dmapOf p₂) (Sem.xenv p₂) (progFuel p₂) p₂.device d₁ with
    | .ok (d₂, outs₂, nexts₂) =>
      (match renorm d₂ with
      | .ok (e₁, m₁) =>
        (match renorm e₁ with
        | .ok (e₂, m₂) =>
          (match renorm e₂ with
          | .ok (e₃, m₃) =>
                 okB p₁.check && okB p₂.check
              && p₁.externs.isEmpty && p₂.externs.isEmpty
              && p₁.device.instances.isEmpty && p₂.device.instances.isEmpty
              && nodupB (p₁.defns.map (·.name)) && nodupB (p₂.defns.map (·.name))
              && nodupB (p₁.device.inputs.map Prod.fst ++ p₁.device.registers.map (·.name))
              && p₁.device.registers.all (fun r => r.init.width == r.width)
              && (p₁.device.registers.zip nexts₁).all (fun rp =>
                    rp.2.1 == rp.1.name
                    && e₃.widthOf (mIdx m₃ (mIdx m₂ (mIdx m₁ rp.2.2))) == rp.1.width)
              && decide (p₁.device.inputs = p₂.device.inputs)
              && decide (p₁.device.outputs = p₂.device.outputs)
              && decide (regTuples p₁.device.registers = regTuples p₂.device.registers)
              && decide (outs₁.map (fun p => (p.1, mIdx m₃ (mIdx m₂ (mIdx m₁ p.2))))
                       = outs₂.map (fun p => (p.1, mIdx m₃ (mIdx m₂ (mIdx m₁ p.2)))))
              && decide (nexts₁.map (fun p => (p.1, mIdx m₃ (mIdx m₂ (mIdx m₁ p.2))))
                       = nexts₂.map (fun p => (p.1, mIdx m₃ (mIdx m₂ (mIdx m₁ p.2)))))
          | .error _ => false)
        | .error _ => false)
      | .error _ => false)
    | .error _ => false)
  | _, _, _ => false

/-- The rewrite-free DAG equivalence checker (raw step indices,
`checkEquiv`'s gates). -/
def checkEquivDagRaw (p₁ p₂ : Program) : Bool :=
  match Sem.mkFEnv p₁, Sem.mkFEnv p₂,
        symStepDag (dmapOf p₁) (Sem.xenv p₁) (progFuel p₁) p₁.device Dag.empty with
  | .ok _, .ok _, .ok (d₁, outs₁, nexts₁) =>
    (match symStepDag (dmapOf p₂) (Sem.xenv p₂) (progFuel p₂) p₂.device d₁ with
    | .ok (_, outs₂, nexts₂) =>
           okB p₁.check && okB p₂.check
        && p₁.externs.isEmpty && p₂.externs.isEmpty
        && p₁.device.instances.isEmpty && p₂.device.instances.isEmpty
        && nodupB (p₁.defns.map (·.name)) && nodupB (p₂.defns.map (·.name))
        && decide (p₁.device.inputs = p₂.device.inputs)
        && decide (p₁.device.outputs = p₂.device.outputs)
        && decide (regTuples p₁.device.registers = regTuples p₂.device.registers)
        && decide (outs₁ = outs₂) && decide (nexts₁ = nexts₂)
    | .error _ => false)
  | _, _, _ => false

/-- Transport a pair list's `cfoldPairsW` through a read-chain. -/
private theorem cfoldPairsW_map {dbig efin : Dag} {fin : Nat → Nat}
    (l : List (String × Nat)) (hl : ∀ p ∈ l, p.2 < dbig.size)
    (hchain : ∀ i, i < dbig.size → efin.read (fin i) = cfoldW3 (dbig.read i)) :
    cfoldPairsW (l.map fun p => (p.1, dbig.read p.2))
      = (l.map fun p => (p.1, fin p.2)).map fun q => (q.1, efin.read q.2) := by
  rw [cfoldPairsW, List.map_map, List.map_map]
  refine List.map_congr_left fun p hp => ?_
  show (p.1, cfoldW3 (dbig.read p.2)) = (p.1, efin.read (fin p.2))
  rw [hchain p.2 (hl p hp)]

/-- The width-aware DAG checker's `true` verdict is a `checkEquivW`
`true` verdict. -/
theorem checkEquivDag_toW {p₁ p₂ : Program} (h : checkEquivDag p₁ p₂ = true) :
    checkEquivW p₁ p₂ = true := by
  unfold checkEquivDag at h
  cases hF₁ : Sem.mkFEnv p₁ with
  | error e => rw [hF₁] at h; simp at h
  | ok F₁ =>
  cases hF₂ : Sem.mkFEnv p₂ with
  | error e => rw [hF₁, hF₂] at h; simp at h
  | ok F₂ =>
  cases hs₁ : symStepDag (dmapOf p₁) (Sem.xenv p₁) (progFuel p₁) p₁.device Dag.empty with
  | error e => rw [hF₁, hF₂, hs₁] at h; simp at h
  | ok res₁ =>
  obtain ⟨d₁, outs₁, nexts₁⟩ := res₁
  rw [hF₁, hF₂, hs₁] at h
  dsimp only at h
  cases hs₂ : symStepDag (dmapOf p₂) (Sem.xenv p₂) (progFuel p₂) p₂.device d₁ with
  | error e => rw [hs₂] at h; simp at h
  | ok res₂ =>
  obtain ⟨d₂, outs₂, nexts₂⟩ := res₂
  rw [hs₂] at h
  dsimp only at h
  cases hr₁ : renorm d₂ with
  | error e => rw [hr₁] at h; simp at h
  | ok re₁ =>
  obtain ⟨e₁, m₁⟩ := re₁
  rw [hr₁] at h
  dsimp only at h
  cases hr₂ : renorm e₁ with
  | error e => rw [hr₂] at h; simp at h
  | ok re₂ =>
  obtain ⟨e₂, m₂⟩ := re₂
  rw [hr₂] at h
  dsimp only at h
  cases hr₃ : renorm e₂ with
  | error e => rw [hr₃] at h; simp at h
  | ok re₃ =>
  obtain ⟨e₃, m₃⟩ := re₃
  rw [hr₃] at h
  dsimp only at h
  -- the two step simulations and three renormalization sweeps
  obtain ⟨W₁, _E₁, Lo₁, Ln₁, hT₁⟩ := symStepDag_sim WF.empty hs₁
  obtain ⟨W₂, E₁₂, Lo₂, Ln₂, hT₂⟩ := symStepDag_sim W₁ hs₂
  obtain ⟨WR₁, ER₁, hsz₁, hinv₁⟩ := renorm_spec W₂ hr₁
  obtain ⟨WR₂, ER₂, hsz₂, hinv₂⟩ := renorm_spec WR₁ hr₂
  obtain ⟨WR₃, ER₃, hsz₃, hinv₃⟩ := renorm_spec WR₂ hr₃
  have hchain : ∀ i, i < d₂.size →
      mIdx m₃ (mIdx m₂ (mIdx m₁ i)) < e₃.size ∧
      e₃.read (mIdx m₃ (mIdx m₂ (mIdx m₁ i))) = cfoldW3 (d₂.read i) := by
    intro i hi
    obtain ⟨h1lt, h1eq⟩ := hinv₁ i hi
    obtain ⟨h2lt, h2eq⟩ := hinv₂ (mIdx m₁ i) h1lt
    obtain ⟨h3lt, h3eq⟩ := hinv₃ (mIdx m₂ (mIdx m₁ i)) h2lt
    refine ⟨h3lt, ?_⟩
    rw [h3eq, h2eq, h1eq]
    rfl
  -- destructure the DAG verdict
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  obtain ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨hc₁, hc₂⟩, he₁⟩, he₂⟩, hi₁⟩, hi₂⟩, hn₁⟩, hn₂⟩, hnIR⟩, hinitw⟩,
    hnwD⟩, hIn⟩, hOut⟩, hReg⟩, hOutsD⟩, hNextsD⟩ := h
  -- assemble the tree verdict
  unfold checkEquivW
  simp only [hF₁, hF₂, hT₁, hT₂]
  simp only [Bool.and_eq_true, decide_eq_true_eq]
  -- side-1 reads transport to the big shared store
  have hmap₁o : outs₁.map (fun p => (p.1, d₁.read p.2))
      = outs₁.map (fun p => (p.1, d₂.read p.2)) :=
    List.map_congr_left fun p hp => by rw [read_ext E₁₂ p.2 (Lo₁ p hp)]
  have hmap₁n : nexts₁.map (fun p => (p.1, d₁.read p.2))
      = nexts₁.map (fun p => (p.1, d₂.read p.2)) :=
    List.map_congr_left fun p hp => by rw [read_ext E₁₂ p.2 (Ln₁ p hp)]
  have hLo₁' : ∀ p ∈ outs₁, p.2 < d₂.size :=
    fun p hp => Nat.lt_of_lt_of_le (Lo₁ p hp) E₁₂.size_le
  have hLn₁' : ∀ p ∈ nexts₁, p.2 < d₂.size :=
    fun p hp => Nat.lt_of_lt_of_le (Ln₁ p hp) E₁₂.size_le
  -- the width gate
  have hnw : nextsWidthsOkB p₁.device
      ⟨outs₁.map (fun p => (p.1, d₁.read p.2)), nexts₁.map (fun p => (p.1, d₁.read p.2))⟩
      = true := by
    rw [nextsWidthsOkB]
    dsimp only
    rw [List.all_eq_true]
    intro rp hrp
    rw [List.zip_map_right] at hrp
    obtain ⟨⟨r, q⟩, hmem, heq⟩ := List.mem_map.mp hrp
    subst heq
    have hg := List.all_eq_true.mp hnwD (r, q) hmem
    simp only [Bool.and_eq_true, beq_iff_eq] at hg
    show ((q.1 == r.name)
      && (annWidth (cfoldW3 (d₁.read q.2)) == some r.width)) = true
    simp only [Bool.and_eq_true, beq_iff_eq]
    have hq₂ : q.2 < d₂.size := hLn₁' q (List.of_mem_zip hmem).2
    refine ⟨hg.1, ?_⟩
    rw [← read_ext E₁₂ q.2 (Ln₁ q (List.of_mem_zip hmem).2), ← (hchain q.2 hq₂).2,
        widthOf_eq WR₃ (hchain q.2 hq₂).1, hg.2]
  -- the two comparisons
  have houts : cfoldPairsW (outs₁.map fun p => (p.1, d₁.read p.2))
      = cfoldPairsW (outs₂.map fun p => (p.1, d₂.read p.2)) := by
    rw [hmap₁o,
        cfoldPairsW_map outs₁ hLo₁' (fun i hi => (hchain i hi).2),
        cfoldPairsW_map outs₂ Lo₂ (fun i hi => (hchain i hi).2),
        hOutsD]
  have hnexts : cfoldPairsW (nexts₁.map fun p => (p.1, d₁.read p.2))
      = cfoldPairsW (nexts₂.map fun p => (p.1, d₂.read p.2)) := by
    rw [hmap₁n,
        cfoldPairsW_map nexts₁ hLn₁' (fun i hi => (hchain i hi).2),
        cfoldPairsW_map nexts₂ Ln₂ (fun i hi => (hchain i hi).2),
        hNextsD]
  exact ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨hc₁, hc₂⟩, he₁⟩, he₂⟩, hi₁⟩, hi₂⟩, hn₁⟩, hn₂⟩, hnIR⟩, hinitw⟩,
    hnw⟩, hIn⟩, hOut⟩, hReg⟩, houts⟩, hnexts⟩

/-- The rewrite-free DAG checker's `true` verdict is a `checkEquiv`
`true` verdict. -/
theorem checkEquivDagRaw_toA {p₁ p₂ : Program} (h : checkEquivDagRaw p₁ p₂ = true) :
    checkEquiv p₁ p₂ = true := by
  unfold checkEquivDagRaw at h
  cases hF₁ : Sem.mkFEnv p₁ with
  | error e => rw [hF₁] at h; simp at h
  | ok F₁ =>
  cases hF₂ : Sem.mkFEnv p₂ with
  | error e => rw [hF₁, hF₂] at h; simp at h
  | ok F₂ =>
  cases hs₁ : symStepDag (dmapOf p₁) (Sem.xenv p₁) (progFuel p₁) p₁.device Dag.empty with
  | error e => rw [hF₁, hF₂, hs₁] at h; simp at h
  | ok res₁ =>
  obtain ⟨d₁, outs₁, nexts₁⟩ := res₁
  rw [hF₁, hF₂, hs₁] at h
  dsimp only at h
  cases hs₂ : symStepDag (dmapOf p₂) (Sem.xenv p₂) (progFuel p₂) p₂.device d₁ with
  | error e => rw [hs₂] at h; simp at h
  | ok res₂ =>
  obtain ⟨d₂, outs₂, nexts₂⟩ := res₂
  rw [hs₂] at h
  dsimp only at h
  obtain ⟨W₁, _E₁, Lo₁, Ln₁, hT₁⟩ := symStepDag_sim WF.empty hs₁
  obtain ⟨W₂, E₁₂, Lo₂, Ln₂, hT₂⟩ := symStepDag_sim W₁ hs₂
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  obtain ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨hc₁, hc₂⟩, he₁⟩, he₂⟩, hi₁⟩, hi₂⟩, hn₁⟩, hn₂⟩, hIn⟩, hOut⟩, hReg⟩,
    hOutsD⟩, hNextsD⟩ := h
  unfold checkEquiv
  simp only [hF₁, hF₂, hT₁, hT₂]
  simp only [Bool.and_eq_true, decide_eq_true_eq]
  have hmap₁o : outs₁.map (fun p => (p.1, d₁.read p.2))
      = outs₂.map (fun p => (p.1, d₂.read p.2)) := by
    rw [show outs₁.map (fun p => (p.1, d₁.read p.2))
          = outs₁.map (fun p => (p.1, d₂.read p.2)) from
        List.map_congr_left fun p hp => by rw [read_ext E₁₂ p.2 (Lo₁ p hp)], hOutsD]
  have hmap₁n : nexts₁.map (fun p => (p.1, d₁.read p.2))
      = nexts₂.map (fun p => (p.1, d₂.read p.2)) := by
    rw [show nexts₁.map (fun p => (p.1, d₁.read p.2))
          = nexts₁.map (fun p => (p.1, d₂.read p.2)) from
        List.map_congr_left fun p hp => by rw [read_ext E₁₂ p.2 (Ln₁ p hp)], hNextsD]
  exact ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨hc₁, hc₂⟩, he₁⟩, he₂⟩, hi₁⟩, hi₂⟩, hn₁⟩, hn₂⟩, hIn⟩, hOut⟩, hReg⟩,
    by rw [hmap₁o]⟩, by rw [hmap₁n]⟩

/-! ## The headline theorems -/

/-- Soundness of the width-aware DAG checker: a `true` verdict gives
run equality on every stimulus that drives the declared input widths
(`checkEquivW_sound`'s honest side condition, inherited through the
reduction). -/
theorem checkEquivDag_sound {p₁ p₂ : Program} (h : checkEquivDag p₁ p₂ = true) :
    ∀ stim, StimWF p₁.device stim → ∀ (E : Sem.EEnv), p₁.run stim E = p₂.run stim E :=
  fun stim hs => checkEquivW_sound (checkEquivDag_toW h) stim hs

/-- Soundness of the rewrite-free DAG checker: UNCONDITIONAL run
equality, inherited from `checkEquiv_sound`. -/
theorem checkEquivDagRaw_sound {p₁ p₂ : Program} (h : checkEquivDagRaw p₁ p₂ = true) :
    ∀ stim (E : Sem.EEnv), p₁.run stim E = p₂.run stim E :=
  fun stim => checkEquiv_sound (checkEquivDagRaw_toA h) stim

end Rwv.Hyle.BridgeDag

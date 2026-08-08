/-
The Eidos↔Hyle correspondence, stated formally (doc/eidos.md §7.5.6):
well-typedness of semantic values, agreement of a machine trace with a
device trace through the representation function (up to and excluding
the halting cycle), and the top-level correspondence Prop over the two
mechanized semantics. This is the statement the differential harness
tests empirically per golden, and the statement the validator's
soundness theorem concludes (`validateProc_corresponds` and its ∀η
variant in Rwv.Eidos.Cstep, with `Rwv.Sim.simP_run` as the induction
connecting per-step obligations to it).
-/
import Rwv.Eidos.Machine
import Rwv.Hyle.Semantics
import Std.Data.HashMap

namespace Rwv.Eidos

open Std (HashMap)

/-- Well-typedness of a semantic value at a representable type
(doc/eidos.md §7.5.1): the inductive carving of `V_τ` out of `Val`.
Constructor values must be constructors of the type's datatype with
fields well-typed at the instantiated field types. -/
inductive Val.HasTy (Δ : DEnv) : Val → Ty → Prop where
  | vec {es : List Val} {t nt τ : Ty} {n : Nat}
      (hty : Ty.flatten t = (.con "Vec", [nt, τ]))
      (hn : Ty.evalNat nt = some n)
      (hlen : es.length = n)
      (helems : ∀ v ∈ es, Val.HasTy Δ v τ) :
      Val.HasTy Δ (.vec es) t
  | finite {t nt : Ty} {n i : Nat}
      (hty : Ty.flatten t = (.con "Finite", [nt]))
      (hn : Ty.evalNat nt = some n)
      (hlt : i < n) :
      Val.HasTy Δ (.finite n i) t
  | integer {t : Ty} {v : BitVec 128}
      (hty : Ty.flatten t = (.con "Integer", [])) :
      Val.HasTy Δ (.integer v) t
  | proxy {t nt : Ty}
      (hty : Ty.flatten t = (.con "Proxy", [nt])) :
      Val.HasTy Δ .proxy t
  | con {t : Ty} {tc c : String} {fields : List Val}
      {sig : Sig} {sub : HashMap TyVar Ty} {doms : List Ty}
      (hty : (Ty.flatten t).1 = .con tc)
      (hctor : ∃ cs, Δ.ctors.get? tc = some cs ∧ c ∈ cs)
      (hsig : Δ.ctorSig.get? c = some sig)
      (hmatch : DEnv.matchTy (Ty.flattenArrow sig.ty).2 t = .ok sub)
      (hdoms : doms = (Ty.flattenArrow sig.ty).1.map (DEnv.substTv sub))
      (hlen : fields.length = doms.length)
      (hfields : ∀ p ∈ fields.zip doms, Val.HasTy Δ p.1 p.2) :
      Val.HasTy Δ (.con t c fields) t

/-- Agreement of a machine trace with a device trace through the
representation function: the rep-encoded machine outputs equal the
device outputs cycle for cycle; when the machine halted, they are a
prefix (the device's remaining cycles are unspecified, §7.5.4). -/
def TraceAgrees (Δ : DEnv) (fuel : Nat) (outTy : Ty)
    (mt : MTrace) (ht : List (List Rwv.Hyle.BV)) : Prop :=
  ∃ encOuts, mt.outs.mapM (Val.portSplit Δ fuel outTy) = .ok encOuts ∧
    match mt.halted with
    | none   => ht = encOuts
    | some _ => encOuts <+: ht

/-- The §7.5.6 correspondence between a machine-mode process and a
compiled Hyle program, over the two mechanized semantics: for every
well-typed input trace, whenever both sides run successfully — the
machine on the algebraic inputs, the device on their port-split
encodings — the traces agree through the representation function.
(Totality of both runs at sufficient fuel on well-formed programs is a
separate obligation, not mechanized; this is the agreement half.)

The η tier: the statement is parameterized by the bit-level
model-less-extern environment `E`, with BOTH runs reading the SAME
one — the machine through `Eval.evalExt`'s decode-gated foreign row,
the device through `evalExp`'s total `Sem.xapply` reading. The
algebraic quantification (∀ η_alg) is this parameter at
`rep ∘ η ∘ decode` instantiations (`Rwv.Eidos.Cstep.etaB`); the
validator's soundness theorem concludes the statement for EVERY `E`,
which is the ∀η reading. At the default (empty) environment the
definition is exactly the pre-extension statement. -/
def Corresponds (Δ : DEnv) (defns : HashMap Int Defn) (evalFuel gotoFuel : Nat)
    (p : Proc) (H : Rwv.Hyle.Program)
    (E : Rwv.Hyle.Sem.EEnv := Rwv.Hyle.Sem.eEmpty) : Prop :=
  ∀ (ins : List Val), (∀ v ∈ ins, Val.HasTy Δ v p.inTy) →
    ∀ encIns, ins.mapM (Val.portSplit Δ evalFuel p.inTy) = .ok encIns →
    ∀ mt, Proc.run Δ defns evalFuel gotoFuel p ins E = .ok mt →
    ∀ ht, H.run encIns E = .ok ht →
    TraceAgrees Δ evalFuel p.outTy mt ht

end Rwv.Eidos

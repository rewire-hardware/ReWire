/-
rwv-check-equiv: the VERIFIED DAG equivalence checker, compiled
(Rwv.Hyle.BridgeDag; soundness `checkEquivDag_sound` /
`checkEquivDagRaw_sound`).

    rwv-check-equiv <raw.rwc> <final.rwc> [--raw-only|--w-only]

Prints `RESULT: W=<bool> (<ms>) RAW=<bool> (<ms>)` and exits 0 when
the width-aware verdict is true, 1 when false, 2 on parse/usage
errors.
-/
import Rwv.Hyle.BridgeDag
import Rwv.Hyle.Parse

open Rwv.Hyle Rwv.Hyle.BridgeDag

def main (args : List String) : IO UInt32 := do
  match args with
  | raw :: fin :: rest => do
      let rawTxt ← IO.FS.readFile raw
      let finTxt ← IO.FS.readFile fin
      match parseProgram rawTxt, parseProgram finTxt with
      | .ok p₁, .ok p₂ => do
          let t0 ← IO.monoMsNow
          let rW := if rest.contains "--raw-only" then false else checkEquivDag p₁ p₂
          let t1 ← IO.monoMsNow
          let rA := if rest.contains "--w-only" then false else checkEquivDagRaw p₁ p₂
          let t2 ← IO.monoMsNow
          IO.println s!"RESULT: W={rW} ({t1-t0}ms) RAW={rA} ({t2-t1}ms)"
          pure (if rW then 0 else 1)
      | .error e, _ => do
          IO.println s!"parse error (raw): {e.take 200}"
          pure 2
      | _, .error e => do
          IO.println s!"parse error (final): {e.take 200}"
          pure 2
  | _ => do
      IO.println "usage: rwv-check-equiv <raw.rwc> <final.rwc> [--raw-only|--w-only]"
      pure 2

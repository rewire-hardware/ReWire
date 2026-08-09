/-
rwv-check-equiv: the VERIFIED DAG equivalence checker, compiled
(Rwv.Hyle.BridgeDag; soundness `checkEquivDag_sound` /
`checkEquivDagRaw_sound`).

    rwv-check-equiv <raw.rwc> <final.rwc> [--raw-only|--w-only]

Runs exactly the selected checks (both by default) and prints
`RESULT: W=<bool> (<ms>) RAW=<bool> (<ms>)`, with `-` for a check that
was not selected. Exits 0 iff every selected check is true, 1 when a
selected check is false, 2 on parse/usage errors (including unknown or
contradictory options).
-/
import Rwv.Hyle.BridgeDag
import Rwv.Hyle.Parse

open Rwv.Hyle Rwv.Hyle.BridgeDag

def usage : String :=
  "usage: rwv-check-equiv <raw.rwc> <final.rwc> [--raw-only|--w-only]"

def main (args : List String) : IO UInt32 := do
  let mut rawOnly := false
  let mut wOnly := false
  let mut pos : List String := []
  for a in args do
    if a = "--raw-only" then rawOnly := true
    else if a = "--w-only" then wOnly := true
    else if a.startsWith "-" && a ≠ "-" then
      IO.eprintln s!"rwv-check-equiv: unknown option: {a}"
      IO.eprintln usage
      return 2
    else pos := pos ++ [a]
  if rawOnly && wOnly then
    IO.eprintln "rwv-check-equiv: --raw-only and --w-only are contradictory"
    return 2
  match pos with
  | [raw, fin] => do
      let rawTxt ← try IO.FS.readFile raw catch ex =>
        IO.eprintln s!"rwv-check-equiv: {raw}: {ex}"; return 2
      let finTxt ← try IO.FS.readFile fin catch ex =>
        IO.eprintln s!"rwv-check-equiv: {fin}: {ex}"; return 2
      match parseProgram rawTxt, parseProgram finTxt with
      | .ok p₁, .ok p₂ => do
          let t0 ← IO.monoMsNow
          let rW := if rawOnly then none else some (checkEquivDag p₁ p₂)
          let t1 ← IO.monoMsNow
          let rA := if wOnly then none else some (checkEquivDagRaw p₁ p₂)
          let t2 ← IO.monoMsNow
          let show' : Option Bool → String := fun | some b => toString b | none => "-"
          IO.println s!"RESULT: W={show' rW} ({t1-t0}ms) RAW={show' rA} ({t2-t1}ms)"
          pure (if (rW.getD true) && (rA.getD true) then 0 else 1)
      | .error e, _ => do
          IO.println s!"parse error (raw): {e.take 200}"
          pure 2
      | _, .error e => do
          IO.println s!"parse error (final): {e.take 200}"
          pure 2
  | _ => do
      IO.eprintln usage
      pure 2

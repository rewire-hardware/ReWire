/-
rwv-diff: the differential-testing driver against rwc's Hyle
interpreter.

    rwv-diff <file.rwc> [stimulus.yaml] [--cycles N]

parses the Hyle program, reads the stimulus file (the same YAML
name -> integer maps `rwc --interpret=<file>` reads, with the same
padding/sticky semantics and the same default cycle count -- see
Rwv.Diff), runs `Rwv.Hyle.Program.run`, and prints the output trace to
stdout in exactly the YAML format rwc writes, so

    rwv-diff foo.rwc stim.yaml --cycles 20 > lean.yaml
    rwc foo.rwc --from-core --interpret=stim.yaml --cycles 20 -o hs.yaml
    diff hs.yaml lean.yaml

decides agreement. Driven across the golden corpus by
verify/test/diff-goldens.py.

Exit codes: 0 success, 1 parse/evaluation failure, 2 usage error.
-/
import Rwv.Hyle.Parse
import Rwv.Hyle.Check
import Rwv.Hyle.Semantics
import Rwv.Diff

open Rwv.Hyle Rwv.Diff

structure Args where
  rwcFile  : String
  stimFile : Option String := none
  cycles   : Option Nat    := none

def usage : String :=
  "usage: rwv-diff <file.rwc> [stimulus.yaml] [--cycles N]"

def parseArgs (argv : List String) : Except String Args := do
  let mut positional : List String := []
  let mut cycles : Option Nat := none
  let mut rest := argv
  repeat
    match rest with
    | [] => break
    | "--cycles" :: n :: more =>
        match n.toNat? with
        | some v => cycles := some v; rest := more
        | none   => throw s!"--cycles: expected a non-negative integer, got '{n}'"
    | ["--cycles"] => throw "--cycles: missing argument"
    | arg :: more =>
        if arg.startsWith "--cycles=" then
          match (arg.drop "--cycles=".length).toString.toNat? with
          | some v => cycles := some v
          | none   => throw s!"--cycles: expected a non-negative integer, got '{arg}'"
        else if arg.startsWith "-" && arg ≠ "-" then
          throw s!"unknown option: {arg}"
        else
          positional := positional ++ [arg]
        rest := more
  match positional with
  | [f]     => return { rwcFile := f, cycles }
  | [f, st] => return { rwcFile := f, stimFile := some st, cycles }
  | _       => throw usage

/-- Load the stimulus entries. An EXPLICITLY named stimulus file that
does not exist or does not parse is a hard error — a harness path
mixup that silently zero-drives both sides of a differential run would
make the byte comparison trivially pass. Only the implicit default
(`inputs.yaml`) may be absent, matching rwc's driving-with-zeros
default for that case. -/
def loadStimulus (args : Args) : IO (Except String (List (List (String × Int)))) := do
  let path := args.stimFile.getD "inputs.yaml"
  let explicit := args.stimFile.isSome
  if ← System.FilePath.pathExists ⟨path⟩ then
    match parseStimulus (← IO.FS.readFile ⟨path⟩) with
    | .ok entries => return .ok entries
    | .error e    =>
        if explicit then return .error s!"could not read inputs from {path} ({e})"
        IO.eprintln s!"rwv-diff: warning: could not read inputs from {path} ({e}); driving all inputs with zeros."
        return .ok []
  else
    if explicit then return .error s!"could not read inputs from {path} (file does not exist)"
    return .ok []

def main (argv : List String) : IO UInt32 := do
  match parseArgs argv with
  | .error e => IO.eprintln s!"rwv-diff: {e}"; return 2
  | .ok args =>
    let contents ← IO.FS.readFile ⟨args.rwcFile⟩
    match Rwv.Hyle.parseProgram contents args.rwcFile with
    | .error e => IO.eprintln s!"rwv-diff: {args.rwcFile}: parse error: {e}"; return 1
    | .ok p    =>
      if let .error e := p.check then
        IO.eprintln s!"rwv-diff: {args.rwcFile}: check failed: {e}"
        return 1
      let entries ← match ← loadStimulus args with
        | .error e => IO.eprintln s!"rwv-diff: {e}"; return 1
        | .ok entries => pure entries
      let ncycles := effectiveCycles args.cycles entries.length
      let stim    := stimulusFor p.device (boundInput ncycles entries)
      match p.run stim with
      | .error e  => IO.eprintln s!"rwv-diff: {args.rwcFile}: {e}"; return 1
      | .ok trace =>
          IO.print (printTrace (p.device.outputs.map Prod.fst) trace)
          return 0

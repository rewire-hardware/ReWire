/-
Batch driver for Rwv.Eidos.parseEir: parses each argument file and
reports PASS/FAIL per file (exit 1 if any fail). `--dump` additionally
prints the parsed (elaborated) Program via Repr.
-/
import Rwv.Eidos.Parse

open Rwv.Eidos

def main (args : List String) : IO UInt32 := do
  let (dump, files) := match args with
    | "--dump" :: rest => (true, rest)
    | _                => (false, args)
  if files.isEmpty then
    IO.eprintln "usage: rwv-eir-parse-check [--dump] <file.eir>..."
    return 2
  let mut failed := 0
  for f in files do
    let txt ← IO.FS.readFile f
    match parseEir txt f with
    | .ok p =>
        IO.println s!"PASS {f}"
        if dump then IO.println (repr p)
    | .error e => do
        IO.println s!"FAIL {f}: {e}"
        failed := failed + 1
  if failed == 0 then return 0
  else do
    IO.eprintln s!"rwv-eir-parse-check: {failed} file(s) failed"
    return 1

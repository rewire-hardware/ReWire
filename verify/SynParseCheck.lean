/-
Batch driver for Rwv.Synolon.parseSyn: parses each argument file and
reports PASS/FAIL per file (exit 1 if any fail). `--dump` additionally
prints the parsed (elaborated) Program via Repr.
-/
import Rwv.Synolon.Parse

open Rwv.Synolon

def main (args : List String) : IO UInt32 := do
  let (dump, files) := match args with
    | "--dump" :: rest => (true, rest)
    | _                => (false, args)
  if files.isEmpty then
    IO.eprintln "usage: rwv-syn-parse-check [--dump] <file.syn|file.eir>..."
    return 2
  let mut failed := 0
  for f in files do
    let txt ← IO.FS.readFile f
    match parseSyn txt f with
    | .ok p =>
        IO.println s!"PASS {f}"
        if dump then IO.println (repr p)
    | .error e => do
        IO.println s!"FAIL {f}: {e}"
        failed := failed + 1
  if failed == 0 then return 0
  else do
    IO.eprintln s!"rwv-syn-parse-check: {failed} file(s) failed"
    return 1

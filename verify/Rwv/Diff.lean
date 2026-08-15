/-
Shared helpers for the rwv-diff differential driver: a reader for rwc's
interpreter stimulus files and a printer for its output traces, each
matching the Haskell side byte for byte so a plain `diff` decides.

The Haskell reference behavior this file mirrors:

* Stimulus loading (rewire-frontend ReWire.FrontEnd, loadInputs /
  boundInput / effectiveCycles): the inputs file is a YAML list of
  per-cycle name -> integer maps; the list is padded to the cycle count
  by repeating its last entry (or the empty map), truncated to the
  cycle count, and made "sticky" -- each effective cycle map is the
  cycle's own entries unioned over the previous effective map, so a
  wire named once keeps its value until overridden. The cycle count is
  the explicit --cycles value if given, else max 10 (#entries). An
  unreadable or absent file drives all inputs with zeros.

* Input driving (ReWire.Hyle.Interp.interp): each device input wire is
  driven with `bitVec width value` of the map entry (missing wires
  zero), i.e. the value taken modulo 2^width, two's complement for
  negatives -- exactly `BitVec.ofInt`.

* Trace printing (ReWire.FrontEnd `YAML.encodeFile` of `[Outs]`, with
  `instance ToJSON BV` in ReWire.Orphans printing `"0x" <> showHex`):
  a YAML block sequence with one mapping per cycle; keys sorted
  lexicographically (aeson's ordered KeyMap); values single-quoted
  lowercase minimal hex (libyaml quotes them because `0x...` reads as
  a YAML 1.1 integer); an empty mapping prints as `- {}`; an empty
  trace prints as `[]`; trailing newline.
-/
import Rwv.Hyle.Syntax
import Std.Data.HashMap

namespace Rwv.Diff

open Rwv.Hyle
open Std (HashMap)

/-- `String.drop`, back to a `String`. -/
private def sdrop (s : String) (n : Nat) : String :=
  (s.drop n).toString

/-- `String.dropEnd`, back to a `String`. -/
private def sdropEnd (s : String) (n : Nat) : String :=
  (s.dropEnd n).toString

/-- ASCII trim, back to a `String`. -/
private def strim (s : String) : String :=
  s.trimAscii.toString

/-! ## Trace printing -/

/-- Lowercase minimal hex, exactly Haskell `Numeric.showHex` (so `0`
prints as `"0"` -- no width padding, no leading zeros). -/
def hexOfNat (n : Nat) : String :=
  String.ofList (Nat.toDigits 16 n)

/-- `ReWire.BitVector.showHex`: `0x` + minimal lowercase hex of the
unsigned reading. Width-independent; a zero-width BV prints `0x0`. -/
def bvHex (v : BV) : String :=
  "0x" ++ hexOfNat v.nat

/-- Render a trace KEY exactly as `Data.Yaml.encode` (libyaml) renders
a block-mapping key: plain when the scalar needs no quoting,
single-quoted (with `'` doubled) when it does. The quoting predicate
was derived by probing libyaml: quote on a leading or trailing space;
a `: ` anywhere or a trailing `:`; a ` #` anywhere or a leading `#`;
a leading indicator among `[]{},&*!|>'"%@\``; or a leading `-`/`?`/`:`
that is the whole name or followed by a space. Names containing
newlines, tabs, or other control characters take libyaml's block/
double-quoted styles, which this printer deliberately does not
reproduce — it errors instead, failing the differential run closed
rather than mis-emitting. -/
def yamlKey (s : String) : Except String String := do
  let cs := s.toList
  if cs.any fun c => c == '\n' || c == '\t' || c.toNat < 0x20 then
    throw s!"port name {s.quote}: control characters are outside the emitter's \
      byte-identical YAML fragment"
  let quote :=
    match cs with
    | [] => true
    | c0 :: rest =>
        c0 == ' ' || "[]{},&*!|>'\"%@`#".toList.contains c0
        || ((c0 == '-' || c0 == '?' || c0 == ':') && (rest.isEmpty || rest.headD ' ' == ' '))
        || cs.getLast? == some ' ' || cs.getLast? == some ':'
        || hasSub cs ':' ' ' || hasSub cs ' ' '#'
  if quote then
    return "'" ++ String.ofList (cs.flatMap fun c => if c == '\'' then ['\'', '\''] else [c]) ++ "'"
  else return s
where
  hasSub : List Char → Char → Char → Bool
    | a :: rest@(b :: _), x, y => (a == x && b == y) || hasSub rest x y
    | _, _, _ => false

/-- One cycle of the trace: a YAML block mapping as a sequence item,
keys sorted lexicographically (aeson ordered-keymap order) and
rendered by `yamlKey`, values single-quoted hex; `- {}` when there are
no outputs. No trailing newline (the caller adds one per cycle). -/
def printCycle (outs : List (String × BV)) : Except String String := do
  match outs.mergeSort (fun a b => a.1 ≤ b.1) with
  | []            => return "- {}"
  | first :: rest => do
        let f ← entry first
        let rs ← rest.mapM entry
        return "- " ++ f ++ String.join (rs.map ("\n  " ++ ·))
where
  entry (p : String × BV) : Except String String := do
    return s!"{← yamlKey p.1}: '{bvHex p.2}'"

/-- The whole trace in `rwc --interpret`'s YAML format: `names` are the
device's output port names in port order, zipped against each cycle's
positional outputs (`Program.run`'s result). An empty trace (zero
cycles) prints as `[]`, as `Data.Yaml.encode []` does. -/
def printTrace (names : List String) (trace : List (List BV)) : Except String String :=
  match trace with
  | [] => return "[]\n"
  | _  => do
      let ls ← trace.mapM fun vs => printCycle (names.zip vs)
      return String.join (ls.map (· ++ "\n"))

/-! ## Stimulus files

A parser for the YAML fragment rwc's inputs files actually use (and
that the diff-goldens harness generates): a block sequence of flat
name -> integer mappings, with `#` comments, blank lines, and optional
`---`/`...` document markers. Anything outside the fragment is a loud
error -- silently diverging from Data.Yaml would mask disagreements. -/

/-- Drop a `#` comment: a `#` at the start of the line or preceded by
whitespace starts a comment — but never inside a quoted key (values
are plain integers, so `#` cannot occur inside a value). Quote state
tracks one level of single (`''` doubling) or double (backslash
escapes) quoting, as the stimulus fragment's keys use. -/
def stripComment (l : String) : String :=
  String.ofList (go true none l.toList)
where
  go : Bool → Option Char → List Char → List Char
    | _, none, []                    => []
    | _, some _, []                  => []
    | true, none, '#' :: _           => []
    | _, none, c :: rest             =>
        let q := if c = '\'' || c = '"' then some c else none
        c :: go (c = ' ' || c = '\t') q rest
    | _, some '"', '\\' :: e :: rest => '\\' :: e :: go false (some '"') rest
    | _, some '\'', '\'' :: '\'' :: rest => '\'' :: '\'' :: go false (some '\'') rest
    | _, some q, c :: rest           =>
        c :: go false (if c = q then none else some q) rest

private def digitVal? (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

private def parseNatBase? (base : Nat) (s : String) : Option Nat :=
  if s.isEmpty then none
  else s.foldl (init := some 0) fun acc? c => do
    let acc ← acc?
    let d ← digitVal? c
    if d < base then some (acc * base + d) else none

/-- An integer literal as Data.Yaml resolves scalars for a FromJSON
Integer: decimal, `0x` hex, or `0o` octal, with an optional sign. -/
def parseIntLit? (s : String) : Option Int :=
  let (neg, t) :=
    if s.startsWith "-" then (true, sdrop s 1)
    else if s.startsWith "+" then (false, sdrop s 1)
    else (false, s)
  let mag? :=
    if t.startsWith "0x" || t.startsWith "0X" then parseNatBase? 16 (sdrop t 2)
    else if t.startsWith "0o" || t.startsWith "0O" then parseNatBase? 8 (sdrop t 2)
    else parseNatBase? 10 t
  mag?.map fun m => if neg then -(m : Int) else (m : Int)

/-- Read a leading quoted key: single quotes with `''` doubling, or
double quotes with `\\`/`\"` escapes (the JSON-style keys the harness
writes and Data.Yaml reads). Returns the key and the rest. -/
private def readQuotedKey (q : Char) : List Char → Option (String × List Char)
  | cs => go cs []
where
  go : List Char → List Char → Option (String × List Char)
    | [], _ => none
    | '\'' :: '\'' :: rest, acc =>
        if q == '\'' then go rest ('\'' :: acc) else go ('\'' :: rest) ('\'' :: acc)
    | '\\' :: e :: rest, acc =>
        if q == '"' then
          match e with
          | '"' => go rest ('"' :: acc)
          | '\\' => go rest ('\\' :: acc)
          | _ => none
        else go (e :: rest) ('\\' :: acc)
    | c :: rest, acc =>
        if c == q then some (String.ofList acc.reverse, rest)
        else go rest (c :: acc)

/-- Split a plain (unquoted) `name: value` line at the first `": "`
(or a trailing `:`), the YAML block-mapping separator — a plain key
may itself contain colons not followed by a space. -/
private def splitPlainPair : List Char → Option (String × String)
  | cs => go cs []
where
  go : List Char → List Char → Option (String × String)
    | [], _ => none
    | ':' :: [], acc => some (String.ofList acc.reverse, "")
    | ':' :: ' ' :: rest, acc => some (String.ofList acc.reverse, String.ofList rest)
    | c :: rest, acc => go rest (c :: acc)

private def parsePair (lineNo : Nat) (t : String) : Except String (String × Int) := do
  let (name, vtxt) ←
    match t.toList with
    | q :: rest =>
        if q == '\'' || q == '"' then
          match readQuotedKey q rest with
          | some (k, rest') =>
              (match rest' with
               | ':' :: rest'' => pure (k, String.ofList rest'')
               | _ => throw s!"stimulus line {lineNo}: expected ':' after quoted key")
          | none => throw s!"stimulus line {lineNo}: malformed quoted key: '{t}'"
        else
          match splitPlainPair t.toList with
          | some (k, v) => pure (k, v)
          | none => throw s!"stimulus line {lineNo}: expected 'name: value', got '{t}'"
    | [] => throw s!"stimulus line {lineNo}: expected 'name: value', got '{t}'"
  let v := strim vtxt
  match parseIntLit? v with
  | some i => .ok (name, i)
  | none   => .error s!"stimulus line {lineNo}: cannot parse input value '{v}' (expected an integer)"

/-- Parse a stimulus file into its per-cycle entries, in file order.
Accepts exactly the fragment described above. -/
def parseStimulus (contents : String) : Except String (List (List (String × Int))) := do
  let mut cycles : Array (Array (String × Int)) := #[]
  let mut lineNo : Nat := 0
  for rawLine in contents.splitOn "\n" do
    lineNo := lineNo + 1
    let raw := if rawLine.endsWith "\r" then sdropEnd rawLine 1 else rawLine
    let t := strim (stripComment raw)
    if t.isEmpty || t = "---" || t = "..." then
      continue
    if t = "[]" then
      if cycles.isEmpty then continue
      else throw s!"stimulus line {lineNo}: unexpected '[]' after sequence entries"
    if t.startsWith "-" then
      let rest := strim (sdrop t 1)
      if rest.isEmpty || rest = "{}" then
        cycles := cycles.push #[]
      else
        cycles := cycles.push #[← parsePair lineNo rest]
    else
      if cycles.isEmpty then
        throw s!"stimulus line {lineNo}: mapping entry outside a sequence item: '{t}'"
      cycles := cycles.modify (cycles.size - 1) (·.push (← parsePair lineNo t))
  return (cycles.map (·.toList)).toList

/-! ## The loader semantics (ReWire.FrontEnd) -/

/-- `effectiveCycles`: the explicit --cycles value if given, else the
larger of 10 and the number of stimulus entries. -/
def effectiveCycles (explicit : Option Nat) (nEntries : Nat) : Nat :=
  explicit.getD (max 10 nEntries)

/-- `boundInput`: pad the entry list to `ncycles` by repeating the last
entry (or the empty map), truncate to `ncycles`, then make it sticky --
each effective map is the entry unioned (entry wins) over the previous
effective map. -/
def boundInput (ncycles : Nat) (entries : List (List (String × Int))) :
    List (HashMap String Int) :=
  let maps    := entries.map fun ps =>
        ps.foldl (fun m (kv : String × Int) => m.insert kv.1 kv.2) (∅ : HashMap String Int)
  let lastM   := maps.getLast?.getD ∅
  let padded  := (maps ++ List.replicate (ncycles - maps.length) lastM).take ncycles
  let step    := fun (st : Array (HashMap String Int) × HashMap String Int) m =>
        let eff := (m : HashMap String Int).toList.foldl
              (fun e (kv : String × Int) => e.insert kv.1 kv.2) st.2
        (st.1.push eff, eff)
  (padded.foldl step (#[], (∅ : HashMap String Int))).1.toList

/-- Drive the device's input ports (positionally, in port order) from
the effective per-cycle maps: missing wires zero, values reduced
modulo 2^width (two's complement for negatives), as the interpreter's
`bitVec sz (findWithDefault 0 x ins)` does. -/
def stimulusFor (dev : Device) (cycles : List (HashMap String Int)) : List (List BV) :=
  cycles.map fun m =>
    dev.inputs.map fun (p : String × Nat) =>
      ⟨p.2, BitVec.ofInt p.2 ((m.get? p.1).getD 0)⟩

end Rwv.Diff

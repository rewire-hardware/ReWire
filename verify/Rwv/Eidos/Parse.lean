/-
Parser for the Eidos concrete syntax (.eir; doc/eidos.md §9) and its
Synolon extension (.syn; doc/synolon.md §9): a transcription of the
reference implementation (rewire-frontend ReWire.Eidos.Parse and
ReWire.Synolon.Parse), in the same tokenizer + recursive descent style
as Rwv.Hyle.Parse.

The concrete syntax does not carry a type for every binder occurrence, so
(as in the reference) parsing is followed by an elaboration pass that
reconstructs what the format leaves implicit:

* variable occurrences print bare (`x#12`) and receive their binder's
  `Id` (unique, occurrence text, signature) from a scope map; an
  occurrence whose unique is not in scope is an error;
* the case binder prints bare and receives the scrutinee's synthesized
  type;
* join-point labels print with no signature; Rwv's `JoinId` carries only
  the arity (taken from the binding at parse time), but a join's
  reconstructed type (arrows from its parameter types to its body's
  synthesized type) is threaded through type synthesis for jump results;
* block labels in terminators receive signatures reconstructed as arrows
  from the block's parameter types to the process output type, and may
  reference blocks declared later in the same process (resolved after
  the whole process is parsed).

Join-point *scoping* is resolved during parsing proper: labels are
lexically scoped, jumps take their arity from the binding, and a jump to
an unbound label is a parse-time error. Two lexical devices keep the
grammar newline-insensitive, as in the reference:

* a name in expression-atom position followed by `::` is not an atom (it
  starts the next definition's signature line);
* an `occ#uniq` token in type-atom position is a type variable only if
  its unique is bound by the enclosing signature's `forall`, so the
  equation name following a signature line never extends the signature's
  type.

Known (deliberate) divergences from the reference, none reachable from
printer output: backtracking here is attempt-based rather than
megaparsec's consumed-input rule, so a few malformed inputs are rejected
with a less precise error position; and a *tightly* written negative
number in type position (`-5 3`) is rejected rather than read as the
prefix application `- 5 3` (the printer always spaces nat operators).
-/
import Rwv.Eidos.Types
import Std.Data.HashMap

namespace Rwv.Eidos

private instance : Inhabited Exp := ⟨.litStr ""⟩

namespace Parse

/-! ## Tokens -/

inductive Tok where
  /-- An identifier (dotted, possibly backtick-quoted) with an optional
  `#uniq` suffix (uniques may be negative: the prim basis'). -/
  | name (quoted : Bool) (s : String) (uniq : Option Int)
  | int (n : Int)
  | str (s : String)
  | lparen | rparen | lbrace | rbrace | lbrack | rbrack
  | comma | semi | dot
  | dcolon | colon | assign
  | arrow | larrow | squig
  | equals | lam | at
  | star | plus | minus
deriving BEq, Repr

structure Token where
  tok  : Tok
  line : Nat
  col  : Nat
deriving Repr

/-! ## Lexer -/

private def isIdentStart (c : Char) : Bool := c.isAlpha || c == '_' || c == '$'

private def isIdentChar (c : Char) : Bool :=
  c.isAlphanum || c == '_' || c == '.' || c == '$' || c == '\''

private def digitsToNat (ds : List Char) : Nat :=
  ds.foldl (fun a c => a * 10 + (c.toNat - '0'.toNat)) 0

/-- The optional `#uniq` suffix of a name token (`#` separates an
identifier from its unique; the unique may be negative). Returns the
unique, the rest of the input, and the consumed width. -/
private def lexUniq (fname : String) (ln col : Nat) :
    List Char → Except String (Option Int × List Char × Nat)
  | '#' :: rest =>
      let (neg, rest') := match rest with
        | '-' :: r => (true, r)
        | r        => (false, r)
      let (ds, rest'') := rest'.span Char.isDigit
      if ds.isEmpty then .error s!"{fname}:{ln}:{col}: expected digits after '#'"
      else
        let n : Int := Int.ofNat (digitsToNat ds)
        .ok (some (if neg then -n else n), rest'', 1 + (if neg then 1 else 0) + ds.length)
  | rest => .ok (none, rest, 0)

/-- The body of a backtick-quoted name (opening backtick consumed): any
characters up to the closing backtick, no escapes (the printer quotes
occurrence text that does not lex as an identifier). Raw newlines
advance the line counter so later diagnostics don't drift. -/
private partial def lexTick (fname : String) (ln0 col0 : Nat) :
    List Char → Nat → Nat → List Char → Except String (String × List Char × Nat × Nat)
  | [], _, _, _ => .error s!"{fname}:{ln0}:{col0}: unterminated backtick-quoted name"
  | '`' :: rest, ln, col, acc => .ok (String.ofList acc.reverse, rest, ln, col + 1)
  | '\n' :: rest, ln, _, acc => lexTick fname ln0 col0 rest (ln + 1) 1 ('\n' :: acc)
  | c :: rest, ln, col, acc => lexTick fname ln0 col0 rest ln (col + 1) (c :: acc)

/-- The body of a string literal (opening quote consumed), with exactly
the escapes the printer emits: `\\ \" \n \t \r`; any other escape is an
error, as in the reference. Raw newlines advance the line counter so
later diagnostics don't drift. -/
private partial def lexStr (fname : String) (ln0 col0 : Nat) :
    List Char → Nat → Nat → List Char → Except String (String × List Char × Nat × Nat)
  | [], _, _, _ => .error s!"{fname}:{ln0}:{col0}: unterminated string literal"
  | '"' :: rest, ln, col, acc => .ok (String.ofList acc.reverse, rest, ln, col + 1)
  | '\\' :: e :: rest, ln, col, acc =>
      let ch? : Option Char := match e with
        | '\\' => some '\\'
        | '"'  => some '"'
        | 'n'  => some '\n'
        | 't'  => some '\t'
        | 'r'  => some '\r'
        | _    => none
      match ch? with
      | some ch => lexStr fname ln0 col0 rest ln (col + 2) (ch :: acc)
      | none    => .error s!"{fname}:{ln}:{col}: invalid escape character '\\{e}' (expected one of \\\\ \\\" \\n \\t \\r)"
  | '\\' :: [], _, _, _ => .error s!"{fname}:{ln0}:{col0}: unterminated string literal"
  | '\n' :: rest, ln, _, acc => lexStr fname ln0 col0 rest (ln + 1) 1 ('\n' :: acc)
  | c :: rest, ln, col, acc => lexStr fname ln0 col0 rest ln (col + 1) (c :: acc)

private partial def lexGo (fname : String) :
    List Char → Nat → Nat → Array Token → Except String (Array Token)
  | [], _, _, acc => .ok acc
  | c :: cs, ln, col, acc =>
    if c == '\n' then lexGo fname cs (ln + 1) 1 acc
    else if c == ' ' || c == '\t' || c == '\r' then lexGo fname cs ln (col + 1) acc
    else if c == '-' then
      match cs with
      | '-' :: cs' => lexGo fname (cs'.dropWhile (· != '\n')) ln (col + 2) acc
      | '>' :: cs' => lexGo fname cs' ln (col + 2) (acc.push ⟨.arrow, ln, col⟩)
      | d :: _ =>
        if d.isDigit then
          -- A tightly written negative integer literal ((-5 :: Integer)).
          let (ds, rest) := cs.span Char.isDigit
          lexGo fname rest ln (col + 1 + ds.length)
            (acc.push ⟨.int (-(Int.ofNat (digitsToNat ds))), ln, col⟩)
        else lexGo fname cs ln (col + 1) (acc.push ⟨.minus, ln, col⟩)
      | [] => lexGo fname cs ln (col + 1) (acc.push ⟨.minus, ln, col⟩)
    else if c.isDigit then
      let (ds, rest) := (c :: cs).span Char.isDigit
      lexGo fname rest ln (col + ds.length)
        (acc.push ⟨.int (Int.ofNat (digitsToNat ds)), ln, col⟩)
    else if isIdentStart c then
      let (xs, rest) := cs.span isIdentChar
      let s := String.ofList (c :: xs)
      let col' := col + 1 + xs.length
      match lexUniq fname ln col' rest with
      | .error e => .error e
      | .ok (u, rest', w) =>
          lexGo fname rest' ln (col' + w) (acc.push ⟨.name false s u, ln, col⟩)
    else if c == '`' then
      match lexTick fname ln col cs ln (col + 1) [] with
      | .error e => .error e
      | .ok (s, rest, ln', col') =>
        match lexUniq fname ln' col' rest with
        | .error e => .error e
        | .ok (u, rest', w) =>
            lexGo fname rest' ln' (col' + w) (acc.push ⟨.name true s u, ln, col⟩)
    else if c == '"' then
      match lexStr fname ln col cs ln (col + 1) [] with
      | .error e => .error e
      | .ok (s, rest, ln', col') => lexGo fname rest ln' col' (acc.push ⟨.str s, ln, col⟩)
    else
      let sym (t : Tok) (k : Nat) (rest : List Char) : Except String (Array Token) :=
        lexGo fname rest ln (col + k) (acc.push ⟨t, ln, col⟩)
      match c, cs with
      | ':', ':' :: cs' => sym .dcolon 2 cs'
      | ':', '=' :: cs' => sym .assign 2 cs'
      | ':', _          => sym .colon 1 cs
      | '<', '-' :: cs' => sym .larrow 2 cs'
      | '~', '>' :: cs' => sym .squig 2 cs'
      | '(', _          => sym .lparen 1 cs
      | ')', _          => sym .rparen 1 cs
      | '{', _          => sym .lbrace 1 cs
      | '}', _          => sym .rbrace 1 cs
      | '[', _          => sym .lbrack 1 cs
      | ']', _          => sym .rbrack 1 cs
      | ',', _          => sym .comma 1 cs
      | ';', _          => sym .semi 1 cs
      | '.', _          => sym .dot 1 cs
      | '=', _          => sym .equals 1 cs
      | '\\', _         => sym .lam 1 cs
      | '@', _          => sym .at 1 cs
      | '*', _          => sym .star 1 cs
      | '+', _          => sym .plus 1 cs
      | _, _            => .error s!"{fname}:{ln}:{col}: unexpected character '{c}'"

def lexAll (fname input : String) : Except String (Array Token) :=
  lexGo fname input.toList 1 1 #[]

/-! ## Parser combinators

The same minimal state monad over the token array as Rwv.Hyle.Parse:
failure carries a located message; `attempt` restores the pre-failure
position for the places that need backtracking. -/

structure PState where
  fname : String
  toks  : Array Token
  i     : Nat

abbrev PM (α : Type) := EStateM String PState α

def errP (msg : String) : PM α := fun s =>
  let pos := match s.toks[s.i]? with
    | some t => s!"{s.fname}:{t.line}:{t.col}"
    | none   => s!"{s.fname}: end of input"
  .error s!"{pos}: {msg}" s

def peek? : PM (Option Token) := fun s => .ok s.toks[s.i]? s

def peekAt (k : Nat) : PM (Option Token) := fun s => .ok s.toks[s.i + k]? s

def advance : PM Unit := fun s => .ok () { s with i := s.i + 1 }

def atEOF : PM Bool := do pure (← peek?).isNone

/-- Run `p`; on failure, restore the input position and return `none`. -/
def attempt (p : PM α) : PM (Option α) := fun s =>
  match p s with
  | .ok a s'    => .ok (some a) s'
  | .error _ _  => .ok none s

def expectTok (t : Tok) (what : String) : PM Unit := do
  match ← peek? with
  | some tk => if tk.tok == t then advance else errP s!"expected {what}"
  | none    => errP s!"expected {what}"

/-- Consume the given token if it is next. -/
def tokOpt (t : Tok) : PM Bool := do
  match ← peek? with
  | some tk => if tk.tok == t then do advance; pure true else pure false
  | none    => pure false

/-- Consume the bare (unquoted, uniqueless) identifier `k` if it is next.
A quoted occurrence or a `k#uniq` token is a name, never a keyword.
(Known divergence, safe direction: the reference parser matches
keywords case-insensitively — megaparsec `string'` — while this
comparison is case-sensitive; the printer emits lowercase, so the Lean
side is strictly narrower on non-printer-emitted inputs.) -/
def keywordOpt (k : String) : PM Bool := do
  match ← peek? with
  | some ⟨.name false s none, _, _⟩ =>
      if s == k then do advance; pure true else pure false
  | _ => pure false

def keyword (k : String) : PM Unit := do
  unless (← keywordOpt k) do errP s!"expected '{k}'"

partial def manyP (p : PM α) : PM (List α) := do
  match ← attempt p with
  | some a => do pure (a :: (← manyP p))
  | none   => pure []

/-- Iterate a parser that signals the end of the sequence itself (so
failures inside an element are reported, not silently swallowed). -/
partial def manyOpt (p : PM (Option α)) : PM (List α) := do
  match ← p with
  | some a => do pure (a :: (← manyOpt p))
  | none   => pure []

partial def sepBy1P (p : PM α) (sep : Tok) : PM (List α) := do
  let a ← p
  if ← tokOpt sep then do pure (a :: (← sepBy1P p sep))
  else pure [a]

/-- `( p , ... )`, possibly empty. -/
def pParenSep (p : PM α) : PM (List α) := do
  expectTok .lparen "'('"
  if ← tokOpt .rparen then pure []
  else do
    let xs ← sepBy1P p .comma
    expectTok .rparen "')'"
    pure xs

/-- `{ p ; ... }`, possibly empty. -/
def pBracedSep (p : PM α) : PM (List α) := do
  expectTok .lbrace "'{'"
  if ← tokOpt .rbrace then pure []
  else do
    let xs ← sepBy1P p .semi
    expectTok .rbrace "'}'"
    pure xs

/-- `[ p , ... ]`, possibly empty. -/
def pBrackSep (p : PM α) : PM (List α) := do
  expectTok .lbrack "'['"
  if ← tokOpt .rbrack then pure []
  else do
    let xs ← sepBy1P p .comma
    expectTok .rbrack "']'"
    pure xs

/-! ## Names -/

def reservedWords : Array String :=
  #[ "let", "in", "rec", "join", "jump", "case", "of", "top", "data"
   , "forall", "inline", "noinline", "from", "baked", "list", "vec"
   , "proc", "entry", "block", "state", "put", "get", "pause", "goto", "halt", "undef"
   , "Nat" ]

/-- A unique-carrying name token, `occ#uniq` (term variables, type
variables, labels). Reserved words are admitted as occurrence text: the
`#` disambiguates them from keywords. -/
def pUniqName : PM (String × Int) := do
  match ← peek? with
  | some ⟨.name _ s (some u), _, _⟩ => do advance; pure (s, u)
  | _ => errP "expected name#unique"

/-- A bare dotted name with no unique (type/data constructors,
primitives, provenance, process and cell names). A bare `_` is not a
name: a constructor named `_` would print identically to the default
case alternative. -/
def pBareName : PM String := do
  match ← peek? with
  | some ⟨.name _ s none, _, _⟩ =>
      if reservedWords.contains s then errP s!"reserved word '{s}' cannot be used as a name"
      else if s == "_" then errP "'_' is not a name"
      else do advance; pure s
  | _ => errP "expected a name"

/-- A tightly written tuple constructor name at token index `i`: `(`,
zero or more `,`, `)`, all adjacent (each token is one character wide).
Returns the name (`()`, `(,)`, `(,,)`, ...) and the token count. -/
private def tightTuple (toks : Array Token) (i : Nat) : Option (String × Nat) := Id.run do
  let some t0 := toks[i]? | return none
  unless t0.tok == .lparen do return none
  let mut k := 0
  let mut go := true
  while go do
    match toks[i + 1 + k]? with
    | some t =>
        if t.tok == .comma && t.line == t0.line && t.col == t0.col + 1 + k then k := k + 1
        else go := false
    | none => go := false
  match toks[i + 1 + k]? with
  | some t =>
      if t.tok == .rparen && t.line == t0.line && t.col == t0.col + 1 + k then
        return some ("(" ++ String.ofList (List.replicate k ',') ++ ")", k + 2)
      else return none
  | none => return none

/-- A tightly written list type constructor name at token index `i`:
`[]` or `[_]`. Returns the name and the token count. -/
private def tightListCon (toks : Array Token) (i : Nat) : Option (String × Nat) := Id.run do
  let some t0 := toks[i]? | return none
  unless t0.tok == .lbrack do return none
  match toks[i + 1]? with
  | some t1 =>
      if t1.tok == .rbrack && t1.line == t0.line && t1.col == t0.col + 1 then
        return some ("[]", 2)
      else if t1.tok == .name false "_" none && t1.line == t0.line && t1.col == t0.col + 1 then
        match toks[i + 2]? with
        | some t2 =>
            if t2.tok == .rbrack && t2.line == t0.line && t2.col == t0.col + 2 then
              return some ("[_]", 3)
            else return none
        | none => return none
      else return none
  | none => return none

/-- Consume a tight tuple constructor name (`()`, `(,)`, ...), if next. -/
def pTupleName? : PM (Option String) := fun s =>
  match tightTuple s.toks s.i with
  | some (n, w) => .ok (some n) { s with i := s.i + w }
  | none        => .ok none s

/-- Consume a tight list constructor name (`[]`, `[_]`), if next. -/
def pListConName? : PM (Option String) := fun s =>
  match tightListCon s.toks s.i with
  | some (n, w) => .ok (some n) { s with i := s.i + w }
  | none        => .ok none s

/-- A constructor name position: bare, the `(,)` family, or the list
type constructors. -/
def pConName : PM String := do
  match ← peek? with
  | some ⟨.name _ _ none, _, _⟩ => pBareName
  | some ⟨.lparen, _, _⟩ =>
      match ← pTupleName? with
      | some n => pure n
      | none   => errP "expected a constructor name"
  | some ⟨.lbrack, _, _⟩ =>
      match ← pListConName? with
      | some n => pure n
      | none   => errP "expected a constructor name"
  | _ => errP "expected a constructor name"

/-! ## Builtins -/

/-- The builtin-name table (the inverse of `Builtin.name`). -/
def lookupBuiltin : String → Option Builtin
  | "rwPrimError" => some .error | "rwPrimExtern" => some .«extern»
  | "rwPrimCryptol" => some .cryptol
  | "rwPrimBind" => some .bind | "rwPrimReturn" => some .ret
  | "rwPrimPut" => some .put | "rwPrimGet" => some .get
  | "rwPrimSignal" => some .signal | "rwPrimLift" => some .lift
  | "rwPrimExtrude" => some .extrude | "rwPrimUnfold" => some .unfold
  | "rwPrimVecFromList" => some .vecFromList | "rwPrimVecReplicate" => some .vecReplicate
  | "rwPrimVecReverse" => some .vecReverse | "rwPrimVecSlice" => some .vecSlice
  | "rwPrimVecRSlice" => some .vecRSlice | "rwPrimVecIndex" => some .vecIndex
  | "rwPrimVecIndexProxy" => some .vecIndexProxy | "rwPrimVecConcat" => some .vecConcat
  | "rwPrimVecMap" => some .vecMap | "rwPrimVecFoldR" => some .vecFoldR
  | "rwPrimVecFoldL" => some .vecFoldL | "rwPrimVecGenerate" => some .vecGenerate
  | "rwPrimFinite" => some .finite | "rwPrimFiniteMinBound" => some .finiteMinBound
  | "rwPrimFiniteMaxBound" => some .finiteMaxBound | "rwPrimToFinite" => some .toFinite
  | "rwPrimToFiniteMod" => some .toFiniteMod | "rwPrimFromFinite" => some .fromFinite
  | "rwPrimNatVal" => some .natVal
  | "rwPrimBits" => some .bits | "rwPrimResize" => some .resize
  | "rwPrimBitSlice" => some .bitSlice | "rwPrimBitIndex" => some .bitIndex
  | "rwPrimAdd" => some .add | "rwPrimSub" => some .sub | "rwPrimMul" => some .mul
  | "rwPrimDiv" => some .div | "rwPrimMod" => some .mod | "rwPrimPow" => some .pow
  | "rwPrimLAnd" => some .lAnd | "rwPrimLOr" => some .lOr
  | "rwPrimAnd" => some .and | "rwPrimOr" => some .or
  | "rwPrimXOr" => some .xor | "rwPrimXNor" => some .xnor
  | "rwPrimLShift" => some .lShift | "rwPrimRShift" => some .rShift
  | "rwPrimRShiftArith" => some .rShiftArith
  | "rwPrimEq" => some .eq | "rwPrimGt" => some .gt | "rwPrimGtEq" => some .gtEq
  | "rwPrimLt" => some .lt | "rwPrimLtEq" => some .ltEq
  | "rwPrimLNot" => some .lNot | "rwPrimNot" => some .not
  | "rwPrimRAnd" => some .rAnd | "rwPrimRNAnd" => some .rNAnd
  | "rwPrimROr" => some .rOr | "rwPrimRNor" => some .rNor
  | "rwPrimRXOr" => some .rXOr | "rwPrimRXNor" => some .rXNor
  | "rwPrimMSBit" => some .msBit
  | "rwPrimUsingExtern" => some .usingExtern
  | _ => none

/-! ## Kinds, types, signatures -/

def monoSig (t : Ty) : Sig := ⟨[], t⟩

/-- A placeholder signature for binders whose types the concrete syntax
does not carry (variable occurrences, case binders, labels); every one
of them is replaced by elaboration. -/
def pendingSig : Sig := monoSig (.con "()")

mutual

partial def pKind : PM Kind := do
  let k ← pKindAtom
  if ← tokOpt .arrow then pure (.fn k (← pKind)) else pure k

partial def pKindAtom : PM Kind := do
  match ← peek? with
  | some ⟨.star, _, _⟩ => do advance; pure .star
  | some ⟨.name false "Nat" none, _, _⟩ => do advance; pure .nat
  | some ⟨.lparen, _, _⟩ => do
      advance
      let k ← pKind
      expectTok .rparen "')'"
      pure k
  | _ => errP "expected a kind"

end

/-- Type variables bound by the enclosing signature's `forall`, by
unique. -/
abbrev TVScope := Std.HashMap Int TyVar

/-- The built-in type-level arithmetic constructor names `+` `-` `*`
(a `-` immediately followed by `>` is an arrow, settled at lex time). -/
def natOpOpt : PM (Option String) := do
  match ← peek? with
  | some ⟨.plus, _, _⟩  => do advance; pure (some "+")
  | some ⟨.star, _, _⟩  => do advance; pure (some "*")
  | some ⟨.minus, _, _⟩ => do advance; pure (some "-")
  | _ => pure none

mutual

/-- Type, at top (arrow) level: arrows are right-associative and bind
loosest. -/
partial def pTy (tvs : TVScope) : PM Ty := do
  let t ← pTyApp tvs
  if ← tokOpt .arrow then pure (.arrow t (← pTy tvs)) else pure t

/-- Type, at application level (left-associative). The arithmetic
constructors are admitted in head position only (the printer emits
prefix form, `+ 1 2`); an unapplied operator has no printable form and
is rejected. -/
partial def pTyApp (tvs : TVScope) : PM Ty := do
  match ← natOpOpt with
  | some op => do
      let as ← manyP (pTyAtom tvs)
      if as.isEmpty then errP "unapplied type-level operator"
      else pure (as.foldl .app (.con op))
  | none => do
      let h ← pTyAtom tvs
      let as ← manyP (pTyAtom tvs)
      pure (as.foldl .app h)

/-- Type, at atom level: a constructor, a type variable bound by the
enclosing signature (an `occ#uniq` whose unique is not in scope is not a
type atom — that is what terminates a signature line before the
equation that follows it), a natural, the unit/tuple/list constructors,
or a parenthesized type (possibly the infix arithmetic sugar
`(ty natop ty)`, which desugars to prefix applications). -/
partial def pTyAtom (tvs : TVScope) : PM Ty := do
  match ← peek? with
  | some ⟨.name _ s (some u), _, _⟩ =>
      match tvs[u]? with
      | some v => do advance; pure (.var v)
      | none   => errP s!"unbound type variable: {s}#{u}"
  | some ⟨.name _ s none, _, _⟩ =>
      if reservedWords.contains s then errP "expected a type atom"
      else do advance; pure (.con s)
  | some ⟨.int n, _, _⟩ =>
      if n < 0 then errP "expected a type atom"
      else do advance; pure (.nat n.toNat)
  | some ⟨.lbrack, _, _⟩ =>
      match ← pListConName? with
      | some n => pure (.con n)
      | none   => errP "expected a type atom"
  | some ⟨.lparen, _, _⟩ =>
      match ← pTupleName? with
      | some n => pure (.con n)
      | none => do
          advance
          let t ← pTy tvs
          match ← natOpOpt with
          | some op => do
              let t2 ← pTy tvs
              expectTok .rparen "')'"
              pure (.app (.app (.con op) t) t2)
          | none => do
              expectTok .rparen "')'"
              pure t
  | _ => errP "expected a type atom"

end

/-- A type variable binder in a `forall`: `(a#7 :: kind)`. -/
def pTyVarBinder : PM TyVar := do
  expectTok .lparen "'('"
  let (occ, u) ← pUniqName
  expectTok .dcolon "'::'"
  let k ← pKind
  expectTok .rparen "')'"
  pure ⟨occ, u, k⟩

/-- A signature: `forall (a#1 :: kind) ... . ty`, or a bare type.
Returns the scope extended with the quantified variables, for the types
that follow (a definition's parameters and body). -/
def pSig (tvs : TVScope) : PM (Sig × TVScope) := do
  if ← keywordOpt "forall" then
    let v  ← pTyVarBinder
    let vs ← manyP pTyVarBinder
    expectTok .dot "'.'"
    let vs := v :: vs
    let tvs' := vs.foldr (fun v m => m.insert v.uniq v) tvs
    let t ← pTy tvs'
    pure (⟨vs, t⟩, tvs')
  else do
    let t ← pTy tvs
    pure (monoSig t, tvs)

/-- A term binder with an ascribed type: `(x#1 :: ty)`. -/
def pParam (tvs : TVScope) : PM Id := do
  expectTok .lparen "'('"
  let (occ, u) ← pUniqName
  expectTok .dcolon "'::'"
  let t ← pTy tvs
  expectTok .rparen "')'"
  pure ⟨occ, u, monoSig t⟩

/-! ## Expressions -/

/-- Scopes threaded through the expression grammar: the enclosing
signature's type variables, and the join points in scope. -/
structure Scope where
  tvs   : TVScope
  joins : Std.HashMap Int JoinId

/-- The scope for expressions inside processes: monomorphic, no joins. -/
def sc0 : Scope := ⟨{}, {}⟩

/-- `(C :: ty)` (data constructor) or `(rwPrimFoo :: ty)` (builtin): the
`rwPrim` prefix selects the builtin table. -/
def conOrPrim (c : String) (t : Ty) : PM Exp :=
  if c.startsWith "rwPrim" then
    match lookupBuiltin c with
    | some b => pure (.prim t b)
    | none   => errP s!"unknown builtin: {c}"
  else pure (.con t c)

mutual

partial def pExp (sc : Scope) : PM Exp := do
  match ← peek? with
  | some ⟨.lam, _, _⟩ => do
      advance
      let p  ← pParam sc.tvs
      let ps ← manyP (pParam sc.tvs)
      expectTok .arrow "'->'"
      let body ← pExp sc
      pure ((p :: ps).foldr .lam body)
  | _ =>
    if ← keywordOpt "let" then do
      let (b, sc') ← pBind sc
      keyword "in"
      pure (.letE b (← pExp sc'))
    else if ← keywordOpt "case" then do
      let e ← pExp sc
      keyword "of"
      let (occ, u) ← pUniqName
      let alts ← pBracedSep (pAlt sc)
      expectTok .dcolon "'::'"
      let t ← pTy sc.tvs
      -- The case binder's signature (the scrutinee's type) is
      -- reconstructed by elaboration.
      pure (.cases t e ⟨occ, u, pendingSig⟩ alts)
    else if ← keywordOpt "jump" then do
      let (occ, u) ← pUniqName
      match sc.joins[u]? with
      | none   => errP s!"jump to unbound join point: {occ}#{u}"
      | some j => do
          let es ← pParenSep (pExp sc)
          pure (.jump j es)
    else pApp sc

partial def pApp (sc : Scope) : PM Exp := do
  let h ← pAtom sc
  let as ← manyOpt (pArg? sc)
  pure (as.foldl .app h)

partial def pArg? (sc : Scope) : PM (Option Arg) := do
  if ← tokOpt .at then do
    pure (some (.tArg (← pTyAtom sc.tvs)))
  else
    match ← attempt (pAtom sc) with
    | some e => pure (some (.eArg e))
    | none   => pure none

partial def pAtom (sc : Scope) : PM Exp := do
  match ← peek? with
  | some ⟨.str s, _, _⟩ => do advance; pure (.litStr s)
  | some ⟨.name _ s (some u), _, _⟩ =>
      -- A name followed by `::` is not an atom: it starts the next
      -- definition's signature line (or the next command).
      match ← peekAt 1 with
      | some ⟨.dcolon, _, _⟩ => errP "name followed by '::' is not an atom"
      | _ => do advance; pure (.var ⟨s, u, pendingSig⟩)
  | some ⟨.lparen, _, _⟩ => do
      advance
      let e ← pAtomInner sc
      expectTok .rparen "')'"
      pure e
  | _ => errP "expected an atom"

/-- The inside of a parenthesized atom: `list [..] :: ty`,
`vec [..] :: ty`, `lit :: ty`, `C :: ty` / `rwPrimFoo :: ty`, or any
parenthesized expression. -/
partial def pAtomInner (sc : Scope) : PM Exp := do
  if ← keywordOpt "list" then do
    let es ← pBrackSep (pExp sc)
    expectTok .dcolon "'::'"
    pure (.litList (← pTy sc.tvs) es)
  else if ← keywordOpt "vec" then do
    let es ← pBrackSep (pExp sc)
    expectTok .dcolon "'::'"
    pure (.litVec (← pTy sc.tvs) es)
  else
    match ← peek? with
    | some ⟨.int n, _, _⟩ => do
        advance
        expectTok .dcolon "'::'"
        pure (.litInt (← pTy sc.tvs) n)
    | some ⟨.name _ s none, _, _⟩ =>
        -- Reserved words (let, case, jump, ...) start expressions;
        -- anything else commits to the ascription form.
        if reservedWords.contains s || s == "_" then pExp sc
        else do
          advance
          expectTok .dcolon "'::'"
          conOrPrim s (← pTy sc.tvs)
    | some ⟨.lparen, _, _⟩ =>
        match ← pTupleName? with
        | some n => do
            expectTok .dcolon "'::'"
            conOrPrim n (← pTy sc.tvs)
        | none => pExp sc
    | some ⟨.lbrack, _, _⟩ =>
        match ← pListConName? with
        | some n => do
            expectTok .dcolon "'::'"
            conOrPrim n (← pTy sc.tvs)
        | none => pExp sc
    | _ => pExp sc

/-- A local binding (the part between `let` and `in`). Returns the scope
for the let body: a `join` adds its label (join points are not
recursive, so the label is not in scope in its own body). Join labels
carry their arity from the binding's parameter count. -/
partial def pBind (sc : Scope) : PM (Bind × Scope) := do
  if ← keywordOpt "rec" then do
    let eqs ← pBracedSep (pEq sc)
    pure (.recB eqs, sc)
  else if ← keywordOpt "join" then do
    let (occ, u) ← pUniqName
    let ps ← pParenSep (pParam sc.tvs)
    expectTok .equals "'='"
    let body ← pExp sc
    let j : JoinId := ⟨occ, u, ps.length⟩
    pure (.join j ps body, { sc with joins := sc.joins.insert u j })
  else do
    let (x, e) ← pEq sc
    pure (.nonRec x e, sc)

/-- One equation of a (non-recursive or recursive) let:
`x#1 :: ty = e`. -/
partial def pEq (sc : Scope) : PM (Id × Exp) := do
  let (occ, u) ← pUniqName
  expectTok .dcolon "'::'"
  let t ← pTy sc.tvs
  expectTok .equals "'='"
  pure (⟨occ, u, monoSig t⟩, ← pExp sc)

/-- A case alternative: `_ -> e` (default; first, if present),
`C (x#1 :: ty) ... -> e`, or `lit -> e`. -/
partial def pAlt (sc : Scope) : PM Alt := do
  match ← peek? with
  | some ⟨.name false "_" none, _, _⟩ => do
      advance
      expectTok .arrow "'->'"
      pure (.mk .default [] (← pExp sc))
  | some ⟨.int n, _, _⟩ => do
      advance
      expectTok .arrow "'->'"
      pure (.mk (.litAlt n) [] (← pExp sc))
  | _ => do
      let c ← pConName
      let ps ← manyP (pParam sc.tvs)
      expectTok .arrow "'->'"
      pure (.mk (.dataAlt c) ps (← pExp sc))

end

/-! ## Definitions and datatypes -/

/-- A definition: the signature line, then the equation line. The
signature's `forall` binders scope over the equation's parameter and
body types; the equation's name must repeat the signature line's. -/
def pDefn : PM Defn := do
  let (occ, u) ← pUniqName
  expectTok .dcolon "'::'"
  let (sig, tvs) ← pSig {}
  let attr ←
    if ← keywordOpt "inline" then pure (some DefnAttr.inline)
    else if ← keywordOpt "noinline" then pure (some DefnAttr.noinline)
    else pure none
  let orig ←
    if ← keywordOpt "from" then do
      let n ← pBareName
      pure (some (SpecOrigin.spec n (← pParenSep (pTy tvs))))
    else if ← keywordOpt "baked" then do
      pure (some (SpecOrigin.baked (← pBareName)))
    else pure none
  let (occ', u') ← pUniqName
  unless occ == occ' && u == u' do
    errP s!"definition equation name {occ'}#{u'} does not match its signature line ({occ}#{u})"
  let ps ← manyP (pParam tvs)
  expectTok .equals "'='"
  let body ← pExp ⟨tvs, {}⟩
  pure { name := ⟨occ, u, sig⟩, params := ps, body := body, attr := attr, origin := orig }

/-- `data T kind { C1 :: sig1; ... }` (the `data` keyword already
consumed; the constructor list may be empty; each constructor signature
quantifies its own type variables). -/
def pDataDefn : PM DataDefn := do
  let n ← pConName
  let k ← pKind
  let cs ← pBracedSep do
    let c ← pConName
    expectTok .dcolon "'::'"
    let (sig, _) ← pSig {}
    pure { name := c, sig := sig : DataCon }
  pure { name := n, kind := k, cons := cs }

/-! ## Processes (doc/synolon.md §3.4 and §9)

Terminator labels resolve after the whole process is parsed (a
terminator may target a block declared later); a label's signature is
arrows from its block's parameter types to the process output type (a
bookkeeping convention — labels are not values). -/

def pCell? : PM (Option Cell) := do
  if ← keywordOpt "state" then do
    let s ← pBareName
    expectTok .colon "':'"
    let t ← pTy {}
    expectTok .assign "':='"
    let e0 ← if ← keywordOpt "undef" then pure none else some <$> pExp sc0
    expectTok .semi "';'"
    pure (some { name := s, ty := t, init := e0 })
  else pure none

def pCmd? : PM (Option Cmd) := do
  if ← keywordOpt "put" then do
    let s ← pBareName
    let a ← pAtom sc0
    expectTok .semi "';'"
    pure (some (.put s a))
  else
    match ← peek? with
    | some ⟨.name _ _ (some _), _, _⟩ => do
        let (occ, u) ← pUniqName
        expectTok .dcolon "'::'"
        let t ← pTy {}
        expectTok .larrow "'<-'"
        let x : Id := ⟨occ, u, monoSig t⟩
        let c ← if ← keywordOpt "get" then Cmd.get x <$> pBareName
                else Cmd.bind x <$> pExp sc0
        expectTok .semi "';'"
        pure (some c)
    | _ => pure none

mutual

partial def pTerm : PM Term := do
  if ← keywordOpt "pause" then do
    let a ← pAtom sc0
    expectTok .arrow "'->'"
    let (occ, u) ← pUniqName
    pure (.pause a ⟨occ, u, pendingSig⟩ (← pParenSep (pAtom sc0)))
  else if ← keywordOpt "goto" then do
    let (occ, u) ← pUniqName
    pure (.goto ⟨occ, u, pendingSig⟩ (← pParenSep (pAtom sc0)))
  else if ← keywordOpt "halt" then do
    pure (.halt (← pAtom sc0))
  else if ← keywordOpt "case" then do
    let a ← pAtom sc0
    keyword "of"
    pure (.cases a (← pBracedSep pTAlt))
  else errP "expected a block terminator"

partial def pTAlt : PM TAlt := do
  match ← peek? with
  | some ⟨.name false "_" none, _, _⟩ => do
      advance
      expectTok .arrow "'->'"
      pure (.mk .default [] (← pTerm))
  | some ⟨.int n, _, _⟩ => do
      advance
      expectTok .arrow "'->'"
      pure (.mk (.litAlt n) [] (← pTerm))
  | _ => do
      let c ← pConName
      let ps ← manyP (pParam {})
      expectTok .arrow "'->'"
      pure (.mk (.dataAlt c) ps (← pTerm))

end

def pBlockBody : PM (List Cmd × Term) := do
  expectTok .lbrace "'{'"
  let cmds ← manyOpt pCmd?
  let term ← pTerm
  expectTok .rbrace "'}'"
  pure (cmds, term)

def pBlock? : PM (Option (Id × Block)) := do
  if ← keywordOpt "block" then do
    let (occ, u) ← pUniqName
    let ps ← pParenSep (pParam {})
    let (cmds, term) ← pBlockBody
    pure (some (⟨occ, u, pendingSig⟩, ⟨ps, cmds, term⟩))
  else pure none

/-- Rewrite terminator targets to the declared labels (with their
finalized signatures). -/
partial def resolveTerm (ltab : Std.HashMap Int Id) : Term → PM Term
  | .pause o l args => do pure (.pause o (← resolveLabel l) args)
  | .goto l args    => do pure (.goto (← resolveLabel l) args)
  | .halt e         => pure (.halt e)
  | .cases s alts   =>
      Term.cases s <$> alts.mapM fun (.mk c xs t) => TAlt.mk c xs <$> resolveTerm ltab t
where
  resolveLabel (l : Id) : PM Id :=
    match ltab[l.uniq]? with
    | some l' => pure l'
    | none    => errP s!"terminator targets an undeclared block label: {l.occ}#{l.uniq}"

/-- `proc P : ty ~> ty clock? { state* entry block* }` (the `proc`
keyword already consumed). -/
def pProc : PM Proc := do
  let n ← pBareName
  expectTok .colon "':'"
  let it ← pTy {}
  expectTok .squig "'~>'"
  let ot ← pTy {}
  let clk ← if ← tokOpt .at then do keyword "clock"; some <$> pBareName else pure none
  expectTok .lbrace "'{'"
  let cells ← manyOpt pCell?
  keyword "entry"
  let (ecmds, eterm) ← pBlockBody
  let blocks ← manyOpt pBlock?
  expectTok .rbrace "'}'"
  let ltab : Std.HashMap Int Id := blocks.foldl (init := {}) fun m (l, b) =>
    m.insert l.uniq ⟨l.occ, l.uniq, monoSig (b.params.foldr (fun p acc => .arrow p.sig.ty acc) ot)⟩
  let entry : Block := ⟨[], ecmds, ← resolveTerm ltab eterm⟩
  let blocks ← blocks.mapM fun (l, b) => do
    pure (ltab.getD l.uniq l, { b with term := ← resolveTerm ltab b.term : Block })
  pure { name := n, inTy := it, outTy := ot, clock := clk
       , cells := cells, entry := entry, blocks := blocks }

/-! ## Programs -/

/-- `data* defn* proc* ('top' var)?`, to end of input. The machine-level
(Synolon, `.syn`) form has no `top` line; a legacy Eidos machine-level
dump or a hand-written program may carry one, in which case it must
name a parsed definition (matched by unique) and takes that
definition's `Id`. -/
partial def pProgram : PM Program := do
  let ds ← manyOpt do
    if ← keywordOpt "data" then some <$> pDataDefn else pure none
  let fs ← manyOpt do
    match ← peek? with
    | some ⟨.name _ _ (some _), _, _⟩ => some <$> pDefn
    | _ => pure none
  let ps ← manyOpt do
    if ← keywordOpt "proc" then some <$> pProc else pure none
  let top? ← if ← keywordOpt "top" then some <$> pUniqName else pure none
  unless (← atEOF) do errP "expected end of input"
  match top? with
  | none => pure { datas := ds, defns := fs, procs := ps, top := none }
  | some (occ, u) =>
    match fs.find? (fun d => d.name.uniq == u) with
    | some d => pure { datas := ds, defns := fs, procs := ps, top := some d.name }
    | none   => errP s!"top: designated device root {occ}#{u} does not name a definition"

/-! ## Elaboration

Reconstruct the types the concrete syntax leaves implicit: variable
occurrences take their binder's `Id`; case binders take the scrutinee's
synthesized type; block labels were finalized at parse time. Join
types (arrows from parameter types to body type) are threaded through
synthesis for jump results, mirroring the reference (where the
reconstructed type lives on the `JoinId` itself). -/

def withCtx (ctx : String) : Except String α → Except String α
  | .ok a    => .ok a
  | .error e => .error s!"{ctx}: {e}"

structure Env where
  vars  : Std.HashMap Int Id
  joins : Std.HashMap Int Ty

def insertVar (x : Id) (env : Env) : Env :=
  { env with vars := env.vars.insert x.uniq x }

/-- Synthesize the type of an (already elaborated) expression, used
where the concrete syntax omits a type that the abstract syntax
carries: the shared trusting synthesizer `Rwv.Eidos.typeOf`
(Rwv.Eidos.Types), applied at this elaboration environment's join
scope. Grossly ill-typed input is rejected here; everything subtler is
the Synolon checker's job (Rwv.Eidos.Check). -/
def synthTy (env : Env) : Exp → Except String Ty :=
  typeOf env.joins

mutual

partial def elabExp (env : Env) : Exp → Except String Exp
  | .var x =>
      match env.vars[x.uniq]? with
      | some xB => .ok (.var xB)
      | none    => .error s!"unbound variable: {x.occ}#{x.uniq}"
  | e@(.con ..)    => .ok e
  | e@(.prim ..)   => .ok e
  | e@(.litInt ..) => .ok e
  | e@(.litStr ..) => .ok e
  | .litList t es => Exp.litList t <$> es.mapM (elabExp env)
  | .litVec t es  => Exp.litVec t <$> es.mapM (elabExp env)
  | .app e a      => do pure (.app (← elabExp env e) (← elabArg env a))
  | .lam x e      => Exp.lam x <$> elabExp (insertVar x env) e
  | .letE b body  => elabLet env b body
  | .jump l es    => do
      let es' ← es.mapM (elabExp env)
      match env.joins[l.uniq]? with
      | some _ => .ok (.jump l es')
      | none   => .error s!"unbound join point: {l.occ}#{l.uniq}"
  | .cases t e x alts => do
      let e' ← elabExp env e
      let ts ← synthTy env e'
      let x' : Id := { x with sig := monoSig ts }
      let env' := insertVar x' env
      Exp.cases t e' x' <$> alts.mapM (elabAlt env')

partial def elabArg (env : Env) : Arg → Except String Arg
  | .eArg e => Arg.eArg <$> elabExp env e
  | a       => .ok a

partial def elabAlt (env : Env) : Alt → Except String Alt
  | .mk c xs body => Alt.mk c xs <$> elabExp (xs.foldr insertVar env) body

partial def elabLet (env : Env) (b : Bind) (body : Exp) : Except String Exp := do
  match b with
  | .nonRec x e => do
      let e' ← elabExp env e
      pure (.letE (.nonRec x e') (← elabExp (insertVar x env) body))
  | .recB eqs => do
      let env' := eqs.foldr (fun (x, _) => insertVar x) env
      let eqs' ← eqs.mapM fun (x, e) => do pure (x, ← elabExp env' e)
      pure (.letE (.recB eqs') (← elabExp env' body))
  | .join j xs e => do
      let e' ← elabExp (xs.foldr insertVar env) e
      let bt ← synthTy env e'
      let jt := xs.foldr (fun x acc => Ty.arrow x.sig.ty acc) bt
      let env' := { env with joins := env.joins.insert j.uniq jt }
      pure (.letE (.join j xs e') (← elabExp env' body))

end

/-- Elaborate the expressions embedded in a process: cell initials in
the top-level scope; block bodies with parameters and (sequentially)
command binders in scope; terminator-alternative binders over their
terms. -/
partial def elabProc (env : Env) (p : Proc) : Except String Proc := do
  let cells ← p.cells.mapM fun c => do
    pure { c with init := ← c.init.mapM (elabExp env) }
  let entry  ← elabBlock p.entry
  let blocks ← p.blocks.mapM fun (l, b) => do pure (l, ← elabBlock b)
  pure { p with cells := cells, entry := entry, blocks := blocks }
where
  elabBlock (b : Block) : Except String Block := do
    let env₀ := b.params.foldr insertVar env
    let (env', cmds) ← b.cmds.foldlM (init := (env₀, ([] : List Cmd))) fun (e, acc) c => do
      match c with
      | .bind x rhs => do
          let rhs' ← elabExp e rhs
          pure (insertVar x e, Cmd.bind x rhs' :: acc)
      | .get x s    => pure (insertVar x e, Cmd.get x s :: acc)
      | .put s a    => do pure (e, Cmd.put s (← elabExp e a) :: acc)
    pure ⟨b.params, cmds.reverse, ← elabTerm env' b.term⟩

  elabTerm (e : Env) : Term → Except String Term
    | .pause o l args => do pure (.pause (← elabExp e o) l (← args.mapM (elabExp e)))
    | .goto l args    => do pure (.goto l (← args.mapM (elabExp e)))
    | .halt x         => Term.halt <$> elabExp e x
    | .cases s alts   => do
        pure (.cases (← elabExp e s) (← alts.mapM fun (.mk c xs t) =>
          TAlt.mk c xs <$> elabTerm (xs.foldr insertVar e) t))

def elabProgram (p : Program) : Except String Program := do
  let env : Env :=
    ⟨Std.HashMap.ofList (p.defns.map fun d => (d.name.uniq, d.name)), {}⟩
  let fs ← p.defns.mapM fun d =>
    withCtx s!"in definition {d.name.occ}#{d.name.uniq}" do
      pure { d with body := ← elabExp (d.params.foldr insertVar env) d.body }
  let ps ← p.procs.mapM fun pr => withCtx s!"in process {pr.name}" (elabProc env pr)
  pure { p with defns := fs, procs := ps }

end Parse

/-- Parse a program from the concrete syntax of doc/eidos.md §9 — the
machine-level `.syn` format (no `top` line), or a legacy Eidos
machine-level `.eir` dump with one — reconstructing the binder types
the format leaves implicit. The optional `fname` is used in error
messages. -/
def parseEir (input : String) (fname : String := "<input>") : Except String Program := do
  let toks ← Parse.lexAll fname input
  match Parse.pProgram.run { fname := fname, toks := toks, i := 0 } with
  | .error e _ => .error e
  | .ok p _    => Parse.withCtx fname (Parse.elabProgram p)

end Rwv.Eidos

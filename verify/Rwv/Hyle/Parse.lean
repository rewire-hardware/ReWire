/-
Parser for the Hyle concrete syntax (doc/hyle.md §10), a transcription of
the reference implementation (rewire-backend ReWire.Hyle.Parse). The
concrete syntax does not carry per-node widths, so parsing is followed by
an elaboration pass that reconstructs the cached widths bottom-up:
variable widths from binders (defn params, let binders; in the device:
inputs, registers, and instance outputs under qualified names), call
result widths from the callee's signature, xcall widths from the extern
declaration (sum of output widths), literal widths from the literal's
written form, slice/coercion widths from their static parameters.

Display-only metadata in the format — `--@` locator lines, `--|` doc
lines, `tag` lines on devices, and the `noinline` equation prefix — is
accepted and discarded (it is excluded from the Haskell structural
equality, and Rwv.Hyle.Syntax does not model it).
-/
import Rwv.Hyle.Syntax
import Std.Data.HashMap

namespace Rwv.Hyle

namespace Parse

/-! ## Tokens -/

inductive Tok where
  | ident (quoted : Bool) (s : String)
  | num (n : Nat)
  | lit (w : Nat) (v : Nat)
  | lparen | rparen | lbrack | rbrack | langle | rangle
  | comma | colon | assign | arrow | equals | hash | plusColon
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

private def isHexDigit (c : Char) : Bool :=
  c.isDigit || ('a' ≤ c && c ≤ 'f') || ('A' ≤ c && c ≤ 'F')

private def hexVal (c : Char) : Nat :=
  if c.isDigit then c.toNat - '0'.toNat
  else if 'a' ≤ c && c ≤ 'f' then 10 + c.toNat - 'a'.toNat
  else 10 + c.toNat - 'A'.toNat

private def digitsToNat (ds : List Char) : Nat :=
  ds.foldl (fun a c => a * 10 + (c.toNat - '0'.toNat)) 0

private def hexToNat (ds : List Char) : Nat :=
  ds.foldl (fun a c => a * 16 + hexVal c) 0

private def headIsIdentChar : List Char → Bool
  | c :: _ => isIdentChar c
  | []     => false

/-- Lex the body of a `"…"`-quoted name (the opening quote already
consumed). Escapes are restricted to the exact printer-emitted subset
(`ppName` escapes only `\"` and `\\`): any other escaped character is
a lex error. This is deliberately NARROWER than the reference parser,
which accepts megaparsec's full Haskell character-literal repertoire —
the validator format is the printer-emitted subset, and narrower can
only reject, never mis-accept. Raw newlines inside a quoted name are
accepted (as the reference does) and advance the line counter, so
later diagnostics don't drift. Returns the name, the rest of the
input, and the updated line and column. -/
private partial def lexQuoted (fname : String) (ln0 col0 : Nat) :
    List Char → Nat → Nat → List Char → Except String (String × List Char × Nat × Nat)
  | [], _, _, _ => .error s!"{fname}:{ln0}:{col0}: unterminated quoted name"
  | '"' :: rest, ln, col, acc => .ok (String.ofList acc.reverse, rest, ln, col + 1)
  | '\\' :: [], _, _, _ => .error s!"{fname}:{ln0}:{col0}: unterminated quoted name"
  | '\\' :: e :: rest, ln, col, acc =>
      match e with
      | '"'  => lexQuoted fname ln0 col0 rest ln (col + 2) ('"' :: acc)
      | '\\' => lexQuoted fname ln0 col0 rest ln (col + 2) ('\\' :: acc)
      | c    => .error s!"{fname}:{ln}:{col}: unsupported escape '\\{c}' in quoted \
          name (the printer emits only \\\" and \\\\)"
  | '\n' :: rest, ln, _, acc => lexQuoted fname ln0 col0 rest (ln + 1) 1 ('\n' :: acc)
  | c :: rest, ln, col, acc => lexQuoted fname ln0 col0 rest ln (col + 1) (c :: acc)

/-- Shape-check a `--@` source-locator line: the reference parser
requires `file:line:col-line:col`, splitting the four position fields
off from the RIGHT (the file part may itself contain colons and
dashes), and fails the parse on a malformed one. Transcribed here so a
malformed locator is a lex error rather than silently skipped. -/
private def spanShapeOk (txt : String) : Bool := Id.run do
  let some (rest1, c2) := breakOnLast ':' txt | return false
  let some (rest2, cl) := breakOnLast ':' rest1 | return false
  let some (_f, l1) := breakOnLast ':' rest2 | return false
  let some (c1, l2) := breakOnFirst '-' cl | return false
  return [l1, c1, l2, c2].all fun s => !s.isEmpty && s.toList.all Char.isDigit
where
  breakOnLast (c : Char) (t : String) : Option (String × String) :=
    let cs := t.toList
    match (cs.reverse.span (· != c)) with
    | (post, _ :: pre) => some (String.ofList pre.reverse, String.ofList post.reverse)
    | (_, []) => none
  breakOnFirst (c : Char) (t : String) : Option (String × String) :=
    match t.toList.span (· != c) with
    | (pre, _ :: post) => some (String.ofList pre, String.ofList post)
    | (_, []) => none

/-- The tokenizer. Plain `--` line comments and `--|` doc lines are
skipped; `--@` locator lines are shape-checked (`spanShapeOk`) and
then skipped — a malformed locator is a lex error, as in the
reference parser. (The reference additionally accepts metadata lines
only at declaration boundaries; this lexer accepts them anywhere —
a deliberate, comments-only divergence in the lax direction.) -/
private partial def lexGo (fname : String) :
    List Char → Nat → Nat → Array Token → Except String (Array Token)
  | [], _, _, acc => .ok acc
  | c :: cs, ln, col, acc =>
    if c == '\n' then lexGo fname cs (ln + 1) 1 acc
    else if c == ' ' || c == '\t' || c == '\r' then lexGo fname cs ln (col + 1) acc
    else if c == '-' then
      match cs with
      | '-' :: '@' :: cs' =>
          let body := cs'.takeWhile (· != '\n')
          let txt := String.ofList (match body with | ' ' :: b => b | b => b)
          if spanShapeOk txt then lexGo fname (cs'.dropWhile (· != '\n')) ln (col + 3) acc
          else .error s!"{fname}:{ln}:{col}: malformed source locator \
            (expected file:line:col-line:col): {txt}"
      | '-' :: cs' => lexGo fname (cs'.dropWhile (· != '\n')) ln (col + 2) acc
      | '>' :: cs' => lexGo fname cs' ln (col + 2) (acc.push ⟨.arrow, ln, col⟩)
      | _          => .error s!"{fname}:{ln}:{col}: unexpected '-'"
    else if c.isDigit then
      let (ds, rest) := (c :: cs).span Char.isDigit
      let n := digitsToNat ds
      let col' := col + ds.length
      match rest with
      | '\'' :: rest' =>
        -- A literal: nat ' [h hexdigits]; no digits means zero (0', 8').
        match rest' with
        | 'h' :: rest'' =>
          let (hs, rest3) := rest''.span isHexDigit
          if hs.isEmpty then
            .error s!"{fname}:{ln}:{col}: expected hex digits after 'h' in literal"
          else if headIsIdentChar rest3 then
            .error s!"{fname}:{ln}:{col}: malformed literal"
          else
            let v := hexToNat hs
            if v ≥ 2 ^ n then
              .error s!"{fname}:{ln}:{col}: literal value does not fit in {n} bits"
            else lexGo fname rest3 ln (col' + 2 + hs.length) (acc.push ⟨.lit n v, ln, col⟩)
        | _ =>
          if headIsIdentChar rest' then .error s!"{fname}:{ln}:{col}: malformed literal"
          else lexGo fname rest' ln (col' + 1) (acc.push ⟨.lit n 0, ln, col⟩)
      | _ => lexGo fname rest ln col' (acc.push ⟨.num n, ln, col⟩)
    else if isIdentStart c then
      let (xs, rest) := cs.span isIdentChar
      lexGo fname rest ln (col + 1 + xs.length) (acc.push ⟨.ident false (String.ofList (c :: xs)), ln, col⟩)
    else if c == '"' then
      match lexQuoted fname ln col cs ln (col + 1) [] with
      | .ok (s, rest, ln', col') => lexGo fname rest ln' col' (acc.push ⟨.ident true s, ln, col⟩)
      | .error e => .error e
    else
      let sym (t : Tok) (k : Nat) (rest : List Char) : Except String (Array Token) :=
        lexGo fname rest ln (col + k) (acc.push ⟨t, ln, col⟩)
      match c, cs with
      | ':', '=' :: cs' => sym .assign 2 cs'
      | ':', _          => sym .colon 1 cs
      | '+', ':' :: cs' => sym .plusColon 2 cs'
      | '(', _          => sym .lparen 1 cs
      | ')', _          => sym .rparen 1 cs
      | '[', _          => sym .lbrack 1 cs
      | ']', _          => sym .rbrack 1 cs
      | '<', _          => sym .langle 1 cs
      | '>', _          => sym .rangle 1 cs
      | ',', _          => sym .comma 1 cs
      | '=', _          => sym .equals 1 cs
      | '#', _          => sym .hash 1 cs
      | _, _            => .error s!"{fname}:{ln}:{col}: unexpected character '{c}'"

def lexAll (fname input : String) : Except String (Array Token) :=
  lexGo fname input.toList 1 1 #[]

/-! ## Parser combinators

A minimal state monad over the token array. Failure carries a located
message; `attempt` restores the pre-failure position, giving the few
places that need it full backtracking. -/

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

/-- Consume the bare identifier `k` if it is next. -/
def keywordOpt (k : String) : PM Bool := do
  match ← peek? with
  | some ⟨.ident false s, _, _⟩ =>
      if s == k then do advance; pure true else pure false
  | _ => pure false

def keyword (k : String) : PM Unit := do
  unless (← keywordOpt k) do errP s!"expected '{k}'"

/-- Keywords and operator names: rejected as bare identifiers (the
reference parser's `reservedWords`). The soft keywords `noinline` and
`tag` are deliberately absent: they still parse as bare names. -/
def reservedWords : Array String :=
  #[ "let", "in", "if", "then", "else", "undef"
   , "extern", "generic", "clock", "reset", "input", "output", "model"
   , "device", "register", "init", "instance", "of", "next"
   , "add", "sub", "mul", "udiv", "umod", "pow", "and", "or", "xor", "not"
   , "shl", "lshr", "ashr"
   , "eq", "ne", "ult", "ule", "ugt", "uge", "slt", "sle", "sgt", "sge"
   , "redand", "redor", "redxor", "zext", "sext", "trunc", "rep" ]

/-- A name: a bare identifier that is not a reserved word, or any
`"…"`-quoted identifier. -/
def pName : PM String := do
  match ← peek? with
  | some ⟨.ident q s, _, _⟩ =>
      if !q && reservedWords.contains s then
        errP s!"reserved word '{s}' cannot be used as a name"
      else do advance; pure s
  | _ => errP "expected a name"

def pNat : PM Nat := do
  match ← peek? with
  | some ⟨.num n, _, _⟩ => do advance; pure n
  | _ => errP "expected a number"

def pLitBV : PM BV := do
  match ← peek? with
  | some ⟨.lit w v, _, _⟩ => do advance; pure ⟨w, BitVec.ofNat w v⟩
  | _ => errP "expected a literal"

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

/-- `{ keyword clause }`: clauses led by (hard) keyword `k`. -/
def manyKw (k : String) (p : PM α) : PM (List α) :=
  manyOpt do
    if ← keywordOpt k then some <$> p else pure none

/-! ## Grammar: expressions

Raw parse trees reuse the target AST with all reconstructible widths
temporarily 0, exactly as the reference parser does. -/

def nullaryOp? : String → Option Op
  | "add"    => some .add
  | "sub"    => some .sub
  | "mul"    => some .mul
  | "udiv"   => some .udiv
  | "umod"   => some .umod
  | "pow"    => some .pow
  | "and"    => some .and
  | "or"     => some .or
  | "xor"    => some .xor
  | "not"    => some .not
  | "shl"    => some .shl
  | "lshr"   => some .lshr
  | "ashr"   => some .ashr
  | "eq"     => some .eq
  | "ne"     => some .ne
  | "ult"    => some .ult
  | "ule"    => some .ule
  | "ugt"    => some .ugt
  | "uge"    => some .uge
  | "slt"    => some .slt
  | "sle"    => some .sle
  | "sgt"    => some .sgt
  | "sge"    => some .sge
  | "redand" => some .redand
  | "redor"  => some .redor
  | "redxor" => some .redxor
  | _        => none

def isOpName (s : String) : Bool :=
  (nullaryOp? s).isSome || s == "zext" || s == "sext" || s == "trunc" || s == "rep"

def pOp : PM Op := do
  match ← peek? with
  | some ⟨.ident false s, _, _⟩ =>
      match s with
      | "zext"  => do advance; Op.zext <$> pNat
      | "sext"  => do advance; Op.sext <$> pNat
      | "trunc" => do advance; Op.trunc <$> pNat
      | "rep"   => do advance; Op.rep <$> pNat
      | _ =>
        match nullaryOp? s with
        | some op => do advance; pure op
        | none    => errP "expected an operator"
  | _ => errP "expected an operator"

/-- `[ nat { , nat } ]` generic arguments in angle brackets, or nothing.
Committed once `<` is consumed, as in the reference parser. -/
def pGenerics : PM (List Nat) := do
  if ← tokOpt .langle then
    let ns ← sepBy1P pNat .comma
    expectTok .rangle "'>'"
    pure ns
  else pure []

mutual

partial def pExpr : PM Exp := do
  if ← keywordOpt "let" then
    let x ← pName
    expectTok .equals "'='"
    let e₁ ← pExpr
    keyword "in"
    let e₂ ← pExpr
    pure (.letE 0 x e₁ e₂)
  else if ← keywordOpt "if" then
    let c ← pExpr
    keyword "then"
    let t ← pExpr
    keyword "else"
    let e ← pExpr
    pure (.ite 0 c t e)
  else pCat

/-- `app { # app }`, folded to the right: `a # b # c` = `cat a (cat b c)`. -/
partial def pCat : PM Exp := do
  let e ← pApp
  pCatRest e

partial def pCatRest (e : Exp) : PM Exp := do
  if ← tokOpt .hash then
    let e' ← pApp
    pure (.cat e (← pCatRest e'))
  else pure e

partial def pApp : PM Exp := do
  match ← peek? with
  | some ⟨.ident false s, _, _⟩ =>
      if isOpName s then
        let op ← pOp
        let es ← manyP pAtom
        pure (.prim 0 op es)
      else pCallOrAtom
  | some ⟨.ident true _, _, _⟩ => pCallOrAtom
  | _ => pAtom

partial def pCallOrAtom : PM Exp := do
  match ← attempt pCall with
  | some e => pure e
  | none   => pAtom

/-- A name applied to generics and/or arguments; a bare name (neither) is
left to `pAtom`. `call` vs `xcall` vs `var` is settled by elaboration. -/
partial def pCall : PM Exp := do
  let n ← pName
  let cs ← pGenerics
  let es ← manyP pAtom
  if cs.isEmpty && es.isEmpty then errP "bare name parses as an atom"
  else pure (if cs.isEmpty then .call 0 n es else .xcall 0 n cs es)

partial def pAtom : PM Exp := do
  let a ← pAtomBase
  pSlices a

partial def pSlices (a : Exp) : PM Exp := do
  if ← tokOpt .lbrack then
    let i ← pNat
    expectTok .plusColon "'+:'"
    let k ← pNat
    expectTok .rbrack "']'"
    pSlices (.slice i k a)
  else pure a

partial def pAtomBase : PM Exp := do
  match ← peek? with
  | some ⟨.lit w v, _, _⟩ => do advance; pure (.lit ⟨w, BitVec.ofNat w v⟩)
  | some ⟨.ident false "undef", _, _⟩ => do advance; Exp.undef <$> pNat
  | some ⟨.lparen, _, _⟩ => do
      advance
      let e ← pExpr
      expectTok .rparen "')'"
      pure e
  | some ⟨.ident _ _, _, _⟩ => do
      let x ← pName
      -- A name followed by ':' (or ':=') is not an atom: it is the target
      -- of an assignment statement or the start of the next definition.
      match ← peek? with
      | some ⟨.colon, _, _⟩  => errP "name followed by ':' is not an atom"
      | some ⟨.assign, _, _⟩ => errP "name followed by ':=' is not an atom"
      | _ => pure (.var 0 x)
  | _ => errP "expected an atom"

end

/-! ## Grammar: declarations -/

def pTy : PM Nat := do
  expectTok .lbrack "'['"
  let n ← pNat
  expectTok .rbrack "']'"
  pure n

def pPort : PM (String × Nat) := do
  let n ← pName
  expectTok .colon "':'"
  let w ← pTy
  pure (n, w)

def pExtern : PM Extern := do
  keyword "extern"
  let n   ← pName
  let gs  ← if ← keywordOpt "generic" then sepBy1P pName .comma else pure []
  let clk ← if ← keywordOpt "clock" then some <$> pName else pure none
  let rst ← if ← keywordOpt "reset" then some <$> pName else pure none
  let ins  ← manyKw "input" pPort
  let outs ← manyKw "output" pPort
  let m   ← if ← keywordOpt "model" then some <$> pName else pure none
  let kind := match clk, rst with
    | none, none => ExternKind.comb
    | _,    _    => ExternKind.seq clk rst
  pure { name := n, generics := gs, kind := kind, ins := ins, outs := outs, model := m }

def pDefn : PM Defn := do
  let n ← pName
  expectTok .colon "':'"
  expectTok .lparen "'('"
  let tys ← match ← peek? with
    | some ⟨.rparen, _, _⟩ => pure []
    | _ => sepBy1P pTy .comma
  expectTok .rparen "')'"
  expectTok .arrow "'->'"
  let res ← pTy
  let _noinline ← keywordOpt "noinline" -- compilation pragma; accepted and discarded
  let n' ← pName
  unless n == n' do errP s!"definition name '{n'}' does not match its signature ('{n}')"
  let ps ← manyP pName
  expectTok .equals "'='"
  let body ← pExpr
  pure { name := n, sig := { params := tys, result := res }, params := ps, body := body }

def pRegister? : PM (Option Register) := do
  if ← keywordOpt "register" then
    let x ← pName
    expectTok .colon "':'"
    let w ← pTy
    keyword "init"
    let bv ← pLitBV
    pure (some { name := x, width := w, init := bv })
  else pure none

/-- A `tag <name> = <nat>` line: a display name for one value of the
resumption-tag register. Accepted and discarded. `tag` is a soft keyword,
so commit only if the whole line parses. -/
def pTagLine? : PM (Option Unit) :=
  attempt do
    keyword "tag"
    let _ ← pName
    expectTok .equals "'='"
    let _ ← pNat

def pInstance? : PM (Option Instance) := do
  if ← keywordOpt "instance" then
    let x ← pName
    keyword "of"
    let ex ← pName
    let cs ← pGenerics
    pure (some { name := x, ext := ex, generics := cs })
  else pure none

/-- Split a dotted statement target: `i.p := e` assigns instance `i`'s
input port `p` (device-local names may not contain dots), split at the
first dot as in the reference parser. -/
def splitInstTarget (x : String) : Option (String × String) :=
  match x.splitOn "." with
  | i :: rest@(_ :: _) =>
      let p := String.intercalate "." rest
      if p.isEmpty then none else some (i, p)
  | _ => none

def pStmt? : PM (Option Stmt) := do
  if ← keywordOpt "let" then
    let x ← pName
    expectTok .equals "'='"
    let e ← pExpr
    pure (some (.sLet x e))
  else if ← keywordOpt "next" then
    let x ← pName
    expectTok .assign "':='"
    let e ← pExpr
    pure (some (.sNext x e))
  else
    match ← attempt (do let x ← pName; expectTok .assign "':='"; pure x) with
    | some x => do
        let e ← pExpr
        match splitInstTarget x with
        | some (i, p) => pure (some (.sInstIn i p e))
        | none        => pure (some (.sOutput x e))
    | none => pure none

def pDevice : PM Device := do
  keyword "device"
  let n     ← pName
  let ins   ← manyKw "input" pPort
  let outs  ← manyKw "output" pPort
  let regs  ← manyOpt pRegister?
  let _tags ← manyOpt pTagLine?
  let insts ← manyOpt pInstance?
  let body  ← manyOpt pStmt?
  pure { name := n, inputs := ins, outputs := outs
       , registers := regs, instances := insts, body := body }

/-- `{ decl }` to end of input. Every declaration form is committed by its
leading token, so failures inside a declaration are reported in place. -/
partial def pProgram : PM (List Extern × List Defn × List Device) := do
  if ← atEOF then pure ([], [], [])
  else
    match ← peek? with
    | some ⟨.ident false "extern", _, _⟩ => do
        let e ← pExtern
        let (es, ds, vs) ← pProgram
        pure (e :: es, ds, vs)
    | some ⟨.ident false "device", _, _⟩ => do
        let d ← pDevice
        let (es, ds, vs) ← pProgram
        pure (es, ds, d :: vs)
    | _ => do
        let f ← pDefn
        let (es, ds, vs) ← pProgram
        pure (es, f :: ds, vs)

/-! ## Elaboration: reconstruct cached widths bottom-up -/

private def withCtx (ctx : String) : Except String α → Except String α
  | .ok a    => .ok a
  | .error e => .error s!"{ctx}: {e}"

/-- The typing rule for each operator (doc/hyle.md §3.3; reference:
`opResultSize`): given the operand widths, the result width, or `none` if
the operands are ill-typed. -/
def opResultWidth (op : Op) (ws : List Nat) : Option Nat :=
  match op, ws with
  | .add, [a, b] | .sub, [a, b] | .mul, [a, b] | .udiv, [a, b] | .umod, [a, b]
  | .pow, [a, b] | .and, [a, b] | .or, [a, b] | .xor, [a, b] =>
      if a = b then some a else none
  | .not, [a] => some a
  | .shl, [a, _] | .lshr, [a, _] | .ashr, [a, _] => some a
  | .eq, [a, b] | .ne, [a, b]
  | .ult, [a, b] | .ule, [a, b] | .ugt, [a, b] | .uge, [a, b]
  | .slt, [a, b] | .sle, [a, b] | .sgt, [a, b] | .sge, [a, b] =>
      if a = b then some 1 else none
  | .redand, [_] | .redor, [_] | .redxor, [_] => some 1
  | .zext m, [a]  => if a ≤ m then some m else none
  | .sext m, [a]  => if 1 ≤ a && a ≤ m then some m else none
  | .trunc m, [a] => if m ≤ a then some m else none
  | .rep k, [a]   => some (k * a)
  | _, _ => none

/-- The operator name in the concrete syntax, for error messages. -/
def opNameStr : Op → String
  | .add => "add" | .sub => "sub" | .mul => "mul" | .udiv => "udiv"
  | .umod => "umod" | .pow => "pow" | .and => "and" | .or => "or"
  | .xor => "xor" | .not => "not" | .shl => "shl" | .lshr => "lshr"
  | .ashr => "ashr" | .eq => "eq" | .ne => "ne" | .ult => "ult"
  | .ule => "ule" | .ugt => "ugt" | .uge => "uge" | .slt => "slt"
  | .sle => "sle" | .sgt => "sgt" | .sge => "sge" | .redand => "redand"
  | .redor => "redor" | .redxor => "redxor" | .zext _ => "zext"
  | .sext _ => "sext" | .trunc _ => "trunc" | .rep _ => "rep"

/-- An extern call's result width: the sum of the extern's output widths. -/
def externResultWidth (e : Extern) : Nat :=
  e.outs.foldl (fun acc p => acc + p.2) 0

structure SigEnv where
  defns   : Std.HashMap String Sig
  externs : Std.HashMap String Extern

/-- Widths of the names in scope: defn params and let binders; in the
device, inputs, registers, wire lets, and instance outputs (as qualified
`inst.port` names). -/
abbrev VarEnv := Std.HashMap String Nat

/-- A name in expression position: a local variable (when there are no
arguments), or a defn or extern call. Locals shadow globals. -/
def resolveName (env : SigEnv) (g : VarEnv) (x : String) (es : List Exp) : Except String Exp :=
  match es.isEmpty, g[x]? with
  | true, some w => .ok (.var w x)
  | _, _ =>
    match env.defns[x]? with
    | some sig => .ok (.call sig.result x es)
    | none =>
      match env.externs[x]? with
      | some ex => .ok (.xcall (externResultWidth ex) x [] es)
      | none    => .error s!"unknown name: {x}"

mutual

def elabExp (env : SigEnv) (g : VarEnv) : Exp → Except String Exp
  | .lit v   => .ok (.lit v)
  | .undef w => .ok (.undef w)
  | .var _ x => resolveName env g x []
  | .cat e₁ e₂ => do pure (.cat (← elabExp env g e₁) (← elabExp env g e₂))
  | .slice i w e => do
      let e' ← elabExp env g e
      if i + w ≤ e'.width then pure (.slice i w e')
      else .error s!"slice [{i} +: {w}] out of bounds for width {e'.width}"
  | .prim _ op es => do
      let es' ← elabExps env g es
      match opResultWidth op (es'.map Exp.width) with
      | some w => pure (.prim w op es')
      | none   => .error s!"ill-typed application of {opNameStr op} to operand widths {es'.map Exp.width}"
  | .call _ f es => do
      resolveName env g f (← elabExps env g es)
  | .xcall _ n cs es => do
      let es' ← elabExps env g es
      match env.externs[n]? with
      | some ex => pure (.xcall (externResultWidth ex) n cs es')
      | none    => .error s!"unknown extern: {n}"
  | .ite _ c t e => do
      let t' ← elabExp env g t
      let c' ← elabExp env g c
      let e' ← elabExp env g e
      pure (.ite t'.width c' t' e')
  | .letE _ x e₁ e₂ => do
      let e₁' ← elabExp env g e₁
      let e₂' ← elabExp env (g.insert x e₁'.width) e₂
      pure (.letE e₂'.width x e₁' e₂')

def elabExps (env : SigEnv) (g : VarEnv) : List Exp → Except String (List Exp)
  | []      => .ok []
  | e :: es => do pure ((← elabExp env g e) :: (← elabExps env g es))

end

def elabDefn (env : SigEnv) (d : Defn) : Except String Defn := withCtx s!"in definition {d.name}" do
  unless d.params.length = d.sig.params.length do
    throw "parameter count does not match signature"
  let body ← elabExp env (Std.HashMap.ofList (d.params.zip d.sig.params)) d.body
  pure { d with body := body }

/-- Elaborate the device body in statement order: `let`-bound wires enter
the variable context as they are bound. -/
def elabStmts (env : SigEnv) : VarEnv → List Stmt → Except String (List Stmt)
  | _, [] => .ok []
  | g, st :: sts => do
    match st with
    | .sLet x e => do
        let e' ← elabExp env g e
        pure (.sLet x e' :: (← elabStmts env (g.insert x e'.width) sts))
    | .sOutput o e => do
        pure (.sOutput o (← elabExp env g e) :: (← elabStmts env g sts))
    | .sNext r e => do
        pure (.sNext r (← elabExp env g e) :: (← elabStmts env g sts))
    | .sInstIn i p e => do
        pure (.sInstIn i p (← elabExp env g e) :: (← elabStmts env g sts))

def elabDevice (env : SigEnv) (dev : Device) : Except String Device := withCtx s!"in device {dev.name}" do
  let g₀ : VarEnv :=
    Std.HashMap.ofList (dev.inputs ++ dev.registers.map fun r => (r.name, r.width))
  let g₀ ← dev.instances.foldlM (init := g₀) fun g inst =>
    match env.externs[inst.ext]? with
    | some ex => .ok (ex.outs.foldl (fun g p => g.insert (inst.name ++ "." ++ p.1) p.2) g)
    | none    => .error s!"instance {inst.name}: unknown extern {inst.ext}"
  let body ← elabStmts env g₀ dev.body
  pure { dev with body := body }

def elabProgram (exts : List Extern) (ds : List Defn) (dev : Device) : Except String Program := do
  let env : SigEnv :=
    { defns   := Std.HashMap.ofList (ds.map fun d => (d.name, d.sig))
    , externs := Std.HashMap.ofList (exts.map fun e => (e.name, e)) }
  let ds'  ← ds.mapM (elabDefn env)
  let dev' ← elabDevice env dev
  pure { externs := exts, defns := ds', device := dev' }

end Parse

/-- Parse a Hyle program from the concrete syntax of doc/hyle.md §10 (the
`.rwc` format printed by `rwc --core`), reconstructing every expression
node's cached width. The optional `fname` is used in error messages. -/
def parseProgram (input : String) (fname : String := "<input>") : Except String Program := do
  let toks ← Parse.lexAll fname input
  match Parse.pProgram.run { fname := fname, toks := toks, i := 0 } with
  | .error e _ => .error e
  | .ok (exts, ds, devs) _ =>
    match devs with
    | [dev] => Parse.withCtx fname (Parse.elabProgram exts ds dev)
    | []    => .error s!"{fname}: no device declaration"
    | _     => .error s!"{fname}: multiple device declarations"

end Rwv.Hyle

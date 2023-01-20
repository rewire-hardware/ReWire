{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.FIRRTL.Parse (parseFIRRTL) where

import ReWire.FIRRTL.Syntax

import safe Data.Text (Text, singleton, pack)
import qualified Data.Text.IO as Txt
import Data.Functor (void, (<&>), ($>))
import Data.List (foldl')
import Data.Functor.Identity (Identity)
import Text.Megaparsec ( Parsec, many, some, optional, try, (<|>), sepBy, manyTill, oneOf, parse, Token, Tokens, between, empty )
import Text.Megaparsec.Char ( hexDigitChar, digitChar, letterChar, char, spaceChar, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Numeric (readHex)

type Parser = Parsec () Text

parseFIRRTL :: FilePath -> IO Circuit
parseFIRRTL p = Txt.readFile p >>= either (error . show) pure . parse (whiteSpace >> circuit) p

sc :: Parser ()
sc = L.space (void spaceChar) lineComment empty
      where lineComment :: Parser ()
            lineComment  = L.skipLineComment ";"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- lang :: L.GenLanguageDef Text () Identity
-- lang = L.LanguageDef
--       { L.commentStart    = ""
--       , L.commentEnd      = ""
--       , L.commentLine     = ";"
--       , L.nestedComments  = False
--       , L.identStart      = letterChar <|> char '_'
--       , L.identLetter     = letterChar <|> char '_' <|> digitChar
--       , L.opStart         = oneOf ""
--       , L.opLetter        = oneOf ""
--       , L.reservedNames   = [ "input", "output", "wire", "reg", "when", "else", "mux",
--             "of", "node", "is", "invalid", "flip", "circuit",
--             "module", "skip", "UInt", "SInt", "SBits", "UBits", "stop",
--             "printf", "undefined", "with"]
--       , L.reservedOpNames = []
--       , L.caseSensitive   = True
--       }

-- lexer :: L.GenTokenParser Text () Identity
-- lexer = L.makeTokenParser lang

ident :: Parser Text
ident = lexeme $ identStart <> (pack <$> many identLetter)
      where identStart :: Parser Text
            identStart = singleton <$> (letterChar <|> char '_')

            identLetter :: Parser Char
            identLetter = letterChar <|> char '_' <|> digitChar

brackets :: Parser e -> Parser e
brackets = between (symbol "[") $ symbol "]"

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

braces :: Parser e -> Parser e
braces = between (symbol "{") $ symbol "}"

reserved :: Text -> Parser ()
reserved x = symbol x $> ()

parens :: Parser e -> Parser e
parens = between (symbol "(") $ symbol ")"

angles :: Parser e -> Parser e
angles = between (symbol "<") $ symbol ">"

whiteSpace :: Parser ()
whiteSpace = sc

decimal :: Parser Integer
decimal = lexeme L.decimal

dot :: Parser Text
dot = symbol "."

stringLiteral :: Parser Text
stringLiteral = char '"' >> pack <$> manyTill L.charLiteral (char '"')

lparen :: Parser ()
lparen = void $ symbol "("

rparen :: Parser ()
rparen = void $ symbol ")"

pair :: Parser a -> Parser (a, a)
pair p = parens $ (,) <$> p <*> (comma *> p)

info :: Parser (Maybe Info)
info = optional $ Annotation . pack <$> (symbol "@[" *> manyTill L.charLiteral (try (char ']')) <* whiteSpace)

labeled :: Text -> Parser a -> Parser a
labeled s p = reserved s *> p

labeled' :: Text -> Parser a -> Parser a
labeled' s p = symbol s *> p

labeledId :: Text -> Parser Text
labeledId lbl = labeled lbl ident

labeledId' :: Text -> Parser Text
labeledId' lbl = labeled' lbl ident

circuit :: Parser Circuit
circuit = Circuit <$> labeledId "circuit" <*> (colon *> info) <*> some modul

modul :: Parser Module
modul = Module <$> labeledId "module" <*> (colon *> info) <*> many port <*> block

port :: Parser Port
port = try input <|> try output

input :: Parser Port
input = Input <$> labeledId "input" <*> typAnn <*> info

output :: Parser Port
output = Output <$> labeledId "output" <*> typAnn <*> info

cmem :: Parser Stmt
cmem = CMem <$> labeledId' "cmem" <*> typAnn <*> info

smem :: Parser Stmt
smem = SMem <$> labeledId' "smem" <*> typAnn <*> info

typNoVector :: Parser Type
typNoVector = try clockTy <|> try uintTy <|> try sintTy <|> try bundleTy

typ :: Parser Type
typ = try vectorTy <|> typNoVector

typAnn :: Parser Type
typAnn = colon *> typ

clockTy :: Parser Type
clockTy = ClockTy <$ reserved "Clock"

width :: Parser Integer
width = angles decimal

uintTy :: Parser Type
uintTy = UIntTy <$> labeled "UInt" (optional width)

sintTy :: Parser Type
sintTy = SIntTy <$> labeled "SInt" (optional width)

vectorTy :: Parser Type
vectorTy = do t <- typNoVector
              (v : vs) <- some (brackets decimal)
              pure $ foldl' VectorTy (VectorTy t v) vs

bundleTy :: Parser Type
bundleTy = BundleTy <$> braces (member `sepBy` comma)

member :: Parser Member
member  = try flipped <|> binding

flipped :: Parser Member
flipped = Flip <$> labeledId "flip" <*> typAnn

binding :: Parser Member
binding = Binding <$> ident <*> typAnn

block :: Parser [Stmt]
block = some stmt <&> (\ case
      [Skip] -> []
      s      -> s)

stmt :: Parser Stmt
stmt = try skip           <|> try when
   <|> try stop           <|> try wire
   <|> try printf         <|> try reg
   <|> try inferMPort     <|> try readMPort
   <|> try writeMPort     <|> try node
   <|> try partialConnect <|> try connect
   <|> try invalid        <|> try inst
   <|> try mem            <|> try cmem
   <|> try smem

connect :: Parser Stmt
connect = Connect <$> expr <*> labeled' "<=" expr <*> info

partialConnect :: Parser Stmt
partialConnect = PartialConnect <$> expr <*> labeled' "<-" expr <*> info

skip :: Parser Stmt
skip = Skip <$ reserved "skip"

when :: Parser Stmt
when = When <$> labeled "when" expr
      <*> (colon *> info)
      <*> block
      <*> optional (try (reserved "else" *> colon *> block))

inst :: Parser Stmt
inst = Instance <$> labeledId "inst" <*> labeledId "of" <*> info

stop :: Parser Stmt
stop = Stop <$> (reserved "stop" *> lparen *> expr)
      <*> (comma *> expr)
      <*> (comma *> decimal <* rparen)

wire :: Parser Stmt
wire = Wire <$> labeledId "wire" <*> typAnn <*> info

reg :: Parser Stmt
reg = Reg <$> labeledId "reg"
      <*> typAnn
      <*> (comma *> expr)
      <*> optional (do reserved "with"
                       colon *> reserved "reset"
                       symbol "=>" *> pair expr)
      <*> info

invalid :: Parser Stmt
invalid = Invalid <$> (expr <* reserved "is" <* reserved "invalid") <*> info

mport :: Text -> Parser Id
mport s = reserved s *> reserved "mport" *> ident <* symbol "="

inferMPort :: Parser Stmt
inferMPort    = InferMPort <$> mport "infer" <*> expr <*> (comma *> expr) <*> info

readMPort :: Parser Stmt
readMPort    = ReadMPort <$> mport "read" <*> expr <*> (comma *> expr) <*> info

writeMPort :: Parser Stmt
writeMPort    = WriteMPort <$> mport "write" <*> expr <*> (comma *> expr) <*> info

node :: Parser Stmt
node = Node <$> labeledId "node" <*> labeled' "=" expr <*> info

conf :: Text -> Parser a -> Parser a
conf s p = symbol s *> symbol "=>" *> p

mem :: Parser Stmt
mem  = Memory <$> labeledId "mem"
      <*> (colon *> info)
      <*> conf "data-type" typ
      <*> conf "depth" decimal
      <*> conf "read-latency" decimal
      <*> conf "write-latency" decimal
      <*> many (try $ conf "reader" ident)
      <*> many (try $ conf "writer" ident)
      <*> many (try $ conf "read-writer" ident)
      <*> conf "read-under-write" behavior

behavior :: Parser RUW
behavior = try (Old <$ reserved "old")
       <|> try (New <$ reserved "new")
       <|> try (Undefined <$ reserved "undefined")

printf :: Parser Stmt
printf = Printf <$> (reserved "printf" *> lparen *> expr)
      <*> (comma *> expr)
      <*> (comma *> stringLiteral)
      <*> (many (comma *> expr) <* rparen)
      <*> info

unop :: (Exp -> Exp) -> Text -> Parser Exp
unop c p  = c <$> (symbol p *> lparen *> expr <* rparen)

binop :: (Exp -> Exp -> Exp) -> Text -> Parser Exp
binop c p = uncurry c <$> (symbol p *> pair expr)

triop :: (Exp -> Exp -> Exp -> Exp) -> Text -> Parser Exp
triop c p = c <$> (symbol p *> lparen *> expr) <*> (comma *> expr) <*> (comma *> expr <* rparen)

exprNoField :: Parser Exp
exprNoField = try (binop Validif "validif")
          <|> try uInt
          <|> try sInt
          <|> try (triop Mux "mux")

          <|> try (binop Add "add")        <|> try (binop Sub "sub")
          <|> try (binop Cat "cat")        <|> try (triop Bits "bits")
          <|> try (binop Head "head")      <|> try (binop Tail "tail")
          <|> try (binop Mul "mul")        <|> try (binop Div "div")
          <|> try (binop Mod "mod")        <|> try (binop Lt "lt")
          <|> try (binop Leq "leq")        <|> try (binop Gt "gt")
          <|> try (binop Geq "geq")        <|> try (binop Eq "eq")
          <|> try (binop Neq "neq")        <|> try (binop Pad "pad")
          <|> try (unop AsUInt "asUInt")   <|> try (unop AsSInt "asSInt")
          <|> try (unop AsClock "asClock") <|> try (binop Shl "shl")
          <|> try (binop Shr "shr")        <|> try (binop Dshl "dshl")
          <|> try (binop Dshr "dshr")      <|> try (unop Cvt "cvt")
          <|> try (unop Neg "neg")         <|> try (unop Not "not")
          <|> try (binop And "and")        <|> try (binop Or "or")
          <|> try (binop XOr "xor")        <|> try (unop Andr "andr")
          <|> try (unop Orr "orr")         <|> try (unop XOrr "xorr")

          <|> try litInt
          <|> try unbundle
          <|> try index
          <|> try ref

expr :: Parser Exp
expr = try field <|> exprNoField

numLit :: Parser Integer
numLit = try decimal <|> try hexLit <|> try octLit <|> try binLit

hexLit :: Parser Integer
hexLit = between (string "\"h") (char '"') L.hexadecimal

binLit :: Parser Integer
binLit = between (string "\"b") (char '"') L.binary

octLit :: Parser Integer
octLit = between (string "\"o") (char '"') L.octal

uInt :: Parser Exp
uInt = UInt <$> (reserved "UInt" *> optional (angles decimal)) <*> parens numLit

sInt :: Parser Exp
sInt = SInt <$> (reserved "SInt" *> optional (angles decimal)) <*> parens numLit

litInt :: Parser Exp
litInt = LitInt <$> numLit

field :: Parser Exp
field = do e <- exprNoField
           (x : xs) <- some (dot *> ident)
           pure $ foldl' Field (Field e x) xs

index :: Parser Exp
index = Index <$> ref <*> brackets decimal

unbundle :: Parser Exp
unbundle = Unbundle <$> ref <*> brackets expr

ref :: Parser Exp
ref = Ref <$> ident

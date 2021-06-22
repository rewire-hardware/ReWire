{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.FIRRTL.Parse (parseFIRRTL) where

import ReWire.FIRRTL.Syntax

import safe Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as Txt
import Data.Functor (void)
import Data.List (foldl')
import Data.Functor.Identity (Identity)
import Text.Parsec ( Parsec, many, many1, optionMaybe, char
                   , hexDigit, try, (<|>), sepBy, manyTill
                   , oneOf, digit, letter, parse, anyChar )
import qualified Text.Parsec.Token as T
import Numeric (readHex)

type Parser = Parsec Text ()

parseFIRRTL :: FilePath -> IO Circuit
parseFIRRTL p = Txt.readFile p
            >>= pure . parse (whiteSpace >> circuit) p
            >>= either (error . show) pure

lang :: T.GenLanguageDef Text () Identity
lang = T.LanguageDef
      { T.commentStart    = ""
      , T.commentEnd      = ""
      , T.commentLine     = ";"
      , T.nestedComments  = False
      , T.identStart      = letter <|> char '_'
      , T.identLetter     = letter <|> char '_' <|> digit
      , T.opStart         = oneOf ""
      , T.opLetter        = oneOf ""
      , T.reservedNames   = [ "input", "output", "wire", "reg", "when", "else", "mux",
            "of", "node", "is", "invalid", "flip", "circuit",
            "module", "skip", "UInt", "SInt", "SBits", "UBits", "stop",
            "printf", "undefined", "with"]
      , T.reservedOpNames = []
      , T.caseSensitive   = True
      }

lexer :: T.GenTokenParser Text () Identity
lexer = T.makeTokenParser lang

ident :: Parser Text
ident = pack <$> T.identifier lexer

brackets :: Parser e -> Parser e
brackets = T.brackets lexer

comma :: Parser Text
comma = pack <$> T.comma lexer

colon :: Parser Text
colon = pack <$> T.colon lexer

braces :: Parser e -> Parser e
braces = T.braces lexer

reserved :: Text -> Parser ()
reserved = T.reserved lexer . unpack

parens :: Parser e -> Parser e
parens = T.parens lexer

angles :: Parser e -> Parser e
angles = T.angles lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

integer :: Parser Integer
integer = T.integer lexer

symbol :: Text -> Parser Text
symbol s = pack <$> (T.symbol lexer $ unpack s)

dot :: Parser Text
dot = pack <$> T.dot lexer

stringLiteral :: Parser Text
stringLiteral = pack <$> T.stringLiteral lexer

lparen :: Parser ()
lparen = void $ symbol "("

rparen :: Parser ()
rparen = void $ symbol ")"

pair :: Parser a -> Parser (a, a)
pair p = parens $ (,) <$> p <*> (comma *> p)

info :: Parser (Maybe Info)
info = optionMaybe $ Annotation <$> (pack <$> (symbol "@[" *> manyTill anyChar (try (char ']')) <* whiteSpace))

labeled :: Text -> Parser a -> Parser a
labeled s p = reserved s *> p

labeled' :: Text -> Parser a -> Parser a
labeled' s p = symbol s *> p

labeledId :: Text -> Parser Text
labeledId lbl = labeled lbl ident

labeledId' :: Text -> Parser Text
labeledId' lbl = labeled' lbl ident

circuit :: Parser Circuit
circuit = Circuit <$> labeledId "circuit" <*> (colon *> info) <*> many1 modul

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
width = angles integer

uintTy :: Parser Type
uintTy = UIntTy <$> labeled "UInt" (optionMaybe width)

sintTy :: Parser Type
sintTy = SIntTy <$> labeled "SInt" (optionMaybe width)

vectorTy :: Parser Type
vectorTy = do t <- typNoVector
              (v : vs) <- many1 (brackets integer)
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
block = many1 stmt >>= pure . \ case
      [Skip] -> []
      s      -> s

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
      <*> (optionMaybe (try (reserved "else" *> colon *> block)))

inst :: Parser Stmt
inst = Instance <$> labeledId "inst" <*> labeledId "of" <*> info

stop :: Parser Stmt
stop = Stop <$> (reserved "stop" *> lparen *> expr)
      <*> (comma *> expr)
      <*> (comma *> integer <* rparen)

wire :: Parser Stmt
wire = Wire <$> labeledId "wire" <*> typAnn <*> info

reg :: Parser Stmt
reg = Reg <$> labeledId "reg"
      <*> typAnn
      <*> (comma *> expr)
      <*> optionMaybe (do reserved "with"
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
      <*> conf "depth" integer
      <*> conf "read-latency" integer
      <*> conf "write-latency" integer
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
          <|> try (binop Xor "xor")        <|> try (unop Andr "andr")
          <|> try (unop Orr "orr")         <|> try (unop Xorr "xorr")

          <|> try litInt
          <|> try unbundle
          <|> try index
          <|> try ref

expr :: Parser Exp
expr = try field <|> exprNoField

num :: Parser Integer
num = try integer <|> try hex

hex :: Parser Integer
hex = fst . head . readHex <$> (char '"' *> char 'h' *> many hexDigit <* char '"' <* whiteSpace)

uInt :: Parser Exp
uInt = UInt <$> (reserved "UInt" *> optionMaybe (angles integer)) <*> parens num

sInt :: Parser Exp
sInt = SInt <$> (reserved "SInt" *> optionMaybe (angles integer)) <*> parens num

litInt :: Parser Exp
litInt = LitInt <$> integer

field :: Parser Exp
field = do e <- exprNoField
           (x : xs) <- many1 (dot *> ident)
           pure $ foldl' Field (Field e x) xs

index :: Parser Exp
index = Index <$> ref <*> brackets integer

unbundle :: Parser Exp
unbundle = Unbundle <$> ref <*> brackets expr

ref :: Parser Exp
ref = Ref <$> ident

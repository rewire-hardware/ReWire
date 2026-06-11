{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.Core.Parse (parseCore) where

import ReWire.Core.Syntax (LId, Size, Sig (Sig), Defn (Defn), Device (Device), Pat (..), ExternSig (ExternSig), Name, Target (..), Exp (..), Wiring (..))
import ReWire.BitVector (BV, bitVec, nil)
import ReWire.Annotation (noAnn)
import ReWire.Error (failAt, MonadError, AstError)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec ( Parsec, many, try, (<|>), (<?>), manyTill, parse, between, empty, errorBundlePretty, sepBy )
import Text.Megaparsec.Char ( alphaNumChar, char, space1 )
import Text.Read (readMaybe)
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseCore :: (MonadError AstError m, MonadIO m) => FilePath -> m Device
parseCore p = liftIO (T.readFile p) >>= either (failAt noAnn . pack . errorBundlePretty) pure . parse (space >> device) p

device :: Parser Device
device = Device <$> (symbol "device" *> name <* colon) 
                <*> wiring
                <*> loop
                <*> state0
                <*> defns
                <?> "device"

wiring :: Parser Wiring
wiring = Wiring <$> (symbol "inputs" *> wires) <*> (symbol "outputs" *> wires) <*> (symbol "states" *> wires)
      <?> "wiring"

wire :: Parser (Name, Size)
wire = parens ((,) <$> (stringLit <* comma) <*> size)
      <?> "wire"

wires :: Parser [(Text, Size)]
wires = brackets (wire `sepBy` comma)
      <?> "wires"

size :: Num a => Parser a
size = decimal

sizeAnn :: Num a => Parser a
sizeAnn = size <* squote

loop :: Parser Defn
loop = defn

state0 :: Parser Defn
state0 = defn

defns :: Parser [Defn]
defns = many defn

defn :: Parser Defn
defn = Defn noAnn
      <$> name
      <*> (colons *> sig)
      <*> (name *> many lid *> eq *> expr)
      <?> "definition"

sig :: Parser Sig
sig = Sig noAnn <$> many (try $ sigSize <* arrow) <*> sigSize

sigSize :: Parser Size
sigSize = char 'W' *> size

lid :: Parser LId
lid = char '$' *> decimal
      <?> "identifier"

expr :: Parser Exp
expr = try call <|> try lvar <|> try cat <|> lit
      <?> "expression"

lit :: Parser Exp
lit = Lit noAnn <$> bv
      <?> "bit-vector literal"

bv :: Parser BV
bv = bitVec <$> sizeAnn <*> (try (char 'h' *> hexadecimal) <|> pure 0)
      <?> "bit-vector"

lvar :: Parser Exp
lvar = LVar noAnn <$> sizeAnn <*> lid

cat :: Parser Exp
cat = try (braces (Concat noAnn <$> (expr <* comma) <*> expr))
      <|> braces (Concat noAnn <$> (expr <* comma) <*> (foldl' (Concat noAnn) <$> (expr <* comma) <*> (expr `sepBy` comma)))
      <?> "concat expression"

call :: Parser Exp
call = do
      sz   <- sizeAnn
      disc <- symbol "case" *> expr <* symbol "of"
      ps   <- braces (pat `sepBy` comma)
      t    <- arrow *> target
      els  <- try (symbol "_" *> arrow *> expr) <|> pure (Lit noAnn nil)
      pure $ Call noAnn sz t disc ps els
      <?> "call expression"

pat :: Parser Pat
pat = try patVar <|> try patWild <|> try patLit
      <?> "pattern"

patVar :: Parser Pat
patVar = PatVar noAnn <$> (sizeAnn <* symbol "@")

patWild :: Parser Pat
patWild = PatWildCard noAnn <$> (sizeAnn <* symbol "_")

patLit :: Parser Pat
patLit = PatLit noAnn <$> bv

target :: Parser Target
target = try getRef
     <|> try setRef
     <|> try extern
     <|> try prim
     <|> try constant
     <|> global
     <?> "target"

getRef :: Parser Target
getRef = symbol "getRef" *> (GetRef <$> stringLit)
      <?> "getRef"

setRef :: Parser Target
setRef = symbol "setRef" *> (SetRef <$> stringLit)
      <?> "setRef"

prim :: Parser Target
prim = name >>= (\ case
      Just p  -> pure $ Prim p
      Nothing -> empty)
      . readMaybe . unpack
      <?> "primitive"

constant :: Parser Target
constant = Const <$> bv
      <?> "constant"

global :: Parser Target
global = Global <$> name
      <?> "global"

extern :: Parser Target
extern = symbol "extern" *> (Extern <$> externSig <*> stringLit <*> stringLit <*> model)
      <?> "extern"
      where model :: Parser (Maybe Name)
            model = try (Just <$> (symbol "model" *> name))
                <|> pure Nothing

externSig :: Parser ExternSig
externSig = parens (symbol "Sig" *> (ExternSig noAnn <$> wires <*> stringLit <*> stringLit <*> wires <*> wires))
      <?> "extern signature"
---

space :: Parser ()
space = L.space (void space1) lineComment empty
      where lineComment :: Parser ()
            lineComment = L.skipLineComment "--"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

colon :: Parser ()
colon = void $ symbol ":"

comma :: Parser ()
comma = void $ symbol ","

squote :: Parser ()
squote = void $ symbol "'"

eq :: Parser ()
eq = void $ symbol "="

stringLit :: Parser Text
stringLit = char '"' >> pack <$> manyTill L.charLiteral (char '"') <* space

arrow :: Parser ()
arrow = void $ symbol "->"

colons :: Parser ()
colons = colon >> colon

parens :: Parser a -> Parser a
parens = between (symbol "(") $ symbol ")"

braces :: Parser a -> Parser a
braces = between (symbol "{") $ symbol "}"

brackets :: Parser a -> Parser a
brackets = between (symbol "[") $ symbol "]"

decimal :: Num a => Parser a
decimal = lexeme L.decimal

hexadecimal :: Parser Integer
hexadecimal = lexeme L.hexadecimal

name :: Parser Name
name = lexeme $ pack <$> many (gidLetter <|> symbolChars <|> otherChars)
      where gidLetter :: Parser Char
            gidLetter = alphaNumChar

            symbolChars :: Parser Char
            symbolChars = char '!'
                  <|> char '#'
                  <|> char '$'
                  <|> char '%'
                  <|> char '&'
                  <|> char '*'
                  <|> char '+'
                  <|> char '.'
                  <|> char '/'
                  <|> char '<'
                  <|> char '='
                  <|> char '>'
                  <|> char '?'
                  <|> char '@'
                  <|> char '\\'
                  <|> char '^'
                  <|> char '|'
                  <|> char '-'
                  <|> char '~'

            otherChars :: Parser Char
            otherChars = char '_' <|> char '\''

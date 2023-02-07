{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.Core.Parse (parseCore) where

import ReWire.Core.Syntax (LId, Size, Sig (Sig), Defn (Defn), Program (Program), Pat (..), ExternSig (ExternSig), Name, Target (..), Exp (..), Wiring (..))
import ReWire.BitVector (BV, bitVec, nil)
import ReWire.Annotation (noAnn)
import ReWire.Error (failAt, MonadError, AstError)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import Data.List (foldl')
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec ( Parsec, many, try, (<|>), manyTill, parse, between, empty, errorBundlePretty, sepBy )
import Text.Megaparsec.Char ( alphaNumChar, char, space1 )
import Text.Read (readMaybe)
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseCore :: (MonadError AstError m, MonadIO m) => FilePath -> m Program
parseCore p = liftIO (T.readFile p) >>= either (failAt noAnn . pack . errorBundlePretty) pure . parse (space >> program) p

program :: Parser Program
program = Program <$> (symbol "device" *> name <* colon) 
                  <*> wiring
                  <*> loop
                  <*> state0
                  <*> defns

wiring :: Parser Wiring
wiring = Wiring <$> wires "inputs" <*> wires "outputs" <*> wires "states"

wires :: Text -> Parser [(Name, Size)]
wires n = symbol n *> braces (namedSize `sepBy` comma)

namedSize :: Parser (Name, Size)
namedSize = (,) <$> (name <* colon) <*> size

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

sig :: Parser Sig
sig = Sig noAnn <$> many (try $ sigSize <* arrow) <*> sigSize

sigSize :: Parser Size
sigSize = char 'W' *> size

lid :: Parser LId
lid = char '$' *> decimal

expr :: Parser Exp
expr = try lit <|> try lvar <|> try cat <|> try call

lit :: Parser Exp
lit = Lit noAnn <$> bv

bv :: Parser BV
bv = bitVec <$> sizeAnn <*> (char 'h' *> hexadecimal)

lvar :: Parser Exp
lvar = LVar noAnn <$> sizeAnn <*> lid

cat :: Parser Exp
cat = try (braces (Concat noAnn <$> (expr <* comma) <*> expr))
      <|> braces (Concat noAnn <$> (expr <* comma) <*> (foldl' (Concat noAnn) <$> (expr <* comma) <*> (expr `sepBy` comma)))

call :: Parser Exp
call = do
      sz   <- sizeAnn
      disc <- symbol "case" *> expr <* symbol "of"
      ps   <- braces (pat `sepBy` comma)
      t    <- arrow *> target
      els  <- try (symbol "_" *> arrow *> expr) <|> pure (Lit noAnn nil)
      pure $ Call noAnn sz t disc ps els

pat :: Parser Pat
pat = try patVar <|> try patWild <|> try patLit

patVar :: Parser Pat
patVar = PatVar noAnn <$> (sizeAnn <* symbol "@")

patWild :: Parser Pat
patWild = PatWildCard noAnn <$> (sizeAnn <* symbol "_")

patLit :: Parser Pat
patLit = PatLit noAnn <$> bv

target :: Parser Target
target = try getRef
      <|> try setRef
      <|> try prim
      <|> try constant
      <|> try global
      <|> try extern

getRef :: Parser Target
getRef = GetRef <$> stringLit

setRef :: Parser Target
setRef = SetRef <$> stringLit

prim :: Parser Target
prim = (readMaybe . unpack <$> name) >>= \ case
      Just p  -> pure $ Prim p
      Nothing -> empty

constant :: Parser Target
constant = Const <$> bv

global :: Parser Target
global = Global <$> name

extern :: Parser Target
extern = Extern externSig <$> name <*> pure ""

-- | TODO(chathhorn)
externSig :: ExternSig
externSig = ExternSig noAnn [] "" [] []

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
stringLit = char '"' >> pack <$> manyTill L.charLiteral (char '"')

arrow :: Parser ()
arrow = void $ symbol "->"

colons :: Parser ()
colons = colon >> colon

braces :: Parser a -> Parser a
braces = between (symbol "{") $ symbol "}"

decimal :: Num a => Parser a
decimal = lexeme L.decimal

hexadecimal :: Parser Integer
hexadecimal = lexeme L.hexadecimal

name :: Parser Name
name = lexeme $ pack <$> many gidLetter
      where gidLetter :: Parser Char
            gidLetter = alphaNumChar
                  <|> char '$'
                  <|> char '.'
                  <|> char '_'
                  <|> char '<'
                  <|> char '>'
                  <|> char '#'
                  <|> char '\''

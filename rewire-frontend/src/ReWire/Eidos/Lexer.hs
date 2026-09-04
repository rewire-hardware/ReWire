{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | The lexical layer of the Eidos concrete syntax (doc/eidos.md §9): the
--   token parsers, the reserved-word table, and the identifier predicate —
--   each defined once, so that 'ReWire.Eidos.Parse' (which reads the
--   format) and 'ReWire.Eidos.Pretty' (which backtick-quotes exactly the
--   occurrence texts that would not lex as identifiers) cannot disagree.
--   The grammar is newline-insensitive: 'space' skips @--@ line comments
--   along with whitespace, and a @#@ separates an identifier from its
--   unique, terminating keywords too (@case#1@ is a name token).
module ReWire.Eidos.Lexer
      ( Parser, failParse
      , space, lexeme, symbol, withSpan, getAnn, keyword
      , reservedWords, isIdentStart, isIdentChar, identStartChar, identChar, identRaw
      , uniqName, bareName, tupleName, conName, listConName, underscore
      , natural, integer, stringLit
      , comma, semi, arrow, dcolon, equals
      , parens, braces, brackets
      ) where

import ReWire.Annotation (Annote, srcAnnote)
import ReWire.Eidos.Syntax (Uniq)
import ReWire.Error (failAt, MonadError, AstError)

import Control.Monad (void, when)
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor (($>))
import Data.Text (Text, pack)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Text.Megaparsec (Parsec, ParseErrorBundle, many, try, (<|>), (<?>), manyTill, between, notFollowedBy, satisfy, anySingle, empty, getSourcePos, attachSourcePos, errorOffset, bundleErrors, bundlePosState, parseErrorTextPretty)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Pos (SourcePos (..), unPos)

import qualified Data.HashSet               as Set
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text                  as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | A parse failure as a located 'AstError' (at the first error's
--   position).
failParse :: MonadError AstError m => ParseErrorBundle Text Void -> m a
failParse bundle = failAt (srcAnnote (sourceName pos) (lc pos) (lc pos)) (pack $ parseErrorTextPretty e)
      where (e, pos) = NE.head $ fst $ attachSourcePos errorOffset (bundleErrors bundle) (bundlePosState bundle)
            lc sp = (unPos $ sourceLine sp, unPos $ sourceColumn sp)


space :: Parser ()
space = L.space (void space1) (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

-- | Run a parser that builds a node from an annotation, supplying it the
--   source span the parser consumed so the node carries a real location.
withSpan :: Parser (Annote -> a) -> Parser a
withSpan p = do
      s <- getSourcePos
      f <- p
      e <- getSourcePos
      pure $ f $ srcAnnote (sourceName s) (lc s) (lc e)
      where lc sp = (unPos $ sourceLine sp, unPos $ sourceColumn sp)

-- | A point annotation at the current position (for nodes built by folds,
--   where 'withSpan' does not fit).
getAnn :: Parser Annote
getAnn = do
      s <- getSourcePos
      pure $ srcAnnote (sourceName s) (lc s) (lc s)
      where lc sp = (unPos $ sourceLine sp, unPos $ sourceColumn sp)

-- | @#@ separates an identifier from its unique, so it terminates keywords
--   too: @case#1@ is a name token, not the keyword @case@.
keyword :: Text -> Parser ()
keyword k = lexeme $ try $ string' k *> notFollowedBy (identChar <|> char '#')
      where string' :: Text -> Parser ()
            string' = mapM_ char . T.unpack

-- | The reserved words of the concrete syntax: every keyword of the
--   grammar, plus @_@ (the default alternative). One table for the parser
--   (a bare name may not be one of these) and the printer (which quotes an
--   occurrence that is one).
reservedWords :: Set.HashSet Text
reservedWords = Set.fromList
      [ "let", "in", "rec", "join", "jump", "case", "of", "top", "data"
      , "forall", "inline", "noinline", "from", "baked", "list", "vec"
      , "proc", "entry", "block", "state", "put", "get", "pause", "goto", "halt", "undef"
      , "Nat", "_"
      ]

-- | The identifier lexeme: a start character, then identifier characters
--   (dotted, primed, and @$@-marked names included). The printer's quoting
--   predicate is the same pair of tests.
isIdentStart, isIdentChar :: Char -> Bool
isIdentStart c = isAlpha c || c == '_' || c == '$'
isIdentChar  c = isAlphaNum c || c `elem` ("_.$'" :: String)

identStartChar :: Parser Char
identStartChar = satisfy isIdentStart

identChar :: Parser Char
identChar = satisfy isIdentChar

-- | Raw (non-lexeme) dotted identifier text, or a backtick-quoted name
--   (arbitrary text; the printer quotes occurrences that do not lex as
--   identifiers, e.g. operator names).
identRaw :: Parser Text
identRaw = quoted <|> plain
      where plain :: Parser Text
            plain = do
                  c  <- identStartChar
                  cs <- many identChar
                  pure $ pack $ c : cs

            quoted :: Parser Text
            quoted = do
                  _  <- char '`'
                  cs <- many $ satisfy (/= '`')
                  _  <- char '`'
                  pure $ pack cs

-- | A unique-carrying name token, @occ#uniq@ (term variables, type
--   variables, labels). Reserved words are admitted as occurrence text:
--   the @#@ disambiguates them from keywords.
uniqName :: Parser (Text, Uniq)
uniqName = lexeme (try $ (,) <$> identRaw <*> (char '#' *> L.signed (pure ()) L.decimal))
      <?> "name#unique"

-- | A bare dotted name with no unique (type/data constructors, primitives,
--   provenance names).
bareName :: Parser Text
bareName = lexeme (try $ do
      x <- identRaw
      when (x `Set.member` reservedWords) $ fail "reserved word"
      -- A bare "_" is not a name: a constructor named "_" would print
      -- identically to the default case alternative. ("_#u" names are fine.)
      when (x == "_") $ fail "'_' is not a name"
      notFollowedBy $ char '#'
      pure x)
      <?> "name"

-- | The unit and tuple constructor names: @()@, @(,)@, @(,,)@, ...
--   (written tightly, as printed).
tupleName :: Parser Text
tupleName = lexeme (try $ do
      _  <- char '('
      cs <- many $ char ','
      _  <- char ')'
      pure $ pack $ "(" <> cs <> ")")
      <?> "tuple constructor"

-- | A constructor name position: bare, the @(,)@ family, or the list type
--   constructors (which appear as declared datatype names in dumps).
conName :: Parser Text
conName = bareName <|> tupleName <|> listConName

listConName :: Parser Text
listConName = lexeme (try ("[_]" <$ symbol "[_]") <|> try ("[]" <$ symbol "[]"))
      <?> "list constructor"

-- | The default-alternative wildcard (@_@ alone is also a valid identifier
--   start, so it needs the same guards as a keyword).
underscore :: Parser ()
underscore = lexeme $ try $ char '_' *> notFollowedBy (identChar <|> char '#')

natural :: Parser Natural
natural = lexeme L.decimal

integer :: Parser Integer
integer = lexeme $ L.signed (pure ()) L.decimal

-- | String literals, with exactly the escapes the printer emits:
--   @\\\\ \\\" \\n \\t \\r@.
stringLit :: Parser Text
stringLit = lexeme (char '"' *> (pack <$> manyTill strChar (char '"')))
      <?> "string literal"
      where strChar :: Parser Char
            strChar = (char '\\' *> escChar) <|> anySingle

            escChar :: Parser Char
            escChar = char '\\'
                  <|> char '"'
                  <|> (char 'n' $> '\n')
                  <|> (char 't' $> '\t')
                  <|> (char 'r' $> '\r')
                  <?> "escape character (one of \\\\ \\\" \\n \\t \\r)"

comma, semi, arrow, dcolon, equals :: Parser ()
comma  = void $ symbol ","
semi   = void $ symbol ";"
arrow  = void $ symbol "->"
dcolon = void $ symbol "::"
equals = void $ symbol "="

parens, braces, brackets :: Parser a -> Parser a
parens   = between (symbol "(") $ symbol ")"
braces   = between (symbol "{") $ symbol "}"
brackets = between (symbol "[") $ symbol "]"


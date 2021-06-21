module ReWire.FIRRTL.Parse (parseFIRRTL) where

import ReWire.FIRRTL.Sintax

import Data.List (foldl')
import Text.Parsec
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Expr as E
import Text.Parsec.Combinator

import System.Environment
import Control.Monad
import Numeric

parseFIRRTL :: FilePath -> IO Circuit
parseFIRRTL p = do s      <- readFile p
                   let pr =  runParser (whiteSpace >> parseCircuit) () p s
                   case pr of
                      Left err -> error $ (show err)
                      Right ss -> return ss

lang :: L.LanguageDef st
lang = L.emptyDef
      { T.commentLine     = ";"
      , T.nestedComments  = True
      , T.identStart      = letter <|> char '_'
      , T.identLetter     = letter <|> char '_' <|> digit
      , T.reservedNames   = [ "input", "output", "wire", "reg", "when", "else", "mux",
            "of", "node", "is", "invalid", "flip", "circuit",
            "module", "skip", "UInt", "SInt", "SBits", "UBits", "stop",
            "printf", "old", "new", "undefined", "validif", "read-latency",
            "reader", "writer", "readwriter", "write-latency", "read-under-write",
            "depth", "data-type", "with"]
      , T.caseSensitive   = True
      }

lexer         = T.makeTokenParser lang

ident         = T.identifier lexer
semi          = T.semi lexer
brackets      = T.brackets lexer
comma         = T.comma lexer
colon         = T.colon lexer
braces        = T.braces lexer
reserved      = T.reserved lexer
parens        = T.parens lexer
angles        = T.angles lexer
whiteSpace    = T.whiteSpace lexer
integer       = T.integer lexer
symbol        = T.symbol lexer
dot           = T.dot lexer
stringLiteral = T.stringLiteral lexer

parseInfo     = do symbol "@["
                   info <- manyTill anyChar (try (char ']'))
                   whiteSpace
                   return (Annotation info)

parseCircuit  = do reserved "circuit"
                   circ <- ident
                   colon
                   info <- optionMaybe parseInfo
                   mods <- many1 parseModule
                   return (Circuit info circ mods)

parseModule   = do reserved "module"
                   md <- ident
                   colon
                   info  <- optionMaybe (try parseInfo)
                   ports <- many parsePort
                   stmts <- parseBlock
                   return (Module info md ports stmts)

parsePort     = try parseInput <|> try parseOutput

parseInput    = do reserved "input"
                   s <- ident
                   colon
                   pt <- parseType
                   info <- optionMaybe parseInfo
                   return (Input info s pt)

parseOutput   = do reserved "output"
                   s <- ident
                   colon
                   pt <- parseType
                   info <- optionMaybe parseInfo
                   return (Output info s pt)

parseCMem = do symbol "cmem"
               s <- ident
               colon
               pt <- parseType
               info <- optionMaybe parseInfo
               return (CMem info s pt)

parseSMem = do symbol "smem"
               s <- ident
               colon
               pt <- parseType
               info <- optionMaybe parseInfo
               return (SMem info s pt)

parseVectorType =   try parseClock
                <|> try parseUInt_t
                <|> try parseSInt_t
                -- <|> try parseVector
                <|> try parseBundle

parseType =    try parseClock
              <|> try parseVector
              <|> try parseUInt_t
              <|> try parseSInt_t
              <|> try parseBundle

parseClock      = Clock <$ reserved "Clock"

parseUInt_t     = do reserved "UInt"
                     w <- optionMaybe parseWidth
                     return (UIntTy w)

parseWidth      = angles integer
parseSInt_t     = SIntTy <$> (reserved "SInt" *> optionMaybe parseWidth)
parseVector     = do t <- parseVectorType
                     (v : vs) <- many1 (brackets integer)
                     pure $ foldl' Vector (Vector t v) vs

parseBundle     = do braces ( do b <- parseSubBundle
                                 bs <- many ( do comma
                                                 btemp <- parseSubBundle
                                                 return btemp
                                             )
                                 return (Bundle ( b:bs ) )
                            )

parseSubBundle  = try (do reserved "flip"
                          n <- ident
                          colon
                          pt <- parseType
                          return (Flip n pt)
                      )
               <|>(do     n <- ident
                          colon
                          pt <- parseType
                          return (Binding n pt)
                  )

parseBlock = do s  <- parseStmt
                ss <- many parseStmt
                pure $ case (s, ss) of
                  (Skip, []) -> []
                  _          -> (s : ss)

parseWhenlessBlock = do s  <- parseWhenless
                        ss <- many parseWhenless
                        pure $ case (s, ss) of
                          (Skip, []) -> []
                          _          -> (s : ss)

parseWhenless =   try parseSkip
              <|> try parseStop
              <|> try parseWire
              <|> try parsePrintf
              <|> try parseReg
              <|> try parseInferMPort
              <|> try parseReadMPort
              <|> try parseWriteMPort
              <|> try parseNode
              <|> try parsePartialConnect
              <|> try parseConnect
              <|> try parseInvalid
              <|> try parseInstance
              <|> try parseMem
              <|> try parseCMem
              <|> try parseSMem

parseStmt     =   try parseSkip
              <|> try parseWhen
              <|> try parseStop
              <|> try parseWire
              <|> try parsePrintf
              <|> try parseReg
              <|> try parseInferMPort
              <|> try parseReadMPort
              <|> try parseWriteMPort
              <|> try parseNode
              <|> try parsePartialConnect
              <|> try parseConnect
              <|> try parseInvalid
              <|> try parseInstance
              <|> try parseMem
              <|> try parseCMem
              <|> try parseSMem

parseConnect    = do ref <- parseExp
                     symbol "<="
                     e <- parseExp
                     info <- optionMaybe parseInfo
                     return (Connect info ref e)

parsePartialConnect = do ref <- parseExp
                         symbol "<-"
                         e <- parseExp
                         info <- optionMaybe parseInfo
                         return (PartialConnect info ref e)

parseSkip       = Skip <$ reserved "skip"

parseWhen   = do reserved "when"
                 e1 <- parseExp
                 colon
                 _ <- optionMaybe parseInfo
                 whenStmts <- parseBlock
                 elseStmts <- optionMaybe (try (reserved "else" *> colon *> parseBlock))
                 return (WhenElse e1 whenStmts elseStmts)

parseWhen'   = do reserved "when"
                  e1 <- parseExp
                  colon
                  _ <- optionMaybe parseInfo
                  whenStmts <- parseWhenlessBlock
                  elseStmts <- optionMaybe (try (reserved "else" *> colon *> parseWhenlessBlock))
                  return (WhenElse e1 whenStmts elseStmts)

parseInstance   = do reserved "inst"
                     s1 <- ident
                     reserved "of"
                     s2 <- ident
                     info <- optionMaybe parseInfo
                     return (Instance info s1 s2)

parseStop       = do reserved "stop"
                     symbol "("
                     s1 <- parseExp
                     comma
                     s2 <- parseExp
                     comma
                     i <- integer
                     symbol ")"
                     return (Stop s1 s2 i)

parseWire       = do reserved "wire"
                     e <- ident
                     colon
                     p <- parseType
                     info <- optionMaybe parseInfo
                     return (Wire info e p)

parseReg        = do reserved "reg"
                     name <- ident
                     colon
                     p1 <- parseType
                     comma
                     e1 <- parseExp
                     reset <- optionMaybe (do reserved "with"
                                              colon
                                              reserved "reset"
                                              reserved "=>"
                                              symbol "("
                                              e2 <- parseExp
                                              comma
                                              e3 <- parseExp
                                              symbol ")"
                                              return (e2, e3)
                                          )
                     info <- optionMaybe parseInfo
                     return (Reg info name p1 e1 reset)

parseInvalid    = Invalid
                  <$> (parseExp <* reserved "is" <* reserved "invalid")
                  <*> (optionMaybe parseInfo)

parseInferMPort    = do reserved "infer"
                        reserved "mport"
                        s <- ident
                        symbol "="
                        e1 <- parseExp
                        comma
                        e2 <- parseExp
                        info <- optionMaybe parseInfo
                        return (InferMPort info s e1 e2)

parseReadMPort    = do reserved "read"
                       reserved "mport"
                       s <- ident
                       symbol "="
                       e1 <- parseExp
                       comma
                       e2 <- parseExp
                       info <- optionMaybe parseInfo
                       return (ReadMPort info s e1 e2)

parseWriteMPort    = do reserved "write"
                        reserved "mport"
                        s <- ident
                        symbol "="
                        e1 <- parseExp
                        comma
                        e2 <- parseExp
                        info <- optionMaybe parseInfo
                        return (WriteMPort info s e1 e2)

parseNode       = do reserved "node"
                     s <- ident
                     symbol "="
                     e <- parseExp
                     info <- optionMaybe parseInfo
                     return (Node info s e)

parseMem        = do string "mem"
                     mName <- ident
                     colon
                     info <- optionMaybe parseInfo
                     dt <- parseDtype
                     dep <- parseDepth
                     rl <- parseRlatency
                     wl <- parseWlatency
                     rdrs <- many parseReader
                     wrtrs <- many parseWriter
                     rdwriters <- many parseReadWriter
                     ruw <- parseRUW
                     let mem = Mem {data_type=dt, depth=dep, readers=rdrs, writers=wrtrs,
                                    readWriters=rdwriters, readLatency=rl, writeLatency=wl,
                                    readUnderWrite=ruw}
                     return (Memory info mName mem)


parseDtype      = reserved "data-type"
               *> symbol "=>"
               *> parseType

parseDepth      = reserved "depth"
               *> symbol "=>"
               *> integer

parseRlatency      = reserved "read-latency"
                  *> symbol "=>"
                  *> integer

parseWlatency      = reserved "write-latency"
                  *> symbol "=>"
                  *> integer

parseReader      = reserved "reader"
                *> symbol "=>"
                *> ident

parseWriter     = reserved "writer"
               *> symbol "=>"
               *> ident

parseReadWriter = reserved "read-writer"
               *> symbol "=>"
               *> ident

parseRUW         = reserved "read-under-write"
                *> symbol "=>"
                *> parseBehavior

parseBehavior    = try parseOld
               <|> try parseNew
               <|> try parseUndefined

parseOld        = Old <$ reserved "old"

parseNew        = New <$ reserved "new"

parseUndefined  = Undefined <$ reserved "undefined"

parsePrintf     = do reserved "printf"
                     symbol "("
                     r1 <- parseExp
                     comma
                     r2 <- parseExp
                     comma
                     s <- stringLiteral
                     es <- many (do comma
                                    e <- parseExp
                                    return e
                                )
                     symbol ")"
                     info <- optionMaybe parseInfo
                     return (Printf r1 r2 s es info)

parseExp      = try parseValidIf
            <|> try parseUInt
            <|> try parseSInt
            <|> try parseMux
            <|> try parsePrimOps
            <|> try parseLitInt
            <|> try parseSubAcc
            <|> try parseSubfield
            <|> try parseSubindex
            <|> try parseRef

parseValidIf  = do reserved "validif"
                   parens (do e1 <- parseExp
                              comma
                              e2 <- parseExp
                              return (Validif e1 e2)
                          )

parseUInt     = do reserved "UInt"
                   i1 <- optionMaybe (angles integer)
                   i2 <- parens parseNum
                   return (LitUInt i1 i2)

parseNum    =    try integer
             <|> try parseHex

parseHex      = do char '"'
                   char 'h'
                   ds <- many hexDigit
                   let (d,_):_  = readHex ds
                   char '"'
                   whiteSpace
                   return d

parseSInt     = do reserved "SInt"
                   i1 <- optionMaybe (angles integer)
                   i2 <- parens parseNum
                   return (LitSInt i1 i2)

parseLitInt   = LitInt <$> integer

parseMux      = do reserved "mux"
                   parens (do e1 <- parseExp
                              comma
                              e2 <- parseExp
                              comma
                              e3 <- parseExp
                              return (Mux e1 e2 e3)
                          )

parsePrimOps =   try parseAdd     <|>  try parseSub
            <|>  try parseCat     <|>  try parseBits
            <|>  try parseHead    <|>  try parseTail
            <|>  try parseMul     <|>  try parseDiv
            <|>  try parseMod     <|>  try parseLT
            <|>  try parseLEQ     <|>  try parseGT
            <|>  try parseGEQ     <|>  try parseEQ
            <|>  try parseNEQ     <|>  try parsePad
            <|>  try parseAsUInt  <|>  try parseAsSInt
            <|>  try parseAsClock <|>  try parseShl
            <|>  try parseShr     <|>  try parseDShl
            <|>  try parseDShr    <|>  try parseCvt
            <|>  try parseNeg     <|>  try parseNot
            <|>  try parseAnd     <|>  try parseOr
            <|>  try parseXor     <|>  try parseAndr
            <|>  try parseOrr     <|>  try parseXorr

lparen = symbol "("
rparen = symbol ")"

pair = parens $ (,) <$> parseExp <*> (comma *> parseExp)

binop c p = uncurry c <$> (symbol p *> pair)
-- binop c p = c <$> (symbol p *> lparen *> parseExp) <*> (comma *> parseExp <* rparen)
unop c p  = c <$> (symbol p *> lparen *> parseExp <* rparen)
triop c p = c <$> (symbol p *> lparen *> parseExp) <*> (comma *> parseExp) <*> (comma *> parseExp <* rparen)

parseAdd      = binop Add "add"
parseSub      = binop Sub "sub"
parseMul      = binop Mul "mul"
parseDiv      = binop Div "div"
parseMod      = binop Mod "mod"
parseLT       = binop Lt "lt"
parseLEQ      = binop Leq "leq"
parseGT       = binop Gt "gt"
parseGEQ      = binop Geq "geq"
parseEQ       = binop Eq "eq"
parseNEQ      = binop Neq "neq"
parsePad      = binop Pad "pad"
parseAsUInt   = unop AsUInt "asUInt"
parseAsSInt   = unop AsSInt "asSInt"
parseAsClock  = unop AsClock "asClock"
parseShl      = binop Shl "shl"
parseShr      = binop Shr "shr"
parseDShl     = binop Dshl "dshl"
parseDShr     = binop Dshr "dshr"
parseCvt      = unop Cvt "cvt"
parseNeg      = unop Neg "neg"
parseNot      = unop Not "not"
parseAnd      = binop And "and"
parseOr       = binop Or "or"
parseXor      = binop Xor "xor"
parseAndr     = unop Andr "andr"
parseOrr      = unop Orr "orr"
parseXorr     = unop Xorr "xorr"
parseCat      = binop Cat "cat"
parseBits     = triop Bits "bits"
parseHead     = binop Head "head"
parseTail     = binop Tail "tail"

parseSubfield = chainl1 parseRef subHelper

subHelper = dot >> return (\ x (Ref y) -> x :. y )

parseSubindex = Subind <$> parseRef <*> brackets integer
parseSubAcc   = Subacc <$> parseRef <*> brackets parseExp
parseRef      = Ref <$> ident

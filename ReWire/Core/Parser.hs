module ReWire.Core.Parser where

import ReWire.Scoping
import ReWire.Core.Syntax
import Text.Parsec
import Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import Data.Char (isUpper,isLower)
import Data.List (nub)
import Control.Monad (liftM,foldM)

rwcDef :: T.LanguageDef st
rwcDef = L.haskellDef { T.reservedNames   = ["data","of","end","def","is","case"],
                        T.reservedOpNames = ["|","\\","->","::"] }

lexer = T.makeTokenParser rwcDef

identifier     = T.identifier lexer
reserved       = T.reserved lexer
operator       = T.operator lexer
reservedOp     = T.reservedOp lexer
charLiteral    = T.charLiteral lexer
stringLiteral  = T.stringLiteral lexer
natural        = T.natural lexer
integer        = T.integer lexer
float          = T.float lexer
naturalOrFloat = T.naturalOrFloat lexer
decimal        = T.decimal lexer
hexadecimal    = T.hexadecimal lexer
octal          = T.octal lexer
symbol         = T.symbol lexer
lexeme         = T.lexeme lexer
whiteSpace     = T.whiteSpace lexer
parens         = T.parens lexer
braces         = T.braces lexer
angles         = T.angles lexer
brackets       = T.brackets lexer
squares        = T.squares lexer
semi           = T.semi lexer
comma          = T.comma lexer
colon          = T.colon lexer
dot            = T.dot lexer
semiSep        = T.semiSep lexer
semiSep1       = T.semiSep1 lexer
commaSep       = T.commaSep lexer
commaSep1      = T.commaSep1 lexer

tblank = RWCTyCon "_"

varid = lexeme $ try $
        do{ name <- identifier
          ; if isUpper (head name)
             then unexpected ("conid " ++ show name)
             else return name
          }

conid = lexeme $ try $
        do{ name <- identifier
          ; if isLower (head name)
             then unexpected ("varid " ++ show name)
             else return name
          }

prog = do dds  <- many datadecl
          defs <- many defn
          return (RWCProg dds defs)

datadecl = do reserved "data"
              i   <- conid
              tvs <- many varid
              reserved "is"
              dcs <- datacon `sepBy` reservedOp "|"
              reserved "end"
              return (RWCData i (map mkId tvs) dcs)

datacon = do i  <- conid
             ts <- many atype
             return (RWCDataCon i ts)

atype = do i <- conid
           return (RWCTyCon i)
    <|> do i <- varid
           return (RWCTyVar (mkId i))
    <|> do ts <- parens (ty `sepBy` comma)
           case ts of
             []  -> return (RWCTyCon "()")
             [t] -> return t
             _   -> return (foldl RWCTyApp (RWCTyCon ("(" ++ replicate (length ts - 1) ',' ++ ")")) ts)
             
btype = do ts <- many atype
           return (foldl1 RWCTyApp ts)

ty = do ts <- btype `sepBy` reservedOp "->"
        return (foldr1 mkArrow ts)

defn = do i <- varid
          reservedOp "::"
          t <- ty
          reserved "is"
          e <- expr
          reserved "end"
          return (RWCDefn (mkId i) (nub (fv t) :-> t) e)

expr = lamexpr
   <|> do es <- many aexpr
          return (foldl RWCApp (head es) (tail es))

aexpr = do i <- varid
           return (RWCVar (mkId i) tblank)
    <|> do i <- conid
           return (RWCCon i tblank)
    <|> do l <- literal
           return (RWCLiteral l)
    <|> do es <- parens (expr `sepBy` comma)
           case es of
             []  -> return (RWCCon "()" tblank)
             [e] -> return e
             _   -> return (foldl RWCApp (RWCCon ("(" ++ replicate (length es - 1) ',' ++ ")") tblank) es)
             
literal = liftM RWCLitInteger natural
      <|> liftM RWCLitFloat float
      <|> liftM RWCLitChar charLiteral

lamexpr = do reservedOp "\\"
             i <- varid
             reservedOp "->"
             e <- expr
             return (RWCLam (mkId i) tblank e)
      <|> do reserved "case"
             e    <- expr
             reserved "of"
             alts <- braces (alt `sepBy` semi)
             return (RWCCase e alts)

alt = do p <- pat
         reservedOp "->"
         e <- expr
         return (RWCAlt p e)

pat = do i <- conid
         pats <- many apat
         return (RWCPatCon i pats)
  <|> apat

apat = do i <- varid
          return (RWCPatVar (mkId i) tblank)
   <|> do i <- conid
          return (RWCPatCon i [])
   <|> do l <- literal
          return (RWCPatLiteral l)
   <|> do ps <- parens (pat `sepBy` comma)
          case ps of
            []  -> return (RWCPatCon "()" [])
            [p] -> return p
            _   -> return (RWCPatCon ("(" ++ replicate (length ps - 1) ',' ++ ")") ps)
parse :: String -> Either String RWCProg
parse = parsewithname "<no filename>"

parsewithname :: FilePath -> String -> Either String RWCProg
parsewithname filename guts =
  case runParser (whiteSpace >> prog >>= \ p -> whiteSpace >> eof >> return p) () filename guts of
    Left e  -> Left (show e)
    Right p -> Right p
            
parsefile :: FilePath -> IO (Either String RWCProg)
parsefile fname = do guts <- readFile fname
                     return (parsewithname fname guts)

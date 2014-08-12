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
rwcDef = L.haskellDef { T.reservedNames   = ["data","of","let","in","end","def","is","case","bind","vhdl"],
                        T.reservedOpNames = ["|","\\","->","::","<-"] }

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

tblank = RWCTyCon (TyConId "_")

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
          pds  <- many primdefn
          defs <- many defn
          return (RWCProg dds pds defs)

datadecl = do reserved "data"
              i   <- conid
              tvs <- many varid
              reserved "is"
              dcs <- datacon `sepBy` reservedOp "|"
              reserved "end"
              return (RWCData (TyConId i) (map mkId tvs) dcs)

datacon = do i  <- conid
             ts <- many atype
             return (RWCDataCon (DataConId i) ts)

mktuplecon :: Int -> String
mktuplecon n = "Tuple" ++ show n

atype = do tm <- angles ty
           tv <- angles ty
           return (RWCTyComp tm tv)
    <|> do i <- conid
           return (RWCTyCon (TyConId i))
    <|> do i <- varid
           return (RWCTyVar (mkId i))
    <|> do ts <- parens (ty `sepBy` comma)
           case ts of
             []  -> return (RWCTyCon (TyConId "Unit"))
             [t] -> return t
             _   -> return (foldl RWCTyApp (RWCTyCon (TyConId $ mktuplecon (length ts))) ts)
             
btype = do ts <- many1 atype
           return (foldl1 RWCTyApp ts)

ty = do ts <- btype `sepBy1` reservedOp "->"
        return (foldr1 mkArrow ts)

primdefn = do reserved "vhdl"
              i <- varid
              reservedOp "::"
              t <- ty
              reserved "is"
              n <- varid
              return (RWCPrim (mkId i) t n)
              
defn = do i <- varid
          reservedOp "::"
          t <- ty
          reserved "is"
          e <- expr
          reserved "end"
          return (RWCDefn (mkId i) (nub (fv t) :-> t) e)

expr = lamexpr
   <|> do es <- many1 aexpr
          return (foldl1 RWCApp es)

aexpr = do i <- varid
           return (RWCVar (mkId i) tblank)
    <|> do i <- conid
           return (RWCCon (DataConId i) tblank)
    <|> do l <- literal
           return (RWCLiteral l)
    <|> do es <- parens (expr `sepBy` comma)
           case es of
             []  -> return (RWCCon (DataConId "Unit") tblank)
             [e] -> return e
             _   -> return (foldl RWCApp (RWCCon (DataConId $ mktuplecon (length es)) tblank) es)
             
literal = liftM RWCLitInteger natural
      <|> liftM RWCLitFloat float
      <|> liftM RWCLitChar charLiteral

lamexpr = do reservedOp "\\"
             i <- varid
             reservedOp "->"
             e <- expr
             return (RWCLam (mkId i) tblank e)
      <|> do reserved "let"
             x <- varid
             reservedOp "="
             e1 <- expr
             reserved "in"
             e2 <- expr
--             reserved "end"
             return (RWCLet (mkId x) e1 e2)
      <|> do reserved "case"
             e    <- expr
             reserved "of"
             alts <- braces (alt `sepBy` semi)
             return (RWCCase e alts)
      <|> do reserved "bind"
             i  <- varid
             reservedOp "<-"
             ei <- expr
             reserved "in"
             eb <- expr
--             reserved "end"
--             return (RWCBind (mkId i) ei eb)
             return (RWCApp (RWCApp (RWCVar (mkId "bind") tblank) ei) (RWCLam (mkId i) tblank eb))

wildcardify fvs (RWCPatCon i ps)               = RWCPatCon i (map (wildcardify fvs) ps)
wildcardify fvs (RWCPatLiteral l)              = RWCPatLiteral l
wildcardify fvs (RWCPatVar i t) | i `elem` fvs = RWCPatVar i t
                                | otherwise    = RWCPatWild
wildcardify fvs RWCPatWild                        = RWCPatWild

alt = do p <- pat
         reservedOp "->"
         e <- expr
         return (RWCAlt (wildcardify (fv e) p) e)

pat = do i <- conid
         pats <- many apat
         return (RWCPatCon (DataConId i) pats)
  <|> apat

apat = do i <- varid
          return (RWCPatVar (mkId i) tblank)
   <|> do i <- conid
          return (RWCPatCon (DataConId i) [])
   <|> do l <- literal
          return (RWCPatLiteral l)
   <|> do symbol "_"
          return RWCPatWild
   <|> do ps <- parens (pat `sepBy` comma)
          case ps of
            []  -> return (RWCPatCon (DataConId "Unit") [])
            [p] -> return p
            _   -> return (RWCPatCon (DataConId $ mktuplecon (length ps)) ps)
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

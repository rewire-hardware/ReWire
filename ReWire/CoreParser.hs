module ReWire.CoreParser where

import ReWire.Core
import Text.Parsec
import Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import Unbound.LocallyNameless
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

tblank = RWCTyCon "TypeNotInferredYet"

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
          return (RWCProg dds (trec defs))

datadecl = do reserved "data"
              i   <- conid
              tvs <- many varid
              reserved "is"
              dcs <- datacon `sepBy` reservedOp "|"
              reserved "end"
              return (RWCData i (bind (map s2n tvs) dcs))

datacon = do i  <- conid
             ts <- many atype
             return (RWCDataCon i ts)

atype = do i <- conid
           return (RWCTyCon i)
    <|> do i <- varid
           return (RWCTyVar (s2n i))
    <|> do ts <- parens (ty `sepBy` comma)
           case ts of
             []  -> return (RWCTyCon "()")
             [t] -> return t
             _   -> return (foldl RWCTyApp (RWCTyCon ("(" ++ replicate (length ts - 1) ',' ++ ")")) ts)
             
mkarrow t1 t2 = RWCTyApp (RWCTyApp (RWCTyCon "(->)") t1) t2

btype = do ts <- many atype
           return (foldl1 RWCTyApp ts)

ty = do ts <- btype `sepBy` reservedOp "->"
        return (foldr1 mkarrow ts)

defn = do i <- varid
          reservedOp "::"
          t <- ty
          reserved "is"
          e <- expr
          reserved "end"
          return (RWCDefn (s2n i) (embed (setbind (nub $ fv t) (t,e))))

mkApp e1 e2 = return (RWCApp tblank e1 e2)
                 
expr = lamexpr
   <|> do es <- many aexpr
          foldM mkApp (head es) (tail es)

aexpr = do i <- varid
           return (RWCVar tblank (s2n i))
    <|> do i <- conid
           return (RWCCon tblank i)
    <|> do l <- literal
           return (RWCLiteral tblank l)
    <|> do es <- parens (expr `sepBy` comma)
           case es of
             []  -> return (RWCCon tblank "()")
             [e] -> return e
             _   -> foldM mkApp (RWCCon tblank ("(" ++ replicate (length es - 1) ',' ++ ")")) es
             
literal = liftM RWCLitInteger natural
      <|> liftM RWCLitFloat float
      <|> liftM RWCLitChar charLiteral

lamexpr = do reservedOp "\\"
             i <- varid
             reservedOp "->"
             e <- expr
             return (RWCLam tblank (bind (s2n i) e))
      <|> do reserved "case"
             e    <- expr
             reserved "of"
             alts <- braces (alt `sepBy` semi)
             return (RWCCase tblank e alts)

alt = do p <- pat
         reservedOp "->"
         e <- expr
         return (RWCAlt (bind p e))

pat = do i <- conid
         pats <- many apat
         return (RWCPatCon i pats)
  <|> apat

apat = do i <- varid
          return (RWCPatVar (embed tblank) (s2n i))
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
  case runParser (whiteSpace >> prog) () filename guts of
    Left e  -> Left (show e)
    Right p -> Right p
            
parsefile :: FilePath -> IO (Either String RWCProg)
parsefile fname = do guts <- readFile fname
                     return (parsewithname fname guts)

{-
{-
constructor = try (do n <- identifier
                      if isUpper (head n)
                        then return n
                        else fail "name was not a constructor")
          <?> "constructor name"
-}

ty = do (t1,t2) <- parens (do t1 <- ty
                              t2 <- ty
                              return (t1,t2))
        return (RWCTyApp t1 t2)
 <|> do n <- identifier
        if isUpper (head n)
           then return (RWCTyCon n)
           else return (RWCTyVar (s2n n))

pat = do (n,ps) <- parens (do n  <- identifier
                              ps <- many pat
                              return (n,ps))
         return (RWCPatCon n ps)
  <|> do n <- identifier
         return (RWCPatVar (s2n n))
  <|> do l <- literal
         return (RWCPatLiteral l)

alt = do p <- angles pat
         e <- expr
         return (RWCAlt (bind p e))

literal = liftM RWCLitInteger integer
      <|> liftM RWCLitFloat float
      <|> liftM RWCLitChar charLiteral

expr = do (e1,e2) <- parens (do e1 <- expr
                                e2 <- expr
                                return (e1,e2))
          return (RWCApp e1 e2)
   <|> (do l <- literal
           t <- angles ty
           return (RWCLiteral t l))
   <|> (do n <- identifier
           t <- angles ty
           if isUpper (head n)
              then return (RWCCon t n)
              else return (RWCVar t (s2n n)))
   <|> (do (n,e) <- braces (do reservedOp "\\"
                               n <- identifier -- FIXME: check caps
                               reservedOp "->"
                               e <- expr
                               return (n,e))
           return (RWCLam (bind (s2n n) e)))
   <|> (do reserved "case"
           e    <- expr
           reserved "of"
           alts <- many alt
           reserved "end"
           return (RWCCase e alts))

defn = do reserved "def"
          n   <- identifier
--          tvs <- angles (many identifier)
          t   <- angles ty
          let tvs :: [Name RWCTy]
              tvs = nub (fv t)
          reserved "is"
          e    <- expr
          reserved "end"
          return (RWCDefn (s2n n) (embed (setbind tvs (t,e))))


datacon = do n  <- identifier
             ts <- angles (commaSep ty)
             return (RWCDataCon n ts)

datadecl = do reserved "data"
              n   <- identifier
              tvs <- angles (many identifier)
              reserved "of"
              dcs <- many datacon
              reserved "end"
              return (RWCData n (bind (map s2n tvs) dcs))

rwcProg = do dds <- many datadecl
             ds  <- many defn
             return (RWCProg { dataDecls    = dds,
                               defns        = trec ds })
-}
module ReWire.CoreParser where
-- FIXME throughout: at any point where "identifier" occurs, we probably want
-- to reject either Constructors or variables.

import ReWire.Core
import Text.Parsec
import Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import Unbound.LocallyNameless
import Data.Char (isUpper)
import Data.List (nub)
import Control.Monad (liftM)

rwcDef :: T.LanguageDef st
rwcDef = T.LanguageDef { T.commentStart    = "{-",
                         T.commentEnd      = "-}",
                         T.commentLine     = "--",
                         T.nestedComments  = True,
                         T.identStart      = letter,
                         T.identLetter     = letter,
                         T.opStart         = fail "no operators",
                         T.opLetter        = fail "no operators",
                         T.reservedNames   = ["data","of","end","def","is","newtype","case","class","where","instance"],
                         T.reservedOpNames = ["|","\\","->"],
                         T.caseSensitive   = True }

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

{-
constructor = try (do n <- identifier
                      if isUpper (head n)
                        then return n
                        else fail "name was not a constructor")
          <?> "constructor name"
-}

constraint = do n  <- identifier
                ts <- angles (commaSep ty)
                return (RWCConstraint n ts)

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

alt = do (p,guard) <- angles (do p <- pat
                                 reservedOp "|"
                                 g <- expr
                                 return (p,g))
         e         <- expr
         return (RWCAlt (bind p (guard,e)))

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

instancemethod = do reserved "def"
                    n   <- identifier
--                    tvs <- angles (many identifier)
                    cs  <- angles (many constraint)
                    t   <- angles ty
                    let tvs :: [Name RWCTy]
                        tvs = nub (fv cs ++ fv t)
                    reserved "is"
                    e   <- expr
                    reserved "end"
                    return (RWCInstanceMethod (s2n n) (setbind tvs (cs,t,e)))
                    

instancedecl = do reserved "instance"
--                  tvs <- angles (many identifier)
                  cs  <- angles (many constraint)
                  ts  <- angles (commaSep ty)
                  let tvs :: [Name RWCTy]
                      tvs = nub (fv cs ++ fv ts)
                  reserved "where"
                  ds  <- many instancemethod
                  reserved "end"
                  return (RWCInstance (setbind tvs (cs,ts)) ds)

defn = do reserved "def"
          n   <- identifier
--          tvs <- angles (many identifier)
          cs  <- angles (many constraint)
          t   <- angles ty
          let tvs :: [Name RWCTy]
              tvs = nub (fv cs ++ fv t)
          reserved "is"
          e    <- expr
          reserved "end"
          return (RWCDefn (s2n n) (embed (setbind tvs (cs,t,e))))
   <|> do reserved "class"
          n   <- identifier
---          tvs <- angles (many identifier)
          cs  <- angles (many constraint)
          ts  <- angles (commaSep ty)
          let tvs :: [Name RWCTy]
              tvs = nub (fv cs ++ fv ts)
          reserved "where"
          ms  <- many classmethod
          is  <- many instancedecl
          reserved "end"
          return (RWCClass n (embed (setbind tvs (cs,ts))) ms (embed is))

classmethod = do reserved "method"
                 n   <- identifier
--                 tvs <- angles (many identifier)
                 cs  <- angles (many constraint)
                 t   <- angles ty
                 let tvs :: [Name RWCTy]
                     tvs = nub (fv cs ++ fv t)
                 impl <- optionMaybe (do reserved "is"
                                         expr)
                 reserved "end"
                 return (RWCClassMethod (s2n n) (embed (setbind tvs (cs,t,impl))))

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

newtypecon = do n <- identifier
                t <- angles ty
                return (RWCNewtypeCon n t)

newtypedecl = do reserved "newtype"
                 n   <- identifier
                 tvs <- angles (many identifier)
                 reserved "of"
                 nc  <- newtypecon
                 reserved "end"
                 return (RWCNewtype n (bind (map s2n tvs) nc))

rwcProg = do dds <- many datadecl
             nds <- many newtypedecl
             ds  <- many defn
             return (RWCProg { dataDecls    = dds,
                               newtypeDecls = nds,
                               defns        = trec ds })
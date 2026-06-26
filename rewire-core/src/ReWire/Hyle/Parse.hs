{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
-- | Parser for the Hyle concrete syntax (doc/hyle.md, section 10).
--
--   The concrete syntax does not carry per-node widths, so parsing is
--   followed by an elaboration pass that reconstructs the cached sizes
--   bottom-up: variable sizes from binders, call result sizes from the
--   signatures of the named defns and externs. Full well-formedness checking
--   (ReWire.Hyle.Check) is separate; elaboration only fails where a size
--   cannot be reconstructed at all.
module ReWire.Hyle.Parse (parseHyle, parseHyleText) where

import ReWire.Annotation (Annote, noAnn)
import ReWire.BitVector (BV, bitVec)
import ReWire.Error (failAt, MonadError, AstError)
import ReWire.Hyle.Syntax
import ReWire.Pretty (showt)

import Control.Monad (unless, when, foldM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor (void)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text, pack)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Text.Megaparsec (Parsec, many, try, (<|>), (<?>), manyTill, parse, between, errorBundlePretty, sepBy, sepBy1, notFollowedBy, optional, satisfy, eof, empty)
import Text.Megaparsec.Char (char, space1)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseHyle :: (MonadError AstError m, MonadIO m) => FilePath -> m Program
parseHyle p = liftIO (T.readFile p) >>= flip parseHyleText p

parseHyleText :: MonadError AstError m => Text -> FilePath -> m Program
parseHyleText txt p = either (failAt noAnn . pack . errorBundlePretty) elabProgram
      $ parse (space *> program <* eof) p txt

---
--- Raw parsing. Sizes that aren't manifest in the syntax are filled with 0
--- and reconstructed by elaboration below.
---

data Decl = DExtern Extern | DDefn Defn | DDevice Device

program :: Parser Program
program = do
      ds <- many decl
      case [ d | DDevice d <- ds ] of
            [dev] -> pure $ Program [ e | DExtern e <- ds ] [ d | DDefn d <- ds ] dev
            []    -> fail "no device declaration"
            _     -> fail "multiple device declarations"

decl :: Parser Decl
decl = DExtern <$> extern
   <|> DDevice <$> device
   <|> DDefn   <$> defn
   <?> "declaration (extern, device, or definition)"

extern :: Parser Extern
extern = do
      keyword "extern"
      n    <- name
      gs   <- option' [] $ keyword "generic" *> (name `sepBy1` comma)
      clk  <- optional $ try $ keyword "clock" *> name
      rst  <- optional $ try $ keyword "reset" *> name
      ins  <- many $ try $ keyword "input"  *> port
      outs <- many $ try $ keyword "output" *> port
      m    <- optional $ try $ keyword "model" *> name
      let k = case (clk, rst) of
            (Nothing, Nothing) -> Comb
            _                  -> Seq clk rst
      pure $ Extern noAnn n gs k ins outs m
      <?> "extern declaration"
      where option' :: a -> Parser a -> Parser a
            option' x p = try p <|> pure x

port :: Parser (Name, Size)
port = (,) <$> name <*> (colon *> ty)

ty :: Parser Size
ty = brackets decimal <?> "type"

defn :: Parser Defn
defn = do
      n   <- name
      sig <- colon *> (Sig noAnn <$> parens (ty `sepBy` comma) <*> (arrow *> ty))
      n'  <- name
      unless (n == n') $ fail "definition name does not match its signature"
      ps  <- many name
      e   <- equals *> expr
      pure $ Defn noAnn n sig ps e
      <?> "definition"

device :: Parser Device
device = do
      keyword "device"
      n     <- name
      ins   <- many $ try $ keyword "input"  *> port
      outs  <- many $ try $ keyword "output" *> port
      regs  <- many $ try register
      insts <- many $ try inst
      body  <- many stmt
      pure $ Device noAnn n ins outs regs insts body
      <?> "device declaration"

register :: Parser Register
register = do
      keyword "register"
      (x, sz) <- port
      bv      <- keyword "init" *> lit
      pure $ Register noAnn x sz bv

inst :: Parser Instance
inst = do
      keyword "instance"
      x  <- name
      ex <- keyword "of" *> name
      cs <- generics
      pure $ Instance noAnn x ex cs

stmt :: Parser Stmt
stmt = (SLet noAnn <$> (keyword "let" *> name) <*> (equals *> expr))
   <|> (SNext noAnn <$> (keyword "next" *> name) <*> (assign *> expr))
   <|> do
            x <- try $ name <* assign
            e <- expr
            -- A dotted target is an instance input port. (Device-local
            -- names may not contain dots; see ReWire.Hyle.Check.)
            case T.breakOn "." x of
                  (i, T.stripPrefix "." -> Just p) | not (T.null p) -> pure $ SInstIn noAnn i p e
                  _                                                 -> pure $ SOutput noAnn x e
   <?> "statement"

---
--- Expressions.
---

expr :: Parser Exp
expr = letE <|> ifE <|> catE
      <?> "expression"

letE :: Parser Exp
letE = do
      keyword "let"
      x  <- name
      e1 <- equals *> expr
      e2 <- keyword "in" *> expr
      pure $ Let noAnn 0 x e1 e2

ifE :: Parser Exp
ifE = do
      keyword "if"
      c <- expr
      t <- keyword "then" *> expr
      e <- keyword "else" *> expr
      pure $ If noAnn 0 c t e

catE :: Parser Exp
catE = foldr1 (Cat noAnn) <$> appE `sepBy1` symbol "#"

appE :: Parser Exp
appE = try primE <|> try callE <|> atomE
      <?> "application"

primE :: Parser Exp
primE = do
      op <- primOp
      es <- many atomE
      pure $ Prim noAnn 0 op es

-- | Operator names are reserved words, so this never collides with 'name'.
primOp :: Parser Op
primOp = choiceOps
      [ ("zext" , ZExt  <$> decimal)
      , ("sext" , SExt  <$> decimal)
      , ("trunc", Trunc <$> decimal)
      , ("rep"  , Rep   <$> decimal)
      ] <|> choiceOps (map (\ op -> (opName op, pure op)) nullaryOps)
      where choiceOps :: [(Text, Parser Op)] -> Parser Op
            choiceOps = foldr (\ (k, p) acc -> try (keyword k *> p) <|> acc) empty

            nullaryOps :: [Op]
            nullaryOps =
                  [ Add, Sub, Mul, UDiv, UMod, Pow, And, Or, XOr, Not
                  , Shl, LShr, AShr
                  , Eq, Ne, ULt, ULe, UGt, UGe, SLt, SLe, SGt, SGe
                  , RedAnd, RedOr, RedXOr
                  ]

-- | A name applied to generics and/or arguments. Resolved by elaboration
--   into a defn call, an extern call, or (when there are no arguments) a
--   variable reference.
callE :: Parser Exp
callE = do
      n  <- name
      cs <- generics
      es <- many atomE
      when (null cs && null es) $ fail "bare name parses as an atom" -- defer to atomE
      pure $ if null cs then Call noAnn 0 n es else XCall noAnn 0 n cs es

atomE :: Parser Exp
atomE = do
      a  <- atomBase
      foldl' (\ e (i, k) -> Slice noAnn i k e) a <$> many sliceSuffix
      where sliceSuffix :: Parser (Index, Size)
            sliceSuffix = brackets $ (,) <$> decimal <*> (symbol "+:" *> decimal)

atomBase :: Parser Exp
atomBase = Lit noAnn <$> lit
       <|> Undef noAnn <$> (keyword "undef" *> decimal)
       -- A name followed by ":" is not an atom: it is the target of an
       -- assignment statement or the start of the next definition. (The
       -- expression grammar itself has no ":".)
       <|> try (Var noAnn 0 <$> (name <* notFollowedBy (symbol ":"))) -- possibly a 0-ary defn call; elaboration resolves
       <|> parens expr
       <?> "atom"

generics :: Parser [Natural]
generics = (symbol "<" *> (decimal `sepBy1` comma) <* symbol ">") <|> pure []

lit :: Parser BV
lit = do
      w <- try $ decimal <* char '\''
      v <- (char 'h' *> L.hexadecimal) <|> pure 0
      notFollowedBy identChar
      space
      when ((v :: Integer) >= 2 ^ (w :: Size)) $
            fail $ "literal value does not fit in " <> show w <> " bits"
      pure $ bitVec (fromIntegral w) v
      <?> "literal"

---
--- Lexing.
---

space :: Parser ()
space = L.space (void space1) (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

keyword :: Text -> Parser ()
keyword k = lexeme $ try $ string' k *> notFollowedBy identChar
      where string' :: Text -> Parser ()
            string' = mapM_ char . T.unpack

identStartChar :: Parser Char
identStartChar = satisfy $ \ c -> isAlpha c || c == '_' || c == '$'

identChar :: Parser Char
identChar = satisfy $ \ c -> isAlphaNum c || c `elem` ("_.$'" :: String)

name :: Parser Name
name = lexeme (try bare) <|> quoted
      <?> "name"
      where bare :: Parser Name
            bare = do
                  c  <- identStartChar
                  cs <- many identChar
                  let x = pack $ c : cs
                  when (x `Set.member` reservedWords) $ fail "reserved word"
                  pure x

            quoted :: Parser Name
            quoted = lexeme $ char '"' *> (pack <$> manyTill L.charLiteral (char '"'))

colon, comma, arrow, equals, assign :: Parser ()
colon  = void $ symbol ":"
comma  = void $ symbol ","
arrow  = void $ symbol "->"
equals = void $ symbol "="
assign = void $ symbol ":="

parens, brackets :: Parser a -> Parser a
parens   = between (symbol "(") $ symbol ")"
brackets = between (symbol "[") $ symbol "]"

decimal :: Num a => Parser a
decimal = lexeme L.decimal

---
--- Elaboration: reconstruct cached sizes bottom-up.
---

data SigEnv = SigEnv
      { envDefns   :: HashMap GId Sig
      , envExterns :: HashMap Name Extern
      }

type VarEnv = HashMap Name Size

elabProgram :: MonadError AstError m => Program -> m Program
elabProgram (Program exts ds dev) = do
      ds'  <- mapM (elabDefn env) ds
      dev' <- elabDevice env dev
      pure $ Program exts ds' dev'
      where env :: SigEnv
            env = SigEnv (Map.fromList $ map (\ d -> (defnName d, defnSig d)) ds)
                         (Map.fromList $ map (\ e -> (extName e, e)) exts)

elabDefn :: MonadError AstError m => SigEnv -> Defn -> m Defn
elabDefn env (Defn an n sig@(Sig _ argSzs _) ps body) = do
      unless (length ps == length argSzs) $
            failAt an $ n <> ": parameter count does not match signature"
      body' <- elabExp env (Map.fromList $ zip ps argSzs) body
      pure $ Defn an n sig ps body'

elabDevice :: MonadError AstError m => SigEnv -> Device -> m Device
elabDevice env (Device an n ins outs regs insts body) = do
      g0    <- foldM instOuts (Map.fromList $ ins <> map (\ (Register _ x sz _) -> (x, sz)) regs) insts
      body' <- snd <$> foldM elabStmt (g0, []) body
      pure $ Device an n ins outs regs insts $ reverse body'
      where instOuts :: MonadError AstError m => VarEnv -> Instance -> m VarEnv
            instOuts g (Instance an' x ex _) = case Map.lookup ex $ envExterns env of
                  Just e  -> pure $ foldr (\ (p, sz) -> Map.insert (x <> "." <> p) sz) g $ extOutputs e
                  Nothing -> failAt an' $ "instance " <> x <> ": unknown extern " <> ex

            elabStmt :: MonadError AstError m => (VarEnv, [Stmt]) -> Stmt -> m (VarEnv, [Stmt])
            elabStmt (g, acc) = \ case
                  SLet an' x e    -> do
                        e' <- elabExp env g e
                        pure (Map.insert x (sizeOf e') g, SLet an' x e' : acc)
                  SOutput an' x e -> (g, ) . (: acc) . SOutput an' x   <$> elabExp env g e
                  SNext an' x e   -> (g, ) . (: acc) . SNext an' x     <$> elabExp env g e
                  SInstIn an' x p e -> (g, ) . (: acc) . SInstIn an' x p <$> elabExp env g e

elabExp :: MonadError AstError m => SigEnv -> VarEnv -> Exp -> m Exp
elabExp env = go
      where go :: MonadError AstError m => VarEnv -> Exp -> m Exp
            go g = \ case
                  e@Lit {}            -> pure e
                  e@Undef {}          -> pure e
                  Var an _ x          -> resolveName g an x []
                  Cat an e1 e2        -> Cat an <$> go g e1 <*> go g e2
                  Slice an i k e      -> do
                        e' <- go g e
                        unless (fromIntegral i + k <= sizeOf e') $
                              failAt an $ "slice [" <> showt i <> " +: " <> showt k
                                       <> "] out of bounds for width " <> showt (sizeOf e')
                        pure $ Slice an i k e'
                  Prim an _ op es     -> do
                        es' <- mapM (go g) es
                        case opResultSize op $ map sizeOf es' of
                              Just sz -> pure $ Prim an sz op es'
                              Nothing -> failAt an $ "ill-typed application of " <> opName op
                                                  <> " to operand widths " <> showt (map sizeOf es')
                  Call an _ n es      -> mapM (go g) es >>= resolveName g an n
                  XCall an _ n cs es  -> do
                        es' <- mapM (go g) es
                        ex  <- lookupExtern an n
                        pure $ XCall an (externResultSize ex) n cs es'
                  If an _ c t e       -> do
                        t' <- go g t
                        If an (sizeOf t') <$> go g c <*> pure t' <*> go g e
                  Let an _ x e1 e2    -> do
                        e1' <- go g e1
                        e2' <- go (Map.insert x (sizeOf e1') g) e2
                        pure $ Let an (sizeOf e2') x e1' e2'

            -- | A name in expression position: a local variable (when there
            --   are no arguments), or a defn or extern call. Locals shadow
            --   globals.
            resolveName :: MonadError AstError m => VarEnv -> Annote -> Name -> [Exp] -> m Exp
            resolveName g an n es
                  | null es, Just sz <- Map.lookup n g                  = pure $ Var an sz n
                  | Just (Sig _ _ res) <- Map.lookup n $ envDefns env   = pure $ Call an res n es
                  | Just ex <- Map.lookup n $ envExterns env            = pure $ XCall an (externResultSize ex) n [] es
                  | otherwise                                           = failAt an $ "unknown name: " <> n

            lookupExtern :: MonadError AstError m => Annote -> Name -> m Extern
            lookupExtern an n = case Map.lookup n $ envExterns env of
                  Just ex -> pure ex
                  Nothing -> failAt an $ "unknown extern: " <> n

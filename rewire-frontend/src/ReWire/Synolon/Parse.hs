{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
-- | Parser for the Synolon concrete syntax (@.syn@; doc/synolon.md sections
--   3.4 and 9): the process declarations, and the program that embeds Eidos
--   datatypes and definitions (parsed and elaborated by
--   'ReWire.Eidos.Parse', as are the expressions inside blocks).
--   'ReWire.Synolon.Pretty' is the other half of the round-trip contract.
--
--   Terminator labels resolve after the whole process is parsed (a
--   terminator may target a block declared later); a label's signature is
--   arrows from its block's parameter types to the process output type (a
--   bookkeeping convention — labels are not values). Expressions inside
--   processes parse in the monomorphic, join-free scope.
module ReWire.Synolon.Parse (parseSyn, parseSynText) where

import ReWire.Eidos.Lexer
import ReWire.Eidos.Parse (Scope (..), pendingSig, tyP, param, expP, atomP, defnP, dataDefnP, Env (..), insertVar, elabExp, elabDefn)
import ReWire.Error (MonadError, AstError)
import ReWire.Synolon.Syntax

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Text.Megaparsec (many, some, try, (<|>), (<?>), parse, sepBy, optional, eof)

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

parseSyn :: (MonadError AstError m, MonadIO m) => FilePath -> m Program
parseSyn p = liftIO (T.readFile p) >>= flip parseSynText p

parseSynText :: MonadError AstError m => Text -> FilePath -> m Program
parseSynText txt p = either failParse elabProgram $ parse (space *> programP <* eof) p txt

-- | @data* defn* proc+@: a Synolon program has no @top@ (its processes are
--   its roots) and at least one process.
programP :: Parser Program
programP = Program <$> many dataDefnP <*> many defnP <*> some procP
      <?> "program"


-- | The scope for expressions inside processes: monomorphic, no joins.
sc0 :: Scope
sc0 = Scope mempty mempty

procP :: Parser Proc
procP = do
      an <- getAnn
      keyword "proc"
      n   <- bareName
      _   <- symbol ":"
      it  <- tyP mempty
      _   <- symbol "~>"
      ot  <- tyP mempty
      clk <- optional $ symbol "@" *> keyword "clock" *> bareName
      (cells, entry, blocks) <- braces $ (,,) <$> many cellP <*> entryP <*> many blockP
      let ltab = Map.fromList
            [ (idUniq l, l { idSig = monoSig $ foldr (Arrow an . sigTy . idSig) ot $ blkParams b })
            | (l, b) <- blocks ]
      entry'  <- resolveBlock ltab entry
      blocks' <- mapM (\ (l, b) -> (Map.lookupDefault l (idUniq l) ltab, ) <$> resolveBlock ltab b) blocks
      pure $ Proc an n it ot clk cells entry' blocks'
      <?> "process"

cellP :: Parser Cell
cellP = do
      an <- getAnn
      keyword "state"
      s  <- bareName
      _  <- symbol ":"
      t  <- tyP mempty
      _  <- symbol ":="
      e0 <- (Nothing <$ keyword "undef") <|> (Just <$> expP sc0)
      semi
      pure $ Cell an s t e0

entryP :: Parser Block
entryP = do
      an <- getAnn
      keyword "entry"
      uncurry (Block an []) <$> braces blockBodyP

blockP :: Parser (Id, Block)
blockP = do
      an <- getAnn
      keyword "block"
      (occ, u)     <- uniqName
      ps           <- parens $ param mempty `sepBy` comma
      (cmds, term) <- braces blockBodyP
      pure (Id occ u pendingSig, Block an ps cmds term)

blockBodyP :: Parser ([Cmd], Term)
blockBodyP = (,) <$> many (try cmdP) <*> termP

cmdP :: Parser Cmd
cmdP = (putC <|> bindC) <* semi
      where putC :: Parser Cmd
            putC = do
                  an <- getAnn
                  keyword "put"
                  CmdPut an <$> bareName <*> atomP sc0

            bindC :: Parser Cmd
            bindC = do
                  an <- getAnn
                  (occ, u) <- uniqName
                  dcolon
                  t <- tyP mempty
                  _ <- symbol "<-"
                  let x = Id occ u $ monoSig t
                  (CmdGet an x <$> (keyword "get" *> bareName))
                        <|> (CmdBind an x <$> expP sc0)

termP :: Parser Term
termP = pauseT <|> gotoT <|> haltT <|> caseT
      <?> "block terminator"
      where pauseT :: Parser Term
            pauseT = do
                  an <- getAnn
                  keyword "pause"
                  a <- atomP sc0
                  arrow
                  (occ, u) <- uniqName
                  Pause an a (Id occ u pendingSig) <$> parens (atomP sc0 `sepBy` comma)

            gotoT :: Parser Term
            gotoT = do
                  an <- getAnn
                  keyword "goto"
                  (occ, u) <- uniqName
                  Goto an (Id occ u pendingSig) <$> parens (atomP sc0 `sepBy` comma)

            haltT :: Parser Term
            haltT = do
                  an <- getAnn
                  keyword "halt"
                  Halt an <$> atomP sc0

            caseT :: Parser Term
            caseT = do
                  an <- getAnn
                  keyword "case"
                  a <- atomP sc0
                  keyword "of"
                  TCase an a <$> braces (taltP `sepBy` semi)

taltP :: Parser TAlt
taltP = defaultAlt <|> litAlt <|> dataAlt
      <?> "terminator alternative"
      where defaultAlt :: Parser TAlt
            defaultAlt = do
                  an <- getAnn
                  underscore
                  arrow
                  TAlt an DefaultAlt [] <$> termP

            litAlt :: Parser TAlt
            litAlt = do
                  an <- getAnn
                  n  <- integer
                  arrow
                  TAlt an (LitAlt n) [] <$> termP

            dataAlt :: Parser TAlt
            dataAlt = do
                  an <- getAnn
                  c  <- conName
                  ps <- many $ param mempty
                  arrow
                  TAlt an (DataAlt c) ps <$> termP

-- | Rewrite terminator targets to the declared labels (with their
--   finalized signatures).
resolveBlock :: HashMap Uniq Id -> Block -> Parser Block
resolveBlock ltab (Block an ps cmds term) = Block an ps cmds <$> resolveTerm term
      where resolveTerm :: Term -> Parser Term
            resolveTerm = \ case
                  Pause a o l args -> (\ l' -> Pause a o l' args) <$> label l
                  Goto a l args    -> (\ l' -> Goto a l' args) <$> label l
                  t@Halt {}        -> pure t
                  TCase a s alts   -> TCase a s <$> mapM (\ (TAlt aan c xs t) -> TAlt aan c xs <$> resolveTerm t) alts

            label :: Id -> Parser Id
            label l = case Map.lookup (idUniq l) ltab of
                  Just l' -> pure l'
                  Nothing -> fail $ "terminator targets an undeclared block label: "
                                  <> T.unpack (idOcc l) <> "#" <> show (idUniq l)


---
--- Elaboration (the expression-level part is ReWire.Eidos.Parse's).
---

elabProgram :: MonadError AstError m => Program -> m Program
elabProgram (Program ds fs ps) = do
      fs' <- mapM (elabDefn env) fs
      ps' <- mapM (elabProc env) ps
      pure $ Program ds fs' ps'
      where env :: Env
            env = Env (Map.fromList [ (idUniq $ defnId d, defnId d) | d <- fs ]) mempty

-- | Elaborate the expressions embedded in a process: cell initials in the
--   top-level scope; block bodies with parameters and (sequentially)
--   command binders in scope; terminator-alternative binders over their
--   terms.
elabProc :: forall m. MonadError AstError m => Env -> Proc -> m Proc
elabProc env (Proc an n it ot clk cells entry blocks) = do
      cells'  <- mapM elabCell cells
      entry'  <- elabBlock entry
      blocks' <- mapM (\ (l, b) -> (l, ) <$> elabBlock b) blocks
      pure $ Proc an n it ot clk cells' entry' blocks'
      where elabCell :: Cell -> m Cell
            elabCell (Cell a s t e0) = Cell a s t <$> mapM (elabExp env) e0

            elabBlock :: Block -> m Block
            elabBlock (Block a ps cmds term) = do
                  (env', cmds') <- foldM elabCmd (foldr insertVar env ps, []) cmds
                  term'         <- elabTerm env' term
                  pure $ Block a ps (reverse cmds') term'

            elabCmd :: (Env, [Cmd]) -> Cmd -> m (Env, [Cmd])
            elabCmd (e, acc) = \ case
                  CmdBind a x rhs  -> do
                        rhs' <- elabExp e rhs
                        pure (insertVar x e, CmdBind a x rhs' : acc)
                  c@(CmdGet _ x _) -> pure (insertVar x e, c : acc)
                  CmdPut a s at    -> do
                        at' <- elabExp e at
                        pure (e, CmdPut a s at' : acc)

            elabTerm :: Env -> Term -> m Term
            elabTerm e = \ case
                  Pause a o l args -> Pause a <$> elabExp e o <*> pure l <*> mapM (elabExp e) args
                  Goto a l args    -> Goto a l <$> mapM (elabExp e) args
                  Halt a x         -> Halt a <$> elabExp e x
                  TCase a s alts   -> TCase a <$> elabExp e s <*> mapM (elabTAlt e) alts

            elabTAlt :: Env -> TAlt -> m TAlt
            elabTAlt e (TAlt a c xs t) = TAlt a c xs <$> elabTerm (foldr insertVar e xs) t


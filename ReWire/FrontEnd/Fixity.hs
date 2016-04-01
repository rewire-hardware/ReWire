{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module ReWire.FrontEnd.Fixity
      ( fixLocalOps
      , deuniquifyLocalOps
      , getFixities
      ) where

import ReWire.SYB

import Control.Monad.Identity (Identity (..))
import Control.Monad.State (State, runState, get, modify)
import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Annotated.Fixity (Fixity (..), applyFixities)
import Language.Haskell.Exts.Annotated.Simplify (sName, sQOp, sAssoc, sModuleName)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)

import qualified Language.Haskell.Exts as S

import Language.Haskell.Exts.Annotated.Syntax hiding (Namespace)

type Fresh = State Int
type OpRenamer = QOp SrcSpanInfo -> QOp SrcSpanInfo

fresh :: Fresh S.ModuleName
fresh = do
      x <- get
      modify $ const $ x + 1
      return $ S.ModuleName $ '$' : show x

enterScope :: S.ModuleName -> [Decl SrcSpanInfo] -> OpRenamer -> OpRenamer
enterScope m ds rn op
      | sQOp op `elem` ops = qual m op
      | otherwise          = rn op
      where ops :: [S.QOp]
            ops = foldr (\ case
                  FunBind _ (Match _ n _ _ _:_)        -> (:) $ S.QVarOp $ S.UnQual $ sName n
                  FunBind _ (InfixMatch _ _ n _ _ _:_) -> (:) $ S.QVarOp $ S.UnQual $ sName n
                  _                                    -> id) [] ds

            qual :: S.ModuleName -> QOp a -> QOp a
            qual (S.ModuleName m) (QVarOp l1 (UnQual l2 n)) = QVarOp l1 $ Qual l2 (ModuleName l2 m) n
            qual _ n                                        = n

getFixities' :: S.ModuleName -> [Decl a] -> [Fixity]
getFixities' m = map qualFixity . getFixities
      where qualFixity :: Fixity -> Fixity
            qualFixity (Fixity asc lvl (S.UnQual n)) = Fixity asc lvl (S.Qual m n)
            qualFixity f                             = f

getFixities :: [Decl a] -> [Fixity]
getFixities = foldr toFixity []
      where toFixity :: Decl a -> [Fixity] -> [Fixity]
            toFixity (InfixDecl _ asc lvl ops) = (++) $ map (Fixity (sAssoc asc) (fromMaybe 9 lvl) . S.UnQual . deOp) ops
            toFixity _ = id

            deOp :: Op a -> S.Name
            deOp = \case
                  VarOp _ n -> sName n
                  ConOp _ n -> sName n

-- | Renames local operators and fixes local and file-scope fixities.
--   applyFixities appears to be able to handle this automatically, but it seems
--   to be buggy and not allow inner fixity declarations to shadow outer ones (on
--   operators with the same name).
fixLocalOps :: Module SrcSpanInfo -> Module SrcSpanInfo
fixLocalOps = applyGlobFixities . fst . flip runState 0 . runPureT (renameDecl id ||> TId)
      where applyGlobFixities :: Module SrcSpanInfo -> Module SrcSpanInfo
            applyGlobFixities m@(Module _ (Just (ModuleHead _ mn _ _)) _ _ ds)
                                                          = runIdentity $ applyFixities (getFixities ds ++ getFixities' (sModuleName mn) ds) m
            applyGlobFixities m@(Module _ Nothing _ _ ds) = runIdentity $ applyFixities (getFixities ds) m
            applyGlobFixities m                           = m

deuniquifyLocalOps :: Module SrcSpanInfo -> Module SrcSpanInfo
deuniquifyLocalOps = runIdentity . runPureT ((||> TId) $ \ case
      QVarOp l1 (Qual l2 (ModuleName _ ('$' : _)) n) -> return $ QVarOp l1 $ UnQual l2 n
      (x :: QOp SrcSpanInfo)                         -> return x)

renameDecl :: OpRenamer -> Decl SrcSpanInfo -> Fresh (Decl SrcSpanInfo)
renameDecl rn = \ case
      PatBind l1 p (UnGuardedRhs l2 e) Nothing -> do
            e' <- renameExp rn e
            return $ PatBind l1 p (UnGuardedRhs l2 e') Nothing
      PatBind l1 p (UnGuardedRhs l2 e) (Just (BDecls l3 ds)) -> do
            m    <- fresh
            e'   <- renameExp (enterScope m ds rn) e
            e''  <- applyFixities (getFixities' m ds) e'
            ds'  <- mapM (renameDecl $ enterScope m ds rn) ds
            ds'' <- mapM (applyFixities $ getFixities' m ds) ds'
            return $ PatBind l1 p (UnGuardedRhs l2 e'') $ Just (BDecls l3 ds'')
      FunBind l ms -> FunBind l <$> mapM (renameMatch rn) ms
      d -> return d

renameMatch :: OpRenamer -> Match SrcSpanInfo -> Fresh (Match SrcSpanInfo)
renameMatch rn = \ case
      Match l1 n ps (UnGuardedRhs l2 e) Nothing -> do
            e' <- renameExp rn e
            return $ Match l1 n ps (UnGuardedRhs l2 e') Nothing
      Match l1 n ps (UnGuardedRhs l2 e) (Just (BDecls l3 ds)) -> do
            m    <- fresh
            e'   <- renameExp (enterScope m ds rn) e
            e''  <- applyFixities (getFixities' m ds) e'
            ds'  <- mapM (renameDecl $ enterScope m ds rn) ds
            ds'' <- mapM (applyFixities $ getFixities' m ds) ds'
            return $ Match l1 n ps (UnGuardedRhs l2 e'') $ Just (BDecls l3 ds'')
      InfixMatch l1 p n ps (UnGuardedRhs l2 e) Nothing -> do
            e' <- renameExp rn e
            return $ InfixMatch l1 p n ps (UnGuardedRhs l2 e') Nothing
      InfixMatch l1 p n ps (UnGuardedRhs l2 e) (Just (BDecls l3 ds)) -> do
            m    <- fresh
            e'   <- renameExp (enterScope m ds rn) e
            e''  <- applyFixities (getFixities' m ds) e'
            ds'  <- mapM (renameDecl $ enterScope m ds rn) ds
            ds'' <- mapM (applyFixities $ getFixities' m ds) ds'
            return $ InfixMatch l1 p n ps (UnGuardedRhs l2 e'') $ Just (BDecls l3 ds'')
      m -> return m

renameExp :: OpRenamer -> Exp SrcSpanInfo -> Fresh (Exp SrcSpanInfo)
renameExp rn = \ case
      InfixApp l e1 op e2     -> do
            e1' <- renameExp rn e1
            e2' <- renameExp rn e2
            return $ InfixApp l e1' (rn op) e2'
      App l e1 e2             -> App l <$> renameExp rn e1 <*> renameExp rn e2
      NegApp l e              -> NegApp l <$> renameExp rn e
      Lambda l ps e           -> Lambda l ps <$> renameExp rn e
      Let l1 (BDecls l2 ds) e -> do
            m    <- fresh
            ds'  <- mapM (renameDecl $ enterScope m ds rn) ds
            ds'' <- mapM (applyFixities $ getFixities' m ds) ds'
            e'   <- renameExp (enterScope m ds rn) e
            e''  <- applyFixities (getFixities' m ds) e'
            return $ Let l1 (BDecls l2 ds'') e''
      If l e1 e2 e3           -> If l <$> renameExp rn e1 <*> renameExp rn e2 <*> renameExp rn e3
      Case l e alts           -> Case l <$> renameExp rn e <*> mapM (renameAlt rn) alts
      Do l stmts              -> Do l <$> renameStmts rn stmts
      Tuple l b es            -> Tuple l b <$> mapM (renameExp rn) es
      List l es               -> List l <$> mapM (renameExp rn) es
      Paren l e               -> Paren l <$> renameExp rn e
      LeftSection l e op      -> LeftSection l <$> renameExp rn e <*> return op
      RightSection l op e     -> RightSection l op <$> renameExp rn e
      e                       -> return e

renameAlt :: OpRenamer -> Alt SrcSpanInfo -> Fresh (Alt SrcSpanInfo)
renameAlt rn = \ case
      Alt l1 p (UnGuardedRhs l2 e) Nothing -> do
            e' <- renameExp rn e
            return $ Alt l1 p (UnGuardedRhs l2 e') Nothing
      Alt l1 p (UnGuardedRhs l2 e) (Just (BDecls l3 ds)) -> do
            m    <- fresh
            e'   <- renameExp (enterScope m ds rn) e
            e''  <- applyFixities (getFixities' m ds) e'
            ds'  <- mapM (renameDecl $ enterScope m ds rn) ds
            ds'' <- mapM (applyFixities $ getFixities' m ds) ds'
            return $ Alt l1 p (UnGuardedRhs l2 e'') $ Just $ BDecls l3 ds''
      a                                  -> return a

renameStmts :: OpRenamer -> [Stmt SrcSpanInfo] -> Fresh [Stmt SrcSpanInfo]
renameStmts _ [] = return []
renameStmts rn (Generator l p e : stmts) = do
      e'     <- renameExp rn e
      stmts' <- renameStmts rn stmts
      return $ Generator l p e' : stmts'
renameStmts rn (Qualifier l e : stmts) = do
      e'     <- renameExp rn e
      stmts' <- renameStmts rn stmts
      return $ Qualifier l e' : stmts'
renameStmts rn (LetStmt l1 (BDecls l2 ds) : stmts) = do
      m       <- fresh
      ds'     <- mapM (renameDecl $ enterScope m ds rn) ds
      ds''    <- mapM (applyFixities $ getFixities' m ds) ds'
      stmts'  <- renameStmts (enterScope m ds rn) stmts
      stmts'' <- mapM (applyFixities $ getFixities' m ds) stmts'
      return $ LetStmt l1 (BDecls l2 ds'') : stmts''
renameStmts _ stmts = return stmts


{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.Fixity
      ( fixLocalOps
      , deuniquifyLocalOps
      , getFixities
      ) where

import ReWire.SYB
import ReWire.Error
import ReWire.Annotation (Annote)

import Control.Monad (void)
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (MonadState, StateT, runStateT, get, modify, lift)
import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Fixity (Fixity (..), applyFixities)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)

import Language.Haskell.Exts.Syntax

type FreshT = StateT Int
type OpRenamer = QOp SrcSpanInfo -> QOp SrcSpanInfo

mark' :: MonadState Annote m => SrcSpanInfo -> FreshT m ()
mark' = lift . mark

fresh :: Monad m => FreshT m (ModuleName ())
fresh = do
      x <- get
      modify (+ 1)
      pure $ ModuleName () $ '$' : show x

enterScope :: ModuleName () -> [Decl SrcSpanInfo] -> OpRenamer -> OpRenamer
enterScope m ds rn op
      | void op `elem` ops = qual m op
      | otherwise          = rn op
      where ops :: [QOp ()]
            ops = foldr (\ case
                  FunBind _ (Match _ n _ _ _:_)        -> (:) $ QVarOp () $ UnQual () $ void n
                  FunBind _ (InfixMatch _ _ n _ _ _:_) -> (:) $ QVarOp () $ UnQual () $ void n
                  _                                    -> id) [] ds

            qual :: ModuleName () -> QOp a -> QOp a
            qual (ModuleName () m) (QVarOp l1 (UnQual l2 n)) = QVarOp l1 $ Qual l2 (ModuleName l2 m) n
            qual _ n                                        = n

getFixities' :: ModuleName () -> [Decl a] -> [Fixity]
getFixities' m = map qualFixity . getFixities
      where qualFixity :: Fixity -> Fixity
            qualFixity (Fixity asc lvl (UnQual () n)) = Fixity asc lvl (Qual () m n)
            qualFixity f                              = f

getFixities :: [Decl a] -> [Fixity]
getFixities = foldr toFixity []
      where toFixity :: Decl a -> [Fixity] -> [Fixity]
            toFixity (InfixDecl _ asc lvl ops) = (++) $ map (Fixity (void asc) (fromMaybe 9 lvl) . UnQual () . deOp) ops
            toFixity _                         = id

            deOp :: Op a -> Name ()
            deOp = \case
                  VarOp _ n -> void n
                  ConOp _ n -> void n

-- | Renames local operators and fixes local and file-scope fixities.
--   applyFixities appears to be intended to handle this automatically, but it seems
--   to be buggy and not allow inner fixity declarations to shadow outer ones (on
--   operators with the same name).
--   Note: applyFixities annoyingly fixes the annotation type as SrcSpanInfo.
fixLocalOps :: (MonadState Annote m, MonadFail m) => Module SrcSpanInfo -> m (Module SrcSpanInfo)
fixLocalOps = (applyGlobFixities =<<) . fmap fst . flip runStateT 0 . runPureT (renameDecl id ||> TId)
      where applyGlobFixities :: (MonadState Annote m, MonadFail m) => Module SrcSpanInfo -> m (Module SrcSpanInfo)
            applyGlobFixities m@(Module _ (Just (ModuleHead _ mn _ _)) _ _ ds)
                                                          = mark (ann m) >> applyFixities (getFixities ds ++ getFixities' (void mn) ds) m
            applyGlobFixities m@(Module _ Nothing _ _ ds) = mark (ann m) >> applyFixities (getFixities ds) m
            applyGlobFixities m                           = pure m

deuniquifyLocalOps :: Module SrcSpanInfo -> Module SrcSpanInfo
deuniquifyLocalOps = runIdentity . runPureT ((||> TId) $ \ case
      QVarOp l1 (Qual l2 (ModuleName _ ('$' : _)) n) -> pure $ QVarOp l1 $ UnQual l2 n
      (x :: QOp SrcSpanInfo)                         -> pure x)

renameDecl :: (MonadState Annote m, MonadFail m) => OpRenamer -> Decl SrcSpanInfo -> FreshT m (Decl SrcSpanInfo)
renameDecl rn = \ case
      PatBind l1 p (UnGuardedRhs l2 e) Nothing -> do
            mark' l1
            e' <- renameExp rn e
            pure $ PatBind l1 p (UnGuardedRhs l2 e') Nothing
      PatBind l1 p (UnGuardedRhs l2 e) (Just (BDecls l3 ds)) -> do
            mark' l1
            (e', ds') <- renameExpInScope rn ds e
            pure $ PatBind l1 p (UnGuardedRhs l2 e') $ Just (BDecls l3 ds')
      FunBind l ms -> do
            mark' l
            FunBind l <$> mapM (renameMatch rn) ms
      d -> pure d

renameMatch :: (MonadState Annote m, MonadFail m) => OpRenamer -> Match SrcSpanInfo -> FreshT m (Match SrcSpanInfo)
renameMatch rn = \ case
      Match l1 n ps (UnGuardedRhs l2 e) Nothing -> do
            mark' l1
            e' <- renameExp rn e
            pure $ Match l1 n ps (UnGuardedRhs l2 e') Nothing
      Match l1 n ps (UnGuardedRhs l2 e) (Just (BDecls l3 ds)) -> do
            mark' l1
            (e', ds') <- renameExpInScope rn ds e

            pure $ Match l1 n ps (UnGuardedRhs l2 e') $ Just (BDecls l3 ds')
      InfixMatch l1 p n ps (UnGuardedRhs l2 e) Nothing -> do
            mark' l1
            e' <- renameExp rn e
            pure $ InfixMatch l1 p n ps (UnGuardedRhs l2 e') Nothing
      InfixMatch l1 p n ps (UnGuardedRhs l2 e) (Just (BDecls l3 ds)) -> do
            mark' l1
            (e', ds') <- renameExpInScope rn ds e
            pure $ InfixMatch l1 p n ps (UnGuardedRhs l2 e') $ Just (BDecls l3 ds')
      m -> pure m

renameExp :: (MonadState Annote m, MonadFail m) => OpRenamer -> Exp SrcSpanInfo -> FreshT m (Exp SrcSpanInfo)
renameExp rn = \ case
      InfixApp l e1 op e2     -> do
            mark' l
            e1' <- renameExp rn e1
            e2' <- renameExp rn e2
            pure $ InfixApp l e1' (rn op) e2'
      App l e1 e2             -> mark' l >> App l <$> renameExp rn e1 <*> renameExp rn e2
      NegApp l e              -> mark' l >> NegApp l <$> renameExp rn e
      Lambda l ps e           -> mark' l >> Lambda l ps <$> renameExp rn e
      Let l1 (BDecls l2 ds) e -> do
            mark' l1
            (e', ds') <- renameExpInScope rn ds e
            pure $ Let l1 (BDecls l2 ds') e'
      If l e1 e2 e3           -> mark' l >> If l <$> renameExp rn e1 <*> renameExp rn e2 <*> renameExp rn e3
      Case l e alts           -> mark' l >> Case l <$> renameExp rn e <*> mapM (renameAlt rn) alts
      Do l stmts              -> mark' l >> Do l <$> renameStmts rn stmts
      Tuple l b es            -> mark' l >> Tuple l b <$> mapM (renameExp rn) es
      List l es               -> mark' l >> List l <$> mapM (renameExp rn) es
      Paren l e               -> mark' l >> Paren l <$> renameExp rn e
      LeftSection l e op      -> mark' l >> LeftSection l <$> renameExp rn e <*> pure op
      RightSection l op e     -> mark' l >> RightSection l op <$> renameExp rn e
      e                       -> pure e

renameAlt :: (MonadState Annote m, MonadFail m) => OpRenamer -> Alt SrcSpanInfo -> FreshT m (Alt SrcSpanInfo)
renameAlt rn = \ case
      Alt l1 p (UnGuardedRhs l2 e) Nothing -> do
            mark' l1
            e' <- renameExp rn e
            pure $ Alt l1 p (UnGuardedRhs l2 e') Nothing
      Alt l1 p (UnGuardedRhs l2 e) (Just (BDecls l3 ds)) -> do
            mark' l1
            (e', ds') <- renameExpInScope rn ds e
            pure $ Alt l1 p (UnGuardedRhs l2 e') $ Just $ BDecls l3 ds'
      a                                  -> pure a

renameStmts :: (MonadState Annote m, MonadFail m) => OpRenamer -> [Stmt SrcSpanInfo] -> FreshT m [Stmt SrcSpanInfo]
renameStmts _ [] = pure []
renameStmts rn (Generator l p e : stmts) = do
      mark' l
      e'     <- renameExp rn e
      stmts' <- renameStmts rn stmts
      pure $ Generator l p e' : stmts'
renameStmts rn (Qualifier l e : stmts) = do
      mark' l
      e'     <- renameExp rn e
      stmts' <- renameStmts rn stmts
      pure $ Qualifier l e' : stmts'
renameStmts rn (LetStmt l1 (BDecls l2 ds) : stmts) = do
      mark' l1
      m       <- fresh
      stmts'  <- renameStmts (enterScope m ds rn) stmts
      stmts'' <- mapM (applyFixities $ getFixities' m ds) stmts'
      ds'     <- mapM (renameDecl $ enterScope m ds rn) ds
      ds''    <- mapM (applyFixities $ getFixities' m ds) ds'
      pure $ LetStmt l1 (BDecls l2 ds'') : stmts''
renameStmts _ stmts = pure stmts

renameExpInScope :: (MonadState Annote m, MonadFail m) => OpRenamer -> [Decl SrcSpanInfo] -> Exp SrcSpanInfo -> FreshT m (Exp SrcSpanInfo, [Decl SrcSpanInfo])
renameExpInScope rn ds e = do
      m    <- fresh
      e'   <- renameExp (enterScope m ds rn) e
      e''  <- applyFixities (getFixities' m ds) e'
      ds'  <- mapM (renameDecl $ enterScope m ds rn) ds
      ds'' <- mapM (applyFixities $ getFixities' m ds) ds'
      pure (e'', ds'')


{-# LANGUAGE OverloadedStrings #-}
module ReWire.HSE.GHCCoreToCrust (toCrust) where

import ReWire.Annotation (noAnn)
import ReWire.Unbound (Embed (..), bind, s2n)
import qualified ReWire.HSE.Rename   as M
import qualified ReWire.Crust.Syntax as M
import qualified ReWire.Crust.Util   as M
import qualified ReWire.Crust.Types  as M
import ReWire.Crust.Types ((|->))

import GHC.Plugins
      ( Bind (..), Expr (..)
      , ppr, Type, Var
      , splitTyConApp_maybe, getTyVar_maybe
      , splitFunTy_maybe
      , varType, varName, tyConName
      , showSDoc, Outputable, DynFlags
      , Literal (..)
      )
import GHC
      ( CoreModule (..)
      , getSessionDynFlags
      , GhcMonad
      )
import GHC.Core (Alt (..))
import GHC.Utils.Monad (concatMapM)
import Data.Text (pack, unpack, Text)
import Data.List (foldl')

toCrust :: GhcMonad m => CoreModule -> m (M.Module, M.Exports)
toCrust c = do
--    cs <- toDataDefns $ cm_types c
      let cs = []
      ds <- concatMapM toDefn $ cm_binds c
      pure (M.Module cs [] ds, mempty)

-- toDataDefns :: GhcMonad m => TypeEnv -> m [M.DataDefn]
-- toDataDefns _ = pure []

toDefn :: GhcMonad m => Bind Var -> m [M.Defn]
toDefn = \ case
      NonRec v e -> pure <$> toDefn' (v, e)
      Rec bs     -> mapM toDefn' bs
      where toDefn' :: GhcMonad m => (Var, Expr Var) -> m M.Defn
            toDefn' (v, e) = do
                  e' <- toExp e
                  t' <- toType $ varType v
                  v' <- toName v
                  pure $ M.Defn noAnn v' ([] |-> t') Nothing $ Embed $ bind [] e'

toExp :: GhcMonad m => Expr Var -> m M.Exp
toExp = \ case
      Var v                     -> M.Var noAnn Nothing Nothing <$> toName v
      Lit (LitNumber _ v)       -> pure $ M.LitInt noAnn Nothing v
      Lit (LitString v)         -> pure $ M.LitStr noAnn Nothing $ pack $ show v
      Lit _                     -> pure $ M.mkError noAnn Nothing "lit"
      App e1 e2                 -> M.mkApp noAnn <$> toExp e1 <*> (pure <$> toExp e2)
      Lam v e                   -> do
            v' <- toName v
            e' <- toExp e
            pure $ M.Lam noAnn Nothing Nothing $ bind v' e'
      Let (NonRec v e) body   -> do
            v'    <- toName v
            e'    <- toExp e
            body' <- toExp body
            pure $ M.mkApp noAnn (M.Lam noAnn Nothing Nothing $ bind v' body') [e']
      Let _ _   -> pure $ ph "Let"
      Case e v _type alts -> do
            e'         <- toExp e
            v'         <- toName v
            alts' <- toCase (M.Var noAnn Nothing Nothing v') $ reverse alts
            pure $ case alts' of
                  Just alts'' -> M.mkApp noAnn (M.Lam noAnn Nothing Nothing $ bind v' alts'') [e']
                  Nothing     -> M.mkApp noAnn (M.Lam noAnn Nothing Nothing $ bind v' $ M.mkError noAnn Nothing "GHCCoreToCrust: case: empty alts.") [e']
      Cast e _coercion          -> toExp e
      Tick _tickish e           -> toExp e
      Type _type                -> pure $ ph "Type"
      Coercion _coercion        -> pure $ ph "Coercion"

-- toCase :: GhcMonad m => M.Exp -> [(AltCon, [Var], Expr Var)] -> m (Maybe M.Exp)
toCase :: GhcMonad m => M.Exp -> [Alt Var] -> m (Maybe M.Exp)
toCase _scr = \ case
--       ((DataAlt c, vs, e) : alts) -> do
--             e'  <- toExp e
--             alts' <- toCase scr alts
--             pat'  <- toPat c vs
--             pure $ Just $ M.Case noAnn Nothing scr (bind pat' e') alts'
--       ((LitAlt _lit, _, e) : alts) -> do
--             _e'    <- toExp e
--             _alts' <- toCase scr alts
--             -- pat'  <- toPat c vs
--             pure $ Just $ M.mkError noAnn Nothing "lit case"
--       [(DEFAULT, _, e)] -> do
--             e'  <- toExp e
--             pure $ Just e'
      []                  -> pure Nothing
      _                   -> pure Nothing
--      where
--          toPat :: GhcMonad m => DataCon -> [Var] -> m M.Pat
--          toPat c vs = do
--                vs' <- mapM toName vs
--                c'  <- dcToName c
--                pure $ M.PatCon noAnn (M.Embed Nothing) (M.Embed c') $ map (M.PatVar noAnn $ Embed Nothing) vs'

--          dcToName :: GhcMonad m => DataCon -> m (M.Name M.DataConId)
--          dcToName c = do
--                dflags <- getSessionDynFlags
--                pure $ s2n $ showGhc dflags $ dataConName c

toName :: GhcMonad m => Var -> m (M.Name a)
toName v = do
      dflags <- getSessionDynFlags
      pure $ s2n $ showGhc dflags $ varName v

toType :: GhcMonad m => Type -> m M.Ty
toType = \ case
      -- (splitForAllTy_maybe -> Just (_tycovar, t)) -> toType t
      (splitFunTy_maybe -> Just (_mult, t1, tr)) -> do
            t1' <- toType t1
            tr' <- toType tr
            pure $ M.mkArrowTy [t1'] tr'
      (getTyVar_maybe -> Just tv) -> do
            tv' <- toName tv
            pure $ M.TyVar noAnn M.kblank tv'
      (splitTyConApp_maybe -> Just (c, trs)) -> do
            dflags <- getSessionDynFlags
            trs' <- mapM toType trs
            pure $ foldl' (M.TyApp noAnn) (M.TyCon noAnn $ s2n $ showGhc dflags $ tyConName c) trs'
      t -> do
            dflags <- getSessionDynFlags
            error $ "what type is this: " <> unpack (showGhc dflags t)

ph :: Text -> M.Exp
ph m = M.mkError noAnn Nothing $ "GHCCoreToCrust: " <> m

showGhc :: Outputable a => DynFlags -> a -> Text
showGhc dflags = pack . showSDoc dflags . ppr

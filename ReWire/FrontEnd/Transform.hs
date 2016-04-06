{-# LANGUAGE LambdaCase, ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module ReWire.FrontEnd.Transform
      ( inline, reduce
      , neuterPrims
      , liftLambdas
      , purge
      ) where

import ReWire.Annotation
import ReWire.FrontEnd.Syntax
import ReWire.SYB hiding (query)

import Control.Monad.Catch (MonadCatch)
import Control.Monad.State (State, runState, StateT (..), get, put)
import Control.Monad (zipWithM)
import Data.List (find, nub, (\\))
import Data.Maybe (fromJust)

import Unbound.Generics.LocallyNameless (Fresh (..), string2Name, name2String, substs, subst)

-- | Inlines defs marked for inlining. Must run before lambda lifting.
inline :: Fresh m => RWMProgram -> m RWMProgram
inline (RWMProgram ts ds) = do
      ds'  <- untrec ds
      subs <- mapM toSubst $ filter defnInline ds'
      return $ RWMProgram ts $ trec $ map (substs subs) ds'
      where toSubst :: Fresh m => RWMDefn -> m (Name RWMExp, RWMExp)
            toSubst (RWMDefn _ n _ _ (Embed e)) = do
                  ([], e') <- unbind e
                  return (n, e')

-- | Replaces the expression in RWMNativeVHDL so we don't descend into it
--   during other transformations.
neuterPrims :: (MonadCatch m, Fresh m) => RWMProgram -> m RWMProgram
neuterPrims = transProg $ \_ -> transform $
      \ (RWMNativeVHDL an s e) -> return $ RWMNativeVHDL an s (RWMError an (typeOf e) "nativeVHDL expression placeholder")

liftLambdas :: (MonadCatch m, Fresh m) => RWMProgram -> m RWMProgram
liftLambdas p = fst <$> runStateT (transProg liftLambdas' p) []
      where liftLambdas' :: (MonadCatch m, Fresh m) => ([RWMData], [RWMDefn]) -> Transform (StateT [RWMDefn] m)
            liftLambdas' (_, ds) =  \ case
                  l@(RWMLam _ _ e) -> do
                        let gvs = map defnName ds -- TODO (chathhorn) non-global locals
                        (x, e') <- unbind e
                        let fvs = filter (not . (`elem` x:gvs)) $ fv e'
                        newApp (fvs ++ [x]) l e'
                  RWMCase an e e1 e2 -> do
                        let gvs = map defnName ds -- TODO (chathhorn) non-global locals
                        (p, e1') <- unbind e1
                        let pvs = patVars p
                            fvs = filter (not . (`elem` gvs ++ pvs)) $ fv e1'
                        e1'' <- newApp (fvs ++ pvs) e1' e1'
                        return $ RWMCase an e (bind p e1'') e2
                  ||> (\ ([] :: [RWMDefn]) -> get) -- damn
                  ||> TId

            newApp :: (Monad m, Fresh m) => [Name RWMExp] -> RWMExp -> RWMExp -> StateT [RWMDefn] m RWMExp
            newApp fvs e body = do
                  fvts <- mapM lookupType fvs
                  let t = foldr mkArrow (typeOf e) $ reverse fvts
                  f    <- fresh $ string2Name "LL"
                  ds   <- get
                  put $ RWMDefn (ann e) f ([] |-> t) False (Embed $ bind fvs body) : ds
                  return $ foldl (\ e' (v, vt) -> RWMApp (ann e) e' $ RWMVar (ann e) vt v) (RWMVar (ann e) t f)
                         $ zip fvs fvts

            -- getType :: RWMDefn -> RWMTy
            -- getType (RWMDefn _ _ ([] :-> t) _ _ _)  = t
            -- getType _ = error "getType: impossible ??"

            lookupType :: (Monad m, Fresh m) => Name RWMExp -> StateT [RWMDefn] m RWMTy
            lookupType _ = return tblank
                  -- TODO(chathhorn)
                  -- query n >>= \ case
                  -- -- TODO(chathhorn) Unify!?!
                  -- Just (GlobalVar d)  -> return $ getType d
                  -- Just (LocalVar t) -> return t
                  -- _ -> getType . fromMaybe (error $ "liftLambdas: failed to find: " ++ show v) . findDefn v <$> get

-- | Purge.

purge :: Fresh m => RWMProgram -> m RWMProgram
purge (RWMProgram ts vs) = do
      vs' <- untrec vs
      return $ RWMProgram ts $ trec $ inuseDefn vs'

inuseDefn :: [RWMDefn] -> [RWMDefn]
inuseDefn ds = case find ((=="Main.start") . name2String . defnName) ds of
      Just n -> map (findDefn' ds) $ snd $ runState (inuseDefn' $ defnName n) [defnName n]
      Nothing -> []
      where inuseDefn' :: Name RWMExp -> State [Name RWMExp] ()
            inuseDefn' n = do
                  inuse     <- get
                  let inuse' = nub $ inuse ++ fvDefn n ds
                  put inuse'
                  mapM_ inuseDefn' $ inuse' \\ inuse

findDefn :: Name RWMExp -> [RWMDefn] -> Maybe RWMDefn
findDefn n = find ((==n) . defnName)

findDefn' :: [RWMDefn] -> Name RWMExp -> RWMDefn
findDefn' ds = fromJust . flip findDefn ds

fvDefn :: Name RWMExp -> [RWMDefn] -> [Name RWMExp]
fvDefn n ds = fv $ findDefn' ds n

-- | Reduce.

reduce :: Fresh m => RWMProgram -> m RWMProgram
reduce (RWMProgram ts vs) = do
      vs' <- untrec vs
      RWMProgram ts <$> (trec <$> mapM reduceDefn vs')

reduceDefn :: Fresh m => RWMDefn -> m RWMDefn
reduceDefn (RWMDefn an n pt b (Embed e)) = do
      (vs, e') <- unbind e
      RWMDefn an n pt b <$> Embed <$> bind vs <$> reduceExp e'

reduceExp :: Fresh m => RWMExp -> m RWMExp
reduceExp = \ case
      RWMApp an e1 e2      -> do
            e1' <- reduceExp e1
            e2' <- reduceExp e2
            case e1' of
                  RWMLam _ _ e -> do
                        (x, e') <- unbind e
                        reduceExp $ subst x e2' e'
                  _              -> return $ RWMApp an e1' e2'
      RWMLam an t e      -> do
            (x, e') <- unbind e
            RWMLam an t <$> bind x <$> reduceExp e'
      RWMCase an e e1 e2 -> do
            (p, e1') <- unbind e1
            e' <- reduceExp e
            mr <- matchPat e' p
            case mr of
                  MatchYes sub -> reduceExp $ substs sub e1'
                  MatchMaybe   -> RWMCase an e' <$> (bind p <$> reduceExp e1') <*> reduceExp e2
                  MatchNo      -> reduceExp e2
      e -> return e

data MatchResult = MatchYes [(Name RWMExp, RWMExp)]
                 | MatchMaybe
                 | MatchNo
                 deriving Show

mergeMatches :: [MatchResult] -> MatchResult
mergeMatches []     = MatchYes []
mergeMatches (m:ms) = case mergeMatches ms of
      MatchYes bs -> case m of
            MatchYes bs' -> MatchYes $ bs' ++ bs
            MatchNo      -> MatchNo
            MatchMaybe   -> MatchMaybe
      MatchNo     -> MatchNo
      MatchMaybe  -> case m of
            MatchYes _ -> MatchMaybe
            MatchNo    -> MatchNo
            MatchMaybe -> MatchMaybe

matchPat :: Fresh m => RWMExp -> RWMPat -> m MatchResult
matchPat e = \ case
      RWMPatCon _ i pats -> case flattenApp e of
            RWMCon _ _ c : es
                  | c == i && length es == length pats -> do
                        ms <- zipWithM matchPat es pats
                        return $ mergeMatches ms
                  | otherwise                          -> return MatchNo
            _                                          -> return MatchMaybe
      RWMPatVar _ _ x    -> return $ MatchYes [(x, e)]

